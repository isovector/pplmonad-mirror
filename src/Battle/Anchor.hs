{-# LANGUAGE Arrows, FlexibleContexts, OverloadedStrings #-}

module Battle.Anchor (anchor) where

import Control.Monad.Cont
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.RWS
import qualified Data.Map as M
import FRP.Yampa
import FRP.Yampa.Geometry
import System.Random

import Activity
import Battle.Moves
import Battle.Output
import Battle.Parameters
import Battle.Vignette
import ControlsMaps
import Inventory.Menu
import Inventory.Parameters
import LabelName
import Lightarrow
import Menu
import Message
import OfflineData
import Ppmn.Menu
import Ppmn.Parameters
import ProseName
import TextBox as TB

anchor exit = anchorLoop 0 exit

anchorLoop k0 exit = do
    modify (\bp -> bp { bpExit = exit })
    friend <- gets bpFriend
    enemy <- gets bpEnemy
    run <- gets bpRun
    scene <- gets sceneDefault
    summary <- gets sceneSummary
    let options = [ (Fight, callCC fight),
                    (LabelName.People, callCC (people 0)),
                    (LabelName.Item, callCC items),
                    (Run, preemptiveTurn run) ]
        menu = boxMenuControl >>> mainMenu options (return (), k0) k0
        embedding = embedArr ((scene >.= summary >.= TB.drawConstant "") >.=)
    k <- local (embedding `compEmbed`) (choiceOverlay menu >>= (\(next, k) -> next >> return k))
    anchorLoop k exit
    
people = selectPpmn ChooseAPerson bpPpmn (personMenu personOptions)

personOptions position =
    [ (Switch, preemptiveTurn $ switchToPpmn position),
      (Cancel, return ()) ]

updateFriend new bp = bp { bpPpmn = ppmn' }
  where
    ppmn' = take k ppmn ++ (new : drop (k + 1) ppmn)
    k = bpFriendIndex bp
    ppmn = bpPpmn bp

updateEnemy new bp = bp { bpEnemies = enemies' }
  where
    enemies' = take k enemies ++ (new : drop (k + 1) enemies)
    k = bpEnemyIndex bp
    enemies = bpEnemies bp

updateRandomGenerator result bp = bp { bpRandomGenerator = mkStdGen $ fst (random result) }

fight cancel = do
    friend <- gets bpFriend
    let friendMoves = map friendMoveByName (ppmnMoves friend)
        menu = menuControl >>> moveMenu (map (mpName &&& run) friendMoves) (cancel (), 0) 0
    (next, _) <- choiceReplace menu
    next
  where
    run move = turn $ do
        ms <- gets (friendMoveState move)
        (result, _) <- lift $ execRWST (mpCont move >> checkFaint) (Embedding id) ms
        modify (updateFriend (actionSubject result))
        modify (updateEnemy (actionObject result))
        modify (updateRandomGenerator result)

items cancel = do
    scene <- gets sceneDefault
    summaryE <- gets sceneSecondSummary
    inventory <- gets (M.elems . bpItems)
    nameA <- gets bpAvatarName
    let run item = do
            iu <- gets (itemUse item)
            (next, result, _) <- lift $ runRWST (itemBattleCont item) (embedArr ((scene >.= summaryE >.= TB.drawConstant "") >.=)) iu
            let ItemUse { iuParams = params } = result
            modify (\bp -> bp { bpItems = if itemStock params == 0
                                                    then M.delete (itemName params) (bpItems bp)
                                                    else M.insert (itemName params) params (bpItems bp) })
            modify (updateFriend (actionSubject result))
            modify (updateRandomGenerator result)
            local (const $ embedArr (scene >.=)) next
    (next, _) <- choiceReplace (menuControl >>> itemMenu run (cancel (), 0) inventory 0)
    preemptiveTurn next
 
turn friendMove = do
    friendSpeed <- gets (ppmnSpeed . bpFriend)
    enemySpeed <- gets (ppmnSpeed . bpEnemy)
    enemyMove <- chooseEnemyMove
    case compare friendSpeed enemySpeed of
        GT -> friendInitiative friendMove enemyMove
        LT -> enemyInitiative friendMove enemyMove
        EQ -> do
            friendFirst <- state random
            if friendFirst
                then friendInitiative friendMove enemyMove
                else enemyInitiative friendMove enemyMove
    checkBattleDone
    exit <- gets bpExit
    local (const $ Embedding id) (anchor exit)
  where
    friendInitiative friendMove enemyMove = do
        friendMove
        checkBattleDone
        enemyMove
    enemyInitiative friendMove enemyMove = do
        enemyMove
        checkBattleDone
        friendMove

preemptiveTurn k = do
    enemyMove <- chooseEnemyMove
    k
    checkBattleDone
    enemyMove
    checkBattleDone
    exit <- gets bpExit
    local (const $ Embedding id) (anchor exit)

chooseEnemyMove = do
    moves  <- gets ((map enemyMoveByName) . ppmnMoves . bpEnemy)
    let  h     = fromIntegral (length moves - 1)
    x      <- state (randomR (0, (1/2) * log (2 * h + 2)))
    let  move  = moves !! round ((1/2) * exp (2 * x :: Double) - 1)
    return $ do
        ms <- gets (enemyMoveState move)
        (result, _) <- lift $ execRWST (mpCont move >> checkFaint) (Embedding id) ms
        modify (updateFriend (actionObject result))
        modify (updateEnemy (actionSubject result))
        modify (updateRandomGenerator result)

checkFaint = do
    object   <- gets actionObject
    if ppmnHitPoints object <= 0
        then do  scene    <- gets sceneDefault
                 summary  <- gets sceneSummary
                 over 0.1 $ constant (scene >.= summary >.= TB.drawConstant "")
                 faint
        else return ()

checkBattleDone = do
    exit <- gets bpExit
    friend <- gets bpFriend
    enemy <- gets bpEnemy
    if ppmnHitPoints enemy <= 0 
        then do
            drawFriend <- gets sceneFirst
            drawSummary <- gets sceneFirstSummary
            let embed = embedArr ((drawFriend >.= drawSummary >.= TB.drawConstant "") >.=)
            local (const embed) $ deployNextEnemy exit
            local (const $ Embedding id) (anchor exit)
        else return ()
    if ppmnHitPoints friend <= 0
        then do
            drawEnemy <- gets sceneSecond
            drawSummary <- gets sceneSecondSummary
            let embed = embedArr ((drawEnemy >.= drawSummary >.= TB.drawConstant "") >.=)
            local (const embed) $ deployNextPpmn exit
        else return ()

deployNextEnemy exit = do
    ppmn <- gets bpEnemies
    k <- gets bpEnemyIndex
    modify (\bp -> bp { bpCounters = kill (bpCounters bp) })
    let ppmn' = take k ppmn ++ drop (k + 1) ppmn
    if null ppmn'
        then do
            win <- gets bpWin
            win >> exit () 
        else do
            modify (\bp -> bp { bpEnemies = ppmn' })
            deployEnemy

deployNextPpmn exit = do
    ppmn <- gets bpPpmn
    k <- gets bpFriendIndex
    modify (\bp -> bp { bpCounters = lose (bpCounters bp) })
    let ppmn' = take k ppmn ++ drop (k + 1) ppmn
        k'    = min k (length ppmn' - 1)
    if null ppmn'
        then do
            lose <- gets bpLose
            lose >> exit ()
        else do
            modify (\bp -> bp { bpFriendIndex = k', bpPpmn = ppmn' })
            deploy
            local (const $ Embedding id) (anchor exit)

switchToPpmn position = do
    recall
    ppmn <- gets bpPpmn
    modify (\bp -> bp { bpFriendIndex = position })
    deploy

moveMenu = (((backgroundMenu 128 88 (Point2 32 16)) .) .) . (scrollMenu 4 scrollPresenter)

mainMenu options cancel initial = backgroundMenu 96 48 (Point2 64 96) (boxMenu options cancel initial)

-- box menu
boxMenu options@[opt1, opt2, opt3, opt4] cancel k0 = attachOffsets >>> listMenu selector drawer dispatcher
  where
    (labels, dispatches) = unzip options
    listeners = map listenSelection (zip dispatches [0 .. length dispatches - 1])
    selector = rangeBoxListSelector k0 2 2
    drawer = columnMenuDraw cursor (columnPresenter labels)
    dispatcher = listDispatcher (listenCancel cancel) listeners
    offsets = [ vector2 0 0, vector2 48 0, vector2 0 16, vector2 48 16 ]
    attachOffsets = first (arr (flip (,) offsets))

boxSelector initial down left right up = arr (mergeEvents . map curse) >>> accumHold initial 
  where
    curse CursorDown  = Event $ down
    curse CursorLeft  = Event $ left
    curse CursorRight = Event $ right
    curse CursorUp    = Event $ up
    curse _           = NoEvent

-- row major order
rangeBoxListSelector initial rows columns = boxSelector initial down left right up
  where
    position k = (x, y)
      where
        x = k `mod` columns
        y = k `div` columns
    clamp (x, y) = y' * columns + x'
      where
        x' = min (columns - 1) (max 0 x)
        y' = min (rows - 1) (max 0 y)
    down  = clamp . second (+ 1) . position
    left  = clamp . first (subtract 1) . position
    right = clamp . first (+ 1) . position
    up    = clamp . second (subtract 1) . position
