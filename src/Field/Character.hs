{-# LANGUAGE Arrows, FlexibleContexts, OverloadedStrings #-}

module Field.Character where

import Control.Applicative ((<|>))
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Cont
import Data.Bool
import qualified Data.Text as T
import FRP.Yampa hiding (left, right)
import FRP.Yampa.Geometry
import System.Random

import Activity
import Controls
import Field.CardinalDirection
import Field.Parameters
import {-# SOURCE #-} Field.Personae
import Field.PersonaName
import Field.Terrain
import Lightarrow
import Message
import OfflineData

data Character = Character {
    cAnimation :: KeepSF () (Point2 Double -> Vector2 Double -> OfflineIO),
    cDraw :: Point2 Double -> Vector2 Double -> OfflineIO,
    cDirection :: CardinalDirection,
    cGaits :: CharacterGaits,
    cIndex :: Int,
    cName :: PersonaName,
    cOccupy :: Terrain -> Terrain,
    cPosition :: (Double, Double),
    cRandomGenerator :: StdGen,
    cSpeed :: Double,
    cStances :: CharacterStances
}

instance Show Character where
    show c = show (cName c, let (x, y) = cPosition c in (round x, round y))

instance Read Character where
    readsPrec = (map (first (\(n, x) -> personaByName n x)) .) . readsPrec

instance RandomGen Character where
    next c    = let (k, g) = next (cRandomGenerator c) in
                    (k, c { cRandomGenerator = g })
    split c   = let (g1, g2) = split (cRandomGenerator c) in
                    (c { cRandomGenerator = g1 }, c { cRandomGenerator = g2 })
    genRange  = genRange . cRandomGenerator

data CharacterStances = CharacterStances {
    csEastStance    :: SF () (Point2 Double -> Vector2 Double -> OfflineIO),
    csNorthStance   :: SF () (Point2 Double -> Vector2 Double -> OfflineIO),
    csSouthStance   :: SF () (Point2 Double -> Vector2 Double -> OfflineIO),
    csWestStance    :: SF () (Point2 Double -> Vector2 Double -> OfflineIO)
}

data CharacterGaits = CharacterGaits {
    cgEastGait      :: SF () (Point2 Double -> Vector2 Double -> OfflineIO),
    cgNorthGait     :: SF () (Point2 Double -> Vector2 Double -> OfflineIO),
    cgSouthGait     :: SF () (Point2 Double -> Vector2 Double -> OfflineIO),
    cgWestGait      :: SF () (Point2 Double -> Vector2 Double -> OfflineIO)
}

blocked direction c = maybe False teCollides . getElem (round (x + dx)) (round (y + dy))
  where  (x, y)    = cPosition c
         (dx, dy)  = displacement direction

pivot newDirection c = c' { cAnimation = keep (gait c') }
  where  c' = c { cDirection = newDirection }

snap c@(Character { cPosition = (x, y) }) = c { cPosition = (s x, s y) }
  where s = fromIntegral . round 

stance Character { cDirection = North,  cStances = stances }  = csNorthStance stances
stance Character { cDirection = West,   cStances = stances }  = csWestStance stances
stance Character { cDirection = South,  cStances = stances }  = csSouthStance stances
stance Character { cDirection = East,   cStances = stances }  = csEastStance stances

gait Character { cDirection = North,  cGaits = gaits }  = cgNorthGait gaits
gait Character { cDirection = West,   cGaits = gaits }  = cgWestGait gaits
gait Character { cDirection = South,  cGaits = gaits }  = cgSouthGait gaits
gait Character { cDirection = East,   cGaits = gaits }  = cgEastGait gaits

displacement North  = (0, -1)
displacement West   = (-1, 0)
displacement South  = (0, 1)
displacement East   = (1, 0)

type CharacterActivity = StateT  Character
                                 (Cont (SF  (Terrain, Event Message)
                                            (Character, Event Message)))

type FieldActivity = FlatEmbeddedActivity  FieldParameters
                                           Controls
                                           OfflineIO
                                           ()

characterXlate = (16 *^) . uncurry vector2 . cPosition

act c0 activity = runCont k final
  where  k              = runStateT activity c0
         final ((), c)  = constant (c, NoEvent)

greeting :: (FieldParameters -> T.Text) -> CharacterActivity (Message, Terrain)
greeting speech = do  (m, t)  <- standing
                      c0      <- get
                      react c0 t m
  where  react c t m  = maybe (return (m, t)) id (reaction c t m)
         reaction     = stop <.> turn <.> move <.> speak
         stop         = reactStop standing
         turn         = reactTurn standing
         move         = reactMove walking standing
         speak c t m  = reactSpeak (speaking speech >> return (Done, t)) c t m


reactingForever ::  (  Character ->
                       Terrain ->
                       Message ->
                       CharacterActivity (Message, Terrain)) ->
                       CharacterActivity ()
reactingForever react = loop CharacterStop empty
  where  loop m0 t0 = do  c0        <- get
                          (m1, t1)  <- react c0 t0 m0
                          loop m1 t1

type Reaction a = Character ->
                  Terrain ->
                  Message ->
                  Maybe (CharacterActivity a)

(<.>) :: Reaction a -> Reaction a -> Reaction a
r1 <.> r2 = \c t m -> r1 c t m <|> r2 c t m

reactStop :: CharacterActivity a -> Reaction a
reactStop  action  c  t  CharacterStop  = Just action
reactStop  _       _  _  _              = Nothing

reactTurn :: CharacterActivity a -> Reaction a
reactTurn  action  c  t  (CharacterTurn d)  = Just (turning d >> action)
reactTurn  _       _  _  _                  = Nothing

reactMove :: CharacterActivity a -> CharacterActivity a -> Reaction a
reactMove  moving  stopping  c  t  (CharacterMove d)
   | blocked d c t                    = Just (turning d >> stopping)
   | d == cDirection c                = Just moving
   | otherwise                        = Just (turning d >> moving)
reactMove  _       _         _  _  _  = Nothing

reactSpeak :: CharacterActivity a -> Reaction a
reactSpeak  action  c  t  (CharacterSpeech k d)
   |  k == cIndex c          = Just $ do  turning (oppositeDirection d)
                                          action
reactSpeak  _       _  _  _  = Nothing

approaching :: (Double, Double) -> Terrain -> CharacterActivity ()
approaching pT@(xT, yT) t = do  c0 <- get
                                let pC@(xC, yC) = cPosition c0
                                unless (pC == pT) $ do
                                    let  dx   = xT - xC
                                         dy   = yT - yC
                                         dir  = if abs dy > abs dx
                                                      then  if dy > 0
                                                              then South
                                                              else North
                                                      else  if dx > 0
                                                              then East
                                                              else West
                                    turning dir
                                    unless (blocked dir c0 t) $ do
                                      (m, t')  <- walking
                                      case m of
                                        CharacterStop  -> return ()
                                        _              -> approaching pT t'

wandering ::  Time ->
              ((Double, Double), (Double, Double)) ->
              CharacterActivity (Message, Terrain)
wandering dt bounds = do
    tWait   <- state (randomR (3, 3 + dt))
    (m, t)  <- waiting tWait Done
    case m of
         Done   -> do  indexD  <- state (randomR (0, 3))
                       c       <- get
                       let  d   = toEnum indexD
                            d'  = if  blocked d c t ||
                                      facingBoundary (c { cDirection = d })
                                    then  oppositeDirection d
                                    else  d
                       do  turning d'
                           s <- state (randomR (1, 3))
                           loop s
         _      -> return (m, t)
  where  loop 1          = walking
         loop steps      = do  (m, t)  <- walking
                               c       <- get
                               if facingBoundary c
                                    then  return (m, t)
                                    else  react steps c t m
         loop :: Int -> CharacterActivity (Message, Terrain)
         react s c t m     = maybe (loop (s - 1)) id (reaction c t m)
            where  reaction  = stop <.> move <.> turn
                   stop      = reactStop (return (m, t))
                   move      = reactMove (loop (s - 1)) (return (m, t))
                   turn      = reactTurn (loop (s - 1))
         facingBoundary c  = x' < xMin || x' > xMax || y' < yMin || y' > yMax
            where  (x',  y')                     = (x + dx, y + dy)
                   (x,   y)                      = cPosition c
                   (dx,  dy)                     = displacement direction
                   direction                     = cDirection c
                   ((xMin, yMin), (xMax, yMax))  = bounds

glancing :: Time -> CharacterActivity (Message, Terrain)
glancing median = do  dt      <- state (randomR (1/2 * median, 3/2 * median))
                      (m, t)  <- waiting dt Done
                      case m of
                           Done  -> do  d <- state (randomR (0, 3))
                                        turning (toEnum d)
                           _     -> return ()
                      return (m, t)

looking :: CardinalDirection -> Time -> CharacterActivity (Message, Terrain)
looking dir median = do  dt      <- state (randomR (1/2 * median, 3/2 * median))
                         (m, t)  <- waiting dt Done
                         case m of
                             Done  -> turning dir
                             m     -> return ()
                         return (m, t)

speaking :: (FieldParameters -> T.Text) -> CharacterActivity ()
speaking speech = posting (FieldAction (stdLecture speech >>= stdWait))

posting :: Message -> CharacterActivity ()
posting m = do  c0 <- get
                let sf = constant (c0, Event m) &&& now ()
                lift (dSwont sf)

turning :: CardinalDirection -> CharacterActivity ()
turning = modify . pivot

waiting :: Time -> Message -> CharacterActivity (Message, Terrain)
waiting interval m0 = do  c0 <- get
                          let sf = proc (t, e) -> do
                               c     <- stand c0           -< t
                               done  <- after interval m0  -< ()
                               e'    <- notYet             -< e `lMerge` done
                               returnA -< ((c, e'), e' `attach` (c, t))
                          (m, (c, t)) <- lift (swont sf)
                          put c
                          return (m, t)

standing :: CharacterActivity (Message, Terrain)
standing = do  c0 <- get
               let sf = proc (t, e) -> do
                    c   <- stand c0  -< t
                    e'  <- notYet    -< e
                    returnA -< ((c, e'), e' `attach` (c, t))
               (m, (c, t)) <- lift (swont sf)
               put c
               return (m, t)

walking :: CharacterActivity (Message, Terrain)
walking = do  c0 <- get
              let  m0  = CharacterMove (cDirection c0)
                   sf  = walk c0 *** (notYet >>> hold m0) >>> arr report
              (r, c) <- lift (swont sf)
              put c
              return r
  where  report ((c, e), m) = ((c, e `tag` m), fmap (\(c, t) -> ((m, t), c)) e)
         report ::  ((Character, Event (Character, Terrain)), Message) ->
                    ((Character, Event Message),
                        Event ((Message, Terrain), Character))

stand :: Character -> SF Terrain Character
stand c0 = proc t -> do
    draw <- drawing -< ()
    returnA -< c0 { cDraw = draw, cOccupy = update }
  where  drawing  = stance c0
         update   = listen (round x) (round y) . occupy (round x) (round y)
         listen   = adjustElem (\te -> te { teInspect = inspect })
         inspect  = return . Event . CharacterSpeech index
         (x, y)   = cPosition c0
         index    = cIndex c0

walk :: Character -> SF Terrain (Character, Event (Character, Terrain))
walk c0 = proc t -> do
    timeup          <- timer      -< ()
    (draw, frozen)  <- animation  -< ()
    x               <- interpX    -< ()
    y               <- interpY    -< ()
    let c        = c0  {  cAnimation = frozen,
                          cDraw = draw,
                          cOccupy = update1 . update0,
                          cPosition = (x, y) }
        cF       = c   {  cPosition = (x1, y1),
                          cOccupy = update1 }
        update0  = occupy (round x0) (round y0)
        update1  = occupy (round x1) (round y1)
    returnA -< (c, timeup `tag` (cF, t))
  where
    direction         = cDirection c0
    KeepSF animation  = cAnimation c0
    (x0, y0)          = cPosition c0
    interpX           = timedInterp x0 x1 interval
    interpY           = timedInterp y0 y1 interval
    timer             = after interval ()
    (dx, dy)          = displacement direction
    (x1, y1)          = (x0 + dx, y0 + dy)
    interval          = 1.0 / cSpeed c0

wander interval rgen = runCont (k rgen) (const $ constant NoEvent)
  where
    k r = do
        let (wait, r')       = randomR (3.0, 3.0 + interval) r
            (walk, r'')      = randomR (interval - 0.25, interval + 0.25) r'
            (dirIndex, r''') = randomR (0 :: Int, 3 :: Int) r''
        cont . switchE $ after wait (CharacterMove (toEnum (dirIndex :: Int)))
        cont . switchE $ after walk CharacterStop
        k r'''

wanderRegion ((xMin, yMin), (xMax, yMax)) interval rgen = proc (c, t) -> do
    let (x, y) = cPosition c
        valid North = y > yMin
        valid West  = x > xMin
        valid South = y < yMax
        valid East  = x < xMax
        fD = bool <$> oppositeDirection <*> id <*> valid
        fM (CharacterMove d) = CharacterMove (fD d)
        fM x = x
        inside = foldl (&&) True (map valid [North, West, South, East])
    stop <- edgeTag CharacterStop -< not inside
    msg  <- wander interval rgen  -< ()
    returnA -< stop `lMerge` fmap fM msg
