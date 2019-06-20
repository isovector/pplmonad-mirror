{-# LANGUAGE Arrows, OverloadedStrings, ViewPatterns #-}

module Field.Editor (fieldEditor) where

import Data.Foldable
import Data.List
import qualified Data.Map as M
import qualified Data.Text as T
import FRP.Yampa
import FRP.Yampa.Geometry

import Controls
import ControlsMaps
import Field.Character
import Field.MapName
import Field.Output
import Field.Terrain
import Field.TerrainElements
import Field.TerrainElementExpression
import LabelName
import Lightarrow
import Message
import MusicName
import OfflineData
import Output
import Ppmn.Species
import SpriteName
import TileName

data EditorSignal = EditorSignal {
    esMouse :: (Int, Int),
    esCommand :: Event Message,
    esXlation :: Vector2 Double,
    esTerrain :: Terrain
}

fieldEditor = proc (controls, xlation, terrain) -> do
    command       <- editorControl -< controls
    (out, update) <- editor        -< EditorSignal (ctlMouse controls) command xlation terrain
    returnA -< (out, update terrain)

editor = selfAlternate closed selfKeeper (id, keep $ opened id)
  where
    closed (update, frozen) = constant (nullOut, update) &&& arr ((`tag` frozen) . listen)
    listen es = filterE isOpen (esCommand es)
    opened update0 = proc es@(EditorSignal (x, y) command xlation terrain) -> do
        let mouseDraw od = do
                drawRectangle 3 1 Black (Point2 (fromIntegral (x - 1)) (fromIntegral y)) od
                drawRectangle 1 3 Black (Point2 (fromIntegral x) (fromIntegral (y - 1))) od
        (toolDraw, update) <- toolbox tools update0 -< es
        let mapDraw = drawDebugTerrain (update terrain) xlation
            posDraw = drawRectangle 32 8 White (Point2 0 136) >.= drawTextScaled 0.5 (T.pack (show $ tileUnderPixel xlation x y)) (Point2 0 138)
            draw = mapDraw >.= toolDraw >.= mouseDraw >.= posDraw
        returnA -< ((draw, update), filterE isClose command `tag` update)
    tools = [ basicTool, exhibitTool, portalTool, habitatTool, eraseTool, clearTool, backgroundTool ] ++ tools

toolbox (t:ts) update0 = toolChanger `switch` id
  where
    toolChanger = proc es -> do
        (out, update) <- t update0 -< es
        command'      <- notYet    -< esCommand es
        let next = filterE isToolNext command'
        returnA -< ((out, update), next `tag` toolbox ts update)

tool' effector shifter options0 update0 = selfAlternate manip config (options0, update0)
  where
    manip = manipulation' effector (\(x, y) -> y < 4)
    config = configuration' shifter (\(x, y) -> y < 24)

tool effector shifter options0 update0 = selfAlternate manip config (update0, options0)
  where
    manip = manipulation effector (\(x, y) -> y < 4)
    config = configuration shifter (\(x, y) -> y < 24)

manipulation' effector isOverBar (options0, update0) = proc es -> do
    done                   <- iEdge False               -< isOverBar (esMouse es)
    (out, options, update) <- effector options0 update0 -< es
    returnA -< ((out, update), done `tag` (options, update))

configuration' shifter isOverBar (options0, update0) = proc es -> do
    done                       <- iEdge False                -< not (isOverBar (esMouse es))
    (setDraw, options, update) <- shifter options0 update0   -< es { esTerrain = update0 (esTerrain es) }
    savDraw                    <- saver 16 16 (Point2 144 0) -< es { esTerrain = update0 (esTerrain es) }
    returnA -< ((drawBg >.= setDraw >.= savDraw, update), done `tag` (options, update))
  where
    drawBg = drawRectangle 160 24 (Translucent White) (Point2 0 0)

manipulation effector isOverBar (update0, options) = proc es -> do
    done          <- iEdge False              -< isOverBar (esMouse es)
    (out, update) <- effector options update0 -< es
    returnA -< ((out, update), done `tag` (update, options))

configuration shifter isOverBar (update, options0) = proc es -> do
    done               <- iEdge False                -< not (isOverBar (esMouse es))
    (setDraw, options) <- shifter options0           -< es { esTerrain = update (esTerrain es) }
    savDraw            <- saver 16 16 (Point2 144 0) -< es { esTerrain = update (esTerrain es) }
    returnA -< ((drawBg >.= setDraw >.= savDraw, update), done `tag` (update, options))
  where
    drawBg = drawRectangle 160 24 (Translucent White) (Point2 0 0)

saver width height position = proc es -> do
    let save = hotClick (inBox width height position) es
        path = maybe "last-edited.txt" id (lookup Field.MapName.LastEdited mapPaths)
    returnA -< event buttonUp (const ((>> writeFile path (show (esTerrain es))) . buttonDown)) save
  where
    buttonUp = drawSaveUp width height position
    buttonDown = drawSaveDown width height position

basicTool = tool' effector shifter (toEnum 0, toEnum 0, False)
  where
    make (tile, orientation, collides) = basic tile orientation collides
    quickShift (tile0, orientation0, collides0) = proc es -> do
        let command = esCommand es
            next = filterE isOptionNext command
            prev = filterE isOptionPrev command
        rec
            tiles <- dHold tiles0 -< next `tag` (tail tiles) `lMerge` prev `tag` (tiles !! (length tileEnums - 1) : tiles)
        returnA -< (head tiles, orientation0, collides0)
      where
        tileEnums = [toEnum 0 ..]
        allTiles = tileEnums ++ allTiles
        tiles0 = [tile0 ..] ++ allTiles
    effector options0 update0 = proc es -> do
        options       <- quickShift options0  -< es
        (out, update) <- tileEffector' (stamper' make) update0 -< (options, es)
        returnA -< (out, options, update)
    shifter (tile0, orientation0, collides0) update0 = proc es -> do
        rec
            let text = show (make settings)
                (_ : (wt, ht, pt) : (wo, ho, po) : (wc, hc, pc) : _) = boxes text
                tClick = hotClick (inBox wt ht pt) es
                oClick = hotClick (inBox wo ho po) es
                cClick = hotClick (inBox wc hc pc) es
                unclick = filterE isUnclick (esCommand es)
            tUpdate    <- dragger1DClamp 0 (length tileEnums - 1) -< es { esCommand = tClick `lMerge` unclick }
            tIndex     <- iPre 0 -< tUpdate 0
            orients  <- dHold orients0  -< oClick `tag` (tail orients)
            collides <- dHold collides0 -< cClick `tag` (not collides)
            let settings = (tiles0 !! tIndex, head orients, collides)
        let display od = do
                drawRectangle wt ht (Light Green) pt od
                drawRectangle wo ho (Light Green) po od
                drawRectangle wc hc (Light Green) pc od
                drawEditorText (T.pack text) od
        returnA -< (display, settings, update0)
      where
        tileEnums = [toEnum 0 ..]
        tiles0 = [tile0 ..] ++ tileEnums ++ tiles0
        orients0 = [orientation0 ..] ++ [toEnum 0 ..] ++ orients0

hotClick inPlace (EditorSignal (x, y) command _ _) = gate click (inPlace mouse')
  where
    click = filterE isClick command
    mouse' = Point2 (fromIntegral x) (fromIntegral y)

eraseTool = tool effector shifter ()
  where
    effector = tileEffector effect
    effect _ = selector deleteElem draw
    draw (x, y) = drawTextScaled 0.5 "Erase" (Point2 (fromIntegral x) (fromIntegral y))
    shifter _ = constant (drawEditorText "Erase", ())

habitatTool = tool effector shifter (0.25, ((0, 3), 1)) 
  where
    effector = tileEffector effect
    effect options = selector (update options) (drawSpriteCursor (Translucent White) Wifi)
    update (rate, ((nIndex, level), pop)) = adjustElem (habitat rate [(getPpmn nIndex level, pop)])
    getPpmn nIndex = ppmnByName (names !! nIndex)
    names = [Ignoloof, Slidek, Blamotage, Unner, Incub]
    nextIndex = (`mod` length names) . (+ 1)
    shifter options0@(rate0, ((nIndex0, level0), pop0)) = proc es -> do
        rec
            rate  <- iPre rate0  -< fromIntegral (rUpdate (round $ rate0 * 100)) / 100
            level <- iPre level0 -< lUpdate level0
            pop   <- iPre pop0   -< pUpdate pop0
            let name = names !! nIndex
                text = intercalate " " ["Habitat", show rate, show name, show level, show pop]
                (_ : (wr, hr, pr) : (wn, hn, pn) : (wl, hl, pl) : (wp, hp, pp) : _) = boxes text
                rClick = hotClick (inBox wr hr pr) es
                nClick = hotClick (inBox wn hn pn) es
                lClick = hotClick (inBox wl hl pl) es
                pClick = hotClick (inBox wp hp pp) es
                unclick = filterE isUnclick (esCommand es)
            rUpdate <- dragger1DClamp 0 100 -< es { esCommand = rClick `lMerge` unclick }
            lUpdate <- dragger1DClamp 1 99  -< es { esCommand = lClick `lMerge` unclick }
            pUpdate <- dragger1DMin 1       -< es { esCommand = pClick `lMerge` unclick }
            nIndex  <- dAccumHold nIndex0   -< nClick `tag` nextIndex
        let options = (rate, ((nIndex, level), pop))
            display od = do
                drawRectangle wr hr (Light Green) pr od
                drawRectangle wn hn (Light Green) pn od
                drawRectangle wl hl (Light Green) pl od
                drawRectangle wp hp (Light Green) pp od
                drawEditorText (T.pack text) od
        returnA -< (display, options)

portalTool = tool effector shifter (toEnum 0, (0, 0), False)
  where
    effector = tileEffector effect
    effect options = selector (update options) cursor
    update (destination, position, fade) = adjustElem (portal destination position fade)
    cursor = drawTileCursor (Translucent White) HouseDoor
    shifter (destination0, position0, fade0) = proc es -> do
        rec
            pos  <- iPre position0    -< update position0
            let text = intercalate " " ["Portal", show (head dests), show pos, show fade]
                (_ : (wd, hd, pd) : (wp, hp, pp) : (wf, hf, pf) : _) = boxes text
                destClick = hotClick (inBox wd hd pd) es
                posClick = hotClick (inBox wp hp pp) es
                fadeClick = hotClick (inBox wf hf pf) es
                unclick = filterE isUnclick (esCommand es)
            dests  <- dHold dests0 -< destClick `tag` tail dests
            fade   <- dHold fade0  -< fadeClick `tag` (not fade)
            update <- dragger2D    -< es { esCommand = posClick `lMerge` unclick }
        let settings = (head dests, pos, fade)
            display od = do
                drawRectangle wd hd (Light Green) pd od
                drawRectangle wp hp (Light Green) pp od
                drawRectangle wf hf (Light Green) pf od
                drawEditorText (T.pack text) od
        returnA -< (display, settings)
      where
        dests0 = [destination0 ..] ++ [toEnum 0 ..] ++ dests0

exhibitTool = tool effector shifter (toEnum 0, toEnum 0)
  where
    effector = tileEffector effect
    effect options = selector (update options) cursor
    update (direction, prose) = adjustElem (exhibit (M.singleton direction prose))
    cursor = drawTileCursor (Translucent White) TextBoxTopLeft
    shifter (direction0, prose0) = proc es -> do
        rec
            let text = intercalate " " ["Exhibit", show (head dirs), show (head proses)]
                (_ : (wd, hd, pd) : (wp, hp, pp) : _) = boxes text
                dirClick = hotClick (inBox wd hd pd) es
                proseClick = hotClick (inBox wp hp pp) es
            dirs   <- dHold dirs0    -< dirClick `tag` tail dirs
            proses <- dHold proses0  -< proseClick `tag` tail proses
        let settings = (head dirs, head proses)
            display od = do
                drawRectangle wd hd (Light Green) pd od
                drawRectangle wp hp (Light Green) pp od
                drawEditorText (T.pack text) od
        returnA -< (display, settings)
      where
        dirs0 = [direction0 ..] ++ [toEnum 0 ..] ++ dirs0
        proses0 = [prose0 ..] ++ [toEnum 0 ..] ++ proses0

dragger2D = selfAlternate listening dragging id
  where
    getClick (EditorSignal (x, y) command _ _) = filterE isClick command `tag` (x, y)
    listening update = (constant update) &&& (arr $ (`attach` update) . getClick)
    dragging ((x, y), update0) = proc es -> do
        let (x', y') = esMouse es
            update = (\(px, py) -> (px + x' - x, py + y' - y)) . update0
            unclick = filterE isUnclick (esCommand es) `tag` update
        returnA -< (update, unclick)

dragger1DClamp minimum maximum = dragger1D (min maximum . max minimum)
dragger1DMin minimum = dragger1D (max minimum)
dragger1D modify = selfAlternate listening dragging id
  where
    getClick (EditorSignal (x, y) command _ _) = filterE isClick command `tag` (x, y)
    listening update = (constant update) &&& (arr $ (`attach` update) . getClick)
    dragging ((x, _), update0) = proc es -> do
        let (x', _) = esMouse es
            update = (\px -> modify $ px + x' - x) . update0
            unclick = filterE isUnclick (esCommand es) `tag` update
        returnA -< (update, unclick)

clearTool = tool clearEffector clearShifter ()
  where
    effect = selector (\_ _ _ -> empty) (drawSpriteCursor (Translucent White) ElectrodeFront)
    clearEffector = tileEffector (const $ effect)
    clearShifter options = constant (drawEditorText "Clear all", options)

backgroundTool = tool effector shifter 0
  where
    effector = tileEffector effect
    effect index = selector (\_ _ -> setBg index) (drawSpriteCursor (Translucent White) SpriteName.Viacom)
    setBg index terrain = terrain { tBackgroundColor = colors !! index }
    colors = [Black, Dark (Dark White), Dark White, White]
    nextIndex = (`mod` length colors) . (+ 1)
    shifter colorIndex0 = proc es -> do
        rec
            let text = "Set BG to " ++ show (colors !! colorIndex)
                (_ : _ : _ : lastBoxes) = boxes text
                w = sum (map (\(w, _, _) -> w + 4) lastBoxes)
                (_, h, p) = head lastBoxes
                cClick = hotClick (inBox w h p) es
            colorIndex <- dAccumHold colorIndex0 -< cClick `tag` nextIndex
        let display = drawRectangle w h (Light Green) p >.= drawEditorText (T.pack text)
        returnA -< (display, colorIndex)

tileEffector' updater update0 = proc (options, EditorSignal (x, y) command xlate terrain) -> do
    let (x', y')  = tileUnderPixel xlate x y
        tilePos   = Point2 (fromIntegral (x' * 16)) (fromIntegral (y' * 16))
        tileDraw  = withXlation xlate (drawRectangle 16 16 (Translucent (Dark Green))) tilePos
        select    = filterE isClick command `tag` TerrainElementSelect x' y'
    (toolDraw, update) <- updater update0 -< (options, select, terrain)
    returnA -< (tileDraw >.= (toolDraw (x, y)), update)

tileEffector updater options update0 = proc (EditorSignal (x, y) command xlate terrain) -> do
    let (x', y')  = tileUnderPixel xlate x y
        tilePos   = Point2 (fromIntegral (x' * 16)) (fromIntegral (y' * 16))
        tileDraw  = withXlation xlate (drawRectangle 16 16 (Translucent (Dark Green))) tilePos
        select    = filterE isClick command `tag` TerrainElementSelect x' y'
    (toolDraw, update) <- updater options update0 -< (select, terrain)
    returnA -< (tileDraw >.= (toolDraw (x, y)), update)

stamper' make = selector' make setElem drawTerrainCursor

stamper te = selector (setElem te) (drawTerrainCursor te)

selector' make elemUpdate draw update0 = proc (options, command, _) -> do
    let te = make options
        select (TerrainElementSelect x y) = Just $ (elemUpdate te x y .)
        select _ = Nothing
    update <- accumHold update0 -< mapFilterE select command
    returnA -< (draw te, update)

selector elemUpdate draw update0 = proc (command, _) -> do
    update <- accumHold update0 -< mapFilterE select command
    returnA -< (draw, update)
  where
    select (TerrainElementSelect x y) = Just $ (elemUpdate x y .)
    select _ = Nothing

isClick EditorClick = True
isClick _           = False

isUnclick EditorUnclick = True
isUnclick _             = False

isClose EditorClose = True
isClose _           = False

isOpen EditorOpen = True
isOpen _          = False

isToolNext EditorToolNext = True
isToolNext _              = False

isOptionNext EditorOptionNext = True
isOptionNext _                = False

isOptionPrev EditorOptionPrevious = True
isOptionPrev _                    = False

isSelect (TerrainElementSelect _ _) = True
isSelect _                          = False

drawEditorText text = drawTextScaled 0.5 text (Point2 8 8)

drawSpriteCursor modulation sprite (x, y) od = do
    let position = Point2 (fromIntegral x) (fromIntegral y)
        offset = vector2 0.5 0.5
    drawSpriteExplicit Original 1.0 modulation offset sprite position od

drawTileCursor modulation tile (x, y) od = do
    let position = Point2 (fromIntegral x) (fromIntegral y)
        offset = vector2 0.5 0.5
    drawTileExplicit modulation offset tile position Original od

drawTerrainCursor (TerrainElement { teTile = (tile, orientation) }) (x, y) od = do
    let texture = odGetTile od tile
        position = Point2 (fromIntegral x - 8 ) (fromIntegral y - 8)
        offset = vector2 0 0
    drawRectangle 16 16 (Translucent $ Translucent White) position od
    drawTileExplicit (Translucent White) offset tile position orientation od

drawSaveUp width height = drawSave (drawRectangle width height (Dark Green))
drawSaveDown width height = drawSave (drawRectangle width height Green)

drawSave bg position@(Point2 bx by) = snd $ foldl' addLetter (0, bg position) (T.chunksOf 1 "Save")
  where
    addLetter (k, out) c = (k + 1, out >.= drawLetter c k)
    drawLetter c k = drawTextScaled 0.5 c (Point2 (bx + 2 + 3 * k) (by + 2 + 3 * k))

debugMark (teExpr -> (Basic _ _ True))  = drawRectangleFrame 16 16 (Translucent Magenta)
debugMark (teExpr -> (Basic TileName.Blank _ _)) = drawRectangleFrame 16 16 (Translucent Blue)
debugMark (teExpr -> (Portal _ _ _ _))  = drawRectangleFrame 16 16 (Translucent (Dark Cyan))
debugMark (teExpr -> (Habitat _ _ _ ))  = drawRectangleFrame 16 16 (Translucent Red)
debugMark _                             = const nullOut

drawDebugTerrain terrain xlation = foldl' (>.=) nullOut $ mapWithIndices (uncurry tiler) elements
  where
    elements = tElements terrain
    tiler r c element = withXlation xlation (debugMark element) (translate r c)
    translate r c = Point2 (fromIntegral $ 16 * c) (fromIntegral $ 16 * r)

boxes = reverse . fst . foldl' addBox ([], 8) . widths
  where
    addBox (bs, x) w = ((w, 4, (Point2 x 8)) : bs, x + w + 4)
    widths = map ((* 4.0) . fromIntegral . length) . words

inBox width height (Point2 bx by) (Point2 x y) = inX && inY
  where
    inX = bx < x && x < bx + width
    inY = by < y && y < by + height
