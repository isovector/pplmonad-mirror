{-# LANGUAGE Arrows, OverloadedStrings #-}

module Output where

import Control.Monad
import Control.Concurrent
import Data.Bool
import qualified Data.Text as T
import Data.Word
import SDL hiding (identity, Event)
import SDL.Mixer
import SDL.Raw.Mixer
import FRP.Yampa
import Data.Point2
import Data.Vector2

import MusicName
import OfflineData
import SpriteName
import SoundName
import TileName

data Color = Black
           | Blue
           | Cyan
           | Dark Color
           | Green
           | Light Color
           | Magenta
           | Red
           | Translucent Color
           | White
           | Yellow
    deriving (Read, Show)

colorRGBA :: Color -> V4 Word8
colorRGBA Black               = V4 0 0 0 255
colorRGBA Blue                = V4 0 0 255 255
colorRGBA Cyan                = V4 0 255 255 255
colorRGBA (Dark color)        = V4 (darken r) (darken g) (darken b) a
  where
    darken = (`div` 2)
    V4 r g b a = colorRGBA color
colorRGBA Green               = V4 0 255 0 255
colorRGBA (Light color)       = V4 (lighten r) (lighten g) (lighten b) a
  where
    lighten = (* 2)
    V4 r g b a = colorRGBA color
colorRGBA Magenta             = V4 255 0 255 255
colorRGBA Red                 = V4 255 0 0 255
colorRGBA (Translucent color) = V4 r g b (a `div` 2)
  where
    V4 r g b a = colorRGBA color
colorRGBA White               = V4 255 255 255 255
colorRGBA Yellow              = V4 255 255 0 255

clearScreen color od = do
    oldColor <- get (rendererDrawColor renderer)
    rendererDrawColor renderer $= colorRGBA color
    clear renderer
    rendererDrawColor renderer $= oldColor
  where
    renderer = odRenderer od

drawSprite = drawSpriteExplicit Original 1.0 White (vector2 0 0)

drawSpriteOriented orient = drawSpriteExplicit orient 1.0 White (vector2 0 0)

drawSpriteExplicit :: DrawOrientation -> Double -> Color -> Vector2 Double -> SpriteName -> Point2 Double -> OfflineData -> IO ()
drawSpriteExplicit orient scale modulation offset name (Point2 x y) od = do
    d <- destination
    copyMod orient (colorRGBA modulation) renderer texture Nothing d
  where
    renderer = odRenderer od
    texture = odGetSprite od name
    destination = do
        info <- queryTexture texture
        let offset' = V2 (vector2X offset) (vector2Y offset)
            position = P $ round <$> (V2 x y - (offset' * (fromIntegral <$> dimensions)))
            dimensions = V2 (round $ scale * fromIntegral (textureWidth info)) (round $ scale * fromIntegral (textureHeight info))
        return (Just $ Rectangle position dimensions)

copyMod orient (V4 r g b a) renderer texture source dest = do
    oldColor <- get (textureColorMod texture)
    oldAlpha <- get (textureAlphaMod texture)
    textureColorMod texture $= V3 r g b
    textureAlphaMod texture $= a
    let flipX = orient == FlipX || orient == FlipBoth
        flipY = orient == FlipY || orient == FlipBoth
        rotate = case orient of
            TurnL -> -90
            TurnR -> 90
            _       -> 0
    copyEx renderer texture source dest rotate Nothing (V2 flipX flipY)
    textureColorMod texture $= oldColor
    textureAlphaMod texture $= oldAlpha

data DrawOrientation = Original | FlipX | FlipY | FlipBoth | TurnL | TurnR
    deriving (Enum, Eq, Read, Show)

drawTile = drawTileExplicit White (vector2 0 0)

drawTileExplicit :: Color -> Vector2 Double -> TileName -> Point2 Double -> DrawOrientation -> OfflineData -> IO ()
drawTileExplicit _ _ TileName.Blank _ _ _ = return ()
drawTileExplicit modulation offset name (Point2 x y) orient od = do
    d <- destination
    copyMod orient (colorRGBA modulation) renderer texture Nothing d
  where
    renderer = odRenderer od
    texture = odGetTile od name
    destination = do
        info <- queryTexture texture
        let offset' = V2 (vector2X offset) (vector2Y offset)
            position = P $ round <$> (V2 x y - (offset' * (fromIntegral <$> dimensions)))
            dimensions = V2 (textureWidth info) (textureHeight info)
        return (Just $ Rectangle position dimensions)

drawTiledBg (Point2 x y) dimensions tiles od = foldMap draw (zip indices tiles)
  where
    draw (index, (tile, orient)) = drawTile tile (positionAtIndex index) orient od
    positionAtIndex (kx, ky) = Point2 (x + 16 * fromIntegral kx) (y + 16 * fromIntegral ky)
    indices = [(x, y) | y <- ys, x <- xs]
    xs = [0 .. tx - 1]
    ys = [0 .. ty - 1]
    (tx, ty) = dimensions

drawText :: T.Text -> Point2 Double -> OfflineData -> IO ()
drawText text (Point2 x y) od = foldMap (uncurry draw) typesetting
  where
    glyph = odGetGlyph od
    renderer = odRenderer od
    draw character offset = copy renderer (glyph character) Nothing (destination offset)
    destination offset = Just $ Rectangle (P $ V2 (round x + offset) (round y)) (V2 8 8)
    typesetting = snd $ T.foldr setChar (fromIntegral $ 8 * (T.length text - 1), []) text
    setChar c (offset, zipped) = (offset - 8, (c, offset) : zipped)

drawTextScaled factor text (Point2 x y) od = do
    scale <- get (rendererScale (odRenderer od))
    rendererScale (odRenderer od) $= (* realToFrac factor) <$> scale
    drawText text (Point2 (x / factor) (y / factor)) od
    rendererScale (odRenderer od) $= scale

sentenceCase = (uncurry T.append) <<< (first T.toUpper) . (T.splitAt 1)

sentence :: Char -> [T.Text] -> T.Text
sentence punctuation = sentenceCase . (flip T.snoc punctuation) . T.intercalate " "

drawRectangle :: Double -> Double -> Color -> Point2 Double -> OfflineData -> IO ()
drawRectangle width height color (Point2 x y) od = do
    oldColor <- get (rendererDrawColor renderer)
    rendererDrawColor renderer $= colorRGBA color
    fillRect renderer (Just $ Rectangle corner diagonal)
    rendererDrawColor renderer $= oldColor
  where
    renderer = odRenderer od
    corner = P (V2 (round x) (round y))
    diagonal = V2 (round width) (round height)

drawRectangleFrame width height color (Point2 x y) od = do
    oldColor <- get (rendererDrawColor renderer)
    rendererDrawColor renderer $= colorRGBA color
    drawRect renderer (Just $ Rectangle corner diagonal)
    rendererDrawColor renderer $= oldColor
  where
    renderer = odRenderer od
    corner = P (V2 (round x) (round y))
    diagonal = V2 (round width) (round height)

drawLabel name position od = drawText (odGetLabel od name) position od
drawProse name position od = drawText (odGetProse od name) position od

playSound :: SoundName -> OfflineData -> IO ()
playSound name od = halt 0 >> playOn 0 Once (odGetSound od name) >> return ()

playSoloSound :: SoundName -> OfflineData -> IO ()
playSoloSound name od = do
    setVolume 0 (1 :: SDL.Mixer.Channel)
    playSound name od
    whenChannelFinished (const $ do
        setVolume 128 (1 :: SDL.Mixer.Channel)
        whenChannelFinished (const $ return ()))

playRepeatingMusic :: MusicName -> OfflineData -> IO ()
playRepeatingMusic name od = do
    mLast <- playedLast 2
    let mIO = mLast >>= return . continue
    maybe play id mIO
    return ()
  where
    continue = bool <$> (const play) <*> ignore <*> (== music)
    play = changeRepeatingMusic name od
    music = odGetMusic od name
    ignore = const (return ())

restartRepeatingMusic :: MusicName -> OfflineData -> IO ()
restartRepeatingMusic name od = do
    playOn 2 Once (odGetMusic od name)
    halt 2
    setVolume 128 (1 :: SDL.Mixer.Channel)
    setVolume 128 (2 :: SDL.Mixer.Channel)
    playOn 1 Forever (odGetMusic od name)
    return ()

changeRepeatingMusic :: MusicName -> OfflineData -> IO ()
changeRepeatingMusic name od = forkOS action >> return ()
  where
    action = do
        vol <- getVolume (2 :: SDL.Mixer.Channel)
        setVolume (vol - 1) (2 :: SDL.Mixer.Channel) -- janky semaphore
        playOn 2 Once (odGetMusic od name)
        halt 2
        fadeOut 1500 1
        threadDelay 1550000 -- playOn fails silently if channel is still fading
        vol <- getVolume (2 :: SDL.Mixer.Channel)
        setVolume (vol + 1) (2 :: SDL.Mixer.Channel) -- janky semaphore again
        when (vol == 127) $ do
                setVolume 128 (1 :: SDL.Mixer.Channel)
                playOn 1 Forever (odGetMusic od name)
                return ()

playMusicWithIntro introName length mainName od = forkOS action >> return ()
  where
    mainMusic = odGetMusic od mainName
    playMain pending = do
        when (pending == mainMusic) $ do
                halt 1
                setVolume 128 (1 :: SDL.Mixer.Channel)
                setVolume 128 (2 :: SDL.Mixer.Channel)
                playOn 1 Forever (odGetMusic od mainName)
                return ()
    action = do
        playOn 2 Once (odGetMusic od mainName)
        halt 2
        setVolume 128 (1 :: SDL.Mixer.Channel)
        setVolume 128 (2 :: SDL.Mixer.Channel)
        playOn 1 Once (odGetMusic od introName)
        threadDelay (round $ 1000000 * length)
        mLast <- playedLast 2
        maybe (return ()) playMain mLast

stopMusic od = do
    setVolume 128 (1 :: SDL.Mixer.Channel)
    setVolume 128 (2 :: SDL.Mixer.Channel)
    halt 1

fadeOutMusic od = forkOS action >> return ()
  where
    action = do
        vol <- getVolume (2 :: SDL.Mixer.Channel)
        setVolume (vol - 1) (2 :: SDL.Mixer.Channel) -- janky semaphore
        halt 2
        fadeOut 1500 1
        threadDelay 1550000 -- playOn fails silently if channel is still fading
        vol <- getVolume (2 :: SDL.Mixer.Channel)
        setVolume 128 (1 :: SDL.Mixer.Channel)
        return ()

soundTrigger :: SoundName -> SF (Event a) (OfflineData -> IO ())
soundTrigger name = arr player
  where
    player NoEvent = const $ return ()
    player (Event _) = playSound name

ballisticArc (Point2 x0 y0) (Point2 x1 y1) height t = Point2 x y
  where
    x = t * x1 + (1 - t) * x0
    y = a * t' ** 2 + b * t' + c
    t' = t - (1 / 2)
    a = 4 * height
    b = y1 - y0
    c = (y0 + y1) / 2 - height
