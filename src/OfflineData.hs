module OfflineData where

import Control.Exception
import Control.Monad
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import SDL
import qualified SDL.Image as Image
import qualified SDL.Mixer as Mixer
import System.Environment.Executable
import System.FilePath.Posix
import System.IO.Error

import Field.MapName
import {-# SOURCE #-} Field.Terrain
import LabelName
import MusicName
import ProseName
import SpriteName
import SoundName
import StateClass
import TextUtil
import TileName

defaultTexturePath = "offline/textures/default.png"

spritePaths :: [(SpriteName, String)]
spritePaths = [(name, "offline/textures/sprites/" ++ show name ++ ".png") | name <- [toEnum 0 ..]]

glyphPaths :: [(Char, String)]
glyphPaths = [(toEnum code, "offline/textures/font/font-" ++ T.unpack (padInt 3 code) ++ ".png") | code <- [32..122]]

soundPaths :: [(SoundName, String)]
soundPaths = [(name, "offline/sounds/" ++ show name ++ ".wav") | name <- [toEnum 0 ..]]

musicPaths :: [(MusicName, String)]
musicPaths = [(name, "offline/sounds/" ++ show name ++ ".wav") | name <- [toEnum 0 ..]]

tilePaths :: [(TileName, String)]
tilePaths = [(name, "offline/textures/tiles/" ++ show name ++ ".png") | name <- [toEnum 0 ..]]

labelPaths :: [(LabelName, String)]
labelPaths = [(name, "offline/strings/labels/" ++ show name ++ ".txt") | name <- [toEnum 0 ..]]

prosePaths :: [(ProseName, String)]
prosePaths = [(name, "offline/strings/prose/" ++ show name ++ ".txt") | name <- [toEnum 0 ..]]

mapPaths :: [(MapName, String)]
mapPaths = [(name, "offline/maps/" ++ show name ++ ".txt") | name <- [toEnum 0 ..]]

data OfflineData = OfflineData {
    odRenderer :: Renderer,
    odGetSprite :: SpriteName -> Texture,
    odGetTile :: TileName -> Texture,
    odGetGlyph :: Char -> Texture,
    odGetSound :: SoundName -> Mixer.Chunk,
    odGetMusic :: MusicName -> Mixer.Chunk,
    odGetLabel :: LabelName -> T.Text,
    odGetProse :: ProseName -> T.Text,
    odGetTerrain :: MapName -> Terrain
}

loadOfflineData :: Renderer -> IO OfflineData
loadOfflineData renderer = do 
    exePath <- getExecutablePath
    let resourcesPath = takeDirectory (takeDirectory exePath) </> "Resources"

    defaultTexture <- Image.loadTexture renderer (resourcesPath </> defaultTexturePath)
    let defaultString = T.pack "DEFAULT"
        textureFail :: SomeException -> IO Texture
        textureFail e = return defaultTexture
        stringFail e = if isDoesNotExistError e then return defaultString else fail (show e)
    sprites <- mapM (handle textureFail . Image.loadTexture renderer) $ fmap (resourcesPath </>) (M.fromList spritePaths)
    tiles <- mapM (handle textureFail . Image.loadTexture renderer) $ fmap (resourcesPath </>) (M.fromList tilePaths)
    glyphs <- mapM (handle textureFail . Image.loadTexture renderer) $ fmap (resourcesPath </>) (M.fromList glyphPaths)
    sounds <- mapM Mixer.load $ fmap (resourcesPath </>) (M.fromList soundPaths)
    music <- mapM Mixer.load $ fmap (resourcesPath </>) (M.fromList musicPaths)
    labels <- mapM (fmap T.strip . handle stringFail . TIO.readFile) $ fmap (resourcesPath </>) (M.fromList labelPaths)
    prose <- mapM (fmap T.strip . handle stringFail . TIO.readFile) $ fmap (resourcesPath </>) (M.fromList prosePaths)
    maps <- mapM (liftM T.unpack . handle stringFail . TIO.readFile) $ fmap (resourcesPath </>) (M.fromList mapPaths)
    return OfflineData {
        odRenderer = renderer,
        odGetSprite = (maybe defaultTexture id) . (flip M.lookup sprites),
        odGetTile = (maybe defaultTexture id) . (flip M.lookup tiles),
        odGetGlyph = (maybe defaultTexture id) . (flip M.lookup glyphs),
        odGetSound = fromJust . (flip M.lookup sounds),
        odGetMusic = fromJust . (flip M.lookup music),
        odGetLabel = (maybe defaultString id) . (flip M.lookup labels),
        odGetProse = (maybe defaultString id) . (flip M.lookup prose),
        odGetTerrain = (maybe Field.Terrain.empty read) . (flip M.lookup maps)
    }

type OfflineIO = OfflineData -> IO ()

nullOut :: OfflineIO
nullOut = const (return ())

(>.=) f g = f >>= (. g) . (>>)

instance TextSource OfflineData where
    prose = flip odGetProse
    label = flip odGetLabel
