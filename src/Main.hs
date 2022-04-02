{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Data.IORef
import Data.Time.Clock.System
import FRP.Yampa
import SDL hiding (copy, Stereo)
import SDL.Mixer hiding (quit)
import System.Exit
import System.IO
import System.Random

import MainAutomaton
import Controls
import OfflineData

desiredAudioSpec = Audio {
    audioFrequency = 44100,
    audioFormat = FormatS16_Sys,
    audioOutput = Stereo
}

screenScale = V2 4 4
windowDimensions = fmap round $ (*) <$> screenScale <*> V2 160 144

floatSeconds tS = fromIntegral (systemSeconds tS) + fromIntegral (systemNanoseconds tS) / 1000000000

main = do
    initializeAll

    window <- createWindow "Peoplemon" (defaultWindow { windowInitialSize = windowDimensions }){- { windowMode = Fullscreen } -}
    renderer <- createRenderer window (-1) defaultRenderer { rendererType = AcceleratedVSyncRenderer }
    rendererScale renderer $= screenScale
    rendererDrawBlendMode renderer $= BlendAlphaBlend
    setMouseLocationMode AbsoluteLocation
    cursorVisible $= False
    openAudio desiredAudioSpec 1024
    rgen <- getStdGen

    od <- loadOfflineData renderer
    tS <- getSystemTime
    let seconds = floatSeconds tS
    tRef <- newIORef seconds
    fpsRef <- newIORef (replicate 480 seconds)
    reactimate (return controlsDefault) (input tRef) (output fpsRef od) (mainAutomaton rgen od)
    quit

commands =
  [ (2.0, Just (controlsDefault { ctlUp = True })),
    (2.0, Just (controlsDefault { ctlRight = True })),
    (2.0, Just (controlsDefault { ctlLeft = True })),
    (2.0, Just (controlsDefault { ctlDown = True })) ]

input tRef _ = do
    es <- pollEvents
    when (any (isQuit . eventPayload) es) exitSuccess
    keyHeld <- getKeyboardState
    buttonHeld <- getMouseButtons
    mouse <- getAbsoluteMouseLocation
    seconds <- readIORef tRef
    tS <- getSystemTime
    let seconds'    = floatSeconds tS
        dt          = min 0.016 (seconds' - seconds)
        scaledMouse = flip div <$> (P $ fmap round screenScale) <*> mouse
        controls    = readControls keyHeld buttonHeld scaledMouse
        dt'                                   -- memory occupancy plummets if
            | keyHeld Scancode0 = 0           -- this feature is disabled
            | keyHeld Scancode1 = 0.25 * dt
            | keyHeld Scancode2 = 4 * dt
            | otherwise         = dt
    writeIORef tRef seconds'
    return (realToFrac dt, Just controls)

isQuit QuitEvent              = True
isQuit (WindowClosedEvent _)  = True
isQuit _                      = False

output fpsRef od _ action = do
    rendererDrawColor renderer $= V4 255 255 255 255
    clear renderer
    action od
    present renderer
    tS <- getSystemTime
    samples <- readIORef fpsRef
    let samples' = floatSeconds tS : init samples
        diffs = fst $ foldr (\s (diffs, prev) -> ((s - prev) : diffs, s)) ([], last samples) samples'
        mean = sum diffs / fromIntegral (length diffs)
    putStr $ "\r" ++ show (1 / mean)
    hFlush stdout
    writeIORef fpsRef samples'
    return False
  where
    renderer = odRenderer od
