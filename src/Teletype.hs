{-# LANGUAGE Arrows #-}

module Teletype (teletype, teletypeLine) where

import Control.Monad.Cont
import Data.List
import FRP.Yampa hiding (next)
import FRP.Yampa.Task

import Lightarrow
import Message

teletype :: Int -> String -> SF (Time, [Message]) ([String], [Message])
teletype width fullText = runCont (loop (PendingText fullText, [])) final
  where
    final ts = constant (ts, [Done])

    loop (Done, ts)           = return ts 
    loop (PendingText tp, ts) = typing (ts, tp) >>= loop

    typing (ts, tp) = cont . dSwitch $ proc (interval, inbox) -> do
        (t, outbox) <- newline  -< (interval, inbox)
        done        <- edgeJust -< find isLineEnd outbox
        let nTs = ts ++ [t]
        returnA -< ((nTs, MarginBell `sayUpon` (filterE isPending done)), done `attach` nTs)
      where
        newline = teletypeLine width tp

data LastWord = LastWord | NotLastWord

teletypeLine :: Int -> String -> SF (Time, [Message]) (String, [Message])
teletypeLine width fullText = runTask_ (nextWord (words fullText) width [])
  where
    nextWord [] _ prev         = mkTask $ constant prev `say` Done &&& never
    nextWord (w:ws) room prev
        | length w > room      = mkTask $ constant prev `say` (PendingText $ unwords (w:ws)) &&& never
        | null ws              = nextGlyph w LastWord prev >>= nextWord [] 0
        | otherwise            = nextGlyph w NotLastWord prev >>= nextWord ws (room - length w - 1)
    nextGlyph [] LastWord prev = mkTask $ interval prev
    nextGlyph [] _        prev = mkTask $ interval (prev ++ " ")
    nextGlyph (g:gs) last prev = (mkTask $ interval (prev ++ [g])) >>= nextGlyph gs last
    interval text = mute (constant text) &&& (arr fst >>> afterInput text)

isPending (PendingText _) = True
isPending _ = False

isDone Done = True
isDone _    = False

isLineEnd = (||) . isPending >>= (. isDone)
