{-# LANGUAGE Arrows #-}

module Tuner where

-- requires BearRiver and Dunai
import Control.Monad.Trans.MStreamF (runReaderS, runReaderS_, readerS)
import FRP.Yampa as Y hiding (left, right)

transform automaton = readerS msf
  where
    msf = proc (dt, (x, input)) -> do
        output <- runReaderS (runReaderS automaton) -< (x, (dt, input))
        returnA -< output

instrumentation automaton = proc controls -> do
    (x, drawTuner) <- tuner -< (up controls, down controls, left controls, right controls)
    drawGame <- automaton -< (x, controls)
    returnA -< (\r -> drawGame r >> drawTuner r)

tuner = proc (up, down, left, right) -> do
    u <- Y.integral -< if up then 100 else 0
    d <- Y.integral -< if down then 100 else 0
    l <- Y.integral -< if left then 100 else 0
    r <- Y.integral -< if right then 100 else 0
    let x = -2 + (r - l)
    let y = 183 + (d - u)
    returnA -< ((x, y), const $ print (x, y))
