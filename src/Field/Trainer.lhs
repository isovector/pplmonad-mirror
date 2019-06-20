\begin{code}
{-# LANGUAGE Arrows #-}

module Field.Trainer where

import Control.Monad.State
import Control.Monad.Trans
import FRP.Yampa

import Field.Character
import Field.Terrain
import Lightarrow
import Message

defeated speech = let loop = greeting speech >> loop in loop

antagonizing ::  Double ->
                 ((Double, Double) -> Terrain -> CharacterActivity ()) ->
                 CharacterActivity () ->
                 CharacterActivity ()
antagonizing d provoked challenging = do  (m, t)  <- scouting d
                                          c       <- get
                                          react c t m
  where  react c t m  = maybe loop id $ (speak <.> alert <.> turn) c t m
         speak        = reactSpeak challenging
         alert        = reactAlert (\x t -> provoked x t >> challenging)
         turn         = reactTurn loop
         loop         = antagonizing d provoked challenging

reactAlert :: ((Double, Double) -> Terrain -> CharacterActivity a)
              -> Reaction a
reactAlert  action  c  t  (TrainerAlert k x)
   |  k == cIndex c          = Just (action x t)
reactAlert  _       _  _  _  = Nothing

scouting :: Double -> CharacterActivity (Message, Terrain)
scouting d = do  c0 <- get
                 let sf = proc (t, e) -> do
                      c   <- scout d c0  -< t
                      e'  <- notYet      -< e
                      returnA -< ((c, e'), e' `attach` (c, t))
                 (m, (c, t)) <- lift (swont sf)
                 put c
                 return (m, t)

scout :: Double -> Character -> SF Terrain Character
scout distance c0 = proc t -> do
    draw <- drawing -< ()
    returnA -< c0 { cDraw = draw, cOccupy = update }
  where  drawing    = stance c0
         update     = listen (round x) (round y) .
                      foldl (.) id (map sight sightline) .
                      occupy (round x) (round y)
         listen     = adjustElem (\te -> te { teInspect = inspect })
         inspect    = return . Event . CharacterSpeech index
         sight (x, y) = adjustElem (\te -> te { teTraverse = alert x y }) (round x) (round y)
         alert x y  = return (Event (TrainerAlert index (x, y)))
         sightline  = [(x + n * dx, y + n * dy) | n <- [1 .. distance]]
         (dx, dy)   = displacement direction
         (x, y)     = cPosition c0
         index      = cIndex c0
         direction  = cDirection c0
\end{code}
