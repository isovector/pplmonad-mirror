{-# LANGUAGE Arrows, FlexibleContexts #-}

module Field.Avatar (animateAvatar) where

import Control.Monad.Cont
import Control.Monad.State
import FRP.Yampa hiding (left, right)

import Field.Character
import qualified Field.Menu
import Field.Terrain
import Message

animateAvatar ::  Character ->
                  SF (Terrain, Event Message) (Character, Event Message)
animateAvatar c0 = act c0 activity
  where  activity                     = reactingForever react
         moving    t                  = do  x <- walking
                                            traversing t
                                            return x
         react     c t Pause          = pausing >> standing
         react     c t AvatarInspect  = inspecting t >> standing
         react     c t m              = maybe standing id (reaction c t m)
         reaction                     = stop <.> turn <.> move
         stop                         = reactStop standing
         turn                         = reactTurn standing
         move      c t                = reactMove (moving t) standing c t

pausing :: CharacterActivity ()
pausing = posting (FieldAction (callCC (Field.Menu.selecting 0)))

traversing :: Terrain -> CharacterActivity ()
traversing t = do  c0    <- get
                   let  (x, y)  = cPosition c0
                        teM     = getElem (round x) (round y) t
                   msgE  <- maybe (return NoEvent) teTraverse teM
                   event (posting AvatarTraverse) posting msgE

inspecting :: Terrain -> CharacterActivity ()
inspecting t = do  c0    <- get
                   let  (x, y)    = cPosition c0
                        dir       = cDirection c0
                        (dx, dy)  = displacement dir
                        teM       = getElem (round x + dx) (round y + dy) t
                   msgE  <- maybe (return NoEvent) (flip teInspect dir) teM
                   event (return ()) posting msgE
