{-# LANGUAGE Arrows, FlexibleContexts, Rank2Types, NoMonomorphismRestriction #-}

module Lightarrow where

import Control.Monad.Cont
import Control.Monad.RWS
import Data.List
import FRP.Yampa
import FRP.Yampa.Geometry
import FRP.Yampa.Task

-- SF library

newtype KeepSF a b = KeepSF { keeper :: SF a (b, KeepSF a b) }

keep :: SF a b -> KeepSF a b
keep sf = KeepSF $ kSwitch (sf &&& (constant $ keep sf)) detect continue
  where
    detect = constant (Event ()) >>> notYet
    continue frozen _ = next
      where
        next = kSwitch (frozen >>> (second $ constant (KeepSF next))) detect continue 

selfKeeper :: KeepSF a (b, Event c) -> SF a (b, Event (c, KeepSF a (b, Event c)))
selfKeeper (KeepSF sf) = proc a -> do
    ((b, done), frozen) <- sf -< a
    returnA -< (b, (done `attach` frozen))

selfSequence :: [SF a (b, Event c)] -> SF a b
selfSequence = runTask_ . mapM_ mkTask

triggerSequence :: [SF a b] -> SF (a, Event ()) b
triggerSequence = runTask_ . mapM_ (mkTask . (*** notYet))

--timedSequence interval = runTask_ . mapM_ (mkTask . (&&& (after interval ())))
timedSequence interval sfs = runCont k (error "timedSequence terminated")
  where k = mapM_ (dSwont . (&&& (after interval ()))) sfs

timedUpdateSequence :: Time -> a -> [SF a a] -> SF a a
timedUpdateSequence interval = (runTask_ .) . foldM receiver 
  where
    receiver result phase = mkTask $ proc _ -> do
        out    <- phase             -< result
        timeup <- after interval () -< ()
        returnA -< (out, timeup `tag` out)

selfAlternate :: (d -> SF a (b, Event c)) -> (c -> SF a (b, Event d)) -> d -> SF a b
selfAlternate f g = runTask_ . alternator
  where
    alternator = mkTask . f >=> mkTask . g >=> alternator

tackOn :: SF a a -> SF (a, Event (SF a a)) a
tackOn sf = (first sf) `switch` (\ k -> tackOn (sf >>> k))

taskMod :: (d -> (b, Event e)) -> (SF a (b, Event c) -> SF a d) -> Task a b c -> Task a b e
taskMod collect modify = mkTask . (arr collect <<<) . modify . taskToSF

afterInput :: a -> SF Time (Event a)
afterInput x = (time &&& identity) >>> (arr $ uncurry (>)) >>> edge >>> arr (`tag` x)

flasher :: a -> a -> SF Time a
flasher on off = constant (on, off) &&& identity >>> flipper

flipper :: SF ((a, a), Time) a
flipper = runCont loop (arr (fst . fst))
  where
    loop = l >> r >> loop
    l = swont $ arr fst *** afterInput ()
    r = swont $ arr snd *** afterInput ()

eventToList :: Event a -> [a]
eventToList = event [] (replicate 1)

eventOutput :: SF (b, Event c) ((b, Event c), Event c)
eventOutput = arr $ \(x, e) -> ((x, e), e)

eventFinal :: b -> c -> SF a (b, Event c)
eventFinal b c = constant (b, Event c)

swont = cont . switch
dSwont = cont . dSwitch

timedInterp before after duration = time >>> arr (\t -> (after * min t duration + before * (duration - min t duration)) / duration)

timedInterpVec :: Vector2 Double -> Vector2 Double -> Double -> SF a (Vector2 Double)
timedInterpVec before after duration = time >>> arr (\t -> (t *^ after ^+^ (duration - t) *^ before) ^/ duration)

type Swont a b = Cont (SF a b)
type Activity r w s a b = RWST r w s (Swont a b)
type EmbeddedActivity s a b c d = Activity (Embedding a b c d) () s c d
type FlatEmbeddedActivity s a b = Activity (FlatEmbedding a b) () s a b

newtype Embedding a b d e = Embedding { getEmbedding :: forall c . SF a (b, Event c) -> SF d (e, Event c) }
type FlatEmbedding a b = Embedding a b a b

compEmbed (Embedding e) (Embedding e') = Embedding (e . e')
embedArr f = Embedding (>>> first (arr f))
embedInpArr f = Embedding (arr f >>>)

withEmbedding = withRWST . curry . first

eventMuteE sf = sf >>> first (identity &&& constant NoEvent)

reportTrigger = identity &&& arr snd

attachOutput (out, e) = (out, e `attach` out)

reportOutput (out, e) = ((out, e), e `attach` out)

switchE = dSwitch . (>>> arr dup)

parBZ = (arr unzip <<<) . parB
parBZ3 = (arr unzip3 <<<) . parB
