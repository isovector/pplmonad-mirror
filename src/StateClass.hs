module StateClass where

import Control.Monad.State
import qualified Data.Text as T
import System.Random

import LabelName
import ProseName

class TextSource s where
    prose :: ProseName -> s -> T.Text
    label :: LabelName -> s -> T.Text
