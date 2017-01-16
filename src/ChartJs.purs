module App.ChartJs where

import Control.Monad.Eff (Eff)
import Data.Array (sort)

import Prelude
  ( ($)
  , (+)
  , (==)
  , const
  , bind
  , Unit )

foreign import data DRAW :: !

foreign import drawChart :: forall eff.  String -> String -> Eff (draw :: DRAW | eff) Unit
