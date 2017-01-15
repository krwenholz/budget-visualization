module App.ChartJs where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Array (sort)

-- Import the library's module(s)
import Data.Foreign.EasyFFI (unsafeForeignProcedure)

import Prelude
  ( ($)
  , (+)
  , (==)
  , const
  , bind
  , Unit )

-- drawChart :: forall eff.  String -> Eff (console :: Unit | eff) Unit
drawChart = unsafeForeignProcedure ["string", ""] "console.log('\n\n\n' + string + '\n\n\n\n');"
