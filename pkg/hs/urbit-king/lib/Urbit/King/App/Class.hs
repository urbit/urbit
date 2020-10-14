{-|
    Code for setting up the RIO environment.
-}
module Urbit.King.App.Class
  ( HasStderrLogFunc(..)
  )
where

import Urbit.Prelude


-- KingEnv ---------------------------------------------------------------------

class HasStderrLogFunc a where
  stderrLogFuncL :: Lens' a LogFunc
