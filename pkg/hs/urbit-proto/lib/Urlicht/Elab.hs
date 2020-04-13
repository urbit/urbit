module Urlicht.Elab where

import ClassyPrelude

import Control.Monad.Except
import Control.Monad.State.Strict
import Data.IntMap.Strict as M
import Data.Void

import Urlicht.Core
import Urlicht.Errors

type Metas = M.IntMap (Value Void)

data ElabState = ElabState
  { _nextMeta :: Meta
  , _metas    :: Metas
  }

type Elab a = StateT ElabState (Either Error) a

--lookupMeta :: Metas -> Meta -> Maybe (Value a)
--lookupMeta ms m = vacuous <$> M.lookup m ms

lookupMeta :: Meta -> Elab (Maybe (Value a))
lookupMeta m = fmap vacuous <$> gets (M.lookup m . _metas)

bindMeta :: Meta -> Value Void -> Elab ()
bindMeta m v = modify \ElabState{..} ->
  ElabState{ _metas = M.insert m v _metas, .. }

report :: MonadError e m => e -> m a
report = throwError
