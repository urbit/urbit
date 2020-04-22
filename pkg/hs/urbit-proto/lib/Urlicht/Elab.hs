module Urlicht.Elab where

import ClassyPrelude

import Bound.Scope
import Control.Monad.Except
import Control.Monad.State.Strict
import Data.IntMap.Strict as M
import Data.Void

import Urlicht.Core
import Urlicht.Errors
import Urlicht.Meta

type Metas = M.IntMap (Value Void)

data ElabState = ElabState
  { _nextMeta :: Meta
  , _metas    :: Metas
  }

type Elab a = StateT ElabState (Either Error) a

runElab :: Elab a -> a
runElab = either (error . show) id . flip evalStateT (ElabState 0 mempty)

--lookupMeta :: Metas -> Meta -> Maybe (Value a)
--lookupMeta ms m = vacuous <$> M.lookup m ms

lookupMeta :: Meta -> Elab (Maybe (Value a))
lookupMeta m = fmap vacuous <$> gets (M.lookup m . _metas)

bindMeta :: Meta -> Value Void -> Elab ()
bindMeta m v = modify \ElabState{..} ->
  ElabState{ _metas = M.insert m v _metas, .. }

freshMeta :: Elab Meta
freshMeta = do
  m <- gets _nextMeta
  modify \ElabState{..} -> ElabState{ _nextMeta = _nextMeta + 1, ..}
  pure m

report :: MonadError e m => e -> m a
report = throwError

-- | Inline any (newly) known metas into the value and evaluate
crank :: Value a -> Elab (Value a)
crank = go where
  go :: Value a -> Elab (Value a)
  go = \case
    VVAp x vs  -> VVAp x <$> traverse go vs
    VMAp m vs  -> lookupMeta m >>= \case
      Just v  -> crank =<< vApps v <$> traverse go vs
      Nothing -> VMAp m  <$> traverse go vs
    VTyp       -> pure VTyp
    VFun v sv  -> VFun <$> go v <*> transverseScope go sv
    VLam sv    -> VLam <$> transverseScope go sv
