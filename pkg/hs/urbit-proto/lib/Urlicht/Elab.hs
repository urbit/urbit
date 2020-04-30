module Urlicht.Elab where

import ClassyPrelude

import Bound.Scope
import Control.Monad.Except
import Control.Monad.State.Strict
import qualified Data.IntMap.Strict as M
import Data.Key (forWithKey_)
import Data.Void

import Urlicht.Core
import Urlicht.Errors
import Urlicht.Meta
import Urlicht.RunicShow

class Monad m => Logging m where
  info :: Text -> m ()

class Logging m => Elab m where
  report      :: Error -> m a
  lookupMeta  :: Meta -> m (Maybe (Value a))
  bindMeta    :: Meta -> Value Void -> m ()
  freshMeta   :: m Meta
  -- | Inline any (newly) known metas into the value and evaluate
  crank       :: Value a -> m (Value a)
  -- | Print out the whole elaboration state
  tell        :: m ()

  crank = go where
    go :: Value a -> m (Value a)
    go = \case
      VVAp x vs  -> VVAp x <$> traverse go vs
      VMAp m vs  -> lookupMeta m >>= \case
        Just v  -> crank =<< vApps v <$> traverse go vs
        Nothing -> VMAp m  <$> traverse go vs
      VTyp       -> pure VTyp
      VFun v sv  -> VFun <$> go v <*> transverseScope go sv
      VLam sv    -> VLam <$> transverseScope go sv

type Metas = M.IntMap (Value Void)

data ElabState = ElabState
  { _nextMeta :: Meta
  , _metas    :: Metas
  }

newtype ElabT m a = ElabT { runElabT :: StateT ElabState (ExceptT Error m) a }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadState ElabState
    , MonadError Error
    )

instance Logging Identity where
  info _ = pure ()

instance Logging IO where
  info t = putStrLn t

instance Logging m => Logging (ElabT m) where
  info = ElabT . lift . lift . info

instance Logging m => Elab (ElabT m) where
  report      = report'
  lookupMeta  = lookupMeta'
  bindMeta    = bindMeta'
  freshMeta   = freshMeta'
  tell        = tell'

instance MonadIO m => MonadIO (ElabT m) where
  liftIO = ElabT . liftIO

runElab :: ElabT Identity a -> a
runElab = either (error . unpack . runic) id
        . runIdentity
        . runExceptT
        . flip evalStateT (ElabState 0 mempty)
        . runElabT

runElabIO :: ElabT IO a -> IO a
runElabIO = fmap (either (error . unpack . runic) id)
          . runExceptT
          . flip evalStateT (ElabState 0 mempty)
          . runElabT

report' :: MonadError e m => e -> m a
report' = throwError

lookupMeta' :: Monad m => Meta -> ElabT m (Maybe (Value a))
lookupMeta' m = fmap vacuous <$> gets (M.lookup m . _metas)

bindMeta' :: Logging m => Meta -> Value Void -> ElabT m ()
bindMeta' m v = do
  ms <- gets _metas
  for_ (lookup m ms) \v ->
    info  ("Overwriting meta " <> showMeta m <> ", previously:\n" <> runic v)
  modify \ElabState{..} ->
    ElabState{ _metas = M.insert m v _metas, .. }

freshMeta' :: Logging m => ElabT m Meta
freshMeta' = do
  m <- gets _nextMeta
  info ("Fresh meta: " <> showMeta m)
  modify \ElabState{..} -> ElabState{ _nextMeta = _nextMeta + 1, ..}
  pure m

tell' :: Logging m => ElabT m ()
tell' = do
  m <- gets _nextMeta
  info ("Next meta: " <> showMeta m)
  ms <- gets _metas
  forWithKey_ ms \m v ->
    info (showMeta m <> ":\n" <> runic v)
