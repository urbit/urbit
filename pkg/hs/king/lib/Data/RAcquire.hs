module Data.RAcquire where
{-
    ( RAcquire (..)
    , Allocated (..)
    , with
    , mkRAcquire
    , ReleaseType (..)
    , mkRAcquireType
    ) where
-}

import Prelude

import qualified Control.Exception     as E
import qualified Control.Monad.Catch   as C ()
import qualified Data.Acquire.Internal as Act

import Control.Applicative     (Applicative(..))
import Control.Monad           (ap, liftM)
import Control.Monad.IO.Unlift (MonadIO(..), MonadUnliftIO, withRunInIO)
import Data.Typeable           (Typeable)
import Control.Monad.Reader

import RIO (RIO, runRIO)

--------------------------------------------------------------------------------

data ReleaseType
    = ReleaseEarly
    | ReleaseNormal
    | ReleaseException
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Typeable)

data Allocated e a
    = Allocated !a !(ReleaseType -> RIO e ())

newtype RAcquire e a
    = RAcquire ((forall b. RIO e b -> RIO e b) -> RIO e (Allocated e a))
  deriving Typeable

--------------------------------------------------------------------------------

class MonadRIO m where
    liftRIO :: RIO e a -> m e a

instance MonadRIO RIO where
    liftRIO = id

class MonadAcquire m where
    liftAcquire :: Act.Acquire a -> m a

--------------------------------------------------------------------------------

instance Functor (RAcquire e) where
    fmap = liftM

instance Applicative (RAcquire e) where
    pure a = RAcquire (\_ -> return (Allocated a (const $ return ())))
    (<*>) = ap

instance Monad (RAcquire e) where
    return = pure
    RAcquire f >>= g' = RAcquire $ \restore -> do
        env <- ask
        Allocated x free1 <- f restore
        let RAcquire g = g' x
        Allocated y free2 <- liftIO $ E.onException
                                        (runRIO env $ g restore)
                                        (runRIO env $ free1 ReleaseException)

        return $! Allocated y $ \rt ->
            liftIO $ E.finally (runRIO env $ free2 rt)
                               (runRIO env $ free1 rt)

instance MonadReader e (RAcquire e) where
    ask                    = liftRIO ask
    local mod (RAcquire f) = RAcquire $ \restore -> local mod (f restore)

--------------------------------------------------------------------------------

instance MonadRIO RAcquire where
    liftRIO f = RAcquire $ \restore -> do
        x <- restore f
        return $! Allocated x (const $ return ())

instance MonadIO (RAcquire e) where
    liftIO = liftRIO . liftIO

unTransRIO :: e -> (RIO e a -> RIO e a) -> IO a -> IO a
unTransRIO env trans act = runRIO env $ trans $ liftIO act

instance MonadAcquire (RAcquire e) where
    liftAcquire (Act.Acquire f) = do
        env <- liftRIO ask
        RAcquire $ \restore -> do
            fmap fixAllo $ liftIO $ f $ unTransRIO env restore
      where
        fixAllo (Act.Allocated x y) = Allocated x $ fmap liftIO (y . fixTy)

        fixTy = \case
          ReleaseEarly     -> Act.ReleaseEarly
          ReleaseNormal    -> Act.ReleaseNormal
          ReleaseException -> Act.ReleaseException

--------------------------------------------------------------------------------

mkRAcquire :: RIO e a
          -> (a -> RIO e ())
          -> RAcquire e a
mkRAcquire create free = RAcquire $ \restore -> do
    x <- restore create
    return $! Allocated x (const $ free x)

mkRAcquireType
    :: RIO e a -- ^ acquire the resource
    -> (a -> ReleaseType -> RIO e ()) -- ^ free the resource
    -> RAcquire e a
mkRAcquireType create free = RAcquire $ \restore -> do
    x <- restore create
    return $! Allocated x (free x)

transRIO :: e -> (IO a -> IO a) -> RIO e a -> RIO e a
transRIO env trans act = liftIO $ trans $ runRIO env act

rwith :: (MonadUnliftIO (m e), MonadReader e (m e))
      => RAcquire e a
      -> (a -> m e b)
      -> m e b
rwith (RAcquire f) g = do
    env <- ask
    withRunInIO $ \run -> E.mask $ \restore -> do
        Allocated x free <- runRIO env $ f $ transRIO env restore
        res <- E.onException (restore $ run $ g x)
                             (runRIO env $ free ReleaseException)
        runRIO env $ free ReleaseNormal
        return res
