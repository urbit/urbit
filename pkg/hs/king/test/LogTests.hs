module LogTests (tests) where

import Data.Acquire
import Data.Conduit
import Data.Conduit.List     hiding (filter)
import Test.QuickCheck       hiding ((.&.))
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.TH
import Ur.Prelude
import Ur.Vere.Log
import Ur.Vere.Pier.Types

import Control.Concurrent (runInBoundThread, threadDelay)
import Data.LargeWord     (LargeKey(..))
import GHC.Natural        (Natural)
import Ur.King.App        (App, runApp)

import qualified Ur.Vere.Log as Log


-- Utils -----------------------------------------------------------------------

withTestDir :: (FilePath -> RIO e a) -> RIO e a
withTestDir = withTempDirectory "./" ".testlog."

data NotEqual = NotEqual String String
  deriving (Eq, Ord, Show)

instance Exception NotEqual where

assertEqual :: MonadIO m => (Show a, Eq a) => a -> a -> m ()
assertEqual x y = do
    unless (x == y) $ io $ throwIO $ NotEqual (show x) (show y)


-- Database Operations ---------------------------------------------------------

data Db = Db LogIdentity [ByteString] (Map Word64 ByteString)
  deriving (Eq, Ord, Show)

addEvents :: Db -> [ByteString] -> Db
addEvents (Db id evs efs) new = Db id (evs <> new) efs

readDb :: EventLog -> RIO App Db
readDb log = do
    events  <- runConduit (streamEvents log 1 .| consume)
    effects <- runConduit (streamEffectsRows log 0 .| consume)
    pure $ Db (Log.identity log) events (mapFromList effects)

withDb :: FilePath -> Db -> (EventLog -> RIO App a) -> RIO App a
withDb dir (Db dId dEvs dFx) act = do
    rwith (Log.new dir dId) $ \log -> do
        Log.appendEvents log (fromList dEvs)
        for_ (mapToList dFx) $ \(k,v) ->
            Log.writeEffectsRow log k v
        act log

--------------------------------------------------------------------------------

tryReadIdentity :: Property
tryReadIdentity = forAll arbitrary (ioProperty . runApp . runTest)
  where
    runTest :: LogIdentity -> RIO App Bool
    runTest ident = do
        env <- ask
        io $ runInBoundThread $ runRIO env $
            withTestDir $ \dir -> do
                rwith (Log.new dir ident) $ \log ->
                    assertEqual ident (Log.identity log)
                rwith (Log.existing dir) $ \log ->
                    assertEqual ident (Log.identity log)
                rwith (Log.existing dir) $ \log ->
                    assertEqual ident (Log.identity log)
        pure True

tryReadDatabase :: Property
tryReadDatabase = forAll arbitrary (ioProperty . runApp . runTest)
  where
    runTest :: Db -> RIO App Bool
    runTest db = do
        env <- ask
        io $ runInBoundThread $ runRIO env $
            withTestDir $ \dir -> do
                withDb dir db $ \log -> do
                    readDb log >>= assertEqual db
        pure True

tryReadDatabaseFuzz :: Property
tryReadDatabaseFuzz = forAll arbitrary (ioProperty . runApp . runTest)
  where
    runTest :: Db -> RIO App Bool
    runTest db = do
        env <- ask
        io $ runInBoundThread $ runRIO env $
            withTestDir $ \dir -> do
                withDb dir db $ \log -> do
                    readDb log >>= assertEqual db
                rwith (Log.existing dir) $ \log -> do
                    readDb log >>= assertEqual db
                rwith (Log.existing dir) $ \log -> do
                    readDb log >>= assertEqual db
                    readDb log >>= assertEqual db
        pure True

tryAppend :: Property
tryAppend = forAll arbitrary (ioProperty . runApp . runTest)
  where
    runTest :: ([ByteString], Db) -> RIO App Bool
    runTest (extra, db) = do
        env <- ask
        io $ runInBoundThread $ runRIO env $
            withTestDir $ \dir -> do
                db' <- pure (addEvents db extra)
                withDb dir db $ \log -> do
                    readDb log >>= assertEqual db
                    Log.appendEvents log (fromList extra)
                    readDb log >>= assertEqual db'
                rwith (Log.existing dir) $ \log -> do
                    readDb log >>= assertEqual db'
        pure True

tryAppendHuge :: Property
tryAppendHuge = forAll arbitrary (ioProperty . runApp . runTest)
  where
    runTest :: ([ByteString], Db) -> RIO App Bool
    runTest (extra, db) = do
        env <- ask
        io $ runInBoundThread $ runRIO env $ do
            extra <- do b <- readFile "./bin/brass.pill"
                        pure (extra <> [b] <> extra)
            withTestDir $ \dir -> do
                db' <- pure (addEvents db extra)
                withDb dir db $ \log -> do
                    readDb log >>= assertEqual db
                    Log.appendEvents log (fromList extra)
                    readDb log >>= assertEqual db'
                rwith (Log.existing dir) $ \log -> do
                    readDb log >>= assertEqual db'
        pure True


tests :: TestTree
tests =
  testGroup "Log"
    [ localOption (QuickCheckTests 10) $
          testProperty "Read/Write Log Identity" $
              tryReadIdentity
    , localOption (QuickCheckTests 15) $
          testProperty "Read/Write Database" $
              tryReadDatabase
    , localOption (QuickCheckTests 5) $
          testProperty "Read/Write Database Multiple Times" $
              tryReadDatabaseFuzz
    , localOption (QuickCheckTests 10) $
          testProperty "Append Random Events" $
              tryAppend
    , localOption (QuickCheckTests 1) $
          testProperty "Append Huge Events" $
              tryAppendHuge
    ]


-- Generate Arbitrary Values ---------------------------------------------------

arb :: Arbitrary a => Gen a
arb = arbitrary

instance Arbitrary ByteString where
  arbitrary = pack <$> arbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (LargeKey a b) where
  arbitrary = LargeKey <$> arb <*> arb

instance Arbitrary Ship where
  arbitrary = Ship <$> arb

arbEffects :: [ByteString] -> Gen (Map Word64 ByteString)
arbEffects evs = do
  hax <- for (zip [1..] evs) $ \(i, bs) -> do keep :: Bool <- arbitrary
                                              pure (keep, (i, bs))
  pure $ mapFromList $ snd <$> filter fst hax

instance Arbitrary Db where
  arbitrary = do
    ident <- arbitrary
    evs   <- arbitrary
    efs   <- arbEffects evs
    pure (Db ident evs efs)

instance Arbitrary LogIdentity where
  arbitrary = LogIdentity <$> arb <*> arb <*> arb
