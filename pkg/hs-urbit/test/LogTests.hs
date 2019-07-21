module LogTests (tests) where

import Data.Acquire
import Test.QuickCheck       hiding ((.&.))
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.TH
import UrbitPrelude
import Vere.Log
import Vere.Pier.Types
import Data.Conduit
import Data.Conduit.List

import Control.Concurrent (threadDelay, runInBoundThread)
import Data.LargeWord     (LargeKey(..))
import GHC.Natural        (Natural)

import qualified Vere.Log as Log


-- Utils -----------------------------------------------------------------------

withTestDir :: (FilePath -> IO a) -> IO a
withTestDir = withTempDirectory "./" ".testlog."

data NotEqual = NotEqual String String
  deriving (Eq, Ord, Show)

instance Exception NotEqual where

assertEqual :: (Show a, Eq a) => a -> a -> IO ()
assertEqual x y = do
    unless (x == y) $
        throwIO (NotEqual (show x) (show y))


-- Database Operations ---------------------------------------------------------

data Db = Db LogIdentity [ByteString]
  deriving (Eq, Ord, Show)

addEvents :: Db -> [ByteString] -> Db
addEvents (Db id evs) new = Db id (evs <> new)

readDb :: EventLog -> IO Db
readDb log = do
    events <- runConduit (streamEvents log 1 .| consume)
    pure $ Db (Log.identity log) events

withDb :: FilePath -> Db -> (EventLog -> IO a) -> IO a
withDb dir (Db dId dEvs) act = do
    with (Log.new dir dId) $ \log -> do
        Log.appendEvents log (fromList dEvs)
        act log

--------------------------------------------------------------------------------

tryReadIdentity :: Property
tryReadIdentity = forAll arbitrary (ioProperty . runTest)
  where
    runTest :: LogIdentity -> IO Bool
    runTest ident = do
        runInBoundThread $
            withTestDir $ \dir -> do
                with (Log.new dir ident) $ \log ->
                    assertEqual ident (Log.identity log)
                with (Log.existing dir) $ \log ->
                    assertEqual ident (Log.identity log)
                with (Log.existing dir) $ \log ->
                    assertEqual ident (Log.identity log)
        pure True

tryReadDatabase :: Property
tryReadDatabase = forAll arbitrary (ioProperty . runTest)
  where
    runTest :: Db -> IO Bool
    runTest db = do
        runInBoundThread $
            withTestDir $ \dir -> do
                withDb dir db $ \log -> do
                    readDb log >>= assertEqual db
                with (Log.existing dir) $ \log -> do
                    readDb log >>= assertEqual db
                with (Log.existing dir) $ \log -> do
                    readDb log >>= assertEqual db
                    readDb log >>= assertEqual db
        pure True

tryAppend :: Property
tryAppend = forAll arbitrary (ioProperty . runTest)
  where
    runTest :: ([ByteString], Db) -> IO Bool
    runTest (extra, db) = do
        runInBoundThread $
            withTestDir $ \dir -> do
                db' <- pure (addEvents db extra)
                withDb dir db $ \log -> do
                    readDb log >>= assertEqual db
                    Log.appendEvents log (fromList extra)
                    readDb log >>= assertEqual db'
                with (Log.existing dir) $ \log -> do
                    readDb log >>= assertEqual db'
        pure True

tryAppendHuge :: Property
tryAppendHuge = forAll arbitrary (ioProperty . runTest)
  where
    runTest :: ([ByteString], Db) -> IO Bool
    runTest (extra, db) = do
        runInBoundThread $ do
            extra <- do b <- readFile "/home/benjamin/r/urbit/bin/brass.pill"
                        pure (extra <> [b] <> extra)
            withTestDir $ \dir -> do
                db' <- pure (addEvents db extra)
                withDb dir db $ \log -> do
                    readDb log >>= assertEqual db
                    Log.appendEvents log (fromList extra)
                    readDb log >>= assertEqual db'
                with (Log.existing dir) $ \log -> do
                    readDb log >>= assertEqual db'
        pure True


tests :: TestTree
tests =
  testGroup "Log"
    [ localOption (QuickCheckTests 10) $
          testProperty "Read/Write Log Identity" $
              tryReadIdentity
    , localOption (QuickCheckTests 10) $
          testProperty "Read/Write Database" $
              tryReadDatabase
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

instance Arbitrary Db where
  arbitrary = Db <$> arbitrary <*> arbitrary

instance Arbitrary LogIdentity where
  arbitrary = LogIdentity <$> arb <*> arb <*> arb
