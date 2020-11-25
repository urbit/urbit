module HoonMapSetTests (tests) where

import RIO.Directory
import Urbit.Prelude

import Data.ByteString.Lazy (ByteString)
import Numeric.Natural         (Natural)
import Test.QuickCheck         hiding ((.&.))
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.TH

import qualified Data.ByteString            as ByteString
import qualified Data.ByteString.Lazy       as ByteString.Lazy
import qualified Data.Text.Lazy             as Text.Lazy
import qualified Data.Text.Lazy.Encoding    as Text.Lazy.Encoding
import qualified Paths_urbit_king
import qualified Test.Tasty.Golden          as Golden
import qualified Test.Tasty.Golden.Advanced as Golden.Advanced

-- Types -----------------------------------------------------------------------

newtype SmallNoun = SN Noun
  deriving newtype (Eq, Ord, Show, ToNoun)

instance Arbitrary SmallNoun where
  arbitrary = SN <$> oneof [a, c, ac, ca, cc]
    where
      a  = A . fromIntegral <$> arbitrary @Word8
      c  = C <$> a <*> a
      ac = C <$> a <*> c
      ca = C <$> c <*> a
      cc = C <$> c <*> c

data TreeTest
    = TTMap (HoonMap Noun Noun)
    | TTSet (HoonSet Noun)

deriveNoun ''TreeTest

type TreeTests = [TreeTest]


-- Utils -----------------------------------------------------------------------

roundTrip :: forall a. Eq a => (a -> a) -> a -> Bool
roundTrip f x = f x == x


-- Props -----------------------------------------------------------------------

mapRoundtrip :: Map SmallNoun SmallNoun -> Bool
mapRoundtrip = roundTrip (mapFromHoonMap . mapToHoonMap)

setRoundtrip :: Set SmallNoun -> Bool
setRoundtrip = roundTrip (setFromHoonSet . setToHoonSet)

-- Golden Tests ----------------------------------------------------------------

treeTestsIdentity :: TreeTests -> TreeTests
treeTestsIdentity = fmap go
  where
    go = \case
        TTSet s -> (TTSet . setToHoonSet . setFromHoonSet) s
        TTMap m -> (TTMap . mapToHoonMap . mapFromHoonMap) m

treeRTMug :: FilePath -> IO ByteString.Lazy.ByteString
treeRTMug inp = do
    byt <- readFile inp
    non <- cueBSExn byt
    tee <- fromNounExn non
    mug <- evaluate $ mug $ toNoun $ treeTestsIdentity tee
    pure $ Text.Lazy.Encoding.encodeUtf8 $ tlshow mug

goldenPill
    :: TestName
    -> String
    -> (FilePath -> IO ByteString.Lazy.ByteString)
    -> TestTree
goldenPill test name action =
  goldenVsString test gold (action pill)
 where
  gold = "test/gold" </> name <.> "gold"
  pill = "test/gold" </> name <.> "pill"
    
-- | Compare a given string against the golden file's contents.
goldenVsString
  :: TestName
  -- ^ Test name
  -> String
  -- ^ The «golden» file that will be retrieved via 'getDataFileName'.
  -> IO ByteString.Lazy.ByteString
  -- ^ Action that returns the string for comparison.
  -> TestTree
  -- ^ Verifies the golden file contents is identical to the returned string.
goldenVsString test name action =
  askOption $ \cutoff ->
    Golden.Advanced.goldenTest name acquire action (comparator cutoff) update
  where
    acquire = do
      path  <- Paths_urbit_king.getDataFileName name
      bytes <- ByteString.readFile path

      pure (ByteString.Lazy.fromStrict bytes)
   
    comparator cutoff x y =
      pure $
        if x == y
          then Nothing
          else Just
             ( printf "Test output was different from '%s'. It was:\n" name
            <> unpackUTF8 (truncate cutoff y)
             )

    unpackUTF8 = Text.Lazy.unpack . Text.Lazy.Encoding.decodeUtf8

    truncate (Golden.SizeCutoff cutoff) bytes =
      if ByteString.Lazy.length bytes <= cutoff
        then bytes
        else ByteString.Lazy.take cutoff bytes
          <> "<truncated>"
          <> "\nUse --accept or increase --size-cutoff to see full output."
 
    -- The update function is a noop as we don't have the golden file name.
    update _ = pure ()
    
-- Test Tree -------------------------------------------------------------------

tests :: TestTree
tests =
  testGroup "Map/Set Conversions"
    [ goldenPill "Golden Map Roundtrip" "hoontree" treeRTMug
    , testProperty "Map Rountrip" mapRoundtrip
    , testProperty "Set Rountrip" setRoundtrip
    ]
