module HoonMapSetTests (tests) where

import RIO.Directory
import Ur.Prelude    hiding (encodeUtf8)

import Data.Text.Lazy.Encoding (encodeUtf8)
import Numeric.Natural         (Natural)
import Test.QuickCheck         hiding ((.&.))
import Test.Tasty
import Test.Tasty.Golden       as G
import Test.Tasty.QuickCheck
import Test.Tasty.TH

import qualified Data.ByteString.Lazy as L

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

roundTrip :: ∀a. Eq a => (a -> a) -> a -> Bool
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

treeRTMug :: FilePath -> IO L.ByteString
treeRTMug inp = do
    byt <- readFile inp
    non <- cueBSExn byt
    tee <- fromNounExn non
    mug <- evaluate $ mug $ toNoun $ treeTestsIdentity tee
    pure $ encodeUtf8 $ tlshow (mug :: Natural)


goldenFile :: String -> String -> (FilePath -> IO L.ByteString) -> TestTree
goldenFile testName testFileName action =
    goldenVsString testName gold (action pill)
  where
    root = "pkg/hs/urbit-king/test/gold" </> testFileName
    gold = root <.> "gold"
    pill = root <.> "pill"


-- Test Tree -------------------------------------------------------------------

tests :: TestTree
tests =
  testGroup "Map/Set Conversions"
    [ goldenFile "Golden Map Roundtrip" "hoontree" treeRTMug
    , testProperty "Map Rountrip" mapRoundtrip
    , testProperty "Set Rountrip" setRoundtrip
    ]
