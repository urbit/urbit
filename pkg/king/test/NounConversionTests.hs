module NounConversionTests (tests) where

import Arvo.Event
import Noun.Conversions
import UrbitPrelude

import Data.Maybe
import Test.QuickCheck        hiding ((.&.))
import Test.QuickCheck.Gen
import Test.QuickCheck.Random
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.TH

import qualified Crypto.Sign.Ed25519 as Ed
-- import qualified Crypto.ECC.Edwards25519 as Ed
-- import qualified Crypto.Error            as Ed
import qualified Data.ByteArray as BA

-- String Representations of Atoms ---------------------------------------------

instance Arbitrary UV where
  arbitrary = UV <$> arbitrarySizedNatural

instance Arbitrary UW where
  arbitrary = UW <$> arbitrarySizedNatural

vRoundTrip :: UV -> Bool
vRoundTrip uv = Just uv == (fromNoun $ toNoun $ uv)

wRoundTrip :: UW -> Bool
wRoundTrip uw = Just uw == (fromNoun $ toNoun uw)

-- Cryptographic Point Representations -----------------------------------------

data ThirtyTwoByteString = ThirtyTwoByteString ByteString
  deriving (Show)

data KeyPair = KeyPair (Ed.PublicKey, Ed.SecretKey)
  deriving (Show)

instance Arbitrary ThirtyTwoByteString where
  arbitrary = (ThirtyTwoByteString . pack) <$> (vector 32)

instance Arbitrary KeyPair where
  arbitrary =
    (KeyPair . fromJust . Ed.createKeypairFromSeed_ . pack) <$> (vector 32)


passRoundTrip :: KeyPair -> KeyPair -> Bool
passRoundTrip (KeyPair (signPubkey, _)) (KeyPair (cryptPubkey, _)) =
    (Just p) == (fromNoun $ toNoun p)
  where
    p = Pass signPubkey cryptPubkey


ringRoundTrip :: ThirtyTwoByteString -> ThirtyTwoByteString -> Bool
ringRoundTrip (ThirtyTwoByteString signSeed) (ThirtyTwoByteString cryptSeed) =
    (Just r) == (fromNoun $ toNoun r)
  where
    r = Ring signSeed cryptSeed


--------------------------------------------------------------------------------

tests :: TestTree
tests =
  testGroup "Noun"
    [ testProperty "0v0 printing/parsing round trip" $ vRoundTrip
    , testProperty "0w0 printing/parsing round trip" $ wRoundTrip
    , testProperty "Pass round trip" $ passRoundTrip
    , testProperty "Ring round trip" $ ringRoundTrip
    ]
