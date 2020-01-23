module DawnTests (tests) where

import Ur.Arvo.Event
import Ur.Noun.Conversions
import Ur.Prelude

import Test.Tasty
import Test.Tasty.HUnit

import qualified Ur.Vere.Dawn as Dawn
import qualified Urbit.Ob     as Ob

--------------------------------------------------------------------------------

-- These golden cases generated in Urbit from entropy to make sure our +mix,
-- +shas, +shaf, etc. were actually calculated correctly.

cordToAtomBytes :: Text -> ByteString
cordToAtomBytes t = cordToAtom t ^. atomBytes

cordToAtom :: Text -> Atom
cordToAtom t = case cordToUW (Cord t) of
    Nothing     -> error "Couldn't parse constant embedded in file."
    Just (UW a) -> a

testString = cordToAtomBytes $ concat
    [ "0w1.XqnKc.onYJK.0zVOU.Uw142.jNx3C.oWV83.TYt6T.kmHUg.cnoq1.zla6B.bKeNa"
    , ".8wUZu.6ZLHJ.c1TKV.KPcb3.9lU3~.p2G8D"
    ]

testSalt = cordToAtomBytes "0wc.~cOwa.Kb-DI.BrjVW.i0U37"

mixByteStrings = (Dawn.mix testSalt testString) @?= expected
  where
    expected = cordToAtomBytes $ concat
      [ "0w1.XqnKc.onYJK.0zVOU.Uw142.jNx3C.oWV83.TYt6T.kmHUg.cnoq1.zla6B.bKeNa"
      , ".8wUZu.6ZLHx.Pd5eP.0UOIL.IeHW5.b2ibw"
      ]

shasByteStrings = (Dawn.shas testSalt testString) @?= expected
  where
    expected = cordToAtomBytes
      "0wfKW.mXzrj.c~IBb.lKd6k.2njoG.bRLcD.9eszA.gSSs8.mHRah"

shafByteStrings = (Dawn.shaf testSalt testString) @?= expected
  where
    expected = cordToAtomBytes "0w3h.Bg1Qh.ZZjoJ.23J~p.PHg-D"

--------------------------------------------------------------------------------

cometShip :: Ship
cometShip = case Ob.parsePatp cometStr of
  Left x  -> error "Invalid ship name"
  Right p -> Ship $ fromIntegral $ Ob.fromPatp p
  where
    cometStr = "~radmes-dilsec-sovlup-lagwep--tonred-waldeb-tocseg-marzod"

cometPass :: Pass
cometPass = case fromNoun (Atom cometPassAtom) of
  Nothing -> error "Keyfile does not seem to contain a seed."
  Just s  -> s
  where
    cometPassAtom = cordToAtom $ concat
      [ "0w99.P80w4.rL7Qt.0i5-h.8yta7.RhgHI.nrXjO.xBCix.Pxx5a.sJ6bv.a-Iwo.OeVBr"
      , ".x8-Gs.1LLG~.FgDRk.GML3Y.X3qFZ.jtlpy"
      ]

cometRawBS = cordToAtomBytes "0w39.q35g-.hrd3f.q9UWK.Zxg40"

-- Verifies the internal usage of +shaf in comet derivation gets the right
-- answer.
testCometFingerprintBS = (Dawn.cometFingerprintBS cometPass) @?= cometRawBS

-- Tests that the real public interface for fingerprint generation does the
-- byte-munging correctly.
testCometFingerprint = (Dawn.cometFingerprint cometPass) @?= cometShip

--------------------------------------------------------------------------------

tests :: TestTree
tests =
  testGroup "Dawn"
    [ testCase "Mix bytestrings of different length" $ mixByteStrings
    , testCase "Shas bytestrings" $ shasByteStrings
    , testCase "Shaf bytestrings" $ shafByteStrings
    , testCase "Fingerprint bytestring derivation" $ testCometFingerprintBS
    , testCase "Fingerprint total derivation" $ testCometFingerprint
    ]
