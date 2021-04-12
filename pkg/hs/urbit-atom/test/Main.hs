{-# OPTIONS_GHC -Wno-deprecations -Wno-orphans #-}


module Main (main) where

--------------------------------------------------------------------------------

import Data.IORef
import Numeric.Natural
import Prelude
import System.Exit
import System.IO.Unsafe
import Test.QuickCheck

import Control.Monad         (when)
import Data.ByteString       (ByteString)
import Data.Vector.Primitive (Prim, Vector)

import qualified Data.ByteString        as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.Vector.Primitive  as VP
import qualified Urbit.Atom.Fast        as F
import qualified Urbit.Atom.Slow        as S


-- Instances -------------------------------------------------------------------

instance Arbitrary Natural where
  arbitrary = fromInteger . abs <$> arbitrary

instance Arbitrary ByteString where
  arbitrary = BS.pack <$> arbitrary

instance (Prim a, Arbitrary a) => Arbitrary (Vector a) where
  arbitrary = VP.fromList <$> arbitrary


-- Utils -----------------------------------------------------------------------

stripBytes :: ByteString -> ByteString
stripBytes buf = BS.take (len - go 0 (len - 1)) buf
 where
  len = BS.length buf
  go n i | i < 0                     = n
         | 0 == BS.unsafeIndex buf i = go (n + 1) (i - 1)
         | otherwise                 = n

stripWords :: Vector Word -> Vector Word
stripWords vec = VP.take (len - go 0 (len - 1)) vec
 where
  len = VP.length vec
  go n i | i < 0                     = n
         | 0 == VP.unsafeIndex vec i = go (n + 1) (i - 1)
         | otherwise                 = n

dumpLoad :: Eq i => (i -> o) -> (o -> i) -> (i -> Bool)
dumpLoad dump load x = x == load (dump x)

loadDump :: Eq o => (o -> i) -> (i -> o) -> (o -> o) -> (o -> Bool)
loadDump load dump norm x = norm x == dump (load x)


-- Test Reference Implementation -----------------------------------------------

prop_atom_bytes_roundtrip :: Natural -> Bool
prop_atom_bytes_roundtrip = dumpLoad S.atomBytes S.bytesAtom

prop_atom_words_roundtrip :: Natural -> Bool
prop_atom_words_roundtrip = dumpLoad S.atomWords S.wordsAtom

prop_bytes_atom_roundtrip :: ByteString -> Bool
prop_bytes_atom_roundtrip = loadDump S.bytesAtom S.atomBytes stripBytes

prop_words_atom_roundtrip :: Vector Word -> Bool
prop_words_atom_roundtrip = loadDump S.wordsAtom S.atomWords stripWords


-- Test Fast Implementation ----------------------------------------------------

prop_fast_atom_bytes_roundtrip :: Natural -> Bool
prop_fast_atom_bytes_roundtrip = dumpLoad F.atomBytes F.bytesAtom

prop_fast_atom_words_roundtrip :: Natural -> Bool
prop_fast_atom_words_roundtrip = dumpLoad F.atomWords F.wordsAtom

prop_fast_bytes_atom_roundtrip :: ByteString -> Bool
prop_fast_bytes_atom_roundtrip = loadDump F.bytesAtom F.atomBytes stripBytes

prop_fast_words_atom_roundtrip :: Vector Word -> Bool
prop_fast_words_atom_roundtrip = loadDump F.wordsAtom F.atomWords stripWords


-- Fast and Reference Implementations are the Same -----------------------------

prop_fast_words_atom_correct :: Vector Word -> Bool
prop_fast_words_atom_correct x = F.wordsAtom x == S.wordsAtom x

prop_fast_atom_words_correct :: Natural -> Bool
prop_fast_atom_words_correct x = F.atomWords x == S.atomWords x

prop_fast_bytes_atom_correct :: ByteString -> Bool
prop_fast_bytes_atom_correct x = F.bytesAtom x == S.bytesAtom x

prop_fast_atom_import_correct :: ByteString -> Bool
prop_fast_atom_import_correct x = F.importBytes x == S.bytesAtom x

prop_fast_atom_bytes_correct :: Natural -> Bool
prop_fast_atom_bytes_correct x = F.atomBytes x == S.atomBytes x

prop_fast_atom_export_correct :: Natural -> Bool
prop_fast_atom_export_correct x = F.exportBytes x == S.atomBytes x


--------------------------------------------------------------------------------

failed :: IORef Int
failed = unsafePerformIO (newIORef 0)

checkProp :: Testable prop => String -> prop -> IO ()
checkProp nm chk = do
  putStrLn nm
  res <- quickCheckResult chk
  putStrLn ""

  case res of
    Success{} -> pure ()
    _         -> modifyIORef' failed succ

main :: IO ()
main = do
  checkProp "Reference: Atom <-> ByteString roundtrip"
    prop_atom_bytes_roundtrip

  checkProp "Reference: Atom <-> Vector Word roundtrip"
    prop_atom_words_roundtrip

  checkProp "Reference: ByteString <-> Atom roundtrip"
    prop_bytes_atom_roundtrip

  checkProp "Reference: Vector Word <-> Atom roundtrip"
    prop_words_atom_roundtrip

  checkProp "Fast: Atom <-> ByteString roundtrip"
    prop_fast_atom_bytes_roundtrip

  checkProp "Fast: Atom <-> Vector Word roundtrip"
    prop_fast_atom_words_roundtrip

  checkProp "Fast: Bytestring <-> Atom roundtrip"
    prop_fast_bytes_atom_roundtrip

  checkProp "Fast: Export->Import roundtrip" $ do
    withMaxSuccess 100000 (dumpLoad F.exportBytes F.importBytes)

  checkProp "Fast: Import->Export roundtrip" $ do
    withMaxSuccess 10000 (loadDump F.importBytes F.exportBytes stripBytes)

  checkProp "Fast: Vector Word <-> Atom roundtrip"
    prop_fast_words_atom_roundtrip

  checkProp "Fast matches reference: Vector Words -> Atom"
    (withMaxSuccess 10000 prop_fast_words_atom_correct)

  checkProp "Fast matches reference: Atom -> Vector Word"
    (withMaxSuccess 10000 prop_fast_atom_words_correct)

  checkProp "Fast matches reference: ByteString -> Atom"
    (withMaxSuccess 10000 prop_fast_bytes_atom_correct)

  checkProp "Fast matches reference: Atom -> ByteString"
    (withMaxSuccess 10000 prop_fast_atom_bytes_correct)

  checkProp "Fast matches reference: Atom Import"
    (withMaxSuccess 10000 prop_fast_atom_import_correct)

  checkProp "Fast matches reference: Atom Export"
    (withMaxSuccess 10000 prop_fast_atom_export_correct)

  res <- readIORef failed
  when (res /= 0) $ do
    putStrLn $ "FAILURE: " <> show res <> " tests failed."
    exitWith (ExitFailure 1)
  putStrLn $ "SUCCESS: All tests passed"
