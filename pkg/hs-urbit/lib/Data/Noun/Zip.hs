{-
  Can de-duplication be orthogonal to serialization?
-}

module Data.Noun.Zip where

import ClassyPrelude hiding (zip, unzip)

import Control.Lens
import Text.Printf
import Control.Applicative
import Data.Noun
import Data.Noun.Atom
import Data.Noun.Jam
import Data.Bits
import GHC.Generics
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Data.Flat
import Data.Flat.Bits
import Data.Either.Extra
import GHC.Natural
import Data.Flat

import Data.Maybe    (fromJust)
import Data.List     (intercalate)
import Data.Typeable (Typeable)

import Control.Monad.State.Strict hiding (forM_, replicateM)
import Control.Monad.Trans.Maybe

import qualified ClassyPrelude
import qualified Data.Vector         as V
import qualified Data.List           as L
import qualified Data.Vector.Unboxed as UV

import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck

-- Atoms Optimized For Small Values --------------------------------------------

data Unary = Z | O Unary
  deriving stock    (Eq, Ord, Show, Generic)
  deriving anyclass Flat

instance IsAtom Unary where
  toAtom Z     = 0
  toAtom (O u) = 1+toAtom u
  fromAtom 0 = Z
  fromAtom n = O (fromAtom (pred n))

data ZipAtom
    = ZATiny Unary
    | ZAWide Natural
  deriving stock    (Eq, Ord, Generic)
  deriving anyclass Flat

instance Show ZipAtom where
  show = show . toAtom

instance IsAtom ZipAtom where
  toAtom (ZATiny u) = toAtom u
  toAtom (ZAWide n) = toAtom n + 8
  fromAtom a | a <= 7 = ZATiny (fromAtom a)
  fromAtom (MkAtom n) = ZAWide (n-8)


-- External Types --------------------------------------------------------------

data ZipNode
    = ZipAtom !ZipAtom
    | ZipCell !ZipRef !ZipRef
  deriving stock    (Eq, Ord, Show, Generic)
  deriving anyclass Flat

data ZipRef
    = ZRInline !ZipNode
    | ZRIndex  !ZipAtom
  deriving stock    (Eq, Ord, Show, Generic)
  deriving anyclass Flat

type Zip = ([ZipNode], ZipNode)

-- Zip and UnZip ---------------------------------------------------------------

refCount :: Noun -> Map Noun Word
refCount = go mempty
  where
    ins :: Noun -> Map Noun Word -> Map Noun Word
    ins = alterMap (Just . maybe 1 (+1))

    go :: Map Noun Word -> Noun -> Map Noun Word
    go acc a@(Atom _)   = ins a acc
    go acc c@(Cell l r) = go (go (ins c acc) l) r

zipTable :: Noun -> (Vector Noun, Map Noun Int)
zipTable top = (V.fromList tbl, keys)
  where
    keys = mapFromList (ClassyPrelude.zip tbl [0..])
    big  = \case Atom a -> a >= 127+8
                 _      -> True
    tbl  = fmap fst
         $ sortBy (comparing snd)
         $ filter (\(k,v) -> big k && v>1)
         $ mapToList
         $ refCount top

zip :: Noun -> Zip
zip top = (V.toList dups, cvtNode top)
  where
    (tbl, keys) = zipTable top
    dups        = cvtNode <$> tbl
    cvtRef n    = lookup n keys & \case Nothing -> ZRInline (cvtNode n)
                                        Just a  -> ZRIndex (fromAtom $ toAtom a)
    cvtNode     = \case Atom a   -> ZipAtom (fromAtom a)
                        Cell l r -> ZipCell (cvtRef l) (cvtRef r)

unzip :: Zip -> Maybe Noun
unzip (V.fromList -> dups, top) = recover top
  where
    recover :: ZipNode -> Maybe Noun
    recover (ZipAtom a)   = pure (Atom $ toAtom a)
    recover (ZipCell l r) = Cell <$> getRef l <*> getRef r

    getRef :: ZipRef -> Maybe Noun
    getRef (ZRInline n) = recover n
    getRef (ZRIndex ix) = dups V.!? fromAtom (toAtom ix) >>= recover


-- Tests -----------------------------------------------------------------------

compareSize :: Noun -> Int
compareSize n = flatSz - jamSz
  where
    Buf jamSz _ = fromAtom (jam' n)
    flatSz      = length (bits (zip n))

compareZipCompression :: Noun -> Int
compareZipCompression n = zipSz - rawSz
  where
    rawSz = length (bits n)
    zipSz = length (bits (zip n))

compareRawToJam :: Noun -> Int
compareRawToJam n = rawSz - jamSz
  where
    rawSz       = length (bits n)
    Buf jamSz _ = fromAtom (jam' n)

prop_zipUnzip :: Noun -> Bool
prop_zipUnzip n = Just n == unzip (zip n)

zipFlat :: Noun -> ByteString
zipFlat = flat . zip

unZipFlat :: ByteString -> Maybe Noun
unZipFlat = (>>= unzip) . eitherToMaybe . unflat

prop_zipFlatRoundTrip :: Noun -> Bool
prop_zipFlatRoundTrip n = Just n == (unZipFlat . zipFlat) n

main :: IO ()
main = $(defaultMainGenerator)

dub :: Noun -> Noun
dub x = Cell x x

allAtoms :: Int -> [Noun]
allAtoms n = Atom . (\n -> 2^n - 1) <$> [0..toAtom n]

allCells :: Int -> [Noun]
allCells 0 = allAtoms 1
allCells n = do
  a <- Atom <$> [0, (2 ^ toAtom n) - 1]
  c <- allCells (n-1)
  [Cell c a, Cell a c, Cell c c]

allNouns :: Int -> [Noun]
allNouns sz = ordNub (allCells sz)

nounSizes :: (Noun -> Int) -> Int -> [(Int, Noun)]
nounSizes f sz = sort (allNouns sz <&> \n -> (f n, n))

jamSz :: Noun -> Int
jamSz = (\(Buf sz _) -> sz) . fromAtom . jam'

showFlatZipSizes :: Int -> IO ()
showFlatZipSizes dep = traverse_ print (nounSizes (length . bits . zip) dep)

showJamSizes :: Int -> IO ()
showJamSizes dep = traverse_ print (nounSizes jamSz dep)

--------------------------------------------------------------------------------

sumJamSizes :: Int -> Int
sumJamSizes dep = sum $ map fst (nounSizes jamSz dep)

sumFlatSizes :: Int -> Int
sumFlatSizes dep = sum $ map fst (nounSizes (length . bits) dep)

sumFlatZipSizes :: Int -> Int
sumFlatZipSizes dep = sum $ map fst (nounSizes (length . bits . zip) dep)

--------------------------------------------------------------------------------

compareSizes :: (Noun -> Int) -> IO ()
compareSizes f = do
  nouns <- join <$> (replicateM 100 (sample' (arbitrary :: Gen Noun)) :: IO [[Noun]])
  traverse_ print $ reverse
                  $ ordNub
                  $ sort
                  $ fmap ((`div` 64) . f)
                  $ nouns
  -- traverse_ print $ filter ((> 1000) . abs . f) nouns

testSizes :: IO ()
testSizes = compareSizes compareSize

testZipCompression :: IO ()
testZipCompression = compareSizes compareZipCompression

testRawToJamSizes :: IO ()
testRawToJamSizes = compareSizes compareRawToJam

allSizeTests :: IO ()
allSizeTests = do
  putStrLn "zipFlat - jam"
  testSizes
  putStrLn "\nzipFlat - flat"
  testZipCompression
  putStrLn "\nflat - jam"
  testRawToJamSizes
