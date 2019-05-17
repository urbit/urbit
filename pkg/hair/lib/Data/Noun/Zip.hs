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

import Data.List     (intercalate)
import Data.Typeable (Typeable)
import Data.Word

import Control.Monad.State.Strict hiding (forM_, replicateM)
import Control.Monad.Trans.Maybe

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
  deriving stock    (Eq, Ord, Show, Generic)
  deriving anyclass Flat

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

-- Zip -------------------------------------------------------------------------

type ZipM a = State ([ZipNode], ZipAtom, Map Noun ZipAtom) a

findDups :: Noun -> Set Noun
findDups = keysSet . filterMap ((> 1) . toAtom) . go mempty
  where
    ins :: Noun -> Map Noun ZipAtom -> Map Noun ZipAtom
    ins = alterMap (Just . maybe (fromAtom 1) (fromAtom . (+1) . toAtom))

    go :: Map Noun ZipAtom -> Noun -> Map Noun ZipAtom
    go acc a@(Atom _)   = ins a acc
    go acc c@(Cell l r) = go (go (ins c acc) l) r

zzip :: Noun -> Zip
zzip = zip

zip :: Noun -> Zip
zip top = evalState exec ([], fromAtom 0, mempty)
  where
    dups :: Set Noun
    dups = findDups top

    ins :: Noun -> ZipNode -> ZipM ZipRef
    ins noun node = do
      (acc, nex, tbl) <- get
      put (node:acc, (fromAtom (toAtom nex + 1)), insertMap noun nex tbl)
      pure (ZRIndex nex)

    doAtom :: Atom -> ZipM ZipRef
    doAtom a = do
      if a >= 128 && member (Atom a) dups
      then ins (Atom a) (ZipAtom (fromAtom a))
      else pure (ZRInline (ZipAtom (fromAtom a)))

    doCell :: (Noun, Noun) -> ZipM ZipRef
    doCell (l,r) = do
      lRef <- loop l
      rRef <- loop r
      let res = ZipCell lRef rRef
      if member (Cell l r) dups
      then ins (Cell l r) res
      else pure (ZRInline res)

    loop :: Noun -> ZipM ZipRef
    loop noun = do
      (acc, nex, tbl) <- get
      case (lookup noun tbl, noun) of
        (Just w,  _)        -> pure (ZRIndex w)
        (Nothing, Atom atm) -> doAtom atm
        (Nothing, Cell l r) -> doCell (l,r)

    exec :: ZipM Zip
    exec = loop top >>= \case
             ZRIndex _  -> error "Impossible -- duplicate top-level node"
             ZRInline x -> do (acc, _, _) <- get
                              pure (reverse acc, x)

-- Unzip -----------------------------------------------------------------------

type UnZipM a = MaybeT (State (ZipAtom, Map ZipAtom Noun)) a

unzip :: Zip -> Maybe Noun
unzip (dups, top) =
    evalState (runMaybeT (go dups >> root top)) (fromAtom 0, mempty)
  where
    root :: ZipNode -> UnZipM Noun
    root (ZipAtom a)   = pure (Atom (toAtom a))
    root (ZipCell l r) = Cell <$> find l <*> find r

    ins :: Noun -> UnZipM Noun
    ins noun = do
      modify $ \(nex, tbl) -> (fromAtom (toAtom nex+1), insertMap nex noun tbl)
      pure noun

    find :: ZipRef -> UnZipM Noun
    find (ZRInline (ZipAtom a))   = pure (Atom (toAtom a))
    find (ZRInline (ZipCell l r)) = Cell <$> find l <*> find r
    find (ZRIndex idx)            = do (nex, tbl) <- get
                                       (MaybeT . pure) $ lookup idx tbl

    go :: [ZipNode] -> UnZipM [Noun]
    go = mapM $ \case ZipAtom a   -> ins (Atom (toAtom a))
                      ZipCell l r -> ins =<< Cell <$> find l <*> find r


-- Tests -----------------------------------------------------------------------

compareSize :: Noun -> Int
compareSize n = flatSz - jamSz
  where
    Buf jamSz _ = fromAtom (jam n)
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
    Buf jamSz _ = fromAtom (jam n)

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
allAtoms n = Atom <$> [0..toAtom n]

allCells :: Int -> [Noun]
allCells 0 = allAtoms 1
allCells n = do
  a <- allAtoms (n*2 - 1)
  c <- allCells (n-1)
  [Cell c a, Cell a c, Cell c c]

allNouns :: Int -> [Noun]
allNouns sz = ordNub (allCells sz <> allAtoms (sz*2))

nounSizes :: (Noun -> Int) -> Int -> [(Int, Noun)]
nounSizes f sz = sort (allNouns sz <&> \n -> (f n, n))

jamSz :: Noun -> Int
jamSz = (\(Buf sz _) -> sz) . fromAtom . jam

showFlatZipSizes :: Int -> IO ()
showFlatZipSizes dep = traverse_ print (nounSizes (length . bits . zip) dep)

showJamSizes :: Int -> IO ()
showJamSizes dep = traverse_ print (nounSizes jamSz dep)

sumFlatZipSizes :: Int -> Int
sumFlatZipSizes dep = sum $ map fst (nounSizes (length . bits . zip) dep)

sumJamSizes :: Int -> Int
sumJamSizes dep = sum $ map fst (nounSizes jamSz dep)


compareSizes :: (Noun -> Int) -> IO ()
compareSizes f = do
  nouns <- join <$> (replicateM 50 (sample' (arbitrary :: Gen Noun)) :: IO [[Noun]])
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
