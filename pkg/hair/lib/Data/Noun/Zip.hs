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


-- External Types --------------------------------------------------------------

data ZipNode
    = ZipAtom !Atom
    | ZipCell !ZipRef !ZipRef
  deriving stock    (Eq, Ord, Show, Generic)
  deriving anyclass Flat

data ZipRef
    = ZRInline !ZipNode
    | ZRIndex  !Word
  deriving stock    (Eq, Ord, Show, Generic)
  deriving anyclass Flat

type Zip = [ZipNode]

-- Zip -------------------------------------------------------------------------

type ZipM a = State ([ZipNode], Word, Map Noun Word) a

findDups :: Noun -> Set Noun
findDups = keysSet . filterMap (> 1) . go mempty
  where
    ins :: Noun -> Map Noun Word -> Map Noun Word
    ins = alterMap (Just . maybe 1 (+1))

    go :: Map Noun Word -> Noun -> Map Noun Word
    go acc a@(Atom _)   = ins a acc
    go acc c@(Cell l r) = go (go (ins c acc) l) r

zip :: Noun -> Zip
zip top = evalState (go top >> end) ([], 0, mempty)
  where
    dups :: Set Noun
    dups = findDups top

    end :: ZipM Zip
    end = do
      (acc, _, _) <- get
      pure (reverse acc)

    ins :: Noun -> ZipNode -> ZipM ZipRef
    ins noun node = do
      (acc, nex, tbl) <- get
      put (node:acc, nex+1, insertMap noun nex tbl)
      pure (ZRIndex nex)

    doAtom :: Atom -> ZipM ZipRef
    doAtom a = do
      if a >= 128 && member (Atom a) dups
      then ins (Atom a) (ZipAtom a)
      else pure (ZRInline (ZipAtom a))

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

    go :: Noun -> ZipM ZipRef
    go noun = do
      loop noun >>= \case
        ZRInline x -> ins noun x
        ZRIndex _  -> error "Impossible -- duplicate top-level node"

-- Unzip -----------------------------------------------------------------------

type UnZipM a = MaybeT (State (Word, Map Word Noun)) a

unzip :: Zip -> Maybe Noun
unzip = \case [] -> Nothing
              zs -> L.last <$> cvt zs
  where
    cvt :: [ZipNode] -> Maybe [Noun]
    cvt nodes = evalState (runMaybeT $ go nodes) (0, mempty)

    ins :: Noun -> UnZipM Noun
    ins noun = do
      modify $ \(nex, tbl) -> (nex+1, insertMap nex noun tbl)
      pure noun

    find :: ZipRef -> UnZipM Noun
    find (ZRInline (ZipAtom a))   = pure (Atom a)
    find (ZRInline (ZipCell l r)) = Cell <$> find l <*> find r
    find (ZRIndex idx)            = do (nex, tbl) <- get
                                       (MaybeT . pure) $ lookup idx tbl

    go :: [ZipNode] -> UnZipM [Noun]
    go = mapM $ \case ZipAtom a   -> ins (Atom a)
                      ZipCell l r -> ins =<< Cell <$> find l <*> find r


-- Tests -----------------------------------------------------------------------

compareSize :: Noun -> Int
compareSize n = flatSz - jamSz
  where
    Buf jamSz _ = fromAtom (jam n)
    flatSz      = UV.length (bits (zip n))

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

testSizes :: IO ()
testSizes = do
  nouns <- join <$> (replicateM 50 (sample' (arbitrary :: Gen Noun)) :: IO [[Noun]])
  traverse_ print $ reverse
                  $ ordNub
                  $ sort
                  $ fmap ((`div` 64) . compareSize)
                  $ nouns
  -- traverse_ print $ filter ((> 1000) . abs . compareSize) nouns
