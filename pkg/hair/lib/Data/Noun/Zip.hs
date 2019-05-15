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

import Data.List     (intercalate)
import Data.Typeable (Typeable)

import Control.Monad.State.Strict hiding (forM_)
import Control.Monad.Trans.Maybe

import qualified Data.Vector as V
import qualified Data.List   as L

import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck


-- External Types --------------------------------------------------------------

newtype Zip = Zip (Vector ZipNode)
  deriving newtype (Eq, Ord, Show)


-- Internal Types --------------------------------------------------------------

data ZipNode
    = ZipAtom !Atom
    | ZipCell !Word !Word
  deriving (Eq, Ord, Show)


--------------------------------------------------------------------------------

tag :: Bool -> Buf -> Buf
tag bit buf = (if bit then Buf 1 1 else Buf 1 0) <> buf

jamZipNode :: ZipNode -> Buf
jamZipNode (ZipAtom a)   = tag False (mat a)
jamZipNode (ZipCell l r) = tag True (mat (toAtom l) <> mat (toAtom r))

jamZip :: Zip -> Buf
jamZip (Zip vec) = fold (length : nodes)
  where
    length = mat (toAtom (V.length vec))
    nodes  = jamZipNode <$> V.toList vec

cueZip :: Buf -> Maybe Zip
cueZip = undefined


-- Zip -------------------------------------------------------------------------

type ZipM a = State ([ZipNode], Word, Map Noun Word) a

zip :: Noun -> Zip
zip = \n -> evalState (go n >> end) ([], 0, mempty)
  where
    end :: ZipM Zip
    end = do
      (acc, _, _) <- get
      pure (Zip $ V.fromList $ reverse acc)

    ins :: Noun -> ZipNode -> ZipM Word
    ins noun node = do
      (acc, nex, tbl) <- get
      put (node:acc, nex+1, insertMap noun nex tbl)
      pure nex

    go :: Noun -> ZipM Word
    go noun = do
      (acc, nex, tbl) <- get
      case (lookup noun tbl, noun) of
        (Just w,  _)        -> pure w
        (Nothing, Atom atm) -> ins noun (ZipAtom atm)
        (Nothing, Cell l r) -> (ZipCell <$> go l <*> go r) >>= ins noun


-- Unzip -----------------------------------------------------------------------

type UnZipM a = MaybeT (State (Word, Map Word Noun)) a

unzip :: Zip -> Maybe Noun
unzip (Zip vec) | V.length vec == 0 = Nothing
unzip (Zip vec) =
    L.last <$> cvt (V.toList vec)
  where
    cvt :: [ZipNode] -> Maybe [Noun]
    cvt nodes = evalState (runMaybeT $ go nodes) (0, mempty)

    ins :: Noun -> UnZipM Noun
    ins noun = do
      modify $ \(nex, tbl) -> (nex+1, insertMap nex noun tbl)
      pure noun

    find :: Word -> UnZipM Noun
    find idx = do
      (nex, tbl) <- get
      lookup idx tbl & \case
        Nothing  -> error "bad zip"
        Just res -> pure res

    go :: [ZipNode] -> UnZipM [Noun]
    go = mapM $ \case ZipAtom a   -> ins (Atom a)
                      ZipCell l r -> ins =<< Cell <$> find l <*> find r


-- Tests -----------------------------------------------------------------------

compareSize :: Noun -> (Int, Int)
compareSize n = (jamSz, zipSz)
  where
    Buf jamSz _ =  fromAtom (jam n)
    Buf zipSz _ =  jamZip (zip n)

prop_zipUnzip :: Noun -> Bool
prop_zipUnzip n = Just n == unzip (zip n)

main :: IO ()
main = $(defaultMainGenerator)
