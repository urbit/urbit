{-
  Can de-duplication be orthogonal to serialization?
-}

module Data.Noun.Zip where

import ClassyPrelude

import Control.Applicative
import Control.Monad
import Data.Noun
import Data.Noun.Atom
import Data.Bits
import GHC.Generics
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Debug.Trace

import Data.List     (intercalate)
import Data.Typeable (Typeable)

import Control.Monad.State.Strict


-- External Types --------------------------------------------------------------

newtype Zip = Zip [ZipNode]
  deriving newtype (Eq, Ord, Show)


-- Internal Types --------------------------------------------------------------

data ZipNode
    = ZipAtom !Atom
    | ZipCell !Word !Word
  deriving (Eq, Ord, Show)

type ZipM a = State ([ZipNode], Word, Map Noun Word) a


--------------------------------------------------------------------------------

zip :: Noun -> Zip
zip = \n -> evalState (go n >> end) ([], 0, mempty)
  where
    end :: ZipM Zip
    end = do
      (acc, _, _) <- get
      pure (Zip $ reverse acc)

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

unzip :: Zip -> Maybe Noun
unzip = undefined
