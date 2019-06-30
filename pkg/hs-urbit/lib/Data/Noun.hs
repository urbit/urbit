module Data.Noun where

import Prelude hiding (all)

import Control.Applicative
import Control.Monad
import Data.Bits
import GHC.Generics
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Data.Flat hiding (getSize)

import ClassyPrelude  (Text, all, unpack)
import Data.Hashable  (Hashable)
import Data.List      (intercalate)
import Data.Noun.Atom (Atom)
import Data.Typeable  (Typeable)

import qualified Control.Monad.Fail as Fail
import qualified Data.Char          as C


-- Types -----------------------------------------------------------------------

data Cell = ACell !Noun !Noun
  deriving (Eq, Ord)

data Noun
    = Atom !Atom
    | Cell !Noun !Noun
  deriving stock    (Eq, Ord, Generic)
  deriving anyclass (Flat, Hashable)

data CellIdx = L | R
  deriving (Eq, Ord, Show)

type NounPath = [CellIdx]


-- Instances -------------------------------------------------------------------

instance Show Noun where
  show (Atom a)   = show a
  show (Cell x y) = fmtCell (show <$> (x : toTuple y))
    where
      fmtCell :: [String] -> String
      fmtCell xs = "[" <> intercalate " " xs <> "]"

instance Arbitrary Noun where
  arbitrary = resize 1000 go
    where
      dub x = Cell x x
      go = do
        sz  <- getSize
        (bit, bat :: Bool) <- arbitrary
        case (sz, bit, bat) of
          ( 0, _,     _    ) -> Atom <$> arbitrary
          ( _, False, _    ) -> Atom <$> arbitrary
          ( _, True,  True ) -> dub <$> arbitrary
          ( _, True,  _    ) -> scale (\x -> x-10) (Cell <$> go <*> go)


-- Predicates ------------------------------------------------------------------

isAtom :: Noun -> Bool
isAtom (Atom _)   = True
isAtom (Cell _ _) = False

isCell :: Noun -> Bool
isCell (Atom _)   = False
isCell (Cell _ _) = True


-- Tuples ----------------------------------------------------------------------

fromTuple :: [Noun] -> Noun
fromTuple []     = Atom 0
fromTuple [x]    = x
fromTuple (x:xs) = Cell x (fromTuple xs)

toTuple :: Noun -> [Noun]
toTuple (Cell x xs) = x : toTuple xs
toTuple atom        = [atom]


-- Lists -----------------------------------------------------------------------

fromList :: [Noun] -> Noun
fromList []     = Atom 0
fromList (x:xs) = Cell x (fromList xs)

toList :: Noun -> Maybe [Noun]
toList (Atom 0)    = Just []
toList (Atom _)    = Nothing
toList (Cell x xs) = (x:) <$> toList xs

example :: Noun
example = fromTuple [Atom 1337, Atom 1338, Atom 0]

exampleIO :: IO ()
exampleIO = do
  print example
