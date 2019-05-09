module Data.Noun where

import Prelude
import Numeric.Natural

import Data.List (intercalate)


--------------------------------------------------------------------------------

type Atom = Natural

data Cell = CCell !Noun !Noun

data Noun
    = Atom !Natural
    | Cell !Noun !Noun
  deriving (Eq, Ord)


-- Unboxed Atom Operations -----------------------------------------------------

cell2List :: Cell -> [Noun]
cell2List = go []
  where
    go acc (CCell x (Cell l r)) = go (x:acc) (CCell l r)
    go acc (CCell x y@(Atom _)) = reverse (y:x:acc)

list2Noun :: [Noun] -> Noun
list2Noun []     = Atom 0
list2Noun [x]    = x
list2Noun (x:xs) = Cell x (list2Noun xs)

instance Show Noun where
  show (Atom a)   = show a
  show (Cell x y) = fmtCell (fmap show (cell2List (CCell x y)))
    where
      fmtCell :: [String] -> String
      fmtCell xs = "[" <> intercalate " " xs <> "]"

example :: Noun
example = list2Noun [Atom 1337, Atom 1338, Atom 0]

exampleIO :: IO ()
exampleIO = do
  print example
