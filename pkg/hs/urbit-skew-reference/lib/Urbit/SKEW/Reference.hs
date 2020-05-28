module Urbit.SKEW.Reference
  ( Expr(..)
  , SKEW(..)
  , step
  , eval
  )
where

import Control.Monad   (guard)
import Data.Foldable   (foldl')
import Data.List       (intercalate)
import Data.Tree       (Tree(Node))
import Numeric.Natural (Natural)

infixl 5 :@;

data SKEW = S | K | E | W deriving (Eq, Ord, Show)
data Expr = N SKEW | Expr :@ Expr deriving (Eq, Ord)

tree :: Expr -> Tree SKEW
tree = go [] where go a = \case { N n -> Node n a; x :@ y -> go (tree y:a) x }

unTree :: Tree SKEW -> Expr
unTree (Node n xs) = foldl' (:@) (N n) (unTree <$> xs)

showTree :: Tree SKEW -> String
showTree (Node n []) = show n
showTree (Node n xs) = "(" <> intercalate " " (show n : fmap showTree xs) <> ")"

instance Show Expr where show = showTree . tree

eval :: Expr -> Expr
eval = \case { (step -> Just x) -> eval x; x -> x }

step :: Expr -> Maybe Expr
step = \case
  N K :@ x :@ y                            -> Just x
  (step -> Just xv) :@ y                   -> Just (xv :@ y)
  x                 :@ (step -> Just yv)   -> Just (x :@ yv)
  (jetRule -> Just (b,xs))                 -> Just (foldl' (:@) b xs)
  N S :@ x :@ y :@ z                       -> Just (x :@ z :@ (y :@ z))
  N W :@ a :@ s :@ k :@ e :@ w :@ (x :@ y) -> Just (a :@ x :@ y)
  N W :@ a :@ s :@ k :@ e :@ w :@ N S      -> Just s
  N W :@ a :@ s :@ k :@ e :@ w :@ N K      -> Just k
  N W :@ a :@ s :@ k :@ e :@ w :@ N E      -> Just e
  N W :@ a :@ s :@ k :@ e :@ w :@ N W      -> Just w
  _                                        -> Nothing

jetRule :: Expr -> Maybe (Expr, [Expr])
jetRule x = do
  (n, rest) <- jetHead (tree x)
  (b, xs)   <- case rest of { t:b:xs -> Just (b,xs); _ -> Nothing }
  guard (fromIntegral n == length xs)
  Just (unTree b, unTree <$> xs)

jetHead :: Tree SKEW -> Maybe (Natural, [Tree SKEW])
jetHead (Node n xs) = guard (n == E) >> pure (go 1 xs)
 where go n (Node E [] : xs) = go (succ n) xs
       go n xs             = (n, xs)
