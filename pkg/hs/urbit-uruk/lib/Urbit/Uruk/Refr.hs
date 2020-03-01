{- |
    This is a reference implementation of the Uruk language.

    It evaluates Uruk by applying reduction repeatedly rules until
    we reach a normal form, and printing the expression after every
    reduction.

    This is a very slow way to implement Uruk, but is a useful debugging
    tool, since it's clearly correct.
-}

module Urbit.Uruk.Refr
  ( Ur(..)
  , Node(..)
  , Lef(..)
  , normalize
  , reduce
  , jam
  , church
  , pattern S
  , pattern K
  , pattern J
  , pattern D
  )
where

import ClassyPrelude
import Data.Bits

import Data.Function ((&))
import GHC.Natural   (Natural)
import Urbit.Atom    (utf8Atom)


-- Types -----------------------------------------------------------------------

infixl 5 :@;

data Node = Ess | Kay | Jay | Dee
 deriving (Eq, Ord)

data Ur = N Node | Ur :@ Ur
 deriving (Eq, Ord)

data Lef = L Node [Lef]
  deriving (Eq, Ord)

pattern S = N Ess
pattern K = N Kay
pattern J = N Jay
pattern D = N Dee


-- Conversion back and forth from Lef ------------------------------------------

toLef :: Ur -> Lef
toLef = go []
 where
  go :: [Lef] -> Ur -> Lef
  go acc = \case
    N n    -> L n acc
    x :@ y -> go (toLef y : acc) x

appN :: Ur -> [Ur] -> Ur
appN acc []     = acc
appN acc (x:xs) = appN (acc :@ x) xs

fromLef :: Lef -> Ur
fromLef (L n xs) = appN (N n) (fromLef <$> xs)


-- Printing --------------------------------------------------------------------

instance Show Node where
  show Ess = "S"
  show Kay = "K"
  show Jay = "J"
  show Dee = "D"

instance Show Ur where
  show = show . toLef

instance Show Lef where
  show (L n []) = show n
  show (L n xs) = "(" <> intercalate " " (show n : fmap show xs) <> ")"


-- Serialization ---------------------------------------------------------------

{- |
    Produces a jetted, church-encoded natural number.
-}
church :: Natural -> Ur
church = jetNat . go
 where
  go 0 = S :@ K
  go n = S :@ (S :@ (K :@ S) :@ K) :@ go (pred n)

  jetNat = (J :@ J :@ K :@)

{- |
    Serialize and Uruk expression and church-encode it.
-}
jam :: Ur -> Ur
jam = church . snd . go
 where
  go :: Ur -> (Int, Natural)
  go (N Ess)  = (3, 4)
  go (N Kay)  = (3, 2)
  go (N Jay)  = (3, 0)
  go (N Dee)  = (3, 6)
  go (x :@ y) = (rBits, rNum)
   where
    (xBits, xNum) = go x
    (yBits, yNum) = go y
    rBits         = 1 + xBits + yBits
    rNum          = 1 .|. shiftL xNum 1 .|. shiftL yNum (1 + xBits)


--------------------------------------------------------------------------------

{- |
    Perform a single reduction on an Uruk tree.
-}
reduce :: Ur -> Maybe Ur
reduce = \case
  K :@ x :@ y              -> Just x
  (reduce -> Just xv) :@ y -> Just (xv :@ y)
  x :@ (reduce -> Just yv) -> Just (x :@ yv)
  S :@ x :@ y :@ z         -> Just (x :@ z :@ (y :@ z))
  D :@ x                   -> Just (jam x)
  (jetRule -> Just (b,xs)) -> Just (appN b xs)
  _                        -> Nothing
 where
  jetRule :: Ur -> Maybe (Ur, [Ur])
  jetRule ur = do
    (n, rest) <- jetHead (toLef ur)
    (b, xs)   <- rest & \case { _:b:xs -> Just (b,xs); _ -> Nothing }
    guard (n == len xs)
    pure (fromLef b, fromLef <$> xs)

  len :: [a] -> Natural
  len = fromIntegral . length

  jetHead :: Lef -> Maybe (Natural, [Lef])
  jetHead (L n xs) = if n == Jay then Just (go 1 xs) else Nothing
   where
    go n (L Jay [] : xs) = go (succ n) xs
    go n xs              = (n, xs)


-- Evaluation ------------------------------------------------------------------

{- |
    Repeatedly perform reductions until the input is fully normalized.
-}
normalize :: Ur -> IO Ur
normalize ur = do
  putStrLn (">>  " <> tshow ur)
  reduce ur & \case
    Nothing -> pure ur
    Just ru -> normalize ru
