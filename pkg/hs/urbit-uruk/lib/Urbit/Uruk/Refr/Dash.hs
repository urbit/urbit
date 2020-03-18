{-# OPTIONS_GHC -Werror #-}

module Urbit.Uruk.Refr.Dash where

import ClassyPrelude

import Data.Function    ((&))
import Data.Hashable    (Hashable)
import Numeric.Natural  (Natural)
import Numeric.Positive (Positive)

import qualified Urbit.Atom as Atom


-- Convienient Aliases ---------------------------------------------------------

type Nat = Natural
type Pos = Positive


-- Types -----------------------------------------------------------------------

infixl 5 :@;

data Val = Node :@ [Val]
 deriving (Eq, Ord, Generic, Hashable)

data Node
  = Jet Nat Val Val -- Unmatched Jet
  | S
  | K
  | J Nat
  | D
  | I
  | B
  | C
  | Sn Pos
  | Bn Pos
  | Cn Pos
  | Seq
  | Yet Nat
  | Fix
  | Nat Nat
  | Bol Bool
  | Iff
  | Pak
  | Zer
  | Eql
  | Add
  | Inc
  | Dec
  | Fec
  | Mul
  | Bex
  | Lsh
  | Sub
  | Ded
  | Uni
  | Lef
  | Rit
  | Cas
  | Con
  | Car
  | Cdr
 deriving (Eq, Ord, Generic, Hashable)


-- Instances -------------------------------------------------------------------

instance Hashable Positive where
  hashWithSalt i p = hashWithSalt i (fromIntegral p :: Natural)

instance Show Val where
  show (n :@ []) = show n
  show (n :@ xs) = "(" <> intercalate " " (show n : fmap show xs) <> ")"

v :: Node -> Val
v x = x :@ []

instance Show Node where
  show = \case
    S         -> "S"
    K         -> "K"
    J n       -> replicate (fromIntegral n) 'J'
    D         -> "D"
    Jet n t b -> "[" <> intercalate " " [show (J n), show t, show b] <> "]"
    Nat n     -> showNat n
    I         -> "I"
    B         -> "B"
    C         -> "C"
    Bn  n     -> "B" <> show n
    Cn  n     -> "C" <> show n
    Sn  n     -> "S" <> show n
    Bol True  -> "Y"
    Bol False -> "N"
    Yet n     -> "W" <> show n
    Seq       -> "seq"
    Pak       -> "pak"
    Fix       -> "fix"
    Iff       -> "iff"
    Add       -> "add"
    Eql       -> "eql"
    Zer       -> "zer"
    Inc       -> "inc"
    Dec       -> "dec"
    Fec       -> "fec"
    Mul       -> "mul"
    Bex       -> "bex"
    Lsh       -> "lsh"
    Sub       -> "sub"
    Lef       -> "lef"
    Rit       -> "rit"
    Cas       -> "cas"
    Con       -> "con"
    Car       -> "car"
    Cdr       -> "cdr"
    Ded       -> "err"
    Uni       -> "uni"
   where
    showNat :: Natural -> String
    showNat n = Atom.atomUtf8 n & \case
      Right t | validSym (unpack t) -> "\"" <> unpack t <> "\""
      _                             -> show n

    validSym []     = False
    validSym "$"    = True
    validSym (x:xs) = headChr x && all bodyChr xs

    headChr :: Char -> Bool
    headChr = flip elem (lower <> upper)

    bodyChr :: Char -> Bool
    bodyChr = flip elem (lower <> upper <> digit <> "-")

    lower = ['a'..'z']
    upper = ['A'..'Z']
    digit = ['0'..'9']


-- Jet Dashabord ---------------------------------------------------------------

{-
    TODO use the Moon compiler and template haskell to generate tables:

    - nodeArity :: Node -> Nat
    - nodeVal   :: Node -> Val
    - match     :: Nat -> Val -> Val -> Node
-}


{-
::
++  C  ::  \f g x -> f x g
  ~/  3  C
  (S :@ (K :@ (S :@ (K :@ (S :@ S :@ (K :@ K))) :@ K)) :@ S)
::
++  B  ::  \f g x -> f (g x)
  ~/  3  B
  (S (K S) K)
::
++  mul  ::  \x y -> pak (\i z -> x (y i) z)
  %-  (J J K)
  (S (K (S (K pak))) (S (K S) K))
::
++  lsh  ::  \exp num -> (mul (bex exp) num)
  ~/  2  lsh
  (S (K mul) bex)
-}
