module Ur.Dock
    ( Dc(..), Pm(..)
    , S(..)
    , PM, epm
    , ST, edc
    ) where

import Ur.Common hiding (GT, L, R)


-- Concise Sum Types -----------------------------------------------------------

data S a b = L a | R b
  deriving (Eq, Ord)

instance (Show a, Show b) => Show (S a b) where
    show (L x) = "L" <> show x
    show (R x) = "R" <> show x


-- Types -----------------------------------------------------------------------

{-
    Dependant Lambda Calculus with:

      - Fixed Point Operator
      - Jets
      - Eval (AST -> a -> b)
      - Some Built-Ins for Convenience:
        - Unit, Pairs, Sums, and Nats

    - FN - Lambda Abstraction
    - PI - Pi Abstraction
    - VR - Variable
    - AP - Function Application
    - FX - Fixed Point
    - EV - Eval
    - JT - Jet a value.
    - PM - Built-in Operations
-}
data Dc
    = FN Dc Dc
    | PI Dc Dc
    | VR At
    | AP Dc Dc
    | FX Dc
    | EV Dc
    | JT At Dc
    | PM Pm
  deriving (Eq, Ord, Show)

{-
    Builtins:

    UN -- Unit Value
    CN -- Cons
    GT -- Car/Cdr [0..] is [undefined, id, car, cdr, car.car, car.cdr, ...]
    GT -- Lef/Rit [0..] is [undefined, id, lef, rit, lef.lef, lef.rit, ...]
    SW -- Case
    NU -- Natural Literal
    UP -- Increment
    DN -- Decrement
-}
data Pm
    = UN
    | CN Dc Dc
    | GT At Dc
    | TG At Dc
    | SW Dc Dc Dc
    | NU At
    | UP Dc
    | DN Dc
  deriving (Eq, Ord, Show)

type UN = ()
type CN = (ST, ST)
type GT = (At, ST)
type TG = (At, ST)
type SW = (ST, ST, ST)
type NU = At
type UP = ST
type DN = ST

type PM =
  (S
    (S
      (S UN CN)
      (S GT TG))
    (S
      (S SW NU)
      (S UP DN)))

type FN = (ST, ST)
type PI = (ST, ST)
type VR = At
type AP = (ST, ST)
type FX = ST
type EV = ST
type JT = (At, ST)

data ST = ST
  (S
    (S
      (S FN PI)
      (S VR AP))
    (S
      (S FX EV)
      (S JT PM)))

instance Show ST where
    show (ST x) = show x


-- Encode Pm and Dc as Sum Types -----------------------------------------------

epm :: Pm -> PM
epm = \case
    UN        -> L $ L $ L $ ()
    CN x y    -> L $ L $ R $ (edc x, edc y)
    GT n x    -> L $ R $ L $ (n, edc x)
    TG n x    -> L $ R $ R $ (n, edc x)
    SW l r x  -> R $ L $ L $ (edc l, edc r, edc x)
    NU v      -> R $ L $ R $ v
    UP x      -> R $ R $ L $ edc x
    DN x      -> R $ R $ R $ edc x

edc :: Dc -> ST
edc = ST . \case
    FN t b    -> L $ L $ L $ (edc t, edc b)
    PI t b    -> L $ L $ R $ (edc t, edc b)
    VR n      -> L $ R $ L $ n
    AP f a    -> L $ R $ R $ (edc f, edc a)
    FX x      -> R $ L $ L $ edc x
    EV x      -> R $ L $ R $ edc x
    JT nm f   -> R $ R $ L $ (nm, edc f)
    PM p      -> R $ R $ R $ epm p
