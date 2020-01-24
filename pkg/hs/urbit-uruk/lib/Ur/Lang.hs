{-
    Ur expressions:

    Call  --  Function application
    Iota  --  iota combinator: (\f → f (\xyz→xz(yz)) (\xy→x))
    Fast  --  Jet registration: (\n a v → v)
    Dumb  --  Encode a value as a church-encoded natural number.

    `Dumb` produces extremely inefficient encodings, but that's
    okay. Instead of using it directly, write a higher-level version
    and jet that. It will need to take the dumb encoding, de-serialize
    it, de-compile it into a higher-level language, and serialize that.

    Semantically, `Jet` ignores it's first two arguments (an arity and
    a name) and returns it's second. Operationally, `Jet` delays
    evaluation until the arguments to the jet are saturated.

    (\xy→x) a             ->  \y→a
    (\xy→x) a b           ->  a
    fast 2 "" (\xy→x) a   ->  fast 2 "" (\xy→x) 1
    fast 2 "" (\xy→x) a b ->  a

    Eval doesn't need to be a primitive. Ur is turing complete, so we
    can just implement eval in Ur and jet it.

    We can trivially implement the SKI combinators using jets.

    Naturals don't need to be baked in, since we can use jets to
    implement nats. The massive size of nats encoded with `dumb`
    doesn't matter, since we will never call it unjetted.

    ADTs don't need to be baked in. We can use implement those using jets,
    and achieve good on-disk representations using our higher-level eval
    and uneval.

    Fix might need to be built in? To avoid making it a primitive,
    I would need to show that it can be de-compiled from a jetter version
    of the z-combinator.
-}

module Ur.Lang where

import Ur.Common hiding (flat, A, succ, L, R, C)
import Data.Flat (Flat)
import GHC.Natural


-- Types -----------------------------------------------------------------------

infixl 5 :@;
data E = !E :@ !E
       | S
       | K
       | J !Natural !Text
       | D

       -- Recursion is not possible in a strict language without builtin case.
       | L
       | R
       | C

       -- Should not be necessary
       | F

       -- Should not be necessary
       | N !Natural
       | Inc
       | Fol
  deriving (Eq, Ord, Generic, NFData, Flat)

instance Show E where
    show = \case
        e@(_ :@ _) → "(" <> intercalate "" (show <$> appList [] e) <> ")"
        S     -> "S"
        K     -> "K"
        J n m -> "J{" <> show n <> "," <> show m <> "}"
        D     -> "D"
        L     -> "L"
        R     -> "R"
        C     -> "C"
        F     -> "F"
        N n   -> show n
        Inc   -> "+"
        Fol   -> "%"
      where
        appList acc (f :@ x) = appList (x:acc) f
        appList acc x        = x:acc
