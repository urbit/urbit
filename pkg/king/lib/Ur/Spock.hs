module Ur.Spock where

import ClassyPrelude hiding (undefined)
import Noun


-- Types -----------------------------------------------------------------------

data Sp
    = DIE
    | LEF
    | RIT
    | IDN
    | APP
    | CEL
    | INC
    | EQU
    | VAL Noun
    | CON Sp Sp
    | DOT Sp Sp
    | CND Sp Sp

instance Show Sp where
    show = \case
        DIE     -> "!"
        LEF     -> "-"
        RIT     -> "+"
        IDN     -> "."
        APP     -> "*"
        CEL     -> "^"
        INC     -> "@"
        EQU     -> "="
        VAL n   -> show n
        CON x y -> mconcat ["(", show x, " ", show y, ")"]
        DOT x y -> mconcat [show y, show x]
        CND x y -> mconcat ["<", show x, " ", show y, ">"]

sp :: (Noun -> Sp) -> Sp -> Noun -> Noun
sp eval = go
  where
    go :: Sp -> Noun -> Noun
    go = curry $ \case
        ( LEF,     C l _     ) -> l
        ( RIT,     C _ r     ) -> r
        ( IDN,     x         ) -> x
        ( APP,     C l r     ) -> go (eval r) l
        ( CEL,     C _ _     ) -> A 0
        ( CEL,     A _       ) -> A 1
        ( INC,     A n       ) -> A (n+1)
        ( EQU,     C x y     ) -> if x == y then A 0 else A 1
        ( VAL x,   _         ) -> x
        ( CON x y, s         ) -> C (go x s) (go y s)
        ( DOT x y, s         ) -> (go x . go y) s
        ( CND x y, C (A 1) s ) -> go x s
        ( CND x y, C (A 0) s ) -> go y s
        ( _      , _         ) -> error "dead-spock"
