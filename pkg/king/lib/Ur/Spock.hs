module Ur.Spock
    ( Vl, Sp(..), sp
    ) where

import Ur.Common


-- Types -----------------------------------------------------------------------

type Vl = Noun

data Sp
    = DIE
    | LEF
    | RIT
    | IDN
    | APP
    | CEL
    | INC
    | EQU
    | SET
    | COR -- HINT: Treat subject as a core.
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
        SET     -> "&"
        COR     -> "C"
        VAL n   -> show n
        CON x y -> "(" <> intercalate " " (show <$> prettyCon [x] y) <> ")"
        DOT x y -> mconcat [show x, show y]
        CND x y -> mconcat ["<", show x, " ", show y, ">"]
      where
        prettyCon acc (CON x y) = prettyCon (x:acc) y
        prettyCon acc x         = reverse (x:acc)


-- Interpreter -----------------------------------------------------------------

sp :: (Noun -> Sp) -> Sp -> Noun -> Noun
sp eval = go
  where
    go :: Sp -> Noun -> Noun
    go = curry $ \case
        ( COR,     x              ) -> x
        ( IDN,     x              ) -> x
        ( LEF,     C l _          ) -> l
        ( RIT,     C _ r          ) -> r
        ( SET,     C (A 0) (C o n)) -> error "bad-set"
        ( SET,     C (A a) (C o n)) -> spSet (ix a) o n
        ( SET,     _              ) -> error "bad-set"
        ( APP,     C l r          ) -> go (eval r) l
        ( CEL,     C _ _          ) -> A 0
        ( CEL,     A _            ) -> A 1
        ( INC,     A n            ) -> A (n+1)
        ( EQU,     C x y          ) -> if x == y then A 0 else A 1
        ( VAL x,   _              ) -> x
        ( CON x y, s              ) -> C (go x s) (go y s)
        ( DOT x y, s              ) -> (go x . go y) s
        ( CND x y, C (A 1) s      ) -> go x s
        ( CND x y, C (A 0) s      ) -> go y s
        ( DIE,     _              ) -> error "bad-nok"
        ( LEF,     A _            ) -> error "bad-lef"
        ( RIT,     A _            ) -> error "bad-rit"
        ( INC,     C _ _          ) -> error "bad-inc"
        ( APP,     A _            ) -> error "bad-app"
        ( EQU,     A _            ) -> error "bad-equ"
        ( CND _ _, _              ) -> error "bad-cnd"

    spSet :: Ix -> Noun -> Noun -> Noun
    spSet ix old new = go (unIx ix) old
      where
        go []     _       = new
        go (_:_)  (A _)   = error "bad-set"
        go (L:ds) (C l r) = C (go ds l) r
        go (R:ds) (C l r) = C l (go ds r)
