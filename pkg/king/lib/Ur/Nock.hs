{-
    CON  *[a [b c] d]        [*[a b c] *[a d]]
    GET  *[a 0 b]            /[b a]
    VAL  *[a 1 b]            b
    APP  *[a 2 b c]          *[*[a b] *[a c]]
    CEL  *[a 3 b]            ?*[a b]
    INC  *[a 4 b]            +*[a b]
    EQU  *[a 5 b c]          =[*[a b] *[a c]]
    IFF  *[a 6 b c d]        *[a *[[c d] 0 *[[2 3] 0 *[a 4 4 b]]]]
    DOT  *[a 7 b c]          *[*[a b] c]
    PSH  *[a 8 b c]          *[[*[a b] a] c]
    ARM  *[a 9 b c]          *[*[a c] 2 [0 1] 0 b]
    SET  *[a 10 [b c] d]     #[b *[a c] *[a d]]
    HNT  *[a 11 [b c] d]     *[[*[a c] *[a d]] 0 3]
    HNT  *[a 11 b c]         *[a c]
    DIE  *a                  *a

    Syntax:
        Nock:
            nock(a)             *a

        Cells are right-assiciative:
            [a b c]             [a [b c]]

        Is-Cell:
            ?[a b]              0
            ?a                  1

        Increment
            +[a b]              +[a b]
            +a                  1 + a

        Equals:
            =[a a]              0
            =[a b]              1

        Get:
            /[1 a]              a
            /[2 a b]            a
            /[3 a b]            b
            /[(a + a) b]        /[2 /[a b]]
            /[(a + a + 1) b]    /[3 /[a b]]
            /a                  /a

        Set:
            #[1 o n]            o
            #[(i + i) o n]      #[i [o /[(i + i + 1) n]] n]
            #[(i + i + 1) o c]  #[i [/[(i + i) n] o] n]
            #i                  #i
-}

module Ur.Nock where

import ClassyPrelude hiding (undefined)
import Noun

import Prelude  (undefined)
import Ur.Spock (Sp)

import qualified Ur.Spock as S


-- Types -----------------------------------------------------------------------

data Dr = L | R
type Ix = [Dr]
type Vl = Noun
type Ht = Noun -- Hint

data N4
    = DIE
    | VAL Vl
    | CON N4 N4
    | GET Ix
    | SET Ix N4 N4
    | APP N4 N4
    | CEL N4
    | INC N4
    | EQU N4 N4
    | IFF N4 N4 N4
    | DOT N4 N4
    | PSH N4 N4
    | ARM Ix N4
    | HNT Ht N4


-- Tree Indexing ---------------------------------------------------------------

ix :: Atom -> Ix
ix 0 = error "ix called with 0 value"
ix 1 = []
ix 2 = [L]
ix 3 = [R]
ix _ = error "TODO"


-- Hint Parsing ----------------------------------------------------------------

ht :: Noun -> Ht
ht = undefined


-- Execute Nock 4 --------------------------------------------------------------

get :: Ix -> Vl -> Vl
get = undefined

set :: Ix -> Vl -> Vl -> Vl
set = undefined

n4 :: N4 -> Vl -> Vl
n4 = curry $ \case
    (DIE,       _) -> error "crash"
    (VAL x,     _) -> x
    (CON x y,   s) -> C (n4 x s) (n4 y s)
    (GET i,     s) -> get i s
    (SET i x y, s) -> set i (n4 x s) (n4 y s)
    (APP x y,   s) -> undefined
    (CEL x,     s) -> undefined
    (INC x,     s) -> undefined
    (EQU x y,   s) -> undefined
    (IFF x y z, s) -> undefined
    (DOT x y,   s) -> n4 y (n4 x s)
    (PSH x y,   s) -> undefined
    (ARM x y,   s) -> undefined
    (HNT x y,   s) -> undefined


-- Compile Nock 4 --------------------------------------------------------------

c4 :: N4 -> Sp
c4 = \case
    DIE       -> S.DIE
    VAL x     -> S.VAL x
    CON x y   -> S.CON (c4 x) (c4 y)
    GET i     -> cGet i
    SET i x y -> undefined
    APP x y   -> S.DOT S.APP (S.CON (c4 x) (c4 y))
    CEL x     -> S.DOT S.CEL (c4 x)
    INC x     -> S.DOT S.INC (c4 x)
    EQU x y   -> S.DOT S.EQU (S.CON (c4 x) (c4 y))
    IFF x y z -> S.DOT (S.CND (c4 y) (c4 z))
                       (S.CON (c4 x) S.IDN)
    DOT x y   -> S.DOT (c4 x) (c4 y)
    PSH x y   -> S.DOT (c4 y) (S.CON (c4 x) S.IDN)
    ARM x y   -> c4 (DOT (APP (GET []) (GET x)) y)
    HNT x y   -> undefined

cGet :: [Dr] -> Sp
cGet []     = S.IDN
cGet [L]    = S.LEF
cGet [R]    = S.RIT
cGet (L:ds) = S.DOT (cGet ds) S.LEF
cGet (R:ds) = S.DOT (cGet ds) S.RIT

-- Load Nock 4 -----------------------------------------------------------------

l4 :: Vl -> N4
l4 = \case
    C x@(C _ _) y                 -> CON (l4 x) (l4 y)
    C (A 00)    (A 0)             -> DIE
    C (A 00)    (A x)             -> GET (ix x)
    C (A 01)    x                 -> VAL x
    C (A 02)    (C x y)           -> APP (l4 x) (l4 y)
    C (A 03)    x                 -> CEL (l4 x)
    C (A 04)    x                 -> INC (l4 x)
    C (A 05)    (C x y)           -> EQU (l4 x) (l4 y)
    C (A 06)    (C x (C y z))     -> IFF (l4 x) (l4 y) (l4 z)
    C (A 07)    (C x y)           -> DOT (l4 x) (l4 y)
    C (A 08)    (C x y)           -> PSH (l4 x) (l4 y)
    C (A 09)    (C (A x) y)       -> ARM (ix x) (l4 y)
    C (A 10)    (C (C (A x) y) z) -> SET (ix x) (l4 y) (l4 z)
    C (A 11)    (C x y)           -> HNT (ht x) (l4 y)
    _                             -> DIE


-- Dump Nock 4 -----------------------------------------------------------------

d4 :: N4 -> Vl
d4 = \case
    CON x y -> C (d4 x) (d4 y)
    DIE     -> C (A 1) (A 0)
    _       -> undefined
