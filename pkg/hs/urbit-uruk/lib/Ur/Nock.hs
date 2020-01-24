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

import Ur.Common hiding (undefined)

import Control.Lens ((&))
import Prelude      (undefined)
import Ur.Spock     (Sp, Vl)

import qualified Ur.Spock as S


-- Types -----------------------------------------------------------------------

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
  deriving (Eq, Ord, Show)

{-
        Get:
            /[1 a]              a
            /[2 a b]            a
            /[3 a b]            b
            /[(a + a) b]        /[2 /[a b]]
            /[(a + a + 1) b]    /[3 /[a b]]
            /a                  /a
-}

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
    SET i x y -> S.DOT S.SET (S.CON (S.VAL (A (axis i)))
                               (S.CON (c4 x) (c4 y)))
    APP x y   -> S.DOT S.APP (S.CON (c4 x) (c4 y))
    CEL x     -> S.DOT S.CEL (c4 x)
    INC x     -> S.DOT S.INC (c4 x)
    EQU x y   -> S.DOT S.EQU (S.CON (c4 x) (c4 y))
    IFF x y z -> S.DOT (S.CND (c4 y) (c4 z))
                       (S.CON (c4 x) S.IDN)
    DOT x y   -> S.DOT (c4 x) (c4 y)
    PSH x y   -> S.DOT (c4 y) (S.CON (c4 x) S.IDN)
    ARM x y   -> S.DOT (c4 (APP (GET (Ix [])) (GET x)))
                   (S.DOT S.COR (c4 y))
    HNT x y   -> c4 y -- XX TODO

cGet :: Ix -> Sp
cGet = go . unIx
  where
    go = \case
        []   -> S.IDN
        [L]  -> S.LEF
        [R]  -> S.RIT
        L:ds -> S.DOT (go ds) S.LEF
        R:ds -> S.DOT (go ds) S.RIT

n4Set :: Ix -> N4 -> N4 -> N4
n4Set ix new old = unIx ix & \case
    []   -> new
    L:ds -> n4Set (Ix ds) (CON new (get (R:ds) old)) old
    R:ds -> n4Set (Ix ds) (CON (get (L:ds) old) new) old
  where
    get :: [Dr] -> N4 -> N4
    get ds exp = DOT (GET $ Ix ds) exp

-- Nock 4 Examples -------------------------------------------------------------

-- [8 x@[9 2.398 0 1.023] y@[9 2 10 z@[6 7 [0 3] 4 1 3] 0 2]]
e4_dec :: Vl
e4_dec = C (A 8) (C x y)
  where
    x = C (A 9) $ C (A 2_389) $ C (A 0) (A 1_023)
    y = C (A 9) $ C (A 2) $ C (A 10) $ C z $ C (A 0) (A 2)
    z = C (A 6) $ C (A 7) $ C (C (A 0) (A 3)) $ C (A 4) $ C (A 1) (A 3)

{-
    |=  x=*  ?@(x +(x) [$(x -:x) $(x +:x)])
    [8 [1 0]
      [1 6 [6 [3 0 6] [1 1] 1 0]
        [4 0 6]
        [9 2 10 [6 0 12] 0 1]
        [9 2 10 [6 0 13] 0 1]]
      [0 1]]
-}
e4_inc_all :: Vl
e4_inc_all = gat `seq` bod
  where
    gat = C (A 8) $ C (C (A 1) (A 0)) $ C (C (A 1) bod) $ C (A 0) (A 1)
    foo = C (A 6) $ C (C (A 3) $ C (A 0) (A 6)) $ C (C (A 1) (A 1))
                                                $ C (A 1) (A 0)
    dat = C (A 4) $ C (A 0) (A 6)

    wut = C (A 9) $ C (A 2) $ C (A 10)
        $ C (C (A 6) $ C (A 0) (A 12))
        $ C (A 0) (A 1)

    luk = C (A 9)
        $ C (A 2)
        $ C (A 10)
        $ C (C (A 6) $ C (A 0) (A 13))
        $ C (A 0) (A 1)

    bod = C (A 6)
        $ C foo
        $ C dat
        $ C wut luk



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
