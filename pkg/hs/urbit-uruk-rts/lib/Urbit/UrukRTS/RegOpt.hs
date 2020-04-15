{-- OPTIONS_GHC -Wall -Werror #-}

{- |
  1. Let bindings with one use site can be eliminated:

     ```
     (LET 9 expr (INC (REG 9)))
       ->  (INC expr)
     ```

  1. Trivial Expressions can always be replaced.

     ```
     (LET 9 (Val VUni) (CON (REG 9) (REG 9)))
       -> (CON (Val VUni) (Val VUni))
     (LET 9 (REG 8) (CON (REG 9) (REG 9)))
       -> (CON (REG 8) (REG 8))
     (LET 9 (REF 0) (CON (REG 9) (REG 9)))
       -> (CON (REF 0) (REF 0))
     ```

-}

module Urbit.UrukRTS.RegOpt (regOpt) where

import ClassyPrelude

import Urbit.UrukRTS.Types

import Control.Monad.State.Strict (State, get, put, runState)


--------------------------------------------------------------------------------

regOpt :: Jet -> Jet
-- Opt j = j { jFast = traceShowId $ noUseless (traceShowId $ jFast j) }
regOpt j = j { jFast = noUseless (jFast j) }

isTrivial :: Exp -> Bool
isTrivial = \case
  VAL _ -> True
  SLF   -> True
  REF _ -> True
  REG _ -> True
  LNIL  -> True
  _     -> False

noUseless :: Exp -> Exp
noUseless = go
 where
  go :: Exp -> Exp
  go = \case
    JET1 j x         -> JET1 j (go x)
    JET2 j x y       -> JET2 j (go x) (go y)
    JET3 j x y z     -> JET3 j (go x) (go y) (go z)
    JET4 j x y z p   -> JET4 j (go x) (go y) (go z) (go p)
    JET5 j x y z p q -> JET5 j (go x) (go y) (go z) (go p) (go q)
    JETN j xs        -> JETN j (go <$> xs)

    -- Boilerplate for traversal. Can be abstracted over, but too lazy
    -- to do it now.

    VAL v -> VAL v
    REF i -> REF i
    REG i -> REG i
    SLF -> SLF

    REC1 x -> REC1 (go x)
    REC2 x y -> REC2 (go x) (go y)
    REC3 x y z -> REC3 (go x) (go y) (go z)
    REC4 x y z p -> REC4 (go x) (go y) (go z) (go p)
    REC5 x y z p q -> REC5 (go x) (go y) (go z) (go p) (go q)
    RECN xs -> RECN (go <$> xs)

    SEQ x y -> SEQ (go x) (go y)
    DED x -> DED (go x)

    INC x -> INC (go x)
    DEC x -> DEC (go x)
    FEC x -> FEC (go x)
    ADD x y -> ADD (go x) (go y)
    MUL x y -> MUL (go x) (go y)

    LSH x y -> LSH (go x) (go y)
    LTH x y -> LTH (go x) (go y)
    FUB x y -> FUB (go x) (go y)
    NOT x -> NOT (go x)
    XOR x y -> XOR (go x) (go y)
    DIV x y -> DIV (go x) (go y)
    TRA x -> TRA (go x)
    MOD x y -> MOD (go x) (go y)
    RAP x y -> RAP (go x) (go y)
    TURN x y -> TURN (go x) (go y)
    SNAG x y -> SNAG (go x) (go y)
    WELD x y -> WELD (go x) (go y)
    ZING x -> ZING (go x)
    NTOT x -> NTOT (go x)

    INT_POSITIVE x -> INT_POSITIVE (go x)
    INT_NEGATIVE x -> INT_NEGATIVE (go x)

    INT_ABS x -> INT_ABS (go x)
    INT_ADD x y -> INT_ADD (go x) (go y)
    INT_DIV x y -> INT_DIV (go x) (go y)
    INT_IS_ZER x -> INT_IS_ZER (go x)
    INT_IS_NEG x -> INT_IS_NEG (go x)
    INT_IS_POS x -> INT_IS_POS (go x)
    INT_LTH x y -> INT_LTH (go x) (go y)
    INT_MUL x y -> INT_MUL (go x) (go y)
    INT_NEGATE x -> INT_NEGATE (go x)
    INT_SUB x y -> INT_SUB (go x) (go y)

    SUB x y -> SUB (go x) (go y)
    ZER x -> ZER (go x)
    EQL x y -> EQL (go x) (go y)

    CON x y -> CON (go x) (go y)
    CAR x -> CAR (go x)
    CDR x -> CDR (go x)
    LEF x -> LEF (go x)
    RIT x -> RIT (go x)

    LCON x y -> LCON (go x) (go y)
    LNIL -> LNIL
    GULF x y -> GULF (go x) (go y)

    CLO1 f x         -> CLO1 f (go x)
    CLO2 f x y       -> CLO2 f (go x) (go y)
    CLO3 f x y z     -> CLO3 f (go x) (go y) (go z)
    CLO4 f x y z p   -> CLO4 f (go x) (go y) (go z) (go p)
    CLO5 f x y z p q -> CLO5 f (go x) (go y) (go z) (go p) (go q)
    CLON f xs -> CLON f (go <$> xs)
    CALN f xs -> CALN (go f) (go <$> xs)

    IFF x t e -> IFF (go x) (go t) (go e)
    CAS i x y z -> CAS i (go x) (go y) (go z)
    FOR i l f -> FOR i (go l) (go f)
    THE x y -> THE (go x) (go y)
    LET i x y ->
      let (x', y') = (go x, go y) in
        case regExp (i, x') y' of
          (_ , 0) -> THE x' y'
          (yr, 1) -> yr                -- Replace reference with expresssion.
          (yr, _) | isTrivial x' -> yr -- Replace reference with expresssion.
          (_,  _) -> LET i x' y'

regExp :: (Int, Exp) -> Exp -> (Exp, Int)
regExp (bound, toExpr) topExpr = runState (go topExpr) 0
 where
  go :: Exp -> State Int Exp
  go = \case
    JET1 f x         -> JET1 f <$> go x
    JET2 f x y       -> JET2 f <$> go x <*> go y
    JET3 f x y z     -> JET3 f <$> go x <*> go y <*> go z
    JET4 f x y z p   -> JET4 f <$> go x <*> go y <*> go z <*> go p
    JET5 f x y z p q -> JET5 f <$> go x <*> go y <*> go z <*> go p <*> go q
    JETN f xs        -> JETN f <$> traverse go xs
    TURN l f         -> TURN <$> go l <*> go f

    -- Boilerplate for traversal. Can be abstracted over, but too lazy
    -- to do it now.

    VAL v -> pure $ VAL v
    REF i -> pure $ REF i
    REG i | i == bound -> do
      count <- get
      put (count + 1)
      pure toExpr

    REG i -> pure $ REG i
    SLF -> pure $ SLF

    IFF x t e -> IFF <$> go x <*> go t <*> go e
    CAS i x y z -> CAS i <$> go x <*> go y <*> go z
    LET i x y -> LET i <$> go x <*> go y
    THE   x y -> THE   <$> go x <*> go y
    FOR i l b -> FOR i <$> go l <*> go b

    REC1 x -> REC1 <$> go x
    REC2 x y -> REC2 <$> go x <*> go y
    REC3 x y z -> REC3 <$> go x <*> go y <*> go z
    REC4 x y z p -> REC4 <$> go x <*> go y <*> go z <*> go p
    REC5 x y z p q -> REC5 <$> go x <*> go y <*> go z <*> go p <*> go q
    RECN xs -> RECN <$> traverse go xs

    SEQ x y -> SEQ <$> go x <*> go y
    DED x -> DED <$> go x

    INC x -> INC <$> go x
    DEC x -> DEC <$> go x
    FEC x -> FEC <$> go x
    ADD x y -> ADD <$> go x <*> go y
    MUL x y -> MUL <$> go x <*> go y

    LSH x y -> LSH <$> go x <*> go y
    LTH x y -> LTH <$> go x <*> go y
    FUB x y -> FUB <$> go x <*> go y
    NOT x -> NOT <$> go x
    XOR x y -> XOR <$> go x <*> go y
    DIV x y -> DIV <$> go x <*> go y
    TRA x -> TRA <$> go x
    MOD x y -> MOD <$> go x <*> go y
    RAP x y -> RAP <$> go x <*> go y

    SNAG x y -> SNAG <$> go x <*> go y
    WELD x y -> WELD <$> go x <*> go y
    ZING x -> ZING <$> go x
    NTOT x -> NTOT <$> go x

    INT_POSITIVE x -> INT_POSITIVE <$> go x
    INT_NEGATIVE x -> INT_NEGATIVE <$> go x

    INT_ABS x -> INT_ABS <$> go x
    INT_ADD x y -> INT_ADD <$> go x <*> go y
    INT_DIV x y -> INT_DIV <$> go x <*> go y
    INT_IS_ZER x -> INT_IS_ZER <$> go x
    INT_IS_NEG x -> INT_IS_NEG <$> go x
    INT_IS_POS x -> INT_IS_POS <$> go x
    INT_LTH x y -> INT_LTH <$> go x <*> go y
    INT_MUL x y -> INT_MUL <$> go x <*> go y
    INT_NEGATE x -> INT_NEGATE <$> go x
    INT_SUB x y -> INT_SUB <$> go x <*> go y

    SUB x y -> SUB <$> go x <*> go y
    ZER x -> ZER <$> go x
    EQL x y -> EQL <$> go x <*> go y

    CON x y -> CON <$> go x <*> go y
    CAR x -> CAR <$> go x
    CDR x -> CDR <$> go x
    LEF x -> LEF <$> go x
    RIT x -> RIT <$> go x

    LCON x y -> LCON <$> go x <*> go y
    LNIL -> pure LNIL
    GULF x y -> GULF <$> go x <*> go y

    CLO1 f x         -> CLO1 f <$> go x
    CLO2 f x y       -> CLO2 f <$> go x <*> go y
    CLO3 f x y z     -> CLO3 f <$> go x <*> go y <*> go z
    CLO4 f x y z p   -> CLO4 f <$> go x <*> go y <*> go z <*> go p
    CLO5 f x y z p q -> CLO5 f <$> go x <*> go y <*> go z <*> go p <*> go q
    CLON f xs -> CLON f <$> traverse go xs
    CALN f xs -> CALN <$> go f <*> traverse go xs
