module Urbit.UrukRTS.Inline (inline) where

import ClassyPrelude

import Urbit.UrukRTS.Types (Exp(..))
import Data.Primitive.SmallArray

--port qualified Urbit.UrukRTS.JetOptimize as O
import qualified Urbit.UrukRTS.Types       as F

--------------------------------------------------------------------------------

inlineJet1 :: F.Jet -> Exp -> Exp
inlineJet1 j x =
  case F.jLoop j of
    True -> JET1 j x
    False -> JET1 j x
 -- False -> LET 0 x (F.jFast j) -- replace REF with REG

inlineJet2 :: F.Jet -> Exp -> Exp -> Exp
inlineJet2 = JET2

inlineJet3 :: F.Jet -> Exp -> Exp -> Exp -> Exp
inlineJet3 = JET3

inlineJet4 :: F.Jet -> Exp -> Exp -> Exp -> Exp -> Exp
inlineJet4 = JET4

inlineJet5 :: F.Jet -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp
inlineJet5 = JET5

inlineJetN :: F.Jet -> SmallArray Exp -> Exp
inlineJetN = JETN

inline :: F.Jet -> F.Jet
inline j@F.Jet{..} = j { F.jFast = go jFast }
 where
  go :: Exp -> Exp
  go = \case
    JET1 j x         -> inlineJet1 j (go x)
    JET2 j x y       -> inlineJet2 j (go x) (go y)
    JET3 j x y z     -> inlineJet3 j (go x) (go y) (go z)
    JET4 j x y z p   -> inlineJet4 j (go x) (go y) (go z) (go p)
    JET5 j x y z p q -> inlineJet5 j (go x) (go y) (go z) (go p) (go q)
    JETN j xs        -> inlineJetN j (go <$> xs)

    -- Boilerplate for traversal. Can be abstracted over, but too lazy
    -- to do it now.

    VAL v -> VAL v
    REF i -> REF i
    REG i -> REG i
    SLF -> SLF

    IFF x t e -> IFF (go x) (go t) (go e)
    CAS i x y z -> CAS i (go x) (go y) (go z)
    LET i x y -> LET i (go x) (go y)
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
