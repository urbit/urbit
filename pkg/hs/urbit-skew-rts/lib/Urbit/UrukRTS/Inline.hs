{-# OPTIONS_GHC -Wall -Werror #-}

module Urbit.UrukRTS.Inline (inline) where

import ClassyPrelude

import Urbit.UrukRTS.Types

--port Text.Show.Pretty           (ppShow)

import Control.Monad.State.Strict (State, get, put, runState)
import Data.Primitive.SmallArray  (SmallArray)

import qualified GHC.Exts as GHC.Exts

--------------------------------------------------------------------------------

unlessRecur :: Jet -> Exp -> State Int Exp -> State Int Exp
unlessRecur j fallback _ | jLoop j = pure fallback
unlessRecur _ _ act      = act

{-
  - Stack references become register references (nextReg + i)
  - Register references get incremented by (nextReg + jArgs)
-}
doSubst :: Jet -> State Int Exp
doSubst Jet{..} = do
  nextReg <- get
  let argRegF i = nextReg + i
  let regRegF i = nextReg + i + jArgs
  put (nextReg + jArgs + jRegs)
  pure $ subst (argRegF, regRegF) jFast

bindArgs :: Int -> [Exp] -> Exp -> Exp
bindArgs reg args funBody = case args of
  []     -> funBody
  x : xs -> LET reg x (bindArgs (succ reg) xs funBody)

inlineJetNoLoop :: Jet -> [Exp] -> State Int Exp
inlineJetNoLoop j xs = do
  nextReg <- get
  newBody <- doSubst j
  let res = bindArgs nextReg xs newBody
  -- traceM ("\nINLINE:")
  -- traceM $ indent $ ppShow res
  pure res

-- indent :: String -> String
-- indent = unlines . fmap ("    | " <>) . lines

inlineJet1 :: Jet -> Exp -> State Int Exp
inlineJet1 j x = unlessRecur j (JET1 j x) $ inlineJetNoLoop j [x]

inlineJet2 :: Jet -> Exp -> Exp -> State Int Exp
inlineJet2 j x y = unlessRecur j (JET2 j x y) $ inlineJetNoLoop j [x,y]

inlineJet3 :: Jet -> Exp -> Exp -> Exp -> State Int Exp
inlineJet3 j x y z = unlessRecur j (JET3 j x y z) $ inlineJetNoLoop j [x,y,z]

inlineJet4 :: Jet -> Exp -> Exp -> Exp -> Exp -> State Int Exp
inlineJet4 j x y z p =
  unlessRecur j (JET4 j x y z p) $ inlineJetNoLoop j [x, y, z, p]

inlineJet5 :: Jet -> Exp -> Exp -> Exp -> Exp -> Exp -> State Int Exp
inlineJet5 j x y z p q =
  unlessRecur j (JET5 j x y z p q) $ inlineJetNoLoop j [x, y, z, p, q]

inlineJetN :: Jet -> SmallArray Exp -> State Int Exp
inlineJetN j xs =
  unlessRecur j (JETN j xs) $ inlineJetNoLoop j (GHC.Exts.toList xs)

-- data Fun = Fun
  -- { fNeed :: !Int
  -- , fHead :: !Node
  -- , fArgs :: CloN -- Lazy on purpose.
  -- }

staticJetOneArgLeft :: Exp -> Maybe (Jet, [Exp])
staticJetOneArgLeft = \case
  VAL (VFun f)     -> doFun f []
  CLO1 f x         -> doFun f [x]
  CLO2 f x y       -> doFun f [x, y]
  CLO3 f x y z     -> doFun f [x, y, z]
  CLO4 f x y z p   -> doFun f [x, y, z, p]
  CLO5 f x y z p q -> doFun f [x, y, z, p, q]
  CLON f xs        -> doFun f (GHC.Exts.toList xs)
  _                -> Nothing
 where
  doFun :: Fun -> [Exp] -> Maybe (Jet, [Exp])
  doFun (Fun {..}) xs = do
    let expectedArgs = length xs + 1
    -- This can be relaxed! We can take closure params and put them on
    -- the stack.
    guard (mempty == fArgs)
    guard (fNeed == expectedArgs)
    j <- nodeJut fHead
    guard (jArgs j == expectedArgs)
    pure (j, xs)

  nodeJut :: Node -> Maybe Jet
  nodeJut (Jut j) = pure j
  nodeJut _       = Nothing

inlineTurn :: Exp -> Exp -> State Int Exp
inlineTurn lis f = case staticJetOneArgLeft f of
  Nothing -> do
    -- traceM ("\nNOINLINE.TURN:")
    -- traceM $ indent $ ppShow f
    pure (TURN lis f)
  Just (j, xs) -> unlessRecur j (TURN lis f) $ do
    nextReg <- get
    newBody <- doSubst j
    turnReg <- pure (nextReg + (jArgs j - 1)) -- last argument
    res     <- pure $ bindArgs nextReg xs $ FOR turnReg lis $ newBody
    -- traceM ("\nINLINE.TURN:")
    -- traceM $ indent $ ppShow res
    pure res

inline :: Jet -> Jet
inline j@Jet{..} =
  let (bod, reg) = runState (go jFast) jRegs
  in  j { jFast = bod, jRegs = reg }
 where
  go :: Exp -> State Int Exp
  go = \case
    JET1 f x         -> join (inlineJet1 f <$> go x)
    JET2 f x y       -> join (inlineJet2 f <$> go x <*> go y)
    JET3 f x y z     -> join (inlineJet3 f <$> go x <*> go y <*> go z)
    JET4 f x y z p   -> join (inlineJet4 f <$> go x <*> go y <*> go z <*> go p)
    JET5 f x y z p q -> join (inlineJet5 f <$> go x <*> go y <*> go z <*> go p <*> go q)
    JETN f xs        -> join (inlineJetN f <$> traverse go xs)
    TURN l f         -> join (inlineTurn <$> go l <*> go f)

    -- Boilerplate for traversal. Can be abstracted over, but too lazy
    -- to do it now.

    VAL v -> pure $ VAL v
    REF i -> pure $ REF i
    REG i -> pure $ REG i
    SLF -> pure $ SLF

    IFF x t e -> IFF <$> go x <*> go t <*> go e
    CAS i x y z -> CAS i <$> go x <*> go y <*> go z
    LET i x y -> LET i <$> go x <*> go y
    THE x y -> THE <$> go x <*> go y
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

    ADD_ASSOC a b c d e ->
      ADD_ASSOC <$> go a <*> go b <*> go c <*> go d <*> go e
    FIND_ASSOC a b c -> FIND_ASSOC <$> go a <*> go b <*> go c

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

    BOX x -> BOX <$> go x
    UNBOX x -> UNBOX <$> go x

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

subst :: (Int -> Int, Int -> Int) -> Exp -> Exp
subst (refReg, regReg) = go
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
    REF i -> REG (refReg i)
    REG i -> REG (regReg i)
    SLF -> SLF

    IFF x t e -> IFF (go x) (go t) (go e)
    CAS i x y z -> CAS (regReg i) (go x) (go y) (go z)
    LET i x y -> LET (regReg i) (go x) (go y)
    THE x y -> THE (go x) (go y)
    FOR i l f -> FOR (regReg i) (go l) (go f)

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

    ADD_ASSOC a b c d e -> ADD_ASSOC (go a) (go b) (go c) (go d) (go e)
    FIND_ASSOC a b c -> FIND_ASSOC (go a) (go b) (go c)

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

    BOX x -> BOX (go x)
    UNBOX x -> UNBOX (go x)

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
