module Urbit.UrukRTS.OptToFast (optToFast) where

import ClassyPrelude             hiding (evaluate, fromList, try)
import Data.Primitive.Array
import Data.Primitive.SmallArray
import System.IO.Unsafe

import Control.Arrow    ((>>>))
import Data.Function    ((&))
import Numeric.Natural  (Natural)
import Numeric.Positive (Positive)
import Prelude          ((!!))

import qualified Urbit.UrukRTS.JetOptimize as O
import qualified Urbit.UrukRTS.Types       as F

--------------------------------------------------------------------------------

optToFast ∷ O.Code → F.Jet
optToFast (O.Code args nm bod exp lop) = F.Jet{..}
 where
  jArgs = fromIntegral args
  jName = nm
  jBody = bod
  jFast = compile jArgs jRegs exp
  jLoop = lop
  jRegs = numReg exp

numReg :: O.Val -> Int
numReg = go 0
 where
  maxi :: [Int] -> Int
  maxi []     = 0
  maxi (x:xs) = max x (maxi xs)

  go :: Int -> O.Val -> Int
  go acc = \case
    O.ValKal _ vs -> maxi (acc : fmap (go acc) vs)
    O.ValRec _    -> acc
    O.ValRef _ vs -> maxi (acc : fmap (go acc) vs)
    O.ValReg n vs -> maxi (count n : acc : fmap (go acc) vs)
    O.ValIff c t e xs ->
      maxi (acc : go acc c : go acc t : go acc e : fmap (go acc) xs)
    O.ValCas reg c l r xs ->
      maxi (count reg : acc : go acc c : go acc l : go acc r : fmap (go acc) xs)
    O.ValLet reg c k   xs ->
      maxi (count reg : acc : go acc c : go acc k : fmap (go acc) xs)

  count :: Natural -> Int
  count = fromIntegral . succ

{-
    TODO VAL (VFun ..)

    TODO Detect undersaturated calls
      CLON !Fun !(SmallArray Exp)    --  Undersaturated call

    TODO Detect fully saturated calls.
      (No AST node for this yet)
-}

compile :: Int -> Int -> O.Val -> F.Exp
compile arity numRegs = go
 where
  go = \case
    O.ValRec xs       -> rec xs
    O.ValRef n []     -> F.REF ((arity - 1) - fromIntegral n)
    O.ValRef n xs     -> F.CALN (F.REF ((arity - 1) - fromIntegral n)) (goArgs xs)
    O.ValReg n []     -> F.REG (fromIntegral n)
    O.ValReg n xs     -> F.CALN (F.REG (fromIntegral n)) (goArgs xs)
    O.ValIff i t e [] -> F.IFF (go i) (go t) (go e)
    O.ValIff i t e xs -> F.CALN (F.IFF (go i) (go t) (go e)) (goArgs xs)

    -- TODO Register Allocation.
    O.ValCas reg x l r [] -> F.CAS (int reg) (go x) (go l) (go r)
    O.ValCas reg x l r xs -> F.CALN (F.CAS (int reg) (go x) (go l) (go r)) (goArgs xs)
    O.ValLet reg x k   [] -> F.LET (int reg) (go x) (go k)
    O.ValLet reg x k   xs -> F.CALN (F.LET (int reg) (go x) (go k)) (goArgs xs)
    O.ValKal ur xs    -> kal ur xs

  int :: Natural -> Int
  int = fromIntegral

  rec [] = F.SLF
  rec xs =
    let len = length xs
    in case (compare len arity, xs) of
         (EQ, [x]         )    -> F.REC1 (go x)
         (EQ, [x, y]      )    -> F.REC2 (go x) (go y)
         (EQ, [x, y, z]   )    -> F.REC3 (go x) (go y) (go z)
         (EQ, [x, y, z, p])    -> F.REC4 (go x) (go y) (go z) (go p)
         (EQ, [x, y, z, p, q]) -> F.REC5 (go x) (go y) (go z) (go p) (go q)
         (EQ, xs          )    -> F.RECN (goArgs xs)
         (LT, xs          )    -> F.CALN F.SLF (goArgs xs) -- TODO
         (GT, xs          )    -> F.CALN F.SLF (goArgs xs) -- TODO

  kal F.Seq     [x, y] = F.SEQ (go x) (go y)
  kal F.Ded     [x]    = F.DED (go x)

  kal F.Uni     []     = F.VAL F.VUni

  kal F.Con     [x, y] = con (go x) (go y)
  kal F.Car     [x]    = F.CAR (go x)
  kal F.Cdr     [x]    = F.CDR (go x)

  kal F.Lef     [x]    = lef (go x)
  kal F.Rit     [x]    = rit (go x)

  kal (F.Nat n) []     = F.VAL (F.VNat n)
  kal (F.Int n) []     = F.VAL (F.VInt n)
  kal (F.Lis n) []     = F.VAL (F.VLis n)
  kal (F.Bol b) []     = F.VAL (F.VBol b)

  kal F.Inc     [x]    = F.INC (go x)
  kal F.Dec     [x]    = F.DEC (go x)
  kal F.Fec     [x]    = F.FEC (go x)
  kal F.Zer     [x]    = F.ZER (go x)
  kal F.Eql     [x, y] = F.EQL (go x) (go y)
  kal F.Add     [x, y] = F.ADD (go x) (go y)

  kal F.Lth     [x, y] = F.LTH (go x) (go y)
  kal F.Lsh     [x, y] = F.LSH (go x) (go y)
  kal F.Fub     [x, y] = F.FUB (go x) (go y)
  kal F.Not     [x]    = F.NOT (go x)
  kal F.Xor     [x,y]  = F.XOR (go x) (go y)
  kal F.Div     [x,y]  = F.DIV (go x) (go y)
  kal F.Tra     [x]    = F.TRA (go x)
  kal F.Mod     [x,y]  = F.MOD (go x) (go y)
  kal F.Rap     [x,y]  = F.RAP (go x) (go y)
  kal F.Gulf    [x,y]  = F.GULF (go x) (go y)
  kal F.Snag    [x,y]  = F.SNAG (go x) (go y)
  kal F.Turn    [x,y]  = F.TURN (go x) (go y)
  kal F.Weld    [x,y]  = F.WELD (go x) (go y)
  kal F.Zing    [x]    = F.ZING (go x)
  kal F.Ntot    [x]    = F.NTOT (go x)
  kal F.LCon    [x,y]  =
    case (go x, go y) of
      (F.VAL xv, F.VAL (F.VLis lv)) ->
        F.VAL $ F.VLis (xv : lv)
      (F.VAL xv, F.LNIL) ->
        F.VAL $ F.VLis [xv]
      (xe, ye) -> F.LCON (go x) (go y)

  kal F.LNil    []     = F.LNIL

  kal F.Sub     [x, y] = F.SUB (go x) (go y)
  kal F.Mul     [x, y] = F.MUL (go x) (go y)

  kal F.AddAssoc [a, b, c, d, e] =
    F.ADD_ASSOC (go a) (go b) (go c) (go d) (go e)
  kal F.FindAssoc [x, y, z] = F.FIND_ASSOC (go x) (go y) (go z)

  kal F.IntPositive [x] = F.INT_POSITIVE (go x)
  kal F.IntNegative [x] = F.INT_NEGATIVE (go x)

  kal F.IntAbs [x] = F.INT_ABS (go x)
  kal F.IntAdd [x,y] = F.INT_ADD (go x) (go y)
  kal F.IntDiv [x,y] = F.INT_DIV (go x) (go y)
  kal F.IntIsZer [x] = F.INT_IS_ZER (go x)
  kal F.IntIsNeg [x] = F.INT_IS_NEG (go x)
  kal F.IntIsPos [x] = F.INT_IS_POS (go x)
  kal F.IntLth [x,y] = F.INT_LTH (go x) (go y)
  kal F.IntMul [x,y] = F.INT_MUL (go x) (go y)
  kal F.IntNegate [x] = F.INT_NEGATE (go x)
  kal F.IntSub [x,y] = F.INT_SUB (go x) (go y)

  kal (F.Box x) [] = F.VAL (F.VBox x)
  kal F.Unbox [x] = F.UNBOX (go x)

  kal (F.Jut (j@F.Jet{ jArgs = 1 })) [x]
    = F.JET1 j (go x)

  kal (F.Jut (j@F.Jet{ jArgs = 2 })) [x,y]
    = F.JET2 j (go x) (go y)

  kal (F.Jut (j@F.Jet{ jArgs = 3 })) [x,y,z]
    = F.JET3 j (go x) (go y) (go z)

  kal (F.Jut (j@F.Jet{ jArgs = 4 })) [x,y,z,p]
    = F.JET4 j (go x) (go y) (go z) (go p)

  kal (F.Jut (j@F.Jet{ jArgs = 5 })) [x,y,z,p,q]
    = F.JET5 j (go x) (go y) (go z) (go p) (go q)

  kal (F.Jut j) xs
    | F.jArgs j == length xs
    = F.JETN j (fromList (go <$> xs))

  kal f []    = rawExp f

  kal (F.Jut j) [x]
    | F.jArgs j > 1
    = F.CLO1 (F.Fun (F.jArgs j) (F.Jut j) mempty) (go x)

  kal (F.Jut j) [x,y]
    | F.jArgs j > 2
    = F.CLO2 (F.Fun (F.jArgs j) (F.Jut j) mempty) (go x) (go y)

  kal (F.Jut j) [x,y,z]
    | F.jArgs j > 3
    = F.CLO3 (F.Fun (F.jArgs j) (F.Jut j) mempty) (go x) (go y) (go z)

  kal (F.Jut j) [x,y,z,p]
    | F.jArgs j > 4
    = F.CLO4 (F.Fun (F.jArgs j) (F.Jut j) mempty) (go x) (go y) (go z) (go p)

  kal (F.Jut j) [x,y,z,p,q]
    | F.jArgs j > 5
    = F.CLO5 (F.Fun (F.jArgs j) (F.Jut j) mempty) (go x) (go y) (go z) (go p) (go q)

  kal (F.Jut j) xs
    | F.jArgs j > length xs
    = F.CLON (F.Fun (F.jArgs j) (F.Jut j) mempty) (fromList (go <$> xs))

  kal f          xs    = F.CALN (rawExp f) (goArgs xs)

  con (F.VAL x) (F.VAL y) = F.VAL (F.VCon x y)
  con x         y         = F.CON x y

  lef (F.VAL x) = F.VAL (F.VLef x)
  lef x         = F.LEF x

  rit (F.VAL x) = F.VAL (F.VRit x)
  rit x         = F.RIT x

  goArgs :: [O.Val] -> SmallArray F.Exp
  goArgs = fromList . fmap go

nodeFun :: F.Node -> F.Fun
nodeFun n = F.Fun (F.nodeArity n) n mempty

rawExp :: F.Node -> F.Exp
rawExp = \case
  F.Nat n -> F.VAL (F.VNat n)
  F.Int i -> F.VAL (F.VInt i)
  F.Lis l -> F.VAL (F.VLis l)
  F.Bol b -> F.VAL (F.VBol b)
  F.Uni   -> F.VAL F.VUni
  n       -> F.VAL (F.VFun (nodeFun n))
