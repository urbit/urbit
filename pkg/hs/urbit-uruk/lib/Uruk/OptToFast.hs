module Uruk.OptToFast (optToFast) where

import ClassyPrelude    hiding (evaluate, try, fromList)
import System.IO.Unsafe
import Data.Primitive.Array

import Control.Arrow    ((>>>))
import Data.Function    ((&))
import Numeric.Natural  (Natural)
import Numeric.Positive (Positive)
import Prelude          ((!!))
import Uruk.JetDemo     (Ur, UrPoly(Fast))

import qualified Uruk.Fast        as F
import qualified Uruk.JetComp     as C
import qualified Uruk.JetDemo     as U
import qualified Uruk.JetOptimize as O

optToFast ∷ O.Code → F.Jet
optToFast (O.Code args nm bod exp) = F.Jet{..}
 where
  jArgs = fromIntegral args
  jName = fastVal nm
  jBody = fastVal bod
  jFast = compile jArgs exp
  jRegs = numReg exp

numReg ∷ O.Val → Int
numReg = const 0 -- TODO

{-
    TODO CAS !Int !Exp !Exp !Exp  --  Pattern Match
    TODO VAL (VFun ..)

    TODO Detect undersaturated calls
      CLON !Fun !(Array Exp)    --  Undersaturated call
    TODO Detect fully saturated calls.
      (No AST node for this yet)
    TODO Detect fully saturated calls to jets.
      JETN !Jet !(Array Exp)   --  Fully saturated call
      JET2 !Jet !Exp !Exp      --  Fully saturated call
-}
compile :: Int -> O.Val -> F.Exp
compile arity = go
 where
  go = \case
    O.ValRec xs       -> rec xs
    O.ValRef n []     -> F.REF ((arity - 1) - fromIntegral n)
    O.ValRef n xs     -> F.CALN (F.REF (fromIntegral n)) (goArgs xs)
    O.ValIff i t e [] -> F.IFF (go i) (go t) (go e)
    O.ValIff i t e xs -> F.CALN (F.IFF (go i) (go t) (go e)) (goArgs xs)
    O.ValCas x l r xs -> error "TODO"
    O.ValKal ur xs    -> kal ur xs

  rec xs =
    let len = length xs
    in  case (compare len arity, xs) of
          (EQ, [x])    -> F.REC1 (go x)
          (EQ, [x, y]) -> F.REC2 (go x) (go y)
          (EQ, xs    ) -> F.RECN (goArgs xs)
          (LT, xs    ) -> F.CALN F.SLF (goArgs xs) -- TODO
          (GT, xs    ) -> F.CALN F.SLF (goArgs xs) -- TODO

  kal O.RSeq [x,y]  = F.SEQ (go x) (go y)
  kal O.RDed [x]    = F.DED (go x)

  kal O.RUni []     = F.VAL F.VUni

  kal O.RCon [x,y]  = con (go x) (go y)
  kal O.RCar [x]    = F.CAR (go x)
  kal O.RCdr [x]    = F.CDR (go x)

  kal O.RLef [x]    = lef (go x)
  kal O.RRit [x]    = rit (go x)

  kal (O.RNat n) []   = F.VAL (F.VNat n)
  kal (O.RBol b) []   = F.VAL (F.VBol b)

  kal O.RInc [x]    = F.INC (go x)
  kal O.RDec [x]    = F.DEC (go x)
  kal O.RFec [x]    = F.FEC (go x)
  kal O.RZer [x]    = F.ZER (go x)
  kal O.REql [x,y]  = F.EQL (go x) (go y)
  kal O.RAdd [x, y] = F.ADD (go x) (go y)
  kal O.RSub [x, y] = F.SUB (go x) (go y)
  kal O.RMul [x, y] = F.MUL (go x) (go y)

  kal f      xs     = F.CALN (rawExp f) (goArgs xs)

  con (F.VAL x) (F.VAL y) = F.VAL (F.VCon x y)
  con x         y         = F.CON x y

  lef (F.VAL x) = F.VAL (F.VLef x)
  lef x         = F.LEF x

  rit (F.VAL x) = F.VAL (F.VRit x)
  rit x         = F.RIT x

  goArgs :: [O.Val] -> Array F.Exp
  goArgs = fromList . fmap go

rawExp ∷ O.RawNode → F.Exp
rawExp = error "TODO"

fastVal :: U.Val -> F.Val
fastVal = error "TODO"
