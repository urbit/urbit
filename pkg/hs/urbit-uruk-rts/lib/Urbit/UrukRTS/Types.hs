{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -funbox-strict-fields -Werror #-}

{-
    TODO Fill out reduce.

    Note that On 64 bit machines, GHC will always use pointer tagging
    as long as there are less than 8 constructors. So, anything that is
    frequently pattern matched on should have at most 7 branches.
-}

module Urbit.UrukRTS.Types where

import ClassyPrelude             hiding (evaluate, fromList, try, seq)
import Control.Monad.Primitive
import Data.Primitive.Array
import Data.Primitive.SmallArray
import GHC.Prim                  hiding (seq)
import System.IO.Unsafe

#if !defined(__GHCJS__)
import Data.Flat
#endif

import Control.Arrow            ((>>>))
import Control.Exception        (throw, try)
import Data.Bits                (shiftL, (.|.))
import Data.Char                (isPrint, isSpace)
import Data.Function            ((&))
import Numeric.Natural          (Natural)
import Prelude                  ((!!))

import qualified GHC.Exts                 as GHC.Exts
import qualified Urbit.Atom               as Atom


-- Useful Types ----------------------------------------------------------------

type Nat = Natural
type Bol = Bool


-- Closure ---------------------------------------------------------------------

type CloN = SmallArray Val

getCloN :: CloN -> Int -> Val
{-# INLINE getCloN #-}
getCloN = indexSmallArray

addCloN :: CloN -> Val -> CloN
{-# INLINE addCloN #-}
addCloN xs x = xs <> GHC.Exts.fromList [x] -- TODO Slow

clo1 :: Val -> CloN
clo1 x = GHC.Exts.fromList [x]


-- Arguments -------------------------------------------------------------------

type ArgN = SmallArray (IO Val)


-- Registers -------------------------------------------------------------------

type RegN = SmallMutableArray RealWorld Val

newRegN :: Int -> IO RegN
{-# INLINE newRegN #-}
newRegN n = newSmallArray n VUni

getRegN :: RegN -> Int -> IO Val
{-# INLINE getRegN #-}
getRegN = readSmallArray

setRegN :: RegN -> Int -> Val -> IO ()
{-# INLINE setRegN #-}
setRegN = writeSmallArray

instance Hashable a => Hashable (SmallArray a) where
  hashWithSalt i x = hashWithSalt i (GHC.Exts.toList x)


-- Types -----------------------------------------------------------------------

data Jet = Jet
  { jArgs :: !Int
  , jName :: Val
  , jBody :: Val
  , jFast :: !Exp
  , jRegs :: !Int -- Number of registers needed.
  }
 deriving (Eq, Ord)

instance Hashable Jet where
  hashWithSalt i (Jet a n b _ _) =
    hashWithSalt i (a,n,b)

instance Show Jet where
  show (Jet{..}) = muck $
    case jName of
      VNat (Atom.atomUtf8 -> Right nm) ->
        "J" <> show jArgs <> "_" <> unpack nm <> "_" <> has
      _ ->
        "J" <> show jArgs <> "_" <> show jName <> "_" <> has
   where
     has = (take 5 $ show $ abs $ hash jBody)
     muck = fmap $ \case
       '-' -> '_'
       x   -> x

data Node
  = Jay Int -- Always >= 1
  | Kay
  | Ess
  | Dee
  | Jut Jet
  | Eye Int
  | Bee Int --  Always >=  1
  | Sea Int --  Always >=  1
  | Sen Int --  Always >=  1
  | Seq
  | Fix
  | Nat Nat
  | Int Integer
  | Bol Bool
  | Iff
  | Pak
  | Zer
  | Eql
  | Add
  | Inc
  | Dec
  | Fec
  | Mul
  | Sub
  | Ded
  | Uni
  | Lef
  | Rit
  | Cas
  | Let
  | Con
  | Car
  | Cdr

  | Lsh
  | Lth
  | Fub
  | Not
  | Xor
  | Div
  | Tra
  | Mod
  | Rap
  | Turn
  | Zing

  | IntPositive
  | IntNegative

  | IntAbs
  | IntAdd
  | IntDiv
  | IntIsZer
  | IntIsNeg
  | IntIsPos
  | IntLth
  | IntMul
  | IntNegate
  | IntSub

 deriving (Eq, Ord, Generic, Hashable)

instance Show Node where
  show = \case
    Jay n     -> replicate (fromIntegral n) 'J'
    Kay       -> "K"
    Ess       -> "S"
    Dee       -> "D"
    Jut j     -> show j
    Eye 1     -> "I"
    Bee 1     -> "B"
    Sea 1     -> "C"
    Eye n     -> "I" <> show n
    Bee n     -> "B" <> show n
    Sea n     -> "C" <> show n
    Sen n     -> "S" <> show n
    Seq       -> "SEQ"
    Fix       -> "FIX"
    Nat n     -> show n
    Int i     -> show i
    Bol True  -> "YES"
    Bol False -> "NAH"
    Iff       -> "IFF"
    Pak       -> "PAK"
    Zer       -> "ZER"
    Eql       -> "EQL"
    Add       -> "ADD"
    Inc       -> "INC"
    Dec       -> "DEC"
    Fec       -> "FEC"
    Mul       -> "MUL"
    Sub       -> "SUB"
    Ded       -> "DED"
    Uni       -> "UNI"
    Lef       -> "LEF"
    Rit       -> "RIT"
    Cas       -> "CAS"
    Let       -> "LET"
    Con       -> "CON"
    Car       -> "CAR"
    Cdr       -> "CDR"

    Lsh       -> "LSH"
    Lth       -> "LTH"
    Fub       -> "FUB"
    Not       -> "NOT"
    Xor       -> "XOR"
    Div       -> "DIV"
    Tra       -> "TRA"
    Mod       -> "MOD"
    Rap       -> "RAP"
    Turn      -> "TURN"
    Zing      -> "ZING"

    IntPositive -> "INT_POSITIVE"
    IntNegative -> "INT_NEGATIVE"

    IntAbs -> "INT_ABS"
    IntAdd -> "INT_ADD"
    IntDiv -> "INT_DIV"
    IntIsZer -> "INT_IS_ZER"
    IntIsNeg -> "INT_IS_NEG"
    IntIsPos -> "INT_IS_POS"
    IntLth -> "INT_LTH"
    IntMul -> "INT_MUL"
    IntNegate -> "INT_NEGATE"
    IntSub -> "INT_SUB"

data Fun = Fun
  { fNeed :: !Int
  , fHead :: !Node
  , fArgs :: CloN -- Lazy on purpose.
  }
 deriving (Eq, Ord, Generic, Hashable)

instance Show Fun where
  show (Fun _ h args) = if sizeofSmallArray args == 0
    then show h
    else mconcat
      ["(", show h <> " ", intercalate " " (show <$> GHC.Exts.toList args), ")"]

data Val
  = VUni
  | VCon !Val !Val
  | VLef !Val
  | VRit !Val
  | VNat !Nat
  | VInt !Integer
  | VBol !Bool
  | VFun !Fun
 deriving (Eq, Ord, Generic, Hashable)

instance NFData Val where
  rnf = \case
    VUni     -> ()
    VCon _ _ -> ()
    VLef _   -> ()
    VRit _   -> ()
    VNat _   -> ()
    VInt _   -> ()
    VBol _   -> ()
    VFun _   -> ()

instance Show Val where
  show = \case
    VUni       -> "U"
    VCon x y   -> "[" <> show x <> ", " <> show y <> "]"
    VLef x     -> "L" <> show x
    VRit x     -> "R" <> show x
    VNat n     -> showNat n
    VInt i     -> showInt i
    VBol True  -> "Y"
    VBol False -> "N"
    VFun f     -> show f

showInt :: Integer -> String
showInt x | x >= 0 = "+" <> show x
showInt x          = "-" <> show (abs x)

showNat :: Nat -> String
showNat at@(Atom.atomUtf8 -> Right nm) =
  let okChar c = isPrint c || isSpace c
  in  if all okChar nm
      then "\"" <> (unpack $ intercalate "\\n" $ lines nm) <> "\""
      else show at
showNat at = show at

data Exp
  = VAL   !Val                    --  Constant Value
  | REF   !Int                    --  Stack Reference
  | REG   !Int                    --  Register Reference
  | SLF                           --  Self Reference

  | IFF   !Exp !Exp !Exp           --  If-Then-Else
  | CAS   !Int !Exp !Exp !Exp      --  Pattern Match
  | LET   !Int !Exp !Exp           --  Pattern Match on single value
  | REC1  !Exp                     --  Recursive Call
  | REC2  !Exp !Exp                --  Recursive Call
  | REC3  !Exp !Exp !Exp           --  Recursive Call
  | REC4  !Exp !Exp !Exp !Exp      --  Recursive Call
  | REC5  !Exp !Exp !Exp !Exp !Exp --  Recursive Call
  | RECN  !(SmallArray Exp)        --  Recursive Call

  | SEQ !Exp !Exp                 --  Evaluate head, return tail
  | DED !Exp                      --  Evaluate argument, then crash.

  | INC !Exp                      --  Increment
  | DEC !Exp                      --  Decrement
  | FEC !Exp                      --  Fast decrement
  | ADD !Exp !Exp                 --  Add
  | MUL !Exp !Exp                 --  Multiply

  | LSH !Exp !Exp                 --  Left Shift
  | LTH !Exp !Exp                 --  Less-Than
  | FUB !Exp !Exp                 --  Fast Subtract
  | NOT !Exp                      --  Boolean Not
  | XOR !Exp !Exp                 --  (?) XOR
  | DIV !Exp !Exp                 --  Atom division
  | TRA !Exp !Exp                 --  Execution Trace
  | MOD !Exp !Exp                 --  Atom Modulus
  | RAP !Exp !Exp                 --  (Generalized) tape to cord.
  | TURN !Exp !Exp                --  Map over a list
  | ZING !Exp                     --  Concatenate list of lists

  | INT_POSITIVE !Exp
  | INT_NEGATIVE !Exp

  | INT_ABS !Exp
  | INT_ADD !Exp !Exp
  | INT_DIV !Exp !Exp
  | INT_IS_ZER !Exp
  | INT_IS_NEG !Exp
  | INT_IS_POS !Exp
  | INT_LTH !Exp !Exp
  | INT_MUL !Exp !Exp
  | INT_NEGATE !Exp
  | INT_SUB !Exp !Exp

  | SUB !Exp !Exp                 --  Subtract
  | ZER !Exp                      --  Is Zero?
  | EQL !Exp !Exp                 --  Atom equality.

  | CON !Exp !Exp                 --  Cons
  | CAR !Exp                      --  Head
  | CDR !Exp                      --  Tail
  | LEF !Exp                      --  Left Constructor
  | RIT !Exp                      --  Right Constructor

  | JET1 !Jet !Exp                --  Fully saturated jet call.
  | JET2 !Jet !Exp !Exp           --  Fully saturated jet call.
  | JET3 !Jet !Exp !Exp !Exp      --  Fully saturated jet call.
  | JET4 !Jet !Exp !Exp !Exp !Exp --  Fully saturated jet call.
  | JET5 !Jet !Exp !Exp !Exp !Exp !Exp --  Fully saturated jet call.
  | JETN !Jet !(SmallArray Exp)   --  Fully saturated jet call.

  | CLON !Fun !(SmallArray Exp)   --  Undersaturated call
  | CALN !Exp !(SmallArray Exp)   --  Call of unknown saturation
 deriving (Eq, Ord, Show)


-- Exceptions ------------------------------------------------------------------

data TypeError = TypeError Text
 deriving (Eq, Ord, Show, Exception)

data Crash = Crash Val
 deriving (Eq, Ord, Show, Exception)

data BadRef = BadRef Jet Int
 deriving (Eq, Ord, Show, Exception)


--------------------------------------------------------------------------------

valFun :: Val -> Fun
{-# INLINE valFun #-}
valFun = \case
  VUni     -> Fun 1 Uni mempty
  VCon h t -> Fun 1 Con (fromList [h, t])
  VLef l   -> Fun 2 Lef (fromList [l])
  VRit r   -> Fun 2 Rit (fromList [r])
  VNat n   -> Fun 2 (Nat n) mempty
  VInt i   -> Fun 1 (Int i) mempty
  VBol b   -> Fun 2 (Bol b) mempty
  VFun f   -> f

nodeArity :: Node -> Int
nodeArity = \case
  Jay _ -> 2
  Kay   -> 2
  Ess   -> 3
  Dee   -> 1
  Jut j -> jArgs j
  Eye n -> 0+n
  Bee n -> 2+n
  Sea n -> 2+n
  Sen n -> 2+n
  Seq   -> 2
  Fix   -> 2
  Nat n -> 2
  Int n -> 1
  Bol b -> 2
  Iff   -> 3
  Pak   -> 1
  Zer   -> 1
  Eql   -> 2
  Add   -> 2
  Inc   -> 1
  Dec   -> 1
  Fec   -> 1
  Mul   -> 2
  Sub   -> 2
  Ded   -> 1
  Uni   -> 1
  Lef   -> 1 -- Hack to convert to value after first arg, actually 3.
  Rit   -> 1 -- Hack to convert to value after first arg, actually 3.
  Cas   -> 3
  Let   -> 2
  Con   -> 2 -- Hack to convert to value after two args, actually 3.
  Car   -> 1
  Cdr   -> 1

  Lsh   -> 2
  Lth   -> 2
  Fub   -> 2
  Not   -> 1
  Xor   -> 2
  Div   -> 2
  Tra   -> 2
  Mod   -> 2
  Rap   -> 2
  Turn  -> 2
  Zing  -> 1

  IntPositive -> 1
  IntNegative -> 1

  IntAbs -> 1
  IntAdd -> 2
  IntDiv -> 2
  IntIsZer -> 1
  IntIsNeg -> 1
  IntIsPos -> 1
  IntLth -> 2
  IntMul -> 2
  IntNegate -> 1
  IntSub -> 2
