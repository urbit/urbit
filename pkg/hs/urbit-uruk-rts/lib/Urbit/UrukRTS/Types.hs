{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -funbox-strict-fields -Werror #-}

{-
    TODO Fill out reduce.

    Note that On 64 bit machines, GHC will always use pointer tagging
    as long as there are less than 8 constructors. So, anything that is
    frequently pattern matched on should have at most 7 branches.
-}

module Urbit.UrukRTS.Types where

import ClassyPrelude             hiding (evaluate, fromList, seq, try)
import Control.Monad.Primitive
import Data.Primitive.Array
import Data.Primitive.SmallArray
import GHC.Prim                  hiding (seq)
import System.IO.Unsafe

#if !defined(__GHCJS__)
import Data.Flat
#endif

import Control.Arrow     ((>>>))
import Control.Exception (throw, try)
import Data.Bits         (shiftL, (.|.))
import Data.Char         (isPrint, isSpace)
import Data.Function     ((&))
import Numeric.Natural   (Natural)
import Prelude           ((!!))

import qualified GHC.Exts            as GHC.Exts
import qualified Urbit.Atom          as Atom
import qualified Urbit.Uruk.Dash.Exp as Exp

-- Useful Types ----------------------------------------------------------------

type Bol = Bool


-- Closure ---------------------------------------------------------------------

type CloN = SmallArray Val

getCloN :: CloN -> Int -> Val
{-# INLINE getCloN #-}
getCloN = indexSmallArray

addCloN :: CloN -> Val -> CloN
{-# INLINE addCloN #-}
addCloN xs x = xs <> GHC.Exts.fromList [x] -- TODO Slow

mkClo1 :: Val -> CloN
mkClo1 x = GHC.Exts.fromList [x]


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
  , jLoop :: Bool
  , jRegs :: !Int -- Number of registers needed.
  }
 deriving (Eq, Ord, Generic, NFData)

instance Hashable Jet where
  hashWithSalt i (Jet a n b _ _ _) =
    hashWithSalt i (a,n,b)

instance Show Jet where
  show (Jet{..}) = muck $
    case jName of
      VNat (Atom.atomUtf8 -> Right nm) ->
        "E" <> show jArgs <> "_" <> unpack nm <> "_" <> has
      _ ->
        "E" <> show jArgs <> "_" <> show jName <> "_" <> has
   where
     has = (take 5 $ show $ abs $ hash jBody)
     muck = fmap $ \case
       '-' -> '_'
       x   -> x

data Match
  = MS !Exp.SingJet
  | MD !Exp.DataJet
 deriving (Eq, Ord, Generic, Hashable, NFData)


data Node
  = Ess
  | Kay
  | Enh Int -- Always >= 1
  | Dub

  -- Jut is the current structure for a recognized set of jet calls represented
  -- as a stack machine(?). This should be introspectable once I get a
  -- `Val -> ASKEW` function.
  | Jut Jet

  | M Match !Natural ![Node]

  | Bee Int --  Always >=  1
  | Sea Int --  Always >=  1
  | Sen Int --  Always >=  1
  | Int Integer
  | Lis [Val]
  | Bol Bool

  | MkBox
  | Box Val
  | Unbox
 deriving (Eq, Ord, Generic, Hashable, NFData)

pattern Nat n = M (MD (Exp.NAT n)) 2 []

pattern Uni = M (MS Exp.UNI) 1 []

pattern LefC = M (MS Exp.LEF) 3 []
pattern RitC = M (MS Exp.RIT) 3 []

pattern ConC = M (MS Exp.CON) 3 []

pattern Car = M (MS Exp.CAR) 1 []
pattern Cdr = M (MS Exp.CDR) 1 []

pattern Ded = M (MS Exp.DED) 1 []
pattern Add = M (MS Exp.ADD) 2 []
pattern Mul = M (MS Exp.MUL) 2 []
pattern Dec = M (MS Exp.DEC) 1 []
pattern Fec = M (MS Exp.FEC) 1 []
pattern Inc = M (MS Exp.INC) 1 []
pattern Eql = M (MS Exp.EQL) 2 []
pattern Zer = M (MS Exp.ZER) 1 []
pattern Pak = M (MS Exp.PAK) 1 []
pattern Seq = M (MS Exp.SEQ) 2 []
pattern Let = M (MS Exp.LET) 2 []
pattern Iff = M (MS Exp.IFF) 3 []

pattern Fix = M (MS Exp.FIX) 2 []

pattern Lth = M (MS Exp.LTH) 2 []

pattern Sub = M (MS Exp.SUB) 2 []
pattern Fub = M (MS Exp.FUB) 2 []

pattern Div = M (MS Exp.DIV) 2 []
pattern Mod = M (MS Exp.MOD) 2 []

pattern Bex = M (MS Exp.BEX) 1 []
pattern Lsh = M (MS Exp.LSH) 2 []
pattern Not = M (MS Exp.NOT) 1 []
pattern Xor = M (MS Exp.XOR) 2 []

-- pattern Eye
pattern Cas = M (MS Exp.CAS) 3 []

pattern Trace = M (MS Exp.TRACE) 2 []

pattern LConC = M (MS Exp.LCON) 4 []
pattern LNil = M (MS Exp.LNIL) 2 []

pattern Gulf = M (MS Exp.GULF) 2 []
pattern Snag = M (MS Exp.SNAG) 2 []
pattern Turn = M (MS Exp.TURN) 2 []
pattern Weld = M (MS Exp.WELD) 2 []
pattern Zing = M (MS Exp.ZING) 2 []

pattern Rap  = M (MS Exp.RAP) 2 []
pattern Ntot = M (MS Exp.NTOT) 1 []

pattern AddAssoc = M (MS Exp.ADD_ASSOC) 5 []
pattern FindAssoc = M (MS Exp.FIND_ASSOC) 3 []

pattern IntPositive = M (MS Exp.INT_POSITIVE) 1 []
pattern IntNegative = M (MS Exp.INT_NEGATIVE) 1 []

pattern IntAbs = M (MS Exp.INT_ABS) 1 []
pattern IntAdd = M (MS Exp.INT_ADD) 2 []
pattern IntDiv = M (MS Exp.INT_DIV) 2 []
pattern IntIsZer = M (MS Exp.INT_IS_ZER) 1 []
pattern IntIsNeg = M (MS Exp.INT_IS_NEG) 1 []
pattern IntIsPos = M (MS Exp.INT_IS_POS) 1 []
pattern IntLth = M (MS Exp.INT_LTH) 2 []
pattern IntMul = M (MS Exp.INT_MUL) 2 []
pattern IntNegate = M (MS Exp.INT_NEGATE) 1 []
pattern IntSub = M (MS Exp.INT_SUB) 2 []

instance Show Node where
  show = \case
    Ess         -> "S"
    Kay         -> "K"
    Enh n       -> replicate (fromIntegral n) 'E'
    Dub         -> "W"
    Jut j       -> show j
    Bee 1       -> "B"
    Sea 1       -> "C"
    Bee n       -> "B" <> show n
    Sea n       -> "C" <> show n
    Sen n       -> "S" <> show n
    Int i       -> show i
    Lis l       -> show l
    Bol True    -> "YES"
    Bol False   -> "NAH"

    MkBox       -> "MKBOX"
    Box v       -> "BOX(" <> show v <> ")"
    Unbox       -> "UNBOX"

    M (MD x) _ _ -> show x
    M (MS x) _ _ -> show x


data Fun = Fun
  { fNeed :: !Int
  , fHead :: !Node
  , fArgs :: CloN -- Lazy on purpose.
  }
 deriving (Eq, Ord, Generic, Hashable, NFData)

instance Show Fun where
  show (Fun _ h args) = if sizeofSmallArray args == 0
    then show h
    else mconcat
      ["(", show h <> " ", intercalate " " (show <$> GHC.Exts.toList args), ")"]


newtype Hash = Hash { unHash :: ByteString }
  deriving (Eq, Ord, Generic)
  deriving newtype (Hashable)

-- A Box refers to a value which can be (or has already been) written to disk
-- separately. This allows for lazy loading of data from disk.
data Box
  = BSaved !Val Hash
  | BUnsaved !Val
  | BUnloaded Hash
  deriving (Eq, Ord, Generic, Hashable)

data Val
  = VUni
  | VCon !Val !Val
  | VLef !Val
  | VRit !Val
  | VNat !Natural
  | VInt !Integer
  | VBol !Bool
  | VLis ![Val]
  | VBox !Val
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
    VLis _   -> ()
    VBox _   -> ()
    VFun _   -> ()

instance Show Val where
  show = \case
    VUni       -> "U"
    VCon x y   -> "(" <> intercalate "," (show <$> unrollCons [x] y) <> ")"
    VLef x     -> "L" <> show x
    VRit x     -> "R" <> show x
    VNat n     -> showNat n
    VInt i     -> showInt i
    VBol True  -> "Y"
    VBol False -> "N"
    VLis vals  -> show vals
    VBox x     -> "!" <> show x
    VFun f     -> show f
   where
    unrollCons acc (VCon x y) = unrollCons (x : acc) y
    unrollCons acc val        = reverse (val : acc)

showInt :: Integer -> String
showInt x | x >= 0 = "+" <> show x
showInt x = "-" <> show (abs x)

showNat :: Natural -> String
showNat at@(Atom.atomUtf8 -> Right nm) =
  let okChar c = isPrint c || isSpace c
  in  if all okChar nm && length nm > 1
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

  | THE   !Exp !Exp                --  Let binding with unused result.
  | FOR   !Int !Exp !Exp           --  Optimized Map over a list

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
  | TRA !Exp                      --  Print Log Message
  | MOD !Exp !Exp                 --  Atom Modulus
  | RAP !Exp !Exp                 --  (Generalized) tape to cord.
  | GULF !Exp !Exp                --  Natural range to list
  | SNAG !Exp !Exp                --  List lookup by index
  | TURN !Exp !Exp                --  Map over a list
  | WELD !Exp !Exp                --  Concatenate two lists.
  | ZING !Exp                     --  Concatenate list of lists
  | NTOT !Exp                     --  Render atom as tape.

  | ADD_ASSOC !Exp !Exp !Exp !Exp !Exp  -- Add to assoc list
  | FIND_ASSOC !Exp !Exp !Exp     --  Find in assoc list

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

  | BOX !Exp
  | UNBOX !Exp

  | SUB !Exp !Exp                 --  Subtract
  | ZER !Exp                      --  Is Zero?
  | EQL !Exp !Exp                 --  Atom equality.

  | CON !Exp !Exp                 --  Cons
  | CAR !Exp                      --  Head
  | CDR !Exp                      --  Tail
  | LEF !Exp                      --  Left Constructor
  | RIT !Exp                      --  Right Constructor

  | LCON !Exp !Exp                --  List cons
  | LNIL                          --  List termination

  | JET1 !Jet !Exp                --  Fully saturated jet call.
  | JET2 !Jet !Exp !Exp           --  Fully saturated jet call.
  | JET3 !Jet !Exp !Exp !Exp      --  Fully saturated jet call.
  | JET4 !Jet !Exp !Exp !Exp !Exp --  Fully saturated jet call.
  | JET5 !Jet !Exp !Exp !Exp !Exp !Exp --  Fully saturated jet call.
  | JETN !Jet !(SmallArray Exp)   --  Fully saturated jet call.

  | CLO1 !Fun !Exp                     --  Undersaturated call
  | CLO2 !Fun !Exp !Exp                --  Undersaturated call
  | CLO3 !Fun !Exp !Exp !Exp           --  Undersaturated call
  | CLO4 !Fun !Exp !Exp !Exp !Exp      --  Undersaturated call
  | CLO5 !Fun !Exp !Exp !Exp !Exp !Exp --  Undersaturated call
  | CLON !Fun !(SmallArray Exp)        --  Undersaturated call

  | CALN !Exp !(SmallArray Exp)   --  Call of unknown saturation
 deriving (Eq, Ord, Show, Generic, NFData)

instance NFData a => NFData (SmallArray a) where
  rnf !_ = ()


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
  VCon h t -> Fun 1 ConC (fromList [h, t])
  VLef l   -> Fun 2 LefC (fromList [l])
  VRit r   -> Fun 2 RitC (fromList [r])
  VNat n   -> Fun 2 (Nat n) mempty
  VInt i   -> Fun 1 (Int i) mempty
  VBol b   -> Fun 2 (Bol b) mempty
  VLis xs  -> Fun 2 (Lis xs) mempty
  VBox x   -> Fun 1 (Box x) mempty
  VFun f   -> f

nodeArity :: Node -> Int
nodeArity = \case
  Ess   -> 3
  Kay   -> 2
  Enh _ -> 2
  Dub   -> 6
  Jut j -> jArgs j
  Bee n -> 2+n
  Sea n -> 2+n
  Sen n -> 2+n
  Int n -> 1
  Bol b -> 2

  Lis []    -> 2
  Lis (_:_) -> 2

  MkBox -> 1
  Box _ -> 1
  Unbox -> 1

  (M _ n _) -> fromIntegral n
