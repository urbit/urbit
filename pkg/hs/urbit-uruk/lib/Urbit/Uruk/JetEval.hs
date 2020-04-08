module Urbit.Uruk.JetEval where

import ClassyPrelude
import Data.Tree

import Data.Bits           (shiftL, (.|.))
import Data.Function       ((&))
import Data.List           (iterate, (!!))
import Numeric.Natural     (Natural)
import Urbit.Moon.Arity    (Arity(..), appArity)
import Urbit.Pos           (Pos)
import Urbit.Uruk.Class    (Uruk(..))
import Urbit.Uruk.Dash.Exp (DataJet(..), ExpTree(..), SingJet(..))

import qualified Debug.Trace             as Debug.Trace
import qualified Urbit.Atom              as Atom
import qualified Urbit.Uruk.Dash.DataJet as Jets
import qualified Urbit.Uruk.Dash.Exp     as Dash
import qualified Urbit.Uruk.Dash.Parser  as Dash
import qualified Urbit.Uruk.Jets         as Jets


-- Typse -----------------------------------------------------------------------

data Match
  = MD !DataJet
  | MS !SingJet
  | MU !Pos !Val !Val
 deriving (Eq, Ord, Generic)
 deriving anyclass NFData

instance Show Match where
  show = \case
    MD dj    -> show dj
    MS sj    -> show sj
    MU n (Nat (Atom.atomUtf8 -> Right txt)) v -> "J_" <> unpack txt
    MU n t v -> "J_" <> show t

data Ur
  = S
  | K
  | J !Pos
  | D
  | M Match !Natural ![Val]
 deriving (Eq, Ord, Generic)
 deriving anyclass NFData

instance Show Ur where
  show = \case
    S        -> "S"
    K        -> "K"
    J n      -> replicate (fromIntegral n) 'J'
    D        -> "D"
    M m n [] -> show m
    M m n xs -> "(" <> intercalate " " (show m : fmap show xs) <> ")"

pattern NS = N S
pattern NK = N K
pattern NJ n = N (J n)
pattern ND = N D
pattern NM x y z = N (M x y z)

pattern Nat n = N (M (MD (NAT n)) 2 [])

pattern Yes = N (M (MS YES) 2 [])
pattern Nah = N (M (MS NAH) 2 [])

pattern Uni = N (M (MS UNI) 1 [])

pattern LefC = N (M (MS LEF) 3 [])
pattern RitC = N (M (MS RIT) 3 [])

pattern Rit x = N (M (MS RIT) 2 [x])
pattern Lef x = N (M (MS LEF) 2 [x])

pattern Con h t = N (M (MS CON) 1 [h,t])
pattern ConC = N (M (MS CON) 3 [])

pattern Car = N (M (MS CAR) 1 [])
pattern Cdr = N (M (MS CDR) 1 [])

pattern Ded = N (M (MS DED) 1 [])
pattern Add = N (M (MS ADD) 2 [])
pattern Mul = N (M (MS MUL) 2 [])
pattern Dec = N (M (MS DEC) 1 [])
pattern Fec = N (M (MS FEC) 1 [])
pattern Inc = N (M (MS INC) 1 [])
pattern Eql = N (M (MS EQL) 2 [])
pattern Zer = N (M (MS ZER) 1 [])
pattern Pak = N (M (MS PAK) 1 [])
pattern Seq = N (M (MS SEQ) 2 [])
pattern Iff = N (M (MS IFF) 3 [])

pattern Fix = N (M (MS FIX) 2 [])

pattern Lth = N (M (MS LTH) 2 [])

pattern Sub = N (M (MS SUB) 2 [])
pattern Fub = N (M (MS FUB) 2 [])

pattern Div = N (M (MS DIV) 2 [])
pattern Mod = N (M (MS MOD) 2 [])

pattern Bex = N (M (MS BEX) 1 [])
pattern Lsh = N (M (MS LSH) 2 [])
pattern Not = N (M (MS NOT) 1 [])
pattern Xor = N (M (MS XOR) 2 [])

pattern Eye = N (M (MD (In 1)) 1 [])
pattern Cas = N (M (MS CAS) 3 [])

pattern Trace = N (M (MS TRACE) 2 [])

type Exp = Dash.ExpTree Ur
type Val = Exp

instance Uruk Exp where
  uApp x y = pure (x :& y)

  uEss = NS
  uKay = NK
  uJay p = NJ (fromIntegral p)
  uDee = ND

  uBee p = N $ M (MD $ Bn $ fromIntegral p) (fromIntegral $ p+2) []
  uSen p = N $ M (MD $ Sn $ fromIntegral p) (fromIntegral $ p+2) []
  uSea p = N $ M (MD $ Cn $ fromIntegral p) (fromIntegral $ p+2) []
  uEye p = N $ M (MD $ In $ fromIntegral p) (fromIntegral $ p) []

  uNat n = N $ M (MD $ NAT n) 2 []

  uBol True  = Yes
  uBol False = Nah

  uUni = Uni
  uCon = ConC
  uSeq = Seq
  uCas = Cas
  uFix = Fix
  uIff = Iff

  uGlobal "lef" = Just LefC
  uGlobal "rit" = Just RitC
  uGlobal "pak" = Just Pak
  uGlobal "zer" = Just Zer
  uGlobal "eql" = Just Eql
  uGlobal "inc" = Just Inc
  uGlobal "dec" = Just Dec
  uGlobal "fec" = Just Fec
  uGlobal "add" = Just Add
  uGlobal "ded" = Just Ded
  uGlobal "div" = Just Div
  uGlobal "sub" = Just Sub
  uGlobal "fub" = Just Fub
  uGlobal "mul" = Just Mul
  uGlobal "car" = Just Car
  uGlobal "cdr" = Just Cdr
  uGlobal "mod" = Just Mod
  uGlobal "bex" = Just Bex
  uGlobal "lsh" = Just Lsh
  uGlobal "lth" = Just Lth
  uGlobal "not" = Just Not
  uGlobal "xor" = Just Xor
  uGlobal "trace" = Just Trace
  uGlobal _     = Nothing

  uArity (N (J n))     = pure $ AriJay n
  uArity (N K)         = pure $ AriKay
  uArity (N S)         = pure $ AriEss
  uArity (N D)         = pure $ AriDee
  uArity (N (M _ n _)) = pure $ AriOth (fromIntegral n)
  uArity (x :& y)      = join $ appArity <$> uArity x <*> uArity y


--------------------------------------------------------------------------------

eval :: Exp -> Val
eval x = maybe x eval (reduce x)

exec :: Exp -> [Exp]
exec x = x : fromMaybe [] (exec <$> reduce x)

reduce :: Exp -> Maybe Exp
reduce = \case
    NK :& x :& y            → Just $ x
    (reduce → Just xv) :& y → Just $ xv :& y
    x :& (reduce → Just yv) → Just $ x :& yv
    NS :& x :& y :& z       → Just $ x :& z :& (y :& z)
    ND :& x                 → Just $ dump x
    NJ n :& NJ 1            → Just $ NJ (succ n)
    NJ n :& t :& b          → Just $ NM (match n t b) (fromIntegral n) []
    NM m 0 xs               → Just $ fromMaybe (jetFallback m xs) (runJet m xs)
    NM m n xs :& x          → Just $ NM m (pred n) (xs <> [x])
    _                       → Nothing

match :: Pos -> Val -> Val -> Match
match n t b = Jets.jetMatch (n, valDash t, valDash b) & \case
  Nothing         -> MU n t b
  Just (Left  dj) -> MD dj
  Just (Right sj) -> MS sj

urDash :: Ur -> Dash.Val
urDash = \case
  S                 -> Jets.NS
  K                 -> Jets.NK
  J n               -> Jets.NJ
  D                 -> Jets.ND
  M (MD dj   ) _ xs -> go (N (Dash.DataJet dj)) xs
  M (MS sj   ) _ xs -> go (N (Dash.SingJet sj)) xs
  M (MU n t b) _ xs -> go (Jets.jn n :& valDash t :& valDash b) xs
 where
  go :: Dash.Val -> [Val] -> Dash.Val
  go x []       = x
  go x (y : ys) = go (x :& valDash y) ys

valDash :: Val -> Dash.Val
valDash = \case
  N n    -> urDash n
  x :& y -> valDash x :& valDash y

dashUr :: Dash.Ur -> Exp
dashUr = \case
  Dash.S -> NS
  Dash.K -> NK
  Dash.J -> NJ 1
  Dash.D -> ND
  Dash.DataJet dj -> N $ M (MD dj) (fromIntegral $ Jets.djArity dj) []
  Dash.SingJet sj -> N $ M (MS sj) (fromIntegral $ Jets.sjArity sj) []

dashVal :: Dash.Val -> Exp
dashVal = \case
  N n    -> dashUr n
  x :& y -> dashVal x :& dashVal y

dump :: Val -> Val
dump = Nat . snd . go . valDash
 where
  go :: Dash.Val -> (Int, Natural)
  go = \case
    N Dash.S            -> (3, 0)
    N Dash.K            -> (3, 2)
    N Dash.J            -> (3, 4)
    N Dash.D            -> (3, 6)
    N (Dash.DataJet dj) -> go (Jets.djUnMatch dj)
    N (Dash.SingJet sj) -> go (Jets.sjUnMatch sj)
    x :& y              -> (rBits, rNum)
     where
      (xBits, xNum) = go x
      (yBits, yNum) = go y
      rBits         = 1 + xBits + yBits
      rNum          = 1 .|. shiftL xNum 1 .|. shiftL yNum (1 + xBits)

jetFallback :: Match -> [Val] -> Exp
jetFallback (MU n t b) xs = foldl' (:&) b xs
jetFallback (MD dj)    xs = foldl' (:&) (dashVal $ Jets.djBody dj) xs
jetFallback (MS sj)    xs = foldl' (:&) (dashVal $ Jets.sjBody sj) xs

runJet :: Match -> [Val] -> Maybe Exp
runJet = curry \case
  (MU _ _ b  , xs       ) -> Just (foldl' (:&) b xs)
  (MD (NAT n), [x, y]   ) -> Just (goNat n x y)
  (MD (Bn  n), f:g:xs   ) -> Just (f :& foldl' (:&) g xs)
  (MD (Cn  n), f:g:xs   ) -> Just (foldl' (:&) f xs :& g)
  (MD (Sn  n), f:g:xs   ) -> Just (foldl' (:&) f xs :& foldl' (:&) g xs)
  (MD (In  n), [f]      ) -> Just f
  (MD (In  n), f:xs     ) -> Just (foldl' (:&) f xs)
  (MS SEQ    , [x, y]   ) -> Just y
  (MS FIX    , [f,x]    ) -> Just (f :& (Fix :& f) :& x)
  (MS IFF    , [c,t,e]  ) -> goIff c t e
  (MS PAK    , [_]      ) -> Nothing
  (MS ZER    , [x]      ) -> goZer x
  (MS EQL    , [x,y]    ) -> goEql x y
  (MS ADD    , [x,y]    ) -> goAdd x y
  (MS INC    , [x]      ) -> goInc x
  (MS DEC    , [x]      ) -> goDec x
  (MS FEC    , [x]      ) -> goFec x
  (MS MUL    , [x,y]    ) -> mul x y
  (MS BEX    , [x]      ) -> goBex x
  (MS LSH    , [x,n]    ) -> goLsh x n
  (MS SUB    , [x,y]    ) -> goSub x y
  (MS FUB    , [x,y]    ) -> goFub x y
  (MS NOT    , [x]      ) -> goNot x
  (MS XOR    , [x,y]    ) -> goXor x y
  (MS DED    , [x]      ) -> error ("DED: " <> show x)
  (MS UNI    , [_]      ) -> Nothing
  (MS YES    , [y,_]    ) -> Just y
  (MS NAH    , [_,n]    ) -> Just n
  (MS LEF    , [x, l, _]) -> Just (l :& x)
  (MS RIT    , [x, _, r]) -> Just (r :& x)
  (MS CAS    , [s, l, r]) -> goCas s l r
  (MS CON    , [x, y, f]) -> Just (f :& x :& y)
  (MS CAR    , [p]      ) -> goCar p
  (MS CDR    , [p]      ) -> goCdr p
  (MS LTH    , [x, y]   ) -> goLth x y
  (MS DIV    , [x, y]   ) -> goDiv x y
  (MS MOD    , [x, y]   ) -> goMod x y
  (MS TRACE  , [x, y]   ) -> goTrace x y

  (MD (NAT _), _        ) -> badArgs
  (MD (Bn  _), _        ) -> badArgs
  (MD (Cn  _), _        ) -> badArgs
  (MD (Sn  _), _        ) -> badArgs
  (MD (In  _), _        ) -> badArgs
  (MS SEQ    , _        ) -> badArgs
  (MS FIX    , _        ) -> badArgs
  (MS IFF    , _        ) -> badArgs
  (MS PAK    , _        ) -> badArgs
  (MS ZER    , _        ) -> badArgs
  (MS EQL    , _        ) -> badArgs
  (MS ADD    , _        ) -> badArgs
  (MS INC    , _        ) -> badArgs
  (MS DEC    , _        ) -> badArgs
  (MS FEC    , _        ) -> badArgs
  (MS MUL    , _        ) -> badArgs
  (MS BEX    , _        ) -> badArgs
  (MS LSH    , _        ) -> badArgs
  (MS SUB    , _        ) -> badArgs
  (MS FUB    , _        ) -> badArgs
  (MS NOT    , _        ) -> badArgs
  (MS XOR    , _        ) -> badArgs
  (MS DED    , _        ) -> badArgs
  (MS UNI    , _        ) -> badArgs
  (MS YES    , _        ) -> badArgs
  (MS NAH    , _        ) -> badArgs
  (MS LEF    , _        ) -> badArgs
  (MS RIT    , _        ) -> badArgs
  (MS CAS    , _        ) -> badArgs
  (MS CON    , _        ) -> badArgs
  (MS CAR    , _        ) -> badArgs
  (MS CDR    , _        ) -> badArgs
  (MS LTH    , _        ) -> badArgs
  (MS DIV    , _        ) -> badArgs
  (MS MOD    , _        ) -> badArgs
  (MS TRACE  , _        ) -> badArgs
 where
  badArgs = error "arity mismatch in jet execution"
  goIff Yes t _ = Just (t :& Uni)
  goIff Nah _ e = Just (e :& Uni)
  goIff _   _ _ = Nothing

  goZer (Nat 0) = Just Yes
  goZer (Nat n) = Just Nah
  goZer _       = Nothing

  goEql (Nat x) (Nat y) = Just (if x==y then Yes else Nah)
  goEql _       _       = Nothing

  goAdd (Nat x) (Nat y) = Just $ Nat (x+y)
  goAdd _       _       = Nothing

  goInc (Nat n) = Just $ Nat $ succ n
  goInc _       = Nothing

  goDec (Nat 0) = Just $ Lef Uni
  goDec (Nat n) = Just $ Rit $ Nat (n-1)
  goDec _       = Nothing

  goFec (Nat 0) = Just (Nat 0)
  goFec (Nat n) = Just (Nat $ pred n)
  goFec _       = Nothing

  goBex (Nat n) = Just $ Nat $ 2 ^ n
  goBex _       = Nothing

  mul (Nat x) (Nat y) = Just (Nat (x*y))
  mul _       _       = Nothing

  goCdr (Con _ t) = Just t
  goCdr _         = Nothing

  goCar (Con h _) = Just h
  goCar _         = Nothing

  goLth (Nat x) (Nat y) = Just (if x<y then Yes else Nah)
  goLth _       _       = Nothing

  goDiv (Nat x) (Nat y) = Just (Nat (x `div` y))
  goDiv _       _       = Nothing

  goMod (Nat x) (Nat y) = Just (Nat (x `mod` y))
  goMod _       _       = Nothing

  goCas (N (M (MS LEF) _ [x])) l _ = Just (l :& x)
  goCas (N (M (MS RIT) _ [x])) _ r = Just (r :& x)
  goCas _                      _ _ = Nothing

  goLsh (Nat x) (Nat n) = Just $ Nat $ shiftL n (fromIntegral x)
  goLsh _       _       = Nothing

  goSub (Nat x) (Nat y) | y > x = Just (Lef Uni)
  goSub (Nat x) (Nat y)         = Just (Rit (Nat (x-y)))
  goSub _       _               = Nothing

  goFub (Nat x) (Nat y) | y > x = Just (Nat (if y > x then 0 else (x-y)))
  goFub _       _               = Nothing

  goNot Yes = Just Nah
  goNot Nah = Just Yes
  goNot _   = Nothing

  goXor Yes Yes = Just Nah
  goXor Yes Nah = Just Yes
  goXor Nah Yes = Just Yes
  goXor Nah Nah = Just Nah
  goXor _   _   = Nothing

  goNat :: Natural -> Val -> Val -> Exp
  goNat 0 i z = z
  goNat n i z = i :& goNat (pred n) i z

  goTrace x y = Debug.Trace.trace ("Trace: " ++ (show x)) (Just (y :& Uni))
