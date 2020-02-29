module Urbit.Uruk.JetExecute where

import ClassyPrelude hiding (try, evaluate)
import System.IO.Unsafe

import Control.Arrow    ((>>>))
import Data.Function    ((&))
import Numeric.Natural  (Natural)
import Numeric.Positive (Positive)
import Prelude          ((!!))
import Urbit.Uruk.JetDemo     (Ur, UrPoly(Fast))


import qualified Urbit.Uruk.JetDemo as Ur
import qualified Urbit.Uruk.JetComp as Comp

--------------------------------------------------------------------------------

type Nat = Natural
type Bol = Bool
type Pos = Positive

--------------------------------------------------------------------------------

{-|
    On 64 bit machines, GHC will always use pointer tagging as long as
    there are less than 8 constructors. So, this should have at most
    7 branches.
-}
data Exp
    = EVal !Val
    | EApp !Exp !(Vector Exp)
    | ERec
    | ERef !Int
    | EIff !Exp !Exp !Exp
    | ECas !Exp !Exp !Exp
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass NFData

-- | Partially applied data.
data Wak
    = WakNat !Nat !Val
    | WakBol !Bol !Val
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass NFData

data Val
    = VBol !Bol
    | VNat !Nat
    | VTup !Val !Val
    | VSum !Bool !Val
    | VJet !Jet !Word !(Vector Val)
    | VFun !Fun !Word !(Vector Val)
    | VWak !Wak
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass NFData

data Fun = Fun
    { funExpr ∷ !Exp
    , funArgs ∷ !Word
    , funName ∷ !Val
    , funValu ∷ !Val
    }
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass NFData

data Jet
    = S | Sn | K | B | Bn | C | Cn | I
    | Seq | Yet | Fix | Iff | Ded
    | Pak | Zer | Eql | Add | Inc | Dec | Fec | Mul | Sub
    | Uni
    | Cas
    | Con | Car | Cdr
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass NFData

execJet ∷ [Val] → Word → Jet → Val
execJet !env !arity = go
  where
    go = \case
        S  → error "TODO"
        Sn → error "TODO"
        _  → error "TODO"

callVal ∷ [Val] → Val → Vector Val → Val
callVal env val args = error "TODO"

execFun ∷ [Val] → Fun → Val
execFun !env !self = go (funExpr self)
  where
    go ∷ Exp → Val
    go = \case
        EVal v     → v
        EApp x xs  → callVal env (go x) (go <$> xs)
        ERec       → VFun self (funArgs self) mempty
        ERef ix    → env !! ix
        EIff c t e → error "TODO"
        ECas x l r → error "TODO"

{-
instance Show Exp where
    show = \case
        Clo r n xs   → sexp "{" "}" [show n] xs
        Kal u xs     → sexp "[" "]" [show u] xs
        Rec xs       → sexp "(" ")" ["Rec"] xs
        Ref n xs     → sexp "(" ")" ["V" <> show n] xs
        Iff c t e xs → sexp "(" ")" ["If", show c, show t, show e] xs
        Cas x l r xs → sexp "(" ")" ["Case", show x, show l, show r] xs
        App x y      → sexp "(" ")" [] [x,y]
      where
        sexp ∷ Show a => String → String → [String] → [a] → String
        sexp _ _ [h] [] = h
        sexp a z hs  xs = a <> intercalate " " (hs <> (show <$> xs)) <> z

prettyExp ∷ Exp → String
prettyExp = go
  where
    go ∷ Exp → String
    go = \case
        Clo r n xs   → sexp "("   ")" [show n] (go <$> xs)
        Kal u xs     → sexp "("   ")" [show u] (go <$> xs)
        Rec xs       → sexp "("   ")" ["$"] (go <$> xs)
        Ref n xs     → sexp "("   ")" [sym (fromIntegral n)] (go <$> xs)
        Iff c t e [] → sexp "?:(" ")" [go c, go t, go e] []
        Iff c t e xs → sexp "("   ")" [go $ Iff c t e []] (go <$> xs)
        Cas x l r [] → sexp "?-(" ")" [go x, go l, go r] []
        Cas x l r xs → sexp "("   ")" [go $ Cas x l r []] (go <$> xs)
        App x y      → sexp "("   ")" [] [go x, go y]
      where
        sexp ∷ String → String → [String] → [String] → String
        sexp _ _ [h] [] = h
        sexp a z hs  xs = a <> intercalate " " (hs <> xs) <> z

prettyVal = prettyExp . valExp

valExp = go
  where
    go = \case
        ValClo r n xs   → Clo r n (go <$> xs)
        ValKal u xs     → Kal u (go <$> xs)
        ValRec xs       → Rec (go <$> xs)
        ValRef n xs     → Ref n (go <$> xs)
        ValIff c t e xs → Iff (go c) (go t) (go e) (go <$> xs)
        ValCas x l r xs → Cas (go x) (go l) (go r) (go <$> xs)

instance Show Val where
    show = \case
        ValClo r n xs   → sexp "{" "}" [show n] xs
        ValKal u xs     → sexp "[" "]" [show u] xs
        ValRec xs       → sexp "(" ")" ["Rec"] xs
        ValRef n xs     → sexp "(" ")" ["V" <> show n] xs
        ValIff c t e xs → sexp "(" ")" ["If", show c, show t, show e] xs
        ValCas x l r xs → sexp "(" ")" ["Case", show x, show l, show r] xs
      where
        sexp ∷ Show a => String → String → [String] → [a] → String
        sexp _ _ [h] [] = h
        sexp a z hs  xs = a <> intercalate " " (hs <> (show <$> xs)) <> z

--------------------------------------------------------------------------------

recursive ∷ Code → Bool
recursive (Code _ v) = go v
  where
    go = \case
        ValRec _        → True
        ValClo _ _ vs   → any go vs
        ValRef _ vs     → any go vs
        ValKal _ vs     → any go vs
        ValIff c t e xs → any go ([c,t,e] <> xs)
        ValCas x l r xs → any go ([x,l,r] <> xs)

infixl 5 %;

(%) ∷ Exp → Exp → Exp
(%) = App

simplify ∷ Node → [Exp] → Exp
simplify = curry $ \case
    ( VSeq, [x,y]   ) → y
    ( VYet, f:xs    ) → go f xs
    ( VS,   [x,y,z] ) → (x % z) % (y % z)
    ( VK,   [x,y]   ) → x
    ( VB,   [f,g,x] ) → f % (g % x)
    ( VC,   [f,g,x] ) → (f % x) % g
    ( VI,   [x]     ) → x
    ( VIff, [c,t,e] ) → Iff c (t % unit) (e % unit) []
    ( VCas, [x,l,r] ) → Cas x (abst l%ref 0) (abst r%ref 0) []
    ( VSn,  f:g:xs  ) → go f xs % go g xs
    ( VBn,  f:g:xs  ) → f % go g xs
    ( VCn,  f:g:xs  ) → go f xs % g

    ( _,    _       ) → error "simplify: bad arity"
  where
    go acc = \case { [] → acc; x:xs → go (acc % x) xs }

ref ∷ Nat → Exp
ref n = Ref n []

abst ∷ Exp → Exp
abst = g 0
  where
    g d = \case
        Clo n f xs      → Clo n f (g d <$> xs)
        Kal f xs        → Kal f (g d <$> xs)
        Rec xs          → Rec (g d <$> xs)
        Ref n xs | n>=d → Ref (n+1) (g d <$> xs)
        Ref n xs        → Ref n (g d <$> xs)
        Iff c t e xs    → Iff (g d c) (g d t) (g d e) (g d <$> xs)
        Cas x l r xs    → Cas (g d x) (g (d+1) l) (g (d+1) r) (g d <$> xs)

unit ∷ Exp
unit = Kal Ur.Uni []

nat ∷ Integral i => i → Nat
nat = fromIntegral

pos ∷ Integral i => i → Pos
pos = fromIntegral

pattern J x = Just x

infixl :@
pattern x :@ y = App x y

nok ∷ Exp → Maybe Exp
nok = \case
    (nok→J x) :@ y → Just (App x y)
    x :@ (nok→J y) → Just (App x y)

    --  Because unit is passed to branches, needs further simplification.
    Iff c (nok→J t) e xs → Just (Iff c t e xs)
    Iff c t (nok→J e) xs → Just (Iff c t e xs)

    --  Result of pattern match is passed into cases on the stack.
    Cas v (nok→J l) r xs → Just (Cas v l r xs)
    Cas v l (nok→J r) xs → Just (Cas v l r xs)

    Clo 1 f xs   :@ x → Just $ simplify f (snoc xs x)
    Clo n f xs   :@ x → Just $ Clo (n-1) f (snoc xs x)
    Kal f xs     :@ x → Just $ Kal f (snoc xs x)
    Rec xs       :@ x → Just $ Rec (snoc xs x)
    Iff c t e xs :@ x → Just $ Iff c t e (snoc xs x)
    Cas v l r xs :@ x → Just $ Cas v l r (snoc xs x)

    _ → Nothing

{-
    App (Clo 11

    Clo n h exp → pure Nothing
    Kal Ur [Exp]
    Rec [Exp]
    Ref Nat [Exp]
    Iff Exp Exp Exp [Exp]
    Cas Exp Exp Exp [Exp]
    App Exp Exp

    K :@ x :@ y             → Just $ x
    (reduce → Just xv) :@ y → Just $ xv :@ y
    x :@ (reduce → Just yv) → Just $ x :@ yv
    S :@ x :@ y :@ z        → Just $ x :@ z :@ (y :@ z)
    D :@ x                  → Just $ jam x
    J n :@ J 1              → Just $ J (succ n)
    J n :@ t :@ b           → Just $ Fast (fromIntegral n) (match n t b) []
    Fast 0 u us             → Just $ runJet u us
    Fast n u us :@ x        → Just $ Fast (pred n) u (us <> [x])
    _                       → Nothing
-}

call ∷ Exp → Exp → Exp
call f x = f & \case
    Clo 1 f xs   → simplify f (snoc xs x)
    Clo n f xs   → Clo (n-1) f (snoc xs x)
    Kal f xs     → Kal f (snoc xs x)
    Rec xs       → Rec (snoc xs x)
    Ref n xs     → Ref n (snoc xs x)
    Iff c t e xs → Iff c t e (snoc xs x)
    Cas x l r xs → Cas x l r (snoc xs x)

eval ∷ Exp → IO Exp
eval exp = do
    putStrLn (pack $ prettyExp exp)
    nok exp & \case
        Nothing → pure exp
        Just e' → eval e'

evaluate ∷ Exp → IO Val
evaluate = fmap go . eval
  where
    go = \case
        Clo n f xs   → ValClo n f (go <$> xs)
        Kal f xs     → ValKal f (go <$> xs)
        Rec xs       → ValRec (go <$> xs)
        Ref n xs     → ValRef n (go <$> xs)
        Iff c t e xs → ValIff (go c) (go t) (go e) (go <$> xs)
        Cas v l r xs → ValCas (go v) (go l) (go r) (go <$> xs)
        App x y      → error "This should not happen"

{-
    If jet has shape `(fix body)`
      Replace with `(body Rec)`
    Then, pass one ref per arity.
    For example: `jetCode 2 "(fix body)"`
      becomes: `(body Rec $1 $0)`
-}
jetCode ∷ Pos → Ur → IO Code
jetCode arity = fmap (Code arity) . evaluate . addArgs (nat arity) . addRecur
  where
    addArgs ∷ Nat → Exp → Exp
    addArgs 0 x = x
    addArgs n x = addArgs (n-1) (x % ref (n-1))

    addRecur ∷ Ur → Exp
    addRecur = \case
        Ur.Fast 1 Ur.JFix [body] → urExp body % Rec []
        body                     → urExp body

funCode ∷ Ur → IO Code
funCode body = Code 1 <$> evaluate (urExp body % ref 0)

justIfExp = Comp.justIf
justIfUr  = Comp.moonStrict justIfExp
justIfOth = Comp.eval $ Comp.deCompile justIfUr
justIf    = compile justIfUr

toBodyExp = Comp.toBody
toBodyUr  = Comp.moonStrict toBodyExp
toBody    = compile toBodyUr

toZeroExp = Comp.toZero
toZeroUr  = Comp.moonStrict toZeroExp
toZero    = compile toZeroUr

ackerExp = Comp.acker
ackerUr  = Comp.moonStrict ackerExp
acker    = compile ackerUr

ickerExp = Comp.icker
ickerOth = Comp.eval $ Comp.deCompile ickerUr
ickerUr  = Comp.moonStrict ickerExp
icker    = compile ickerUr

compile ∷ Ur → IO Code
compile = Ur.simp >>> \case
    Fast _ (Ur.Slow n t b) [] → jetCode n b
    body                      → funCode body

urExp ∷ Ur → Exp
urExp = go
  where
    go = \case
        x Ur.:@ y      → urExp x % urExp y
        Ur.S           → Clo 3 VS []
        Ur.K           → Clo 2 VK []
        Ur.J n         → Kal (Ur.J n) []
        Ur.D           → Kal Ur.D []
        Ur.Fast n j xs → foldl' (%) (fast arity j) (urExp <$> xs)
          where arity = n + nat (length xs)

    --  BKIx →
    fast ∷ Nat → Ur.Jet → Exp
    fast arity jet = jet & \case
        Ur.Eye        → Clo arity VI []
        Ur.Bee        → Clo arity VB []
        Ur.Sea        → Clo arity VC []
        Ur.Sn n       → Clo arity VSn []
        Ur.Bn n       → Clo arity VBn []
        Ur.Cn n       → Clo arity VCn []
        Ur.JSeq       → Clo arity VSeq []
        Ur.Yet n      → Clo arity VYet []
--      Ur.JIff       → Kal (Fast arity jet []) []
        Ur.JIff       → Clo arity VIff []
        Ur.JCas       → Clo arity VCas []
        Ur.Slow _ _ _ → Kal (Fast arity jet []) []
        Ur.JFix       → Kal (Fast arity jet []) []
        Ur.JNat n     → Kal (Fast arity jet []) []
        Ur.JBol n     → Kal (Fast arity jet []) []
        Ur.JPak       → Kal (Fast arity jet []) []
        Ur.JZer       → Kal (Fast arity jet []) []
        Ur.JEql       → Kal (Fast arity jet []) []
        Ur.JAdd       → Kal (Fast arity jet []) []
        Ur.JInc       → Kal (Fast arity jet []) []
        Ur.JDec       → Kal (Fast arity jet []) []
        Ur.JFec       → Kal (Fast arity jet []) []
        Ur.JMul       → Kal (Fast arity jet []) []
        Ur.JSub       → Kal (Fast arity jet []) []
        Ur.JDed       → Kal (Fast arity jet []) []
        Ur.JUni       → Kal (Fast arity jet []) []
        Ur.JLef       → Kal (Fast arity jet []) []
        Ur.JRit       → Kal (Fast arity jet []) []
        Ur.JCon       → Kal (Fast arity jet []) []
        Ur.JCar       → Kal (Fast arity jet []) []
        Ur.JCdr       → Kal (Fast arity jet []) []

{-
    = VSeq
    | VB
    | VC
    | VI
    | VIff
    | VCas
    | VSn Pos
    | VBn Pos
    | VCn Pos

data Exp
    = Clo Pos Node [Exp]
    | Kal Ur [Exp]
    | Rec [Exp]
    | Ref Nat [Exp]
    | Iff Exp Exp Exp [Exp]
    | Cas Exp Exp Exp [Exp]
    | App Exp Exp
-}
-}
