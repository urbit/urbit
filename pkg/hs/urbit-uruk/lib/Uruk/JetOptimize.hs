{-
    ### What should `(add 3 4 5)` compile to?

    Add has arity 2, but we don't want to reduce it.

    There's three kinds of head values:

        1. Things that we *do* want to reduce once saturated.

        2. Things that we want to consider to be opaque.

          - Some of these are actually opaque (arguments)
          - Some of them we just don't want to evaluate during decompilation.

    ### What's the propper way to handle recursion?

    Jet recursion is easy. It will always have the form: `(fix body)`

    Nested recursion is hard. For example,

        /~  %asdf  2
        |=  (x/@ y/@)
        /=  sum  (add x y)
        %.  ~[x y]
        %.  sum
        ..  recur
        |=  (acc xs)
        ?-  xs
        ++  L~       acc
        ++  R[x xs]  (recur (add x acc) xs)
        ==

    Basically, this should not be optimized specially at all. It should
    just be a call to `fix`. For faster code, the compiler should always
    jet recursion points.

    ### What's the propper way to handle branching?

    The simple case is that we see `(iff c t e)`.

      That becomes a branch `?:(c (t ~) (e ~))`

    The other case is that we have a call to `iff` that isn't saturated.

      Same with recursion, this is not our problem.

    Does recognizing this even matter?

      Yes! A recognized branch optimizes to a `goto`. An unrecognized
      branch optimizes to a function call.

    ### What about sum types?

    The simple case, again is `(cas x l r)`.

      This becomes a switch: `?-(c; Lx (l x); Rx (r x))`

    The other case is that we have a call to `cas` that isn't saturated.

      Same with recursion and branching, this is not our problem.

    Does recognizing this even matter?

      Yes! A recognized switch becomes a push and a jump table. An
      unrecognized switch becomes a function call. The function call
      requires pushing callbacks to the stack, and passing a value to
      one of those callbacks. This is real overhead.

    Also, (L x l r) becomes `(l x)` and `(R x l r)` becomes `(R r)`

    ### What about data structures? Sums/Products/Nats

    Skip for now.

      But does it matter?

      I don't know.

      Would allow simplification in some cases.

      But that's getting ahead of ourselves.
-}

module Uruk.JetOptimize where

import ClassyPrelude hiding (try)

import Numeric.Natural  (Natural)
import Numeric.Positive (Positive)
import Uruk.JetDemo     (Ur, UrPoly((:@), Fast))
import Control.Arrow ((>>>))
import Data.Function ((&))
import System.IO.Unsafe

import qualified Uruk.JetDemo as Ur
import qualified Uruk.JetComp as Comp

--------------------------------------------------------------------------------

type Nat = Natural
type Pos = Positive

--------------------------------------------------------------------------------

data Node
    = VSeq
    | VWait
    | VS
    | VK
    | VB
    | VC
    | VI
    | VIff
    | VCas
    | VSn
    | VBn
    | VCn
  deriving stock (Eq, Ord, Generic)
  deriving anyclass NFData

instance Show Node where
    show = \case
        VSeq   → "Q"
        VWait  → "W"
        VS     → "S"
        VK     → "K"
        VB     → "B"
        VC     → "C"
        VI     → "I"
        VIff   → "Iff"
        VCas   → "Cas"
        VSn    → "Sn"
        VBn    → "Bn"
        VCn    → "Cn"

data Code = Code Pos Val
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass NFData

data Val
    = Clo Nat Node [Val]
    | Kal Ur [Val]
    | Rec [Val]
    | Ref Nat [Val]
    | Iff Val Val Val [Val]
    | Cas Val Val Val [Val]
  deriving stock (Eq, Ord, Generic)
  deriving anyclass NFData

instance Show Val where
    show = \case
        Clo r n xs   → sexp "{" "}" [show n] xs
        Kal u xs     → sexp "[" "]" [show u] xs
        Rec xs       → sexp "(" ")" ["Rec"] xs
        Ref n xs     → sexp "(" ")" ["V" <> show n] xs
        Iff c t e xs → sexp "(" ")" ["If", show c, show t, show e] xs
        Cas x l r xs → sexp "(" ")" ["Case", show x, show l, show r] xs
      where
        sexp ∷ Show a => String → String → [String] → [a] → String
        sexp _ _ [h] [] = h
        sexp a z hs  xs = a <> intercalate " " (hs <> (show <$> xs)) <> z

recursive ∷ Code → Bool
recursive (Code _ v) = go v
  where
    go = \case
        Rec _        → True
        Clo _ _ vs   → any go vs
        Ref _ vs     → any go vs
        Kal _ vs     → any go vs
        Iff c t e xs → any go ([c,t,e] <> xs)
        Cas x l r xs → any go ([x,l,r] <> xs)

infixl 5 %;

(%) ∷ Val → Val → Val
f%x = unsafePerformIO $ do
    res <- evaluate (force (call f x))
    putStrLn $ pack (show f <> " % " <> show x <> " -> " <> show res)
    pure res

call ∷ Val → Val → Val
call f x = f & \case
    Clo 1 f xs   → simplify f (snoc xs x)
    Clo n f xs   → Clo (n-1) f (snoc xs x)
    Kal f xs     → Kal f (snoc xs x)
    Rec xs       → Rec (snoc xs x)
    Ref n xs     → Ref n (snoc xs x)
    Iff c t e xs → Iff c t e (snoc xs x)
    Cas x l r xs → Cas x l r (snoc xs x)

simplify ∷ Node → [Val] → Val
simplify = curry $ \case
    ( VSeq,  [x,y]   ) → y
    ( VWait, f:xs    ) → go f xs
    ( VS,    [x,y,z] ) → x % z % (y % z)
    ( VK,    [x,y]   ) → x
    ( VB,    [f,g,x] ) → f % (g % x)
    ( VC,    [f,g,x] ) → f % x % g
    ( VI,    [x]     ) → x
    ( VIff,  [c,t,e] ) → Iff c (t % unit) (e % unit) []
    ( VCas,  [x,l,r] ) → Cas x (abst l%ref 0) (abst r%ref 0) []
    ( VSn,   f:g:xs  ) → f % go g xs
    ( VBn,   f:g:xs  ) → go f xs % g
    ( VCn,   f:g:xs  ) → go f xs % go g xs
    ( _,     _       ) → error "simplify: bad arity"
  where
    go acc = \case { [] → acc; x:xs → go (acc % x) xs }

ref ∷ Nat → Val
ref n = Ref n []

abst ∷ Val → Val
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

unit ∷ Val
unit = Kal Ur.Uni []

nat ∷ Integral i => i → Nat
nat = fromIntegral

pos ∷ Integral i => i → Pos
pos = fromIntegral

{-
    If jet has shape `(fix body)`
      Replace with `(body Rec)`
    Then, pass one ref per arity.
    For example: `jetCode 2 "(fix body)"`
      becomes: `(body Rec $1 $0)`
-}
jetCode ∷ Pos → Ur → Code
jetCode arity = Code arity . addArgs (nat arity) . addRecur
  where
    addArgs ∷ Nat → Val → Val
    addArgs 0 x = x
    addArgs n x = addArgs (n-1) (x % ref (n-1))

    addRecur ∷ Ur → Val
    addRecur = \case
        Ur.Fast 1 Ur.JFix [body] → urVal body % Rec []
        body                     → urVal body

funCode ∷ Ur → Code
funCode body = Code 1 (urVal body % ref 0)

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

compile ∷ Ur → Code
compile = Ur.simp >>> \case
    Fast _ (Ur.Slow n t b) [] → jetCode n b
    body                      → funCode body

urVal ∷ Ur → Val
urVal = go
  where
    go = \case
        x :@ y         → urVal x % urVal y
        Ur.S           → Clo 3 VS []
        Ur.K           → Clo 2 VK []
        Ur.J n         → Kal (Ur.J n) []
        Ur.D           → Kal Ur.D []
        Ur.Fast n j xs → foldl' (%) (fast arity j) (urVal <$> xs)
          where arity = n + nat (length xs)

    fast ∷ Nat → Ur.Jet → Val
    fast arity jet = jet & \case
        Ur.Eye        → Clo arity VI []
        Ur.Bee        → Clo arity VB []
        Ur.Sea        → Clo arity VC []
        Ur.Sn n       → Clo arity VSn []
        Ur.Bn n       → Clo arity VBn []
        Ur.Cn n       → Clo arity VCn []
        Ur.JSeq       → Clo arity VSeq []
        Ur.Wait n     → Clo arity VWait []
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

data Val
    = Clo Pos Node [Val]
    | Kal Ur [Val]
    | Rec [Val]
    | Ref Nat [Val]
    | Iff Val Val Val [Val]
    | Cas Val Val Val [Val]
-}
