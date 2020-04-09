module Urbit.UrukRTS.JetOptimize where

import ClassyPrelude hiding (try, evaluate)
import System.IO.Unsafe

import Control.Arrow    ((>>>))
import Data.Function    ((&))
import Numeric.Natural  (Natural)
import Numeric.Positive (Positive)
import Prelude          ((!!))

import qualified GHC.Exts            as GHC
import qualified Urbit.UrukRTS.Types as F

--------------------------------------------------------------------------------

type Nat = Natural
type Pos = Positive

--------------------------------------------------------------------------------

data Node
    = VSeq
    | VS
    | VK
    | VIn Pos
    | VBn Pos
    | VCn Pos
    | VSn Pos
    | VIff
    | VCas
  deriving stock (Eq, Ord, Generic)

instance Show Node where
  show = \case
    VSeq  -> "Q"
    VS    -> "S"
    VK    -> "K"
    VIn 1 -> "I"
    VBn 1 -> "B"
    VCn 1 -> "C"
    VIn n -> "I" <> show n
    VSn n -> "S" <> show n
    VBn n -> "B" <> show n
    VCn n -> "C" <> show n
    VIff  -> "Iff"
    VCas  -> "Cas"

data Code = Code
    { cArgs :: Pos
    , cName :: F.Val
    , cBody :: F.Val
    , cFast :: Val
    }
  deriving stock (Eq, Ord, Generic)

syms = singleton <$> "xyzpqrstuvwxyzabcdefghijklmnop"

sym i | i >= length syms = "v" <> show i
sym i                    = syms !! i

instance Show Code where
    show c@(Code n nm _ v) =
        regr <> header (fromIntegral n) <> prettyVal v
      where
        arity ∷ Int
        arity = fromIntegral n

        regr = "~/  " <> show n <> "  " <> show nm <> "\n"

        header ∷ Int → String
        header 0 | recursive c = "..  $\n"
        header 0               = ""
        header n               = header (n-1) <> "|=  " <> sym (arity - n) <> "\n"

{- |
    There are three kinds of things

    - Functions that we know how to reduce.
    - Functions that we don't know how to reduce.
    - Functions that we want to turn into control flow.
    - Recursive references.
    - Stack references.

    `Kal` is a function that we don't want to reduce.

    `Clo` is a partially-saturated thing that we *do* know how to reduce.

    `Rec` and `Ref` are recursive calls and stack references.

    `App` is unevaluated function application.

    `Cas` and `Iff` are understood control flow.
-}
data Exp
    = Clo Int Node [Exp]
    | Kal F.Node [Exp]
    | Rec [Exp]
    | Ref Nat [Exp]
    | Iff Exp Exp Exp [Exp]
    | Cas Exp Exp Exp [Exp]
    | App Exp Exp
  deriving stock (Eq, Ord, Generic)

{- |
    A `Val` is the same as an expression except that it contains no
    `App` nodes (and no `Clo` nodes).

    - Everything has been evaluated as far as possible, and everything
      is in closure-form.

    - `Clo` nodes are eliminated because the distinction between `Kal`
      and `Clo` is only relevant during the reduction that happens in
      this module.
-}
data Val
    = ValKal F.Node [Val]
    | ValRec [Val]
    | ValRef Nat [Val]
    | ValIff Val Val Val [Val]
    | ValCas Val Val Val [Val]
  deriving stock (Eq, Ord, Generic)

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

instance Show Val where
    show = \case
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
recursive (Code _ _ _ v) = go v
  where
    go = \case
        ValRec _        → True
        ValRef _ vs     → any go vs
        ValKal _ vs     → any go vs
        ValIff c t e xs → any go ([c,t,e] <> xs)
        ValCas x l r xs → any go ([x,l,r] <> xs)

infixl 5 %;

(%) ∷ Exp → Exp → Exp
(%) = App

simplify :: Node -> [Exp] -> Exp
simplify = curry $ \case
  (VS    , [x, y, z] ) -> (x % z) % (y % z)
  (VK    , [x, y]    ) -> x
  (VSeq  , [x, y]    ) -> y
  (VIn _ , f : xs    ) -> go f xs
  (VBn _ , f : g : xs) -> f % go g xs
  (VCn _ , f : g : xs) -> go f xs % g
  (VSn _ , f : g : xs) -> go f xs % go g xs
  (VIff  , [c, t, e] ) -> Iff c (t % unit) (e % unit) []
  (VCas  , [x, l, r] ) -> Cas x (abst l % ref 0) (abst r % ref 0) []
  (n     , xs        ) -> error ("simplify: bad arity (" <> show n <> " " <> show xs <> ")")
 where
  go acc = \case
    []     -> acc
    x : xs -> go (acc % x) xs

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
        App x y         → error "TODO: Handle `App` in `abst`"

unit ∷ Exp
unit = Kal F.Uni []

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
    Ref n xs     :@ x → Just $ Ref n (snoc xs x)

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
    App x y      → error "TODO: Handle `App` in `call` (?)"

eval ∷ Exp → IO Exp
eval exp = do
    --  putStrLn (pack $ prettyExp exp)
    nok exp & \case
        Nothing → pure exp
        Just e' → eval e'

nodeRaw :: Nat -> Node -> F.Node
nodeRaw arity = \case
  VS    -> F.Ess
  VK    -> F.Kay
  VIn n -> F.Eye (fromIntegral n)
  VBn n -> F.Bee (fromIntegral n)
  VCn n -> F.Sea (fromIntegral n)
  VSn n -> F.Sen (fromIntegral n)
  VSeq  -> F.Seq
  VIff  -> F.Iff
  VCas  -> F.Cas

evaluate ∷ Exp → IO Val
evaluate = fmap go . eval
  where
    go = \case
        Clo r n xs   → ValKal (nodeRaw (fromIntegral r) n) (go <$> xs)
        Kal u xs     → ValKal u (go <$> xs)
        Rec xs       → ValRec (go <$> xs)
        Ref n xs     → ValRef n (go <$> xs)
        Iff c t e xs → ValIff (go c) (go t) (go e) (go <$> xs)
        Cas v l r xs → ValCas (go v) (go l) (go r) (go <$> xs)
        App x y      → trace (show x) $
                       trace (show y) $
                       error "This should not happen"

{-
    If jet has shape `(fix body)`
      Replace with `(body Rec)`
    Then, pass one ref per arity.
    For example: `jetCode 2 "(fix body)"`
      becomes: `(body Rec $1 $0)`
-}
jetCode :: Pos -> F.Val -> F.Val -> IO Code
jetCode arity nm bod =
  (fmap (Code arity nm bod) . evaluate . addArgs (nat arity) . addRecur) bod
 where

  addArgs :: Nat -> Exp -> Exp
  addArgs 0 x = x
  addArgs n x = addArgs (n - 1) (x % ref (n - 1))

  addRecur :: F.Val -> Exp
  addRecur = \case
    F.VFun (F.Fun 1 F.Fix xs) -> fastExp (F.getCloN xs 0) % Rec []
    body                      -> fastExp body

funCode ∷ F.Val → IO Code
funCode body = Code 1 fak fak <$> evaluate (fastExp body % ref 0)
 where
  fak = error "TODO: funCode.fak"

compile ∷ Int → F.Val → F.Val → IO Code
compile n t b = jetCode (fromIntegral n) t b

funVal :: F.Fun -> Val
funVal (F.Fun _ f xs) = ValKal f (fastVal <$> GHC.toList xs)

fastVal :: F.Val -> Val
fastVal = funVal . F.valFun

{-
    x F.:@ y      -> goAcc (go y : acc) x
    F.S           -> ValKal F.Ess acc
    F.K           -> ValKal F.Kay acc
    F.J n         -> ValKal (F.Jay 2) acc
    F.D           -> ValKal F.Dee acc
    F.Fast _ j xs -> ValKal (jRaw j) (fmap go xs <> acc)

  jRaw :: F.Jet -> F.Node
  jRaw = \case
    F.Slow r n b -> F.Jet r (go n) (go b)
    F.Eye        -> F.Eye
    F.Bee        -> F.Bee
    F.Sea        -> F.Sea
    F.Sn n       -> F.Sen n
    F.Bn n       -> F.Ben n
    F.Cn n       -> F.Sea n
    F.JSeq       -> F.Seq
    F.Eye n      -> F.Eye n
    F.JFix       -> F.Fix
    F.JNat n     -> F.Nat n
    F.JBol b     -> F.Bol b
    F.JIff       -> F.Iff
    F.JPak       -> F.Pak
    F.JZer       -> F.Zer
    F.JEql       -> F.Eql
    F.JAdd       -> F.Add
    F.JInc       -> F.Inc
    F.JDec       -> F.Dec
    F.JFec       -> F.Fec
    F.JMul       -> F.Mul
    F.JSub       -> F.Sub
    F.JDed       -> F.Ded
    F.JUni       -> F.Uni
    F.JLef       -> F.Lef
    F.JRit       -> F.Rit
    F.JCas       -> F.Cas
    F.JCon       -> F.Con
    F.JCar       -> F.Car
    F.JCdr       -> F.Cdr
-}

valExp :: Val -> Exp
valExp = go
 where
  go :: Val -> Exp
  go = \case
    ValKal rn xs    -> rawExp rn (go <$> xs)
    ValRec xs       -> Rec (go <$> xs)
    ValRef n xs     -> Ref n (go <$> xs)
    ValIff c t e xs -> Iff (go c) (go t) (go e) (go <$> xs)
    ValCas x l r xs -> Cas (go x) (go l) (go r) (go <$> xs)

  rawExp :: F.Node -> [Exp] -> Exp
  rawExp rn xs = rn & \case
    F.Kay   -> clo 2 VK
    F.Ess   -> clo 3 VS
    F.Eye n -> clo (int n)     (VIn $ fromIntegral n)
    F.Bee n -> clo (int n + 2) (VBn $ fromIntegral n)
    F.Sea n -> clo (int n + 2) (VCn $ fromIntegral n)
    F.Sen n -> clo (int n + 2) (VSn $ fromIntegral n)
    F.Seq   -> clo 2 VSeq
    F.Iff   -> clo 3 VIff
    F.Cas   -> clo 3 VCas
    other  -> kal other
   where
    kal :: F.Node -> Exp
    kal n = Kal n xs

    clo :: Int -> Node -> Exp
    clo r n = Clo (r - args) n xs

    args :: Int
    args = length xs

    int :: Integral i => i -> Int
    int = fromIntegral

fastExp :: F.Val -> Exp
fastExp = valExp . fastVal
