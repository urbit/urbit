{-
    Replace normalization with strict evaluation.

    - [X] Implement a strict interpreter
    - [X] Jet matching.
-}

module Ur.MilliMoloch where

import Ur.Common hiding (flat)
import GHC.Natural

import Control.Lens     (from, view, (^.))
import Data.Flat        (Flat, flat, unflat)
import Urbit.Atom       (atomBytes, bytesAtom)
import System.IO.Unsafe (unsafePerformIO)

--------------------------------------------------------------------------------

type Nat = Natural


-- Expressions -----------------------------------------------------------------

-- | Syntax tree for expressions
data Exp
    {- 00 -} = Var Integer
    {- 01 -} | Lam Exp
    {- 02 -} | App Exp Exp
    {- 03 -} | Lit Nat
    {- 04 -} | Inc Exp
    {- 05 -} | Dec Exp
    {- 06 -} | Fix Exp
    {- 07 -} | Box Exp
    {- 08 -} | Fir Exp Exp
    {- 09 -} | Eva Exp
    {- 10 -} | Dum Exp
    {- 11 -} | Uni
    {- 12 -} | Con Exp Exp
    {- 13 -} | Get Nat Exp
    {- 14 -} | Tag Nat Exp
    {- 15 -} | Eat Exp Exp Exp
  deriving (Eq, Ord, Generic)
  deriving anyclass (Flat, NFData)


-- Printing --------------------------------------------------------------------

instance Show Exp where
  show = go
    where
      appList (App f x) acc = appList f (x:acc)
      appList e         acc = (e:acc)

      conList (Con l r) acc = conList r (l:acc)
      conList e         acc = reverse (e:acc)

      go = \case
          Var i     → varName i
          Lam e     → "\\"  <> go e
          App f x   → "("   <> intercalate " " (go <$> appList f [x]) <> ")"
          Fix x     → "&"   <> go x
          Lit n     → show n
          Inc x     → "+"   <> go x
          Dec x     → "-"   <> go x
          Box x     → "@"   <> go x
          Fir j x   → "!"   <> go (App j x)
          Eva x     → "?"   <> go x
          Dum x     → "_"   <> go x
          Uni       → "~"
          Con l r   → "["   <> intercalate " " (go <$> conList r [l]) <> "]"
          Get 2 x   → "<"   <> go x
          Get 3 x   → ">"   <> go x
          Get i x   → "G("  <> show i <> " " <> go x <> ")"
          Tag 2 x   → "L"   <> go x
          Tag 3 x   → "R"   <> go x
          Tag t v   → "T("  <> show t <> " " <> go v <> ")"
          Eat x l r → mconcat [ "%(", go x, " ", go l, " ", go r, ")" ]


varName :: Integral i => i -> String
varName (fromIntegral -> i) = vars !! i

vars :: [String]
vars = from 1
  where
    from n = go n <> from (succ n)

    go 0 = []
    go 1 = singleton <$> ['a' .. 'z']
    go n = do c <- go 1
              cs <- go (pred n)
              pure (c <> cs)


-- Ig -- Values with no semantic meaning ---------------------------------------

newtype Ig a = Ig a

instance Eq (Ig a) where
  x == y = True

instance Ord (Ig a) where
  compare x y = EQ

instance Show (Ig a)
  where
    show = const "Ig"


-- Interpreter Values ----------------------------------------------------------

data Val
    = VFun Exp [Val]
    | VFix Exp [Val]
    | VNat Nat
    | VJet Val (Ig (Val → IO Val))
    | VCon Val Val
    | VLef Val
    | VUni
    | VRit Val
  deriving (Eq, Ord, Show)

-- instance Show Val where
    -- show = show . valExp


-- Interpreter -----------------------------------------------------------------

run :: Exp -> IO ()
run = runEnv [] >=> print

runEnv :: [Val] -> Exp -> IO Val
runEnv = go 1
  where
    go :: Nat -> [Val] -> Exp -> IO Val
    go d env exp = do

        let loop = go (succ d)

        -- let pre = replicate (fromIntegral d) ' ' <> "  "
        -- putStrLn $ pack (pre <> show exp)
        -- for_ (zip [0..] env) $ \(i, v) → do
            -- putStrLn $ pack (pre <> "    - " <> varName i <> " <- " <> show v)

        case exp of
            App f x -> do
                let pre = replicate (fromIntegral d) '#' <> "  "
                putStrLn $ pack (pre <> show f)
                putStrLn $ pack (pre <> "    " <> show x)
                for_ (zip [0..] env) $ \(i, v) → do
                    putStrLn $ pack (pre <> "    - " <> varName i <> " <- " <> show v)
            Fir j x -> do
                let pre = replicate (fromIntegral d) '!' <> "  "
                putStrLn $ pack (pre <> show j)
                putStrLn $ pack (pre <> "    " <> show x)
                for_ (zip [0..] env) $ \(i, v) → do
                    putStrLn $ pack (pre <> "    - " <> varName i <> " <- " <> show v)
            _ -> pure ()

        res <- exp & \case
            Lam b   -> pure (VFun b env)
            App f a -> do av <- loop env a
                          fv <- loop env f
                          fv & \case
                              VFun b fenv -> loop (av : fenv) b
                              VFix b fenv -> loop (av : fv : fenv) b
                              _           -> error "bad-app"
            Var n   -> evaluate (env !! fromIntegral n)
            Fix x   -> do xv@(VFun b fenv) ← loop env x
                          pure (VFix b fenv)

            Lit n   -> pure (VNat n)
            Inc x   -> do VNat n <- loop env x
                          pure $ VNat $ succ n
            Dec x   -> do loop env x >>= \case
                            VNat 0 -> pure $ VLef VUni
                            VNat n -> pure $ VRit (VNat (pred n))
                            v      -> print v >> error "bad-dec"

            Box x   -> do xv ← loop env x
                          pure (jet xv)
            Fir j x -> do VJet jv (Ig jf) <- loop env j
                          xv              <- loop env x

                          jf xv
            Eva x   -> do VNat n <- loop env x
                          loop [] (eval n)
            Dum x   -> do xv <- loop env x
                          pure $ VNat $ dump $ valExp xv

            Uni     -> pure VUni

            Con l r -> VCon <$> loop env l <*> loop env r
            Get i v -> getIdx i <$> loop env v
            Tag t v -> tag t <$> loop env v
            Eat x l r -> do VFun lf lenv <- loop env l
                            VFun rf renv <- loop env r
                            loop env x >>= \case
                              VLef v -> loop (v:lenv) lf
                              VRit v -> loop (v:renv) rf
                              _      -> error "bad-eat"

        -- let pre = replicate (fromIntegral d) '>' <> "  "
        -- putStrLn $ pack (pre <> show res)
        -- for_ (zip [0..] env) $ \(i, v) → do
            -- putStrLn $ pack (pre <> "    - " <> varName i <> " <- " <> show v)

        pure res

getIdx :: Nat -> Val -> Val
getIdx 0 v          = error "bad-get"
getIdx 1 v          = v
getIdx 2 (VCon l _) = l
getIdx 3 (VCon _ r) = r
getIdx i v | even i = getIdx 2 (getIdx (i `div` 2) v)
getIdx i v          = getIdx 3 (getIdx (i `div` 2) v)

tag :: Nat -> Val -> Val
tag 0 v          = error "bad-tag"
tag 1 v          = v
tag 2 v          = VLef v
tag 3 v          = VRit v
tag t v | even t = VLef $ tag (t `div` 2) v
tag t v          = VRit $ tag (t `div` 2) v

-- Jets ------------------------------------------------------------------------

dashboard :: Map Exp (Text, Val → IO Val)
dashboard = mapFromList
    [ (simp addJetBody, ("ADD", addFast))
    , (simp mulJetBody, ("MUL", mulFast))
    ]
  where
    addFast :: Val → IO Val
    addFast (VCon (VNat x) (VNat y)) = pure $ VNat (x + y)
    addFast _                        = error "bad-add"

    mulFast :: Val → IO Val
    mulFast (VCon (VNat x) (VNat y)) = pure $ VNat (x * y)
    mulFast _                        = error "bad-mul"

simp :: Exp → Exp
simp = valExp . unsafePerformIO . runEnv []

jet :: Val → Val
jet jetVal = lookup (valExp jetVal) dashboard & \case
    Just (nm, exe) → VJet jetVal $ Ig (match nm exe)
    Nothing        → VJet jetVal $ Ig fallback
  where
    match :: Text → (Val → IO Val) → (Val → IO Val)
    match nam exe arg = putStrLn nam >> exe arg

    fallback :: Val → IO Val
    fallback arg = do
        putStrLn "NOJET"
        jetVal & \case
            VFun b env → runEnv (arg:env) b
            _          → error "Fir expects a function"


-- Convert values to expressions for `uneval` ----------------------------------

valExp :: Val -> Exp
valExp = \case
    VNat n     → Lit n
    VUni       → Uni
    VJet x _   → Box (valExp x)
    VFun b []  → Lam b
    VCon l r   → Con (valExp l) (valExp r)
    VLef x     → Tag 2 (valExp x)
    VRit x     → Tag 3 (valExp x)
    VFix b e   → Fix (valExp (VFun b e))
    VFun b env → shift (negate $ fromIntegral $ length env)
               $ foldl' (\exp (i,v) -> subst i (valExp v) exp) (Lam b)
               $ zip [0..] env
  where
    {-| Substitute all occurrences of a variable with an expression

    > subst n C B  ~  B[x@n := C]
    -}
    subst :: Integer -> Exp -> Exp -> Exp
    subst n e' e = case e of
        Lam b     -> Lam (subst (succ n) (shift 1 e') b)
        App f a   -> App (subst n e' f)  (subst n e' a)
        Var n'    -> if n == fromIntegral n' then e' else e
        Lit n     -> Lit n
        Inc x     -> Inc (subst n e' x)
        Dec x     -> Dec (subst n e' x)
        Box x     -> Box (subst n e' x)
        Fir j a   -> Fir (subst n e' j) (subst n e' a)
        Eva x     -> Eva (subst n e' x)
        Dum x     -> Dum (subst n e' x)
        Con x y   -> Con (subst n e' x) (subst n e' y)
        Get x y   -> Get x (subst n e' y)
        Tag x y   -> Tag x (subst n e' y)
        Eat x l r -> Eat (subst n e' x) (subst n e' l) (subst n e' r)
        Fix x     -> Fix (subst n e' x)
        Uni       -> Uni

    {-| @shift n x@ adds @n@ to the index of all free variables named @x@
        within an `Exp`
    -}
    shift :: Integer -> Exp -> Exp
    shift d e0 = go e0 0
      where
        go e c = case e of
            Lam b     -> Lam (go  b $! succ c)
            App f a   -> App (go f c) (go a c)
            Var n     -> Var $! if n >= c then n + d else n
            Fix f     -> Fix (go f c)

            Lit n     -> Lit n
            Inc x     -> Inc (go x c)
            Dec x     -> Dec (go x c)

            Box f     -> Box (go f c)
            Fir j a   -> Fir (go j c) (go a c)

            Eva x     -> Eva (go x c)
            Dum x     -> Dum (go x c)

            Uni       -> Uni

            Con x y   -> Con (go x c) (go y c)
            Get x y   -> Get x (go y c)

            Tag x y   -> Tag x (go y c)
            Eat x l r -> Eat (go x c) (go l c) (go r c)



-- Eval and Dump ---------------------------------------------------------------

eval :: Nat -> Exp
eval expNat =
    -- maybe (lef unit) rit (decodeExp expNat)
    decodeExp expNat & \case
        Just exp -> exp
        Nothing  -> error "bad eval"
  where
    decodeExp :: Nat -> Maybe Exp
    decodeExp n =
        unflat (atomBytes n) & \case
            Left err -> Nothing
            Right ex -> pure ex

dump :: Exp -> Nat
dump = bytesAtom . flat


-- Jetted Increment, Add, and Multiply -----------------------------------------

--  curry f x y = f (x, y)
mCurry :: Exp
mCurry = Lam $ Lam $ Lam (Var 2 `App` (Con (Var 1) (Var 0)))

car = Get 2
cdr = Get 3

--  uncurry f (x, y) = f x y
mUncurry :: Exp
mUncurry = Lam $ Lam $ Var 1 `App` car (Var 0)
                             `App` cdr (Var 0)

mkJet1 :: Exp -> (Exp, Exp)
mkJet1 x = (Lam $ Fir (Box x) (Var 0), x)

mkJet2 :: Exp -> (Exp, Exp)
mkJet2 x = (ifac, body)
  where
    ifac = Lam $ Lam $ Fir (Box body) (Con (Var 1) (Var 0))
    body = Lam (x `App` car (Var 0) `App` cdr (Var 0))

--  inc x = +(x)
incRaw ∷ Exp
incRaw = Lam $ Inc (Var 0)

fix :: Exp -> Exp
fix = Fix
-- fix = (zCombinator `App`)

--  z f = (\x → f (\v → x x v))
--        (\x → f (\v → x x v))
--
--  λ f = (λ(1 (λ(1 1 0))))
--        (λ(1 (λ(1 1 0))))
--
--  λ(λ(1 λ(1 1 0)) λ(1 λ(1 1 0)))
zCombinator :: Exp
zCombinator = Lam (bod `App` bod)
  where
    bod = Lam
        $ Var 1 `App` (Lam (Var 1 `App` Var 1 `App` Var 0))

--  add (x, y) = go x y
--    where
--      go 0 y = y
--      go x y = go (pred x) (succ y)
addRaw :: Exp
addRaw = fix -- go
       $ Lam -- x
       $ Lam -- y
       $ Eat (Dec (Var 1))
             (Lam (Var 1))
             (Lam (Var 3 `App` Var 0 `App` Inc (Var 1)))

--  mul add x = go
--    where
--      go 0  = 0
--      go n  = go n = add x (go (n-1))
mulRaw :: Exp
mulRaw = Lam -- add
       $ Lam -- x
       $ Fix -- go
       $ Lam -- y
       $ Eat (Dec (Var 0))
             (Lam (Lit 0))
             (Lam (Var 4 `App` Var 3 `App` (Var 2 `App` Var 0)))

inc, add, mul :: Exp
incJetBody, addJetBody, mulJetBody :: Exp
(inc, incJetBody) = mkJet1 incRaw
(add, addJetBody) = mkJet2 addRaw
(mul, mulJetBody) = mkJet2 (mulRaw `App` add)

encodeList :: [Exp] -> Exp
encodeList []     = Tag 2 Uni
encodeList (x:xs) = Tag 3 $ Con x $ encodeList xs
