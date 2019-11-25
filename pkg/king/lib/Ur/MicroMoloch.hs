{-
    Replace normalization with strict evaluation.

    - [X] Implement a strict interpreter
    - [ ] Jet matching.
-}

module Ur.MicroMoloch where

import Ur.Common hiding (flat)
import GHC.Natural

import Control.Lens (view, from, (^.))
import Data.Flat    (Flat, flat, unflat)
import Noun.Atom    (atomBytes)

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
    {- 05 -} | Fun Exp

    {- 06 -} | Jet Exp
    {- 07 -} | Fir Exp Exp

    {- 08 -} | Eva Exp
    {- 09 -} | Dum Exp
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Flat, NFData)


-- Printing --------------------------------------------------------------------

prettyExp :: Exp -> String
prettyExp = go
  where
    appList (App f x) acc = appList f (x:acc)
    appList e         acc = (e:acc)

    go = \case
        Var i   → show i
        Lam e   → "\\" <> go e
        App f x → "("  <> intercalate " " (go <$> appList f [x]) <> ")"
        Lit n   → "#"  <> show n
        Inc x   → "+"  <> go x
        Fun x   → "%"  <> go x
        Jet x   → "@"  <> go x
        Fir f j → "!"  <> go (App f j)
        Eva x   → "?"  <> go x
        Dum x   → "_"  <> go x

printExp :: Exp -> IO ()
printExp = putStrLn . pack . prettyExp


-- Simple Expressions ----------------------------------------------------------

{-
    zero = \z i → z
    succ n = \z i → i (n z i)
-}
natFun :: Nat -> Exp
natFun 0 = Lam $ Lam $ Var 1
natFun n = Lam $ Lam $ App (Var 0) (natFun (pred n) `App` Var 1 `App` Var 0)

{-
    lef x l r = l x
    rit x l r = l x
-}
lef, rit :: Exp -> Exp
lef x = Lam $ Lam (App (Var 1) x)
rit x = Lam $ Lam (App (Var 0) x)

{-
    () = \x→x
-}
unit :: Exp
unit = Lam (Var 0)


-- Interpreter -----------------------------------------------------------------

data Val
    = VFun Exp [Val]
    | VNat Nat
    | VJet Val
  deriving (Eq, Ord, Show)

prettyVal :: Val -> String
prettyVal = prettyExp . valExp
    -- VFun b ev → valExp
    -- VFun b ev → prettyExp b <> "[" <> intercalate " " (prettyVal <$> ev) <> "]"
    -- VNat n    → "#" <> show n
    -- VJet x    → "@" <> prettyVal x

printVal :: Val -> IO ()
printVal = putStrLn . pack . prettyVal

run :: Exp -> IO ()
run = go 1 [] >=> printVal
  where
    go :: Nat -> [Val] -> Exp -> IO Val
    go d env exp = do

        let loop = go (succ d)

        -- let pre = replicate (fromIntegral d) '#' <> "  "
        -- putStrLn $ pack (pre <> prettyExp exp)
        -- for_ (zip [0..] env) $ \(i, v) → do
            -- let pre = "\t[" <> show i <> "]\t"
            -- putStrLn $ pack (pre <> prettyVal v)

        res <- exp & \case
            Lam b   -> pure (VFun b env)
            App f a -> do VFun b fenv <- loop env f
                          a'          <- loop env a
                          loop (a' : fenv) b
            Var n   -> evaluate (env !! fromIntegral n)
            Lit n   -> pure (VNat n)
            Inc x   -> do VNat n <- loop env x
                          pure $ VNat $ succ n
            Fun x   -> do VNat n <- loop env x
                          loop [] (natFun n)
            Jet x   -> VJet <$> loop env x
            Fir f j -> do VFun b fenv <- loop env f
                          VJet x      <- loop env j
                          loop (x : fenv) b
            Eva x   -> do VNat n <- loop env x
                          loop [] (eval n)
            Dum x   -> do xv <- loop env x
                          pure $ VNat $ dump $ valExp xv

        -- let pre = replicate (fromIntegral d) '>' <> "  "
        -- putStrLn $ pack (pre <> prettyVal res)

        pure res

valExp :: Val -> Exp
valExp = \case
    VNat n     → Lit n
    VJet x     → Jet (valExp x)
    VFun b []  → Lam b
    VFun b env → shift (negate $ fromIntegral $ length env)
               $ foldl' (\exp (i,v) -> subst i (valExp v) exp) b
               $ zip [0..] env


-- Reductions ------------------------------------------------------------------

{-| Substitute all occurrences of a variable with an expression

> subst n C B  ~  B[x@n := C]
-}
subst :: Integer -> Exp -> Exp -> Exp
subst n e' e = case e of
    Lam b   -> Lam (subst (succ n) (shift 1 e') b)
    App f a -> App (subst n e' f)  (subst n e' a)
    Var n'  -> if n == n' then e' else e
    Lit n   -> Lit n
    Inc x   -> Inc (subst n e' x)
    Fun x   -> Fun (subst n e' x)
    Jet x   -> Jet (subst n e' x)
    Fir j a -> Fir (subst n e' j) (subst n e' a)
    Eva x   -> Eva (subst n e' x)
    Dum x   -> Dum (subst n e' x)

{-| @shift n x@ adds @n@ to the index of all free variables named @x@ within an
    `Exp`
-}
shift :: Integer -> Exp -> Exp
shift d e0 = go e0 0
  where
    go e c = case e of
        Lam b   -> Lam (go  b $! succ c)
        App f a -> App (go f c) (go a c)
        Var n   -> Var $! if n >= c then n + d else n
        Lit n   -> Lit n
        Inc x   -> Inc (go x c)
        Fun x   -> Fun (go x c)
        Jet f   -> Jet (go f c)
        Fir j a -> Fir (go j c) (go a c)
        Eva x   -> Eva (go x c)
        Dum x   -> Dum (go x c)

-- | Returns whether a variable is free in an expression
freeIn :: Integer -> Exp -> Bool
freeIn n = go
  where
    go = \case
        Lam b   -> freeIn (succ n) b
        Var n'  -> n == n'
        App f x -> go f || go x
        Lit _   -> False
        Inc x   -> go x
        Fun x   -> go x
        Jet x   -> go x
        Fir f x -> go f || go x
        Eva x   -> go x
        Dum x   -> go x

-- | Reduce an expression to weak-head normal form
whnf :: Exp -> Exp
whnf = \case
    App f a -> app a (whnf f)
    e       -> e
  where
    app a = \case
        Lam b -> appLam a b
        f'    -> App f' a

    appLam a b = whnf (shift (-1) b')
      where
        a' = shift 1 a
        b' = subst 0 a' b

{-| Reduce an expression to its normal form, performing both beta reduction and
    eta reduction

    `normalize` does not type-check the expression.  You may want to type-check
    expressions before normalizing them since normalization can convert an
    ill-typed expression into a well-typed expression.
-}
normalize :: Exp -> Exp
normalize e = case e of
    Lam b -> case b' of App f a -> app f a
                        _       -> Lam b'
      where
        b'    = normalize b
        app f = whnf >>> \case
            Var v' | 0 == v' && not (0 `freeIn` f) -> shift (-1) f
            Var _                                  -> Lam b'
            _                                      -> Lam b'

    App f a -> normalize f & \case
        Lam b -> normalize (shift (-1) b')
                   where
                     a' = shift 1 (normalize a)
                     b' = subst 0 a' b
        f'    -> App f' (normalize a)

    Var _ -> e
    Lit _ -> e

    Inc x -> normalize x & \case
       Lit n  -> Lit (succ n)
       xn     -> Inc xn

    Fun x -> normalize x & \case
        Lit n -> natFun n
        e     -> Fun e

    Jet f   -> Jet (normalize f)
    Fir f j -> Fir (normalize f) (normalize j)
    Eva x   -> Eva (normalize x)
    Dum x   -> Dum (normalize x)

exe :: Exp -> Exp
exe e = case e of
    Lam b -> case b' of
        App f a -> app f a
        _       -> Lam b'
      where
        b'    = exe b
        app f = whnf >>> \case
            Var v' | 0 == v' && not (0 `freeIn` f) -> shift (-1) f
            Var _                                  -> Lam b'
            _                                      -> Lam b'

    App f a -> exe f & \case
        Lam b -> exe (shift (-1) b')
                   where
                     a' = shift 1 (exe a)
                     b' = subst 0 a' b
        f'    -> App f' (exe a)

    Var _ -> e
    Lit _ -> e

    Inc x -> exe x & \case
       Lit n  -> Lit (succ n)
       xn     -> Inc xn

    Fun x -> exe x & \case
        Lit n -> natFun n
        e     -> Fun e

    Jet f   -> Jet (exe f)

    Fir f j -> exe j & \case Jet x -> exe (App f x)
                             j'    -> Fir f j'

    Eva x   -> exe x & \case Lit n -> exe (eval n)
                             x'    -> Eva x'

    Dum x   -> Lit (dump (exe x))


eval :: Nat -> Exp
eval expNat =
    decodeExp expNat & \case
        Just exp -> exp
        Nothing  -> error "bad eval"
    -- maybe (lef unit) rit $ do
        -- decodeExp expNat

dump :: Exp -> Nat
dump = encodeExp

decodeExp :: Nat -> Maybe Exp
decodeExp n =
    unflat (n ^. atomBytes) & \case
        Left err -> Nothing
        Right ex -> pure ex

encodeExp :: Exp -> Nat
encodeExp = view (from atomBytes) . flat


-- Examples --------------------------------------------------------------------

jet1 :: Exp
jet1 =  Lam $ Lam $ Fir (Lam (Var 0 `App` Var 1)) (Jet $ Var 1)

jet2 :: Exp
jet2 =  Lam $ Lam $ Lam $ Fir (Lam (Var 0 `App` Var 2 `App` Var 1))
                              (Jet $ Var 2)

incRaw ∷ Exp
incRaw = Lam $ Inc (Var 0)

inc ∷ Exp
inc = App jet1 incRaw

{-
    add x y = foldNat y (foldNat x 0 inc) inc
-}
addRaw :: Exp
addRaw = (Lam . Lam)
        $ (Fun $ Var 1)
            `App` ((Fun $ Var 0) `App` Lit 0 `App` incRaw)
            `App` incRaw

{-
    add x y = foldNat y (foldNat x 0 inc) inc
-}
add :: Exp
add = (App jet2 . Lam . Lam)
    $ (Fun $ Var 1)
        `App` ((Fun $ Var 0) `App` Lit 0 `App` inc)
        `App` inc

{-
    mul x y = foldNat x 0 (add y)
-}
mul :: Exp
mul = (App jet2 . Lam . Lam)
    $ (Fun $ Var 1)
        `App` Lit 0
        `App` (add `App` Var 0)
