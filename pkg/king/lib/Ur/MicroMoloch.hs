{-
    Replace normalization with strict evaluation.

    - [X] Implement a strict interpreter
    - [ ] Jet matching.
-}

module Ur.MicroMoloch where

import Ur.Common hiding (flat)
import GHC.Natural

import Control.Lens     (from, view, (^.))
import Data.Flat        (Flat, flat, unflat)
import Noun.Atom        (atomBytes)
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
    {- 05 -} | Fun Exp
    {- 06 -} | Jet Exp
    {- 07 -} | Fir Exp Exp
    {- 08 -} | Eva Exp
    {- 09 -} | Dum Exp
  deriving (Eq, Ord, Generic)
  deriving anyclass (Flat, NFData)


-- Printing --------------------------------------------------------------------

instance Show Exp where
  show = go
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


-- Simple Expressions ----------------------------------------------------------

--  zero   = \z i → z
--  succ n = \z i → i (n z i)
natFun :: Nat -> Exp
natFun 0 = Lam $ Lam (Var 1)
natFun n = Lam $ Lam (Var 0 `App` (natFun (pred n) `App` Var 1 `App` Var 0))

--  lef x l r = l x
--  rit x l r = l x
lef, rit :: Exp -> Exp
lef x = Lam $ Lam (Var 1 `App` x)
rit x = Lam $ Lam (Var 0 `App` x)

--  () = \x→x
unit :: Exp
unit = Lam (Var 0)


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
    | VNat Nat
    | VJet Val (Ig (Exp → [Val] → IO Val))
  deriving (Eq, Ord)

instance Show Val where
    show = show . valExp


-- Interpreter -----------------------------------------------------------------

run :: Exp -> IO ()
run = runEnv [] >=> print

runEnv :: [Val] -> Exp -> IO Val
runEnv = go 1
  where
    go :: Nat -> [Val] -> Exp -> IO Val
    go d env exp = do

        let loop = go (succ d)

        -- let pre = replicate (fromIntegral d) '#' <> "  "
        -- putStrLn $ pack (pre <> show exp)
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
            Jet x   -> do xv ← loop env x
                          pure (jet xv)
            Fir f j -> do VJet x (Ig jf) <- loop env j
                          jf f env
            Eva x   -> do VNat n <- loop env x
                          loop [] (eval n)
            Dum x   -> do xv <- loop env x
                          pure $ VNat $ dump $ valExp xv

        -- let pre = replicate (fromIntegral d) '>' <> "  "
        -- putStrLn $ pack (pre <> prettyVal res)

        pure res


-- Jets ------------------------------------------------------------------------

dashboard :: Map Val (Text, Exp, [Val] → IO Val)
dashboard = mapFromList
    [ (incBody, ("INC", fire1Exp, incFast))
    , (addBody, ("ADD", fire2Exp, addFast))
    , (mulBody, ("MUL", fire2Exp, mulFast))
    ]
  where
    incFast, addFast, mulFast :: [Val] → IO Val

    incFast = \case (VNat x:_)        → pure $ VNat (succ x)
                    _                 → error "bad-inc"
    addFast = \case (VNat x:VNat y:_) → pure $ VNat (x + y)
                    _                 → error "bad-add"
    mulFast = \case (VNat x:VNat y:_) → pure $ VNat (x * y)
                    _                 → error "bad-mul"

    incBody = unsafePerformIO $ runEnv [] incRaw
    addBody = unsafePerformIO $ runEnv [] addClose
    mulBody = unsafePerformIO $ runEnv [] mulClose

    fire1Exp = Lam (Var 0 `App` Var 1)
    fire2Exp = Lam (Var 0 `App` Var 2 `App` Var 1)

jet :: Val → Val
jet jetVal = lookup jetVal dashboard & \case
    Just (nm, exp, exe) → VJet jetVal $ Ig (match nm exp exe)
    Nothing             → VJet jetVal $ Ig fallback
  where
    match :: Text → Exp → ([Val] → IO Val) -> Exp → [Val] → IO Val
    match nam expect exe actual env =
        (expect == actual) & \case
            True  → putStrLn nam       >> exe env
            False → putStrLn "NOMATCH" >> fallback actual env

    fallback :: Exp -> [Val] -> IO Val
    fallback exp env = do
        runEnv env exp >>= \case
            VFun b env → runEnv (jetVal : env) b
            _          → error "Fir expects a function"


-- Convert values to expressions for `uneval` ----------------------------------

valExp :: Val -> Exp
valExp = \case
    VNat n     → Lit n
    VJet x _   → Jet (valExp x)
    VFun b []  → Lam b
    VFun b env → shift (negate $ fromIntegral $ length env)
               $ foldl' (\exp (i,v) -> subst i (valExp v) exp) (Lam b)
               $ zip [0..] env
  where
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

    {-| @shift n x@ adds @n@ to the index of all free variables named @x@
        within an `Exp`
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
        unflat (n ^. atomBytes) & \case
            Left err -> Nothing
            Right ex -> pure ex

dump :: Exp -> Nat
dump = view (from atomBytes) . flat


-- Jetted Increment, Add, and Multiply -----------------------------------------

jet1 :: Exp
jet1 =  Lam $ Lam $ Fir (Lam (Var 0 `App` Var 1)) (Jet $ Var 1)

jet2 :: Exp
jet2 =  Lam $ Lam $ Lam $ Fir (Lam (Var 0 `App` Var 2 `App` Var 1))
                              (Jet $ Var 2)
--  inc x = +(x)
incRaw ∷ Exp
incRaw = Lam $ Inc (Var 0)

--  add inc x y = (%x (%y 0 inc) inc)
addRaw :: Exp
addRaw =
  Lam $ Lam $ Lam $ (Fun (Var 1))
                      `App` ((Fun (Var 0)) `App` Lit 0 `App` Var 2)
                      `App` (Var 2)

--  mul add x y = (%x 0 (add y))
mulRaw :: Exp
mulRaw = Lam $ Lam $ Lam $ (Fun (Var 1))
                             `App` Lit 0
                             `App` (Var 2 `App` Var 0)

addClose, mulClose :: Exp
addClose = addRaw `App` inc
mulClose = mulRaw `App` add

inc, add, mul :: Exp
inc = jet1 `App` incRaw
add = jet2 `App` addClose
mul = jet2 `App` mulClose
