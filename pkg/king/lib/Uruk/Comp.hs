module Uruk.Comp where

import ClassyPrelude hiding (union, (\\), lookup)

import Data.List (union, (\\), lookup, (!!), iterate)

import qualified Uruk.Demo as Ur


-- Types -----------------------------------------------------------------------

data Expr = Expr :@ Expr | Var String | Lam String Expr
  deriving Show


-- Entry Point -----------------------------------------------------------------

ur ∷ Expr → Ur.Ur
ur = cvt . babs []
  where
    cvt ∷ Expr → Ur.Ur
    cvt (Var "s") = Ur.S
    cvt (Var "k") = Ur.K
    cvt (Var "d") = Ur.D
    cvt (Var "j") = Ur.J
    cvt (Var x)   = error ("undefined variable: " <> x)
    cvt (Lam _ _) = error ("internal error: lambdas should be gone by now")
    cvt (x :@ y)  = cvt x Ur.:@ cvt y


-- Unoptimized Bracket Pattern -------------------------------------------------

fv vs (Var s) | s `elem` vs = []
fv vs (Var s) | otherwise   = [s]
fv vs (x :@ y)              = fv vs x `union` fv vs y
fv vs (Lam s f)             = fv (s:vs) f

babs0 :: [(String, Expr)] -> Expr -> Expr
babs0 env (Lam x e)
  | Var y <- t, x == y  = Var "s" :@ Var "k" :@ Var "k"
  | x `notElem` fv [] t = Var "k" :@ t
  | m :@ n <- t         = Var "s" :@ babs0 env (Lam x m) :@ babs0 env (Lam x n)
  | otherwise           = error "babs0-wut"
  where t = babs0 env e
babs0 env (Var s)
  | Just t <- lookup s env = babs0 env t
  | otherwise              = Var s
babs0 env (m :@ n) = babs0 env m :@ babs0 env n


-- Optimized Bracket Pattern ---------------------------------------------------

babs env (Lam x e)
  | Var "s" :@ Var"k" :@ _ <- t = Var "s" :@ Var "k"
  | x `notElem` fv [] t = Var "k" :@ t
  | Var y <- t, x == y  = Var "s" :@  Var "k" :@ Var "k"
  | m :@ Var y <- t, x == y, x `notElem` fv [] m = m
  | Var y :@ m :@ Var z <- t, x == y, x == z =
    babs env $ Lam x $ Var "s" :@ Var "s" :@ Var "k" :@ Var x :@ m
  | m :@ (n :@ l) <- t, isComb m, isComb n =
    babs env $ Lam x $ Var "s" :@ Lam x m :@ n :@ l
  | (m :@ n) :@ l <- t, isComb m, isComb l =
    babs env $ Lam x $ Var "s" :@ m :@ Lam x l :@ n
  | (m :@ l) :@ (n :@ l') <- t, l `noLamEq` l', isComb m, isComb n
    = babs env $ Lam x $ Var "s" :@ m :@ n :@ l
  | m :@ n <- t        = Var "s" :@ babs env (Lam x m) :@ babs env (Lam x n)
  | otherwise          = error "babs-wut"
  where t = babs env e
babs env (Var s)
  | Just t <- lookup s env = babs env t
  | otherwise              = Var s
babs env (m :@ n) = babs env m :@ babs env n

isComb e = null $ fv [] e \\ ["s", "k"]

noLamEq (Var x) (Var y) = x == y
noLamEq (a :@ b) (c :@ d) = a `noLamEq` c && b `noLamEq` d
noLamEq _ _ = False


-- Oleg's Combinators ----------------------------------------------------------

data Deb = Zero | Succ Deb | Abs Deb | App Deb Deb deriving Show

infixl 5 :#

data Com = Com :# Com | S | I | C | K | B | Sn Int | Bn Int | Cn Int

sz ∷ Com → Int
sz (x :# y) = sz x + sz y
sz _        = 1

instance Show Com where
    show = \case
        S    → "s"
        I    → "i"
        C    → "c"
        K    → "k"
        B    → "b"
        Sn i → "s" <> show i
        Bn i → "b" <> show i
        Cn i → "c" <> show i
        x:#y → "[" <> intercalate " " (show <$> foldApp x [y]) <> "]"
      where
        foldApp (x :# y) acc = foldApp x (y:acc)
        foldApp x        acc = x : acc

ski :: Deb -> (Int, Com)
ski deb = case deb of
  Zero                           -> (1,       I)
  Succ d    | x@(n, _) <- ski d  -> (n + 1,   f (0, K) x)
  App d1 d2 | x@(a, _) <- ski d1
            , y@(b, _) <- ski d2 -> (max a b, f x y)
  Abs d | (n, e) <- ski d -> case n of
                               0 -> (0,       K :# e)
                               _ -> (n - 1,   e)
  where
  f (a, x) (b, y) = case (a, b) of
    (0, 0)             ->         x :# y
    (0, n)             -> Bn n :# x :# y
    (n, 0)             -> Cn n :# x :# y
    (n, m) | n == m    -> Sn n :# x :# y
           | n < m     ->                Bn (m - n) :# (Sn n :# x) :# y
           | otherwise -> Cn (n - m) :# (Bn (n - m) :#  Sn m :# x) :# y

linBulk :: Com -> Com
linBulk b = case b of
  Bn n   -> iterate ((B:#        B):#) B !! (n - 1)
  Cn n   -> iterate ((B:#(B:#C):#B):#) C !! (n - 1)
  Sn n   -> iterate ((B:#(B:#S):#B):#) S !! (n - 1)
  x :# y -> linBulk x :# linBulk y
  _      -> b

logBulk :: Com -> Com
logBulk b = case b of
  -- C' = \cfgx.c(fx) g   = B(BC)B
  -- S' = \cfgx.c fx (gx) = B(BS)B
  Bn n   -> go n (K:#I)         :# B              :# I
  Cn n   -> go n (K:#(C:#I:#I)) :# (B:#(B:#C):#B) :# I
  Sn n   -> go n (K:#(C:#I:#I)) :# (B:#(B:#S):#B) :# I
  x :# y -> logBulk x :# logBulk y
  _      -> b
  where
    go n base = foldr (:#) base $ ([b0, b1]!!) <$> bits [] n
    bits acc 0 = reverse acc
    bits acc n | (q, r) <- divMod n 2 = bits (r:acc) q
    b0 = C:#B:#(S:#B:#I)
    b1 = C:#(B:#S:#(B:#(B:#B):#(C:#B:#(S:#B:#I)))):#B

bulk :: Com -> Com
bulk b = case b of
  Bn n | n>=30  -> logBulk (Bn n)
  Bn n          -> linBulk (Bn n)
  Cn n | n >=15 -> logBulk (Cn n)
  Cn n          -> linBulk (Cn n)
  Sn n | n >=15 -> logBulk (Sn n)
  Sn n          -> linBulk (Sn n)
  x :# y        -> bulk x :# bulk y
  _             -> b
