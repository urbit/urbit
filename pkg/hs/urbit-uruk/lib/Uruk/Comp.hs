module Uruk.Comp where

import ClassyPrelude hiding (try)

import Data.List   (iterate, (!!))
import GHC.Natural (Natural)
import Uruk.Demo   (Ur((:@)), jet)

import qualified Uruk.Demo as Ur


-- Types -----------------------------------------------------------------------

type Nat = Natural

data Exp = Lam Exp | Var Nat | Go Exp Exp

data Deb = Zero | Succ Deb | Abs Deb | App Deb Deb
  deriving Show

infixl 5 :#

data Com = Com :# Com | S | I | C | K | B | Sn Nat | Bn Nat | Cn Nat

--------------------------------------------------------------------------------

try ∷ Exp → Ur
try = ur . traceShowId . snd . ski . expDeb

expDeb ∷ Exp → Deb
expDeb = go
  where
    go (Lam x)  = Abs (go x)
    go (Var x)  = peano x
    go (Go x y) = App (go x) (go y)
    peano 0 = Zero
    peano n = Succ (peano (pred n))

-- Oleg's Combinators ----------------------------------------------------------

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
        x:#y → "(" <> intercalate " " (show <$> foldApp x [y]) <> ")"
      where
        foldApp (x :# y) acc = foldApp x (y:acc)
        foldApp x        acc = x : acc

ski :: Deb -> (Nat, Com)
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

--  Until I unstash my jet registration code, the jetted output looks insane.
j _x _y z = z -- TODO HACK!

s = Ur.S
k = Ur.K
i = j 1 "i" (s:@k:@k)
b = j 3 "b" (s:@(k:@s):@k)
c = j 3 "c" (s:@(k:@(s:@(k:@(s:@s:@(k:@k))):@k)):@s)

{-
  Compile Oleg's combinators to jetted Ur combinators.
-}
ur :: Com -> Ur
ur = \case
    Bn 1        -> b
    Cn 1        -> c
    Sn 1        -> s
    Bn n | n<32 -> j (2+n) "b-lin" $ iterate ((b:@        b):@) b !! (int n-1)
    Cn n | n<16 -> j (2+n) "c-lin" $ iterate ((b:@(b:@c):@b):@) c !! (int n-1)
    Sn n | n<16 -> j (2+n) "s-lin" $ iterate ((b:@(b:@s):@b):@) s !! (int n-1)
    Bn n        -> j (2+n) "b-log" $ go n (k:@i)         :@ b              :@i
    Cn n        -> j (2+n) "c-log" $ go n (k:@(c:@i:@i)) :@ (b:@(b:@c):@b) :@i
    Sn n        -> j (2+n) "s-log" $ go n (k:@(c:@i:@i)) :@ (b:@(b:@s):@b) :@i
    x :# y      -> ur x :@ ur y
    B           -> b
    C           -> c
    S           -> s
    I           -> i
    K           -> k
  where
    int = fromIntegral
    go (int -> n) base = foldr (:@) base $ ([b0, b1]!!) <$> bits [] n
    bits acc 0 = reverse acc
    bits acc n | (q, r) <- divMod n 2 = bits (r:acc) q
    b0 = c:@b:@(s:@b:@i)
    b1 = c:@(b:@s:@(b:@(b:@b):@(c:@b:@(s:@b:@i)))):@b
