{-
    Evaluation Rules (over-complicated for now)

    *J      → J
    *(Ja)   → J (*x)
    *(Jan)  → J (*x) (*y)

    *(J(N0)nx)   → *
    *(J(N1)nf)   → (J(N1)(*n)(*f))
    *(J(N1)nfx)  → *(fx)
    *(J(N2)nf)   → (J(N2)(*n)(*f))
    *(J(N2)nfx)  → (J(N2)(*n)(*f)(*x))
    *(J(N2)nfxy) → *(fxy)
    *(Jxyz)      → *(J(*x)(*y)(*z))

    *K     → K
    *(Kx)  → K (*x)
    *(Kxy) → *x

    *S      → S
    *(Sx)   → S (*x)
    *(Sxy)  → S (*x) (*y)
    *(Sxyz) → (*x) (*z) *(y z)

    *D    → D
    *(Dx) → DUMP (*x)

    F   → F
    Fx  → F(*x)
    Fxy → *(x(Fx)y)

    N n       → N n
    Inc       → Inc
    Inc (N n) → N (n+1)
    Inc x     → Inc (*x)
    Fol       → Fol
    Fol (N 0) → SK
    Fol (N 1) → SKK
    Fol (N n) → (S(S(KS)K)) (Fol(N(n-1)))

    *(fx) → *((*f)(*x))
-}

module Ur.Simplify where

import Ur.Common hiding (A, flat, succ, L, R, C)
import Ur.Lang

import Control.Concurrent (threadDelay)
import Control.Lens       (from, view)
import Data.Flat          (flat)
import GHC.Natural        (Natural)
import Urbit.Atom         (atomBytes, bytesAtom)
import System.IO.Unsafe   (unsafePerformIO)


-- Interpreter -----------------------------------------------------------------

simp ∷ E → E
simp = \case
    D      → D
    D :@ x → N $ bytesAtom $ flat $ go x

    S                → S
    S :@ x           → S :@ x
    S :@ x :@ y      → S :@ x :@ y
    S :@ x :@ y :@ z → go (x :@ z :@ (y :@ z))

    K           → K
    K :@ x      → K :@ x
    K :@ x :@ y → go x

    J n t                     → J n t
    J 0 _ :@ b                → go b
    J 1 _ :@ b :@ x           → go (b :@ x)
    J 2 _ :@ b :@ x :@ y      → go (b :@ x :@ y)
    J 3 _ :@ b :@ x :@ y :@ z → go (b :@ x :@ y :@ z)
    J n t :@ b                → J n t :@ b
    J n t :@ b :@ x           → J n t :@ b :@ x
    J n t :@ b :@ x :@ y      → J n t :@ b :@ x :@ y
    J n t :@ b :@ x :@ y :@ z → J n t :@ b :@ x :@ y :@ z

    L                → L
    L :@ x           → L :@ x
    R                → R
    R :@ x           → R :@ x
    C                → C
    C :@ l           → C :@ l
    C :@ l :@ r      → C :@ l :@ r
    C :@ l :@ r :@ x → go x & \case L :@ lv → go(l :@ lv)
                                    R :@ rv → go(r :@ rv)
                                    xv      → error "bad-case"

    -- Can I implement the fixedpoint operator using a jet?
    F           → F
    F :@ x      → F :@ x
    F :@ x :@ y → go (x :@ (F :@ x) :@ y)

    -- Can I implement naturals using jets?
    N n      → N n
    Inc      → Inc
    Inc :@ x → go x & \case { N n → N (n+1); xv → Inc :@ xv }
    Fol      → Fol
    Fol :@ x → go x & \case { N n → fromNat n; xv → Fol :@ xv }

    f :@ x   → go (go f :@ x)
  where
    go exp = unsafePerformIO $ do
        unless (simple exp) $ do
            putStrLn ("\t\t\t" <> tshow exp)
        res <- evaluate (force $ simp exp)
        unless (simple exp) $ do
            print exp
            putStrLn ("\t" <> tshow res)
            threadDelay 33333
        pure (simp exp)

    trivial = \case
        _ :@ _ → False
        _      → True

    simple = \case
        x :@ y → trivial x && trivial y
        e      → trivial e

fromNat ∷ Natural → E
fromNat 0 = zer
fromNat 1 = one
fromNat n = inc :@ fromNat (pred n)

toNat ∷ E → Natural
toNat exp =
    case simp (simp exp :@ Inc :@ N 0) of
        N n → n
        e   → error ("bad-nat: " <> show e)

inc, zer, one ∷ E
inc = J 1 "inc" :@ (S :@ (S :@ (K :@ S) :@ K))
zer = S :@ K
one = S :@ K :@ K

normalize = unsafePerformIO . stepper

stepList :: [E] -> Maybe [E]
stepList []     = Nothing
stepList [x]    = sequence $ singleton $ step x
stepList (x:xs) = step x & \case Nothing -> (x:) <$> stepList xs
                                 Just xv -> Just (xv:xs)

step ∷ E → Maybe E
step = \case
    D      → Nothing
    D :@ x → pure $ (step x, x) & \case
               (Just xv, _ ) → D :@ xv
               (Nothing, xv) → N $ bytesAtom $ flat $ normalize xv

    S                → Nothing
    S :@ x           → (S :@) <$> step x
    S :@ x :@ y      → case (step x, step y) of
                         (Just xv, _) -> pure (S :@ xv :@ y)
                         (_, Just yv) -> pure (S :@ x :@ yv)
                         _            -> Nothing

    S :@ x :@ y :@ z → pure $ x :@ z :@ (y :@ z)

    K           → Nothing
    K :@ x      → (K :@) <$> step x
    K :@ x :@ y → pure x

    J n t                     → Nothing
    J 0 _ :@ b                → pure b
    J 1 _ :@ b :@ x           → pure (b :@ x)
    J 2 _ :@ b :@ x :@ y      → pure (b :@ x :@ y)
    J 3 _ :@ b :@ x :@ y :@ z → pure (b :@ x :@ y :@ z)
    J n t :@ b                → (J n t :@) <$> step b
    J n t :@ b :@ x           → case (step b, step x) of
                                  (Just bv, _) -> pure (J n t :@ bv :@ x)
                                  (_, Just xv) -> pure (J n t :@ b :@ xv)
                                  (_, _      ) -> Nothing
    J n t :@ b :@ x :@ y      → undefined
    J n t :@ b :@ x :@ y :@ z → undefined



    L                → Nothing
    L :@ x           → (L :@) <$> step x
    R                → Nothing
    R :@ x           → (R :@) <$> step x
    C                → Nothing
    C :@ l           → Nothing
    C :@ l :@ r      → Nothing
    C :@ l :@ r :@ x → case (x, step x) of
                         (L :@ lv, _      ) → pure (l :@ lv)
                         (R :@ rv, _      ) → pure (r :@ rv)
                         (_,       Nothing) → Nothing
                         (_,       Just xv) → pure (C :@ l :@ r :@ xv)

    -- Can I implement the fixedpoint operator using a jet?
    F           → Nothing
    F :@ x      → (F :@) <$> step x
    F :@ x :@ y → pure (x :@ (F :@ x) :@ y)

    -- Can I implement naturals using jets?
    N n      → Nothing
    Inc      → Nothing
    Inc :@ x → case x of { N n → pure (N (n+1)); xv → (Inc :@) <$> step x }
    Fol      → Nothing
    Fol :@ x → case x of { N n → pure (fromNat n); xv → (Fol :@) <$> step x }

    f :@ x   → case (step f, step x) of
                 (Just fv, _      ) -> pure (fv :@ x)
                 (Nothing, Just xv) -> pure (f :@ xv)
                 (Nothing, Nothing) -> Nothing

stepper :: E → IO E
stepper e = do
    print e
    putStrLn ""
    case step e of
        Nothing -> pure e
        Just e' -> stepper e'
