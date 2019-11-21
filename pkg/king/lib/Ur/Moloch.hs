module Ur.Moloch where

import Ur.Common hiding (flat)
import GHC.Natural

import Control.Lens (view, from, (^.))
import Data.Flat    (Flat, flat, unflat)
import Noun.Atom    (atomBytes)

--------------------------------------------------------------------------------

type Nat = Natural


--------------------------------------------------------------------------------

data Cns
    = Tar
    | Box
  deriving (Eq, Ord, Show, Bounded, Enum, Generic)
  deriving anyclass Flat

instance NFData Cns where
    rnf c = seq c ()


--------------------------------------------------------------------------------

-- | Syntax tree for expressions
data Exp
    = Cns Cns
    | Var Integer
    | Lam Exp Exp
    | Pie Exp Exp
    | App Exp Exp

    | Unit
    | UnitLit

    | Pair Exp Exp
    | PairLit Exp Exp
    | PairCar Exp
    | PairCdr Exp

    | Sum Exp Exp
    | SumLef Exp
    | SumRit Exp
    | SumCase Exp Exp Exp

    | Nat
    | NatLit Nat
    | NatInc Exp
    | NatDec Exp

    | Jet Exp Exp
    | JetLit Exp
    | JetApp Exp Exp

    | Eval Exp Exp
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass Flat

instance NFData Exp where
    rnf = \case Cns c         -> rnf c
                Var v         -> rnf v
                Lam _A b      -> rnf _A `seq` rnf b
                Pie _A _B     -> rnf _A `seq` rnf _B
                App f a       -> rnf f `seq` rnf a
                Unit          -> ()
                UnitLit       -> ()
                Pair x y      -> rnf x `seq` rnf y
                PairLit x y   -> rnf x `seq` rnf y
                PairCar x     -> rnf x
                PairCdr x     -> rnf x
                Sum x y       -> rnf x `seq` rnf y
                SumLef x      -> rnf x
                SumRit x      -> rnf x
                SumCase l r x -> rnf l `seq` rnf r `seq` rnf x
                Nat           -> ()
                NatLit n      -> rnf n
                NatInc x      -> rnf x
                NatDec x      -> rnf x
                Eval t x      -> rnf t `seq` rnf x
                Jet x y       -> rnf x `seq` rnf y
                JetLit x      -> rnf x
                JetApp x y    -> rnf x `seq` rnf y


--------------------------------------------------------------------------------

{-| Substitute all occurrences of a variable with an expression

> subst n C B  ~  B[x@n := C]
-}
subst :: Integer -> Exp -> Exp -> Exp
subst n e' e = case e of
    Lam _A  b     -> Lam (subst n e' _A) (subst (succ n) (shift 1 e') b)
    Pie _A _B     -> Pie (subst n e' _A) (subst (succ n) (shift 1 e') _B)
    App f a       -> App (subst n e' f)  (subst n e' a)
    Var n'        -> if n == n' then e' else e
    Cns k         -> Cns k
    Unit          -> Unit
    UnitLit       -> UnitLit
    Pair x y      -> Pair (subst n e' x) (subst n e' y)
    PairLit x y   -> PairLit (subst n e' x) (subst n e' y)
    PairCar x     -> PairCar (subst n e' x)
    PairCdr x     -> PairCdr (subst n e' x)
    Sum x y       -> Sum (subst n e' x) (subst n e' y)
    SumLef x      -> SumLef (subst n e' x)
    SumRit x      -> SumRit (subst n e' x)
    SumCase l r x -> SumCase (subst n e' l) (subst n e' r) (subst n e' x)
    Nat           -> Nat
    NatLit n      -> NatLit n
    NatInc x      -> NatInc (subst n e' x)
    NatDec x      -> NatDec (subst n e' x)
    Eval t x      -> Eval (subst n e' t) (subst n e' x)
    Jet t f       -> Jet (subst n e' t) (subst n e' f)
    JetLit x      -> JetLit (subst n e' x)
    JetApp j a    -> JetApp (subst n e' j) (subst n e' a)

{-| @shift n x@ adds @n@ to the index of all free variables named @x@ within an
    `Exp`
-}
shift :: Integer -> Exp -> Exp
shift d e0 = go e0 0
  where
    go e c = case e of
        Lam _A  b     -> Lam (go _A c) (go  b $! succ c)
        Pie _A _B     -> Pie (go _A c) (go _B $! succ c)
        App f a       -> App (go f c) (go a c)
        Var n         -> Var $! if n >= c then n + d else n
        Cns k         -> Cns k
        Unit          -> Unit
        UnitLit       -> UnitLit
        Pair x y      -> Pair (go x c) (go y c)
        PairLit x y   -> PairLit (go x c) (go y c)
        PairCar x     -> PairCar (go x c)
        PairCdr x     -> PairCdr (go x c)
        Sum x y       -> Sum (go x c) (go y c)
        SumLef x      -> SumLef (go x c)
        SumRit x      -> SumRit (go x c)
        SumCase l r x -> SumCase (go l c) (go r c) (go x c)
        Nat           -> Nat
        NatLit n      -> NatLit n
        NatInc x      -> NatInc (go x c)
        NatDec x      -> NatDec (go x c)
        Eval t x      -> Eval (go t c) (go x c)
        Jet t f       -> Jet (go t c) (go f c)
        JetLit f      -> JetLit (go f c)
        JetApp j a    -> JetApp (go j c) (go a c)

-- | Returns whether a variable is free in an expression
freeIn :: Integer -> Exp -> Bool
freeIn n = go
  where
    go = \case
        Lam _A b      -> go _A || freeIn (succ n) b
        Pie _A _B     -> go _A || freeIn (succ n) _B
        Var n'        -> n == n'
        App f a       -> go f || go a
        Cns _         -> False
        Unit          -> False
        UnitLit       -> False
        Pair x y      -> go x || go y
        PairLit x y   -> go x || go y
        PairCar x     -> go x
        PairCdr x     -> go x
        Sum x y       -> go x || go y
        SumLef x      -> go x
        SumRit x      -> go x
        SumCase l r x -> go l || go r || go x
        Nat           -> False
        NatLit _      -> False
        NatInc x      -> go x
        NatDec x      -> go x
        Eval t x      -> go t || go x
        Jet t f       -> go t || go f
        JetLit f      -> go f
        JetApp j a    -> go j || go a

-- | Reduce an expression to weak-head normal form
whnf :: Exp -> Exp
whnf = \case App f a -> app a (whnf f)
             e       -> e
  where
    app a = \case
        Lam _A b -> whnf (shift (-1) b')
          where
            a' = shift 1 a
            b' = subst 0 a' b
        f'         -> App f' a


{-| Reduce an expression to its normal form, performing both beta reduction and
    eta reduction

    `normalize` does not type-check the expression.  You may want to type-check
    expressions before normalizing them since normalization can convert an
    ill-typed expression into a well-typed expression.
-}
normalize :: Exp -> Exp
normalize e = case e of
    Lam _A b -> case b' of App f a -> app f a
                           _       -> e'
      where
        b'    = normalize b
        e'    = Lam (normalize _A) b'
        app f = whnf >>> \case
                    Var v' | 0 == v' && not (0 `freeIn` f) -> shift (-1) f
                    Var _                                  -> e'
                    _                                      -> e'
    Pie _A _B -> Pie (normalize _A) (normalize _B)
    App f a -> normalize f & \case
        Lam _A b -> normalize (shift (-1) b')
                      where
                        a' = shift 1 (normalize a)
                        b' = subst 0 a' b
        f'         -> App f' (normalize a)
    Var   _    -> e
    Cns _      -> e
    Unit       -> Unit
    UnitLit    -> UnitLit

    Pair x y    -> Pair (normalize x) (normalize y)
    PairLit x y -> PairLit (normalize x) (normalize y)
    PairCar x   -> normalize x & \case PairLit x _ -> x
                                       e'          -> PairCar e'
    PairCdr x   -> normalize x & \case PairLit _ y -> y
                                       e'          -> PairCdr e'

    Sum x y       -> Sum (normalize x) (normalize y)
    SumLef x      -> SumLef (normalize x)
    SumRit x      -> SumRit (normalize x)
    SumCase l r x -> (normalize l, normalize r, normalize x) & \case
        (lf@(Lam _ _), _,            SumLef lv) -> normalize (App lf lv)
        (_,            rf@(Lam _ _), SumRit rv) -> normalize (App rf rv)
        (lv,           rv,           xv       ) -> SumCase lv rv xv

    Nat           -> e
    NatLit _      -> e
    NatInc x      -> normalize x & \case NatLit n -> NatLit (succ n)
                                         xn       -> NatInc xn

    NatDec x      -> normalize x & \case NatLit 0 -> SumLef UnitLit
                                         NatLit n -> SumRit (NatLit (pred n))
                                         xn       -> NatDec xn

    Eval t x -> (normalize t, normalize x) & \case
        (tv, NatLit n) -> normalize (eval tv n)
        (tv, xv      ) -> Eval tv xv

    Jet t f    -> Jet (normalize t) (normalize f)
    JetLit f   -> JetLit (normalize f)
    JetApp j a -> normalize j & \case
        JetLit f -> normalize (App f a)
        jv       -> JetApp jv (normalize a)

encodeExp :: Exp -> Nat
encodeExp = view (from atomBytes) . flat

decodeExp :: Nat -> Maybe Exp
decodeExp n =
    unflat (n ^. atomBytes) & \case
        Left err -> Nothing
        Right ex -> pure ex

eval :: Exp -> Nat -> Exp
eval ty expNat =
    maybe (SumLef UnitLit) SumRit $ do
        exp   <- decodeExp expNat
        expTy <- either (const Nothing) Just (typeOf exp)
        guard (expTy == ty)
        pure exp


--------------------------------------------------------------------------------

-- | The specific type error
data TypeMessage
    = UnboundVariable
    | InvalidInputType Exp
    | InvalidOutputType Exp
    | NotAFunction
    | NotAJet
    | NotAPair
    | NotASum
    | NotANat
    | TypeMismatch Exp Exp
    | Untyped Cns
    deriving (Show)

instance NFData TypeMessage where
    rnf tm = case tm of
        UnboundVariable     -> ()
        InvalidInputType e  -> rnf e
        InvalidOutputType e -> rnf e
        NotAFunction        -> ()
        NotAJet             -> ()
        NotAPair            -> ()
        NotASum             -> ()
        NotANat             -> ()
        TypeMismatch e1 e2  -> rnf e1 `seq` rnf e2
        Untyped c           -> rnf c

-- | A structured type error that includes context
data TypeError = TypeError
    { context     :: [Exp]
    , current     :: Exp
    , typeMessage :: TypeMessage
    } deriving (Typeable, Show)

instance Exception TypeError

instance NFData TypeError where
    rnf (TypeError ctx crr tym) = rnf ctx `seq` rnf crr `seq` rnf tym

axiom :: Cns -> Either TypeError Cns
axiom Tar = pure Box
axiom Box = Left (TypeError [] (Cns Box) (Untyped Box))

rule :: Cns -> Cns -> Either TypeError Cns
rule Tar Box = pure Box
rule Tar Tar = pure Tar
rule Box Box = pure Box
rule Box Tar = pure Tar

infixr 5 !?;

(!?) :: [a] -> Nat -> Maybe a
(!?) [] _     = Nothing
(!?) (x:xs) 0 = pure x
(!?) (x:xs) n = xs !? pred n

{-| Type-check an expression and return the expression's type if type-checking
    suceeds or an error if type-checking fails

    `typeWith` does not necessarily normalize the type since full normalization
    is not necessary for just type-checking.  If you actually care about the
    returned type then you may want to `normalize` it afterwards.
-}
typeWith :: [Exp] -> Exp -> Either TypeError Exp
typeWith ctx e = case e of
    Cns c     -> fmap Cns (axiom c)
    Var n -> case ctx !? fromIntegral n of
        Nothing -> Left (TypeError ctx e UnboundVariable)
        Just a  -> pure a
    Lam _A b  -> do
        _ <- typeWith ctx _A
        let ctx' = fmap (shift 1) (_A : ctx)
        _B <- typeWith ctx' b
        let p = Pie _A _B
        _t <- typeWith ctx p
        return p
    Pie _A _B -> do
        eS <- fmap whnf (typeWith ctx _A)
        s  <- case eS of
            Cns s -> return s
            _     -> Left (TypeError ctx e (InvalidInputType _A))
        let ctx' = shift 1 <$> (_A : ctx)
        eT <- fmap whnf (typeWith ctx' _B)
        t  <- case eT of
            Cns t -> return t
            _     -> Left (TypeError ctx' e (InvalidOutputType _B))
        Cns <$> rule s t
    App f a     -> do
        e' <- fmap whnf (typeWith ctx f)
        (_A, _B) <- case e' of Pie _A _B -> return (_A, _B)
                               _         -> Left (TypeError ctx e NotAFunction)
        _A' <- typeWith ctx a
        if _A == _A'
            then do
                let a'  = shift 1 a
                    _B' = subst 0 a' _B
                return (shift (-1) _B')
            else do
                let nf_A  = normalize _A
                    nf_A' = normalize _A'
                Left (TypeError ctx e (TypeMismatch nf_A nf_A'))

    Unit    -> pure (Cns Tar)
    UnitLit -> pure Unit

    Pair x y -> do
        _ <- typeWith ctx x
        _ <- typeWith ctx y
        pure (Cns Tar)

    PairLit x y -> Pair <$> typeWith ctx x <*> typeWith ctx y

    PairCar x -> typeWith ctx x >>= \case
        Pair x _ -> pure x
        _        -> Left (TypeError ctx e NotAPair)

    PairCdr x -> typeWith ctx x >>= \case
        Pair _ y -> pure y
        _        -> Left (TypeError ctx e NotAPair)

    Sum x y -> do
        _ <- typeWith ctx x
        _ <- typeWith ctx y
        pure (Cns Tar)

    SumLef x -> do
        xt <- typeWith ctx x
        pure (Sum xt (Cns Tar))

    SumRit x -> do
        xt <- typeWith ctx x
        pure (Sum (Cns Tar) xt)

    SumCase l r x -> do
        (lt, rt) <- typeWith ctx x >>= \case
                        Sum x y -> pure (x, y)
                        _       -> Left (TypeError ctx e NotASum)

        resLef <- typeWith ctx l >>= \case
                      Pie a res | lt == a -> pure res
                      Pie a res           -> Left (TypeError ctx e (TypeMismatch a l))
                      _                   -> Left (TypeError ctx e NotAFunction)

        resRit <- typeWith ctx r >>= \case
                      Pie a res | rt == a -> pure res
                      Pie a res           -> Left (TypeError ctx e (TypeMismatch a r))
                      _                   -> Left (TypeError ctx e NotAFunction)

        if resLef == resRit
            then pure resLef
            else Left (TypeError ctx e $ TypeMismatch resLef resRit)

    Nat -> pure (Cns Tar)

    NatLit _ -> pure Nat

    NatInc x -> typeWith ctx x >>= \case
        Nat -> pure Nat
        _   -> Left (TypeError ctx e NotANat)

    NatDec x -> typeWith ctx x >>= \case
        Nat -> pure (Sum Unit Nat)
        _   -> Left (TypeError ctx e NotANat)

    Eval t x -> do
        typeWith ctx t >>= \case
            Cns Tar -> pure ()
            _       -> Left (TypeError ctx e (InvalidOutputType t))

        typeWith ctx x >>= \case
            Nat -> pure ()
            _   -> Left (TypeError ctx e NotANat)

        pure (Sum Unit t)

    Jet x y -> typeWith ctx (Pie x y)

    JetLit x -> typeWith ctx x >>= \case
        Pie x y -> pure (Jet x y)
        _       -> Left (TypeError ctx e NotAFunction)

    JetApp j a -> do
        at <- typeWith ctx a
        typeWith ctx j >>= \case -- TODO XX Is this correct?
            Jet arg res | arg == at -> pure res
            Jet arg res             -> Left (TypeError ctx e (TypeMismatch arg at))
            _                       -> Left (TypeError ctx e NotAJet)

{-| `typeOf` is the same as `typeWith` with an empty context, meaning that the
    expression must be closed (i.e. no free variables), otherwise type-checking
    will fail.
-}
typeOf :: Exp -> Either TypeError Exp
typeOf = typeWith []

try :: Exp -> IO ()
try exp = do
    typeOf exp & \case
        Left  err -> throwIO err
        Right ty  -> print ty

    putStrLn ""

    print (normalize exp)
