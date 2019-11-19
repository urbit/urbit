module Ur.Moloch where

import Ur.Common
import GHC.Natural
import Data.Flat

--------------------------------------------------------------------------------

type Nat = Natural


--------------------------------------------------------------------------------

data Cns = Tar | Box
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
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass Flat

instance NFData Exp where
    rnf = \case Cns c     -> rnf c
                Var v     -> rnf v
                Lam _A b  -> rnf _A `seq` rnf b
                Pie _A _B -> rnf _A `seq` rnf _B
                App f a   -> rnf f `seq` rnf a


--------------------------------------------------------------------------------

{-| Substitute all occurrences of a variable with an expression

> subst n C B  ~  B[x@n := C]
-}
subst :: Integer -> Exp -> Exp -> Exp
subst n e' e = case e of
    Lam _A  b -> Lam (subst n e' _A) (subst (succ n) (shift 1 e') b)
    Pie _A _B -> Pie (subst n e' _A) (subst (succ n) (shift 1 e') _B)
    App f a   -> App (subst n e' f)  (subst n e' a)
    Var n'    -> if n == n' then e' else e
    Cns k     -> Cns k

{-| @shift n x@ adds @n@ to the index of all free variables named @x@ within an
    `Exp`
-}
shift :: Integer -> Exp -> Exp
shift d e0 = go e0 0
  where
    go e c = case e of
        Lam _A  b -> Lam (go _A c) (go  b $! succ c)
        Pie _A _B -> Pie (go _A c) (go _B $! succ c)
        App f a   -> App (go f c) (go a c)
        Var n     -> Var $! if n >= c then n + d else n
        Cns k     -> Cns k

-- | Returns whether a variable is free in an expression
freeIn :: Integer -> Exp -> Bool
freeIn n = go
  where
    go = \case
        Lam _A b  -> go _A || freeIn (succ n) b
        Pie _A _B -> go _A || freeIn (succ n) _B
        Var n'    -> n == n'
        App f a   -> go f || go a
        Cns _     -> False

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

{-| `typeOf` is the same as `typeWith` with an empty context, meaning that the
    expression must be closed (i.e. no free variables), otherwise type-checking
    will fail.
-}
typeOf :: Exp -> Either TypeError Exp


--------------------------------------------------------------------------------

-- | The specific type error
data TypeMessage
    = UnboundVariable
    | InvalidInputType Exp
    | InvalidOutputType Exp
    | NotAFunction
    | TypeMismatch Exp Exp
    | Untyped Cns
    deriving (Show)

instance NFData TypeMessage where
    rnf tm = case tm of
        UnboundVariable     -> ()
        InvalidInputType e  -> rnf e
        InvalidOutputType e -> rnf e
        NotAFunction        -> ()
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

typeOf = typeWith []
