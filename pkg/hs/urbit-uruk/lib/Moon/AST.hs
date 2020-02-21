module Moon.AST where

import Bound
import ClassyPrelude
import GHC.Natural
import Data.Deriving (deriveEq1, deriveOrd1, deriveRead1, deriveShow1)


-- Types -----------------------------------------------------------------------

type Nat = Natural

data Exp a
    = Lam (Scope () Exp a)
    | Var a
    | App (Exp a) (Exp a)
    | Sig
    | Con (Exp a) (Exp a)
    | Cas (Exp a) (Scope () Exp a) (Scope () Exp a)
    | Iff (Exp a) (Exp a) (Exp a)
    | Lit Nat
    | Str Text
  deriving (Functor, Foldable, Traversable)

data AST
    = ALam Text AST
    | AVar Text
    | AApp AST AST
    | ALet Text AST AST
    | ASig
    | ACon AST AST
    | ACas AST (Text, AST) (Text, AST)
    | AIff AST AST AST
    | ALit Nat
    | AStr Text
  deriving (Eq, Ord)


-- Instances -------------------------------------------------------------------

deriveEq1   ''Exp
deriveOrd1  ''Exp
deriveRead1 ''Exp
deriveShow1 ''Exp

instance Applicative Exp where
  pure = Var
  (<*>) = ap

instance Monad Exp where
  return = Var
  Var a     >>= f = f a
  Lam b     >>= f = Lam (b >>>= f)
  App x y   >>= f = App (x >>= f) (y >>= f)
  Con x y   >>= f = Con (x >>= f) (y >>= f)
  Iff c t e >>= f = Iff (c >>= f) (t >>= f) (e >>= f)
  Cas x l r >>= f = Cas (x >>= f) (l >>>= f) (r >>>= f)
  Sig       >>= _ = Sig
  Lit n     >>= _ = Lit n
  Str s     >>= _ = Str s

instance IsString AST where
    fromString = AVar . pack

instance Num AST where
    fromInteger = ALit . fromIntegral
    (+)    = error "Fake Num instance for `Exp`"
    (-)    = error "Fake Num instance for `Exp`"
    (*)    = error "Fake Num instance for `Exp`"
    abs    = error "Fake Num instance for `Exp`"
    signum = error "Fake Num instance for `Exp`"
    negate = error "Fake Num instance for `Exp`"

instance Show AST where
  show = \case
    AVar t   -> unpack t
    AApp f x -> "(" <> show f <> " " <> show x <> ")"
    ASig     -> "~"
    ALit n   -> show n
    AStr n   -> "'" <> unpack n <> "'"
    ACon h t -> "[" <> show h <> " " <> show t <> "]"
    ALam v b -> mconcat ["|=(", unpack v <> " ", show b <> ")"]
    ALet t x b ->
      mconcat ["/=(", unpack t <> " ", show x <> " ", show b <> ")"]

    ACas c (ln, l) (rn, r) -> mconcat
      [ "?-("
      , show c <> "; "
      , "L" <> unpack ln <> " "
      , show l <> ", "
      , "R" <> unpack rn <> " "
      , show r <> ")"
      ]

    AIff c t e -> mconcat ["?:(", show c <> " ", show t <> " ", show e <> ")"]

--------------------------------------------------------------------------------

bind ∷ AST → Exp Text
bind = go
 where
  go = \case
    ALam v b → Lam (abstract1 v (go b))
    AVar v → Var v
    AApp x y → App (go x) (go y)
    ALet n x b → go (ALam n b `AApp` x)
    ASig → Sig
    ACon x y → Con (go x) (go y)
    ACas x (ln, l) (rn, r) → Cas (go x) (abstract1 ln (go l)) (abstract1 rn (go r))
    AIff c t e → Iff (go c) (go t) (go e)
    ALit n → Lit n
    AStr s → Str s
