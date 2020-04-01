module Urbit.Moon.AST where

import Prelude ()
import Bound
import ClassyPrelude
import GHC.Natural
import Data.Deriving (deriveEq1, deriveOrd1, deriveRead1, deriveShow1)
import Data.Functor.Classes (showsPrec1)


-- Types -----------------------------------------------------------------------

type Nat = Natural

data Exp a
    = Lam (Scope () Exp a)
    | Var a
    | App (Exp a) (Exp a)
    | Jet Nat Text (Exp a)
    | Sig
    | Con (Exp a) (Exp a)
    | Cas (Exp a) (Scope () Exp a) (Scope () Exp a)
    | Iff (Exp a) (Exp a) (Exp a)
    | Lit Nat
    | Bol Bool
    | Str Text
    | Fix (Scope () Exp a)
  deriving (Functor, Foldable, Traversable)

data AST
    = ALam Text AST
    | AVar Text
    | AApp AST AST
    | AJet Nat Text AST
    | ALet Text AST AST
    | ACon AST AST
    | ACas AST (Text, AST) (Text, AST)
    | AIff AST AST AST
    | ASig
    | ABol Bool
    | ALit Nat
    | AStr Text
    | AFix Text AST
  deriving (Eq, Ord)


-- Instances -------------------------------------------------------------------

deriveEq1   ''Exp
deriveOrd1  ''Exp
deriveRead1 ''Exp
deriveShow1 ''Exp

instance Show a => Show (Exp a) where showsPrec = showsPrec1

instance Applicative Exp where
  pure = Var
  (<*>) = ap

instance Monad Exp where
  return = Var
  Var a     >>= f = f a
  Lam b     >>= f = Lam (b >>>= f)
  App x y   >>= f = App (x >>= f) (y >>= f)
  Jet n t b >>= f = Jet n t (b >>= f)
  Fix b     >>= f = Fix (b >>>= f)
  Con x y   >>= f = Con (x >>= f) (y >>= f)
  Iff c t e >>= f = Iff (c >>= f) (t >>= f) (e >>= f)
  Cas x l r >>= f = Cas (x >>= f) (l >>>= f) (r >>>= f)
  Sig       >>= _ = Sig
  Lit n     >>= _ = Lit n
  Bol b     >>= _ = Bol b
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
    AVar t     -> unpack t
    AApp f x   -> "(" <> show f <> " " <> show x <> ")"
    ASig       -> "~"
    ALit n     -> show n
    ABol True  -> "%.y"
    ABol False -> "%.n"
    AStr n     -> "'" <> unpack n <> "'"
    ACon h t   -> "[" <> show h <> " " <> show t <> "]"
    ALam v b   -> mconcat ["|=(", unpack v <> " ", show b <> ")"]
    AJet r n b -> "~/(" <> show r <> " '" <> unpack n <> "' " <> show b <> ")"
    AFix n b   -> "..(" <> unpack n <> " " <> show b <> ")"
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

bind :: AST -> Exp Text
bind = go
 where
  go = \case
    ALam v b               -> Lam (abstract1 v (go b))
    AVar v                 -> Var v
    AApp x y               -> App (go x) (go y)
    ALet n x b             -> go (ALam n b `AApp` x)
    ASig                   -> Sig
    ACon x y               -> Con (go x) (go y)
    AJet r n b             -> Jet r n (go b)
    AFix n b               -> Fix (abstract1 n (go b))
    ACas x (ln, l) (rn, r) -> cas x ln l rn r
    AIff c t       e       -> Iff (go c) (go t) (go e)
    ALit n                 -> Lit n
    ABol b                 -> Bol b
    AStr s                 -> Str s

  cas x ln l rn r = Cas (go x) (abstract1 ln (go l)) (abstract1 rn (go r))
