module Moon.AST where

import ClassyPrelude
import GHC.Natural

type Nat = Natural

data Exp
    = Lam Text Exp
    | Var Text
    | App Exp Exp

    | Sig

    | Con Exp Exp
    | Car Exp
    | Cdr Exp

    | Lef Exp
    | Rit Exp
    | Cas Exp (Text, Exp) (Text, Exp)

    | Yea
    | Nah
    | Iff Exp Exp Exp

    | Lit Nat
  deriving (Eq, Ord)

instance IsString Exp where
    fromString = Var . pack

instance Num Exp where
    fromInteger = Lit . fromIntegral
    (+)    = error "Fake Num instance for `Exp`"
    (-)    = error "Fake Num instance for `Exp`"
    (*)    = error "Fake Num instance for `Exp`"
    abs    = error "Fake Num instance for `Exp`"
    signum = error "Fake Num instance for `Exp`"
    negate = error "Fake Num instance for `Exp`"

instance Show Exp where
    show = \case
        Lam v b → "|=(" <> show v <> " " <> show b <> ")"
        Var t   → unpack t
        App f x → "(" <> show f <> " " <> show x <> ")"

        Sig → "~"

        Con h t → "[" <> show h <> " " <> show t <> "]"
        Car x   → "-" <> show x
        Cdr x   → "+" <> show x

        Lef x → "L" <> show x
        Rit x → "R" <> show x

        Cas c (ln, l) (rn, r) → mconcat
            [ "?-("
            , show c <> "; "
            , "L" <> unpack ln <> " "
            , show l <> ", "
            , "R" <> unpack rn <> " "
            , show r <> ")"
            ]

        Lit n → show n

        Yea       → "%.y"
        Nah       → "%.n"
        Iff c t e → "?:(" <> show c <> " " <> show t <> " " <> show e <> ")"
