module Moon.Uruk where

import Bound
import ClassyPrelude
import GHC.Natural
import Moon.AST

import qualified Moon.Parser  as Parser
import qualified Urbit.Atom   as Atom
import qualified Uruk.JetComp as Uruk
import qualified Uruk.JetDemo as Ur

--------------------------------------------------------------------------------

getGlobal :: Text -> Uruk.Exp
getGlobal = Uruk.Prim . \case
  "s"    -> Ur.S
  "k"    -> Ur.K
  "j"    -> Ur.J 1
  "d"    -> Ur.D
  "id"   -> Ur.I
  "dot"  -> Ur.B
  "flip" -> Ur.C
  "cas"  -> Ur.Cas
  "lef"  -> Ur.Lef
  "rit"  -> Ur.Rit
  "yea"  -> Ur.Bol True
  "nah"  -> Ur.Bol False
  "iff"  -> Ur.Iff
  "seq"  -> Ur.Seq
  "pak"  -> Ur.Pak
  "zer"  -> Ur.Zer
  "eql"  -> Ur.Eql
  "inc"  -> Ur.Inc
  "dec"  -> Ur.Dec
  "fec"  -> Ur.Fec
  "add"  -> Ur.Add
  "sub"  -> Ur.Sub
  "mul"  -> Ur.Mul
  "fix"  -> Ur.Fix
  "ded"  -> Ur.Ded
  "uni"  -> Ur.Uni
  "con"  -> Ur.Con
  "car"  -> Ur.Car
  "cdr"  -> Ur.Cdr
  str    -> error ("undefined variable: " <> unpack str)
  where p = Uruk.Prim

{-
    | Sn !Positive
    | Bn !Positive
    | Cn !Positive
    | Yet !Natural
-}

toUruk :: Exp Text -> Ur.Ur
toUruk = Uruk.moonStrict . toLC getGlobal

gogogo :: Text -> Ur.Ur
gogogo text = Ur.simp (Uruk.moonStrict lam)
 where
  Right ast = traceShowId (Parser.parseAST text)
  exp       = bind ast
  lam       = traceShowId (toLC getGlobal exp)

toLC :: (Text -> Uruk.Exp) -> Exp Text -> Uruk.Exp
toLC getGlobal = go (Left . getGlobal)
 where
  go :: (a -> Either Uruk.Exp Nat) -> Exp a -> Uruk.Exp
  go f = \case
    Var a -> case f a of
      Left  e -> e
      Right v -> Uruk.Var v

    Lam b     -> Uruk.Lam (go env' $ fromScope b) where env' = wrap f

    App x y   -> Uruk.Go (go f x) (go f y)
    Sig       -> Uruk.Prim Ur.Uni
    Con x y   -> Uruk.cons `Uruk.Go` go f x `Uruk.Go` go f y
    Cas x l r -> Uruk.Case (go f x)
                           (go env' $ fromScope l)
                           (go env' $ fromScope r)
      where env' = wrap f

    Iff c t e -> Uruk.If (go f c) (go f t) (go f e)
    Lit n     -> Uruk.Prim $ Ur.Nat n
    Str n     -> Uruk.Prim $ Ur.Nat $ Atom.utf8Atom n

  wrap f = \case
    B () -> Right 0
    F x  -> succ <$> f x
