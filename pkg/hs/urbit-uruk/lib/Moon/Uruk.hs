module Moon.Uruk where

import Bound
import ClassyPrelude
import GHC.Natural
import Moon.AST

import qualified Moon.Parser  as Parser
import qualified Urbit.Atom   as Atom
import qualified Uruk.JetComp as Uruk
import qualified Uruk.JetDemo as Uruk

--------------------------------------------------------------------------------

toUruk :: Exp a -> Uruk.Ur
toUruk = Uruk.moonStrict . toLC

gogogo :: Text -> Uruk.Ur
gogogo text = Uruk.moonStrict lam
 where
  Right ast = traceShowId (Parser.parseAST text)
  exp       = bind ast
  lam       = traceShowId (toLC exp)

toLC :: Exp a -> Uruk.Exp
toLC = go (error "free variable")
 where
  go :: (a -> Nat) -> Exp a -> Uruk.Exp
  go f = \case
    Var a -> Uruk.Var (f a)
    Lam b -> Uruk.Lam (go env' $ fromScope b)
     where
      env' = \case
        B () -> 0
        F x  -> succ (f x)

    App x y   -> Uruk.Go (go f x) (go f y)
    Sig       -> Uruk.Prim Uruk.Uni
    Con x y   -> Uruk.cons `Uruk.Go` go f x `Uruk.Go` go f y
    Cas x l r -> Uruk.Case (go f x)
                           (go env' $ fromScope l)
                           (go env' $ fromScope r)
     where
      env' = \case
        B () -> 0
        F x  -> succ (f x)

    Iff c t e -> Uruk.If (go f c) (go f t) (go f e)
    Lit n     -> Uruk.Prim $ Uruk.Nat n
    Str n     -> Uruk.Prim $ Uruk.Nat $ Atom.utf8Atom n
