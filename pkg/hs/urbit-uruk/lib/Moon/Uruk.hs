module Moon.Uruk where

import ClassyPrelude
import GHC.Natural
import Moon.AST

import qualified Urbit.Atom   as Atom
import qualified Uruk.JetComp as Uruk
import qualified Uruk.JetDemo as Uruk

--------------------------------------------------------------------------------

toUruk ∷ Exp a → Uruk.Ur
toUruk = Uruk.moonStrict . toLC

toLC ∷ Exp a → Uruk.Exp
toLC = go
 where
  go = \case
    Var a     → error "TODO"
    Lam b     → error "TODO"
    App x y   → Uruk.Go (go x) (go y)
    Sig       → Uruk.Prim Uruk.Uni
    Con x y   → Uruk.cons `Uruk.Go` go x `Uruk.Go` go y
    Cas x l r → Uruk.Case (go x) (error "TODO") (error "TODO")
    Iff c t e → Uruk.If (go c) (go t) (go e)
    Lit n     → Uruk.Prim $ Uruk.Nat n
    Str n     → Uruk.Prim $ Uruk.Nat $ Atom.utf8Atom n
