module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Unsafe.Coerce (unsafeCoerce)
import Prim.Row (class Union)
import Type.Equality

--------------------------------------------------------------------------------

undefined :: forall a. a
undefined = unsafeCoerce unit

--------------------------------------------------------------------------------

data Nil (r :: # Type) h t = Nil

type End = Nil () {} {}

type Rec a = Record a

data Ctx (r :: # Type) h t = Ctx h t

head :: forall r h t. Ctx r h t -> h
head (Ctx h _) = h

tail :: forall r h t. Ctx r h t -> t
tail (Ctx _ t) = t

--------------------------------------------------------------------------------

empty :: Ctx () {} {}
empty = Ctx {} {}

single :: forall h. Rec h -> Ctx h (Rec h) End
single h = Ctx h Nil

cons :: forall r h ctx cr ch ct
      . Union h cr r
     => Rec h
     -> ctx cr ch ct
     -> Ctx r (Rec h) (ctx cr ch ct)
cons h t = Ctx h t

ex1 :: Ctx (x :: Int) {x :: Int} End
ex1 = single {x:3}

ex2 :: Ctx (x::Int, y::Int) {y::Int} (Ctx (x::Int) {x::Int} End)
ex2 = Ctx {y:3} (Ctx {x:4} Nil)

exCons :: Ctx (x::Int, y::Int) {y::Int} (Ctx (x::Int) {x::Int} End)
exCons = cons {y:3} ex1

bindX :: forall r h t a
       . a -> Ctx r h t -> Ctx (x::a | r) {x::a} (Ctx r h t)
bindX x ctx = cons {x:x} ctx

dumbHeadGetX :: forall r hr t a
              . Ctx (x::a | r) {x::a | hr} t -> a
dumbHeadGetX (Ctx h _) = h.x

--------------------------------------------------------------------------------

class HasX (r :: # Type) (hr :: # Type) t a ctx
        | hr -> ctx, a -> ctx
    where
  getX :: ctx (x::a | r) (Record hr) t -> a

--------------------------------------------------------------------------------

class HeadHasX (r :: # Type) (hr :: # Type) t a ctx
        | hr -> ctx, a -> ctx
    where
  headGetX :: ctx (x::a | r) {x::a | hr} t -> a

--------------------------------------------------------------------------------

instance xInHead
      :: HeadHasX r hr t a ctx
      => HasX r hr t a ctx
    where
  getX = undefined -- headGetX

{-
instance headHasX
      :: TypeEquals ctx (Ctx r hr t)
      => HeadHasX r hr t a ctx
    where
  headGetX = undefined -- headGetX
-}

getX_ :: forall r hr t a ctx
      . HasX r hr t a ctx
     => ctx (x::a | r) (Record hr) t -> a
getX_ = getX

{-
instance equalFieldsCons
  ::
  ( IsSymbol name
  , Eq ty
  , Cons name ty tailRow row
  , EqualFields tail row
  ) => EqualFields (Cons name ty tail) row where
  equalFields _ a b = get' a == get' b && equalRest a b
    where
      get' = get (SProxy :: SProxy name)
      equalRest = equalFields (RLProxy :: RLProxy tail)
-}

{-
    TODO In order to get this to work, I need a type-level recursion,
    branching on `Lacks "x" r1` and `Has "x" r1`. That would look something
    like this:

    getX :: forall r1 r2 t a. Ctx {x::a, r} (Rec h) t -> a
    getX ctx@(Ctx h t) = tyif (Has "x" h)
                         then headGetX ctx
                         else getX t

    getX :: forall r1 h t a
          . Ctx (x::a | r1) (Rec h) t -> a
    getX = undefined
-}

main :: Effect Unit
main = do
  log "Hello world!"
