module Urbit.Urlicht.Errors where

import ClassyPrelude

data Error
  = EUnknown
  | EUnify
  | ESpine
  | EScope
  | EOccurs
  | EName
  | ENotFun

instance (Show Error) where
  show = \case
    EUnknown -> "Very sorry sir, but your program does not compile."
    EUnify -> "Ah, excuse me, but it seems some terms don't unify."
    ESpine -> "If you would please refrain from applying metas to non-vars."
    EScope -> "Please sir! Your extraneous vars make unification quite hard."
    EOccurs -> "Begging your pardon sir, but I never did understand recursion."
    EName -> "I'm afraid I don't recognize that variable, sir."
    ENotFun -> "A function type, if you please."
