module Untyped.ShittyCorePrinter where

-- it's pretty clowny but whatever
-- TODO: handle the new cases (maybe don't do)

import Prelude

import Bound
import Data.Foldable

import Untyped.Core

prettyPrec :: [String] -> Bool -> Int -> Exp String -> ShowS
prettyPrec _      d n (Var a)    = showString a
prettyPrec vs     d n (App x y)  = showParen d $ 
  prettyPrec vs False n x . showChar ' ' . prettyPrec vs True n y
prettyPrec (v:vs) d n (Lam b)    = showParen d $ 
  showString v . showString ". " . prettyPrec vs False n (instantiate1 (Var v) b)

prettyWith :: [String] -> Exp String -> String
prettyWith vs t = prettyPrec (filter (`notElem` toList t) vs) False 0 t ""

pretty :: Exp String -> String
pretty = prettyWith $ [ [i] | i <- ['a'..'z']] ++ [i : show j | j <- [1 :: Int ..], i <- ['a'..'z'] ]
