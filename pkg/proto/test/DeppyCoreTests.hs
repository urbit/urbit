module DeppyCoreTests where

import ClassyPrelude

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH

import Deppy.Core

instance IsString (Exp String) where
  fromString = Var

-- basic subtyping tests

emp = const undefined

case_type_in_type = nest @String emp Typ Typ @?= pure ()

case_type_not_in_wut = nest @String emp Typ (wut [1]) @?= Nothing

case_wut_not_in_type = nest @String emp (wut [1]) Typ @?= Nothing

case_wut_in_wut = nest @String emp (wut [1]) (wut [1,2]) @?= pure ()

case_wut_not_in_wut = nest @String emp (wut [1,2]) (wut [1]) @?= Nothing

case_cel_co = nest @String emp (cel_ (wut [1]) (wut [3])) (cel_ (wut [1,2]) (wut [3,4])) @?= pure ()

case_fun_contra = nest @String emp (fun_ (wut [1,2]) (wut [3])) (fun_ (wut [1]) (wut [3,4])) @?= pure ()

-- cases and cores

-- case_case_sub = undefined

-- distribution

-- case_cas_cel_in_cel_cas = nest emp (cas (Var "x") Typ [(0, cel_ (wut [0]) (wut [1])), (1, cel_ (wut [2]), (wut [3]]

-- silly recursion

case_dangling_rec_in = nest emp (rec "_" Typ $ wut [1]) (wut [1,2]) @?= pure ()

case_in_dangling_rec = nest emp (wut [1]) (rec "_" Typ $ wut [1,2]) @?= pure ()

case_dangling_rec_not_in = nest emp (rec "_" Typ $ wut [1,2]) (wut [1]) @?= Nothing

case_not_in_dangling_rec = nest emp (wut [1,2]) (rec "_" Typ $ wut [1]) @?= Nothing

-- list test cases

pattern SIG = 0
pattern NIL = 917
pattern CONS = 3095

nilT t = cel_ (Tag NIL) (Tag SIG)

consT t self = cel_ (Tag CONS) $ cel_ t self

listCellT = undefined


tests :: TestTree
tests = $(testGroupGenerator)
