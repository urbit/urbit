module DeppyCoreTests where

import ClassyPrelude

import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH

import Deppy.Core

instance IsString s => IsString (Typ s) where
  fromString = Var . fromString

-- basic subtyping tests

emp = const undefined
env bs v = fromJust $ Map.lookup v $ mapFromList bs

case_type_in_type = nest @String emp Typ Typ @?= pure ()

case_type_not_in_wut = nest @String emp Typ (wut [1]) @?= Nothing

case_wut_not_in_type = nest @String emp (wut [1]) Typ @?= Nothing

case_wut_in_wut = nest @String emp (wut [1]) (wut [1,2]) @?= pure ()

case_wut_not_in_wut = nest @String emp (wut [1,2]) (wut [1]) @?= Nothing

case_cel_co = nest @String emp (cel_ (wut [1]) (wut [3])) (cel_ (wut [1,2]) (wut [3,4])) @?= pure ()

case_fun_contra = nest @String emp (fun_ (wut [1,2]) (wut [3])) (fun_ (wut [1]) (wut [3,4])) @?= pure ()

-- used for other tests
case_free_var_nests = nest emp "x" "x" @?= pure ()
case_free_vars_don't_nest = nest emp "x" "y" @?= Nothing

-- cases and cores

-- case_case_sub = undefined

-- distribution

case_cas_cel_in_cel_cas =
  nest
    (env [("x", wut [0, 1]), ("y", wut [2, 3])])
    (cas "x" [(0, cel_ "t1" "t2"), (1, cel_ "t3" "t4")])
    (cel_ (cas "y" [(2, "t1"), (3, "t3")]) (cas "y" [(2, "t2"), (3, "t4")]))
  @?= pure ()

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

-- typings

case_cas_rule_doesn't_screw_us =
  check
    (env [("x", wut [0, 1])])
    (cas "x" [(0, Tag 111), (1, Tag 222)])
    (wut [111, 222])
  @?= pure ()


tests :: TestTree
tests = $(testGroupGenerator)
