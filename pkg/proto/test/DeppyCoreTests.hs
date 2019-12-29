module DeppyCoreTests where

import ClassyPrelude

import qualified Data.Map as Map
import Data.Either
import Data.Maybe (fromJust)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH

import Deppy.Core

instance IsString s => IsString (Typ s) where
  fromString = Var . fromString

assertLeft = assertBool "expected type error" . isLeft

-- basic subtyping tests

emp = const undefined
env bs v = fromJust $ Map.lookup v $ mapFromList bs

case_type_in_type = nest @Text emp Typ Typ @?= pure ()

case_type_not_in_wut = assertLeft $ nest @Text emp Typ (wut [1])

case_wut_not_in_type = assertLeft $ nest @Text emp (wut [1]) Typ

case_wut_in_wut = nest @Text emp (wut [1]) (wut [1,2]) @?= pure ()

case_wut_not_in_wut = assertLeft $ nest @Text emp (wut [1,2]) (wut [1])

case_cel_co = nest @Text emp (cel_ (wut [1]) (wut [3])) (cel_ (wut [1,2]) (wut [3,4])) @?= pure ()

case_fun_contra = nest @Text emp (fun_ (wut [1,2]) (wut [3])) (fun_ (wut [1]) (wut [3,4])) @?= pure ()

-- used for other tests
case_free_var_nests = nest @Text emp "x" "x" @?= pure ()
case_free_vars_don't_nest = assertLeft $ nest @Text emp "x" "y"

-- cases and cores

coreT, caseT :: [(Tag, Typ Text)] -> Typ Text

coreT as =
  fun "$private_var_name"
    (wut $ map fst as)
    (cas "$private_var_name" as)

caseT cs =
  cel "$private_var_name"
    (wut $ map fst cs)
    (cas "$private_var_name" cs)

case_core_sub =
  nest
    emp
    (coreT [(0, "x"), (1, wut [111])])
    (coreT [(1, wut [111, 222])])
  @?= pure ()

case_case_sub =
  nest
    emp
    (caseT [(1, wut [111])])
    (caseT [(0, "x"), (1, wut [111, 222])])
  @?= pure ()

-- distribution

-- WRONG.
-- case_cas_cel_in_cel_cas =
--   nest
--     (env [("x", wut [0, 1]), ("y", wut [2, 3])])
--     (cas "x" [(0, cel_ "t1" "t2"), (1, cel_ "t3" "t4")])
--     (cel_ (cas "y" [(2, "t1"), (3, "t3")]) (cas "y" [(2, "t2"), (3, "t4")]))
--   @?= pure ()

case_cel_cas_not_in_cas_cel = assertLeft $
  nest
    (env [("x", wut [0, 1]), ("y", wut [2, 3])])
    (cel_ (cas "y" [(2, "t1"), (3, "t3")]) (cas "y" [(2, "t2"), (3, "t4")]))
    (cas "x" [(0, cel_ "t1" "t2"), (1, cel_ "t3" "t4")])

case_cel_cas_in_cas_cel = assertLeft $
  nest
    (env [("x", wut [0, 1, 2, 3]), ("y", wut [2, 3])])
    (cel_ (cas "y" [(2, "t1"), (3, "t3")]) (cas "y" [(2, "t2"), (3, "t4")]))
    (cas "x" [ (0, cel_ "t1" "t2")
             , (1, cel_ "t3" "t4")
             , (2, cel_ "t1" "t4")
             , (3, cel_ "t3" "t2")
             ])

-- silly recursion

case_dangling_rec_in = nest emp (rec "_" Typ $ wut [1]) (wut [1,2]) @?= pure ()

case_in_dangling_rec = nest emp (wut [1]) (rec "_" Typ $ wut [1,2]) @?= pure ()

case_dangling_rec_not_in = assertLeft $ nest emp (rec "_" Typ $ wut [1,2]) (wut [1])

case_not_in_dangling_rec = assertLeft $ nest emp (wut [1,2]) (rec "_" Typ $ wut [1])

-- recursive types

-- these cases are taken from Harper, PFPL 2 ed., pp. 220-221

case_harper_bad_rec_doesn't_nest = assertLeft $
  nest
    emp
    (rec "t" Typ $ coreT [(0, fun_ "t" $ wut [11]),     (1, fun_ "t" $ wut [11, 22])])
    (rec "t" Typ $ coreT [(0, fun_ "t" $ wut [11, 22]), (1, fun_ "t" $ wut [11, 22])])

labeled_binary = rec "t" Typ $ caseT [ (0, wut [0])
                                     , (1, coreT [(10, wut [123]), undefined])
                                     ]

-- list test cases

pattern SIG = 0
pattern NIL = 917
pattern CONS = 3095

nilT t = cel_ (Tag NIL) (Tag SIG)

consT t self = cel_ (Tag CONS) $ cel_ t self

listCellT = undefined

-- typings

case_cas_rule_doesn't_screw_us =
  check @Text
    (env [("x", wut [0, 1])])
    (cas "x" [(0, Tag 111), (1, Tag 222)])
    (wut [111, 222])
  @?= pure ()


tests :: TestTree
tests = $(testGroupGenerator)
