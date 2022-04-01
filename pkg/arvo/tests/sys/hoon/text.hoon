::  Test for (4b)Text Processing
::
/+  *test
|%
::  Specs
::
+$  sung-spec  [name=tape input=@t]
::  Helpers
::
++  run-sung
  |=  [exp=? specs=(list sung-spec)]
  %-  zing
  %-  turn
  :-  specs
  |=  spec=sung-spec
  ^-  tang
  =+  result=(expect-eq !>(exp) !>((sung input.spec)))
  ?~  result  ~
  `tang`[[%leaf "in {name.spec}:"] result]
::
++  run-invalid-sung
  |=  specs=(list sung-spec)
  (run-sung | specs)
::
++  run-valid-sung
  |=  specs=(list sung-spec)
  (run-sung & specs)
::
::  UTF8 validation tests
::
::  Invalid UTF8 tests
::
++  test-invalid-utf8
  %-  run-invalid-sung
  :~
    :-  "invalid_first_byte"    '\80'
    :-  "invalid_second_byte"   '\C2a'
    :-  "invalid_third_byte"    '\E1\8Ea'
    :-  "invalid_fourth_byte"   '\F3\A0\80a'
    :-  "bad_E0_input"          '\E0\9F\80'
    :-  "bad_ED_input"          '\ED\A0\80'
    :-  "bad_F0_input"          '\F0\8F\80\80'
    :-  "bad_F4_input"          '\F4\90\80\80'
  ==
::
::  Valid UTF8 tests
::
++  test-valid-utf8
  %-  run-valid-sung
  :~
    :-  "single_byte"         'a'
    :-  "two_bytes"           '\C2\BD'        ::  ½
    :-  "three_bytes"         '\E1\8E\A9'     ::  Ꭹ
    :-  "four_bytes"          '\F3\A0\80\80'
    :-  "good_E0_input"       '\E0\A0\80'     ::  ࠀ
    :-  "good_ED_input"       '\ED\9F\84'     ::  ퟄ
    :-  "good_F0_input"       '\F0\90\80\81'  ::  𐀁
    :-  "good_F4_input"       '\F4\80\80\80'
    :-  "multi_single_byte"   'abc'
    :-  "low_byte"            '\01'
  ==
--
