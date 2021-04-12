::   tests for html
::
/+  *test
=,  html
=,  de-xml:html
=,  en-xml:html
|%
::  de-xml takes a cord but en-xml returns a tape?
::
++  test-de-xml
  ;:  weld
    ::  Basic use
    ::
    %+  expect-eq
      !>  ^-  manx  +:(de-xml:html (crip "<html><head><title>My first webpage</title></head><body><h1>Welcome!</h1>Hello, world! We are on the web.\0a<div></div><script src=\"http://unsafely.tracking.you/cookiemonster.js\"></script></body></html>"))
    !>  ^-  manx
    ;html
        ;head
          ;title: My first webpage
        ==
        ;body
          ;h1: Welcome!
          ; Hello, world! We are on the web.
          ;div;
          ;script(src "http://unsafely.tracking.you/cookiemonster.js");
        ==
      ==
    ::  CDATA sections
    ::
    %+  expect-eq
      !>  ^-  manx
      +:(de-xml:html (crip "<elem><![CDATA[text]]></elem>"))
    !>  ^-  manx
    ;elem: text
    ::  comments
    ::
    %+  expect-eq
      !>  ^-  manx
      ;elem: text
    !>  +:(de-xml:html (crip "<elem>text<!-- comment --></elem>"))
    %+  expect-eq
      !>  ^-  manx
      ;elem;
    !>  +:(de-xml:html (crip "<elem><!-- comment --></elem>"))
    ::  entities
    ::
    %+  expect-eq
      !>  ^-  manx
      +:(de-xml:html (crip "<elem>&#62;</elem>"))
      !>  ^-  manx
      ;elem: >
    :: self-closing tag
    ::
    %+  expect-eq
      !>  ^-  manx
      +:(de-xml:html (crip "<img />"))
      !>  ^-  manx
      ;img;

  ==

::
++  test-en-xml
  ;:  weld
    ::  Entities
    ::
    %+  expect-eq
      !>  "<elem>&gt;</elem>"
    !>  %-  en-xml:html
    ;elem: >
    ::  Basic use
    ::
    %+  expect-eq
      !>   %-  en-xml:html
      ;html
        ;head
          ;title: My first webpage
        ==
        ;body
          ;h1: Welcome!
          ; Hello, world!
          ; We are on the web.
          ;div;
          ;script@"http://unsafely.tracking.you/cookiemonster.js";
        ==
      ==
    !>  "<html><head><title>My first webpage</title></head><body><h1>Welcome!</h1>Hello, world!\0aWe are on the web.\0a<div></div><script src=\"http://unsafely.tracking.you/cookiemonster.js\"></script></body></html>"
    ::  Attributes
    ::
    %+  expect-eq
      !>  "<input type=\"submit\">Submit</input>"
      !>  %-  en-xml:html
      ;input(type "submit"): Submit
  ==
::  JSON encoding/decoding
::
++  from-code-points
  |=  points=(list @)
  (tufa `(list @c)`points)
++  from-code-point
  |=  point=@
  (tuft point)
+$  json-parse-spec  [name=tape input=cord expected=json]
+$  json-parse-rejection-spec  [input=cord name=tape]
::  For checking a large list of examples against expected values.
::  It also nicely formats any failures.
::
++  run-parse-specs
  ::  legend tells of a man who made the Kessel run in less than 12
  ::  parse-specs...
  |=  specs=(list json-parse-spec)
  %-  zing
  %-  turn
  :-  specs
  |=  spec=json-parse-spec
  ^-  tang
  =+  result=(expect-eq !>(`expected.spec) !>((de-json:html input.spec)))
  ?~  result  ~
  `tang`[[%leaf "in {name.spec}:"] result]
::  Checks that a list of examples all fail to parse
::
++  run-parse-rejection-specs
  |=  specs=(list json-parse-rejection-spec)
  %-  zing
  %-  turn
  :-  specs
  |=  spec=json-parse-rejection-spec
  ^-  tang
  =+  result=(expect-eq !>(~) !>((de-json:html input.spec)))
  ?~  result  ~
  `tang`[[%leaf "in {name.spec}:"] result]
::  example values used in tests
::
++  ex
  |%
  ++  two  `json`[%n '2']
  ++  tru  `json`[%b &]
  --
::  encoding naked values
::
++  test-en-json-basics
  ;:  weld
    %+  expect-eq
      !>  "true"
      !>  (en-json [%b &])
    %+  expect-eq
      !>  "false"
      !>  (en-json [%b |])
    %+  expect-eq
      !>  "null"
      !>  (en-json ~)
    %+  expect-eq
      !>  "123.45"
      !>  (en-json [%n '123.45'])
  ==
::  encoding strings, with proper escaping rules
::
++  test-en-json-strings
  ::  A less-confusing representation of theses strings are included in comments
  ::
  ::    Things get confusing with hoon string literal escapes. The
  ::    version included as a comment is if you opened the json output
  ::    in a simple text editor.
  ::
  ;:  weld
    ::  "hello"
    ::
    %+  expect-eq
      !>  "\"hello\""
      !>  (en-json [%s 'hello'])
    ::  it escapes quotes
    ::  "he said \"wow\""
    ::
    %+  expect-eq
      !>  "\"he said \\\"wow\\\"\""
      !>  (en-json [%s 'he said "wow"'])
    ::  it escapes backslashes
    ::  "Delete C:\\Windows\\System32"
    ::
    %+  expect-eq
      !>  "\"Delete C:\\\\Windows\\\\System32\""
      !>  (en-json [%s 'Delete C:\\Windows\\System32'])
    ::  it uses \n for newlines
    ::  "hello\nworld"
    ::
    %+  expect-eq
      !>  "\"hello\\nworld\""
      !>  (en-json [%s 'hello\0aworld'])
    ::  it uses \u encoding for control characters (0x1f and below)
    ::  "ding!\u0007"
    ::
    %+  expect-eq
      !>  "\"ding!\\u0007\""
      !>  (en-json [%s 'ding!\07'])
    ::  it supports null bytes
    ::
    %+  expect-eq
      !>  "\"null\\u0000byte\\u0000separator\""
      !>  (en-json [%s 'null\00byte\00separator'])
    ::  inline unicode characters
    ::
    %+  expect-eq
      !>  "\"lmao 🤣\""
      !>  (en-json [%s 'lmao 🤣'])
  ==
::  encoding arrays
::
++  test-en-json-arrays
  ;:  weld
    ::  empty array
    ::
    %+  expect-eq
      !>  "[]"
      !>  (en-json [%a ~])
    ::  1 element
    ::
    %+  expect-eq
      !>  "[2]"
      !>  (en-json [%a ~[two:ex]])
    ::  multiple elements are comma-separated
    ::
    %+  expect-eq
      !>  "[2,2,2]"
      !>  (en-json [%a ~[two:ex two:ex two:ex]])
  ==
::  encoding basic objects
::
++  test-en-json-objects
  ::  opening curly braces are escaped to avoid urbit string literal
  ::  interpolation
  ::
  ;:  weld
    ::  empty object
    ::
    %+  expect-eq
      !>  "\{}"
      !>  (en-json [%o ~])
    ::  one property
    ::
    %+  expect-eq
      !>  "\{\"foo\":2}"
      !>  (en-json [%o (molt ~[['foo' two:ex]])])
    ::  multiple properties are comma-separated
    ::
    %+  expect-eq
      !>  "\{\"foo\":2,\"bar\":true}"
      !>  (en-json [%o (molt ~[['foo' two:ex] ['bar' tru:ex]])])
    ::  object keys use same encoding logic as strings
    ::
    %+  expect-eq
      ::  {"\u0007\"\n\\":true}
      ::
      !>  "\{\"\\u0007\\\"\\n\\\\\":true}"
      !>  (en-json [%o (molt ~[['\07"\0a\\' tru:ex]])])
  ==
::  object encoding stress-test
::
++  test-en-json-complex-structure
  %+  expect-eq
    ::  [{}, 4, [[], [{foo: {"4": 4, "true": true}}]]]
    ::
    !>  "[\{},4,[[],[\{\"foo\":\{\"4\":4,\"true\":true}}]]]"
    !>  %-  en-json:html
        :-  %a
        :~  [%o ~]
            [%n '4']
            :-  %a
            :~  [%a ~]
                :-  %a
                :~  %+  frond:enjs:format
                      'foo'
                    (pairs:enjs:format ~[['4' [%n '4']] ['true' [%b &]]])
                ==
            ==
        ==
::  decoding naked values
::
++  test-de-json-simple-values
  =,  html
  ;:  weld
    %+  expect-eq
      !>  `~
      !>  (de-json 'null')
    %+  expect-eq
      !>  `[%b &]
      !>  (de-json 'true')
    %+  expect-eq
      !>  `[%b |]
      !>  (de-json 'false')
  ==
::  The following parser test suite (test-de-json-bad-examples and
::  test-en-json-suite) is adapted from https://github.com/nst/JSONTestSuite/
::  (Copyright (c) 2016 Nicolas Seriot) under the terms of the MIT license.
::
::  These are all inputs that should be rejected by a valid json parser
::
++  test-de-json-bad-examples
  %-  run-parse-rejection-specs
  :~
    ['[1 true]' "n_array_1_true_without_comma"]
    ['[aÂ]' "n_array_a_invalid_utf8"]
    ['["": 1]' "n_array_colon_instead_of_comma"]
    ['[""],' "n_array_comma_after_close"]
    ['[,1]' "n_array_comma_and_number"]
    ['[1,,2]' "n_array_double_comma"]
    ['["x",,]' "n_array_double_extra_comma"]
    ['["x"]]' "n_array_extra_close"]
    ['["",]' "n_array_extra_comma"]
    ['["x"' "n_array_incomplete"]
    ['[x' "n_array_incomplete_invalid_value"]
    ['[3[4]]' "n_array_inner_array_no_comma"]
    ['[ˇ]' "n_array_invalid_utf8"]
    ['[1:2]' "n_array_items_separated_by_semicolon"]
    ['[,]' "n_array_just_comma"]
    ['[-]' "n_array_just_minus"]
    ['[   , ""]' "n_array_missing_value"]
    ['["a",\0a4\0a,1,' "n_array_newlines_unclosed"]
    ['[1,]' "n_array_number_and_comma"]
    ['[1,,]' "n_array_number_and_several_commas"]
    ['["\0b"\\f]' "n_array_spaces_vertical_tab_formfeed"]
    ['[*]' "n_array_star_inside"]
    ['[""' "n_array_unclosed"]
    ['[1,' "n_array_unclosed_trailing_comma"]
    ['[1,\0a1\0a,1' "n_array_unclosed_with_new_lines"]
    ['[{}' "n_array_unclosed_with_object_inside"]
    ['[fals]' "n_incomplete_false"]
    ['[nul]' "n_incomplete_null"]
    ['[tru]' "n_incomplete_true"]
    ['[++1234]' "n_number_++"]
    ['[+1]' "n_number_+1"]
    ['[+Inf]' "n_number_+Inf"]
    ['[-01]' "n_number_-01"]
    ['[-1.0.]' "n_number_-1.0."]
    ['[-NaN]' "n_number_-NaN"]
    ['[.-1]' "n_number_.-1"]
    ['[.2e-3]' "n_number_.2e-3"]
    ['[0.1.2]' "n_number_0.1.2"]
    ['[1 000.0]' "n_number_1_000"]
    ['[1eE2]' "n_number_1eE2"]
    ['[Inf]' "n_number_Inf"]
    ['[NaN]' "n_number_NaN"]
    ['[Ôºë]' "n_number_U+FF11_fullwidth_digit_one"]
    ['[1+2]' "n_number_expression"]
    ['[0x1]' "n_number_hex_1_digit"]
    ['[0x42]' "n_number_hex_2_digits"]
    ['[Infinity]' "n_number_infinity"]
    ['[0e+-1]' "n_number_invalid+-"]
    ['[-123.123foo]' "n_number_invalid-negative-real"]
    ['[123Â]' "n_number_invalid-utf-8-in-bigger-int"]
    ['[1e1Â]' "n_number_invalid-utf-8-in-exponent"]
    ['[0Â]' "n_number_invalid-utf-8-in-int"]
    ['[-Infinity]' "n_number_minus_infinity"]
    ['[-foo]' "n_number_minus_sign_with_trailing_garbage"]
    ['[- 1]' "n_number_minus_space_1"]
    ['[-012]' "n_number_neg_int_starting_with_zero"]
    ['[-.123]' "n_number_neg_real_without_int_part"]
    ['[-1x]' "n_number_neg_with_garbage_at_end"]
    ['[1ea]' "n_number_real_garbage_after_e"]
    ['[1eÂ]' "n_number_real_with_invalid_utf8_after_e"]
    ['[.123]' "n_number_starting_with_dot"]
    ['[1.2a-3]' "n_number_with_alpha"]
    ['[1.8011670033376514H-308]' "n_number_with_alpha_char"]
    ['[012]' "n_number_with_leading_zero"]
    ['["x", truth]' "n_object_bad_value"]
    ['{[: "x"}' "n_object_bracket_key"]
    ['{"x", null}' "n_object_comma_instead_of_colon"]
    ['{"x"::"b"}' "n_object_double_colon"]
    ['{üá®üá≠}' "n_object_emoji"]
    ['{"a":"a" 123}' "n_object_garbage_at_end"]
    ['{key: \'value\'}' "n_object_key_with_single_quotes"]
    ['{"π":"0",}' "n_object_lone_continuation_byte_in_key_and_trailing_comma"]
    ['{"a" b}' "n_object_missing_colon"]
    ['{:"b"}' "n_object_missing_key"]
    ['{"a" "b"}' "n_object_missing_semicolon"]
    ['{"a":' "n_object_missing_value"]
    ['{"a"' "n_object_no-colon"]
    ['{1:1}' "n_object_non_string_key"]
    ['{9999E9999:1}' "n_object_non_string_key_but_huge_number_instead"]
    ['{null:null,null:null}' "n_object_repeated_null_null"]
    ['{"id":0,,,,,}' "n_object_several_trailing_commas"]
    ['{\'a\':0}' "n_object_single_quote"]
    ['{"id":0,}' "n_object_trailing_comma"]
    ['{"a":"b"}/**/' "n_object_trailing_comment"]
    ['{"a":"b"}/**//' "n_object_trailing_comment_open"]
    ['{"a":"b"}//' "n_object_trailing_comment_slash_open"]
    ['{"a":"b"}/' "n_object_trailing_comment_slash_open_incomplete"]
    ['{"a":"b",,"c":"d"}' "n_object_two_commas_in_a_row"]
    ['{a: "b"}' "n_object_unquoted_key"]
    ['{"a":"a' "n_object_unterminated-value"]
    ['{ "foo" : "bar", "a" }' "n_object_with_single_string"]
    ['{"a":"b"}#' "n_object_with_trailing_garbage"]
    [' ' "n_single_space"]
    ['["\\uD800\\"]' "n_string_1_surrogate_then_escape"]
    ['["\\uD800\\u"]' "n_string_1_surrogate_then_escape_u"]
    ['["\\uD800\\u1"]' "n_string_1_surrogate_then_escape_u1"]
    ['["\\uD800\\u1x"]' "n_string_1_surrogate_then_escape_u1x"]
    ['[√©]' "n_string_accentuated_char_no_quotes"]
    ['["\\x00"]' "n_string_escape_x"]
    ['["\\\\\\"]' "n_string_escaped_backslash_bad"]
    ['["\\\09"]' "n_string_escaped_ctrl_char_tab"]
    ['["\\üåÄ"]' "n_string_escaped_emoji"]
    ['["\\"]' "n_string_incomplete_escape"]
    ['["\\u00A"]' "n_string_incomplete_escaped_character"]
    ['["\\uD834\\uDd"]' "n_string_incomplete_surrogate"]
    ['["\\uD800\\uD800\\x"]' "n_string_incomplete_surrogate_escape_invalid"]
    ['["\\uÂ"]' "n_string_invalid-utf-8-in-escape"]
    ['["\\a"]' "n_string_invalid_backslash_esc"]
    ['["\\uqqqq"]' "n_string_invalid_unicode_escape"]
    ['["\\Â"]' "n_string_invalid_utf8_after_escape"]
    ['[\\u0020"asd"]' "n_string_leading_uescaped_thinspace"]
    ['[\\n]' "n_string_no_quotes_with_bad_escape"]
    ['"' "n_string_single_doublequote"]
    ['[\'single quote\']' "n_string_single_quote"]
    ['abc' "n_string_single_string_no_double_quotes"]
    ['["\\' "n_string_start_escape_unclosed"]
    ['["new' "n_string_unescaped_newline"]
    ['line"]' "n_string_unescaped_newline"]
    ['["\09"]' "n_string_unescaped_tab"]
    ['"\\UA66D"' "n_string_unicode_CapitalU"]
    ['""x' "n_string_with_trailing_garbage"]
    ['[‚Å†]' "n_structure_U+2060_word_joined"]
    ['Ôªø' "n_structure_UTF8_BOM_no_data"]
    ['<.>' "n_structure_angle_bracket_."]
    ['[<null>]' "n_structure_angle_bracket_null"]
    ['[1]x' "n_structure_array_trailing_garbage"]
    ['[1]]' "n_structure_array_with_extra_array_close"]
    ['["asd]' "n_structure_array_with_unclosed_string"]
    ['a√•' "n_structure_ascii-unicode-identifier"]
    ['[True]' "n_structure_capitalized_True"]
    ['1]' "n_structure_close_unopened_array"]
    ['{"x": true,' "n_structure_comma_instead_of_closing_brace"]
    ['[][]' "n_structure_double_array"]
    [']' "n_structure_end_array"]
    ['Ôª{}' "n_structure_incomplete_UTF8_BOM"]
    ['Â' "n_structure_lone-invalid-utf-8"]
    ['[' "n_structure_lone-open-bracket"]
    ['["a\00a"]' "n_string_unescaped_crtl_char"]
    ['["\\00"]' "n_string_backslash_00"]
  ==
::  TODO: de-json is accepting a slew of number formats it shouldn't.
::
::    Tracking issue here: https://github.com/urbit/urbit/issues/1775
::    Re-enable this test by removing the disable- prefix
::
++  disable-test-reject-invalid-numbers
    %-  run-parse-rejection-specs
    :~
      ['123\00' "n_multidigit_number_then_00"]
      ['[1.]' "n_number_real_without_fractional_part"]
      ['[2.e+3]' "n_number_2.e+3"]
      ['[2.e-3]' "n_number_2.e-3"]
      ['[2.e3]' "n_number_2.e3"]
      ['[9.e+]' "n_number_9.e+"]
      ['[0.3e+]' "n_number_0.3e+"]
      ['[0.3e]' "n_number_0.3e"]
      ['[0.e1]' "n_number_0.e1"]
      ['[0E+]' "n_number_0_capital_E+"]
      ['[0E]' "n_number_0_capital_E"]
      ['[0e+]' "n_number_0e+"]
      ['[0e]' "n_number_0e"]
      ['[1.0e+]' "n_number_1.0e+"]
      ['[1.0e-]' "n_number_1.0e-"]
      ['[1.0e]' "n_number_1.0e"]
      ['[-2.]' "n_number_-2."]
    ==
::   these are all inputs that should be accepted by a valid parser
::
++  test-en-json-suite
  =+  frond=frond:enjs:format
  =+  pairs=pairs:enjs:format
  %-  run-parse-specs
  :~
    :*  "y_array_arraysWithSpaces"
        '[[]   ]'
        [%a ~[[%a ~]]]
    ==
    :*  "y_array_empty-string"
        '[""]'
        [%a ~[[%s '']]]
    ==
    :*  "y_array_empty"
        '[]'
        [%a ~]
    ==
    :*  "y_array_ending_with_newline"
        '["a"]\0a'
        [%a ~[[%s 'a']]]
    ==
    :*  "y_array_false"
        '[false]'
        [%a ~[[%b |]]]
    ==
    :*  "y_array_heterogeneous"
        '[null, 1, "1", {}]'
        [%a ~[~ [%n '1'] [%s '1'] [%o ~]]]
    ==
    :*  "y_array_null"
        '[null]'
        [%a ~[~]]
    ==
    :*  "y_array_with_1_and_newline"
        '[1\0a]'
        [%a ~[[%n '1']]]
    ==
    :*  "y_array_with_leading_space"
        ' [1]'
        [%a ~[[%n '1']]]
    ==
    :*  "y_array_with_several_null"
        '[1,null,null,null,2]'
        [%a ~[[%n '1'] ~ ~ ~ [%n '2']]]
    ==
    :*  "y_array_with_trailing_space"
        '[2] '
        [%a ~[[%n '2']]]
    ==
    :*  "y_number"
        '[123e65]'
        [%a ~[[%n '123e65']]]
    ==
    :*  "y_number_0e+1"
        '[0e+1]'
        [%a ~[[%n '0e+1']]]
    ==
    :*  "y_number_0e1"
        '[0e1]'
        [%a ~[[%n '0e1']]]
    ==
    :*  "y_number_after_space"
        '[ 4]'
        [%a ~[[%n '4']]]
    ==
    :*  "y_number_double_close_to_zero"
        '[-0.0000000000000000000000000000000000000000000000000000000\
        /00000000000000000000001]'
        [%a ~[[%n '-0.0000000000000000000000000000000000000000000000\
        /00000000000000000000000000000001']]]
    ==
    :*  "y_number_int_with_exp"
        '[20e1]'
        [%a ~[[%n '20e1']]]
    ==
    :*  "y_number_minus_zero"
        '[-0]'
        [%a ~[[%n '-0']]]
    ==
    :*  "y_number_negative_int"
        '[-123]'
        [%a ~[[%n '-123']]]
    ==
    :*  "y_number_negative_one"
        '[-1]'
        [%a ~[[%n '-1']]]
    ==
    :*  "y_number_negative_zero"
        '[-0]'
        [%a ~[[%n '-0']]]
    ==
    :*  "y_number_real_capital_e"
        '[1E22]'
        [%a ~[[%n '1E22']]]
    ==
    :*  "y_number_real_capital_e_neg_exp"
        '[1E-2]'
        [%a ~[[%n '1E-2']]]
    ==
    :*  "y_number_real_capital_e_pos_exp"
        '[1E+2]'
        [%a ~[[%n '1E+2']]]
    ==
    :*  "y_number_real_exponent"
        '[123e45]'
        [%a ~[[%n '123e45']]]
    ==
    :*  "y_number_real_fraction_exponent"
        '[123.456e78]'
        [%a ~[[%n '123.456e78']]]
    ==
    :*  "y_number_real_neg_exp"
        '[1e-2]'
        [%a ~[[%n '1e-2']]]
    ==
    :*  "y_number_real_pos_exponent"
        '[1e+2]'
        [%a ~[[%n '1e+2']]]
    ==
    :*  "y_number_simple_int"
        '[123]'
        [%a ~[[%n '123']]]
    ==
    :*  "y_number_simple_real"
        '[123.456789]'
        [%a ~[[%n '123.456789']]]
    ==
    :*  "y_object"
        '{"asd":"sdf", "dfg":"fgh"}'
        (pairs ~[['asd' [%s 'sdf']] ['dfg' [%s ['fgh']]]])
    ==
    :*  "y_object_basic"
        '{"asd":"sdf"}'
        (frond ['asd' [%s 'sdf']])
    ==
    ::   duplicated keys, it takes the latest one.
    ::
    :*  "y_object_duplicated_key"
        '{"a":"b","a":"c"}'
        (frond ['a' [%s 'c']])
    ==
    :*  "y_object_duplicated_key_and_value"
        '{"a":"b","a":"b"}'
        (frond ['a' [%s 'b']])
    ==
    :*  "y_object_empty"
        '{}'
        [%o ~]
    ==
    :*  "y_object_empty_key"
        '{"":0}'
        (frond ['' [%n '0']])
    ==
    :*  "y_object_extreme_numbers"
        '{ "min": -1.0e+28, "max": 1.0e+28 }'
        (pairs ~[['min' [%n '-1.0e+28']] ['max' [%n '1.0e+28']]])
    ==
    =/  long=@t  'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
    :*  "y_object_long_strings"
        '{"x":[{"id": "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"}], \
        /"id": "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"}'
        (pairs ~[['id' [%s long]] ['x' [%a ~[(frond ['id' [%s long]])]]]])
    ==
    :*  "y_object_simple"
        '{"a":[]}'
        (frond 'a' [%a ~])
    ==
    :*  "y_object_string_unicode"
        '{"title":"\\u041f\\u043e\\u043b\\u0442\\u043e\\u0440\\u0430 \
        /\\u0417\\u0435\\u043c\\u043b\\u0435\\u043a\\u043e\\u043f\\u0430" }'
        (frond 'title' [%s 'Полтора Землекопа'])
    ==
    :*  "y_object_with_newlines"
        '{\0a"a": "b"\0a}'
        (frond 'a' [%s 'b'])
    ==
    :*  "y_string_allowed_escapes"
        '["\\"\\\\\\/\\b\\f\\n\\r\\t"]'
        [%a ~[[%s '"\\/\08\0c\0a\0d\09']]]
    ==
    :*  "y_string_backslash_and_u_escaped_zero"
        '["\\\\u0000"]'
        [%a ~[[%s '\\u0000']]]
    ==
    :*  "y_string_backslash_doublequotes"
        '["\\""]'
        [%a ~[[%s '"']]]
    ==
    :*  "y_string_comments"
        '["a/*b*/c/*d//e"]'
        [%a ~[[%s 'a/*b*/c/*d//e']]]
    ==
    :*  "y_string_double_escape_a"
        '["\\\\a"]'
        [%a ~[[%s '\\a']]]
    ==
    :*  "y_string_double_escape_n"
        '["\\\\n"]'
        [%a ~[[%s '\\n']]]
    ==
    :*  "y_string_escaped_control_character"
        '["\\u0012"]'
        [%a ~[[%s '\12']]]
    ==
    :*  "y_string_in_array_with_leading_space"
        '[ "asd"]'
        [%a ~[[%s 'asd']]]
    ==
    :*  "y_string_nonCharacterInUTF-8_U+10FFFF"
        '["􏿿"]'
        [%a ~[[%s (from-code-point 0x10.ffff)]]]
    ==
    :*  "y_string_nonCharacterInUTF-8_U+FFFF"
        '["￿"]'
        [%a ~[[%s (from-code-point 0xffff)]]]
    ==
    :*  "y_string_null_escape"
        '["\\u0000"]'
        [%a ~[[%s '\00']]]
    ==
    :*  "y_string_one-byte-utf-8"
        '["\\u002c"]'
        [%a ~[[%s '\2c']]]
    ==
    :*  "y_string_pi"
        '["π"]'
        [%a ~[[%s 'π']]]
    ==
    :*  "y_string_reservedCharacterInUTF-8_U+1BFFF"
        '["𛿿"]'
        [%a ~[[%s (from-code-point 0x1.bfff)]]]
    ==
    :*  "y_string_simple_ascii"
        '["asd "]'
        [%a ~[[%s 'asd ']]]
    ==
    :*  "y_string_space"
        '" "'
        [%s ' ']
    ==
    :*  "y_string_three-byte-utf-8"
        '["\\u0821"]'
        [%a ~[[%s (from-code-point 0x821)]]]
    ==
    :*  "y_string_two-byte-utf-8"
        '["\\u0123"]'
        [%a ~[[%s (from-code-point 0x123)]]]
    ==
    :*  "y_string_u+2028_line_sep"
        '[" "]'
        [%a ~[[%s (from-code-point 0x2028)]]]
    ==
    :*  "y_string_u+2029_par_sep"
        '[" "]'
        [%a ~[[%s (from-code-point 0x2029)]]]
    ==
    :*  "y_string_unicode_2"
        '["⍂㈴⍂"]'
        [%a ~[[%s '⍂㈴⍂']]]
    ==
    :*  "y_string_unicode_U+2064_invisible_plus"
        '["\\u2064"]'
        [%a ~[[%s (from-code-point 0x2064)]]]
    ==
    :*  "y_string_unicode_escaped_double_quote"
        '["\\u0022"]'
        [%a ~[[%s (from-code-point 0x22)]]]
    ==
    :*  "y_string_utf8"
        '["€𝄞"]'
        [%a ~[[%s '€𝄞']]]
    ==
    :*  "y_structure_lonely_false"
        'false'
        [%b |]
    ==
    :*  "y_structure_lonely_int"
        '42'
        [%n '42']
    ==
    :*  "y_structure_lonely_negative_real"
        '-0.1'
        [%n '-0.1']
    ==
    :*  "y_structure_lonely_null"
        'null'
        ~
    ==
    :*  "y_structure_lonely_string"
        '"asd"'
        [%s 'asd']
    ==
    :*  "y_structure_lonely_true"
        'true'
        [%b &]
    ==
    :*  "y_structure_string_empty"
        '""'
        [%s '']
    ==
    :*  "y_structure_trailing_newline"
        '["a"]\0a'
        [%a ~[[%s 'a']]]
    ==
    :*  "y_structure_true_in_array"
        '[true]'
        [%a ~[[%b &]]]
    ==
    :*  "y_structure_whitespace_array"
        ' [] '
        [%a ~]
    ==
  ==
::  TODO: de-json is rejecting or dropping unicode escape sequences
::
::    Tracking issue here: https://github.com/urbit/urbit/issues/1776
::    Re-enable this test by removing the disable- prefix
::
++  disable-test-parse-unicode-escape-sequences
  =+  frond=frond:enjs:format
  =+  pairs=pairs:enjs:format
  %-  run-parse-specs
  :~
    :*  "y_string_with_del_character"
        '["a\7fa"]'
        [%a ~[[%s 'a\7fa']]]
    ==
    :*  "y_string_unicode_U+FDD0_nonchar"
        '["\\uFDD0"]'
        [%a ~[[%s (from-code-point 0xfdd0)]]]
    ==
    :*  "y_string_unicode_U+FFFE_nonchar"
        '["\\uFFFE"]'
        [%a ~[[%s (from-code-point 0xfffe)]]]
    ==
    :*  "y_string_unicode_U+10FFFE_nonchar"
        '["\\uDBFF\\uDFFE"]'
        [%a ~[[%s (crip (from-code-points ~[0xdbff 0xdffe]))]]]
    ==
    :*  "y_string_unicode_U+1FFFE_nonchar"
        '["\\uD83F\\uDFFE"]'
        [%a ~[[%s (crip (from-code-points ~[0xd83f 0xdffe]))]]]
    ==
    :*  "y_string_unicode_U+200B_ZERO_WIDTH_SPACE"
        '["\\u200B"]'
        [%a ~[[%s (from-code-point 0x200b)]]]
    ==
    :*  "y_string_uEscape"
        '["\\u0061\\u30af\\u30EA\\u30b9"]'
        [%a ~[[%s (crip (from-code-points ~[0x61 0x30af 0x30ea 0x30b9]))]]]
    ==
    :*  "y_string_uescaped_newline"
        '["new\\u000Aline"]'
        [%a ~[[%s 'new\0aline']]]
    ==
    :*  "y_string_unescaped_char_delete"
        '["\7f"]'
        [%a ~[[%s '\7f']]]
    ==
    :*  "y_string_unicode"
        '["\\uA66D"]'
        [%a ~[[%s (from-code-point 0xa66d)]]]
    ==
    :*  "y_string_unicodeEscapedBackslash"
        '["\\u005C"]'
        [%a ~[[%s (from-code-point 0x5c)]]]
    ==
    :*  "y_string_surrogates_U+1D11E_MUSICAL_SYMBOL_G_CLEF"
        '["\\uD834\\uDd1e"]'
        [%a ~[[%s (crip (from-code-points ~[0xd834 0xdd1e]))]]]
    ==
    :*  "y_string_last_surrogates_1_and_2"
        '["\\uDBFF\\uDFFF"]'
        [%a ~[[%s (crip (from-code-points ~[0xdbff 0xdfff]))]]]
    ==
    :*  "y_string_nbsp_uescaped"
        '["new\\u00A0line"]'
        [%a ~[[%s (crip "new{(from-code-points ~[0xa0])}line")]]]
    ==
    :*  "y_string_escaped_noncharacter"
        '["\\uFFFF"]'
        [%a ~[[%s (from-code-point 0xffff)]]]
    ==
    :*  "y_string_escaped_null"
        '"foo\\u0000bar"'
        [%s 'foo\00bar']
    ==
    :*  "y_object_escaped_null_in_key"
        '{"foo\\u0000bar": 42}'
        (frond ['foo\00bar' [%n '42']])
    ==
    :*  "y_string_1_2_3_bytes_UTF-8_sequences"
        '["\\u0060\\u012a\\u12AB"]'
        [%a ~[[%s '`Īካ']]]
    ==
    :*  "y_string_accepted_surrogate_pair"
        '["\\uD801\\udc37"]'
        [%a ~[[%s '𐐷']]]
    ==
    :*  "y_string_accepted_surrogate_pairs"
        '["\\ud83d\\ude39\\ud83d\\udc8d"]'
        [%a ~[[%s '😹💍']]]
    ==
  ==
--
