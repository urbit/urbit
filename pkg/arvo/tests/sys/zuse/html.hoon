/+  *test
=,  html
|%
++  from-code-points
  |=  points=(list @)
    (tufa `(list @c)`points)
++  from-code-point
  |=  point=@
    (tuft point)
++  test-en-json-basics
  :: question - should the serializer reject badly formatted number
  :: strings? like should (en-json [%n 'xyz']) crash?
  :: at the moment it just writes it on through:
  :: > (en-json:html [%a ~[[%n 'xyz']]])
  :: "[xyz]"
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
++  test-en-json-strings
  :: things get a little confusing with hoon string literal escaping,
  :: so I'll also include a more readable version in comments, like
  :: if you just opened the output in a normal text editor.
  ;:  weld
    :: "hello"
    %+  expect-eq
      !>  "\"hello\""
      !>  (en-json [%s 'hello'])
    :: it escapes quotes
    :: "he said \"wow\""
    %+  expect-eq
      !>  "\"he said \\\"wow\\\"\""
      !>  (en-json [%s 'he said "wow"'])
    :: it escapes backslashes
    :: "Delete C:\\Windows\\System32"
    %+  expect-eq
      !>  "\"Delete C:\\\\Windows\\\\System32\""
      !>  (en-json [%s 'Delete C:\\Windows\\System32'])
    :: it uses \n for newlines
    :: "hello\nworld"
    %+  expect-eq
      !>  "\"hello\\nworld\""
      !>  (en-json [%s 'hello\0aworld'])
    :: it uses \u encoding for control characters (0x1f and below)
    :: "ding!\u0007"
    %+  expect-eq
      !>  "\"ding!\\u0007\""
      !>  (en-json [%s 'ding!\07'])
    :: it supports null bytes
    %+  expect-eq
      !>  "\"null\\u0000byte\\u0000separator\""
      !>  (en-json [%s 'null\00byte\00separator'])
    :: inline unicode characters
    %+  expect-eq
      !>  "\"lmao 🤣\""
      !>  (en-json [%s 'lmao 🤣'])
  ==
  ++  two  `json`[%n '2']
  ++  tru  `json`[%b &]
  ++  foo  `json`[%s 'foo']
  ++  test-en-json-arrays
    ;:  weld
      :: empty array
      %+  expect-eq
        !>  "[]"
        !>  (en-json [%a ~])
      :: 1 element
      %+  expect-eq
        !>  "[2]"
        !>  (en-json [%a ~[two]])
      :: multiple elements are comma-separated
      %+  expect-eq
        !>  "[2,2,2]"
        !>  (en-json [%a ~[two two two]])
    ==
  ++  test-en-json-objects
    :: opening curly braces are escaped to avoid urbit string literal
    :: interpolation
    ;:  weld
      :: empty object
      %+  expect-eq
        !>  "\{}"
        !>  (en-json [%o ~])
      :: one property
      %+  expect-eq
        !>  "\{\"foo\":2}"
        !>  (en-json [%o (molt ~[['foo' two]])])
      :: multiple properties are comma-separated
      %+  expect-eq
        !>  "\{\"foo\":2,\"bar\":true}"
        !>  (en-json [%o (molt ~[['foo' two] ['bar' tru]])])
      :: object keys use same encoding logic as strings
      %+  expect-eq
        :: {"\u0007\"\n\\":true}
        !>  "\{\"\\u0007\\\"\\n\\\\\":true}"
        !>  (en-json [%o (molt ~[['\07"\0a\\' tru]])])
    ==
  ++  test-en-json-complex-structure
    %+  expect-eq
      !>  "[\{},4,[[],[\{\"foo\":\{\"4\":4,\"true\":true}}]]]"
      !>  (en-json [%a ~[[%o ~] [%n '4'] [%a ~[[%a ~] [%a ~[(frond:enjs:format 'foo' (pairs:enjs:format ~[['4' [%n '4']] ['true' [%b &]]]))]]]]]])
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
  :: The following parser test suite (test-de-json-bad-examples and
  :: test-en-json-suite) is adapted from https://github.com/nst/JSONTestSuite/
  ::
  ::
  :: MIT License
  ::
  :: Copyright (c) 2016 Nicolas Seriot
  ::
  :: Permission is hereby granted, free of charge, to any person
  :: obtaining a copy of this software and associated documentation
  :: files (the "Software"), to deal in the Software without
  :: restriction, including without limitation the rights to use, copy,
  :: modify, merge, publish, distribute, sublicense, and/or sell copies
  :: of the Software, and to permit persons to whom the Software is
  :: furnished to do so, subject to the following conditions:
  ::
  :: The above copyright notice and this permission notice shall be
  :: included in all copies or substantial portions of the Software.
  ::
  :: THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY
  :: KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
  :: WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
  :: NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
  :: BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
  :: ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
  :: CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  :: SOFTWARE.
  ::
  ::
  :: These are all inputs that should be rejected by a valid json parser
  ++  test-de-json-bad-examples
    %-  zing
    %-  turn
    :_  |=  [exmpl=@t name=tape]
          ^-  tang
          =+  result=(expect-eq !>(~) !>((de-json:html exmpl)))
          ?~  result  ~
          `tang`[[%leaf "in {name}:"] result] 
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
      ['[-2.]' "n_number_-2."]
      ['[-NaN]' "n_number_-NaN"]
      ['[.-1]' "n_number_.-1"]
      ['[.2e-3]' "n_number_.2e-3"]
      ['[0.1.2]' "n_number_0.1.2"]
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
      ['[1 000.0]' "n_number_1_000"]
      ['[1eE2]' "n_number_1eE2"]
      ['[2.e+3]' "n_number_2.e+3"]
      ['[2.e-3]' "n_number_2.e-3"]
      ['[2.e3]' "n_number_2.e3"]
      ['[9.e+]' "n_number_9.e+"]
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
      ['[1.]' "n_number_real_without_fractional_part"]
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
      ['123\00' "n_multidigit_number_then_00"]
      ['["a\00a"]' "n_string_unescaped_crtl_char"]
      ['["\\00"]' "n_string_backslash_00"]
    ==
::  these are all inputs that should be accepted by a valid parser
++  test-en-json-suite
  =+  frond=frond:enjs:format
  =+  pairs=pairs:enjs:format
  %-  zing
  %-  turn
  :_  |=  [name=tape str=@t val=json]
        ^-  tang
        =+  result=(expect-eq !>(`val) !>((de-json:html str)))
        ?~  result  ~
        `tang`[[%leaf "in {name}:"] result]
  :~
    :*  "y_array_arraysWithSpaces"
        '[[]   ]'
        `json`[%a ~[[%a ~]]]
    ==
    :*  "y_array_empty-string"
        '[""]'
        `json`[%a ~[[%s '']]]
    ==
    :*  "y_array_empty"
        '[]'
        `json`[%a ~]
    ==
    :*  "y_array_ending_with_newline"
        '["a"]\0a'
        `json`[%a ~[[%s 'a']]]
    ==
    :*  "y_array_false"
        '[false]'
        `json`[%a ~[[%b |]]]
    ==
    :*  "y_array_heterogeneous"
        '[null, 1, "1", {}]'
        `json`[%a ~[~ [%n '1'] [%s '1'] [%o ~]]]
    ==
    :*  "y_array_null"
        '[null]'
        `json`[%a ~[~]]
    ==
    :*  "y_array_with_1_and_newline"
        '[1\0a]'
        `json`[%a ~[[%n '1']]]
    ==
    :*  "y_array_with_leading_space"
        ' [1]'
        `json`[%a ~[[%n '1']]]
    ==
    :*  "y_array_with_several_null"
        '[1,null,null,null,2]'
        `json`[%a ~[[%n '1'] ~ ~ ~ [%n '2']]]
    ==
    :*  "y_array_with_trailing_space"
        '[2] '
        `json`[%a ~[[%n '2']]]
    ==
    :*  "y_number"
        '[123e65]'
        `json`[%a ~[[%n '123e65']]]
    ==
    :*  "y_number_0e+1"
        '[0e+1]'
        `json`[%a ~[[%n '0e+1']]]
    ==
    :*  "y_number_0e1"
        '[0e1]'
        `json`[%a ~[[%n '0e1']]]
    ==
    :*  "y_number_after_space"
        '[ 4]'
        `json`[%a ~[[%n '4']]]
    ==
    :*  "y_number_double_close_to_zero"
        '[-0.000000000000000000000000000000000000000000000000000000000000000000000000000001]'
        `json`[%a ~[[%n '-0.000000000000000000000000000000000000000000000000000000000000000000000000000001']]]
    ==
    :*  "y_number_int_with_exp"
        '[20e1]'
        `json`[%a ~[[%n '20e1']]]
    ==
    :*  "y_number_minus_zero"
        '[-0]'
        `json`[%a ~[[%n '-0']]]
    ==
    :*  "y_number_negative_int"
        '[-123]'
        `json`[%a ~[[%n '-123']]]
    ==
    :*  "y_number_negative_one"
        '[-1]'
        `json`[%a ~[[%n '-1']]]
    ==
    :*  "y_number_negative_zero"
        '[-0]'
        `json`[%a ~[[%n '-0']]]
    ==
    :*  "y_number_real_capital_e"
        '[1E22]'
        `json`[%a ~[[%n '1E22']]]
    ==
    :*  "y_number_real_capital_e_neg_exp"
        '[1E-2]'
        `json`[%a ~[[%n '1E-2']]]
    ==
    :*  "y_number_real_capital_e_pos_exp"
        '[1E+2]'
        `json`[%a ~[[%n '1E+2']]]
    ==
    :*  "y_number_real_exponent"
        '[123e45]'
        `json`[%a ~[[%n '123e45']]]
    ==
    :*  "y_number_real_fraction_exponent"
        '[123.456e78]'
        `json`[%a ~[[%n '123.456e78']]]
    ==
    :*  "y_number_real_neg_exp"
        '[1e-2]'
        `json`[%a ~[[%n '1e-2']]]
    ==
    :*  "y_number_real_pos_exponent"
        '[1e+2]'
        `json`[%a ~[[%n '1e+2']]]
    ==
    :*  "y_number_simple_int"
        '[123]'
        `json`[%a ~[[%n '123']]]
    ==
    :*  "y_number_simple_real"
        '[123.456789]'
        `json`[%a ~[[%n '123.456789']]]
    ==
    :*  "y_object"
        '{"asd":"sdf", "dfg":"fgh"}'
        `json`(pairs ~[['asd' [%s 'sdf']] ['dfg' [%s ['fgh']]]])
    ==
    :*  "y_object_basic"
        '{"asd":"sdf"}'
        `json`(frond ['asd' [%s 'sdf']])
    ==
    ::  duplicated keys, it takes the latest one.
    :*  "y_object_duplicated_key"
        '{"a":"b","a":"c"}'
        `json`(frond ['a' [%s 'c']])
    ==
    :*  "y_object_duplicated_key_and_value"
        '{"a":"b","a":"b"}'
        `json`(frond ['a' [%s 'b']])
    ==
    :*  "y_object_empty"
        '{}'
        `json`[%o ~]
    ==
    :*  "y_object_empty_key"
        '{"":0}'
        `json`(frond ['' [%n '0']])
    ==
    :*  "y_string_escaped_null"
        '"foo\\u0000bar"'
        `json`[%s 'foo\00bar']
    ==
    :*  "y_object_escaped_null_in_key"
        '{"foo\\u0000bar": 42}'
        `json`(frond ['foo\00bar' [%n '42']])
    ==
    :*  "y_object_extreme_numbers"
        '{ "min": -1.0e+28, "max": 1.0e+28 }'
        `json`(pairs ~[['min' [%n '-1.0e+28']] ['max' [%n '1.0e+28']]])
    ==
    :*  "y_object_long_strings"
        '{"x":[{"id": "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"}], "id": "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"}'
        `json`(pairs ~[['id' [%s 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx']] ['x' [%a ~[(frond ['id' [%s 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx']])]]]])
    ==
    :*  "y_object_simple"
        '{"a":[]}'
        `json`(frond 'a' [%a ~])
    ==
    :*  "y_object_string_unicode"
        '{"title":"\\u041f\\u043e\\u043b\\u0442\\u043e\\u0440\\u0430 \\u0417\\u0435\\u043c\\u043b\\u0435\\u043a\\u043e\\u043f\\u0430" }'
        `json`(frond 'title' [%s 'Полтора Землекопа'])
    ==
    :*  "y_object_with_newlines"
        '{\0a"a": "b"\0a}'
        `json`(frond 'a' [%s 'b'])
    ==
    :*  "y_string_1_2_3_bytes_UTF-8_sequences"
        '["\\u0060\\u012a\\u12AB"]'
        `json`[%a ~[[%s '`Īካ']]]
    ==
    :*  "y_string_accepted_surrogate_pair"
        '["\\uD801\\udc37"]'
        `json`[%a ~[[%s '𐐷']]]
    ==
    :*  "y_string_accepted_surrogate_pairs"
        '["\\ud83d\\ude39\\ud83d\\udc8d"]'
        `json`[%a ~[[%s '😹💍']]]
    ==
    :*  "y_string_allowed_escapes"
        '["\\"\\\\\\/\\b\\f\\n\\r\\t"]'
        `json`[%a ~[[%s '"\\/\08\0c\0a\0d\09']]]
    ==
    :*  "y_string_backslash_and_u_escaped_zero"
        '["\\\\u0000"]'
        `json`[%a ~[[%s '\\u0000']]]
    ==
    :*  "y_string_backslash_doublequotes"
        '["\\""]'
        `json`[%a ~[[%s '"']]]
    ==
    :*  "y_string_comments"
        '["a/*b*/c/*d//e"]'
        `json`[%a ~[[%s 'a/*b*/c/*d//e']]]
    ==
    :*  "y_string_double_escape_a"
        '["\\\\a"]'
        `json`[%a ~[[%s '\\a']]]
    ==
    :*  "y_string_double_escape_n"
        '["\\\\n"]'
        `json`[%a ~[[%s '\\n']]]
    ==
    :*  "y_string_escaped_control_character"
        '["\\u0012"]'
        `json`[%a ~[[%s '\12']]]
    ==
    :*  "y_string_escaped_noncharacter"
        '["\\uFFFF"]'
        `json`[%a ~[[%s (from-code-point 0xffff)]]]
    ==
    :*  "y_string_in_array_with_leading_space"
        '[ "asd"]'
        `json`[%a ~[[%s 'asd']]]
    ==
    :*  "y_string_last_surrogates_1_and_2"
        '["\\uDBFF\\uDFFF"]'
        `json`[%a ~[[%s (crip (from-code-points ~[0xdbff 0xdfff]))]]]
    ==
    :*  "y_string_nbsp_uescaped"
        '["new\\u00A0line"]'
        `json`[%a ~[[%s (crip "new{(from-code-points ~[0xa0])}line")]]]
    ==
    :*  "y_string_nonCharacterInUTF-8_U+10FFFF"
        '["􏿿"]'
        `json`[%a ~[[%s (from-code-point 0x10.ffff)]]]
    ==
    :*  "y_string_nonCharacterInUTF-8_U+FFFF"
        '["￿"]'
        `json`[%a ~[[%s (from-code-point 0xffff)]]]
    ==
    :*  "y_string_null_escape"
        '["\\u0000"]'
        `json`[%a ~[[%s '\00']]]
    ==
    :*  "y_string_one-byte-utf-8"
        '["\\u002c"]'
        `json`[%a ~[[%s '\2c']]]
    ==
    :*  "y_string_pi"
        '["π"]'
        `json`[%a ~[[%s 'π']]]
    ==
    :*  "y_string_reservedCharacterInUTF-8_U+1BFFF"
        '["𛿿"]'
        `json`[%a ~[[%s (from-code-point 0x1.bfff)]]]
    ==
    :*  "y_string_simple_ascii"
        '["asd "]'
        `json`[%a ~[[%s 'asd ']]]
    ==
    :*  "y_string_space"
        '" "'
        `json`[%s ' ']
    ==
    :*  "y_string_surrogates_U+1D11E_MUSICAL_SYMBOL_G_CLEF"
        '["\\uD834\\uDd1e"]'
        `json`[%a ~[[%s (crip (from-code-points ~[0xd834 0xdd1e]))]]]
    ==
    :*  "y_string_three-byte-utf-8"
        '["\\u0821"]'
        `json`[%a ~[[%s (from-code-point 0x821)]]]
    ==
    :*  "y_string_two-byte-utf-8"
        '["\\u0123"]'
        `json`[%a ~[[%s (from-code-point 0x123)]]]
    ==
    :*  "y_string_u+2028_line_sep"
        '[" "]'
        `json`[%a ~[[%s (from-code-point 0x2028)]]]
    ==
    :*  "y_string_u+2029_par_sep"
        '[" "]'
        `json`[%a ~[[%s (from-code-point 0x2029)]]]
    ==
    :*  "y_string_uEscape"
        '["\\u0061\\u30af\\u30EA\\u30b9"]'
        `json`[%a ~[[%s (crip (from-code-points ~[0x61 0x30af 0x30ea 0x30b9]))]]]
    ==
    :*  "y_string_uescaped_newline"
        '["new\\u000Aline"]'
        `json`[%a ~[[%s 'new\0aline']]]
    ==
    :*  "y_string_unescaped_char_delete"
        '["\7f"]'
        `json`[%a ~[[%s '\7f']]]
    ==
    :*  "y_string_unicode"
        '["\\uA66D"]'
        `json`[%a ~[[%s (from-code-point 0xa66d)]]]
    ==
    :*  "y_string_unicodeEscapedBackslash"
        '["\\u005C"]'
        `json`[%a ~[[%s (from-code-point 0x5c)]]]
    ==
    :*  "y_string_unicode_2"
        '["⍂㈴⍂"]'
        `json`[%a ~[[%s '⍂㈴⍂']]]
    ==
    :*  "y_string_unicode_U+10FFFE_nonchar"
        '["\\uDBFF\\uDFFE"]'
        `json`[%a ~[[%s (crip (from-code-points ~[0xdbff 0xdffe]))]]]
    ==
    :*  "y_string_unicode_U+1FFFE_nonchar"
        '["\\uD83F\\uDFFE"]'
        `json`[%a ~[[%s (crip (from-code-points ~[0xd83f 0xdffe]))]]]
    ==
    :*  "y_string_unicode_U+200B_ZERO_WIDTH_SPACE"
        '["\\u200B"]'
        `json`[%a ~[[%s (from-code-point 0x200b)]]]
    ==
    :*  "y_string_unicode_U+2064_invisible_plus"
        '["\\u2064"]'
        `json`[%a ~[[%s (from-code-point 0x2064)]]]
    ==
    :*  "y_string_unicode_U+FDD0_nonchar"
        '["\\uFDD0"]'
        `json`[%a ~[[%s (from-code-point 0xfdd0)]]]
    ==
    :*  "y_string_unicode_U+FFFE_nonchar"
        '["\\uFFFE"]'
        `json`[%a ~[[%s (from-code-point 0xfffe)]]]
    ==
    :*  "y_string_unicode_escaped_double_quote"
        '["\\u0022"]'
        `json`[%a ~[[%s (from-code-point 0x22)]]]
    ==
    :*  "y_string_utf8"
        '["€𝄞"]'
        `json`[%a ~[[%s '€𝄞']]]
    ==
    :*  "y_string_with_del_character"
        '["a\7fa"]'
        `json`[%a ~[[%s 'a\7fa']]]
    ==

    :*  "y_structure_lonely_false"
        'false'
        `json`[%b |]
    ==
    :*  "y_structure_lonely_int"
        '42'
        `json`[%n '42']
    ==
    :*  "y_structure_lonely_negative_real"
        '-0.1'
        `json`[%n '-0.1']
    ==
    :*  "y_structure_lonely_null"
        'null'
        `json`~
    ==
    :*  "y_structure_lonely_string"
        '"asd"'
        `json`[%s 'asd']
    ==
    :*  "y_structure_lonely_true"
        'true'
        `json`[%b &]
    ==
    :*  "y_structure_string_empty"
        '""'
        `json`[%s '']
    ==
    :*  "y_structure_trailing_newline"
        '["a"]\0a'
        `json`[%a ~[[%s 'a']]]
    ==
    :*  "y_structure_true_in_array"
        '[true]'
        `json`[%a ~[[%b &]]]
    ==
    :*  "y_structure_whitespace_array"
        ' [] '
        `json`[%a ~]
    ==
  ==
--
