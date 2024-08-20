::  Tests for html
::
/+  *test
=,  html
=,  enjs:format
|%
::  JSON encoding/decoding
::
::  Functions for checking large lists of examples against expected values which
::  also nicely format any failures.
::
::  Custom types need to skip one 'tape' face due to 'tape' arm in 'enjs:format'
::
+$  json-decode-spec  [name=^tape input=cord expected=^json]
+$  json-decode-rejection-spec  [input=cord name=^tape]
+$  json-encode-spec  [name=^tape input=^json expected=cord]
::
++  run-decode-specs
  ::  legend tells of a man who made the Kessel run in less than 12
  ::  parse-specs...
  |=  specs=(list json-decode-spec)
  %-  zing
  %-  turn
  :-  specs
  |=  spec=json-decode-spec
  ^-  tang
  =+  result=(expect-eq !>(`expected.spec) !>((de:json input.spec)))
  ?~  result  ~
  `tang`[[%leaf "in {name.spec}:"] result]
::
++  run-decode-rejection-specs
  |=  specs=(list json-decode-rejection-spec)
  %-  zing
  %-  turn
  :-  specs
  |=  spec=json-decode-rejection-spec
  ^-  tang
  =+  result=(expect-eq !>(~) !>((de:json input.spec)))
  ?~  result  ~
  `tang`[[%leaf "in {name.spec}:"] result]
++  run-encode-specs
  |=  specs=(list json-encode-spec)
  %-  zing
  %-  turn
  :-  specs
  |=  spec=json-encode-spec
  ^-  tang
  =+  result=(expect-eq !>(expected.spec) !>((en:json input.spec)))
  ?~  result  ~
  `tang`[[%leaf "in {name.spec}:"] result]
::  JSON decoding test suite
::
::  The following test suite is adapted from
::  https://github.com/nst/JSONTestSuite/ (Copyright (c) 2016 Nicolas Seriot)
::  under the terms of the MIT license. It influenced the following tests:
::    - test-de-json-invalid-input
::    - test-de-json-valid-input
::    - test-de-json-invalid-ambiguous-input
::    - test-de-json-valid-ambiguous-input
::  Note: The tests below are not 1:1 with the "JSONTestSuite"; tests have been
::        added, dropped, edited, and renamed.
::
::  These are all inputs that should be rejected by a valid JSON parser.
::
++  test-de-json-invalid-input
  %-  run-decode-rejection-specs
  :~
    ['["x", truth]' "n_array_bad_value"]
    ['["": 1]' "n_array_colon_instead_of_comma"]
    ['[""],' "n_array_comma_after_close"]
    ['[1 true]' "n_array_comma_missing"]
    ['[,]' "n_array_comma_only"]
    ['[1,,2]' "n_array_double_comma"]
    ['[,"x"]' "n_array_early_comma"]
    ['[\\f]' "n_array__escape_outside_string"]
    ['["x"]]' "n_array_extra_close"]
    ['["x",]' "n_array_extra_comma"]
    ['["x"' "n_array_incomplete"]
    ['[\FF]' "n_array_invalid_UTF8"]
    ['[\0B]' "n_array_invalid_whitespace"]
    ['+1' "n_number_+1"]
    ['+Inf' "n_number_+Inf"]
    ['00' "n_number_double_zero"]
    ['00e1' "n_number_double_zero_exponent"]
    ['-00e1' "n_number_double_zero_exponent_neg"]
    ['00.0' "n_number_double_zero_fraction"]
    ['-00.0' "n_number_double_zero_fraction_neg"]
    ['-00' "n_number_double_zero_neg"]
    ['1+2' "n_number_expression"]
    ['1e+-2' "n_number_exponent_+-"]
    ['1e-+2' "n_number_exponent_+-_reverse"]
    ['1ee2' "n_number_exponent_extra_e"]
    ['1eE2' "n_number_exponent_extra_E"]
    ['1EE2' "n_number_exponent_extra_E_double"]
    ['1Ee2' "n_number_exponent_extra_E_reverse"]
    ['1.2.' "n_number_extra_decimal_with_digit"]
    ['-1.2.' "n_number_extra_decimal_with_digit_neg"]
    ['1.2.3' "n_number_extra_decimal_without_digit"]
    ['-1.2.3' "n_number_extra_decimal_without_digit_neg"]
    ['0x1' "n_number_hex_1_digit"]
    ['0x42' "n_number_hex_2_digits"]
    ['01' "n_number_leading_zero"]
    ['-01' "n_number_leading_zero_neg"]
    ['.-1' "n_number_minus_after_decimal"]
    ['1e' "n_number_missing_exponent_digits"]
    ['-1e' "n_number_missing_exponent_digits_neg"]
    ['1E' "n_number_missing_exponent_digits_E"]
    ['-1E' "n_number_missing_exponent_digits_E_neg"]
    ['1.' "n_number_missing_real_fractional_digits"]
    ['-1.' "n_number_missing_real_fractional_digits_neg"]
    ['.1' "n_number_missing_real_integer_digits"]
    ['-.1' "n_number_missing_real_integer_digits_neg"]
    ['\EF\BC\91' "n_number_unicode_digit"]
    ['12 34' "n_number_space_between_digits"]
    ['- 1' "n_number_space_between_minus"]
    ['Inf' "n_number_text_based_value_Inf"]
    ['-Inf' "n_number_text_based_value_Inf_neg"]
    ['Infinity' "n_number_text_based_value_Infinity"]
    ['-Infinity' "n_number_text_based_value_Infinity_neg"]
    ['NaN' "n_number_text_based_value_NaN"]
    ['-NaN' "n_number_text_based_value_NaN_neg"]
    ['[1e1\E5]' "n_number_UTF8_in_exponent"]
    ['[1.1\E5]' "n_number_UTF8_in_fraction"]
    ['[1\E5]' "n_number_UTF8_in_int"]
    ['[1.2a-3]' "n_number_with_alpha_lower"]
    ['[1.8011670033376514H-308]' "n_number_with_alpha_upper"]
    ['{"a","b"}' "n_object_comma_instead_of_colon"]
    ['{"x":/*comment*/"b"}' "n_object_comment"]
    ['{"x"::"b"}' "n_object_extra_colon"]
    ['{"x":"b",,"y":"z"}' "n_object_extra_comma"]
    ['{[: "x"}' "n_object_key_bracket"]
    ['{🇨🇭: 0}' "n_object_key_emoji"]
    ['{key: "value"}' "n_object_key_no_quotes"]
    ['{1: "value"}' "n_object_key_not_string"]
    ['{null: "value"}' "n_object_key_null"]
    ['{\'key\': "value"}' "n_object_key_single_quotes"]
    ['{"a" "b"}' "n_object_missing_colon"]
    ['{:"b"}' "n_object_missing_key"]
    ['{"a":}' "n_object_missing_value"]
    ['{"a"}' "n_object_missing_value_and_colon"]
    ['{"foo":"bar", "a" "b"}' "n_object_second_item_missing_colon"]
    ['{"foo":"bar", : "b"}' "n_object_second_item_missing_key"]
    ['{"foo":"bar", "a" :}' "n_object_second_item_missing_value"]
    ['{"foo":"bar", "a"}' "n_object_second_item_missing_value_and_colon"]
    ['{"id":0,}' "n_object_trailing_comma"]
    ['{"a": "b"' "n_object_unclosed"]
    ['{"a":"a' "n_object_unterminated_value"]
    ['"\FF"' "n_string_EOF"]
    ['"\\\\\\"' "n_string_escaped_backslash_bad"]
    ['"\\🌀"' "n_string_escaped_emoji"]
    ['"\\00"' "n_string_escaped_UTF8"]
    ['"' "n_string_incomplete"]
    ['"\\"' "n_string_incomplete_escape"]
    ['"\\u00A"' "n_string_incomplete_first_surrogate"]
    ['"\\a"' "n_string_invalid_escape"]
    ['"\\uqqqq"' "n_string_invalid_escaped_unicode"]
    ['"\\\E5"' "n_string_invalid_UTF8_after_escape"]
    ['"\\u000\E5"' "n_string_invalid_UTF8_in_escape"]
    ['"\\uD834\\uDd"' "n_string_incomplete_second_surrogate"]
    ['"\\uD800\\"' "n_string_one_surrogate_then_escape"]
    ['"\\uD800\\u"' "n_string_one_surrogate_then_escape_u"]
    ['"\\uD800\\u1"' "n_string_one_surrogate_then_escape_u1"]
    ['"\\uD800\\u1x"' "n_string_one_surrogate_then_escape_u1x"]
    ['abc' "n_string_no_quotes"]
    ['\'single quote\'' "n_string_single_quotes"]
    ['"a\00a"' "n_string_unescaped_ctrl_char"]
    ['"new\0aline"' "n_string_unescaped_newline"]
    ['"\09"' "n_string_unescaped_tab"]
    ['"abc' "n_string_unclosed"]
    ['"\\UA66D"' "n_string_UTF16_capital_u"]
    ['<>' "n_value_angle_brackets"]
    ['False' "n_value_capitalized_false"]
    ['Null' "n_value_capitalized_null"]
    ['True' "n_value_capitalized_true"]
    ['[][]' "n_value_double_array"]
    ['{}{}' "n_value_double_object"]
    ['1]' "n_value_close_unopened_array"]
    ['1}' "n_value_close_unopened_object"]
    ['\\n"asd"' "n_value_escape_outside_string"]
    ['fals' "n_value_incomplete_false"]
    ['nul' "n_value_incomplete_null"]
    ['tru' "n_value_incomplete_true"]
    ['\EF\BB{}' "n_value_incomplete_UTF8_BOM"]
    ['\0C[]' "n_value_invalid_whitespace"]
    ['a' "n_value_lone_character"]
    ['\E5' "n_value_lone_invalid_UTF8"]
    ['-' "n_value_lone_minus"]
    ['*' "n_value_lone_star"]
    [' ' "n_value_lone_whitespace"]
    ['1 "a" true' "n_value_multiple_values"]
    ['' "n_value_no_data"]
    ['\00[]' "n_value_null_byte_outside_string"]
    ['{"a":"b"}/**/' "n_value_trailing_comment"]
    ['{"a":"b"}//' "n_value_trailing_comment_slash_open"]
    ['[1]#' "n_value_trailing_garbage_array"]
    ['false#' "n_value_trailing_garbage_boolean_false"]
    ['true#' "n_value_trailing_garbage_boolean_true"]
    ['-#' "n_value_trailing_garbage_minus"]
    ['1#' "n_value_trailing_garbage_number"]
    ['-1#' "n_value_trailing_garbage_number_neg"]
    ['1.0#' "n_value_trailing_garbage_number_real"]
    ['1.0e2#' "n_value_trailing_garbage_number_real_exp"]
    ['-1.0e2#' "n_value_trailing_garbage_number_real_exp_neg"]
    ['-1.0#' "n_value_trailing_garbage_number_real_neg"]
    ['null#' "n_value_trailing_garbage_null"]
    ['{"a":"b"}#' "n_value_trailing_garbage_object"]
    ['{"a":#"a"}' "n_value_trailing_garbage_object_colon"]
    ['{"a"#:"a"}' "n_value_trailing_garbage_object_key"]
    ['{"a":"a"#}' "n_value_trailing_garbage_object_value"]
    ['"a"x' "n_value_trailing_garbage_string"]
    ['[\E2\81\A0]' "n_value_U+2060_word_joined_array"]
    ['{\E2\81\A0}' "n_value_U+2060_word_joined_object"]
    ['\\u0020"asd"' "n_value_UTF16_outside_string"]
    ['\EF\BB\BF' "n_value_UTF8_BOM_no_data"]
  ==
::  These are all inputs that should be accepted by a valid JSON parser.
::
++  test-de-json-valid-input
  %-  run-decode-specs
  :~
    :*  "y_array_empty"
        '[]'
        [%a ~]
    ==
    :*  "y_array_empty_whitespace"
        '[ \09\0A\0D]'
        [%a ~]
    ==
    :*  "y_array_val_array"
        '[[]]'
        [%a ~[[%a ~]]]
    ==
    :*  "y_array_val_boolean_false"
        '[false]'
        [%a ~[[%b |]]]
    ==
    :*  "y_array_val_boolean_true"
        '[true]'
        [%a ~[[%b &]]]
    ==
    :*  "y_array_val_null"
        '[null]'
        [%a ~[~]]
    ==
    :*  "y_array_val_num"
        '[123]'
        [%a ~[[%n '123']]]
    ==
    :*  "y_array_val_object"
        '[{}]'
        [%a ~[[%o ~]]]
    ==
    :*  "y_array_val_string"
        '["abc"]'
        [%a ~[[%s 'abc']]]
    ==
    :*  "y_array_vals_heterogeneous"
        '[null,1,"1",{},[]]'
        [%a ~[~ [%n '1'] [%s '1'] [%o ~] [%a ~]]]
    ==
    :*  "y_array_vals_homogeneous"
        '[null,null,null,null]'
        [%a ~[~ ~ ~ ~]]
    ==
    :*  "y_array_vals_homogeneous_whitespace"
        '[ null , null , null , null ]'
        [%a ~[~ ~ ~ ~]]
    ==
    :*  "y_number_0"
        '0'
        [%n '0']
    ==
    :*  "y_number_0_neg"
        '-0'
        [%n '-0']
    ==
    :*  "y_number_0e0"
        '0e0'
        [%n '0e0']
    ==
    :*  "y_number_0e0_neg"
        '-0e0'
        [%n '-0e0']
    ==
    :*  "y_number_0e000"
        '0e000'
        [%n '0e000']
    ==
    :*  "y_number_0e000_neg"
        '-0e000'
        [%n '-0e000']
    ==
    :*  "y_number_0e1"
        '0e1'
        [%n '0e1']
    ==
    :*  "y_number_0e1_neg"
        '-0e1'
        [%n '-0e1']
    ==
    :*  "y_number_0e+0"
        '0e+0'
        [%n '0e+0']
    ==
    :*  "y_number_0e+0_neg"
        '-0e+0'
        [%n '-0e+0']
    ==
    :*  "y_number_0e+000"
        '0e+000'
        [%n '0e+000']
    ==
    :*  "y_number_0e+000_neg"
        '-0e+000'
        [%n '-0e+000']
    ==
    :*  "y_number_0e+1"
        '0e+1'
        [%n '0e+1']
    ==
    :*  "y_number_0e+1_neg"
        '-0e+1'
        [%n '-0e+1']
    ==
    :*  "y_number_0e-0"
        '0e-0'
        [%n '0e-0']
    ==
    :*  "y_number_0e-0_neg"
        '-0e-0'
        [%n '-0e-0']
    ==
    :*  "y_number_0e-000"
        '0e-000'
        [%n '0e-000']
    ==
    :*  "y_number_0e-000_neg"
        '-0e-000'
        [%n '-0e-000']
    ==
    :*  "y_number_0e-1"
        '0e-1'
        [%n '0e-1']
    ==
    :*  "y_number_0e-1_neg"
        '-0e-1'
        [%n '-0e-1']
    ==
    :*  "y_number_0E0"
        '0E0'
        [%n '0E0']
    ==
    :*  "y_number_0E0_neg"
        '-0E0'
        [%n '-0E0']
    ==
    :*  "y_number_0E000"
        '0E000'
        [%n '0E000']
    ==
    :*  "y_number_0E000_neg"
        '-0E000'
        [%n '-0E000']
    ==
    :*  "y_number_0E1"
        '0E1'
        [%n '0E1']
    ==
    :*  "y_number_0E1_neg"
        '-0E1'
        [%n '-0E1']
    ==
    :*  "y_number_0E+0"
        '0E+0'
        [%n '0E+0']
    ==
    :*  "y_number_0E+0_neg"
        '-0E+0'
        [%n '-0E+0']
    ==
    :*  "y_number_0E+000"
        '0E+000'
        [%n '0E+000']
    ==
    :*  "y_number_0E+000_neg"
        '-0E+000'
        [%n '-0E+000']
    ==
    :*  "y_number_0E+1"
        '0E+1'
        [%n '0E+1']
    ==
    :*  "y_number_0E+1_neg"
        '-0E+1'
        [%n '-0E+1']
    ==
    :*  "y_number_0E-0"
        '0E-0'
        [%n '0E-0']
    ==
    :*  "y_number_0E-0_neg"
        '-0E-0'
        [%n '-0E-0']
    ==
    :*  "y_number_0E-000"
        '0E-000'
        [%n '0E-000']
    ==
    :*  "y_number_0E-000_neg"
        '-0E-000'
        [%n '-0E-000']
    ==
    :*  "y_number_0E-1"
        '0E-1'
        [%n '0E-1']
    ==
    :*  "y_number_0E-1_neg"
        '-0E-1'
        [%n '-0E-1']
    ==
    :*  "y_number_0.0"
        '0.0'
        [%n '0.0']
    ==
    :*  "y_number_0.0_neg"
        '-0.0'
        [%n '-0.0']
    ==
    :*  "y_number_0.0e0"
        '0.0e0'
        [%n '0.0e0']
    ==
    :*  "y_number_0.0e0_neg"
        '-0.0e0'
        [%n '-0.0e0']
    ==
    :*  "y_number_0.0e000"
        '0.0e000'
        [%n '0.0e000']
    ==
    :*  "y_number_0.0e000_neg"
        '-0.0e000'
        [%n '-0.0e000']
    ==
    :*  "y_number_0.0e1"
        '0.0e1'
        [%n '0.0e1']
    ==
    :*  "y_number_0.0e1_neg"
        '-0.0e1'
        [%n '-0.0e1']
    ==
    :*  "y_number_0.0e+0"
        '0.0e+0'
        [%n '0.0e+0']
    ==
    :*  "y_number_0.0e+0_neg"
        '-0.0e+0'
        [%n '-0.0e+0']
    ==
    :*  "y_number_0.0e+000"
        '0.0e+000'
        [%n '0.0e+000']
    ==
    :*  "y_number_0.0e+000_neg"
        '-0.0e+000'
        [%n '-0.0e+000']
    ==
    :*  "y_number_0.0e+1"
        '0.0e+1'
        [%n '0.0e+1']
    ==
    :*  "y_number_0.0e+1_neg"
        '-0.0e+1'
        [%n '-0.0e+1']
    ==
    :*  "y_number_0.0e-0"
        '0.0e-0'
        [%n '0.0e-0']
    ==
    :*  "y_number_0.0e-0_neg"
        '-0.0e-0'
        [%n '-0.0e-0']
    ==
    :*  "y_number_0.0e-000"
        '0.0e-000'
        [%n '0.0e-000']
    ==
    :*  "y_number_0e-000_neg"
        '-0.0e-000'
        [%n '-0.0e-000']
    ==
    :*  "y_number_0.0e-1"
        '0.0e-1'
        [%n '0.0e-1']
    ==
    :*  "y_number_0.0e-1_neg"
        '-0.0e-1'
        [%n '-0.0e-1']
    ==
    :*  "y_number_0.0E0"
        '0.0E0'
        [%n '0.0E0']
    ==
    :*  "y_number_0.0E0_neg"
        '-0.0E0'
        [%n '-0.0E0']
    ==
    :*  "y_number_0.0E000"
        '0.0E000'
        [%n '0.0E000']
    ==
    :*  "y_number_0E000_neg"
        '-0.0E000'
        [%n '-0.0E000']
    ==
    :*  "y_number_0.0E1"
        '0.0E1'
        [%n '0.0E1']
    ==
    :*  "y_number_0.0E1_neg"
        '-0.0E1'
        [%n '-0.0E1']
    ==
    :*  "y_number_0.0E+0"
        '0.0E+0'
        [%n '0.0E+0']
    ==
    :*  "y_number_0.0E+0_neg"
        '-0.0E+0'
        [%n '-0.0E+0']
    ==
    :*  "y_number_0.0E+000"
        '0.0E+000'
        [%n '0.0E+000']
    ==
    :*  "y_number_0E+000_neg"
        '-0.0E+000'
        [%n '-0.0E+000']
    ==
    :*  "y_number_0.0E+1"
        '0.0E+1'
        [%n '0.0E+1']
    ==
    :*  "y_number_0.0E+1_neg"
        '-0.0E+1'
        [%n '-0.0E+1']
    ==
    :*  "y_number_0.0E-0"
        '0.0E-0'
        [%n '0.0E-0']
    ==
    :*  "y_number_0.0E-0_neg"
        '-0.0E-0'
        [%n '-0.0E-0']
    ==
    :*  "y_number_0.0E-000"
        '0.0E-000'
        [%n '0.0E-000']
    ==
    :*  "y_number_0E-000_neg"
        '-0.0E-000'
        [%n '-0.0E-000']
    ==
    :*  "y_number_0.0E-1"
        '0.0E-1'
        [%n '0.0E-1']
    ==
    :*  "y_number_0.0E-1_neg"
        '-0.0E-1'
        [%n '-0.0E-1']
    ==
    :*  "y_number_1"
        '1'
        [%n '1']
    ==
    :*  "y_number_1_neg"
        '-1'
        [%n '-1']
    ==
    :*  "y_number_1e1"
        '1e1'
        [%n '1e1']
    ==
    :*  "y_number_1e1_neg"
        '-1e1'
        [%n '-1e1']
    ==
    :*  "y_number_1e+1"
        '1e+1'
        [%n '1e+1']
    ==
    :*  "y_number_1e+1_neg"
        '-1e+1'
        [%n '-1e+1']
    ==
    :*  "y_number_1e-1"
        '1e-1'
        [%n '1e-1']
    ==
    :*  "y_number_1e-1_neg"
        '-1e-1'
        [%n '-1e-1']
    ==
    :*  "y_number_1E1"
        '1E1'
        [%n '1E1']
    ==
    :*  "y_number_1E1_neg"
        '-1E1'
        [%n '-1E1']
    ==
    :*  "y_number_1E+1"
        '1E+1'
        [%n '1E+1']
    ==
    :*  "y_number_1E+1_neg"
        '-1E+1'
        [%n '-1E+1']
    ==
    :*  "y_number_1E-1"
        '1E-1'
        [%n '1E-1']
    ==
    :*  "y_number_1E-1_neg"
        '-1E-1'
        [%n '-1E-1']
    ==
    :*  "y_number_1.0"
        '1.0'
        [%n '1.0']
    ==
    :*  "y_number_1.1"
        '1.1'
        [%n '1.1']
    ==
    :*  "y_number_1.1_neg"
        '-1.1'
        [%n '-1.1']
    ==
    :*  "y_number_1.1e1"
        '1.1e1'
        [%n '1.1e1']
    ==
    :*  "y_number_1.1e1_neg"
        '-1.1e1'
        [%n '-1.1e1']
    ==
    :*  "y_number_1.1e+1"
        '1.1e+1'
        [%n '1.1e+1']
    ==
    :*  "y_number_1.1e+1_neg"
        '-1.1e+1'
        [%n '-1.1e+1']
    ==
    :*  "y_number_1.1e-1"
        '1.1e-1'
        [%n '1.1e-1']
    ==
    :*  "y_number_1.1e-1_neg"
        '-1.1e-1'
        [%n '-1.1e-1']
    ==
    :*  "y_number_1.1E1"
        '1.1E1'
        [%n '1.1E1']
    ==
    :*  "y_number_1.1E1_neg"
        '-1.1E1'
        [%n '-1.1E1']
    ==
    :*  "y_number_1.1E+1"
        '1.1E+1'
        [%n '1.1E+1']
    ==
    :*  "y_number_1.1E+1_neg"
        '-1.1E+1'
        [%n '-1.1E+1']
    ==
    :*  "y_number_1.1E-1"
        '1.1E-1'
        [%n '1.1E-1']
    ==
    :*  "y_number_1.1E-1_neg"
        '-1.1E-1'
        [%n '-1.1E-1']
    ==
    :*  "y_number_int_exp_e"
        '123e7'
        [%n '123e7']
    ==
    :*  "y_number_int_exp_e_neg"
        '-123e7'
        [%n '-123e7']
    ==
    :*  "y_number_int_exp_e+"
        '123e+7'
        [%n '123e+7']
    ==
    :*  "y_number_int_exp_e+_neg"
        '-123e+7'
        [%n '-123e+7']
    ==
    :*  "y_number_int_exp_e-"
        '123e-7'
        [%n '123e-7']
    ==
    :*  "y_number_int_exp_e-_neg"
        '-123e-7'
        [%n '-123e-7']
    ==
    :*  "y_number_int_exp_E"
        '123E7'
        [%n '123E7']
    ==
    :*  "y_number_int_exp_E_neg"
        '-123E7'
        [%n '-123E7']
    ==
    :*  "y_number_int_exp_E+"
        '123E+7'
        [%n '123E+7']
    ==
    :*  "y_number_int_exp_E+_neg"
        '-123E+7'
        [%n '-123E+7']
    ==
    :*  "y_number_int_exp_E-"
        '123E-7'
        [%n '123E-7']
    ==
    :*  "y_number_int_exp_E-_neg"
        '-123E-7'
        [%n '-123E-7']
    ==
    :*  "y_number_int_simple"
        '123'
        [%n '123']
    ==
    :*  "y_number_int_simple_neg"
        '-123'
        [%n '-123']
    ==
    :*  "y_number_real_exp_e"
        '123.456e7'
        [%n '123.456e7']
    ==
    :*  "y_number_real_exp_e_neg"
        '-123.456e7'
        [%n '-123.456e7']
    ==
    :*  "y_number_real_exp_e+"
        '123.456e+7'
        [%n '123.456e+7']
    ==
    :*  "y_number_real_exp_e+_neg"
        '-123.456e+7'
        [%n '-123.456e+7']
    ==
    :*  "y_number_real_exp_e-"
        '123.456e-7'
        [%n '123.456e-7']
    ==
    :*  "y_number_real_exp_e-_neg"
        '-123.456e-7'
        [%n '-123.456e-7']
    ==
    :*  "y_number_real_exp_E"
        '123.456E7'
        [%n '123.456E7']
    ==
    :*  "y_number_real_exp_E_neg"
        '-123.456E7'
        [%n '-123.456E7']
    ==
    :*  "y_number_real_exp_E+"
        '123.456E+7'
        [%n '123.456E+7']
    ==
    :*  "y_number_real_exp_E+_neg"
        '-123.456E+7'
        [%n '-123.456E+7']
    ==
    :*  "y_number_real_exp_E-"
        '123.456E-7'
        [%n '123.456E-7']
    ==
    :*  "y_number_real_exp_E-_neg"
        '-123.456E-7'
        [%n '-123.456E-7']
    ==
    :*  "y_number_real_simple"
        '123.456'
        [%n '123.456']
    ==
    :*  "y_number_real_simple_neg"
        '-123.456'
        [%n '-123.456']
    ==
    :*  "y_number_very_close_to_zero"
        '-0.0000000000000000000000000000000000000000000000000000000\
        /00000000000000000000001'
        [%n '-0.0000000000000000000000000000000000000000000000\
        /00000000000000000000000000000001']
    ==
    :*  "y_object_duplicated_key"
        '{"a":"b","a":"c"}'
        (frond ['a' [%s 'c']])
    ==
    :*  "y_object_duplicated_key_and_value"
        '{"a":"b","a":"b"}'
        (frond ['a' [%s 'b']])
    ==
    :*  "y_object_duplicated_key_reverse"
        '{"a":"c","a":"b"}'
        (frond ['a' [%s 'b']])
    ==
    :*  "y_object_empty"
        '{}'
        [%o ~]
    ==
    :*  "y_object_key_empty"
        '{"":0}'
        (frond ['' [%n '0']])
    ==
    :*  "y_object_empty_whitespace"
        '{ \09\0A\0D}'
        [%o ~]
    ==
    :*  "y_object_val_array"
        '{"x": []}'
        (frond ['x' [%a ~]])
    ==
    :*  "y_object_val_boolean_false"
        '{"x": false}'
        (frond ['x' [%b |]])
    ==
    :*  "y_object_val_boolean_true"
        '{"x": true}'
        (frond ['x' [%b &]])
    ==
    :*  "y_object_val_null"
        '{"x": null}'
        (frond ['x' ~])
    ==
    :*  "y_object_val_num"
        '{"x": 123}'
        (frond ['x' [%n '123']])
    ==
    :*  "y_object_val_object"
        '{"x": {}}'
        (frond ['x' [%o ~]])
    ==
    :*  "y_object_val_string"
        '{"x": "y"}'
        (frond ['x' [%s 'y']])
    ==
    =/  long=@t  'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
    :*  "y_object_val_string_long"
        '{"id1": "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx", \
        /"id2": "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"}'
        (pairs ~[['id1' [%s long]] ['id2' [%s long]]])
    ==
    :*  "y_object_val_string_unicode"
        '{"title":"\\u041f\\u043e\\u043b\\u0442\\u043e\\u0440\\u0430 \
        /\\u0417\\u0435\\u043c\\u043b\\u0435\\u043a\\u043e\\u043f\\u0430" }'
        (frond 'title' [%s 'Полтора Землекопа'])
    ==
    :*  "y_object_vals_heterogeneous"
        '{"a":null,"b":1,"c":"1","d":[],"e":{}}'
        (pairs ~[['a' ~] ['b' [%n '1']] ['c' [%s '1']] ['d' [%a ~]] ['e' [%o ~]]])
    ==
    :*  "y_object_vals_homogeneous"
        '{"a":null,"b":null,"c":null,"d":null,"e":null}'
        (pairs ~[['a' ~] ['b' ~] ['c' ~] ['d' ~] ['e' ~]])
    ==
    :*  "y_object_vals_homogeneous_whitespace"
        '{ "a"  : null , "b" : null , "c" : null , "d" : null , "e" :  null }'
        (pairs ~[['a' ~] ['b' ~] ['c' ~] ['d' ~] ['e' ~]])
    ==
    :*  "y_object_whitespace_newlines"
        '{\0A"a"\0A:\0A"b"\0A}'
        (frond 'a' [%s 'b'])
    ==
    :*  "y_string_basic_ASCII_multi"
        '"1234567890 abcdefghijklmnopqrstuvwxyz"'
        [%s '1234567890 abcdefghijklmnopqrstuvwxyz']
    ==
    :*  "y_string_basic_ASCII_single"
        '"a"'
        [%s 'a']
    ==
    :*  "y_string_basic_escape_multi"
        '"\\"\\\\\\/\\b\\f\\n\\r\\t"'
        [%s '"\\/\08\0C\0A\0D\09']
    ==
    :*  "y_string_basic_escape_single"
        '"\\\\"'
        [%s '\\']
    ==
    :*  "y_string_basic_UTF16_multi"
        '"\\u00fe\\u0123"'
        [%s 'þģ']
    ==
    :*  "y_string_basic_UTF16_single"
        '"\\u00fe"'
        [%s 'þ']
    ==
    :*  "y_string_basic_UTF16_surrogate_pair_multi"
        '"\\uD83d\\uDe39\\uD83d\\uDc8d"'
        [%s '😹💍']
    ==
    :*  "y_string_basic_UTF16_surrogate_pair_single"
        '"\\uD801\\uDc37"'
        [%s '𐐷']
    ==
    :*  "y_string_basic_UTF8_multi"
        '"€𝄞"'
        [%s '€𝄞']
    ==
    :*  "y_string_basic_UTF8_single"
        '"𝄞"'
        [%s '𝄞']
    ==
    :*  "y_string_combination_all"
        '"a\\"\\u00fe𝄞"'
        [%s 'a"þ𝄞']
    ==
    :*  "y_string_combination_ASCII_escape"
        '"a\\""'
        [%s 'a"']
    ==
    :*  "y_string_combination_ASCII_escape_reverse"
        '"\\"a"'
        [%s '"a']
    ==
    :*  "y_string_combination_ASCII_UTF16"
        '"a\\u00fe"'
        [%s 'aþ']
    ==
    :*  "y_string_combination_ASCII_UTF16_reverse"
        '"\\u00fea"'
        [%s 'þa']
    ==
    :*  "y_string_combination_ASCII_UTF16_surrogate_pair"
        '"a\\uD801\\uDc37"'
        [%s 'a𐐷']
    ==
    :*  "y_string_combination_ASCII_UTF16_surrogate_pair_reverse"
        '"\\uD801\\uDc37a"'
        [%s '𐐷a']
    ==
    :*  "y_string_combination_ASCII_UTF8"
        '"a𝄞"'
        [%s 'a𝄞']
    ==
    :*  "y_string_combination_ASCII_UTF8_reverse"
        '"𝄞a"'
        [%s '𝄞a']
    ==
    :*  "y_string_combination_escape_UTF16"
        '"\\"\\u00fe"'
        [%s '"þ']
    ==
    :*  "y_string_combination_escape_UTF16_reverse"
        '"\\u00fe\\""'
        [%s 'þ"']
    ==
    :*  "y_string_combination_escape_UTF16_surrogate_pair"
        '"\\"\\uD801\\uDc37"'
        [%s '"𐐷']
    ==
    :*  "y_string_combination_escape_UTF16_surrogate_pair_reverse"
        '"\\uD801\\uDc37\\""'
        [%s '𐐷"']
    ==
    :*  "y_string_combination_escape_UTF8"
        '"\\"𝄞"'
        [%s '"𝄞']
    ==
    :*  "y_string_combination_escape_UTF8_reverse"
        '"𝄞\\""'
        [%s '𝄞"']
    ==
    :*  "y_string_combination_UTF8_UTF16"
        '"𝄞\\u00fe"'
        [%s '𝄞þ']
    ==
    :*  "y_string_combination_UTF8_UTF16_reverse"
        '"\\u00fe𝄞"'
        [%s 'þ𝄞']
    ==
    :*  "y_string_combination_UTF8_UTF16_surrogate_pair"
        '"𝄞\\uD801\\uDc37"'
        [%s '𝄞𐐷']
    ==
    :*  "y_string_combination_UTF8_UTF16_surrogate_pair_reverse"
        '"\\uD801\\uDc37𝄞"'
        [%s '𐐷𝄞']
    ==
    :*  "y_string_comments"
        '"a/*b*/c/*d//e"'
        [%s 'a/*b*/c/*d//e']
    ==
    :*  "y_string_double_escape"
        '"\\\\n"'
        [%s '\\n']
    ==
    :*  "y_string_empty"
        '""'
        [%s '']
    ==
    :*  "y_string_UTF16_ASCII_character"
        '"\\u0061"'
        [%s 'a']
    ==
    :*  "y_string_UTF16_control_character"
        '"\\u0012"'
        [%s '\12']
    ==
    :*  "y_string_UTF16_escape_character"
        '"\\u005C"'
        [%s '\\']
    ==
    :*  "y_string_UTF16_invalid_unicode_U+10FFFE"
        '"\\uDBFF\\uDFFE"'
        [%s (tuft `@c`0xd10.fffe)]
    ==
    :*  "y_string_UTF16_invisible_character_U+2064"
        '"\\u2064"'
        [%s '\E2\81\A4']
    ==
    :*  "y_string_UTF16_noncharacter"
        '"\\uFFFF"'
        [%s (tuft `@c`0xffff)]
    ==
    :*  "y_string_UTF16_null"
        '"\\u0000"'
        [%s '\00']
    ==
    :*  "y_string_UTF16_zero_width_character_U+200B"
        '"\\u200B"'
        [%s '\E2\80\8B']
    ==
    :*  "y_string_UTF8_1_2_3_4_bytes_as_UTF16"
        '"\\u0060\\u012a\\u12ab\\uD83e\\uDd80"'
        [%s '`Īካ🦀']
    ==
    :*  "y_string_UTF8_invalid_character_U+1BFFF"
        '"\F0\9B\BF\BF"'
        [%s (tuft `@c`0x1.bfff)]
    ==
    :*  "y_string_UTF8_non_displayable_character_U+2028"
        '" "'
        [%s '\E2\80\A8']
    ==
    :*  "y_string_UTF8_reserved_unicode_U+10FFFF"
        '"\F4\8F\BF\BF"'
        [%s (tuft `@c`0x10.ffff)]
    ==
    :*  "y_string_with_del_character"
        '"a\7Fa"'
        [%s 'a\7Fa']
    ==
    :*  "y_value_lonely_boolean_false"
        'false'
        [%b |]
    ==
    :*  "y_value_lonely_boolean_true"
        'true'
        [%b &]
    ==
    :*  "y_value_lonely_null"
        'null'
        ~
    ==
    :*  "y_value_whitespace_array"
        '\09\0A\0D [1] \09\0A\0D'
        [%a ~[[%n '1']]]
    ==
    :*  "y_value_whitespace_boolean_false"
        '\09\0A\0D false \09\0A\0D'
        [%b |]
    ==
    :*  "y_value_whitespace_boolean_true"
        '\09\0A\0D true \09\0A\0D'
        [%b &]
    ==
    :*  "y_value_whitespace_null"
        '\09\0A\0D null \09\0A\0D'
        ~
    ==
    :*  "y_value_whitespace_number"
        '\09\0A\0D 123 \09\0A\0D'
        [%n '123']
    ==
    :*  "y_value_whitespace_object"
        '\09\0A\0D {"a": "b"} \09\0A\0D'
        (frond ['a' [%s 'b']])
    ==
    :*  "y_value_whitespace_string"
        '\09\0A\0D "a" \09\0A\0D'
        [%s 'a']
    ==
  ==
::  Certain input is ambiguous under the JSON standard and a parser is free to
::  either accept or reject it. For jetting purposes, we need to know what the
::  behaviour of the Hoon parser is for each of these inputs so that the jet
::  semantics can match exactly.
::
::  These inputs are ambiguous under the JSON standard, but are rejected by
::  Urbit.
::
++  test-de-json-invalid-ambiguous-input
  %-  run-decode-rejection-specs
  :~
    ['"\FA"' "i_string_extended_ascii"]
    ['"\\uD888\\u1234"' "i_string_UTF16_invalid_2nd_surrogate"]
    ['"\\uDd1e\\uD834"' "i_string_UTF16_inverted_surrogates"]
    ['"\\uDADA"' "i_string_UTF16_lone_1st_surrogate"]
    ['"\\uDFAA"' "i_string_UTF16_lone_2nd_surrogate"]
    ['"\\uD800\\uD800\\n"' "i_string_UTF16_two_1st_surrogates"]
    ['"\\uDc00\\uDc00\\n"' "i_string_UTF16_two_2nd_surrogates"]
    ['"日ш\FA"' "i_string_UTF8_invalid_sequence"]
    ['"\E0"' "i_string_UTF8_lone_starting_byte"]
    ['"\81"' "i_string_UTF8_lone_continuation_byte"]
    ['"\F4\BF\BF\BF"' "i_string_UTF8_not_in_unicode_range"]
    ['"\C0\AF"' "i_string_UTF8_overlong_sequence_2_bytes"]
    ['"\FC\83\BF\BF\BF\BF"' "i_string_UTF8_overlong_sequence_6_bytes"]
    ['"\FC\80\80\80\80\80"' "i_string_UTF8_overlong_sequence_6_bytes_null"]
    ['"\ED\A0\80"' "i_string_UTF8_surrogate_U+D800"]
    ['"\E2\80"' "i_string_UTF8_truncated"]
  ==
::  These inputs are ambiguous under the JSON standard, but are accepted by
::  Urbit.
::
++  test-de-json-valid-ambiguous-input
  %-  run-decode-specs
  :~
    :*  "i_number_big_int"
        '123123123123123123123123123123'
        [%n '123123123123123123123123123123']
    ==
    :*  "i_number_big_int_neg"
        '-123123123123123123123123123123'
        [%n '-123123123123123123123123123123']
    ==
    :*  "i_number_huge_exp_int"
        '1e987654321987654321'
        [%n '1e987654321987654321']
    ==
    :*  "i_number_huge_exp_int_neg"
        '-1e987654321987654321'
        [%n '-1e987654321987654321']
    ==
    :*  "i_number_huge_exp_lead_0s_int"
        '1e00000000987654321987654321'
        [%n '1e00000000987654321987654321']
    ==
    :*  "i_number_huge_exp_lead_0s_real"
        '1.5e00000000987654321987654321'
        [%n '1.5e00000000987654321987654321']
    ==
    :*  "i_number_huge_exp_real"
        '1.5e987654321987654321'
        [%n '1.5e987654321987654321']
    ==
    :*  "i_number_huge_exp_real_neg"
        '-1.5e987654321987654321'
        [%n '-1.5e987654321987654321']
    ==
    :*  "i_number_huge_neg_exp_int"
        '1e-987654321987654321'
        [%n '1e-987654321987654321']
    ==
    :*  "i_number_huge_neg_exp_int_neg"
        '-1e-987654321987654321'
        [%n '-1e-987654321987654321']
    ==
    :*  "i_number_huge_neg_exp_real"
        '1.5e-987654321987654321'
        [%n '1.5e-987654321987654321']
    ==
    :*  "i_number_huge_neg_exp_real_neg"
        '-1.5e-987654321987654321'
        [%n '-1.5e-987654321987654321']
    ==
    :*  "i_number_real_neg_overflow"
        '-123123e100000'
        [%n '-123123e100000']
    ==
    :*  "i_number_real_pos_overflow"
        '123123e100000'
        [%n '123123e100000']
    ==
    :*  "i_number_real_underflow"
        '123e-1000000'
        [%n '123e-1000000']
    ==
  ==
::  Decoding stress-test
::
::    Random twitter user profile data used as practical example; no idea who
::    he is (I disavow!)
::
++  test-de-json-complex-structure
  %+  expect-eq
    !>  %-  some
        :-  %o
        %-  malt
        ^-  (list [@t ^json])
        :~
          :-  'data'
          :-  %o
          %-  malt
          ^-  (list [@t ^json])
          :~
            :-  'user'
            :-  %o
            %-  malt
            ^-  (list [@t ^json])
            :~
              :-  'result'
              :-  %o
              %-  malt
              ^-  (list [@t ^json])
              :~
                ['__typename' [%s 'User']]
                ['id' [%s 'VXNlcjo4MjYyNjE5MTQ=']]
                ['rest_id' [%s '826261914']]
                ['affiliates_highlighted_label' [%o ~]]
                ['has_nft_avatar' [%b |]]
                ::
                :-  'legacy'
                :-  %o
                %-  malt
                ^-  (list [@t ^json])
                :~
                  ['created_at' [%s 'Sun Sep 16 01:32:21 +0000 2012']]
                  ['default_profile' [%b &]]
                  ['default_profile_image' [%b |]]
                  ['description' [%s 'Not-NYT Bestselling author. Math PhD. Founder of New Discourses. Apolitical. Against totalitarianism and supremacy of all kinds. For freedom. 🇺🇲🇺🇲🇺🇲']]
                  ::
                  :-  'entities'
                  :-  %o
                  %-  malt
                  ^-  (list [@t ^json])
                  :~
                    :-  'description'
                    :-  %o
                    %-  malt
                    ^-  (list [@t ^json])
                    :~
                      [p='urls' q=[%a p=~]]
                    ==
                    ::
                    :-  'url'
                    :-  %o
                    %-  malt
                    ^-  (list [@t ^json])
                    :~
                      :-  'urls'
                      :-  %a
                      %-  limo
                      :~
                        :-  %o
                        %-  malt
                        ^-  (list [@t ^json])
                        :~
                          ['display_url' [%s 'NewDiscourses.com']]
                          ['expanded_url' [%s 'http://NewDiscourses.com']]
                          ['url' [%s 'https://t.co/RDZVUxIOWN']]
                          ['indices' [%a ~[[%n '0'] [%n '23']]]]
                        ==
                      ==
                    ==
                  ==
                  ::
                  ['fast_followers_count' [%n '0']]
                  ['favourites_count' [%n '193322']]
                  ['followers_count' [%n '284274']]
                  ['friends_count' [%n '338']]
                  ['has_custom_timelines' [%b &]]
                  ['is_translator' [%b |]]
                  ['listed_count' [%n '1867']]
                  ['location' [%s 'Knoxville, TN']]
                  ['media_count' [%n '11074']]
                  ['name' [%s 'James Lindsay, pro-freedom']]
                  ['normal_followers_count' [%n '284274']]
                  ['pinned_tweet_ids_str' [%a ~[[%s '1238111933905190913']]]]
                  ::
                  :-  'profile_banner_extensions'
                  :-  %o
                  %-  malt
                  ^-  (list [@t ^json])
                  :~
                    :-  'mediaColor'
                    :-  %o
                    %-  malt
                    ^-  (list [@t ^json])
                    :~
                      :-  'r'
                      :-  %o
                      %-  malt
                      ^-  (list [@t ^json])
                      :~
                        :-  'ok'
                        :-  %o
                        %-  malt
                        ^-  (list [@t ^json])
                        :~
                          :-  'palette'
                          :-  %a
                          %-  limo
                          :~
                            :-  %o
                            %-  malt
                            ^-  (list [@t ^json])
                            :~
                              ['percentage' [%n '95.91']]
                              ::
                              :-  'rgb'
                              :-  %o
                              %-  malt
                              :~
                                ['red' [%n '33']]
                                ['blue' [%n '41']]
                                ['green' [%n '36']]
                              ==
                            ==
                            ::
                            :-  %o
                            %-  malt
                            ^-  (list [@t ^json])
                            :~
                              ['percentage' [%n '3.65']]
                              ::
                              :-  'rgb'
                              :-  %o
                              %-  malt
                              :~
                                ['red' [%n '113']]
                                ['blue' [%n '118']]
                                ['green' [%n '115']]
                              ==
                            ==
                            ::
                            :-  %o
                            %-  malt
                            ^-  (list [@t ^json])
                            :~
                              ['percentage' [%n '0.44']]
                              ::
                              :-  'rgb'
                              :-  %o
                              %-  malt
                              :~
                                ['red' [%n '200']]
                                ['blue' [%n '202']]
                                ['green' [%n '201']]
                              ==
                            ==
                          ==
                        ==
                      ==
                    ==
                  ==
                  ::
                  ['profile_banner_url' [%s 'https://pbs.twimg.com/profile_banners/826261914/1582653360']]
                  ::
                  :-  'profile_image_extensions'
                  :-  %o
                  %-  malt
                  ^-  (list [@t ^json])
                  :~
                    :-  'mediaColor'
                    :-  %o
                    %-  malt
                    ^-  (list [@t ^json])
                    :~
                      :-  'r'
                      :-  %o
                      %-  malt
                      ^-  (list [@t ^json])
                      :~
                        :-  'ok'
                        :-  %o
                        %-  malt
                        ^-  (list [@t ^json])
                        :~
                          :-  'palette'
                          :-  %a
                          %-  limo
                          :~
                            :-  %o
                            %-  malt
                            ^-  (list [@t ^json])
                            :~
                              ['percentage' [%n '72.12']]
                              ::
                              :-  'rgb'
                              :-  %o
                              %-  malt
                              :~
                                ['red' [%n '35']]
                                ['blue' [%n '16']]
                                ['green' [%n '22']]
                              ==
                            ==
                            ::
                            :-  %o
                            %-  malt
                            ^-  (list [@t ^json])
                            :~
                              ['percentage' [%n '17.13']]
                              ::
                              :-  'rgb'
                              :-  %o
                              %-  malt
                              :~
                                ['red' [%n '146']]
                                ['blue' [%n '100']]
                                ['green' [%n '123']]
                              ==
                            ==
                            ::
                            :-  %o
                            %-  malt
                            ^-  (list [@t ^json])
                            :~
                              ['percentage' [%n '10.57']]
                              ::
                              :-  'rgb'
                              :-  %o
                              %-  malt
                              :~
                                ['red' [%n '193']]
                                ['blue' [%n '129']]
                                ['green' [%n '153']]
                              ==
                            ==
                            ::
                            :-  %o
                            %-  malt
                            ^-  (list [@t ^json])
                            :~
                              ['percentage' [%n '2.03']]
                              ::
                              :-  'rgb'
                              :-  %o
                              %-  malt
                              :~
                                ['red' [%n '222']]
                                ['blue' [%n '125']]
                                ['green' [%n '149']]
                              ==
                            ==
                            ::
                            :-  %o
                            %-  malt
                            ^-  (list [@t ^json])
                            :~
                              ['percentage' [%n '1.58']]
                              ::
                              :-  'rgb'
                              :-  %o
                              %-  malt
                              :~
                                ['red' [%n '131']]
                                ['blue' [%n '55']]
                                ['green' [%n '53']]
                              ==
                            ==
                          ==
                        ==
                      ==
                    ==
                  ==
                  ::
                  ['profile_image_url_https' [%s 'https://pbs.twimg.com/profile_images/1459175734602350593/cW3fs5lR_normal.jpg']]
                  ['profile_interstitial_type' [%s '']]
                  ['protected' [%b |]]
                  ['screen_name' [%s 'ConceptualJames']]
                  ['statuses_count' [%n '137296']]
                  ['translator_type' [%s 'none']]
                  ['verified' [%b |]]
                  ['url' [%s 'https://t.co/RDZVUxIOWN']]
                  ['withheld_in_countries' [%a ~]]
                ==
                ::
                ['legacy_extended_profile' [%o ~]]
                ['is_profile_translatable' [%b |]]
              ==
            ==
          ==
        ==
    !>  %-  de:json  '{"data":{"user":{"result":{"has_nft_avatar":false,"rest_id":"826261914","affiliates_highlighted_label":{},"__typename":"User","id":"VXNlcjo4MjYyNjE5MTQ=","is_profile_translatable":false,"legacy_extended_profile":{},"legacy":{"protected":false,"pinned_tweet_ids_str":["1238111933905190913"],"entities":{"url":{"urls":[{"indices":[0,23],"display_url":"NewDiscourses.com","url":"https://t.co/RDZVUxIOWN","expanded_url":"http://NewDiscourses.com"}]},"description":{"urls":[]}},"name":"James Lindsay, pro-freedom","default_profile":true,"location":"Knoxville, TN","url":"https://t.co/RDZVUxIOWN","verified":false,"profile_interstitial_type":"","fast_followers_count":0,"description":"Not-NYT Bestselling author. Math PhD. Founder of New Discourses. Apolitical. Against totalitarianism and supremacy of all kinds. For freedom. 🇺🇲🇺🇲🇺🇲","created_at":"Sun Sep 16 01:32:21 +0000 2012","followers_count":284274,"withheld_in_countries":[],"listed_count":1867,"profile_image_url_https":"https://pbs.twimg.com/profile_images/1459175734602350593/cW3fs5lR_normal.jpg","has_custom_timelines":true,"normal_followers_count":284274,"favourites_count":193322,"default_profile_image":false,"media_count":11074,"statuses_count":137296,"profile_banner_extensions":{"mediaColor":{"r":{"ok":{"palette":[{"rgb":{"green":36,"blue":41,"red":33},"percentage":95.91},{"rgb":{"green":115,"blue":118,"red":113},"percentage":3.65},{"rgb":{"green":201,"blue":202,"red":200},"percentage":0.44}]}}}},"profile_banner_url":"https://pbs.twimg.com/profile_banners/826261914/1582653360","screen_name":"ConceptualJames","translator_type":"none","friends_count":338,"profile_image_extensions":{"mediaColor":{"r":{"ok":{"palette":[{"rgb":{"green":22,"blue":16,"red":35},"percentage":72.12},{"rgb":{"green":123,"blue":100,"red":146},"percentage":17.13},{"rgb":{"green":153,"blue":129,"red":193},"percentage":10.57},{"rgb":{"green":149,"blue":125,"red":222},"percentage":2.03},{"rgb":{"green":53,"blue":55,"red":131},"percentage":1.58}]}}}},"is_translator":false}}}}}'
::  JSON encoding test suite
::
::  Encode arrays
::
++  test-en-json-arrays
  %-  run-encode-specs
  :~
    :*  "e_array_empty"
        [%a ~]
        '[]'
    ==
    :*  "e_array_single_boolean_false"
        [%a ~[[%b |]]]
        '[false]'
    ==
    :*  "e_array_single_boolean_true"
        [%a ~[[%b &]]]
        '[true]'
    ==
    :*  "e_array_single_null"
        [%a ~[~]]
        '[null]'
    ==
    :*  "e_array_single_number"
        [%a ~[[%n '123.456']]]
        '[123.456]'
    ==
    :*  "e_array_single_object"
        [%a ~[[%o ~]]]
        '[{}]'
    ==
    :*  "e_array_single_string"
        [%a ~[[%s 'abc']]]
        '["abc"]'
    ==
    :*  "e_array_nested_array"
        [%a ~[[%a ~]]]
        '[[]]'
    ==
    :*  "e_array_multiple_homogenous"
        [%a ~[[%n '1'] [%n '2'] [%n '3'] [%n '4'] [%n '5']]]
        '[1,2,3,4,5]'
    ==
    :*  "e_array_multiple_heterogenous"
        [%a ~[[%b |] ~ [%n '1'] [%s 'a'] [%o ~] [%a ~]]]
        '[false,null,1,"a",{},[]]'
    ==
  ==
::  Encode numbers
::
::  .en-json:html just converts the number as a cord to tape - the real work is
::  done by .enjs:format
::
++  test-en-json-numbers
  %-  run-encode-specs
  :~
    :*  "e_number_sanity_check"
        [%n '-123.456e789']
        '-123.456e789'
    ==
  ==
::  Encode objects
::
++  test-en-json-objects
  %-  run-encode-specs
  :~
    :*  "e_object_empty"
        [%o ~]
        '{}'
    ==
    :*  "e_object_single_boolean_false"
        (frond 'x' [%b |])
        '{"x":false}'
    ==
    :*  "e_object_single_boolean_true"
        (frond 'x' [%b &])
        '{"x":true}'
    ==
    :*  "e_object_single_null"
        (frond 'x' ~)
        '{"x":null}'
    ==
    :*  "e_object_single_number"
        (frond 'x' [%n '123.456'])
        '{"x":123.456}'
    ==
    :*  "e_object_single_string"
        (frond 'x' [%s 'abc'])
        '{"x":"abc"}'
    ==
    :*  "e_object_single_array"
        (frond 'x' [%a ~])
        '{"x":[]}'
    ==
    :*  "e_object_nested_object"
        (frond 'x' [%o ~])
        '{"x":{}}'
    ==
    ::  the order of elements for the below two tests is determined by hash, and
    ::  therefore "random" but consistent 
    :*  "e_object_multiple_homogenous"
        (pairs ~[['a' [%n '97']] ['b' [%n '98']] ['c' [%n '99']] ['d' [%n '100']] ['e' [%n '101']] ['f' [%n '102']]])
        '{"c":99,"a":97,"f":102,"d":100,"b":98,"e":101}'
    ==
    :*  "e_object_multiple_heterogenous"
        (pairs ~[['a' [%b |]] ['b' ~] ['c' [%n '1']] ['d' [%s 'a']] ['e' [%a ~]] ['f' [%o ~]]])
        '{"c":1,"a":false,"f":{},"d":"a","b":null,"e":[]}'
    ==
  ==
::  Encode strings
::
::  Summary of .en-json:html string encoding rules:
::    - '"', '\', and newlines are escaped
::    - bytes \00 to \1F and \7F are UTF16 escaped
::    - everything else (ASCII & unicode) is written directly as UTF8
::
++  test-en-json-strings
  %-  run-encode-specs
  :~
    :*  "e_string_empty"
        [%s '']
        '""'
    ==
    :*  "e_string_null_wrapped"
        [%s 'a\00a']
        '"a\\u0000a"'
    ==
    :*  "e_string_ascii"
        [%s 'a']
        '"a"'
    ==
    :*  "e_string_escape_backslash"
        [%s 'Delete C:\\Windows\\System32']
        '"Delete C:\\\\Windows\\\\System32"'
    ==
    :*  "e_string_escape_delete"
        [%s 'delete\7Fme']
        '"delete\\u007fme"'
    ==
    :*  "e_string_escape_newline"
        [%s 'new\0Aline']
        '"new\\nline"'
    ==
    :*  "e_string_escape_quotes"
        [%s 'he said "wow"']
        '"he said \\"wow\\""'
    ==
    :*  "e_string_escape_whitespace"
        [%s 'add\01some\09space\1Fbetween here']
        '"add\\u0001some\\u0009space\\u001fbetween here"'
    ==
    :*  "e_string_unicode_bytes"
        [%s '\F0\9D\84\9E']
        '"𝄞"'
    ==
    :*  "e_string_unicode_bytes_invalid"
        [%s '\F0\9B\BF\BF']
        '"𛿿"'
    ==
    :*  "e_string_unicode_bytes_invisible"
        [%s '\E2\81\A4']
        '"⁤"'
    ==
    :*  "e_string_unicode_inline"
        [%s '𝄞']
        '"𝄞"'
    ==
    :*  "e_string_utf16_inline"
        [%s '\\uD83C\\uDDFA\\uD83C\\uDDF2']
        '"\\\\uD83C\\\\uDDFA\\\\uD83C\\\\uDDF2"'
    ==
    :*  "e_string_multiple_ascii"
        [%s '123456789 abcdefghijklmnop']
        '"123456789 abcdefghijklmnop"'
    ==
    :*  "e_string_multiple_escapes"
        [%s '\09"wow!"\0Ahe said\\']
        '"\\u0009\\"wow!\\"\\nhe said\\\\"'
    ==
    :*  "e_string_multiple_unicode"
        [%s '𝄞\F0\9D\84\9E']
       '"𝄞𝄞"'
    ==
  ==
::  Encode naked values
::
++  test-en-json-values
  %-  run-encode-specs
  :~
    :*  "e_value_bool_false"
        [%b |]
        'false'
    ==
    :*  "e_value_bool_true"
        [%b &]
        'true'
    ==
    :*  "e_value_null"
        ~
        'null'
    ==
  ==
::  Encoding stress-test
::
::    Random twitter user profile data used as practical example; no idea who
::    he is (I disavow!)
::
++  test-en-json-complex-structure
  %+  expect-eq
    !>  '{"data":{"user":{"result":{"has_nft_avatar":false,"rest_id":"826261914","affiliates_highlighted_label":{},"__typename":"User","id":"VXNlcjo4MjYyNjE5MTQ=","is_profile_translatable":false,"legacy_extended_profile":{},"legacy":{"protected":false,"pinned_tweet_ids_str":["1238111933905190913"],"entities":{"url":{"urls":[{"indices":[0,23],"display_url":"NewDiscourses.com","url":"https://t.co/RDZVUxIOWN","expanded_url":"http://NewDiscourses.com"}]},"description":{"urls":[]}},"name":"James Lindsay, pro-freedom","default_profile":true,"location":"Knoxville, TN","url":"https://t.co/RDZVUxIOWN","verified":false,"profile_interstitial_type":"","fast_followers_count":0,"description":"Not-NYT Bestselling author. Math PhD. Founder of New Discourses. Apolitical. Against totalitarianism and supremacy of all kinds. For freedom. 🇺🇲🇺🇲🇺🇲","created_at":"Sun Sep 16 01:32:21 +0000 2012","followers_count":284274,"withheld_in_countries":[],"listed_count":1867,"profile_image_url_https":"https://pbs.twimg.com/profile_images/1459175734602350593/cW3fs5lR_normal.jpg","has_custom_timelines":true,"normal_followers_count":284274,"favourites_count":193322,"default_profile_image":false,"media_count":11074,"statuses_count":137296,"profile_banner_extensions":{"mediaColor":{"r":{"ok":{"palette":[{"rgb":{"green":36,"blue":41,"red":33},"percentage":95.91},{"rgb":{"green":115,"blue":118,"red":113},"percentage":3.65},{"rgb":{"green":201,"blue":202,"red":200},"percentage":0.44}]}}}},"profile_banner_url":"https://pbs.twimg.com/profile_banners/826261914/1582653360","screen_name":"ConceptualJames","translator_type":"none","friends_count":338,"profile_image_extensions":{"mediaColor":{"r":{"ok":{"palette":[{"rgb":{"green":22,"blue":16,"red":35},"percentage":72.12},{"rgb":{"green":123,"blue":100,"red":146},"percentage":17.13},{"rgb":{"green":153,"blue":129,"red":193},"percentage":10.57},{"rgb":{"green":149,"blue":125,"red":222},"percentage":2.03},{"rgb":{"green":53,"blue":55,"red":131},"percentage":1.58}]}}}},"is_translator":false}}}}}'
    !>  %-  en:json
        :-  %o
        %-  malt
        ^-  (list [@t ^json])
        :~
          :-  'data'
          :-  %o
          %-  malt
          ^-  (list [@t ^json])
          :~
            :-  'user'
            :-  %o
            %-  malt
            ^-  (list [@t ^json])
            :~
              :-  'result'
              :-  %o
              %-  malt
              ^-  (list [@t ^json])
              :~
                ['__typename' [%s 'User']]
                ['id' [%s 'VXNlcjo4MjYyNjE5MTQ=']]
                ['rest_id' [%s '826261914']]
                ['affiliates_highlighted_label' [%o ~]]
                ['has_nft_avatar' [%b |]]
                ::
                :-  'legacy'
                :-  %o
                %-  malt
                ^-  (list [@t ^json])
                :~
                  ['created_at' [%s 'Sun Sep 16 01:32:21 +0000 2012']]
                  ['default_profile' [%b &]]
                  ['default_profile_image' [%b |]]
                  ['description' [%s 'Not-NYT Bestselling author. Math PhD. Founder of New Discourses. Apolitical. Against totalitarianism and supremacy of all kinds. For freedom. 🇺🇲🇺🇲🇺🇲']]
                  ::
                  :-  'entities'
                  :-  %o
                  %-  malt
                  ^-  (list [@t ^json])
                  :~
                    :-  'description'
                    :-  %o
                    %-  malt
                    ^-  (list [@t ^json])
                    :~
                      [p='urls' q=[%a p=~]]
                    ==
                    ::
                    :-  'url'
                    :-  %o
                    %-  malt
                    ^-  (list [@t ^json])
                    :~
                      :-  'urls'
                      :-  %a
                      %-  limo
                      :~
                        :-  %o
                        %-  malt
                        ^-  (list [@t ^json])
                        :~
                          ['display_url' [%s 'NewDiscourses.com']]
                          ['expanded_url' [%s 'http://NewDiscourses.com']]
                          ['url' [%s 'https://t.co/RDZVUxIOWN']]
                          ['indices' [%a ~[[%n '0'] [%n '23']]]]
                        ==
                      ==
                    ==
                  ==
                  ::
                  ['fast_followers_count' [%n '0']]
                  ['favourites_count' [%n '193322']]
                  ['followers_count' [%n '284274']]
                  ['friends_count' [%n '338']]
                  ['has_custom_timelines' [%b &]]
                  ['is_translator' [%b |]]
                  ['listed_count' [%n '1867']]
                  ['location' [%s 'Knoxville, TN']]
                  ['media_count' [%n '11074']]
                  ['name' [%s 'James Lindsay, pro-freedom']]
                  ['normal_followers_count' [%n '284274']]
                  ['pinned_tweet_ids_str' [%a ~[[%s '1238111933905190913']]]]
                  ::
                  :-  'profile_banner_extensions'
                  :-  %o
                  %-  malt
                  ^-  (list [@t ^json])
                  :~
                    :-  'mediaColor'
                    :-  %o
                    %-  malt
                    ^-  (list [@t ^json])
                    :~
                      :-  'r'
                      :-  %o
                      %-  malt
                      ^-  (list [@t ^json])
                      :~
                        :-  'ok'
                        :-  %o
                        %-  malt
                        ^-  (list [@t ^json])
                        :~
                          :-  'palette'
                          :-  %a
                          %-  limo
                          :~
                            :-  %o
                            %-  malt
                            ^-  (list [@t ^json])
                            :~
                              ['percentage' [%n '95.91']]
                              ::
                              :-  'rgb'
                              :-  %o
                              %-  malt
                              :~
                                ['red' [%n '33']]
                                ['blue' [%n '41']]
                                ['green' [%n '36']]
                              ==
                            ==
                            ::
                            :-  %o
                            %-  malt
                            ^-  (list [@t ^json])
                            :~
                              ['percentage' [%n '3.65']]
                              ::
                              :-  'rgb'
                              :-  %o
                              %-  malt
                              :~
                                ['red' [%n '113']]
                                ['blue' [%n '118']]
                                ['green' [%n '115']]
                              ==
                            ==
                            ::
                            :-  %o
                            %-  malt
                            ^-  (list [@t ^json])
                            :~
                              ['percentage' [%n '0.44']]
                              ::
                              :-  'rgb'
                              :-  %o
                              %-  malt
                              :~
                                ['red' [%n '200']]
                                ['blue' [%n '202']]
                                ['green' [%n '201']]
                              ==
                            ==
                          ==
                        ==
                      ==
                    ==
                  ==
                  ::
                  ['profile_banner_url' [%s 'https://pbs.twimg.com/profile_banners/826261914/1582653360']]
                  ::
                  :-  'profile_image_extensions'
                  :-  %o
                  %-  malt
                  ^-  (list [@t ^json])
                  :~
                    :-  'mediaColor'
                    :-  %o
                    %-  malt
                    ^-  (list [@t ^json])
                    :~
                      :-  'r'
                      :-  %o
                      %-  malt
                      ^-  (list [@t ^json])
                      :~
                        :-  'ok'
                        :-  %o
                        %-  malt
                        ^-  (list [@t ^json])
                        :~
                          :-  'palette'
                          :-  %a
                          %-  limo
                          :~
                            :-  %o
                            %-  malt
                            ^-  (list [@t ^json])
                            :~
                              ['percentage' [%n '72.12']]
                              ::
                              :-  'rgb'
                              :-  %o
                              %-  malt
                              :~
                                ['red' [%n '35']]
                                ['blue' [%n '16']]
                                ['green' [%n '22']]
                              ==
                            ==
                            ::
                            :-  %o
                            %-  malt
                            ^-  (list [@t ^json])
                            :~
                              ['percentage' [%n '17.13']]
                              ::
                              :-  'rgb'
                              :-  %o
                              %-  malt
                              :~
                                ['red' [%n '146']]
                                ['blue' [%n '100']]
                                ['green' [%n '123']]
                              ==
                            ==
                            ::
                            :-  %o
                            %-  malt
                            ^-  (list [@t ^json])
                            :~
                              ['percentage' [%n '10.57']]
                              ::
                              :-  'rgb'
                              :-  %o
                              %-  malt
                              :~
                                ['red' [%n '193']]
                                ['blue' [%n '129']]
                                ['green' [%n '153']]
                              ==
                            ==
                            ::
                            :-  %o
                            %-  malt
                            ^-  (list [@t ^json])
                            :~
                              ['percentage' [%n '2.03']]
                              ::
                              :-  'rgb'
                              :-  %o
                              %-  malt
                              :~
                                ['red' [%n '222']]
                                ['blue' [%n '125']]
                                ['green' [%n '149']]
                              ==
                            ==
                            ::
                            :-  %o
                            %-  malt
                            ^-  (list [@t ^json])
                            :~
                              ['percentage' [%n '1.58']]
                              ::
                              :-  'rgb'
                              :-  %o
                              %-  malt
                              :~
                                ['red' [%n '131']]
                                ['blue' [%n '55']]
                                ['green' [%n '53']]
                              ==
                            ==
                          ==
                        ==
                      ==
                    ==
                  ==
                  ::
                  ['profile_image_url_https' [%s 'https://pbs.twimg.com/profile_images/1459175734602350593/cW3fs5lR_normal.jpg']]
                  ['profile_interstitial_type' [%s '']]
                  ['protected' [%b |]]
                  ['screen_name' [%s 'ConceptualJames']]
                  ['statuses_count' [%n '137296']]
                  ['translator_type' [%s 'none']]
                  ['verified' [%b |]]
                  ['url' [%s 'https://t.co/RDZVUxIOWN']]
                  ['withheld_in_countries' [%a ~]]
                ==
                ::
                ['legacy_extended_profile' [%o ~]]
                ['is_profile_translatable' [%b |]]
              ==
            ==
          ==
        ==
::  XML encoding/decoding
::
::  XML decoding
::
++  test-de-xml
  ;:  weld
    ::  Basic use
    ::
    %+  expect-eq
      !>  ^-  manx  +:(de-xml (crip "<html><head><title>My first webpage</title></head><body><h1>Welcome!</h1>Hello, world! We are on the web.\0a<div></div><script src=\"http://unsafely.tracking.you/cookiemonster.js\"></script></body></html>"))
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
      +:(de-xml (crip "<elem><![CDATA[text]]></elem>"))
    !>  ^-  manx
    ;elem: text
    ::  comments
    ::
    %+  expect-eq
      !>  ^-  manx
      ;elem: text
    !>  +:(de-xml (crip "<elem>text<!-- comment --></elem>"))
    %+  expect-eq
      !>  ^-  manx
      ;elem;
    !>  +:(de-xml (crip "<elem><!-- comment --></elem>"))
    ::  entities
    ::
    %+  expect-eq
      !>  ^-  manx
      +:(de-xml (crip "<elem>&#62;</elem>"))
      !>  ^-  manx
      ;elem: >
    :: self-closing tag
    ::
    %+  expect-eq
      !>  ^-  manx
      +:(de-xml (crip "<img />"))
      !>  ^-  manx
      ;img;

  ==
:: XML encoding
::
++  test-en-xml
  ;:  weld
    ::  Entities
    ::
    %+  expect-eq
      !>  "<elem>&gt;</elem>"
    !>  %-  en-xml
    ;elem: >
    ::  Basic use
    ::
    %+  expect-eq
      !>   %-  en-xml
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
      !>  %-  en-xml
      ;input(type "submit"): Submit
  ==
--
