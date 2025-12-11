::  Tests for +differ (a suite of Hunt-McIlroy diff and merge algorithms)
::
/+  *test
::
=,  differ
::  Testing arms
::
|%
::  ++berk:differ: invert diff patch
::
++  test-berk  ^-  tang
  ::  An inverted diff between %a and %b can be checked
  ::  by patching %b and obtaining %a
  ::
  ;:  weld
    ::  (some) test examples adapted from:
    ::  https://github.com/gioele/diff-lcs/blob/master/test/test_diff-lcs.rb
    ::
    =/  a  "abcehjlmnp"
    =/  b  "bcdefjklmrst"
    =/  diff-a-b  (lusk a b (loss a b))
    =/  diff-b-a  (lusk b a (loss b a))
    =/  patch-a  (lurk b (berk diff-a-b))
    =/  patch-b  (lurk a (berk diff-b-a))
    %+  expect-eq
      !>  a^b
      !>  patch-a^patch-b
    ::
    =/  a  "abcde"
    =/  b  "ae"
    =/  diff-a-b  (lusk a b (loss a b))
    =/  diff-b-a  (lusk b a (loss b a))
    =/  patch-a  (lurk b (berk diff-a-b))
    =/  patch-b  (lurk a (berk diff-b-a))
    %+  expect-eq
      !>  a^b
      !>  patch-a^patch-b
    ::
    =/  a  "ae"
    =/  b  "abcde"
    =/  diff-a-b  (lusk a b (loss a b))
    =/  diff-b-a  (lusk b a (loss b a))
    =/  patch-a  (lurk b (berk diff-a-b))
    =/  patch-b  (lurk a (berk diff-b-a))
    %+  expect-eq
      !>  a^b
      !>  patch-a^patch-b
    ::
    =/  a  "vxae"
    =/  b  "wyabcde"
    =/  diff-a-b  (lusk a b (loss a b))
    =/  diff-b-a  (lusk b a (loss b a))
    =/  patch-a  (lurk b (berk diff-a-b))
    =/  patch-b  (lurk a (berk diff-b-a))
    %+  expect-eq
      !>  a^b
      !>  patch-a^patch-b
    ::
    =/  a  "xae"
    =/  b  "abcde"
    =/  diff-a-b  (lusk a b (loss a b))
    =/  diff-b-a  (lusk b a (loss b a))
    =/  patch-a  (lurk b (berk diff-a-b))
    =/  patch-b  (lurk a (berk diff-b-a))
    %+  expect-eq
      !>  a^b
      !>  patch-a^patch-b
    ::
    =/  a  "ae"
    =/  b  "xabcde"
    =/  diff-a-b  (lusk a b (loss a b))
    =/  diff-b-a  (lusk b a (loss b a))
    =/  patch-a  (lurk b (berk diff-a-b))
    =/  patch-b  (lurk a (berk diff-b-a))
    %+  expect-eq
      !>  a^b
      !>  patch-a^patch-b
    ::
    =/  a  "aev"
    =/  b  "xabcdewx"
    =/  diff-a-b  (lusk a b (loss a b))
    =/  diff-b-a  (lusk b a (loss b a))
    =/  patch-a  (lurk b (berk diff-a-b))
    =/  patch-b  (lurk a (berk diff-b-a))
    %+  expect-eq
      !>  a^b
      !>  patch-a^patch-b
    ::
    ::  individuals diffs
    ::
    =/  a  "10qawsedrftg"
    =/  b  "1Aqawsedrftg"
    =/  diff=(urge:clay cord)
      :~  ::  copies first match
          ::
          [%.y 1]
          ::  replaces 0 with 'A'
          ::
          [%.n "0" "A"]
          ::  copies the rest
          ::
          [%.y 10]
      ==
    %+  expect-eq
      !>  a
      !>  (lurk b (berk diff))
    ::
    =/  a  "1qawsedrftg10"
    =/  b  "1Aqawsedrftg"
    =/  diff=(urge:clay cord)
      :~  ::  copies first match
          ::
          [%.y 1]
          ::  inserts 'A'
          ::
          [%.n ~ "A"]
          ::  copies all matches
          ::
          [%.y 10]
          ::  copies '10'
          ::
          [%.n (flop "10") ~]
      ==
    %+  expect-eq
      !>  a
      !>  (lurk b (berk diff))
  ==
::
::  ++loss:differ: longest subsequence
::
++  test-loss  ^-  tang
  ;:  weld
    ::  null case
    ::
    %+  expect-eq
      !>  ~
      !>  (loss "abc" "xyz")
    ::  common prefix
    ::
    %+  expect-eq
      !>  "abc"
      !>  (loss "abcq" "abcxyz")
    %+  expect-eq
      !>  "qaz"
      !>  (loss "qaz" "qazxyz")
    ::  common suffix
    ::
    %+  expect-eq
      !>  "wsx"
      !>  (loss "qwsx" "xyzwsx")
    %+  expect-eq
      !>  "edc"
      !>  (loss "edc" "xyzedc")
    ::  overlap
    ::
    %+  expect-eq
      !>  "rfv"
      !>  (loss "qrfvp" "xyzrfvdef")
    %+  expect-eq
      !>  "tgb"
      !>  (loss "qwertytgb" "tgbasdfgh")
    :: Non contiguous
    ::
    %+  expect-eq
      ::  Example from wikipedia:
      ::  https://en.wikipedia.org/wiki/Longest_common_subsequence_problem
      ::
      !>  "MJAU"
      !>  (loss "XMJYAUZ" "MZJAWXU")
    %+  expect-eq
      !>  "qawsxcf"
      !>  (loss "qazwsxedcrfvtb" "qqqawsxcf")
  ==
::
::  ++lurk:differ: apply list patch
::
++  test-lurk  ^-  tang
  ;:  weld
    ::  (some) test examples adapted from:
    ::  https://github.com/gioele/diff-lcs/blob/master/test/test_diff-lcs.rb
    ::
    =/  a  "abcehjlmnp"
    =/  b  "bcdefjklmrst"
    =/  diff-a-b  (lusk a b (loss a b))
    =/  diff-b-a  (lusk b a (loss b a))
    =/  patch-b   (lurk a diff-a-b)
    =/  patch-a   (lurk b diff-b-a)
    %+  expect-eq
      !>  a^b
      !>  patch-a^patch-b
    ::
    =/  a  "abcde"
    =/  b  "ae"
    =/  diff-a-b  (lusk a b (loss a b))
    =/  diff-b-a  (lusk b a (loss b a))
    =/  patch-b   (lurk a diff-a-b)
    =/  patch-a   (lurk b diff-b-a)
    %+  expect-eq
      !>  a^b
      !>  patch-a^patch-b
    ::
    =/  a  "ae"
    =/  b  "abcde"
    =/  diff-a-b  (lusk a b (loss a b))
    =/  diff-b-a  (lusk b a (loss b a))
    =/  patch-b   (lurk a diff-a-b)
    =/  patch-a   (lurk b diff-b-a)
    %+  expect-eq
      !>  a^b
      !>  patch-a^patch-b
    ::
    =/  a  "vxae"
    =/  b  "wyabcde"
    =/  diff-a-b  (lusk a b (loss a b))
    =/  diff-b-a  (lusk b a (loss b a))
    =/  patch-b   (lurk a diff-a-b)
    =/  patch-a   (lurk b diff-b-a)
    %+  expect-eq
      !>  a^b
      !>  patch-a^patch-b
    ::
    =/  a  "xae"
    =/  b  "abcde"
    =/  diff-a-b  (lusk a b (loss a b))
    =/  diff-b-a  (lusk b a (loss b a))
    =/  patch-b   (lurk a diff-a-b)
    =/  patch-a   (lurk b diff-b-a)
    %+  expect-eq
      !>  a^b
      !>  patch-a^patch-b
    ::
    =/  a  "ae"
    =/  b  "xabcde"
    =/  diff-a-b  (lusk a b (loss a b))
    =/  diff-b-a  (lusk b a (loss b a))
    =/  patch-b   (lurk a diff-a-b)
    =/  patch-a   (lurk b diff-b-a)
    %+  expect-eq
      !>  a^b
      !>  patch-a^patch-b
    ::
    =/  a  "aev"
    =/  b  "xabcdewx"
    =/  diff-a-b  (lusk a b (loss a b))
    =/  diff-b-a  (lusk b a (loss b a))
    =/  patch-b   (lurk a diff-a-b)
    =/  patch-a   (lurk b diff-b-a)
    %+  expect-eq
      !>  a^b
      !>  patch-a^patch-b
    ::
    ::  individuals diffs
    ::
    =/  a  "10qawsedrftg"
    =/  b  "1Aqawsedrftg"
    =/  diff=(urge:clay cord)
      :~  ::  copies first match
          ::
          [%.y 1]
          ::  replaces 0 with 'A'
          ::
          [%.n "0" "A"]
          ::  copies the rest
          ::
          [%.y 10]
      ==
    %+  expect-eq
      !>  b
      !>  (lurk a diff)
    ::
    =/  a  "1qawsedrftg10"
    =/  b  "1Aqawsedrftg"
    =/  diff=(urge:clay cord)
      :~  ::  copies first match
          ::
          [%.y 1]
          ::  inserts 'A'
          ::
          [%.n ~ "A"]
          ::  copies all matches
          ::
          [%.y 10]
          ::  copies '10'
          ::
          [%.n (flop "10") ~]
      ==
    %+  expect-eq
      !>  b
      !>  (lurk a diff)
  ==
::  ++lusk:differ: lcs to list patch
::
++  test-lusk  ^-  tang
  ;:  weld
    ::  (some) test examples adapted from:
    ::  https://github.com/gioele/diff-lcs/blob/master/test/test_diff-lcs.rb
    ::
    =/  a  "abcehjlmnp"
    =/  b  "bcdefjklmrst"
    =/  diff
      :~  [%.n ~['a'] ~]
          [%.y 2]
          [%.n ~ ~['d']]
          [%.y 1]
          [%.n ~['h'] ~['f']]
          [%.y 1]
          [%.n ~ ~['k']]
          [%.y 2]
          [%.n (flop "np") (flop "rst")]
      ==
    %+  expect-eq
      !>  diff
      !>  (lusk a b (loss a b))
    ::
    =/  a  "abcde"
    =/  b  "ae"
    =/  diff
      :~  [%.y 1]
          [%.n (flop "bcd") ~]
          [%.y 1]
      ==
    %+  expect-eq
      !>  diff
      !>  (lusk a b (loss a b))
    ::
    =/  a  "ae"
    =/  b  "abcde"
    =/  diff
      :~  [%.y 1]
          [%.n ~ (flop "bcd")]
          [%.y 1]
      ==
    %+  expect-eq
      !>  diff
      !>  (lusk a b (loss a b))
    ::
    =/  a  "vxae"
    =/  b  "wyabcde"
    =/  diff
      :~  [%.n (flop "vx") (flop "wy")]
          [%.y 1]
          [%.n ~ (flop "bcd")]
          [%.y 1]
      ==
    %+  expect-eq
      !>  diff
      !>  (lusk a b (loss a b))
    ::
    =/  a  "xae"
    =/  b  "abcde"
    =/  diff
      :~  [%.n "x" ~]
          [%.y 1]
          [%.n ~ (flop "bcd")]
          [%.y 1]
      ==
    %+  expect-eq
      !>  diff
      !>  (lusk a b (loss a b))
    ::
    =/  a  "ae"
    =/  b  "xabcde"
    =/  diff
      :~  [%.n ~ "x"]
          [%.y 1]
          [%.n ~ (flop "bcd")]
          [%.y 1]
      ==
    %+  expect-eq
      !>  diff
      !>  (lusk a b (loss a b))
    ::
    =/  a  "aev"
    =/  b  "xabcdewx"
    =/  diff
      :~  [%.n ~ "x"]
          [%.y 1]
          [%.n ~ (flop "bcd")]
          [%.y 1]
          [%.n "v" (flop "wx")]
      ==
    %+  expect-eq
      !>  diff
      !>  (lusk a b (loss a b))
  ==
--
