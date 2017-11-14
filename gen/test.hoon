::  todo: think about using horns to import all tests in %/tests?
/+  new-hoon
|%
::  ----------------------------------------------------------------------
::  Eventually should be in %/lib/tester/hoon.
::  ----------------------------------------------------------------------
++  test-lib
  |%
  ++  init-test-vase
    |=  {cookie/@uvJ}
    ^-  vase
    !>((init-test cookie))
  ::
  ++  init-test
    |=  {cookie/@uvJ}
    ~(. tester `(list tape)`~ cookie 10 0)
  ::
  ++  tester-type  _(init-test `@uvJ`0)
  ::
  ++  tester
    |_  $:  error-lines/(list tape)                     ::  output messages
            eny/@uvJ                                    ::  entropy
            check-iterations/@u                         ::  # of check trials
            current-iteration/@u                        ::  current iteration
        ==
    ::  ||  %check
    ::
    ::  +|
    +-  check
      |*  {generator/$-(@uvJ *) test/$-(* ?)}
      |-                                                ::  why do i have to |-?
      ^+  +>.$
      ?:  (gth current-iteration check-iterations)
        +>.$
      ::  todo: wrap generator in mule so it can crash.
      =+  sample=(generator eny)
      ::  todo: wrap test in mule so it can crash.
      =+  ret=(test sample)
      ?:  ret
        %=  $
          eny    (shaf %huh eny)                        ::  xxx: better random?
          current-iteration  (add current-iteration 1)
        ==
      =+  case=(add 1 current-iteration)
      =+  case-plural=?:(=(case 1) "case" "cases")
      %=  +>.$
        error-lines  :*
          "falsified after {(noah !>(case))} {case-plural} by '{(noah !>(sample))}'"
          error-lines
        ==
      ==
    ::
    ::  todo: a generate function that takes an arbitrary span.
    ::
    ++  generate-range
      |=  {min/@ max/@}
      |=  c/@uvJ
      ^-  @
      =+  gen=(random:new-hoon c)
      =^  num  gen  (range:gen min max)
      num
    ::
    ++  generate-dict
      :>  generator which will produce a dict with {count} random pairs.
      |=  count/@u
      :>  generate a dict with entropy {c}.
      |=  c/@uvJ
      =/  gen  (random:new-hoon c)
      =|  i/@u
      =|  m/(dict:new-hoon @ud @ud)
      |-
      ^-  (dict:new-hoon @ud @ud)
      ?:  =(i count)
        m
      =^  first  gen  (range:gen 0 100)
      =^  second  gen  (range:gen 0 100)
      $(m (put:dct:new-hoon m first second), i +(i))
    ::
    ::  ||  %test
    ::
    ::  +|
    ::  todo: unit testing libraries have a lot more to them than just eq.
    ++  expect-eq
      |*  {a/* b/* c/tape}
      ^+  +>
      ?:  =(a b)
        +>.$
      %=  +>.$
        error-lines  :*
          "failure: '{c}'"
          "  actual:   '{(noah !>(a))}'"
          "  expected: '{(noah !>(b))}'"
          error-lines
        ==
      ==
    ::
    ++  results
      ::  returns results.
      ::
      ::  returns the test run's identity cookie and the list of failures.
      ^-  {@uvJ (list tape)}
      [eny error-lines]
    --
  --
--
|%
::  ----------------------------------------------------------------------
::  Eventually should be in %/test/basic/hoon.
::  ----------------------------------------------------------------------
++  test-core
  |_  tester-type:test-lib
  ++  check-decrement
    %+  check
      (generate-range 0 100)
      |=(a/@ =(a (dec (add 2 a))))
  ++  test-decrement
    (expect-eq (dec 5) 4 "decrement failure")
  ++  test-freedom
    (expect-eq (add 2 2) 4 "freedom is the freedom to say...")
  ++  test-a-failure
    (expect-eq (add 2 2) 5 "freedom is the freedom to say...")
  ++  test-crash
    !!
  --
--
|%
::  ----------------------------------------------------------------------
::  Eventually should be in %/test/basic/hoon.
::  ----------------------------------------------------------------------
++  test-thr
  =,  thr:new-hoon
  =/  data/(list (either @u tape))  [[%& 1] [%| "one"] [%& 2] [%| "two"] ~]
  |_  tester-type:test-lib
  ++  test-apply
    %^  expect-eq
      %^  apply
      `(either @u tape)`[%| "one"]
      |=(a/@u "left")
      |=(b/tape "right")
    "right"
    "apply"
  ::
  ++  test-firsts
    %^  expect-eq
    (firsts data)
    [1 2 ~]
    "firsts"
  ::
  ++  test-seconds
    %^  expect-eq
    (seconds data)
    ["one" "two" ~]
    "seconds"
  ::
  ++  test-partition
    %^  expect-eq
    (partition data)
    [[1 2 ~] ["one" "two" ~]]
    "partition"
  --
--
|%
++  test-myb
  =,  myb:new-hoon
  |_  tester-type:test-lib
  ++  test-from-list-null
    (expect-eq (from-list ~) ~ "from-list")
  ::
  ++  test-from-list-real
    (expect-eq (from-list [5 ~]) [~ 5] "from-list")
  ::
  ++  test-to-list-null
    (expect-eq (to-list ~) ~ "to-list")
  ::
  ++  test-to-list-real
    (expect-eq (to-list [~ 5]) [5 ~] "to-list")
  ::
  ++  test-concat-null
    (expect-eq (concat ~) ~ "concat")
  ::
  ++  test-concat-real
    ::  wait, if i pull the cast out from below, the concat implementation
    ::  doesn't compile anymore?
    (expect-eq (concat `(list (maybe @ud))`[~ [~ 1] ~ [~ 2] ~]) [1 2 ~] "concat")
  ::
  ++  test-map
    %^  expect-eq
      %+  map:myb
      [1 2 3 2 ~]
      |=(a/@u ?:(=(2 a) [~ 2] ~))
    [2 2 ~]
    "map"
  --
--
|%
++  test-ls
  =,  ls:new-hoon
  |_  tester-type:test-lib
  ++  test-head
    (expect-eq (head [1 ~]) 1 "head")
  ::
  ++  test-last
    (expect-eq (last:ls [1 2 ~]) 2 "last")
  ::
  ++  test-tail
    (expect-eq (tail [1 2 3 ~]) [2 3 ~] "tail")
  ::
  ++  test-init
    (expect-eq (init [1 2 3 ~]) [1 2 ~] "init")
  ::
  ++  test-size
    (expect-eq (size ['a' 'b' 'c' ~]) 3 "size")
  ::
  ++  test-map
    (expect-eq (map:ls [1 2 ~] |=(a/@ (add 1 a))) [2 3 ~] "map")
  ::
  ++  test-reverse
    (expect-eq (reverse [1 2 3 ~]) [3 2 1 ~] "reverse")
  ::
  ++  test-intersperse
    (expect-eq (intersperse 1 [5 5 5 ~]) [5 1 5 1 5 ~] "intersperse")
  ::
  ++  test-intercalate
    %^  expect-eq
    (intercalate "," ["one" "two" "three" ~])
    ["one,two,three"]
    "intercalate"
  ::
  ++  test-transpose
    %^  expect-eq
    (transpose ~[~[1 2 3] ~[4 5 6]])
    ~[~[1 4] ~[2 5] ~[3 6]]
    "transpose"
  ::
  ++  test-foldl
    (expect-eq (foldl [1 2 3 ~] 3 |=({a/@ b/@} (add a b))) 9 "foldl")
  ::
  ++  test-foldr
    (expect-eq (foldr [1 2 3 ~] 1 |=({a/@ b/@} (add a b))) 7 "foldr")
  ::
  ++  test-concat
    (expect-eq (concat ~[~[1 2] ~[3 4]]) ~[1 2 3 4] "concat")
  ::
  ++  test-weld
    (expect-eq (weld:ls ~[1 2 3] ~["one" "two"]) ~[1 2 3 "one" "two"] "weld")
  ::
  ++  test-any-true
    (expect-eq (any [1 2 3 ~] |=(a/@ =(a 2))) %.y "any true")
  ::
  ++  test-any-false
    (expect-eq (any [1 2 3 ~] |=(a/@ =(a 8))) %.n "any false")
  ::
  ++  test-all-true
    (expect-eq (all [1 1 1 ~] |=(a/@ =(a 1))) %.y "all true")
  ::
  ++  test-all-false
    (expect-eq (all [1 3 1 ~] |=(a/@ =(a 1))) %.n "all false")
  ::
  ++  test-scanl
    %^  expect-eq
    (scanl ~[1 2 3] 0 |=({a/@ b/@} (add a b)))
    ~[0 1 3 6]
    "scanl"
  ::
  ++  test-scanl1
    %^  expect-eq
    (scanl1 ~[1 2 3] |=({a/@ b/@} (add a b)))
    ~[1 3 6]
    "scanl1"
  ::
  ++  test-scanr
    %^  expect-eq
    (scanr ~[1 2 3] 0 |=({a/@ b/@} (add a b)))
    ~[6 5 3 0]
    "scanr"
  ::
  ++  test-scanr1
    %^  expect-eq
    (scanr1 ~[1 2 3] |=({a/@ b/@} (add a b)))
    ~[6 5 3]
    "scanr1"
  ::
  ++  test-map-foldl
    %^  expect-eq
    (map-foldl ~[1 2 3] 1 |=({a/@ b/@} [(add a b) (add 1 a)]))
    [7 ~[2 3 5]]
    "map-foldl"
  ::
  ++  test-map-foldr
    %^  expect-eq
    (map-foldr ~[1 2 3] 1 |=({a/@ b/@} [(add a b) (add 1 a)]))
    [7 ~[7 5 2]]
    "map-foldr"
  ::
  ++  test-unfoldr
    %^  expect-eq
    (unfoldr 5 |=(a/@ ?:(=(a 0) ~ `[a (dec a)])))
    [5 4 3 2 1 ~]
    "unfoldr"
  ::
  ++  test-take
    %^  expect-eq
    (take 3 ~[1 2 3 4 5])
    [1 2 3 ~]
    "take"
  ::
  ++  test-drop
    %^  expect-eq
    (drop:ls 3 ~[1 2 3 4 5])
    [4 5 ~]
    "drop"
  ::
  ++  test-split-at
    %^  expect-eq
    (split-at 3 ~[1 2 3 4 5])
    [[1 2 3 ~] [4 5 ~]]
    "split-at"
  ::
  ++  test-take-while
    %^  expect-eq
    (take-while ~[1 2 3 4 5] |=(a/@ (lth a 3)))
    [1 2 ~]
    "take-while"
  ::
  ++  test-drop-while
    %^  expect-eq
    (drop-while ~[1 2 3 4 5] |=(a/@ (lth a 3)))
    [3 4 5 ~]
    "drop-while"
  ::
  ++  test-drop-while-end
    %^  expect-eq
    (drop-while-end ~[5 5 1 5 5] |=(a/@ =(a 5)))
    [5 5 1 ~]
    "drop-while-end"
  ::
  ++  test-split-on
    %^  expect-eq
    (split-on ~[1 2 3 4 1 2 3 4] |=(a/@ (lth a 3)))
    [[1 2 ~] [3 4 1 2 3 4 ~]]
    "split-on"
  ::
  ++  test-break
    %^  expect-eq
    (break ~[1 2 3 4 1 2 3 4] |=(a/@ (gth a 3)))
    [[1 2 3 ~] [4 1 2 3 4 ~]]
    "break"
  ::
  ++  test-strip-prefix
    %^  expect-eq
    (strip-prefix "foo" "foobar")
    [~ "bar"]
    "break"
  ::
  ++  test-inits
    %^  expect-eq
    (inits "abc")
    ["a" "ab" "abc" ~]
    "inits"
  ::
  ++  test-tails
    %^  expect-eq
    (tails "abc")
    ["abc" "bc" "c" ~]
    "tails"
  ::
  ++  test-is-prefix-of
    %^  expect-eq
    (is-prefix-of "foo" "foobar")
    %.y
    "is-prefix-of"
  ::
  ++  test-is-suffix-of
    %^  expect-eq
    (is-suffix-of "bar" "foobar")
    %.y
    "is-suffix-of"
  ::
  ++  test-is-infix-of
    %^  expect-eq
    (is-infix-of "ob" "foobar")
    %.y
    "is-infix-of"
  ::
  ++  test-elem
    %^  expect-eq
    (elem 5 [1 2 3 4 5 ~])
    %.y
    "elem"
  ::
  ++  test-lookup
    %^  expect-eq
    (lookup "two" [["one" 1] ["two" 2] ["three" 3] ~])
    [~ 2]
    "lookup"
  ::
  ++  test-find
    %^  expect-eq
    (find:ls [3 2 1 5 1 2 3 ~] |=(a/@ (gth a 3)))
    [~ 5]
    "find"
  ::
  ++  test-filter
    %^  expect-eq
    (filter [1 2 1 2 1 ~] |=(a/@ =(a 2)))
    [1 1 1 ~]
    "filter"
  ::
  ++  test-partition
    %^  expect-eq
    (partition [1 2 1 2 1 ~] |=(a/@ =(a 2)))
    [[2 2 ~] [1 1 1 ~]]
    "partition"
  ::
  ++  test-elem-index
    %^  expect-eq
    (elem-index 2 [1 2 3 4 ~])
    `1
    "elem-index"
  ::
  ++  test-elem-indices
    %^  expect-eq
    (elem-indices 2 [1 2 1 2 ~])
    [1 3 ~]
    "elem-indices"
  ::
  ++  test-find-index
    %^  expect-eq
    (find-index [1 2 3 ~] |=(a/@ =(a 2)))
    `1
    "find-index"
  ::
  ++  test-find-indices
    %^  expect-eq
    (find-indices [1 2 1 2 ~] |=(a/@ =(a 2)))
    [1 3 ~]
    "find-indices"
  ::
  ++  test-zip
    %^  expect-eq
    (zip [[1 2 3 ~] [4 5 6 ~] [7 8 9 ~] ~])
    [[1 4 7 ~] [2 5 8 ~] [3 6 9 ~] ~]
    "zip"
  ::
  ++  test-unique
    %^  expect-eq
    (unique [1 2 3 1 2 3 ~])
    [1 2 3 ~]
    "unique"
  ::
  ++  test-delete
    %^  expect-eq
    (delete 2 [1 2 3 2 ~])
    [1 3 2 ~]
    "delete"
  ::
  ++  test-delete-firsts
    %^  expect-eq
    (delete-firsts [1 2 2 2 3 4 5 ~] [2 2 5 ~])
    [1 2 3 4 ~]
    "delete-firsts"
  ::
  ++  test-union
    %^  expect-eq
    (union [1 2 3 ~] [4 2 5 ~])
    [1 2 3 4 5 ~]
    "union"
  ::
  ++  test-intersect
    %^  expect-eq
    (intersect [5 6 6 7 8 ~] [9 8 8 6 ~])
    [6 6 8 ~]
    "intersect"
  --
--
|%
++  test-mp
  =,  dct:new-hoon
  =+  four=(from-list [[1 "one"] [2 "two"] [3 "three"] [4 "four"] ~])
  =+  three=(from-list [[1 "one"] [2 "two"] [3 "three"] ~])
  |_  tester-type:test-lib
  ++  test-empty
    (expect-eq (empty four) %.n "empty")
  ::
  ++  test-size
    (expect-eq (size four) 4 "size")
  ::
  ++  test-member
    (expect-eq (member four 4) %.y "member")
  ::
  ++  test-put-with
    =+  ints=(from-list [["one" 1] ["two" 2] ["three" 3] ["four" 4] ~])
    %^  expect-eq
    (put-with ints "three" 2 add)
    (from-list [["one" 1] ["two" 2] ["three" 5] ["four" 4] ~])
    "put-with"
  ::
  ++  test-put-with-key
    %^  expect-eq
    (put-with-key four 4 "four" |=({a/@ud b/tape c/tape} (weld (scow %ud a) b)))
    (from-list [[1 "one"] [2 "two"] [3 "three"] [4 "4four"] ~])
    "put-with-key"
  ::
  ++  test-put-lookup-with-key
    %^  expect-eq
      %-  put-lookup-with-key  :^
        four
        4
        "five"
        |=({key/@ud old/tape new/tape} new)
    :-  `"four"
      (from-list [[1 "one"] [2 "two"] [3 "three"] [4 "five"] ~])
    "put-lookup-with-key"
  ::
  ++  test-delete
    %^  expect-eq
    (delete four 4)
    three
    "delete"
  ::
  ++  test-adjust
    %^  expect-eq
      %^  adjust
      four
      3
      |=(a/tape (weld "this" a))
    (from-list [[1 "one"] [2 "two"] [3 "thisthree"] [4 "four"] ~])
    "adjust"
  ::
  ++  test-adjust-with-key
    %^  expect-eq
      %^  adjust-with-key
      four
      3
      |=({a/@ud b/tape} (weld (scow %ud a) b))
    (from-list [[1 "one"] [2 "two"] [3 "3three"] [4 "four"] ~])
    "adjust-with-key"
  ::
  ++  test-update
    %^  expect-eq
      %^  update
      four
      3
      |=(a/tape `(maybe tape)`~)
    (from-list [[1 "one"] [2 "two"] [4 "four"] ~])
    "update"
  ::
  ++  test-update-with-key
    %^  expect-eq
      %^  update-with-key
      four
      3
      |=({a/@u b/tape} `(maybe tape)`[~ (weld (scow %ud a) b)])
    (from-list [[1 "one"] [2 "two"] [3 "3three"] [4 "four"] ~])
    "update-with-key"
  ::
  ++  test-alter-as-add
    %^  expect-eq
      %^  alter
      four
      5
      |=(a/(maybe tape) `(maybe tape)`[~ "five"])
    (from-list [[1 "one"] [2 "two"] [3 "three"] [4 "four"] [5 "five"] ~])
    "alter (as add)"
  ::
  ++  test-alter-as-delete
    %^  expect-eq
      %^  alter
      four
      2
      |=(a/(maybe tape) `(maybe tape)`~)
    (from-list [[1 "one"] [3 "three"] [4 "four"] ~])
    "alter (as delete)"
  ::
  ++  test-alter-as-change
    %^  expect-eq
      %^  alter
      four
      2
      |=(a/(maybe tape) `(maybe tape)`[~ "dos"])
    (from-list [[1 "one"] [2 "dos"] [3 "three"] [4 "four"] ~])
    "alter (as change)"
  ::
  ++  check-alter
    ::  check random dicts of 50 items with 40 random operations done on them
    ::  for validity.
    %+  check
      (generate-dict 50)
    |=  a/(dict @ud @ud)
    ::  this is dumb, but use {a} as entropy?
    =/  gen  (random:new-hoon (jam a))
    =|  i/@u
    |-
    ?:  =(i 40)
      %.y
    =^  key  gen  (range:gen 0 100)
    =^  value  gen  (range:gen 0 100)
    =.  a  %^  alter-with-key  a  key
      |=  {key/@ud current/(maybe @ud)}
      ^-  (maybe @ud)
      =+  action=(mod key 2)
      ?:  =(action 0)                                   ::  return nothing
        ~
      ?:  =(action 1)                                   ::  add/set value
        `value
      ~                                                 ::  impossible
    ?.  (valid a)
      %.n
    $(i +(i))
  ::
  ++  test-union
    %^  expect-eq
      %+  union
      (from-list [[1 "left"] [2 "left"] ~])
      (from-list [[2 "right"] [3 "right"] ~])
    (from-list [[1 "left"] [2 "left"] [3 "right"] ~])
    "union"
  ::
  ++  test-union-with
    %^  expect-eq
      %^  union-with
      (from-list [[1 "left"] [2 "left"] ~])
      (from-list [[2 "right"] [3 "right"] ~])
      |=({a/tape b/tape} (weld a b))
    (from-list [[1 "left"] [2 "leftright"] [3 "right"] ~])
    "union-with"
  ::
  ++  test-union-with-key
    %^  expect-eq
      %^  union-with-key
      (from-list [[1 "left"] [2 "left"] ~])
      (from-list [[2 "right"] [3 "right"] ~])
      |=({a/@ud b/tape c/tape} :(weld `tape`(scow %ud a) b c))
    (from-list [[1 "left"] [2 "2leftright"] [3 "right"] ~])
    "union-with-key"
  ::
  ++  test-map
    %^  expect-eq
      %+  map:dct
      three
      crip
    (from-list [[1 'one'] [2 'two'] [3 'three'] ~])
    "map"
  ::
  ++  test-map-with-key
    %^  expect-eq
      %+  map-with-key
      three
      |=({a/@u b/tape} (weld (scow %ud a) b))
    (from-list [[1 "1one"] [2 "2two"] [3 "3three"] ~])
    "map-with-key"
  ::
  ++  test-map-fold
    %^  expect-eq
      %^  map-fold
      three
      "Everything: "
      |=  {accumulator/tape value/tape}
      [(weld accumulator value) (weld value "X")]
    :-  "Everything: twoonethree"
      (from-list [[1 "oneX"] [2 "twoX"] [3 "threeX"] ~])
    "map-fold"
  ::
  ++  test-map-keys
    %^  expect-eq
      %+  map-keys
      three
      |=  a/@u
      (add a 10)
    (from-list [[11 "one"] [12 "two"] [13 "three"] ~])
    "map-keys"
  ::
  ++  test-map-keys-with
    %^  expect-eq
      %^  map-keys-with
      three
      |=(a/@u 42)
      weld
    (from-list [[42 "twothreeone"] ~])
    "map-keys-with"
  ::
  ++  test-fold
    %^  expect-eq
      %^  fold
      three
      "Everything: "
      ::  todo: this works but replacing with just ++weld causes an out of loom.
      |=  {accumulator/tape value/tape}
      ^-  tape
      (weld accumulator value)
    "Everything: twoonethree"
    "map-fold"
  ::
  ++  test-fold-with-keys
    %^  expect-eq
      %^  fold-with-keys
      three
      "Everything: "
      |=  {accumulator/tape key/@u value/tape}
      ^-  tape
      :(weld accumulator (scow %ud key) value)
    "Everything: 2two1one3three"
    "map-fold-with-keys"
  ::
  ++  test-elems
    %^  expect-eq
    (elems three)
    ["two" "three" "one" ~]
    "elems"
  ::
  ++  test-keys
    %^  expect-eq
    (keys three)
    [2 3 1 ~]
    "keys"
  ::
  ++  test-keys-set
    %^  expect-eq
    (keys-set three)
    (si:nl [2 3 1 ~])
    "keys-set"
  ::
  ++  test-from-set
    %^  expect-eq
      %+  from-set
      (si:nl [1 2 3 ~])
      |=  a/@u
      (scow %ud a)
    (from-list [[1 "1"] [2 "2"] [3 "3"] ~])
    "from-set"
  ::
  ++  test-from-list-with
    %^  expect-eq
      %+  from-list-with
      [[1 1] [2 1] [2 1] [3 3] ~]
      add
    (from-list [[1 1] [2 2] [3 3] ~])
    "from-list-with"
  ::
  ++  test-filter
    %^  expect-eq
      %+  filter
      (from-list [[1 1] [2 1] [3 2] [4 1] ~])
      |=(a/@u !=(a 1))
    (from-list [[1 1] [2 1] [4 1] ~])
    "filter"
  ::
  ++  test-filter-with-key
    %^  expect-eq
      %+  filter-with-key
      (from-list [[1 1] [2 1] [3 2] [4 1] ~])
      |=({a/@u b/@u} =(a 2))
    (from-list [[1 1] [3 2] [4 1] ~])
    "filter-with-key"
  ::
  ++  test-restrict-keys
    %^  expect-eq
      %+  restrict-keys
      (from-list [[1 1] [2 2] [3 3] [4 4] [5 5] ~])
      (si:nl [1 3 5 ~])
    (from-list [[1 1] [3 3] [5 5] ~])
    "restrict-keys"
  ::
  ++  test-without-keys
    %^  expect-eq
      %+  without-keys
      (from-list [[1 1] [2 2] [3 3] [4 4] [5 5] ~])
      (si:nl [1 3 5 ~])
    (from-list [[2 2] [4 4] ~])
    "restrict-keys"
  ::
  ++  test-partition
    %^  expect-eq
      %+  partition
      (from-list [[1 1] [2 2] [3 3] [4 4] [5 5] ~])
      |=(a/@u |(=(a 1) =(a 3)))
    :-  (from-list [[1 1] [3 3] ~])
      (from-list [[2 2] [4 4] [5 5] ~])
    "partition"
  ::
  ++  test-map-maybe
    %^  expect-eq
      %+  map-maybe
      (from-list [[1 1] [2 2] [3 3] [4 4] [5 5] ~])
      |=(a/@u ?:(=(a 3) ~ `a))
    (from-list [[1 1] [2 2] [4 4] [5 5] ~])
    "map-maybe"
  ::
  ++  test-map-maybe-with-key
    %^  expect-eq
      %+  map-maybe-with-key
      (from-list [[1 2] [2 3] [3 4] [4 5] [5 6] ~])
      |=({k/@u v/@u} ?:(=(k 3) ~ `v))
    (from-list [[1 2] [2 3] [4 5] [5 6] ~])
    "map-maybe-with-key"
  ::
  ++  test-map-either
    %^  expect-eq
    %+  map-either
      (from-list [[1 1] [2 2] [3 3] [4 4] [5 5] ~])
      |=  value/@u
      ?:  =(0 (mod value 2))
        [%& "even"]
      [%| 1]
    :-  (from-list [[2 "even"] [4 "even"] ~])
      (from-list [[1 1] [3 1] [5 1] ~])
    "map-either"
  ::
  ++  test-map-either-with-key
    %^  expect-eq
    %+  map-either-with-key
      (from-list [[1 1] [2 1] [3 1] [4 1] [5 1] ~])
      |=  {key/@u value/@u}
      ?:  =(0 (mod key 2))
        [%& "even"]
      [%| 1]
    :-  (from-list [[2 "even"] [4 "even"] ~])
      (from-list [[1 1] [3 1] [5 1] ~])
    "map-either"
  ::
  ++  test-is-subdict
    %^  expect-eq
      %^  is-subdict-by
      (from-list [[1 1] [4 4] ~])
      (from-list [[1 1] [2 2] [3 3] [4 4] [5 5] ~])
      |=({a/* b/*} =(a b))
    %.y
    "is-subdict"
  ::
  ++  test-valid
    %^  expect-eq
    (valid (from-list [[1 1] [2 2] [3 3] [4 4] [5 5] [6 6] [7 7] [8 8] [9 9] ~]))
    %.y
    "valid"
  --
--
|%
::  ----------------------------------------------------------------------
::  Stays in the generator.
::  ----------------------------------------------------------------------
++  local
  |%
  ++  perform-test-suite
    ::  takes a testing core and executes all tests in it.
    |=  {name/tape v/vase eny/@uvJ}
    ^-  tang
    =+  core-arms=(sort (sloe p.v) aor)
    ?:  =(~ core-arms)
      [[%leaf :(weld "error: " name " is not a valid testing core.")] ~]
    =|  out/tang
    |-
    ?~  core-arms
      out
    %=  $
      out  (weld (perform-test-arm name i.core-arms v eny) out)
      core-arms  t.core-arms
    ==
  ::
  ++  perform-test-arm
    ::  performs a single test.
    |=  {suite-name/tape arm-name/term v/vase eny/@uvJ}
    ::  todo: terminal color on the output
    ^-  tang
    =+  run=(run-arm-in-test-core arm-name v eny)
    =+  full-name=:(weld suite-name "/" (trip arm-name))
    ?-  -.run
      $|  ::  the stack is already flopped for output?
          ;:  weld
            p:run
            `tang`[[%leaf (weld full-name " CRASHED")] ~]
          ==
      $&  ::  todo: test the cookie to make sure it returned the same core.
          ?:  =(~ +.p:run)
            [[%leaf (weld full-name " OK")] ~]
          ::  Create a welded list of all failures indented.
          %-  flop
          ;:  weld
            `tang`[[%leaf (weld full-name " FAILED")] ~]
            %+  turn  +.p:run
              |=  {i/tape}
              ^-  tank
              [%leaf (weld "  " i)]
          ==
    ==
  ::
  ++  run-arm-in-test-core
    ::  runs a single arm.
    ::
    ::  returns the output of `++mule` so that we can react to crashes
    ::  appropriately.
    |=  {arm-name/term v/vase eny/@uvJ}
    ^-  (each {@uvJ (list tape)} (list tank))
    =/  t  (init-test-vase:test-lib eny)
    ::  run the tests in the interpreter so we catch crashes.
    %-  mule  |.
    =/  r  (slap (slop t v) [%cnsg [arm-name ~] [%$ 3] [[%$ 2] ~]])
    ::  return just the results or we will be here forever while we try to copy
    ::  the entire kernel.
    ((hard {@uvJ (list tape)}) q:(slap r [%limb %results]))
  --
::  ----------------------------------------------------------------------
--
:-  %say
|=  $:  {now/@da eny/@uvJ bec/beak}
        $~
        $~
    ==
:-  %tang
::  todo: right now, we hard code ++test-core. but eventually, we must instead
::  scry ford for the core from the hoon file. that doesn't exist yet.
::(perform-test-suite:local "test-core" !>(test-core) eny)
::(perform-test-suite:local "test-thr" !>(test-thr) eny)
::(perform-test-suite:local "test-myb" !>(test-myb) eny)
(perform-test-suite:local "test-ls" !>(test-ls) eny)
::(perform-test-suite:local "test-mp" !>(test-mp) eny)
