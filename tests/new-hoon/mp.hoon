/+  new-hoon, tester
=,  dct:new-hoon
=+  four=(from-list [[1 "one"] [2 "two"] [3 "three"] [4 "four"] ~])
=+  three=(from-list [[1 "one"] [2 "two"] [3 "three"] ~])
|_  tester-type:tester
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

