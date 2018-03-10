/+  new-hoon, tester
=,  dct:new-hoon
=+  four=(from-list [[1 "one"] [2 "two"] [3 "three"] [4 "four"] ~])
=+  three=(from-list [[1 "one"] [2 "two"] [3 "three"] ~])
|_  _tester:tester
++  test-empty
  (expect-eq !>([%.n (empty four)]))
::
++  test-size
  (expect-eq !>([4 (size four)]))
::
++  test-member
  (expect-eq !>([%.y (member four 4)]))
::
++  test-put-with
  %-  expect-eq  !>
  :-  (from-list [["one" 1] ["two" 2] ["three" 5] ["four" 4] ~])
  =/  ints  (from-list [["one" 1] ["two" 2] ["three" 3] ["four" 4] ~])
  (put-with ints "three" 2 add)
::
++  test-put-with-key
  %-  expect-eq  !>
  :-  (from-list [[1 "one"] [2 "two"] [3 "three"] [4 "4four"] ~])
  (put-with-key four 4 "four" |=({a/@ud b/tape c/tape} (weld (scow %ud a) b)))
::
++  test-put-lookup-with-key
  %-  expect-eq  !>
  :-  [`"four" (from-list [[1 "one"] [2 "two"] [3 "three"] [4 "five"] ~])]
  %^  put-lookup-with-key  four
    4
  :-  "five"
  |=({key/@ud old/tape new/tape} new)
::
++  test-delete
  %-  expect-eq  !>
  :-  three
  (delete four 4)
::
++  test-adjust
  %-  expect-eq  !>
  :-  (from-list [[1 "one"] [2 "two"] [3 "thisthree"] [4 "four"] ~])
  %^  adjust  four
    3
  |=(a/tape (weld "this" a))
::
++  test-adjust-with-key
  %-  expect-eq  !>
  :-  (from-list [[1 "one"] [2 "two"] [3 "3three"] [4 "four"] ~])
  %^  adjust-with-key  four
    3
  |=({a/@ud b/tape} (weld (scow %ud a) b))
::
++  test-update
  %-  expect-eq  !>
  :-  (from-list [[1 "one"] [2 "two"] [4 "four"] ~])
  %^  update  four
    3
  |=(a/tape `(maybe tape)`~)
::
++  test-update-with-key
  %-  expect-eq  !>
  :-  (from-list [[1 "one"] [2 "two"] [3 "3three"] [4 "four"] ~])
  %^  update-with-key  four
    3
  |=({a/@u b/tape} `(maybe tape)`[~ (weld (scow %ud a) b)])
::
++  test-alter-as-add
  %-  expect-eq  !>
  :-  (from-list [[1 "one"] [2 "two"] [3 "three"] [4 "four"] [5 "five"] ~])
  %^  alter  four
    5
  |=(a/(maybe tape) `(maybe tape)`[~ "five"])
::
++  test-alter-as-delete
  %-  expect-eq  !>
  :-  (from-list [[1 "one"] [3 "three"] [4 "four"] ~])
  %^  alter  four
    2
  |=(a/(maybe tape) `(maybe tape)`~)
::
++  test-alter-as-change
  %-  expect-eq  !>
  :-  (from-list [[1 "one"] [2 "dos"] [3 "three"] [4 "four"] ~])
  %^  alter  four
    2
  |=(a/(maybe tape) `(maybe tape)`[~ "dos"])
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
  %-  expect-eq  !>
  :-  (from-list [[1 "left"] [2 "left"] [3 "right"] ~])
  %+  union
    (from-list [[1 "left"] [2 "left"] ~])
  (from-list [[2 "right"] [3 "right"] ~])
::
++  test-union-with
  %-  expect-eq  !>
  :-  (from-list [[1 "left"] [2 "leftright"] [3 "right"] ~])
  %^    union-with
      (from-list [[1 "left"] [2 "left"] ~])
    (from-list [[2 "right"] [3 "right"] ~])
  |=({a/tape b/tape} (weld a b))
::
++  test-union-with-key
  %-  expect-eq  !>
  :-  (from-list [[1 "left"] [2 "2leftright"] [3 "right"] ~])
  %^    union-with-key
      (from-list [[1 "left"] [2 "left"] ~])
    (from-list [[2 "right"] [3 "right"] ~])
  |=({a/@ud b/tape c/tape} :(weld `tape`(scow %ud a) b c))
::
++  test-map
  %-  expect-eq  !>
  :-  (from-list [[1 'one'] [2 'two'] [3 'three'] ~])
  (map:dct three crip)
::
++  test-map-with-key
  %-  expect-eq  !>
  :-  (from-list [[1 "1one"] [2 "2two"] [3 "3three"] ~])
  %+  map-with-key  three
  |=({a/@u b/tape} (weld (scow %ud a) b))
::
++  test-map-fold
  %-  expect-eq  !>
  :-  :-  "Everything: two one three"
      (from-list [[1 "oneX"] [2 "twoX"] [3 "threeX"] ~])
  %^  map-fold  three
    "Everything:"
  |=  {accumulator/tape value/tape}
  [:(weld accumulator " " value) (weld value "X")]
::
++  test-map-keys
  %-  expect-eq  !>
  :-  (from-list [[11 "one"] [12 "two"] [13 "three"] ~])
  %+  map-keys  three
  |=(a/@u (add a 10))
::
++  test-map-keys-with
  %-  expect-eq  !>
  :-  (from-list [[42 "twothreeone"] ~])
  %^  map-keys-with  three
    |=(a/@u 42)
  weld
::
++  test-fold
  %-  expect-eq  !>
  :-  "Everything: twoonethree"
  %^  fold  three
    "Everything: "
  ::  todo: this works but replacing with just ++weld causes an out of loom.
  |=  {accumulator/tape value/tape}
  ^-  tape
  (weld accumulator value)
::
++  test-fold-with-keys
  %-  expect-eq  !>
  :-  "Everything: 2two1one3three"
  %^  fold-with-keys  three
    "Everything: "
  |=  {accumulator/tape key/@u value/tape}
  ^-  tape
  :(weld accumulator (scow %ud key) value)
::
++  test-elems
  %-  expect-eq  !>
  :-  ["two" "three" "one" ~]
  (elems three)
::
++  test-keys
  %-  expect-eq  !>
  :-  [2 3 1 ~]
  (keys three)
::
++  test-keys-set
  %-  expect-eq  !>
  :-  (si:nl [2 3 1 ~])
  (keys-set three)
::
++  test-from-set
  %-  expect-eq  !>
  :-  (from-list [[1 "1"] [2 "2"] [3 "3"] ~])
  %+  from-set
    (si:nl [1 2 3 ~])
  |=(a/@u (scow %ud a))
::
++  test-from-list-with
  %-  expect-eq  !>
  :-  (from-list [[1 1] [2 2] [3 3] ~])
  %+  from-list-with
    [[1 1] [2 1] [2 1] [3 3] ~]
  add
::
++  test-filter
  %-  expect-eq  !>
  :-  (from-list [[1 1] [2 1] [4 1] ~])
  %+  filter
    (from-list [[1 1] [2 1] [3 2] [4 1] ~])
  |=(a/@u !=(a 1))
::
++  test-filter-with-key
  %-  expect-eq  !>
  :-  (from-list [[1 1] [3 2] [4 1] ~])
  %+  filter-with-key
    (from-list [[1 1] [2 1] [3 2] [4 1] ~])
  |=({a/@u b/@u} =(a 2))
::
++  test-restrict-keys
  %-  expect-eq  !>
  :-  (from-list [[1 1] [3 3] [5 5] ~])
  %+  restrict-keys
    (from-list [[1 1] [2 2] [3 3] [4 4] [5 5] ~])
  (si:nl [1 3 5 ~])
::
++  test-without-keys
  %-  expect-eq  !>
  :-  (from-list [[2 2] [4 4] ~])
  %+  without-keys
    (from-list [[1 1] [2 2] [3 3] [4 4] [5 5] ~])
  (si:nl [1 3 5 ~])
::
++  test-partition
  %-  expect-eq  !>
  :-  :-  (from-list [[1 1] [3 3] ~])
      (from-list [[2 2] [4 4] [5 5] ~])
  %+  partition
    (from-list [[1 1] [2 2] [3 3] [4 4] [5 5] ~])
  |=(a/@u |(=(a 1) =(a 3)))
::
++  test-map-maybe
  %-  expect-eq  !>
  :-  (from-list [[1 1] [2 2] [4 4] [5 5] ~])
  %+  map-maybe
    (from-list [[1 1] [2 2] [3 3] [4 4] [5 5] ~])
  |=(a/@u ?:(=(a 3) ~ `a))
::
++  test-map-maybe-with-key
  %-  expect-eq  !>
  :-  (from-list [[1 2] [2 3] [4 5] [5 6] ~])
  %+  map-maybe-with-key
    (from-list [[1 2] [2 3] [3 4] [4 5] [5 6] ~])
  |=({k/@u v/@u} ?:(=(k 3) ~ `v))
::
++  test-map-either
  %-  expect-eq  !>
  :-  :-  (from-list [[2 "even"] [4 "even"] ~])
      (from-list [[1 1] [3 1] [5 1] ~])
  %+  map-either
    (from-list [[1 1] [2 2] [3 3] [4 4] [5 5] ~])
    |=  value/@u
    ?:  =(0 (mod value 2))
      [%& "even"]
    [%| 1]
::
++  test-map-either-with-key
  %-  expect-eq  !>
  :-  :-  (from-list [[2 "even"] [4 "even"] ~])
      (from-list [[1 1] [3 1] [5 1] ~])
  %+  map-either-with-key
    (from-list [[1 1] [2 1] [3 1] [4 1] [5 1] ~])
  |=  {key/@u value/@u}
  ?:  =(0 (mod key 2))
    [%& "even"]
  [%| 1]
::
++  test-is-subdict
  %-  expect-eq  !>
  :-  &
  %^    is-subdict-by
      (from-list [[1 1] [4 4] ~])
    (from-list [[1 1] [2 2] [3 3] [4 4] [5 5] ~])
  |=({a/* b/*} =(a b))
::
++  test-valid
  %-  expect-eq  !>
  :-  &
  (valid (from-list [[1 1] [2 2] [3 3] [4 4] [5 5] [6 6] [7 7] [8 8] [9 9] ~]))
--

