::  Tests for +list (list logic)
::
/+  *test
::
::  Testing arms
::
|%
++  test-list-snoc
  =/  a-list=(list @)   ~[1 2 3 4]
  =/  b-list=(list @)   ~[1 2 3 4 5]
  ;:  weld
    %+  expect-eq
      !>  b-list
      !>  (snoc a-list 5)
    %+  expect-eq
      !>  a-list
      !>  (snoc (snoc (snoc (snoc `(list)`~ 1) 2) 3) 4)
   ==
::
++  test-list-fand
  =/  l=(list @)  ~[1 1 1 2 3]
  ;:  weld
    ::  Checks checks looking for a single item
    ::
    %+  expect-eq
      !>  `(list @)`~[0 1 2]
      !>  (fand `(list @)`~[1] l)
    ::  checks looking for a sub list
    ::
    %+  expect-eq
      !>  `(list @)`~[2]
      !>  (fand `(list @)`~[1 2 3] l)
    ::  Checks when the value is not present
    ::
    %+  expect-eq
      !>  ~
      !>  (fand `(list @)`~[6] `(list @)`~[1 1 1 2 3])
  ==
::
++  test-list-find
  =/  l=(list @)  ~[1 1 1 2 3]
  ;:  weld
    ::  Checks checks looking for a single item
    ::
    %+  expect-eq
      !>  (some 0)
      !>  (find `(list @)`~[1] l)
    ::  checks looking for a sub list
    ::
    %+  expect-eq
      !>  (some 2)
      !>  (find `(list @)`~[1 2 3] l)
    ::  Checks when the value is not present
    ::
    %+  expect-eq
      !>  ~
      !>  (find `(list @)`~[6] l)
  ==
::
++  test-list-flop
  ;:  weld
    ::  A list reversed 2x is the same list
    ::
    %+  expect-eq
      !>  `(list @)`~[1 1 1 2 3]
      !>  (flop (flop `(list @)`~[1 1 1 2 3]))
    :: An empty list reversed is still empty
    ::
    %+  expect-eq
      !>  ~
      !>  (flop `(list @)`~)
  ==
::
++  test-list-gulf
  ::  check for an increasing list
  ::
  %+  expect-eq
    !>  ~[0 1 2 3]
    !>  (gulf 0 3)
::
++  test-list-join
  %+  expect-eq
    !>  ~[2 1 3]
    !>  (join 1 `(list @)`~[2 3])
::
++  test-list-lent
  ;:  weld
    %+  expect-eq
      !>  2
      !>  (lent `(list @)`~[2 3])
    %+  expect-eq
      !>  0
      !>  (lent *(list @))
  ==
::
++  test-list-levy
  =/  l=(list @)  ~[2 3]
  ;:  weld
    :: Tail fails condition
    ::
    %+  expect-eq
      !>  %.n
      !>  (levy l |=(x=@ (gth 3 x)))
    :: Head fails condition
    ::
    %+  expect-eq
      !>  %.n
      !>  (levy l |=([x=@] (lth 2 x)))
    :: Both pass condition
    ::
    %+  expect-eq
      !>  %.y
      !>  (levy l |=([x=@] (lth x 10)))
  ==
::
++  test-list-lien
  =/  l=(list @)  ~[2 3]
  ;:  weld
    :: only Tail fails condition
    ::
    %+  expect-eq
      !>  %.y
      !>  (lien l |=(x=@ (gth 3 x)))
    :: only head fails condition
    ::
    %+  expect-eq
      !>  %.y
      !>  (lien l |=([x=@] (lth 2 x)))
    :: Both pass condition
    ::
    %+  expect-eq
      !>  %.y
      !>  (lien l |=([x=@] (lth x 10)))
      :: Neither
      ::
    %+  expect-eq
      !>  %.y
      !>  (lien l |=([x=@] (gth 10 x)))
  ==
::
++  test-list-murn
  %+  expect-eq
    !>  ~[6 10]
    !>  %+  murn  `(list @)`~[2 3 4 5]
          |=  [x=@]
          ^-  (unit)
          ?:  =((mod x 2) 0)  ~
          (some (mul x 2))
::
++  test-list-oust
  ;:  weld
    %+  expect-eq
      !>  ~[2 5]
      !>  (oust [1 2] `(list @)`~[2 3 4 5])
    %+  expect-eq
      !>  ~[2 3 4 5]
      !>  (oust [1 0] `(list @)`~[2 3 4 5])
  ==
::
++  test-list-reap
  %+  expect-eq
    !>  ~[1 1 1]
    !>  (reap 3 1)
::
++  test-list-reel
  :: Use non-associative operation
  ::
  %+  expect-eq
    !>  93
    !>  (reel `(list @)`~[100 10 5 2] sub)
::
++  test-list-roll
  :: Use non-associative operation
  ::
  %+  expect-eq
    !>  6
    !>  (roll `(list @)`~[1 5 10] sub)
::
++  test-list-scag
  %+  expect-eq
    !>  ~[1 2]
    !>  (scag 2 `(list @)`~[1 2 3])
::
++  test-list-skid
  %+  expect-eq
    !>  [p=`(list @)`~[2 4 6 8] q=`(list @)`~[3 5 7]]
    !>  %+  skid  (gulf 2 8)
        |=  [x=@]
        =((mod x 2) 0)
::
++  test-list-skim
  %+  expect-eq
    !>  ~[2 4 6 8]
    !>  %+  skim  (gulf 2 8)
        |=  [x=@]
        =((mod x 2) 0)
::
++  test-list-skip
  %+  expect-eq
    !>  ~[3 5 7]
    !>  %+  skip  (gulf 2 8)
        |=  [x=@]
        =((mod x 2) 0)
::
++  test-list-slag
  %+  expect-eq
    !>  ~[3]
    !>  (slag 2 `(list @)`~[1 2 3])
::
++  test-list-snag
    %+  expect-eq
      !>  3
      !>  (snag 2 `(list @)`~[1 2 3])
::
++  test-list-sort
  =/  l=(list @)  ~[2 1 3]
  ;:  weld
    %+  expect-eq
      !>  ~[1 2 3]
      !>  (sort l lth)
    %+  expect-eq
      !>  (sort l lth)
      !>  (sort (sort l lth) lth)
  ==
::
++  test-list-spin
  %+  expect-eq
    !>  [p=~[0 3 8] q=3]
    !>  %^  spin  `(list @)`~[2 3 4]  0
        |=  [x=@ y=@]
        [(mul x y) +(y)]
::
++  test-list-spun
  %+  expect-eq
    !>  ~[0 3 8]
    !>  %+  spun  `(list @)`~[2 3 4]
        |=  [x=@ y=@]
        [(mul x y) +(y)]
::
++  test-list-turn
  %+  expect-eq
    !>  ~[3 4 5]
    !>  %+  turn  `(list @)`~[2 3 4]
        |=(x=@ +(x))
::
++  test-list-weld
  %+  expect-eq
    !>  ~[2 3 4 5 6]
    !>  %+  weld  `(list @)`~[2 3 4]
        (limo ~[5 6])
::
++  test-list-into
  %+  expect-eq
    !>  ~[2 11 3 4]
    !>  (into `(list @)`~[2 3 4] 1 11)
::
++  test-list-snap
  %+  expect-eq
    !>  ~[2 11 4]
    !>  (snap `(list @)`~[2 3 4] 1 11)
--
