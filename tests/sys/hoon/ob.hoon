/+  *test
|%
++  test-fein-fynd-inverses
  ;:  weld
    %+  expect-eq
      !>  0
      !>  (fynd:ob (fein:ob 0))
    ::
    %+  expect-eq
      !>  15.663.360
      !>  (fynd:ob (fein:ob 15.663.360))
    ::
    %+  expect-eq
      !>  1.208.402.137
      !>  (fynd:ob (fein:ob 1.208.402.137))
    ::
    %+  expect-eq
      !>  123.456.789.012.345
      !>  (fynd:ob (fein:ob 123.456.789.012.345))
    ::
    %+  expect-eq
      !>  4.267.685.634
      !>  (fynd:ob (fein:ob 4.267.685.634))
    ::
    %+  expect-eq
      !>  1.625.882.369
      !>  (fynd:ob (fein:ob 1.625.882.369))
    ::
  ==
::
++  test-fein-fynd-match-reference-vals
  ;:  weld
    %+  expect-eq
      !>  1.897.766.331
      !>  (fein:ob 123.456.789)
    ::
    %+  expect-eq
      !>  1.208.402.137
      !>  (fein:ob 15.663.360)
    ::
    %+  expect-eq
      !>  15.663.360
      !>  (fynd:ob 1.208.402.137)
    ::
    %+  expect-eq
      !>  123.456.789
      !>  (fynd:ob 1.897.766.331)
    ::
  ==
::
++  test-feis-tail-inverses
  ;:  weld
    %+  expect-eq
      !>  15.663.360
      !>  (tail:ob (feis:ob 15.663.360))
    ::
    %+  expect-eq
      !>  1.208.402.137
      !>  (tail:ob (feis:ob 1.208.402.137))
    ::
    %+  expect-eq
      !>  4.267.685.634
      !>  (tail:ob (feis:ob 4.267.685.634))
    ::
    %+  expect-eq
      !>  1.625.882.369
      !>  (tail:ob (feis:ob 1.625.882.369))
    ::
  ==
::
++  test-feis-tail-match-reference-vals
  ;:  weld
    %+  expect-eq
      !>  2.060.458.291
      !>  (feis:ob 123.456.789)
    ::
    %+  expect-eq
      !>  1.195.593.620
      !>  (feis:ob 15.663.360)
    ::
    %+  expect-eq
      !>  1.107.963.580
      !>  (tail:ob 123.456.789)
    ::
    %+  expect-eq
      !>  15.663.360
      !>  (tail:ob 1.195.593.620)
    ::
  ==
::
++  test-exhaustive-small
  =/  a=(list @)  ~[5 9 2 6 4 0 8 7 1 10 3 11]
  =/  b=(list @)  ~[2 1 0 3 10 4 9 5 7 11 6 8]
  =/  c=(list @)  ~[10 6 7 1 0 11 3 9 5 2 8 4]
  =/  d=(list @)  ~[11 0 3 5 9 8 6 10 4 1 2 7]
  ::
  =/  prf
  |=  [j=@ r=@]
  ^-  @
  ?:  =(j 0)
    (snag r a)
  ?:  =(j 1)
    (snag r b)
  ?:  =(j 2)
    (snag r c)
  (snag r d)
  ::
  ::
  =/  feis
  |=  arg=@
  ^-  @
  (fee:ob 4 3 4 12 prf arg)
  ::
  =/  tail
  |=  arg=@
  ^-  @
  (feen:ob 4 3 4 12 prf arg)
  ::
  =/  emm=(list @)  ~[0 1 2 3 4 5 6 7 8 9 10 11]
  =/  semm=(set @)  (sy emm)
  ::
  =/  perm=(list @)  (turn emm feis)
  =/  inv=(list @)  (turn perm tail)
  =/  distincts=(set @)  (sy perm)
  ::
  ;:  weld
    %+  expect-eq
      !>  (lent perm)
      !>  (lent ~(tap in distincts))
    ::
    %+  expect-eq
      !>  &
      !>  (roll perm |=([x=@ acc=?] &((~(has in semm) x) acc)))
    ::
    %+  expect-eq
      !>  emm
      !>  inv
    ::
  ==
--
