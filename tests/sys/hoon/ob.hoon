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
--
