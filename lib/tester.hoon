::  common testing library.
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
    ^-  (list tape)
    error-lines
  --
--
