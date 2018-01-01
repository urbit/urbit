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
  |_  $:  error-lines=(list tape)                     :<  output messages
          eny=@uvJ                                    :<  entropy
          check-iterations=@u                         :<  # of check trials
          current-iteration=@u                        :<  current iteration
      ==
  :>  #
  :>  #  %check
  :>  #
  :>    gates for quick check style tests.
  +|
  +-  check
    |*  [generator=$-(@uvJ *) test=$-(* ?)]
    |-
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
    |=  [min=@ max=@]
    |=  c=@uvJ
    ^-  @
    =+  gen=(random:new-hoon c)
    =^  num  gen  (range:gen min max)
    num
  ::
  ++  generate-dict
    :>  generator which will produce a dict with {count} random pairs.
    |=  count=@u
    :>  generate a dict with entropy {c}.
    |=  c=@uvJ
    :>
    :>  gen: stateful random number generator
    :>  out: resulting map
    :>  i: loop counter
    :>
    =/  gen  (random:new-hoon c)
    =|  out=(dict:new-hoon @ud @ud)
    =|  i=@u
    |-
    ^-  (dict:new-hoon @ud @ud)
    ?:  =(i count)
      out
    =^  first  gen  (range:gen 0 100)
    =^  second  gen  (range:gen 0 100)
    $(out (put:dct:new-hoon out first second), i +(i))
  :>  #
  :>  #  %test
  :>  #
  :>    assertions on state
  +|
  ::  todo: unit testing libraries have a lot more to them than just eq.
  ++  expect-eq
    |*  [a=* b=* c=tape]
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
  :>  #
  :>  #  %output
  :>  #
  :>    called by the test harness after test completion
  ::
  ++  results
    :>  returns results.
    ^-  (list tape)
    error-lines
  --
--
