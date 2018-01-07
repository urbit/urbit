/+  new-hoon
::
:>  testing utilities
|%
:>  #  %models
+|
+=  tests
  :>    a hierarchical structure of tests
  :>
  :>  a recursive association list mapping a part of a path
  :>  to either a test trap or a sublist of the same type.
  (list instance)
::
+=  instance
  :>  a mapping between a term and part of a test tree.
  (pair term (each $-(@uvJ (list tape)) tests))
::
:>  #  %generate
:>    utilities for generating ++tests from files and directories.
+|
++  merge-base-and-recur
  :>    combine the current file and subdirectory.
  :>
  :>  this merges the file {base} with its child files {recur}.
  |=  [base=vase recur=(map @ta tests:tester)]
  ^-  tests
  =+  a=(gen-tests base)
  =+  b=(test-map-to-test-list recur)
  ::  todo: why does ++weld not work here? {a} and {b} are cast and have the
  ::  correct faces.
  (welp a b)
::
++  test-map-to-test-list
  :>    translates ford output to something we can work with.
  :>
  :>  ford gives us a `(map @ta tests:tester)`, but we actually
  :>  want something like ++tests.
  |=  a=(map @ta tests:tester)
  ::  todo: i'd like to sort this, but ++sort has -find.a problems much like
  ::  ++weld does above!?
  ^-  tests
  %+  turn
    (to-list:dct:new-hoon a)
  |=  {key/@ta value/tests:tester}
  [key [%| value]]
::
++  gen-tests
  :>  creates a {tests} list out of a vase of a test suite
  |=  v=vase
  ^-  tests
  =+  arms=(sort (sloe p.v) aor)
  %+  turn  arms
  |=  arm/term
  :-  arm
  :-  %&
  |=  eny=@uvJ
  =+  context=(slop !>((init-test eny)) v)
  =/  r  (slap context [%cnsg [arm ~] [%$ 3] [[%$ 2] ~]])
  ((hard (list tape)) q:(slap r [%limb %results]))
::
:>  #  %per-test
:>    data initialized on a per-test basis.
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
  :>    test expectation functions
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
  :>    called by the test harness
  ::
  ++  results
    :>  returns results.
    ^-  (list tape)
    error-lines
  --
--
