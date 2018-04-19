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
  (pair term (each $-(@uvJ (list tank)) tests))
::
:>  #  %generate
:>    utilities for generating ++tests from files and directories.
+|
++  merge-base-and-recur
  :>    combine the current file and subdirectory.
  :>
  :>  this merges the file {base} with its child files {recur}.
  |=  [base=vase recur=(map @ta tests)]
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
  :>  ford gives us a `(map @ta tests)`, but we actually
  :>  want something like ++tests.
  |=  a=(map @ta tests)
  ::  todo: i'd like to sort this, but ++sort has -find.a problems much like
  ::  ++weld does above!?
  ^-  tests
  %+  turn
    (to-list:dct:new-hoon a)
  |=  {key/@ta value/tests}
  [key [%| value]]
::
++  has-test-prefix
  |=  a=term  ^-  ?
  ?|  =((end 3 5 a) 'test-')
      =((end 3 6 a) 'check-')
  ==
::
++  gen-tests
  :>  creates a {tests} list out of a vase of a test suite
  |=  v=vase
  ^-  tests
  =+  arms=(sort (sloe p.v) aor)
  %+  turn  (skim arms has-test-prefix)
  |=  arm/term
  ::REVIEW fewer asserts? recouple the nock and eat the runtime compile cost?
  ?>  (~(nest ut (~(peek ut p.v) %free 6)) & p:!>((init-test)))
  =/  call  (~(mint ut p.v) p:!>(*tang) [%limb arm])
  ?>  (~(nest ut p:!>(*tang)) & p.call)
  ::
  :-  arm
  :-  %&
  |=  eny=@uvJ  ^-  tang
  ((hard tang) .*(q.v(+6 (init-test eny)) q.call))
::
:>  #  %per-test
:>    data initialized on a per-test basis.
::
++  init-test  |=({eny/@uvJ} %*(. tester eny eny, check-iterations 10))
::
++  tester
  |_  $:  eny=@uvJ                                    :<  entropy
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
    |-  ^-  tang
    ?:  (gth current-iteration check-iterations)
      ~
    ::  todo: wrap generator in mule so it can crash.
    =+  sample=(generator eny)
    ::  todo: wrap test in mule so it can crash.
    ?:  (test sample)
      %=  $
        eny    (shaf %huh eny)                        ::  xxx: better random?
        current-iteration  (add current-iteration 1)
      ==
    =/  case  +(current-iteration)
    =/  pl  ?+(case "" %1 "s")
    ::XXX sample is a noun
    [leaf+"falsified after {<case>} case{pl} by '{<`*`sample>}', seed {<eny>}"]~
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
    |=  a=vase
    ^-  tang
    ?@  q.a  [palm+[": " ~ ~ ~]^~[>%ex-expected-pair< (sell a)]]~
    ?:  =(-.q.a +.q.a)
      ~
    :~  palm+[": " ~ ~ ~]^~[leaf+"expected" (sell (slot 2 a))]
        palm+[": " ~ ~ ~]^~[leaf+"actual" (sell (slot 3 a))]
    ==
  :>  #
  :>  #  %formatting
  :>  #
  :>    test result presentation
  +|
  ++  category
    |=  [a=tape b=tang]  ^-  tang
    ?:  =(~ b)  ~  :: test OK
    :-  leaf+"in: '{a}'"
    (turn b |=(c=tank rose+[~ "  " ~]^~[c]))
  --
--
