/+  new-hoon, tester
/=  all-tests
  /^  (map path (list test-arm:tester))
  /:  /===/tests
  /*  /test-gen/
::
=,  new-hoon
|%
::
++  test-runner
  |=  [defer=? eny=@uvJ tests=(list test:tester)]
  ^-  tang
  ::
  %-  zing
  %+  turn  tests
  |=  [=path test-func=test-func:tester]
  ^-  tang
  ::
  =/  test-results=tang  (run-test path eny test-func)
  ::  if :defer is set, produce errors; otherwise print them and produce ~
  ::
  ?:  defer
    test-results
  ((slog (flop test-results)) ~)
::
++  run-test
  ::  executes an individual test.
  |=  [pax=path eny=@uvJ test=test-func:tester]
  ^-  tang
  =+  name=(spud (flop pax))
  =+  run=(mule |.((test eny)))
  ?-  -.run
    %|  ::  the stack is already flopped for output?
        ;:  weld
          p.run
          `tang`[[%leaf (weld name " CRASHED")] ~]
        ==
    %&  ?:  =(~ p.run)
          [[%leaf (weld name " OK")] ~]
        ::  Create a welded list of all failures indented.
        %-  flop
        ;:  weld
          `tang`[[%leaf (weld name " FAILED")] ~]
          ::TODO indent
          :: %+  turn  p:run
          ::   |=  {i/tape}
          ::   ^-  tank
          ::   [%leaf (weld "  " i)]
          p.run
        ==
  ==
::  +filter-tests-by-prefix
::
++  filter-tests-by-prefix
  |=  [prefix=path tests=(list test:tester)]
  ^+  tests
  ::
  =/  prefix-length=@ud  (lent prefix)
  ::
  %+  skim  tests
  ::
  |=  [=path *]
  =(prefix (scag prefix-length path))
--
::
:-  %say
|=  $:  [now=@da eny=@uvJ bec=beak]
        [filter=$?(~ [pax=path ~])]
        [defer=_& seed=?(~ @uvJ)]
    ==
::  start printing early if we're not deferring output
::
~?  !defer  %tests-compiled
:-  %tang
::  use empty path prefix if unspecified
::
=/  prefix=path  ?~(filter ~ pax.filter)
=/  entropy  ?~(seed eny seed)
::
=/  filtered-tests=(list test:tester)
  %+  filter-tests-by-prefix
    prefix
  (resolve-test-paths:tester all-tests)
::
(test-runner defer entropy filtered-tests)
