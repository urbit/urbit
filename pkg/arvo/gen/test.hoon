::  Run tests
/+  test-runner
/=  all-tests
  /^  (map path (list test-arm:test-runner))
  /:  /===/tests
  /*  /test-gen/
::
|%
++  main
  |=  [defer=? tests=(list test:test-runner)]
  ^-  tang
  ::
  %-  zing
  %+  turn  tests
  |=  [=path test-func=test-func:test-runner]
  ^-  tang
  ::
  =/  test-results=tang  (run-test path test-func)
  ::  if :defer is set, produce errors; otherwise print them and produce ~
  ::
  ?:  defer
    test-results
  ((slog (flop test-results)) ~)
::
++  run-test
  ::  executes an individual test.
  |=  [pax=path test=test-func:test-runner]
  ^-  tang
  =+  name=(spud pax)
  =+  run=(mule test)
  ?-  -.run
    %|  ::  the stack is already flopped for output?
        ;:  weld
          p.run
          `tang`[[%leaf (weld "CRASHED " name)] ~]
        ==
    %&  ?:  =(~ p.run)
          [[%leaf (weld "OK      " name)] ~]
        ::  Create a welded list of all failures indented.
        %-  flop
        ;:  weld
          `tang`[[%leaf (weld "FAILED  " name)] ~]
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
  |=  [prefix=path tests=(list test:test-runner)]
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
::
=/  filtered-tests=(list test:test-runner)
  %+  filter-tests-by-prefix
    prefix
  (resolve-test-paths:test-runner all-tests)
::
(main defer filtered-tests)
