/+  new-hoon, tester
/=  all-tests
  /^  (map @ta tests:tester)
  /:  /===/tests
  /_  /test-tree/
::
=,  new-hoon
|%
::
++  test-runner
  :>  run all tests in {a} with a filter.
  =|  pax=path
  |=  [filter=path [defer=? eny=@uvJ] a=tests:tester]
  ^-  tang
  %-  concat:ls
  %+  turn  a
  |=  b=instance:tester
  ^-  tang
  =;  tan/tang
    ?:  defer  tan
    ((slog (flop tan)) ~)
  =^  matches  filter  (match-filter filter p.b)
  ?.  matches
    ~
  ?-  -.q.b
      %&
    (run-test [p.b pax] eny p.q.b)
  ::
      %|
    ^$(pax [p.b pax], a p.q.b)
  ==
::
++  run-test
  :>  executes an individual test.
  |=  [pax=path eny=@uvJ test=$-(@uvJ (list tape))]
  ^-  tang
  =+  name=(spud (flop pax))
  =+  run=(mule |.((test eny)))
  ?-  -.run
    $|  ::  the stack is already flopped for output?
        ;:  weld
          p:run
          `tang`[[%leaf (weld name " CRASHED")] ~]
        ==
    $&  ?:  =(~ p:run)
          [[%leaf (weld name " OK")] ~]
        ::  Create a welded list of all failures indented.
        %-  flop
        ;:  weld
          `tang`[[%leaf (weld name " FAILED")] ~]
          %+  turn  p:run
            |=  {i/tape}
            ^-  tank
            [%leaf (weld "  " i)]
        ==
  ==
::
++  match-filter
  :>  checks to see if {name} matches the head of {filter}.
  |=  [filter=path name=term]
  ^-  [? path]
  ?~  filter
    ::  when there's no filter, we always match.
    [%.y ~]
  [=(i.filter name) t.filter]
--
::
:-  %say
|=  $:  [now=@da eny=@uvJ bec=beak]
        [filter=$?($~ [pax=path $~])]
        [defer=_& seed=?(~ @uvJ)]
    ==
~?  !defer  %test-compiled
:-  %tang
%^    test-runner
    ?~(filter ~ pax.filter)
  [defer ?~(seed eny seed)]
(test-map-to-test-list:tester all-tests)
