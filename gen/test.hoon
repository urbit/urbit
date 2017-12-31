::  todo: think about using horns to import all tests in %/tests?
::
::  i should be able to use /: ?
/+  new-hoon, tester

::  ok, doing this as a list first. then do it automated. is there an equivalent
::  to /_ which works on an arbitrary directory?
/=  test-thr  /:  /===/tests/thr  /!noun/
/=  test-myb  /:  /===/tests/myb  /!noun/
/=  test-ls  /:  /===/tests/ls  /!noun/
/=  test-mp  /:  /===/tests/mp  /!noun/

=,  new-hoon
|%
:>  #  %models
+|
+=  tests
  :>    a hierarchical structure of tests
  :>
  :>  an alphabetically sorted recursive association list
  :>  mapping a part of a path to either a test trap or a
  :>  sublist of the same type.
  (list (pair term (either (trap (list tape)) tests)))
::
:>  #  %traps
+|
++  gen-tests
  :>  creates a {tests} list out of a vase of a test suite
  |=  [v=vase eny=@uvJ]
  ^-  tests
  =+  arms=(sort (sloe p.v) aor)
  =+  context=(slop (init-test-vase:tester eny) v)
  %+  map:ls  arms
  |=  arm/term
  :-  arm
  :-  %&
  |.
  ::  todo: pull out the entropy from the result interface.
  =/  r  (slap context [%cnsg [arm ~] [%$ 3] [[%$ 2] ~]])
  +:((hard {@uvJ (list tape)}) q:(slap r [%limb %results]))
::
++  test-runner
  :>  run all tests in {a}.
  ::
  ::  todo: pass in a path to filter on.
  =|  pax/path
  |=  a/tests  ^-  tang
  %-  concat:ls
  %+  map:ls  a
  |=  b/(pair term (either (trap (list tape)) tests))
  ^-  tang
  ?-  -.q.b
    %&  (run-test [p.b pax] p.q.b)
    %|  ^$(pax [p.b pax], a p.q.b)
  ==
::
++  run-test
  :>  executes an individual test.
  |=  {pax/path test/(trap (list tape))}
  ^-  tang
  =+  name=(spud (reverse:ls pax))
  =+  run=(mule test)
  ~!  run
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
          ~!  p:run
          %+  turn  p:run
            |=  {i/tape}
            ^-  tank
            [%leaf (weld "  " i)]
        ==
  ==
--
::
:-  %say
|=  $:  {now/@da eny/@uvJ bec/beak}
        $~
        $~
    ==
:-  %tang
%-  test-runner
^-  tests
:~
  ::  todo: for now, this is manually constructed. later, this should
  ::  be generated from the contents of %/tests, without addressing the
  ::  files individually. if possible, lift the call to ++gen-tests into
  ::  the build steps for caching.
  ['ls' [%| (gen-tests !>(test-ls) eny)]]
  ['mp' [%| (gen-tests !>(test-mp) eny)]]
  ['myb' [%| (gen-tests !>(test-myb) eny)]]
  ['thr' [%| (gen-tests !>(test-thr) eny)]]
==
