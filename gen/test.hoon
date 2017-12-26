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

|%
++  perform-test-suite
  ::  takes a testing core and executes all tests in it.
  |=  {name/tape v/vase eny/@uvJ}
  ^-  tang
  =+  core-arms=(sort (sloe p.v) aor)
  ?:  =(~ core-arms)
    ~&  p.v
    [[%leaf :(weld "error: " name " is not a valid testing core.")] ~]
  =|  out/tang
  |-
  ?~  core-arms
    out
  %=  $
    out  (weld (perform-test-arm name i.core-arms v eny) out)
    core-arms  t.core-arms
  ==
::
++  perform-test-arm
  ::  performs a single test.
  |=  {suite-name/tape arm-name/term v/vase eny/@uvJ}
  ::  todo: terminal color on the output
  ^-  tang
  =+  run=(run-arm-in-test-core arm-name v eny)
  =+  full-name=:(weld suite-name "/" (trip arm-name))
  ?-  -.run
    $|  ::  the stack is already flopped for output?
        ;:  weld
          p:run
          `tang`[[%leaf (weld full-name " CRASHED")] ~]
        ==
    $&  ::  todo: test the cookie to make sure it returned the same core.
        ?:  =(~ +.p:run)
          [[%leaf (weld full-name " OK")] ~]
        ::  Create a welded list of all failures indented.
        %-  flop
        ;:  weld
          `tang`[[%leaf (weld full-name " FAILED")] ~]
          %+  turn  +.p:run
            |=  {i/tape}
            ^-  tank
            [%leaf (weld "  " i)]
        ==
  ==
::
++  run-arm-in-test-core
  ::  runs a single arm.
  ::
  ::  returns the output of `++mule` so that we can react to crashes
  ::  appropriately.
  |=  {arm-name/term v/vase eny/@uvJ}
  ^-  (each {@uvJ (list tape)} (list tank))
  =/  t  (init-test-vase:tester eny)
  ::  run the tests in the interpreter so we catch crashes.
  %-  mule  |.
  =/  r  (slap (slop t v) [%cnsg [arm-name ~] [%$ 3] [[%$ 2] ~]])
  ::  return just the results or we will be here forever while we try to copy
  ::  the entire kernel.
  ((hard {@uvJ (list tape)}) q:(slap r [%limb %results]))
--
::
:-  %say
|=  $:  {now/@da eny/@uvJ bec/beak}
        $~
        $~
    ==
:-  %tang
::  todo: right now, we hard code ++test-core. but eventually, we must instead
::  scry ford for the core from the hoon file. that doesn't exist yet.
::(perform-test-suite "test-thr" !>(test-thr) eny)
::(perform-test-suite "test-myb" !>(test-myb) eny)
::(perform-test-suite "test-ls" !>(test-ls) eny)
(perform-test-suite "test-mp" !>(test-mp) eny)
