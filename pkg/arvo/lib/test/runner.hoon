|%
::  $test: a test with a fully resolved path
::
+$  test  [=path func=test-func]
::  $test-arm: a test with a name (derived from its arm name in a test core)
::
+$  test-arm  [name=term func=test-func]
::  $test-func: a single test, as a gate; sample is entropy, produces failures
::
+$  test-func  (trap tang)
--
|%
++  run-test
  ::  executes an individual test.
  |=  [pax=path test=test-func]
  ^-  [ok=? =tang]
  =+  name=(spud pax)
  =+  run=(mule test)
  ?-  -.run
    %|  :-  %|  ::  the stack is already flopped for output?
        ;:  weld
          p.run
          `tang`[[%leaf (weld "CRASHED " name)] ~]
        ==
    %&  ?:  =(~ p.run)
          &+[[%leaf (weld "OK      " name)] ~]
        ::  Create a welded list of all failures indented.
        :-  %|
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
  |=  [prefix=path tests=(list test)]
  ^+  tests
  ::
  =/  prefix-length=@ud  (lent prefix)
  ::
  %+  skim  tests
  ::
  |=  [=path *]
  =(prefix (scag prefix-length path))
::  +resolve-test-paths: add test names to file paths to form full identifiers
::
++  resolve-test-paths
  |=  paths-to-tests=(map path (list test-arm))
  ^-  (list test)
  ::
  %-  sort  :_  |=([a=test b=test] !(aor path.a path.b))
  ::
  ^-  (list test)
  %-  zing
  %+  turn  ~(tap by paths-to-tests)
  |=  [=path test-arms=(list test-arm)]
  ^-  (list test)
  ::  strip off leading 'tests' from :path
  ::
  =.  path
    ?>  ?=(^ path)
    ?>  ?=(%tests i.path)
    t.path
  ::  for each test, add the test's name to :path
  ::
  %+  turn  test-arms
  |=  =test-arm
  ^-  test
  [(weld path /[name.test-arm]) func.test-arm]
::  +get-test-arms: convert test arms to functions and produce them
::
++  get-test-arms
  |=  [test-core-type=type test-core=*]
  ^-  (list test-arm)
  ::
  =/  arms=(list @tas)  (sloe test-core-type)
  ::
  %+  turn  (skim arms has-test-prefix)
  |=  name=term
  ^-  test-arm
  ::
  =/  run-arm=[=type =nock]
    ~|  [%failed-to-compile-test-arm name]
    (~(mint ut test-core-type) p:!>(*tang) [%limb name])
  ::
  :-  name
  ^-  test-func
  ::
  |.
  ;;  tang
  .*  test-core
  nock.run-arm
::  +has-test-prefix: does the arm define a test we should run?
::
++  has-test-prefix
  |=  a=term  ^-  ?
  =((end 3 5 a) 'test-')
--

