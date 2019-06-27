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

