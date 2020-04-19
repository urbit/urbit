/-  spider
/+  strandio
=,  strand=strand:spider
=>
|%
::  $test: a test with a fully resolved path
::  $test-arm: test with name (derived from its arm name in a test core)
::  $test-func: single test, as gate; sample is entropy, produces failures
::
+$  test       [=path func=test-func]
+$  test-arm   [name=term func=test-func]
+$  test-func  (trap tang)
--
=>
|%
::  +run-test: execute an individual test
::
++  run-test
  |=  [pax=path test=test-func]
  ^-  [ok=? =tang]
  =+  name=(spud pax)
  =+  run=(mule test)
  ?-  -.run
    %|  |+(welp p.run leaf+"CRASHED {name}" ~)
    %&  ?:  =(~ p.run)
          &+[leaf+"OK      {name}"]~
        |+(flop `tang`[leaf+"FAILED  {name}" p.run])
  ==
::  +filter-tests-by-prefix: TODO document
::
++  filter-tests-by-prefix
  |=  [prefix=path tests=(list test)]
  ^+  tests
  =/  prefix-length=@ud  (lent prefix)
  (skim tests |=([p=path *] =(prefix (scag prefix-length p))))
::  +resolve-test-paths: add test names to file paths to form full identifiers
::
++  resolve-test-paths
  |=  paths-to-tests=(map path (list test-arm))
  ^-  (list test)
  %-  sort  :_  |=([a=test b=test] !(aor path.a path.b))
  ^-  (list test)
  %-  zing
  %+  turn  ~(tap by paths-to-tests)
  |=  [=path test-arms=(list test-arm)]
  ^-  (list test)
  ::  strip off leading 'tests' from :path
  ::
  =.  path  ?>(?=([%tests *] path) t.path)
  ::  for each test, add the test's name to :path
  ::
  %+  turn  test-arms
  |=  =test-arm
  ^-  test
  [(weld path /[name.test-arm]) func.test-arm]
::  +get-test-arms: convert test arms to functions and produce them
::
++  get-test-arms
  |=  [typ=type cor=*]
  ^-  (list test-arm)
  =/  arms=(list @tas)  (sloe typ)
  %+  turn  (skim arms has-test-prefix)
  |=  name=term
  ^-  test-arm
  =/  fire-arm=nock
    ~|  [%failed-to-compile-test-arm name]
    q:(~(mint ut typ) p:!>(*tang) [%limb name])
  [name |.(;;(tang .*(cor fire-arm)))]
::  +has-test-prefix: does the arm define a test we should run?
::
++  has-test-prefix
  |=  a=term  ^-  ?
  =((end 3 5 a) 'test-')
--
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
=/  paz=(list path)  (turn !<((list path) arg) |=(path [%tests +<]))
;<  bek=beak  bind:m  get-beak:strandio
=|  test-arms=(map path (list test-arm))
|-  ^-  form:m
=*  gather-tests  $
?^  paz
  ;<  cor=vase  bind:m  (build-file:strandio bek hoon+(flop i.paz))
  =.  test-arms  (~(put by test-arms) i.paz (get-test-arms cor))
  gather-tests(paz t.paz)
%-  pure:m  !>  ^=  ok
%+  roll  (resolve-test-paths test-arms)
|=  [[=path =test-func] ok=_`?`%&]
^+  ok
=/  res  (run-test path test-func)
%-  (slog (flop tang.res))
&(ok ok.res)
