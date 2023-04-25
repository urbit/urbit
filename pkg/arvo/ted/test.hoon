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
  [name |.(;;(tang ~>(%bout.[1 name] .*(cor fire-arm))))]
::  +has-test-prefix: does the arm define a test we should run?
::
++  has-test-prefix
  |=  a=term  ^-  ?
  =((end [3 5] a) 'test-')
::
++  find-test-files
  =|  fiz=(set [=beam test=(unit term)])
  =/  m  (strand ,_fiz)
  |=  bez=(list beam)
  ^-  form:m
  =*  loop  $
  ?~  bez
    (pure:m fiz)
  ;<  hav=?  bind:m  (check-for-file:strandio -.i.bez (snoc s.i.bez %hoon))
  ?:  hav
    loop(bez t.bez, fiz (~(put in fiz) [i.bez(s (snoc s.i.bez %hoon)) ~]))
  ;<  fez=(list path)  bind:m  (list-tree:strandio i.bez)
  ?.  =(~ fez)
    =/  foz
      %+  murn  fez
      |=  p=path
      ?.  =(%hoon (rear p))  ~
      (some [[-.i.bez p] ~])
    loop(bez t.bez, fiz (~(gas in fiz) foz))
  ::
  ::  XX this logic appears to be vestigial
  ::
  =/  tex=term
    ~|  bad-test-beam+i.bez
    =-(?>(((sane %tas) -) -) (rear s.i.bez))
  =/  xup=path  (snip s.i.bez)
  ;<  hov=?  bind:m  (check-for-file:strandio i.bez(s (snoc xup %hoon)))
  ?.  hov
    ~|(no-tests-at-path+i.bez !!)
  loop(bez t.bez, fiz (~(put in fiz) [[-.i.bez (snoc xup %hoon)] `tex]))
--
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
;<  =bowl:strand  bind:m  get-bowl:strandio
=/  paz=(list path)
  :: if no args, test everything under /=base=/tests
  ::
  ?~  q.arg
    ~[/(scot %p our.bowl)/[q.byk.bowl]/(scot %da now.bowl)/tests]
  :: else cast path to ~[path] if needed
  ::
  ?@  +<.q.arg
    [(tail !<([~ path] arg)) ~]
  (tail !<([~ (list path)] arg))
=/  bez=(list beam)
  (turn paz |=(p=path ~|([%test-not-beam p] (need (de-beam p)))))
;<  fiz=(set [=beam test=(unit term)])  bind:m  (find-test-files bez)
=>  .(fiz (sort ~(tap in fiz) aor))
=|  test-arms=(map path (list test-arm))
=|  build-ok=?
|-  ^-  form:m
=*  gather-tests  $
?^  fiz
  ;<  cor=(unit vase)  bind:m  (build-file:strandio beam.i.fiz)
  ?~  cor
    ~>  %slog.0^leaf+"FAILED  {(spud s.beam.i.fiz)} (build)"
    gather-tests(fiz t.fiz, build-ok |)
  ~>  %slog.0^leaf+"built   {(spud s.beam.i.fiz)}"
  =/  arms=(list test-arm)  (get-test-arms u.cor)
  ::  if test path specified an arm prefix, filter arms to match
  =?  arms  ?=(^ test.i.fiz)
    %+  skim  arms
    |=  test-arm
    =((end [3 (met 3 u.test.i.fiz)] name) u.test.i.fiz)
  =.  test-arms  (~(put by test-arms) (snip s.beam.i.fiz) arms)
  gather-tests(fiz t.fiz)
%-  pure:m  !>  ^=  ok
%+  roll  (resolve-test-paths test-arms)
|=  [[=path =test-func] ok=_build-ok]
^+  ok
=/  res  (run-test path test-func)
%-  (slog (flop tang.res))
&(ok ok.res)
