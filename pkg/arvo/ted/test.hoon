/-  spider
/+  strandio, *test-runner
=,  strand=strand:spider
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
=/  paz=(list path)  (turn !<((list path) arg) |=(path [%tests +<]))
;<  =bowl:spider  bind:m  get-bowl:strandio
=/  bek=beak  [our q.byk da+now]:bowl
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
