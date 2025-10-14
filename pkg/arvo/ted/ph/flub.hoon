/-  spider
/+  *ph-io
/*  pub-agent  %hoon  /tests/app/pub/hoon
/*  sub-agent  %hoon  /tests/app/sub/hoon
=,  strand=strand:spider
^-  thread:spider
|=  vase
=/  m  (strand ,vase)
;<  ~  bind:m  start-simple
;<  ~  bind:m  (init-ship ~bud &)
;<  ~  bind:m  (init-ship ~dev &)
;<  ~  bind:m  (dojo ~bud "|ames/verb %fin %for %ges %kay %msg %odd %rcv %rot %snd %sun")
;<  ~  bind:m  (dojo ~dev "|ames/verb %fin %for %ges %kay %msg %odd %rcv %rot %snd %sun")
::
;<  ~  bind:m  (send-hi ~bud ~dev)
::
;<  ~  bind:m  (dojo ~bud "|mount %base")
;<  ~  bind:m  (dojo ~dev "|mount %base")
::
;<  ~  bind:m  (copy-file ~bud /app/sub/hoon sub-agent)
;<  ~  bind:m  (dojo ~bud "|start %sub")
::  XX wait until the /gf $plea has been acked
::  this allows the retry /gf plea that whas dropped on
::  first contact to be resend
::
;<  ~  bind:m  (sleep ~s5)
::  poke a non-running agent
::
;<  ~  bind:m  (dojo ~bud ":sub [%sub ~dev %pub]")
::  check that remote flubs are received
::
;<  ~  bind:m  (sleep ~s1)  :: XX if this is not hear we scry into the future

;<  ~  bind:m  (wait-for-flub ~bud ~dev %pub)
::  check that ~dev has halted this flow
::
;<  ~  bind:m  (sleep ~s1)  :: XX if this is not hear we scry into the future
;<  ~  bind:m  (wait-for-has-halt ~dev ~bud %pub)  :: XX
::
;<  ~  bind:m  (copy-file ~dev /app/pub/hoon pub-agent)
;<  ~  bind:m  (dojo ~dev "|start %pub")
::   check that the %spur is sent
::
;<  ~  bind:m  (sleep ~s1)  :: XX if this is not hear we scry into the future

;<  ~  bind:m  (wait-for-spur ~bud ~dev %pub)
::   ... and that the flow is not halted anymore
::
;<  ~  bind:m  (dojo ~dev "|pass [%a %prod ~]")
::  XX no need to sleep...
::
;<  ~  bind:m  (wait-for-del-halt ~dev ~bud %pub)
::
;<  ~  bind:m  end
(pure:m *vase)
