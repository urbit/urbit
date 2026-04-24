/-  spider
/+  *ph-io
/*  pub-agent  %hoon  /tests/app/pub/hoon
/*  sub-agent  %hoon  /tests/app/sub/hoon
=,  strand=strand:spider
^-  thread:spider
|=  vase
=/  m  (strand ,vase)
=/  cores=(list (pair ?(%mesa %ames) ?(%mesa %ames)))  ~[%mesa %ames]
|-  ^-  form:m
?~  cores  (pure:m *vase)
;<  ~  bind:m  start-simple
::
=*  loop  $
;<  ~  bind:m  (init-ship ~bud fake=&)
;<  ~  bind:m  (dojo ~bud "|pass [%a %load {<p.i.cores>}]")
;<  ~  bind:m  (init-ship ~dev fake=&)
;<  ~  bind:m  (dojo ~dev "|pass [%a %load {<q.i.cores>}]")
::
;<  ~  bind:m  (dojo ~bud "|ames/verb %fin %for %ges %kay %msg %odd %rcv %rot %snd %sun")
;<  ~  bind:m  (dojo ~dev "|ames/verb %fin %for %ges %kay %msg %odd %rcv %rot %snd %sun")
;<  ~  bind:m  (send-hi ~bud ~dev)
::
;<  ~  bind:m  (dojo ~bud "|mount %base")
;<  ~  bind:m  (dojo ~dev "|mount %base")
::
;<  ~  bind:m  (copy-file ~bud /app/sub/hoon sub-agent)
;<  ~  bind:m  (dojo ~bud "|start %sub")
::
;<  ~  bind:m  (copy-file ~dev /app/pub/hoon pub-agent)
;<  ~  bind:m  (dojo ~dev "|start %pub")
;<  ~  bind:m  (sleep ~s1)
;<  ~  bind:m  (dojo ~bud ":sub [%sub ~dev %pub]")
;<  ~  bind:m  (sleep ~s1)
:: ;<  ~  bind:m  (dojo ~dev ":pub send+45")
::  this will %kick the flow, causing a %cork $plea to be enqueued
::
;<  ~  bind:m  (dojo ~dev ":pub [%bye ~bud]")
;<  ~  bind:m  (sleep ~s1)
::
;<  ~  bind:m  (sleep ~m4)
::
;<  ~  bind:m  end
$(cores t.cores)
