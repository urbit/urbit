/-  spider
/+  ph=ph-io
=,  strand=strand:spider
^-  thread:spider
|=  vase
=/  m  (strand ,vase)
=+  ~(. ph %lago)
=|  tids=drivers
=/  cores=(list ?(%mesa %ames))  ~[%mesa %ames]
|-  ^-  form:m
?~  cores
  (pure:m *vase)
;<  =_tids  bind:m  start-lago
;<  ~  bind:m  (aqua-setup %ames-retry ~s2)
;<  ~  bind:m  (switch-network-core i.cores)
;<  ~  bind:m  (aqua-setup ahoy-on/|)
::
;<  ~  bind:m  (init-ship ~bud &)
;<  ~  bind:m  (init-ship ~dev &)
;<  ~  bind:m  (init-ship ~marbud &)
;<  ~  bind:m  (init-ship ~dister-dozzod-dozbud &)
;<  ~  bind:m  (dojo ~bud "|ames/verb %fin %for %ges %kay %msg %odd %rcv %rot %snd %sun")
;<  ~  bind:m  (dojo ~dev "|ames/verb %fin %for %ges %kay %msg %odd %rcv %rot %snd %sun")
;<  ~  bind:m  (dojo ~marbud "|ames/verb %fin %for %ges %kay %msg %odd %rcv %rot %snd %sun")
;<  ~  bind:m  (dojo ~dister-dozzod-dozbud "|ames/verb %fin %for %ges %kay %msg %odd %rcv %rot %snd %sun")
;<  ~  bind:m  (send-hi ~bud ~dev)
;<  ~  bind:m  (send-hi ~bud ~marbud)
;<  ~  bind:m  (send-hi ~dister-dozzod-dozbud ~marbud)
;<  ~  bind:m  (send-hi ~marbud ~dister-dozzod-dozbud)
;<  ~  bind:m  (send-hi ~dister-dozzod-dozbud ~dev)
::  stop all driver threads and clean up ames packets in %lago
::
;<  ~  bind:m  (end tids)
;<  ~  bind:m  (send-events ~[reset-routing/~])
::
$(cores t.cores)