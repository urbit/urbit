/-  spider
/+  *ph-io
=,  strand=strand:spider
^-  thread:spider
|=  vase
=/  m  (strand ,vase)
=|  tids=drivers
;<  =_tids  bind:m  start-simple
;<  ~       bind:m  (aqua-setup ahoy-on/|)
;<  ~       bind:m  (switch-network-core %ames)
;<   ~      bind:m  (init-ship ~bud &)
;<   ~      bind:m  (init-ship ~dev &)
::
:: ;<  ~  bind:m
::   (dojo ~bud "|ames/verb %fin %for %ges %kay %msg %odd %rcv %rot %snd %sun")
:: ;<  ~  bind:m
::   (dojo ~dev "|ames/verb %fin %for %ges %kay %msg %odd %rcv %rot %snd %sun")
::
;<   ~      bind:m  (poke-our %aqua %aqua-rule !>([%hold-link ~bud ~dev]))
;<   ~      bind:m  (sleep ~s5)
::  don't wait for any of this
::
::
;<   ~      bind:m  (dojo ~bud "|hi ~dev '1'")
;<   ~      bind:m  (dojo ~bud "|hi ~dev '2'")
;<   ~      bind:m  (dojo ~bud "|hi ~dev '3'")
;<   ~      bind:m  (dojo ~bud "|hi ~dev '4'")
;<   ~      bind:m  (dojo ~bud "|hi ~dev '5'")
;<   ~      bind:m  (sleep ~s5)
;<   ~      bind:m  (poke-our %aqua %aqua-rule !>([%flush-link ~bud ~dev]))
;<   ~      bind:m  (wait-for-output ~dev "< ~bud: 1")
;<   ~      bind:m  (wait-for-output ~dev "< ~bud: 2")
;<   ~      bind:m  (wait-for-output ~dev "< ~bud: 3")
;<   ~      bind:m  (wait-for-output ~dev "< ~bud: 4")
;<   ~      bind:m  (wait-for-output ~dev "< ~bud: 5")
;<   ~      bind:m  (end tids)
(pure:m *vase)
