/-  spider
/+  *ph-io
=,  strand=strand:spider
^-  thread:spider
|=  arg=vase
=+  !<([who=@p ~] arg)
=/  m  (strand ,vase)
;<  az=tid:spider  bind:m  start-azimuth
=/  hearer=@p  ?:(=(who ~zod) ~bus ~zod)
::
;<  ~  bind:m  (spawn az ~zod)
;<  ~  bind:m  (spawn az ~bus)
;<  ~  bind:m  (spawn az ~web)
;<  ~  bind:m  (send-hi who hearer)
;<  ~  bind:m  (breach-and-hear az who hearer)
;<  ~  bind:m  (send-hi-not-responding hearer who)
;<  ~  bind:m  (real-ship az who)
;<  ~  bind:m  (wait-for-output hearer "hi {<who>} successful")
;<  ~  bind:m  (wait-for-output hearer "XXXX")
(pure:m *vase)
