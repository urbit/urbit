/-  spider
/+  *ph-io
=,  strand=strand:spider
^-  thread:spider
|=  vase
=/  m  (strand ,vase)
;<  ~  bind:m  start-azimuth
;<  ~  bind:m  (spawn ~zod)
;<  ~  bind:m  (spawn ~bus)
;<  ~  bind:m  (spawn ~web)
::
;<  ~  bind:m  (init-ship ~zod |)
;<  ~  bind:m  (init-ship ~bus |)
;<  ~  bind:m  (init-ship ~web |)
::
;<  ~  bind:m  (send-hi ~zod ~web)
;<  ~  bind:m  (send-hi ~zod ~bus)
;<  ~  bind:m  (send-hi ~web ~zod)
;<  ~  bind:m  (send-hi ~bus ~zod)
;<  ~  bind:m  (send-hi ~bus ~web)
;<  ~  bind:m  (send-hi ~web ~bus)
::
(pure:m *vase)
