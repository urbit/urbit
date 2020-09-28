/-  spider
/+  *ph-io
=,  strand=strand:spider
^-  thread:spider
|=  vase
=/  m  (strand ,vase)
;<  az=tid:spider  bind:m  start-azimuth
~&  thread-id+az
;<  ~  bind:m  (spawn az ~zod)
;<  ~  bind:m  (spawn az ~bus)
;<  ~  bind:m  (spawn az ~web)
::
;<  ~  bind:m  (real-ship az ~zod)
;<  ~  bind:m  (real-ship az ~bus)
;<  ~  bind:m  (real-ship az ~web)
::
;<  ~  bind:m  (send-hi ~zod ~web)
;<  ~  bind:m  (send-hi ~zod ~bus)
;<  ~  bind:m  (send-hi ~web ~zod)
;<  ~  bind:m  (send-hi ~bus ~zod)
;<  ~  bind:m  (send-hi ~bus ~web)
;<  ~  bind:m  (send-hi ~web ~bus)
::
;<  ~  bind:m  end-azimuth
(pure:m *vase)
