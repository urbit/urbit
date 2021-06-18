::  This is useful to test that we properly hear about breaches outside
::  our sponsorship tree.  We usually hear about these via ship-to-ship
::  communication.
::
/-  spider
/+  *ph-io
=,  strand=strand:spider
^-  thread:spider
|=  vase
=/  m  (strand ,vase)
;<  ~  bind:m  start-azimuth
;<  ~  bind:m  (spawn ~bud)
;<  ~  bind:m  (spawn ~dev)
;<  ~  bind:m  (spawn ~marbud)
;<  ~  bind:m  (spawn ~mardev)
;<  ~  bind:m  (init-ship ~bud |)
;<  ~  bind:m  (init-ship ~dev |)
;<  ~  bind:m  (init-ship ~marbud |)
;<  ~  bind:m  (init-ship ~mardev |)
;<  ~  bind:m  (send-hi ~marbud ~mardev)
;<  ~  bind:m  (breach-and-hear ~mardev ~marbud)
;<  ~  bind:m  (send-hi-not-responding ~marbud ~mardev)
;<  ~  bind:m  (init-ship ~mardev |)
;<  ~  bind:m  (wait-for-output ~marbud "hi ~mardev successful")
;<  ~  bind:m  end
(pure:m *vase)
