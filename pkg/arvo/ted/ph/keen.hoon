::  Perform a remote scry read for an Aqua virtual ship
::
/-  spider
/+  *ph-io
=,  strand=strand:spider
^-  thread:spider
|=  vase
=/  m  (strand ,vase)
;<  ~  bind:m  start-simple
;<  ~  bind:m  (init-ship ~bud &)
;<  ~  bind:m  (init-ship ~dev &)
;<  ~  bind:m  (dojo ~bud "-keen ~dev /c/x/1/kids/sys/kelvin")
;<  ~  bind:m  (wait-for-output ~bud "kal=[lal=%zuse num={(scow %ud zuse)}]")
;<  ~  bind:m  end
(pure:m *vase)
