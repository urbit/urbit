/-  spider
/+  *ph-io
=,  strand=strand:spider
^-  thread:spider
|=  args=vase
=/  m  (strand ,vase)
;<  ~  bind:m  start-simple
;<  ~  bind:m  (init-ship ~bud &)
;<  ~  bind:m  (dojo ~bud "[%test-result (add 2 3)]")
;<  ~  bind:m  (wait-for-output ~bud "[%test-result 5]")
;<  ~  bind:m  end
(pure:m *vase)
