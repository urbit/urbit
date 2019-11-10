/-  spider
/+  *ph-io
=,  thread=thread:spider
^-  imp:spider
|=  [=bowl:spider vase]
=/  m  (thread ,vase)
;<  ~  bind:m  start-azimuth
;<  ~  bind:m  (spawn ~bud)
;<  ~  bind:m  (spawn ~dev)
;<  ~  bind:m  (spawn ~marbud)
;<  ~  bind:m  (spawn ~mardev)
;<  ~  bind:m  (real-ship ~bud)
;<  ~  bind:m  (real-ship ~dev)
;<  ~  bind:m  (real-ship ~marbud)
;<  ~  bind:m  (real-ship ~mardev)
;<  ~  bind:m  (send-hi ~marbud ~mardev)
;<  ~  bind:m  (breach-and-hear ~mardev ~marbud)
;<  ~  bind:m  (send-hi-not-responding ~marbud ~mardev)
;<  ~  bind:m  (real-ship ~mardev)
;<  ~  bind:m  (wait-for-output ~marbud "hi ~mardev successful")
;<  ~  bind:m  end-azimuth
(pure:m *vase)
