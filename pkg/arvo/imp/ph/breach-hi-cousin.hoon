/-  spider
/+  *ph-io
=,  thread=thread:spider
^-  imp:spider
|=  [=bowl:spider vase]
=/  m  (thread ,vase)
;<  az=iid:spider  bind:m
  start-azimuth
;<  ~  bind:m  (spawn az ~bud)
;<  ~  bind:m  (spawn az ~dev)
;<  ~  bind:m  (spawn az ~marbud)
;<  ~  bind:m  (spawn az ~mardev)
;<  ~  bind:m  (real-ship az ~bud)
;<  ~  bind:m  (real-ship az ~dev)
;<  ~  bind:m  (real-ship az ~marbud)
;<  ~  bind:m  (real-ship az ~mardev)
;<  ~  bind:m  (send-hi ~marbud ~mardev)
;<  ~  bind:m  (breach-and-hear az ~mardev ~marbud)
;<  ~  bind:m  (send-hi-not-responding ~marbud ~mardev)
;<  ~  bind:m  (real-ship az ~mardev)
;<  ~  bind:m  (wait-for-output ~marbud "hi ~mardev successful")
;<  ~  bind:m  end-azimuth
(pure:m *vase)
