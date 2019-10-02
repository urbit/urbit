/-  spider
/+  ph-io
=,  thread=thread:spider
^-  imp:spider
|=  =bowl:mall
=/  m  (thread ,~)
;<  ~  bind:m  start-azimuth:ph-io
;<  ~  bind:m  (spawn:ph-io ~bud)
;<  ~  bind:m  (spawn:ph-io ~dev)
;<  ~  bind:m  (spawn:ph-io ~marbud)
;<  ~  bind:m  (spawn:ph-io ~mardev)
;<  ~  bind:m  (real-ship:ph-io ~bud)
;<  ~  bind:m  (real-ship:ph-io ~dev)
;<  ~  bind:m  (real-ship:ph-io ~marbud)
;<  ~  bind:m  (real-ship:ph-io ~mardev)
;<  ~  bind:m  (send-hi:ph-io ~marbud ~mardev)
;<  ~  bind:m  (breach-and-hear:ph-io ~mardev ~marbud)
;<  ~  bind:m  (send-hi-not-responding:ph-io ~marbud ~mardev)
;<  ~  bind:m  (real-ship:ph-io ~mardev)
;<  ~  bind:m  (wait-for-output:ph-io ~marbud "hi ~mardev successful")
;<  ~  bind:m  end-azimuth:ph-io
(pure:m ~)
