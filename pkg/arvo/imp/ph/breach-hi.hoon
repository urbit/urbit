/-  spider
/+  ph-io
=,  thread=thread:spider
^-  imp:spider
|=  =bowl:mall
=/  m  (thread ,~)
;<  ~  bind:m  start-azimuth:ph-io
;<  ~  bind:m  (spawn:ph-io ~bud)
;<  ~  bind:m  (spawn:ph-io ~dev)
;<  ~  bind:m  (real-ship:ph-io ~bud)
;<  ~  bind:m  (real-ship:ph-io ~dev)
;<  ~  bind:m  (send-hi:ph-io ~bud ~dev)
;<  ~  bind:m  (breach-and-hear:ph-io ~dev ~bud)
;<  ~  bind:m  (send-hi-not-responding:ph-io ~bud ~dev)
;<  ~  bind:m  (real-ship:ph-io ~dev)
;<  ~  bind:m  (wait-for-output:ph-io ~bud "hi ~dev successful")
;<  ~  bind:m  end-azimuth:ph-io
(pure:m ~)
