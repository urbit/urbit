/-  spider
/+  ph-io
=,  thread=thread:spider
^-  imp:spider
|=  =bowl:mall
=/  m  (thread ,~)
;<  ~        bind:m  start-azimuth:ph-io
;<  ~        bind:m  (spawn:ph-io ~bud)
;<  ~        bind:m  (spawn:ph-io ~marbud)
;<  ~        bind:m  (real-ship:ph-io ~bud)
;<  ~        bind:m  (real-ship:ph-io ~marbud)
;<  file=@t  bind:m  (touch-file:ph-io ~bud %base)
;<  ~        bind:m  (check-file-touched:ph-io ~marbud %home file)
;<  ~        bind:m  (breach-and-hear:ph-io ~bud ~marbud)
;<  ~        bind:m  (real-ship:ph-io ~bud)
;<  ~        bind:m  (dojo:ph-io ~bud "|merge %base ~marbud %kids, =gem %this")
;<  file=@t  bind:m  (touch-file:ph-io ~bud %base)
;<  file=@t  bind:m  (touch-file:ph-io ~bud %base)
;<  ~        bind:m  (check-file-touched:ph-io ~marbud %home file)
;<  ~        bind:m  end-azimuth:ph-io
(pure:m ~)
