/-  spider
/+  ph-io, threadio
=,  thread=thread:spider
^-  imp:spider
|=  =bowl:mall
=/  m  (thread ,~)
;<  ~        bind:m  start-simple:ph-io
;<  ~        bind:m  (raw-ship:ph-io ~bud ~)
;<  file=@t  bind:m  (touch-file:ph-io ~bud %home)
;<  ~        bind:m  (check-file-touched:ph-io ~bud %home file)
;<  ~        bind:m  (sleep:threadio ~d1)
;<  ~        bind:m  end-simple:ph-io
(pure:m ~)
