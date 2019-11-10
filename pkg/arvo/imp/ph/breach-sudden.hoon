/-  spider
/+  *ph-io
=,  thread=thread:spider
^-  imp:spider
|=  [=bowl:mall vase]
=/  m  (thread ,vase)
;<  ~        bind:m  start-azimuth
;<  ~        bind:m  (spawn ~bud)
;<  ~        bind:m  (spawn ~marbud)
;<  ~        bind:m  (real-ship ~bud)
;<  ~        bind:m  (real-ship ~marbud)
;<  file=@t  bind:m  (touch-file ~bud %base)
;<  ~        bind:m  (check-file-touched ~marbud %home file)
;<  ~        bind:m  (breach ~bud)
;<  ~        bind:m  (real-ship ~bud)
;<  ~        bind:m  (dojo ~bud "|merge %base ~marbud %kids, =gem %this")
;<  file=@t  bind:m  (touch-file ~bud %base)
;<  file=@t  bind:m  (touch-file ~bud %base)
;<  ~        bind:m  (check-file-touched ~marbud %home file)
;<  ~        bind:m  end-azimuth
(pure:m *vase)
