/-  spider
/+  *ph-io
=,  thread=thread:spider
^-  imp:spider
|=  [=bowl:spider vase]
=/  m  (thread ,vase)
;<  ~        bind:m  start-simple
;<  ~        bind:m  (raw-ship ~bud ~)
;<  ~        bind:m  (raw-ship ~marbud ~)
;<  file=@t  bind:m  (touch-file ~bud %base)
;<  ~        bind:m  (check-file-touched ~marbud %home file)
;<  ~        bind:m  end-simple
(pure:m *vase)
