/-  spider
/+  *ph-io
=,  thread=thread:spider
^-  imp:spider
|=  [=bowl:spider vase]
=/  m  (thread ,vase)
;<  ~        bind:m  start-simple
;<  ~        bind:m  (raw-ship ~bud ~)
;<  file=@t  bind:m  (touch-file ~bud %home %foo)
;<  ~        bind:m  (check-file-touched ~bud %home file)
;<  ~        bind:m  end-simple
(pure:m *vase)
