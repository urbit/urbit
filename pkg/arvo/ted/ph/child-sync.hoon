/-  spider
/+  *ph-io
=,  strand=strand:spider
^-  thread:spider
|=  vase
=/  m  (strand ,vase)
;<  ~        bind:m  start-simple
;<  ~        bind:m  (raw-ship ~bud ~)
;<  ~        bind:m  (raw-ship ~marbud ~)
;<  file=@t  bind:m  (touch-file ~bud %base %foo)
;<  ~        bind:m  (check-file-touched ~marbud %home file)
;<  ~        bind:m  end-simple
(pure:m *vase)
