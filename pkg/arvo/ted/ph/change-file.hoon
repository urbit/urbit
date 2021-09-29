/-  spider
/+  *ph-io
=,  strand=strand:spider
^-  thread:spider
|=  vase
=/  m  (strand ,vase)
;<  ~        bind:m  start-simple
;<  ~        bind:m  (init-ship ~bud &)
;<  file=@t  bind:m  (touch-file ~bud %home %foo)
;<  ~        bind:m  (check-file-touched ~bud %home file)
;<  ~        bind:m  end
(pure:m *vase)
