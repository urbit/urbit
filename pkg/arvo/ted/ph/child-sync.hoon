/-  spider
/+  *ph-io
=,  strand=strand:spider
^-  thread:spider
|=  vase
=/  m  (strand ,vase)
;<  ~        bind:m  start-simple
;<  ~        bind:m  (init-ship ~bud &)
;<  ~        bind:m  (init-ship ~marbud &)
;<  file=@t  bind:m  (touch-file ~bud %home %foo)
;<  ~        bind:m  (dojo ~bud "|merge %kids our %home")
;<  ~        bind:m  (check-file-touched ~marbud %home file)
;<  ~        bind:m  end
(pure:m *vase)
