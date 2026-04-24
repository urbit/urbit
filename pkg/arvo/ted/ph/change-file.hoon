/-  spider
/+  *ph-io
=,  strand=strand:spider
^-  thread:spider
|=  vase
=/  m  (strand ,vase)
=|  tids=drivers
;<  =_tids   bind:m  start-simple
;<  ~        bind:m  (init-ship ~bud &)
;<  file=@t  bind:m  (touch-file ~bud %base %foo)
;<  ~        bind:m  (check-file-touched ~bud %base file)
;<  ~        bind:m  (end tids)
(pure:m *vase)
