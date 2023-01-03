::  This tests breaches of both parent and child in succession.
::
/-  spider
/+  *ph-io
=,  strand=strand:spider
^-  thread:spider
|=  vase
=/  m  (strand ,vase)
;<  ~        bind:m  start-azimuth
;<  ~        bind:m  (spawn ~bud)
;<  ~        bind:m  (spawn ~marbud)
;<  ~        bind:m  (init-ship ~bud |)
;<  ~        bind:m  (init-ship ~marbud |)
;<  file=@t  bind:m  (touch-file ~bud %kids %foo)
;<  ~        bind:m  (check-file-touched ~marbud %base file)
;<  ~        bind:m  (breach-and-hear ~bud ~marbud)
;<  ~        bind:m  (init-ship ~bud |)
;<  ~        bind:m  (breach-and-hear ~marbud ~bud)
;<  ~        bind:m  (init-ship ~marbud |)
;<  file=@t  bind:m  (touch-file ~bud %kids %bar)
;<  file=@t  bind:m  (touch-file ~bud %kids %baz)
;<  ~        bind:m  (check-file-touched ~marbud %base file)
;<  ~        bind:m  end
(pure:m *vase)
