::  This tests whether we can handle the case where our immediate
::  sponsor dies without telling us about its breach, so we must hear
::  about it from somewhere else.
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
;<  ~        bind:m  (breach ~bud)
;<  ~        bind:m  (init-ship ~bud |)
;<  ~        bind:m
  (dojo ~bud "|merge %base ~marbud %kids, =gem %only-this")
;<  file=@t  bind:m  (touch-file ~bud %kids %bar)
;<  file=@t  bind:m  (touch-file ~bud %kids %baz)
;<  ~        bind:m  (check-file-touched ~marbud %base file)
;<  ~        bind:m  end
(pure:m *vase)
