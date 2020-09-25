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
;<  az=tid:spider  bind:m  start-azimuth
;<  ~              bind:m  (spawn az ~bud)
;<  ~              bind:m  (spawn az ~marbud)
;<  ~              bind:m  (real-ship az ~bud)
;<  ~              bind:m  (real-ship az ~marbud)
;<  file=@t        bind:m  (touch-file ~bud %kids %foo)
;<  ~              bind:m  (check-file-touched ~marbud %home file)
;<  ~              bind:m  (breach az ~bud)
;<  ~              bind:m  (real-ship az ~bud)
;<  ~              bind:m
  (dojo ~bud "|merge %home ~marbud %kids, =gem %only-this")
;<  file=@t        bind:m  (touch-file ~bud %kids %bar)
;<  file=@t        bind:m  (touch-file ~bud %kids %baz)
;<  ~              bind:m  (check-file-touched ~marbud %home file)
;<  ~              bind:m  end-azimuth
(pure:m *vase)
