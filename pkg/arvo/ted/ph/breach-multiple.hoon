::  This tests breaches of both parent and child in succession.
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
;<  file=@t        bind:m  (touch-file ~bud %base %foo)
;<  ~              bind:m  (check-file-touched ~marbud %home file)
;<  ~              bind:m  (breach-and-hear az ~bud ~marbud)
;<  ~              bind:m  (real-ship az ~bud)
;<  ~              bind:m  (breach-and-hear az ~marbud ~bud)
;<  ~              bind:m  (real-ship az ~marbud)
;<  file=@t        bind:m  (touch-file ~bud %base %bar)
;<  file=@t        bind:m  (touch-file ~bud %base %baz)
;<  ~              bind:m  (check-file-touched ~marbud %home file)
;<  ~              bind:m  end-azimuth
(pure:m *vase)
