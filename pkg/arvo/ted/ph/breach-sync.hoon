::  This tests that syncs are correctly restarted after a breach
::
::TODO  breach tests broken by dangling bone?
/-  spider
/+  *ph-io
=,  strand=strand:spider
^-  thread:spider
|=  vase
=/  m  (strand ,vase)
;<  ~              bind:m  start-azimuth
;<  ~              bind:m  (spawn ~bud)
;<  ~              bind:m  (spawn ~marbud)
;<  ~              bind:m  (init-ship ~bud |)
;<  ~              bind:m  (init-ship ~marbud |)
;<  file=@t        bind:m  (touch-file ~bud %kids %foo)
;<  ~              bind:m  (check-file-touched ~marbud %base file)
::  Merge so that when we unify history with the %only-this merge later, we
::  don't get a spurious conflict in %base
::
;<  ~              bind:m  (dojo ~marbud "|merge %kids our %base")
;<  ~              bind:m  (breach-and-hear ~bud ~marbud)
;<  ~              bind:m  (init-ship ~bud |)
;<  ~              bind:m
  (dojo ~bud "|merge %kids ~marbud %kids, =gem %only-this")
;<  file=@t        bind:m  (touch-file ~bud %kids %bar)
;<  file=@t        bind:m  (touch-file ~bud %kids %baz)
;<  ~              bind:m  (check-file-touched ~marbud %base file)
;<  ~              bind:m  end
(pure:m *vase)
