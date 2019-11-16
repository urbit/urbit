/-  spider
/+  *ph-io
=,  thread=thread:spider
^-  imp:spider
|=  vase
=/  m  (thread ,vase)
;<  az=iid:spider  bind:m  start-azimuth
;<  ~              bind:m  (spawn az ~bud)
;<  ~              bind:m  (spawn az ~marbud)
;<  ~              bind:m  (real-ship az ~bud)
;<  ~              bind:m  (real-ship az ~marbud)
;<  file=@t        bind:m  (touch-file ~bud %base %foo)
;<  ~              bind:m  (check-file-touched ~marbud %home file)
;<  ~              bind:m  (breach az ~bud)
;<  ~              bind:m  (real-ship az ~bud)
;<  ~              bind:m  (dojo ~bud "|merge %base ~marbud %kids, =gem %this")
;<  file=@t        bind:m  (touch-file ~bud %base %bar)
;<  file=@t        bind:m  (touch-file ~bud %base %baz)
;<  ~              bind:m  (check-file-touched ~marbud %home file)
;<  ~              bind:m  end-azimuth
(pure:m *vase)
