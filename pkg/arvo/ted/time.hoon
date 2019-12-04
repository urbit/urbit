/-  spider
/+  strandio
=,  strand=strand:spider
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
=+  !<([arg=@dr ~] arg)
;<  now-1=@da  bind:m  get-time:strandio
;<  ~          bind:m  (sleep:strandio arg)
;<  now-2=@da  bind:m  get-time:strandio
(pure:m !>(`@dr`(sub now-2 now-1)))
