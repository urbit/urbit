/-  spider
/+  threadio
=,  thread=thread:spider
^-  imp:spider
|=  arg=vase
=/  m  (thread ,vase)
^-  form:m
=+  !<([arg=@dr ~] arg)
;<  now-1=@da  bind:m  get-time:threadio
;<  ~          bind:m  (sleep:threadio arg)
;<  now-2=@da  bind:m  get-time:threadio
(pure:m !>(`@dr`(sub now-2 now-1)))
