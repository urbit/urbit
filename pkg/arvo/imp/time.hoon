/-  spider
/+  threadio
=,  thread=thread:spider
^-  imp:spider
|=  [=bowl:spider arg=vase]
=/  m  (thread ,vase)
^-  form:m
=+  !<([arg=@dr ~] arg)
;<  ~  bind:m  (sleep:threadio arg)
;<  now=@da  bind:m  get-time:threadio
(pure:m !>(`@dr`(sub now now.bowl)))
