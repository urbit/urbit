/-  spider
/+  strandio
=,  strand=strand:spider
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
=+  !<([who=ship mez=$@(~ [=tape ~])] arg)
=/  message  ?~(mez '' (crip tape.mez))
;<  ~  bind:m  (poke:strandio [who %hood] %helm-hi !>(message))
(pure:m !>("hi {<who>} successful"))
