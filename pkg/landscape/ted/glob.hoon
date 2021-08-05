/-  spider, glob
/+  strandio
=,  strand=strand:spider
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
=+  !<([~ url=cord] arg)
;<  =glob:glob  bind:m
  %+  (retry:strandio ,glob:glob)  `5
  =/  n  (strand ,(unit glob:glob))
  ;<  =cord  bind:n  (fetch-cord:strandio (trip url))
  %-  pure:n
  %-  mole
  |.
  ;;(=glob:glob (cue cord))
(pure:m !>(glob))
