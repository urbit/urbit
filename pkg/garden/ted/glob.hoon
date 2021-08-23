/-  spider, docket
/+  strandio
=,  strand=strand:spider
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
=+  !<([~ url=cord] arg)
;<  =glob:docket  bind:m
  %+  (retry:strandio ,glob:docket)  `5
  =/  n  (strand ,(unit glob:docket))
  ;<  =cord  bind:n  (fetch-cord:strandio (trip url))
  %-  pure:n
  %-  mole
  |.
  ;;(=glob:docket (cue cord))
(pure:m !>(glob))
