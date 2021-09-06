/-  spider, docket
/+  strandio
=,  strand=strand:spider
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
=+  !<([~ loc=glob-location:docket base=term] arg)
|^
?-  -.loc
  %http  (fetch-http url.loc)
  %ames  (fetch-ames ship.loc base)
==
::
++  fetch-http
  |=  url=cord
  ^-  form:m
  ;<  =glob:docket  bind:m
    %+  (retry:strandio ,glob:docket)  `5
    =/  n  (strand ,(unit glob:docket))
    ;<  =cord  bind:n  (fetch-cord:strandio (trip url))
    %-  pure:n
    %-  mole
    |.
    ;;(=glob:docket (cue cord))
  (pure:m !>(glob))
::
::  download from ship's docket state
++  fetch-ames
  |=  [=ship base=term]
  ^-  form:m
  ;<  =bowl:spider  bind:m  get-bowl:strandio
  ;<  =cage  bind:m
    (watch-one:strandio /glob/(scot %da now.bowl) [ship %docket] /glob/[base])
  ?>  ?=(%glob p.cage)
  (pure:m q.cage)
--
