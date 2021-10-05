/-  spider, docket
/+  strandio
=,  strand=strand:spider
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
=+  !<([~ ref=glob-reference:docket base=term] arg)
|^
?-  -.location.ref
  %http  (fetch-http [url.location hash]:ref)
  %ames  (fetch-ames [ship.location hash]:ref base)
==
::
++  fetch-http
  |=  [url=cord hash=@uvH]
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
  |=  [[=ship hash=@uvH] base=term]
  ^-  form:m
  ;<  =bowl:spider  bind:m  get-bowl:strandio
  ;<  =cage  bind:m
    (watch-one:strandio /glob/(scot %da now.bowl) [ship %docket] /glob/[base]/(scot %uv hash))
  ?>  ?=(%glob p.cage)
  (pure:m q.cage)
--
