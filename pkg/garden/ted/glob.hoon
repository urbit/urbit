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
  %ames  !!  ::NOTE  done within docket itself
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
--
