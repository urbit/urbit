::  connect to .fief and take %received until .want bytes arrive
::
::    for the flood server mode. reports how many bytes and events
::    arrived, the largest event (the driver caps them at 1 MiB), and
::    how the connection closed, whether by us or by the driver
::
/-  spider
/+  io=strandio
=,  strand=strand:spider
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
=+  !<([~ want=@ud =fief] arg)
;<  =bowl:spider  bind:m  get-bowl:io
=/  wir=wire  /(scot %uv (cut 5 [0 1] eny.bowl))
;<  ~  bind:m  (send-raw-card:io [%pass wir %arvo %m %connect | fief])
;<  [* first=sign-arvo]  bind:m  take-sign-arvo:io
?.  ?=([%mach %connected *] first)
  (strand-fail:strand %connection-failed >first< ~)
=|  [bytes=@ud events=@ud biggest=@ud]
=/  closing=?  |
|-  ^-  form:m
=*  loop  $
?:  &(!closing (gte bytes want))
  ;<  ~  bind:m  (send-raw-card:io [%pass wir %arvo %m %close ~])
  loop(closing &)
;<  [=wire =sign-arvo]  bind:m  take-sign-arvo:io
?.  =(wir wire)
  (strand-fail:strand %wrong-wire >wire< ~)
?+  sign-arvo  (strand-fail:strand %unexpected-sign ~)
    [%mach %received *]
  =*  len  p.octs.sign-arvo
  loop(bytes (add bytes len), events +(events), biggest (max biggest len))
::
    [%mach %closed *]
  %-  pure:m
  !>([bytes=bytes events=events biggest=biggest by-us=closing why=p.sign-arvo])
==
