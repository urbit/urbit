::  connect to .fief, then connect again under the same name from a
::  second wire; report the second connect's %closed, the open names
::  and whether the first still echoes, then close it
::
::    for a %connect on a name in use. %mach should refuse it with
::    [%connect %taken] on the second wire and leave the first open.
::    point .fief at a server in echo mode
::
/-  spider
/+  io=strandio
=,  strand=strand:spider
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
=+  !<([~ fief=fief:mach] arg)
;<  =bowl:spider  bind:m  get-bowl:io
=/  wir=wire  /(scot %uv (cut 5 [0 1] eny.bowl))
=/  two=wire  (snoc wir %two)
;<  ~  bind:m  (send-raw-card:io [%pass wir %arvo %m %connect wir fief])
;<  [* first=sign-arvo]  bind:m  take-sign-arvo:io
?.  ?=([%mach %connected *] first)
  (strand-fail:strand %connection-failed >first< ~)
;<  ~  bind:m  (send-raw-card:io [%pass two %arvo %m %connect wir fief])
;<  [=wire second=sign-arvo]  bind:m  take-sign-arvo:io
?.  =(two wire)
  (strand-fail:strand %wrong-wire >wire< ~)
;<  names=(list name:mach)  bind:m  (scry:io (list name:mach) /ma//connections)
;<  ~  bind:m  (send-raw-card:io [%pass wir %arvo %m %send wir 4 'ping'])
;<  [* third=sign-arvo]  bind:m  take-sign-arvo:io
;<  ~  bind:m  (send-raw-card:io [%pass wir %arvo %m %close wir])
;<  [* last=sign-arvo]  bind:m  take-sign-arvo:io
(pure:m !>([second=second names=names echo=third last=last]))
