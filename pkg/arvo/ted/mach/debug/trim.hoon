::  connect to .fief, then send %mach a %trim, and report how the
::  connection closed
::
::    point it at a peer that keeps the connection open (echo, silent);
::    %mach should close it with [%local %trimmed], and the driver
::    should close the socket
::
/-  spider
/+  io=strandio
=,  strand=strand:spider
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
=+  !<([~ =fief] arg)
;<  =bowl:spider  bind:m  get-bowl:io
=/  wir=wire  /(scot %uv (cut 5 [0 1] eny.bowl))
;<  ~  bind:m  (send-raw-card:io [%pass wir %arvo %m %connect | fief])
;<  [* first=sign-arvo]  bind:m  take-sign-arvo:io
?.  ?=([%mach %connected *] first)
  (strand-fail:strand %connection-failed >first< ~)
;<  ~  bind:m  (send-raw-card:io [%pass /trim %arvo %m %trim 0])
;<  [=wire =sign-arvo]  bind:m  take-sign-arvo:io
?.  =(wir wire)
  (strand-fail:strand %wrong-wire >wire< ~)
?.  ?=([%mach %closed *] sign-arvo)
  (strand-fail:strand %unexpected-sign ~)
(pure:m !>(why=p.sign-arvo))
