::  connect to .fief, then send .size bytes every .every until %mach
::  closes the connection, and report how it closed
::
::    for the retransmission test: point it at a silent server, then
::    drop the server's packets with tcp-server/pf-drop.sh. the sends
::    keep data in flight, so keepalive never starts, and the driver's
::    retransmission bound should close with [%peer %dead] about 180s
::    after the first unacknowledged send
::
/-  spider
/+  io=strandio
=,  strand=strand:spider
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
=+  !<([~ every=@dr size=@ud =fief] arg)
;<  =bowl:spider  bind:m  get-bowl:io
=/  wir=wire  /(scot %uv (cut 5 [0 1] eny.bowl))
;<  ~  bind:m  (send-raw-card:io [%pass wir %arvo %m %connect | fief])
;<  [* first=sign-arvo]  bind:m  take-sign-arvo:io
?.  ?=([%mach %connected *] first)
  (strand-fail:strand %connection-failed >first< ~)
;<  start=@da  bind:m  get-time:io
=/  next=@da  (add start every)
;<  ~  bind:m  (send-raw-card:io [%pass /stall %arvo %b %wait next])
=|  sends=@ud
|-  ^-  form:m
=*  loop  $
;<  [=wire =sign-arvo]  bind:m  take-sign-arvo:io
?+  sign-arvo  (strand-fail:strand %unexpected-sign ~)
    [%behn %wake *]
  ;<  ~  bind:m  (send-raw-card:io [%pass wir %arvo %m %send size (fil 3 size 'x')])
  ;<  ~  bind:m  (send-raw-card:io [%pass /stall %arvo %b %wait (add next every)])
  loop(sends +(sends), next (add next every))
::
    [%mach %closed *]
  ;<  now=@da  bind:m  get-time:io
  ;<  ~  bind:m  (send-raw-card:io [%pass /stall %arvo %b %rest next])
  (pure:m !>([sends=sends why=p.sign-arvo took=`@dr`(sub now start)]))
==
