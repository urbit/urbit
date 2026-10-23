::  connect to .fief over tls, send an http/1.1 GET for /, and
::  return the domain reached, the status line, and how the
::  connection closed
::
::    the server closes after responding, since we ask it to
::
/-  spider
/+  io=strandio
=,  strand=strand:spider
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
=+  !<([~ =fief] arg)
?.  ?=([%turf ^ *] fief)
  (strand-fail:strand %tls-needs-turf ~)
;<  =bowl:spider  bind:m  get-bowl:io
=/  wir=wire  /(scot %uv (cut 5 [0 1] eny.bowl))
;<  ~  bind:m  (send-raw-card:io [%pass wir %arvo %m %connect & fief])
;<  [* first=sign-arvo]  bind:m  take-sign-arvo:io
?.  ?=([%mach %connected ^] first)
  (strand-fail:strand %connection-failed >first< ~)
::
::  name the domain the driver reached, which may not be the first
=/  host=tape  (trip (en-turf:html u.p.first))
=/  req=@t
  (crip "GET / HTTP/1.1\0d\0aHost: {host}\0d\0aConnection: close\0d\0a\0d\0a")
;<  ~  bind:m  (send-raw-card:io [%pass wir %arvo %m %send (as-octs:mimes:html req)])
=|  got=(list octs)
|-  ^-  form:m
=*  loop  $
;<  [=wire =sign-arvo]  bind:m  take-sign-arvo:io
?.  =(wir wire)
  (strand-fail:strand %wrong-wire >wire< ~)
?+  sign-arvo  (strand-fail:strand %unexpected-sign ~)
  [%mach %received *]  loop(got [octs.sign-arvo got])
::
    [%mach %closed *]
  =/  body=@t  (can 3 (flop got))
  =/  text=tape  (trip body)
  %-  pure:m
  !>([host=(crip host) status=(crip (scag (fall (find "\0d" text) 0) text)) bytes=(met 3 body) why=p.sign-arvo])
==
