::  connect to .fief, send .size bytes, close, and report how it closed
::
::    for the discard and silent server modes. a silent peer never reads,
::    so once .size outgrows the socket buffers the driver can't drain
::    its queue, and should close with %timeout after 30 seconds
::
/-  spider
/+  io=strandio
=,  strand=strand:spider
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
=+  !<([~ size=@ud =fief] arg)
;<  =bowl:spider  bind:m  get-bowl:io
=/  wir=wire  /(scot %uv (cut 5 [0 1] eny.bowl))
;<  ~  bind:m  (send-raw-card:io [%pass wir %arvo %m %connect | fief])
;<  [* first=sign-arvo]  bind:m  take-sign-arvo:io
?.  ?=([%mach %connected *] first)
  (strand-fail:strand %connection-failed >first< ~)
::
::  +fil isn't jetted, so double with +cat instead
=/  data=@
  =/  a=@  'x'
  |-
  ?:  (gte (met 3 a) size)
    (end [3 size] a)
  $(a (cat 3 a a))
;<  ~  bind:m  (send-raw-card:io [%pass wir %arvo %m %send size data])
;<  ~  bind:m  (send-raw-card:io [%pass wir %arvo %m %close ~])
;<  start=@da  bind:m  get-time:io
=|  bytes=@ud
|-  ^-  form:m
=*  loop  $
;<  [=wire =sign-arvo]  bind:m  take-sign-arvo:io
?.  =(wir wire)
  (strand-fail:strand %wrong-wire >wire< ~)
?+  sign-arvo  (strand-fail:strand %unexpected-sign ~)
  [%mach %received *]  loop(bytes (add bytes p.octs.sign-arvo))
::
    [%mach %closed *]
  ;<  now=@da  bind:m  get-time:io
  (pure:m !>([received=bytes why=p.sign-arvo took=`@dr`(sub now start)]))
==
