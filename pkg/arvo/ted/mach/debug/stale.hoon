::  connect to .one, wait .wait, then connect again on the same wire
::  to .two and close; report every gift and the open ids before and
::  after the second connect
::
::    for an id %mach still holds after the event carrying its %closed
::    crashed. a %connect on the same duct should make %mach drop the
::    old id and tell the driver to %close it. to make the crash, run
::    a vane that crashes on [%peer %done] and point .one at a server
::    in close mode
::
/-  spider
/+  io=strandio
=,  strand=strand:spider
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
=+  !<([~ wait=@dr one=fief two=fief] arg)
;<  =bowl:spider  bind:m  get-bowl:io
=/  wir=wire  /(scot %uv (cut 5 [0 1] eny.bowl))
;<  ~  bind:m  (send-raw-card:io [%pass wir %arvo %m %connect | one])
;<  [* first=sign-arvo]  bind:m  take-sign-arvo:io
?.  ?=([%mach %connected *] first)
  (strand-fail:strand %connection-failed >first< ~)
;<  ~  bind:m  (sleep:io wait)
;<  before=(list @ud)  bind:m  (scry:io (list @ud) /ma//connections)
;<  ~  bind:m  (send-raw-card:io [%pass wir %arvo %m %connect | two])
;<  [* second=sign-arvo]  bind:m  take-sign-arvo:io
?.  ?=([%mach %connected *] second)
  (strand-fail:strand %connection-failed >second< ~)
;<  after=(list @ud)  bind:m  (scry:io (list @ud) /ma//connections)
;<  ~  bind:m  (send-raw-card:io [%pass wir %arvo %m %close ~])
;<  [* third=sign-arvo]  bind:m  take-sign-arvo:io
(pure:m !>([before=before after=after last=third]))
