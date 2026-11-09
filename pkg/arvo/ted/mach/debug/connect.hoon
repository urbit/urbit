::  connect to .addr and report every gift until %mach closes it, and
::  how long that took. .opened says whether %connected came
::
::    covers server modes that end the connection themselves (close,
::    reset) and connects that never open (refused, timeout); hangs if
::    the peer keeps the connection open
::
/-  spider
/+  io=strandio
=,  strand=strand:spider
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
=+  !<([~ addr=addr:mach] arg)
;<  =bowl:spider  bind:m  get-bowl:io
=*  start  now.bowl
=/  wir=wire  /(scot %uv (cut 5 [0 1] eny.bowl))
;<  ~  bind:m  (send-raw-card:io [%pass wir %arvo %m %connect wir addr])
=/  opened=?  |
=|  bytes=@ud
|-  ^-  form:m
=*  loop  $
;<  [=wire =sign-arvo]  bind:m  take-sign-arvo:io
?.  =(wir wire)
  (strand-fail:strand %wrong-wire >wire< ~)
?+  sign-arvo  (strand-fail:strand %unexpected-sign ~)
  [%mach %connected *]  loop(opened &)
  [%mach %received *]   loop(bytes (add bytes p.octs.sign-arvo))
::
    [%mach %closed *]
  ;<  now=@da  bind:m  get-time:io
  %-  pure:m
  !>([opened=opened bytes=bytes why=p.sign-arvo took=`@dr`(sub now start)])
==
