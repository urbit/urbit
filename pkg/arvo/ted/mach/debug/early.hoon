::  connect to .addr, then send .size bytes and close at once, without
::  waiting for %connected; report every gift and how long %closed took
::
::    for a %close while the driver is still connecting. with bytes
::    queued the driver should finish connecting, flush, and close with
::    [%close %done], never giving %connected; a failed connect should
::    close with its [%connect ...] cause. a .size of 0 skips the
::    %send, and should close with [%close %done] at once
::
/-  spider
/+  io=strandio
=,  strand=strand:spider
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
=+  !<([~ size=@ud addr=addr:mach] arg)
;<  =bowl:spider  bind:m  get-bowl:io
=*  start  now.bowl
=/  wir=wire  /(scot %uv (cut 5 [0 1] eny.bowl))
;<  ~  bind:m  (send-raw-card:io [%pass wir %arvo %m %connect wir addr])
;<  ~  bind:m
  ?:  =(0 size)
    (pure:(strand ,~) ~)
  ::
  ::  +fil isn't jetted, so double with +cat instead
  =/  data=@
    =/  a=@  'x'
    |-
    ?:  (gte (met 3 a) size)
      (end [3 size] a)
    $(a (cat 3 a a))
  (send-raw-card:io [%pass wir %arvo %m %send wir size data])
;<  ~  bind:m  (send-raw-card:io [%pass wir %arvo %m %close wir])
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
