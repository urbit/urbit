/-  spider
/+  io=strandio
=,  strand=strand:spider
|%
::  +take-echo: take %received gifts on .wir until .want bytes arrive
::
::    the driver caps each %received at 1 MiB, so a long echo arrives
::    in chunks; we join them in order and fail if the peer closes early
::
++  take-echo
  |=  [wir=wire want=@ud]
  =/  m  (strand ,octs)
  =|  got=(list octs)
  =|  have=@ud
  |-  ^-  form:m
  =*  loop  $
  ?:  (gte have want)
    (pure:m [have (can 3 (flop got))])
  ;<  [=wire =sign-arvo]  bind:m  take-sign-arvo:io
  ?.  =(wir wire)
    (strand-fail:strand %wrong-wire >wire< ~)
  ?+  sign-arvo  (strand-fail:strand %unexpected-sign ~)
      [%mach %received *]
    loop(got [octs.sign-arvo got], have (add have p.octs.sign-arvo))
  ::
      [%mach %closed *]
    (strand-fail:strand %closed-early >p.sign-arvo< ~)
  ==
::
::  +take-closed: wait for %closed on .wir, so spider doesn't get a
::  sign after the thread ends; a %received may still be in flight
::
++  take-closed
  |=  wir=wire
  =/  m  (strand ,wye:mach)
  ^-  form:m
  ;<  [=wire =sign-arvo]  bind:m  take-sign-arvo:io
  ?.  =(wir wire)
    (strand-fail:strand %wrong-wire >wire< ~)
  ?+  sign-arvo  (strand-fail:strand %unexpected-sign ~)
    [%mach %received *]  (take-closed wir)
    [%mach %closed *]    (pure:m p.sign-arvo)
  ==
--
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
=+  !<([~ txt=@t =fief] arg)
;<  =bowl:spider  bind:m  get-bowl:io
=/  wir=wire  /(scot %uv (cut 5 [0 1] eny.bowl))
;<  ~  bind:m  (send-raw-card:io [%pass wir %arvo %m %connect | fief])
;<  [=wire =sign-arvo]  bind:m  take-sign-arvo:io
?.  ?=([%mach %connected *] sign-arvo)
  (strand-fail:strand %connection-failed ~)
=/  out=octs  (as-octs:mimes:html txt)
;<  ~  bind:m  (send-raw-card:io [%pass wir %arvo %m %send out])
;<  echo=octs  bind:m  (take-echo wir p.out)
;<  ~  bind:m  (send-raw-card:io [%pass wir %arvo %m %close ~])
;<  why=wye:mach  bind:m  (take-closed wir)
(pure:m !>([echo=`@t`q.echo why=why]))
