/-  spider
/+  *strandio
::
=,  strand=strand:spider
::
::  send on /desk/dude
::
|%
++  buds                                                 ::  get agents currently running
  |=  p=desk
  =/  m  (strand ,(list dude:gall))
  ^-  form:m
  ?.  =(%$ p)
    ;<  q=(list dude:gall)  bind:m  (suds p)
    (pure:m q)
  ;<  q=(list desk)  bind:m  duds
  =|  r=(list (list dude:gall))
  |-  ^-  form:m
  =*  s  $
  ?~  q  (pure:m (zing r))
  ;<  t=(list dude:gall)  bind:m  (suds i.q)
  s(q t.q, r [t r])
::
++  suds                                                 ::  clean %ge scry
  |=  p=desk
  =/  m  (strand ,(list dude:gall))
  ^-  form:m
  ;<  q=(set [dude:gall ?])  bind:m
    (scry (set ,[dude:gall ?]) /ge/(scot %tas p))
  %-  pure:m
  (murn ~(tap in q) |=([dude:gall ?] ?.(+.+< ~ `-.+<)))
::
++  duds                                                 ::  get desks
  =/  m  (strand ,(list desk))
  ^-  form:m
  ;<  p=(set desk)  bind:m  (scry (set ,desk) /cd/base)
  (pure:m ~(tap in p))
--
::
^-  thread:spider
|=  jon=vase
=/  m  (strand ,vase)
^-  form:m
;<  =bowl:spider  bind:m  get-bowl
=,  bowl
?~  know=!<((unit json) jon)
  (pure:m !>(`json`[%s 'invalid-request']))
?.  ?=([%s @] u.know)
  (pure:m !>(`json`[%s 'invalid-request']))
=,  format
;<  breh=(list @tas)  bind:m  (buds (so:dejs u.know))
%-  pure:m  
!>(`json`(frond:enjs 'buds' a/(turn breh |=(@tas s/+<))))
