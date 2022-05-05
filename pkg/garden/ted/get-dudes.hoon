/-  spider
/+  *strandio
::
=,  strand=strand:spider
::
::  send on /spider/garden/json/get-dudes/json
::
|%
++  buds                                                 ::  get agents currently running
  |=  p=@tas
  =/  m  (strand ,(list @tas))
  ^-  form:m
  ?.  =(%base p)
    ;<  q=(list @tas)  bind:m  (suds p)
    (pure:m q)
  ;<  q=(list @tas)  bind:m  duds
  =|  r=(list (list @tas))
  |-  ^-  form:m
  =*  loop  $
  ?~  q  (pure:m (zing r))
  ;<  t=(list @tas)  bind:m  (suds i.q)
  loop(q t.q, r [t r])
::
++  suds                                                 ::  clean %ge scry
  |=  p=@tas
  =/  m  (strand ,(list @tas))
  ^-  form:m
  ;<  q=(set [@tas ?])  bind:m
    (scry (set ,[@tas ?]) /ge/(scot %tas p))
  %-  pure:m
  (murn ~(tap in q) |=([@tas ?] ?.(+.+< ~ `-.+<)))
::
++  duds                                                 ::  get desks
  =/  m  (strand ,(list @tas))
  ^-  form:m
  ;<  p=(set @tas)  bind:m  (scry (set ,@tas) /cd/base)
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
