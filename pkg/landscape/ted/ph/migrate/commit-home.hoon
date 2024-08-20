::
::  warning: using this thread will clobber aqua's currently active piers
::
/-  spider, *aquarium
/+  *ph-io
=,  strand=strand:spider
=>
|%
++  commit
  |=  [our=@p now=@da ships=(list @p)]
  ^-  (list aqua-event)
  %+  turn  ships
  |=  her=@p
  :+  %event  her
  =/  paths  .^((list path) %ct /(scot %p our)/home/(scot %da now))
  =/  mod=mode:clay
    %+  murn  paths
    |=  pat=path
    ^-  (unit [path (unit mime)])
    ?.  =((snag (dec (lent pat)) pat) %hoon)
      ~
    =/  clay-pax=path  (weld /(scot %p our)/home/(scot %da now) pat)
    =/  file  [/text/plain (as-octs:mimes:html .^(@ %cx clay-pax))]
    `[pat `file]
  :-  //sync/0v1n.2m9vh
  [%into %home | mod]
::
++  restore-fleet
  |=  label=term
  ^-  (list aqua-event)
  [%restore-snap label]~
::
++  snap-fleet
  |=  [label=term ships=(list @p)]
  ^-  (list aqua-event)
  [%snap-ships label ships]~
--
^-  thread:spider
|=  arg=vase
=+  !<(fleets=(list term) arg)
=/  m  (strand ,vase)
;<  ~  bind:m  start-simple
=/  ships=(list @p)  ~[~zod ~bus ~web]
;<  =bowl:spider  bind:m  get-bowl
=/  commit-events  (commit our.bowl now.bowl ships)
|-
=*  fleet-loop  $
?~  fleets
  (pure:m *vase)
::
;<  ~  bind:m  (send-events (restore-fleet i.fleets))
;<  ~  bind:m  (sleep ~s0)
;<  ~  bind:m  (send-events commit-events)
;<  ~  bind:m  (sleep ~s0)
;<  =bowl:spider  bind:m  get-bowl
=/  full-ships
  .^((list @p) %gx /(scot %p our.bowl)/aqua/(scot %da now.bowl)/ships/noun)
;<  ~  bind:m  (send-events (snap-fleet i.fleets full-ships))
;<  ~  bind:m  (sleep ~s0)
::
fleet-loop(fleets t.fleets)
