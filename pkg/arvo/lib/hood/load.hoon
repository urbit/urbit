/=  gall-raw  /sys/vane/gall
::
|%
::  %eyre unsubscriptions have been broken since #6561 when we began using an
::  incorrect duct for the %gall %leave. This means that the %eyre channel
::  subscriptions get removed but the underlying %gall subscription never gets
::  cleaned up. Notably this is broken even when we reap the channel every ~h12.
::
::  (note that the issue at hand isn't an absolutely massive memory leak because
::  if any of the stale Gall subscriptions ever get a %fact emitted it ends up
::  in %eyre with the error message removing watch for non-existent channel x
::  and this does successfully clean up the subscription. In other words we have
::  only leaked subscriptions that never emit a %fact after the HTTP client has
::  disconnected.
::
::  since %gall removed its larval core, scries on load are not possible
::  (see #7079 for a solution to that). Instead, we move the logic to find these
::  stale subscriptions here to compare every agent's %eyre channel subscription
::  with the state of %eyre's current subscription.
::
::  Here we use the %lave task in %gall (introduced to remove stale incoming
::  subscriptions in %gall agents due to an ames-gall desync) to pass the duct
::  of the subscription we want to delete.
::
++  eyre-clean
  |=  [our=ship now=@da]
  ^-   (list card:agent:gall)
  =/  m-gall  (gall-raw our)
  =/  yokes   .^((map term yoke:m-gall) %gy /(scot %p our)//(scot %da now)/$)
  =/  channel
    .^(channel-state:eyre %e /(scot %p our)/channel-state/(scot %da now)/$)
  =;  subs=(list [%g ship term duct])
    [%pass /helm %arvo %g %lave subs]~
  ::
  %+  roll  ~(tap by yokes)
  |=  [[=dude:gall =yoke:m-gall] subs=(list [%g ship term duct])]
  ?>  ?=(%live -.yoke)
  %+  roll  ~(tap by bitt.yoke)
  |=  [[=duct =ship =path] s=_subs]
  ?.  ?=([[%eyre %channel %subscription session=@ta sub=@ta * *] *] duct)
    s
  =*  ses-id     &4:i.duct
  =*  sub-id     &5:i.duct
  ?~  chan=(~(get by session.channel) ses-id)
    [g/ship^dude^duct s]
  ?^  sub=(~(get by subscriptions.u.chan) (slav %ud sub-id))
    s
  [g/ship^dude^duct s]
::
--
