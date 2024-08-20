::  Deletes all stale ames flows from failed (re) subscriptions
::
::    It runs in dry mode by default, printing the flows that can be closed.
::    To actually close the flows, run with |close-flows, =dry |
::
:-  %say
|=  [[now=@da eny=@uvJ bec=beak] arg=~ peer=(unit @p) dry=? veb=?]
::
=/  peers-map
  .^((map ship ?(%alien %known)) %ax /(scot %p p.bec)//(scot %da now)/peers)
::
=/  peers=(list ship)
  %+  murn  ~(tap by peers-map)
  |=  [=ship val=?(%alien %known)]
  ?:  =(ship p.bec)
    ~  ::  this is weird, but we saw it
  ?-  val
    %alien  ~
    %known  (some ship)
  ==
::
=;  bones=(list [ship bone])
  :-  %helm-ames-kroc
  ~?  dry  "#{<(lent bones)>} flows can be closed"
  dry^bones
::
%+  roll  peers
|=  [=ship bones=(list [ship bone])]
?:  &(?=(^ peer) !=(u.peer ship))
  bones
::
=+  .^  =ship-state:ames
        %ax  /(scot %p p.bec)//(scot %da now)/peers/(scot %p ship)
    ==
=/  =peer-state:ames  ?>(?=(%known -.ship-state) +.ship-state)
|^
=/  subs=(jar path [bone sub-nonce=@])  resubscriptions
%+  roll  ~(tap by subs)
|=  [[=wire flows=(list [bone sub-nonce=@])] bones=_bones]
::
%-  flop  %-  tail
%+  roll  (sort flows |=([[@ n=@] [@ m=@]] (lte n m)))
|=  [[=bone nonce=@] resubs=_(lent flows) bones=_bones]
=/  app=term  ?>(?=([%gall %use sub=@ *] wire) i.t.t.wire)
=/  =path     (slag 7 wire)
=/  log=tape  "[bone={<bone>} agent={<app>} nonce={<nonce>}] {<path>}"
=;  corkable=?
  =?  bones  corkable  [[ship bone] bones]
  (dec resubs)^bones
::  checks if this is a stale re-subscription
::
?.  =(resubs 1)
  ~?  veb  [ship (weld "stale %watch plea " log)]
  &
::  the current subscription can be safely corked if there
::  is a flow with a naxplanation ack on a backward bone
::
=+  backward-bone=(mix 0b10 bone)
?.  =(%2 (mod backward-bone 4))
  |
?~  (~(get by rcv.peer-state) backward-bone)
  |
~?  veb  [ship (weld "failed %watch plea " log)]
&
::
++  resubscriptions
  %+  roll  ~(tap by snd.peer-state)
  |=  $:  [=forward=bone message-pump-state:ames]
          subs=(jar path [bone sub-nonce=@])
      ==
  ?:  (~(has in closing.peer-state) forward-bone)
    ~?  veb
      :-  ship
      %+  weld  "stale flow bone={<forward-bone>} in closing, "
      "#{<~(wyt in live:packet-pump-state)>} packets retrying"
    subs
  ?~  duct=(~(get by by-bone.ossuary.peer-state) forward-bone)
    subs
  ?.  ?=([* [%gall %use sub=@ @ %out @ @ *] *] u.duct)
    subs
  =/  =wire           i.t.u.duct
  =/  nonce=(unit @)  ?~((slag 7 wire) ~ (slaw %ud &8.wire))
  %-  ~(add ja subs)
  ::  0 for old pre-nonce subscriptions
  ::
  :_  [forward-bone ?~(nonce 0 u.nonce)]
  ?~  nonce  wire
  ::  don't include the sub-nonce in the key
  ::
  (weld (scag 7 wire) (slag 8 wire))
--
