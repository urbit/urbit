::  +stale-flows: prints number of ames flows that can be closed
::
::    |stale-flows, =veb %1  :: flows from nacking initial subscriptions
::    |stale-flows, =veb %2  :: stale flows that keep (re)trying to connect
::    |stale-flows, =veb %21 :: ... per app (only forward)
::    |stale-flows, =veb %3  :: stale resubscriptions
::
=>  |%
    +$  subs  (jar path [ship bone @])
    +$  pags  (jar app=term [dst=term =ship =path])  ::  per-agent
    +$  naks  (set [ship bone])
    ::  verbosity
    ::
    +$  veb  ?(%0 %1 %2 %21 %3 %31)
    ::
    ++  resubs
      |=  [=subs =veb]
      ^-  @
      ::=/  sorted
      ::  %+  sort  ~(tap by subs)
      ::  |=([a=(list *) b=(list *)] (lte (lent a) (lent b)))
      %+  roll  ~(tap by subs)::sorted
      |=  [[k=path v=(list [ship bone @])] num=@]
      ~?  &(=(%3 veb) (gth (lent v) 1))
        "#{<(dec (lent v))>} stale resubs on {<k>}"
      ?.  (gth (lent v) 1)  num
      (add (lent v) num)
    --
::
:-  %say
|=  $:  [now=@da eny=@uvJ bec=beak]
        [arg=~ dry=? =veb]
    ==
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
=;  [[=subs =pags backward=@ forward=@] =naks]
  :-  %tang  %-  flop
  %+  weld
    :~  leaf+"#{<~(wyt in naks)>} flows from %nacking %watches"
        leaf+"#{<backward>} backward flows with >10 retries"
        leaf+"#{<forward>} forward flows with >10 retries"
        leaf+"#{<(resubs subs veb)>} stale resubscriptions"
    ==
  ?.  =(%21 veb)  ~
  :-  leaf+"----------------------------------"
  %+  turn  %+  sort  ~(tap by pags)
            |=  [[* v=(list)] [* w=(list)]]
            (gth (lent v) (lent w))
  |=  [app=term v=(list [dst=term =ship =path])]
  :-  %leaf
  %+  weld  "#{<(lent v)>} flows for {<app>} with >10 retries"
  ?.  =(1 (lent v))  ~
  ?>  ?=(^ v)
  " on {<ship.i.v>} to {<dst.i.v>} at {<path.i.v>}"
::
%+  roll  peers
|=  [=ship [=subs p=pags b=@ f=@] n=naks]
=+  .^  =ship-state:ames
        %ax  /(scot %p p.bec)//(scot %da now)/peers/(scot %p ship)
    ==
=/  =peer-state:ames  ?>(?=(%known -.ship-state) +.ship-state)
::
|^  [stale nacks]
::
++  stale
  %+  roll  ~(tap by snd.peer-state)
  |=  [[=bone message-pump-state:ames] subs=_subs pags=_p backward=_b forward=_f]
  =,  packet-pump-state
  :-  ?~  duct=(~(get by by-bone.ossuary.peer-state) bone)  subs
      ?.  ?=([* [%gall %use sub=@ @ %out @ @ nonce=@ pub=@ *] *] u.duct)
        subs
      =/  =wire           i.t.u.duct
      =/  nonce=(unit @)  (rush i.t.t.t.t.t.t.t.i.t.u.duct dem)
      %-  ~(add ja subs)
      :_  [ship bone ?~(nonce 0 u.nonce)]  :: 0, 1?
      ?~  nonce  wire
      ::  don't include the sub-nonce in the key
      ::
      (weld (scag 7 wire) (slag 8 wire))
  %+  roll  ~(tap in live)
  |=  [[* [packet-state:ames *]] pags=_pags out=[b=_backward f=_forward]]
  ::
  ::  only forward flows
  ::
  =?  pags  &(=(0 (end 0 bone)) (gth retries 10))
    ?~  duct=(~(get by by-bone.ossuary.peer-state) bone)
      pags
    ?.  ?=([* [%gall %use sub=@ @ %out @ @ nonce=@ pub=@ *] *] u.duct)
      pags
    =/  =wire  i.t.u.duct
    (~(add ja pags) (snag 2 wire) (snag 8 wire) ship (slag 9 wire))
  ::
  ~?  &(=(%2 veb) (gth retries 10))
    =+  arrow=?:(=(0 (end 0 bone)) "<-" "->")
    "{arrow} ({(cite:title ship)}) bone #{<bone>}, retries: #{<retries>}"
  :-  pags
  =?  out  (gth retries 10)
    ?:  =(0 (end 0 bone))
      [b.out +(f.out)]
    [+(b.out) f.out]
  out
::
++  nacks
  %+  roll  ~(tap by rcv.peer-state)
  |=  [[=bone *] nacks=_n]
  ?.  &(=(0 (end 0 bone)) =(1 (end 0 (rsh 0 bone))))
    ::  not a naxplanation ack bone
    ::
    nacks
  ::  by only corking forward flows that have received
  ::  a nack we avoid corking the current subscription
  ::
  =+  target=(mix 0b10 bone)
  ::  make sure that the nack bone has a forward flow
  ::
  ?~  duct=(~(get by by-bone.ossuary.peer-state) target)
    nacks
  ?.  ?=([* [%gall %use sub=@ @ %out @ @ nonce=@ pub=@ *] *] u.duct)
    nacks
  =/  =wire      i.t.u.duct
  ?>  ?=([%gall %use sub=@ @ %out @ @ nonce=@ pub=@ *] wire)
  =/  app=term   i.t.t.wire
  =/  nonce=@
    =-  ?~(- 0 u.-)
    (rush i.t.t.t.t.t.t.t.wire dem)
  =/  =path      t.t.t.t.t.t.t.t.wire
  ~?  =(%1 veb)  "[bone={<target>} nonce={<nonce>} agent={<app>}] {<path>}"
  (~(put in nacks) [ship target])
--
