::  Prints number of Ames flows that can be closed
::
::    |stale-flows, =veb %1  :: all flows that got %nacked
::    |stale-flows, =veb %2  :: stale flows that keep (re)trying to connect
::    |stale-flows, =veb %21 :: ... per app (only forward)
::    |stale-flows, =veb %3  :: stale resubscriptions
::    |stale-flows, =veb %31 :: ... still sending live packets
::    |stale-flows, =veb %32 :: ... with additional log per subscription
::    |stale-flows, =veb %4  :: print live naxplanation flows
::    |stale-flows, =veb %5  :: nacked pokes
::
/=  gall-raw  /sys/vane/gall
::
=>  |%
    +$  key   [subscriber=term =path =ship app=term]
    +$  val   [=ship =bone nonce=@ =flow close=? live=?]
    +$  subs  (jar key val)
    +$  pags  (jar app=term [dst=term =ship =path])  ::  per-agent
    +$  naks  (set [ship bone =flow close=?])
    +$  flow  (unit ?(%poke [%watch nonce=@ud]))
    ::  verbosity
    ::
    +$  veb  ?(%0 %1 %2 %21 %3 %31 %4 %5 ~)
    ::
    ++  active-nonce
      |=  [=flow nonce=@ud]
       ?&  ?=([~ %watch nonce=@ud] flow)
           =(nonce.u.flow nonce)
       ==
    ::
    ++  resubs
      |=  [=subs =veb]
      ^-  [stale=@ closing=@]
      %+  roll  ~(tap by subs)
      |=  [[=key v=(list val)] s=@ c=@]
      =/  in-close=@
        (roll v |=([[@ @ @ * c=? @] n=@] ?:(c +(n) n)))
      ~?  &(=(%3 veb) (gth (lent v) 1))
        %+  weld  ?:  =(in-close 0)  ""
          "[#{<in-close>} %close] "
        "#{<(dec (lent v))>} stale resubs on {<key>}"
      :_  (add c in-close)
      %+  add   s
      %+  roll  v
      |=  [val n=@]
      ~?  &(=(%31 veb) live !(active-nonce flow nonce))
        "stale resubscription on {<key>} still live, skip"
      ?:  |(live (active-nonce flow nonce))  n
      ~?  >>  =(%32 veb)  key^wire-nonce=nonce^flow=flow
      +(n)
    ::
    ++  nacked-flow
      |=  [=naks action=?(%poke %watch)]
      ^-  [out=@ closing=@]
      %-  ~(rep by naks)
      |=  [[@ @ =flow close=?] out=@ closing=@]
      =?  closing  &(close ?=(%watch action))  +(closing)
      :_  closing
      =?  out  ?|  &(=(%poke action) ?=([~ %poke] flow))
                   &(=(%watch action) ?=([~ %watch *] flow))
               ==
        +(out)
      out
    --
::
:-  %say
|=  $:  [now=@da eny=@uvJ bec=beak]
        [arg=~ peer=(unit @p) dry=? =veb]
    ==
::
=/  peers-map
  .^((map ship ?(%alien %known)) %ax /(scot %p p.bec)//(scot %da now)/peers)
::
=/  our-gall  (gall-raw p.bec)
=/  gall-yokes
  .^((map dude:gall yoke:our-gall) %gy /(scot %p p.bec)//(scot %da now)/$)
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
=;  [[=subs =pags close=@ incoming=@ outgoing=@ nax=@] =naks]
  :-  %tang  %-  flop
  %+  weld
    :~  leaf+"#{<incoming>} live backward flows"
        leaf+"#{<outgoing>} live forward flows"
        leaf+"#{<nax>} live naxplanations"
        leaf+"#{<close>} flows in closing"
        leaf+"#{<(resubs subs veb)>} stale resubscriptions"
        leaf+"#{<(nacked-flow naks %watch)>} nacked %watch(s)"
        leaf+"#{<(nacked-flow naks %poke)>} nacked %poke(s)"
    ==
  ?.  =(%21 veb)  ~
  :-  leaf+"----------------------------------"
  %+  turn  %+  sort  ~(tap by pags)
            |=  [[* v=(list)] [* w=(list)]]
            (gth (lent v) (lent w))
  |=  [app=term v=(list [dst=term =ship =path])]
  :-  %leaf
  %+  weld  "#{<(lent v)>} flows for {<app>}"
  ?.  =(1 (lent v))  ~
  ?>  ?=(^ v)
  " on {<ship.i.v>} to {<dst.i.v>} at {<path.i.v>}"
::
%+  roll  peers
|=  [=ship [=subs p=pags cl=@ in=@ ou=@ na=@] =naks]
?:  ?&  ?=(^ peer)
        !=(u.peer ship)
    ==
  +<+
=+  .^  =ship-state:ames
        %ax  /(scot %p p.bec)//(scot %da now)/peers/(scot %p ship)
    ==
=/  =peer-state:ames  ?>(?=(%known -.ship-state) +.ship-state)
::
|^  [stale nacks]
::
++  stale
  %+  roll  ~(tap by snd.peer-state)
  |=  $:  [=bone message-pump-state:ames]
          subs=_subs   pags=_p
          close=_cl
          incoming=_in  outgoing=_ou  nax=_na
      ==
  =,  packet-pump-state
  =+  closing=(~(has ^in closing.peer-state) bone)
  :-  ?~  duct=(~(get by by-bone.ossuary.peer-state) bone)  subs
      ?.  ?=([* [%gall %use sub=@ @ %out ship=@ app=@ *] *] u.duct)
        subs
      =/  =wire  i.t.u.duct
      ::  nonce = 0 => pre-nonce subscription
      ::
      =/  nonce=@     ?~((slag 7 wire) 0 ?~(n=(slaw %ud &8.wire) 0 u.n))
      =*  subscriber  &3.wire
      =*  agent       &7.wire
      ::  skip the sub-nonce in the subscription path
      ::
      =/  path  ?:(=(0 nonce) |7.wire |8.wire)
      =+  key=[path ship agent]
      =/  =flow  (flow-type key nonce)
      ?~  yoke=(~(get by gall-yokes) agent)
        subs
      ?:  ?=(%nuke -.u.yoke)
        subs
      ?:  &(=(0 nonce) !(~(has by boat.u.yoke) key))
        ::  %pokes don't have an entry in boat.yoke, so we skip them
        ::
        ::  this could also be an %ames/%gall desync where %gall deleted the
        ::  (pre-nonce) subscription but %ames didn't
        ::  XX  we can't know for sure (we would need to know inspect the agent)
        ::
        subs
      (~(add ja subs) subscriber^key [ship bone nonce flow closing ?=(^ live)])
  ?~  live  [pags close incoming outgoing nax]
  ::  only forward flows
  ::
  =?  pags  =(%0 (mod bone 4))
    ?~  duct=(~(get by by-bone.ossuary.peer-state) bone)
      pags
    ?.  ?=([* [%gall %use sub=@ @ %out @ @ *] *] u.duct)
      pags
    =/  =wire  i.t.u.duct
    (~(add ja pags) (snag 2 wire) (snag 6 wire) ship (slag 7 wire))
  ::
  =?  close  closing  +(close)
  ~?  =(%2 veb)
    =/  arrow=tape
      ?+  (mod bone 4)  ~|([%odd-bone bone] !!)
        %0  "<-"
        %1  "->"
        %3  "<-"
      ==
    "{arrow} ({(cite:title ship)}) bone=#{<bone>} closing={<closing>}"
  ::
  =/  is-nax=?  =(%3 (mod bone 4))
  ~?  &(=(%4 veb) is-nax)
    "nax: ({(cite:title ship)}) bone=#{<bone>} closing={<closing>}"
  :+  pags  close
  ?+  (mod bone 4)  ~|([%odd-bone bone] !!)
    %0  [incoming +(outgoing) nax]
    %1  [+(incoming) outgoing nax]
    %3  [incoming outgoing +(nax)]
  ==
::
++  flow-type
  |=  [key=[path @p agent=term] nonce=@ud]
  ^-  flow
  ?~  yoke=(~(get by gall-yokes) agent.key)
    ~
  ?:  ?=(%nuke -.u.yoke)  ~
  ?:  (~(has by boat.u.yoke) key)
    [~ %watch (~(got by boar.u.yoke) key)]
  ?.  =(nonce 0)
    [~ %watch 0]
  ::  XX  if %gall and %ames got desynced, this could still be a pre-nonce
  ::  %watch flow that was nacked, removed from %gall, but not from %ames
  ::  because of the %cork/%leave desync
  ::
  `%poke
::
++  nacks
  %+  roll  ~(tap by rcv.peer-state)
  |=  [[=bone *] n=_naks]
  ?.  =(%2 (mod bone 4))
    ::  not a naxplanation ack bone
    ::
    n
  ::  by only corking forward flows that have received
  ::  a nack we avoid corking the current subscription
  ::
  =+  target=(mix 0b10 bone)
  ::  make sure that the nack bone has a forward flow
  ::
  ?~  duct=(~(get by by-bone.ossuary.peer-state) target)
    n
  ?.  ?=([* [%gall %use sub=@ @ %out @ @ *] *] u.duct)
    n
  =/  =wire    i.t.u.duct
  =/  nonce=@  ?~((slag 7 wire) 0 ?~(n=(slaw %ud &8.wire) 0 u.n))
  =*  agent    &3.wire
  =/  path     ?~(=(0 nonce) |7.wire |8.wire)
  =/  =flow    (flow-type [path ship agent] nonce)
  =+  closing=(~(has ^in closing.peer-state) target)
  =/  log=tape
    =+  apps="[sub={<&3.wire>} -> pub={<agent>}]"
    %+  weld  "[bone={<target>} nonce={<nonce>} {apps}"
    " close={<closing>} {<flow=flow>}] {<path>}"
  ~?  =(%1 veb)  flow^log
  ~?  &(=(%5 veb) ?=([~ %poke] flow))  log
  (~(put ^in n) [ship target flow closing])
--
