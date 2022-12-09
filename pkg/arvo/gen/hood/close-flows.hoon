::  (TODO: rename) |close-flows: shows how many stale ames flows can be closed
::                (each ames flow is represented by its first bone)
::
::    Run in verbose mode my default, to turn of: |close-flows, =veb |
::
::    TODO:
::    corks all ames flows that are in a stale state due to some
::    previous unhandled errors
::
::    It runs in dry mode by default, printing the number of flows that
::    can be closed. To actually send the corks, run as:
::
::      |close-flows, =dry-run |
::
:-  %say
|=  $:  [now=@da eny=@uvJ bec=beak]
        [arg=~ dry=? veb=?]
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
=/  agents-map
  .^  (list [app=term sub-nonce=@ud run-nonce=@t =boat:gall =boar:gall])
    %gr  (scot %p p.bec)  %base  (scot %da now)  ~
  ==
::
|^  ~?  veb  "{<~(wyt in nacks)>} flows from %nacking %watches"
    ~?  veb  "{<~(wyt in resubs)>} flows from stale resubscriptions"
    :+  %helm-ames-kroc  dry
    ~(tap in (~(uni in nacks) resubs))
::
++  resubs
  =+  peers=(~(gas in *(set ship)) peers)
  %+  roll  agents-map
  |=  $:  [app=term next-nonce=@ run-nonce=@t =boat:gall =boar:gall]
          resubs=(set [ship bone])
      ==
  =/  subs  ~(tap in ~(key by boat))
  ?~  subs  resubs
  =/  [=wire =dock]    i.subs
  =/  sub=@ud  (~(got by boar) wire dock)
  ::  ignores subscription with nonce=0, this is handled by +ap-rake
  ::  XX TODO handle also here?
  ::
  ?:  =(sub 0)  resubs
  ::  skip current subscription
  ::
  ?.  (lth sub (dec next-nonce))  resubs
  ?:  !(~(has in peers) p.dock)   resubs
  =+  .^  =ship-state:ames
          %ax  /(scot %p p.bec)//(scot %da now)/peers/(scot %p p.dock)
      ==
  =/  =peer-state:ames  ?>(?=(%known -.ship-state) +.ship-state)
  %-  ~(rep by by-duct.ossuary.peer-state)
  |=  [[=duct =bone] resubs=_resubs]
   =+  sub-nonce=(scot %ud sub)
   =/  mod=^wire
    :*  %gall  %use  app  run-nonce
        %out  (scot %p p.dock)  q.dock
        sub-nonce  wire
    ==
  ?.  ?&  ?=([* [%gall %use sub=@ @ %out @ @ nonce=@ pub=@ *] *] duct)
          =(mod i.t.duct(i.t.t.t.t.t.t.t sub-nonce))
      ==
    resubs
  (~(put in resubs) [p.dock bone])
::
++  nacks
  %+  roll  peers
  |=  [=ship nacks=(set [ship bone])]
  =+  .^  =ship-state:ames
          %ax  /(scot %p p.bec)//(scot %da now)/peers/(scot %p ship)
      ==
  =/  =peer-state:ames  ?>(?=(%known -.ship-state) +.ship-state)
  ::
  %+  roll  ~(tap by rcv.peer-state)
  |=  [[=bone *] nacks=_nacks]
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
  ?.  ?=([* [%gall %use sub=@ @ %out @ @ nonce=@ pub=@ *] *] duct)
    nacks
  (~(put in nacks) [ship target])
--
