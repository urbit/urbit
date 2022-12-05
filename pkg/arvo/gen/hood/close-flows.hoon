::  |close-flows: sends a %cork to all ships that have nacked a %watch
::
::    It runs in dry mode by default, printing the number of flows that
::    can be closed. To actually send the corks, run as:
::
::      |close-flows, =dry-run |
::
:-  %say
|=  $:  [now=@da eny=@uvJ bec=beak]
        [arg=~ dry-run=?]
    ==
::
=/  peers-map
  .^  (map ship ?(%alien %known))
      %ax  /(scot %p p.bec)//(scot %da now)/peers
  ==
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
=;  krocs=(list [=ship =bone])
    helm-ames-kroc+[krocs dry-run]
::
%-  zing
%+  turn  peers
|=  =ship
=+  .^  =ship-state:ames
        %ax  /(scot %p p.bec)//(scot %da now)/peers/(scot %p ship)
    ==
=/  =peer-state:ames  ?>(?=(%known -.ship-state) +.ship-state)
=/  forward-flows=(set @)
  %-  ~(gas in *(set @))
  %+  turn
    %+  sort  ~(tap by by-bone.ossuary.peer-state)
    |=  [[a=@ *] [b=@ *]]  (lth a b)
  head
::
=/  nacks=(list bone)
  %+  turn
    (sort ~(tap by rcv.peer-state) |=([[a=@ *] [b=@ *]] (lth a b)))
  head
::
^-  (list [@p @ud])
%+  murn  nacks
|=  =bone
?.  &(=(0 (end 0 bone)) =(1 (end 0 (rsh 0 bone))))
  ::  not a naxplanation ack bone
  ::
  ~
::  by only corking forward flows that have received
::  a nack we avoid corking the current subscription
::
=+  target=(mix 0b10 bone)
::  make sure that the nack bone has a forward flow
::
?.  (~(has in forward-flows) target)  ~
?~  duct=(~(get by by-bone.ossuary.peer-state) target)
  ~
=/  =wire  (snag 1 u.duct)
::  only wires with nonces are subscriptions
::
?.  ?=([%gall %use sub=@ @ %out @ @ nonce=@ pub=@ ~] wire)
  ~
::  extra security check so we know that the nonce is a number
::
?~  (rush i.t.t.t.t.t.t.t.wire dem)  ~
`[ship target]
