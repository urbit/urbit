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
::
^-  (list [@p @ud])
%+  murn  ~(tap by rcv.peer-state)
|=  [=bone *]
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
?~  duct=(~(get by by-bone.ossuary.peer-state) target)
  ~
=;  =wire
  ?~(wire ~ `[ship target])
::  TMI
::
=>  .(duct `^duct`duct)
|-  ^-  wire
?~  duct  ~
::  inspect the wires in the duct, looking for subscriptions
::
?.  ?=([%gall %use sub=@ @ %out @ @ nonce=@ pub=@ *] i.duct)
  $(duct t.duct)
::  extra security check so we know that the nonce is a number
::
?~  (rush i.t.t.t.t.t.t.t.i.duct dem)  ~
i.duct
