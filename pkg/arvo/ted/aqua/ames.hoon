::  This needs a better SDN solution.  Every ship should have an IP
::  address, and we should eventually test changing those IP
::  addresses.
::
::  For now, we broadcast every packet to every ship and rely on them
::  to drop them.
::
/-  aquarium, spider
/+  aqua-vane-thread
=,  aquarium
|%
++  emit-aqua-events
  |=  [our=ship aes=(list aqua-event)]
  ^-  (list card:agent:gall)
  [%pass /aqua-events %agent [our %aqua] %poke %aqua-events !>(aes)]~
::
++  handle-restore
  |=  [our=ship who=@p]
  ^-  (list card:agent:gall)
  %+  emit-aqua-events  our
  [%event who [/a/newt/0v1n.2m9vh %born ~]]~
::
++  handle-send
  =,  ames
  |=  [our=ship now=@da sndr=@p way=wire %send lan=lane pac=@]
  ^-  (list card:agent:gall)
  =/  rcvr=ship  (lane-to-ship lan)
  =/  hear-lane  (ship-to-lane sndr)
  =/  [ames=? =packet]  (decode-packet pac)
  ?:  &(!ames !resp==(& (cut 0 [2 1] pac)))
    =/  [=peep =purr]  (decode-request-info `@ux`(rsh 3^64 content.packet))
    %+  emit-aqua-events  our
    [%read [rcvr path.peep] [hear-lane num.peep]]~
  %+  emit-aqua-events  our
  [%event rcvr /a/newt/0v1n.2m9vh %hear hear-lane pac]~
::  +lane-to-ship: decode a ship from an aqua lane
::
::    Special-case one comet, since its address doesn't fit into a lane.
::
++  lane-to-ship
  |=  =lane:ames
  ^-  ship
  ::
  ?-  -.lane
    %&  p.lane
    %|  =/  s  `ship``@`p.lane
        ?.  =(s 0xdead.beef.cafe)
          s
        ~bosrym-podwyl-magnes-dacrys--pander-hablep-masrym-marbud
  ==
::  +ship-to-lane: encode a lane to look like it came from .ship
::
::    Never shows up as a galaxy, because Vere wouldn't know that either.
::    Special-case one comet, since its address doesn't fit into a lane.
::
++  ship-to-lane
  |=  =ship
  ^-  lane:ames
  :-  %|
  ^-  address:ames  ^-  @
  ?.  =(ship ~bosrym-podwyl-magnes-dacrys--pander-hablep-masrym-marbud)
    ship
  0xdead.beef.cafe
--
::
%+  aqua-vane-thread  ~[%restore %send]
|_  =bowl:spider
+*  this  .
++  handle-unix-effect
  |=  [who=@p ue=unix-effect]
  ^-  (quip card:agent:gall _this)
  =/  cards
    ?+  -.q.ue  ~
      %restore  (handle-restore our.bowl who)
      %send     (handle-send our.bowl now.bowl who ue)
    ==
  [cards this]
::
++  handle-arvo-response  |=(* !!)
--
