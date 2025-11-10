::  This needs a better SDN solution.  Every ship should have an IP
::  address, and we should eventually test changing those IP
::  addresses.
::
::  For now, we broadcast every packet to every ship and rely on them
::  to drop them.
::
/-  aquarium, spider
/+  aqua-vane-thread
/=  ames-raw  /sys/vane/ames
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
  =/  =shot      (sift-shot pac)
  ?:  &(!sam.shot req.shot)  :: is fine request
    =/  [%0 =peep]  (sift-wail `@ux`content.shot)
    %+  emit-aqua-events  our
    :_  ~
    :-  %read
    [[[rcvr rcvr-tick.shot] path.peep] [hear-lane sndr-tick.shot] num.peep]
  %+  emit-aqua-events  our
  [%event rcvr /a/newt/0v1n.2m9vh %hear hear-lane pac]~
::  XX  this should use the (TODO) message layer in %ames
::
++  handle-push
  =,  ames
  |=  [our=ship now=@da sndr=@p way=wire %push lan=(list lane:pact:ames) q=@]
  ^-  (list card:agent:gall)
  =/  =pact:pact:ames  (parse-packet:ames-raw q)
  =/  rcvr=ship
    ?-  +<.pact
      %peek  her.name.pact
      %poke  her.ack.pact
      %page  ?>  ?=(^ lan)
             ?>  ?=(@ i.lan)
             `@p`i.lan
    ==
  =/  lan=lane:pact:ames  ?:(?=(%page +<.pact) `@ux`rcvr `@ux`sndr)
  %+  emit-aqua-events  our
  [%event rcvr /a/newt/0v1n.2m9vh %heer lan q]~
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
        ~londeg-tirlys-somlyd-poltus--pintyn-tarbyl-bicnux-marbud
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
  ?.  =(ship ~londeg-tirlys-somlyd-poltus--pintyn-tarbyl-bicnux-marbud)
    ship
  0xdead.beef.cafe
::
--
::
%+  aqua-vane-thread  ~[%restore %send %push]
|_  =bowl:spider
+*  this  .
++  handle-unix-effect
  |=  [who=@p ue=unix-effect]
  ^-  (quip card:agent:gall _this)
  =/  cards
    ?+  -.q.ue  ~
      %restore  (handle-restore our.bowl who)
      %send     (handle-send our.bowl now.bowl who ue)
      %push     (handle-push our.bowl now.bowl who ue)
    ==
  [cards this]
::
++  handle-arvo-response  |=(* !!)
--
