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
+$  held-pacs  (list [who=@p unix-effect])  ::  ames/mesa packets
+$  driver-state
  $:  rules=(map [from=@p to=@p] net-rule)  ::  XX more than one rule per link
      ames=held-pacs  :: XX  make a FIFO queue
      mesa=held-pacs  ::
  ==
::
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
  =+  ^=  peers
      ;;  (unit (map ship ?(%alien %known)))
      .^  *
          %gx
          (scot %p our)
          %aqua
          (scot %da now)
          /i/(scot %p rcvr)/ax/(scot %p rcvr)//(scot %da now)/peers/noun
      ==
  =/  is-known=?
    ?.  ?=(^ peers)  |
    =+  peer=(~(get by u.peers) sndr)
    ?.  ?=(^ peer)  |
    =(%known u.peer)
  ?:  ?&  :: =-  ~?  -  %is-pawn
          ::     -
          ?|  ?=(%pawn (clan:title sndr))
              ?=(%pawn (clan:title sndr.shot))
          ==
          ::  if this is going to be forwarded, skip checks
          ::
          :: =-  ~?  -  %not-forwarded
          ::     -
          =(rcvr rcvr.shot)
          =+  ;;(out=(soft [~ signature=@ signed=@]) (mole |.((cue content.shot))))
          ?|  ?&  ?=(~ out)
                  ::  if this is not an attestation packet, check that the receiver
                  ::  has the peer as known
                  ::
                  !is-known
              ==
              ?&  ?=(^ out)
                  ?=(^ ;;((soft [~ open-packet:ames-raw]) (mole |.((cue signed:(need (need out)))))))
                  ::  if this is an attestation packet, check if the rcvr has the comet
                  ::  as %known -- this is a workaround to prevent a bail:evil that will
                  ::  end up blocking the queue of the %aqua host, when it tries to decrypt
                  ::  an open-packet
                  ::
                  is-known
      ==  ==  ==
    :: ~&  >   "skip packet"^content.shot
    ~
  :: ~&  >>   "inject packet"^content.shot
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
=|  state=driver-state
%+  aqua-vane-thread  ~[%restore %send %push %filter]
|_  =bowl:spider
+*  this  .
++  handle-unix-effect
  |=  [who=@p ue=unix-effect]
  ^-  (quip card:agent:gall _this)
  =^  cards  this
    ?+  -.q.ue  `this
      %restore  (handle-restore our.bowl who)^this
    ::
        %send
      =/  rcvr=@p  (lane-to-ship p.q.ue)
      =+  rule=(~(get by rules.state) who rcvr=(lane-to-ship p.q.ue))
      ?.  ?&  ?=(^ rule)
              ?=(?(%drop-link [%drop-next ~] %hold-link) u.rule)
          ==
        (handle-send our.bowl now.bowl who ue)^this
      ?-    u.rule
          %drop-link  `this  :: drop all packets [sndr -> rcvr]
      ::
          [%drop-next n=@]   :: drop this packet [sndr -> rcvr], and update count
        =.  rules.state
          ?:  =(0 n.u.rule)
            (~(del by rules.state) who^rcvr)
          (~(put by rules.state) who^rcvr u.rule(n (dec n.u.rule)))
        `this
      ::
          %hold-link     :: hold onto this packet
        `this(ames.state [who^ue ames.state])
      ==
    ::
        %push
      ::  XX handle network rules
      (handle-push our.bowl now.bowl who ue)^this
    ==
  [cards this]
::
::
++  handle-aqua-rule
  |=  aq=rule-actions
  ^-  (quip card:agent:gall _this)
  ?-    -.aq
      %drop-link    `this
      %drop-next    `this
      %flush-link
    :_  this(ames.state ~, rules.state (~(del by rules.state) [from to]:aq))
    ^-  (list card:agent:gall)
    %-  zing
    %+  turn  ames.state
    |=  [who=@p ue=unix-effect]
    ?>  ?=(%send -.q.ue)
    (handle-send our.bowl now.bowl who ue)
    ::
      %clear-rules  `this
    ::
      %hold-link
    =.  state
      state(rules (~(put by rules.state) [from to]:aq %hold-link))
    `this
  ==
::
++  handle-arvo-response  |=(* !!)
--
