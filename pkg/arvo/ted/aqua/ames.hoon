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
    ::  packets from comets;
    ::  to guarantee that attestations are always injected first
    ::
      comets=(set to=@p)
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
++  handle-send
  =,  ames
  |=  [sndr=@p rcvr=@p lan=lane =shot pac=@ comets=(set @p)]
  ^-  %+  each
        (unit aqua-event)
      [? aqua-event] :: if comet attestation handle and update state
  ::
  ?:  &(!sam.shot req.shot)  :: is fine request; handle
    =/  [%0 =peep]  (sift-wail `@ux`content.shot)
    :-  %&
    :-  ~  ^-  aqua-event
    :-  %read
    [[[rcvr rcvr-tick.shot] path.peep] [lan sndr-tick.shot] num.peep]
  ::
  =+  ;;  sign-attest=(soft [~ signature=@ signed=@])
          (mole |.((cue content.shot)))
  =+  ^=  open-pack
      ?.  ?=(^ sign-attest)
        ~
      ;;  (soft [~ open-packet:ames-raw])
      (mole |.((cue signed:(need u.sign-attest))))
  ::
  =/  first-atts=?  &(?=(^ open-pack) !(~(has in comets) rcvr))
  ?:  ?&  ::  skip if sndr is not a comet
          ::
          ?|  ?=(%pawn (clan:title sndr))
              ?=(%pawn (clan:title sndr.shot))
          ==
          ::  if this is going to be forwarded, skip checks
          ::
          =(rcvr rcvr.shot)
          ::  check attestation
          ::
          ?|  ?&  ?=(~ open-pack)
                  ::  if this is not an attestation packet, check that we have already
                  ::  added an attestation to the set
                  ::
                  !(~(has in comets) rcvr)
              ==
              ?&  ::  if this is an attestation packet, check if the rcvr has
                  ::  the comet as %known -- this is a workaround to prevent a
                  ::  bail:evil that will end up blocking the queue of the
                  ::  %aqua host, when it tries to decrypt an open-packet
                  ::
                  ::  is-known  XX
                  ::  XX this is not enough. at this point the comet might not
                  ::
                  ::  be known but a previous resend of the attestation packet
                  ::  will make it known, and this one will fail to decrypt and
                  ::  then we will bail:evil.
                  ::
                  ::  the solution is to track attestation packets and only
                  ::  deliver them one time, and flag them as such
                  ::
                  ::  we track attestation packets to every ship to only deliver
                  ::  them once.
                  ::
                  ::  if we have processed the attestaton for this rcvr; skip
                  ::
                  !first-atts
      ==  ==  ==
    [%& ~]
  :-  %|
  :-  first-atts  ^-  aqua-event
  [%event rcvr /a/newt/0v1n.2m9vh %hear lan pac]
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
      =+  rule=(~(get by rules.state) sndr=who rcvr)
      ?.  ?&  ?=(^ rule)
              ?=(?(%drop-link [%drop-next *] %hold-link) u.rule)
          ==
        =/  hear-lane   (ship-to-lane who)
        =/  =shot:ames  (sift-shot:ames q.q.ue)
        =/  ev=(each (unit aqua-event) [? aqua-event])
          (handle-send who rcvr hear-lane shot pac=q.q.ue comets.state)
        ?:  ?=([%& ~] ev)  `this
        :_  this
        ?:  ?=([%| *] ev)
          :: ~!  ev
          =?  comets.state  -.p.ev
            (~(put in comets.state) rcvr)
          (emit-aqua-events our.bowl ^-((list aqua-event) [+.p.ev ~]))
        ::  don't update state, but process packet
        ::
        (emit-aqua-events our.bowl ^-((list aqua-event) [u.p.ev ~]))
      ?-    u.rule
          %drop-link  `this  :: drop all packets [sndr -> rcvr]
      ::
          [%drop-next n=@]   :: drop this packet [sndr -> rcvr]; update count
        =.  rules.state
          ?:  =(1 n.u.rule)
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
++  handle-aqua-rule
  |=  aq=rule-actions
  ^-  (quip card:agent:gall _this)
  ?-    -.aq
      %drop-link
    =.  state
      state(rules (~(put by rules.state) [from to]:aq %drop-link))
    `this
  ::
      %drop-next
    =.  state
      state(rules (~(put by rules.state) [from to]:aq [%drop-next n.aq]))
    `this
  ::
      %flush-link
    ::  remove all rules
    ::
    =.  rules.state  (~(del by rules.state) [from to]:aq)
    =;  [c=(list card:agent:gall) =_this]
      [c this(ames.state ~)]  :: clear queue
    %+  reel   ames.state
    |=  [[who=@p ue=unix-effect] c=(list card:agent:gall) t=_this]
    ?>  ?=(%send -.q.ue)
    =^  new-c  t  (handle-unix-effect who ue)
    ?>  ?=([^ ~] new-c)
    ::  new-c has only one card so we handle in reverse of arrival order
    ::    (XX add flag for in-order arrival)
    ::
    [(weld new-c c)]^t
  ::
      %clear-rules
    =.  state
      state(rules (~(del by rules.state) [from to]:aq))
    `this
  ::
      %hold-link
    =.  state
      state(rules (~(put by rules.state) [from to]:aq %hold-link))
    `this
  ::
  ==
::
++  handle-arvo-response  |=(* !!)
--
