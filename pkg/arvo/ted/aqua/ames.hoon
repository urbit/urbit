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
:: $nat-arg: config for a single NAT
::
::   .type: restricted-cone or symmetrical
::   .timeout: time before inactive NAT hole closes
::             if null, no expiry
::
+$  nat-arg
  [type=?(%cone %symm) timeout=(unit @dr)]
:: $ames-thread-arg: initialization args for ames thread
::
::   .drop-prob: 1/x probability of dropping a packet
::               if null, no packets dropped
::   .eny: entropy to initialize RNG for packet dropping
::   .nat: $nat-arg for each NAT'd ship
::
+$  ames-thread-arg
  [drop-prob=(unit @ud) eny=@ nat=(map ship nat-arg)]
:: $priv: private address, maybe behind NAT
::
+$  priv    address:ames
:: $pub: public address, maybe in front of NAT
::
+$  pub     address:ames
:: $remote: receipient address, unknown if NAT'd
::
+$  remote  address:ames
:: $timeouts: NAT hole inactivity timeouts
::            if null, no timeout
::
+$  timeouts  (map priv (unit @dr))
:: $cone: restricted-cone NAT tables
::   
::   .in: inbound mappings
::   .out: outbound mappings
::   .exp: remote connection expiry times
::
+$  cone
  $:  in=(map pub priv)
      out=(map priv pub)
      exp=(map [pub remote] (unit @da))
  ==
:: $symm: symmetric NAT tables
::
::   .in: inbound mappings
::   .out: outbound mappings
::   .ind: indexes
::         .p: NAT number (max 63)
::         .q: binding count (max 1024 per NAT)
::   .exp: remote connection expiry times
::
+$  symm
  $:  in=(map [pub remote] priv)
      out=(map [priv remote] pub)
      ind=(map priv (pair @ @))
      exp=(map [pub remote] (unit @da))
  ==
--
::
=|  [=cone =symm =timeouts drop=(unit @ud) rng=_og]
=*  state  -
::
|%
++  ames-core
  |_  =bowl:spider
  :: +handle-init: initialize NAT & packet drop config
  ::
  ++  handle-init
    |=  ames-thread-arg
    ^-  (quip card:agent:gall _state)
    =/  nat-args=(list [=ship nat-arg])
      ~(tap by nat)
    =/  count-symm  1
    =/  count-cone  1
    |-
    ?~  nat-args
      ?~  drop-prob
        [~ state]
      [~ state(drop drop-prob, rng ~(. og eny))]
    =/  =priv  (ship-to-address ship.i.nat-args)
    ?-    type.i.nat-args
        %cone
      ?:  =(count-cone 65.535)  ~|(%max-65535-cone-nats !!)
      =/  =pub  (cat 3 ~natres-natres count-cone)
      %=  $
        nat-args    t.nat-args
        count-cone  +(count-cone)
        out.cone    (~(put by out.cone) priv pub)
        in.cone     (~(put by in.cone) pub priv)
        timeouts    (~(put by timeouts) priv timeout.i.nat-args)
      ==
    ::
        %symm
      ?:  =(count-symm 63)  ~|(%max-63-symmetric-nats !!)
      %=  $
        nat-args    t.nat-args
        count-symm  +(count-symm)
        ind.symm    (~(put by ind.symm) priv [count-symm 0])
        timeouts    (~(put by timeouts) priv timeout.i.nat-args)
      ==
    ==
  ::
  ++  emit-aqua-events
    |=  aes=(list aqua-event)
    ^-  (list card:agent:gall)
    [%pass /aqua-events %agent [our.bowl %aqua] %poke %aqua-events !>(aes)]~
  ::
  ++  handle-restore
    |=  who=@p
    ^-  (quip card:agent:gall _state)
    :_  state
    %-  emit-aqua-events
    [%event who [/a/newt/0v1n.2m9vh %born ~]]~
  ::
  ++  handle-send
    =,  ames
    |=  [sndr=@p way=wire %send lan=lane pac=@]
    ^-  (quip card:agent:gall _state)
    :: maybe pass out through NAT
    ::
    =/  sndr-priv  (ship-to-address sndr)
    =/  rcvr-pub  `@ux`p.lan
    =^  sndr-pub  state  (outbound-nat sndr-priv rcvr-pub)
    :: maybe randomly drop packet
    ::
    =^  dropped  state  maybe-drop
    ?:  dropped
       ~&  >>  [%dropped from=sndr as=sndr-pub to=rcvr-pub]
      [~ state]
    :: maybe pass in through NAT
    ::
    =^  rcvr-priv=(unit priv)  state  (inbound-nat sndr-pub rcvr-pub)
    ?~  rcvr-priv 
      ~&  >>  [%nat-blocked from=sndr as=sndr-pub to=rcvr-pub]
      [~ state]
    =/  rcvr=@p  (address-to-ship u.rcvr-priv)
    :: deliver
    ::
    =/  [ames=? =packet]  (decode-packet pac)
    ?:  &(!ames !resp==(& (cut 0 [2 1] pac)))
      =/  [=peep =purr]  (decode-request-info `@ux`(rsh 3^64 content.packet))
      :_  state
      %+  emit-aqua-events  our
      [%read [rcvr path.peep] [%.n sndr-pub] num.peep]~
    :_  state
    %-  emit-aqua-events
    [%event rcvr /a/newt/0v1n.2m9vh %hear [%.n sndr-pub] pac]~
  :: +maybe-drop: calculate if we should drop a given packet
  ::
  ++  maybe-drop
    ^-  [? _state]
    ?~  drop
      [| state]
    =^  rnd  rng  (rads:rng u.drop)
    ?:  =(0 rnd)
      [& state]
    [| state]
  :: +outbound-nat: maybe put outbound packet through NAT
  ::
  ++  outbound-nat
    |=  [=priv =remote]
    ^-  [pub _state]
    =^  u-pub-a=(unit pub)  state
      (outbound-cone priv remote)
    =^  u-pub  state
      ?~  u-pub-a
        (outbound-symm priv remote)
      [u-pub-a state]
    ?~  u-pub
      [priv state]
    [u.u-pub state]
  :: +inbound-nat: maybe inbound blocked or traverses NAT
  ::
  ++  inbound-nat
    |=  [=remote =pub]
    ^-  [(unit priv) _state]
    ?:  =(~natres-natres (^sein:title `@p`pub))
      (inbound-cone remote pub)
    ?:  =(~natsym-natsym (^sein:title `@p`pub))
      (inbound-symm remote pub)
    [`pub state]
  :: +outbound-cone: outbound restricted-cone NAT processor
  ::
  ++  outbound-cone
    |=  [=priv =remote]
    ^-  [(unit pub) _state]
    =/  u-pub  (~(get by out.cone) priv)
    ?~  u-pub
      [~ state]
    =/  timeout  (~(got by timeouts) priv)
    =/  exp=(unit @da)
      ?~  timeout  ~
      (some (add now.bowl u.timeout))
    =.  exp.cone  (~(put by exp.cone) [u.u-pub remote] exp)
    [u-pub state]
  :: +outbound-symm: outbound symmetric NAT processor
  ::
  ++  outbound-symm
    |=  [=priv =remote]
    ^-  [(unit pub) _state]
    =/  u-ind  (~(get by ind.symm) priv)
    ?~  u-ind
      [~ state]
    =/  timeout  (~(got by timeouts) priv)
    =/  exp=(unit @da)
      ?~  timeout  ~
      (some (add now.bowl u.timeout))
    =/  u-pub  (~(get by out.symm) [priv remote])
    ?~  u-pub
      ?:  =(1.024 q.u.u-ind)  ~|(%max-1024-symmetric-bindings !!)
      =/  =pub
        (cat 3 ~natsym-natsym (mix (lsh 1^5 p.u.u-ind) q.u.u-ind))
      =:  in.symm   (~(put by in.symm) [pub remote] priv)
          out.symm  (~(put by out.symm) [priv remote] pub)
          ind.symm  (~(put by ind.symm) priv [p.u.u-ind +(q.u.u-ind)])
          exp.symm  (~(put by exp.symm) [pub remote] exp)
        ==
      [`pub state]
    =.  exp.symm  (~(put by exp.symm) [u.u-pub remote] exp)
    [u-pub state]
  :: +inbound-cone: inbound restricted-cone NAT processor
  ::
  ++  inbound-cone
    |=  [=remote =pub]
    ^-  [(unit priv) _state]
    =/  u-priv  (~(get by in.cone) pub)
    ?~  u-priv
      [~ state]
    =/  u-exp  (~(get by exp.cone) [pub remote])
    ?~  u-exp
      [~ state]
    ?~  u.u-exp
      [u-priv state]
    ?:  (lth u.u.u-exp now.bowl)
      [~ state]
    =/  timeout  (need (~(got by timeouts) u.u-priv))
    =/  exp=(unit @da)  (some (add now.bowl timeout))
    =.  exp.cone  (~(put by exp.cone) [pub remote] exp)
    [u-priv state]
  :: +inbound-symm: inbound symmetric NAT processor
  ::
  ++  inbound-symm
    |=  [=remote =pub]
    ^-  [(unit priv) _state]
    =/  u-priv  (~(get by in.symm) [pub remote])
    ?~  u-priv
      [~ state]
    =/  u-exp  (~(get by exp.symm) [pub remote])
    ?~  u-exp
      [~ state]
    ?~  u.u-exp
      [u-priv state]
    ?:  (lth u.u.u-exp now.bowl)
      [~ state]
    =/  timeout  (need (~(got by timeouts) u.u-priv))
    =/  exp=(unit @da)  (some (add now.bowl timeout))
    =.  exp.symm  (~(put by exp.symm) [pub remote] exp)
    [u-priv state]
  ::  +address-to-ship: decode a ship from an aqua lane's address
  ::
  ::    Special-case one comet, since its address doesn't fit into a lane.
  ::
  ++  address-to-ship
    |=  =address:ames
    ^-  ship  ^-  @
    ?.  =(address 0xdead.beef.cafe)
      address
    ~bosrym-podwyl-magnes-dacrys--pander-hablep-masrym-marbud
  ::  +ship-to-address: encode an address to look like it came from .ship
  ::
  ::    Special-case one comet, since its address doesn't fit into a lane.
  ::
  ++  ship-to-address
    |=  =ship
    ^-  address:ames  ^-  @
    ?.  =(ship ~bosrym-podwyl-magnes-dacrys--pander-hablep-masrym-marbud)
      ship
    0xdead.beef.cafe
  --
--
::
%+  aqua-vane-thread  ~[%restore %send]
|_  =bowl:spider
+*  this  .
++  init
  |=  args=vase
  ^-  (quip card:agent:gall _this)
  ?:  =(*vase args)
    [~ this]
  =^  cards  state
    (~(handle-init ames-core bowl) !<(ames-thread-arg args))
  [cards this]
::
++  handle-unix-effect
  |=  [who=@p ue=unix-effect]
  ^-  (quip card:agent:gall _this)
  =^  cards  state
    ?+  -.q.ue  [~ state]
      %restore  (~(handle-restore ames-core bowl) who)
      %send     (~(handle-send ames-core bowl) who ue)
    ==
  [cards this]
::
++  handle-arvo-response  _!!
--
