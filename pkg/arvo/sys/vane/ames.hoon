::    Ames extends Arvo's %pass/%give move semantics across the network.
::
::    Ames receives packets as Arvo events and emits packets as Arvo
::    effects.  The runtime is responsible for transferring the bytes in
::    an Ames packet across a physical network to another ship.
::
::    The runtime tells Ames which physical address a packet came from,
::    represented as an opaque atom.  Ames can emit a packet effect to
::    one of those opaque atoms or to the Urbit address of a galaxy
::    (root node), which the runtime is responsible for translating to a
::    physical address.  One runtime implementation sends UDP packets
::    using IPv4 addresses for ships and DNS lookups for galaxies, but
::    other implementations may overlay over other kinds of networks.
::
::    A local vane can pass Ames a %plea request message.  Ames
::    transmits the message over the wire to the peer ship's Ames, which
::    passes the message to the destination vane.
::
::    Once the peer has processed the %plea message, it sends a
::    message-acknowledgment packet over the wire back to the local
::    Ames.  This ack can either be positive to indicate the request was
::    processed, or negative to indicate the request failed, in which
::    case it's called a "nack".  (Don't confuse Ames nacks with TCP
::    nacks, which are a different concept).
::
::    When the local Ames receives either a positive message-ack or a
::    combination of a nack and naxplanation (explained in more detail
::    below), it gives an %done move to the local vane that had
::    requested the original %plea message be sent.
::
::    A local vane can give Ames zero or more %boon response messages in
::    response to a %plea, on the same duct that Ames used to pass the
::    %plea to the vane.  Ames transmits a %boon over the wire to the
::    peer's Ames, which gives it to the destination vane on the same
::    duct the vane had used to pass the original %plea to Ames.
::
::    %boon messages are acked automatically by the receiver Ames.  They
::    cannot be nacked, and Ames only uses the ack internally, without
::    notifying the client vane that gave Ames the %boon.
::
::    If the Arvo event that completed receipt of a %boon message
::    crashes, Ames instead sends the client vane a %lost message
::    indicating the %boon was missed.
::
::    %plea messages can be nacked, in which case the peer will send
::    both a message-nack packet and a naxplanation message, which is
::    sent in a way that does not interfere with normal operation.  The
::    naxplanation is sent as a full Ames message, instead of just a
::    packet, because the contained error information can be arbitrarily
::    large.  A naxplanation can only give rise to a positive ack --
::    never ack an ack, and never nack a naxplanation.
::
::    Ames guarantees a total ordering of messages within a "flow",
::    identified in other vanes by a duct and over the wire by a "bone":
::    an opaque number.  Each flow has a FIFO queue of %plea requests
::    from the requesting ship to the responding ship and a FIFO queue
::    of %boon's in the other direction.
::
::    Message order across flows is not specified and may vary based on
::    network conditions.
::
::    Ames guarantees that a message will only be delivered once to the
::    destination vane.
::
::    Ames encrypts every message using symmetric-key encryption by
::    performing an elliptic curve Diffie-Hellman using our private key
::    and the public key of the peer.  For ships in the Jael PKI
::    (public-key infrastructure), Ames looks up the peer's public key
::    from Jael.  Comets (128-bit ephemeral addresses) are not
::    cryptographic assets and must self-attest over Ames by sending a
::    single self-signed packet containing their public key.
::
::    When a peer suffers a continuity breach, Ames removes all
::    messaging state related to it.  Ames does not guarantee that all
::    messages will be fully delivered to the now-stale peer.  From
::    Ames's perspective, the newly restarted peer is a new ship.
::    Ames's guarantees are not maintained across a breach.
::
::    A vane can pass Ames a %heed $task to request Ames track a peer's
::    responsiveness.  If our %boon's to it start backing up locally,
::    Ames will give a %clog back to the requesting vane containing the
::    unresponsive peer's urbit address.  This interaction does not use
::    ducts as unique keys.  Stop tracking a peer by sending Ames a
::    %jilt $task.
::
::    Debug output can be adjusted using %sift and %spew $task's.
::
!:
=,  ames
=*  point               point:jael
=*  public-keys-result  public-keys-result:jael
::  veb: verbosity flags
::
=/  veb-all-off
  :*  snd=`?`%.n  ::  sending packets
      rcv=`?`%.n  ::  receiving packets
      odd=`?`%.n  ::  unusual events
      msg=`?`%.n  ::  message-level events
      ges=`?`%.n  ::  congestion control
      for=`?`%.n  ::  packet forwarding
      rot=`?`%.n  ::  routing attempts
      kay=`?`%.n  ::  is ok/not responding
      fin=`?`%.n  ::  remote-scry
  ==
=/  packet-size  13
=>
~%  %ames  ..part  ~
|%
+|  %helpers
::  +trace: print if .verb is set and we're tracking .ship
::
++  trace
  |=  [mode=?(%ames %fine) verb=? =ship ships=(set ship) print=(trap tape)]
  ^+  same
  ?.  verb
    same
  ?.  =>  [ship=ship ships=ships in=in]
      ~+  |(=(~ ships) (~(has in ships) ship))
    same
  (slog leaf/"{(trip mode)}: {(scow %p ship)}: {(print)}" ~)
::  +qos-update-text: notice text for if connection state changes
::
++  qos-update-text
  |=  [=ship mode=?(%ames %fine) old=qos new=qos k=? ships=(set ship)]
  ^-  (unit tape)
  ::
  =+  trace=(cury trace mode)
  ?+  [-.old -.new]  ~
    [%unborn %live]  `"; {(scow %p ship)} is your neighbor"
    [%dead %live]    ((trace k ship ships |.("is ok")) ~)
    [%live %dead]    ((trace k ship ships |.("not responding still trying")) ~)
    [%unborn %dead]  ((trace k ship ships |.("not responding still trying")) ~)
    [%live %unborn]  `"; {(scow %p ship)} has sunk"
    [%dead %unborn]  `"; {(scow %p ship)} has sunk"
  ==
::  +split-message: split message into kilobyte-sized fragments
::
::    We don't literally split it here since that would allocate many
::    large atoms with no structural sharing.  Instead, each
::    static-fragment has the entire message and a counter.  In
::    +encrypt, we interpret this to get the actual fragment.
::
++  split-message
  ~/  %split-message
  |=  [=message-num =message-blob]
  ^-  (list static-fragment)
  ::
  =/  num-fragments=fragment-num  (met packet-size message-blob)
  =|  counter=@
  ::
  |-  ^-  (list static-fragment)
  ?:  (gte counter num-fragments)
    ~
  ::
  :-  [message-num num-fragments counter `@`message-blob]
  $(counter +(counter))
::  +assemble-fragments: concatenate fragments into a $message
::
++  assemble-fragments
  ~/  %assemble-fragments
  |=  [num-fragments=fragment-num fragments=(map fragment-num fragment)]
  ^-  *
  ::
  =|  sorted=(list fragment)
  =.  sorted
    =/  index=fragment-num  0
    |-  ^+  sorted
    ?:  =(index num-fragments)
      sorted
    $(index +(index), sorted [(~(got by fragments) index) sorted])
  ::
  (cue (rep packet-size (flop sorted)))
::  +jim: caching +jam
::
++  jim  |=(n=* ~+((jam n)))
++  spit
  |=  =path
  ^-  [pat=@t wid=@ud]
  =+  pat=(spat path)
  =+  wid=(met 3 pat)
  ?>  (lte wid 384)
  [pat wid]
::  +make-bone-wire: encode ship, rift and bone in wire for sending to vane
::
++  make-bone-wire
  |=  [her=ship =rift =bone]
  ^-  wire
  ::
  /bone/(scot %p her)/(scot %ud rift)/(scot %ud bone)
::  +parse-bone-wire: decode ship, bone and rift from wire from local vane
::
++  parse-bone-wire
  |=  =wire
  ^-  %-  unit
      $%  [%old her=ship =bone]
          [%new her=ship =rift =bone]
      ==
  ?.  ?|  ?=([%bone @ @ @ ~] wire)
          ?=([%bone @ @ ~] wire)
      ==
    ::  ignore malformed wires
    ::
    ~
  ?+    wire  ~
      [%bone @ @ ~]
    `[%old `@p`(slav %p i.t.wire) `@ud`(slav %ud i.t.t.wire)]
  ::
      [%bone @ @ @ ~]
    %-  some
    :^    %new
        `@p`(slav %p i.t.wire)
      `@ud`(slav %ud i.t.t.wire)
    `@ud`(slav %ud i.t.t.t.wire)
  ==
::  +make-pump-timer-wire: construct wire for |packet-pump timer
::
++  make-pump-timer-wire
  |=  [her=ship =bone]
  ^-  wire
  /pump/(scot %p her)/(scot %ud bone)
::  +parse-pump-wire: parse .her and .bone from |packet-pump wire
::
++  parse-pump-wire
  |=  [ship=@ bone=@]
  ^-  (unit [%pump her=^ship =^bone])
  ?~  ship=`(unit @p)`(slaw %p ship)
    ~
  ?~  bone=`(unit @ud)`(slaw %ud bone)
    ~
  `pump/[u.ship u.bone]
::
++  parse-fine-wire
  |=  [ship=@ =wire]
  ^-  (unit [%fine her=^ship =^wire])
  ?~  ship=`(unit @p)`(slaw %p ship)
    ~
  `fine/[u.ship wire]
::  +derive-symmetric-key: $symmetric-key from $private-key and $public-key
::
::    Assumes keys have a tag on them like the result of the |ex:crub core.
::
++  derive-symmetric-key
  ~/  %derive-symmetric-key
  |=  [=public-key =private-key]
  ^-  symmetric-key
  ::
  ?>  =('b' (end 3 public-key))
  =.  public-key  (rsh 8 (rsh 3 public-key))
  ::
  ?>  =('B' (end 3 private-key))
  =.  private-key  (rsh 8 (rsh 3 private-key))
  ::
  `@`(shar:ed:crypto public-key private-key)
::  +encode-keys-packet: create key request $packet
::
++  encode-keys-packet
  ~/  %encode-keys-packet
  |=  [sndr=ship rcvr=ship sndr-life=life]
  ^-  shot
  :*  [sndr rcvr]
      &
      &
      (mod sndr-life 16)
      `@`1
      origin=~
      content=`@`%keys
  ==
::
++  response-size  13  ::  1kb
::  +sift-roar: assemble scry response fragments into full message
::
++  sift-roar
  |=  [total=@ud hav=(list have)]
  ^-  [sig=@ux dat=$@(~ (cask))]
  =/  mes=@
    %+  rep  response-size
    (roll hav |=([=have dat=(list @ux)] [dat.have dat]))
  =+  sig=(end 9 mes)
  :-  sig
  =+  dat=(rsh 9 mes)
  ?~  dat  ~
  =/  non  ~|(%fine-cue (cue dat))
  ~|  [%fine %response-not-cask]
  ;;((cask) non)
::  +etch-hunk: helper core to serialize a $hunk
::
++  etch-hunk
  |=  [=ship =life =acru:ames]
  |%
  ::
  +|  %helpers
  ::  +show-meow: prepare $meow for printing
  ::
  ++  show-meow
    |=  =meow
    :*  sig=`@q`(mug sig.meow)
        num=num.meow
        dat=`@q`(mug dat.meow)
    ==
  ::
  ++  make-meow
    |=  [=path mes=@ num=@ud]
    ^-  meow
    =/  tot  (met 13 mes)
    =/  dat  (cut 13 [(dec num) 1] mes)
    =/  wid  (met 3 dat)
    :*  sig=(sign-fra path num dat)           ::  fragment signature
        num=tot                               ::  number of fragments
        dat=dat                               ::  response data fragment
    ==
  ::
  ++  etch-meow
    |=  =meow
    ^-  yowl
    %+  can  3
    :~  64^sig.meow
        4^num.meow
        (met 3 dat.meow)^dat.meow
    ==
  ::
  +|  %keys
  ::
  ++  sign  sigh:as:acru
  ++  sign-fra
    |=  [=path fra=@ud dat=@ux]
    ::~>  %bout.[1 %sign-fra]
    (sign (jam path fra dat))
  ::
  ++  full
    |=  [=path data=$@(~ (cask))]
    =/  buf  (jam ship life path data)
    ::=/  nam  (crip "sign-full {<(met 3 buf)>}")
    ::~>  %bout.[1 nam]
    (sign buf)
  ::
  +|  %serialization
  ::
  ++  etch
    |=  [=path =hunk data=$@(~ (cask))]
    ^-  (list yowl)
    =/  mes=@
      =/  sig=@  (full path data)
      ?~  data  sig
      (mix sig (lsh 9 (jam data)))
      ::(cat 9 sig (jam data))
    ::
    =/  las  (met 13 mes)
    =/  tip  (dec (add [lop len]:hunk))
    =/  top  (min las tip)
    =/  num  lop.hunk
    ?>  (lte num top)
    =|  res=(list yowl)
    |-  ^+  res
    ?:  =(num top)
      =-  (flop - res)
      (etch-meow (make-meow path mes num))
    $(num +(num), res :_(res (etch-meow (make-meow path mes num))))
  --
::  +etch-open-packet: convert $open-packet attestation to $shot
::
++  etch-open-packet
  ~/  %etch-open-packet
  |=  [pac=open-packet =acru:ames]
  ^-  shot
  :*  [sndr rcvr]:pac
      req=&  sam=&
      (mod sndr-life.pac 16)
      (mod rcvr-life.pac 16)
      origin=~
      content=`@`(sign:as:acru (jam pac))
  ==
::  +sift-open-packet: decode comet attestation into an $open-packet
::
++  sift-open-packet
  ~/  %sift-open-packet
  |=  [=shot our=ship our-life=@]
  ^-  open-packet
  ::  deserialize and type-check packet contents
  ::
  =+  ;;  [signature=@ signed=@]  (cue content.shot)
  =+  ;;  =open-packet            (cue signed)
  ::  assert .our and .her and lives match
  ::
  ?>  .=       sndr.open-packet  sndr.shot
  ?>  .=       rcvr.open-packet  our
  ?>  .=  sndr-life.open-packet  1
  ?>  .=  rcvr-life.open-packet  our-life
  ::  only a star can sponsor a comet
  ::
  ?>  =(%king (clan:title (^sein:title sndr.shot)))
  =/  crub  (com:nu:crub:crypto public-key.open-packet)
  ::  comet public-key must hash to its @p address
  ::
  ?>  =(sndr.shot fig:ex:crub)
  ::  verify signature
  ::
  ?>  (safe:as:crub signature signed)
  open-packet
::  +etch-shut-packet: encrypt and packetize a $shut-packet
::
++  etch-shut-packet
  ~/  %etch-shut-packet
  :: TODO add rift to signed messages to prevent replay attacks?
  ::
  |=  $:  =shut-packet
          =symmetric-key
          sndr=ship
          rcvr=ship
          sndr-life=@
          rcvr-life=@
      ==
  ^-  shot
  ::
  =?    meat.shut-packet
      ?&  ?=(%& -.meat.shut-packet)
          (gth (met packet-size fragment.p.meat.shut-packet) 1)
      ==
    %_    meat.shut-packet
        fragment.p
      (cut packet-size [[fragment-num 1] fragment]:p.meat.shut-packet)
    ==
  ::
  =/  vec  ~[sndr rcvr sndr-life rcvr-life]
  =/  [siv=@uxH len=@ cyf=@ux]
    (~(en sivc:aes:crypto (shaz symmetric-key) vec) (jam shut-packet))
  ::
  :*  ^=       dyad  [sndr rcvr]
      ^=        req  ?=(%& -.meat.shut-packet)
      ^=        sam  &
      ^=  sndr-tick  (mod sndr-life 16)
      ^=  sndr-tick  (mod rcvr-life 16)
      ^=     origin  ~
      ^=    content  :(mix siv (lsh 7 len) (lsh [3 18] cyf))
  ==
::  +sift-shut-packet: decrypt a $shut-packet from a $shot
::
++  sift-shut-packet
  ~/  %sift-shut-packet
  |=  [=shot =symmetric-key sndr-life=@ rcvr-life=@]
  ^-  (unit shut-packet)
  ?.  ?&  =(sndr-tick.shot (mod sndr-life 16))
          =(rcvr-tick.shot (mod rcvr-life 16))
      ==
    ~
  =/  siv  (end 7 content.shot)
  =/  len  (end 4 (rsh 7 content.shot))
  =/  cyf  (rsh [3 18] content.shot)
  ~|  ames-decrypt+[[sndr rcvr origin]:shot len siv]
  =/  vec  ~[sndr.shot rcvr.shot sndr-life rcvr-life]
  %-  some  ;;  shut-packet  %-  cue  %-  need
  (~(de sivc:aes:crypto (shaz symmetric-key) vec) siv len cyf)
::
++  is-peer-dead
  |=  [now=@da =peer-state]
  ^+  peer-state
  =/  expiry=@da  (add ~s30 last-contact.qos.peer-state)
  =?  -.qos.peer-state  (gte now expiry)
    %dead
  peer-state
::
++  update-peer-route
  |=  [peer=ship =peer-state]
  ^+  peer-state
  ::   If the peer is not responding, mark the .lane.route as
  ::   indirect.  The next packets we emit will be sent to the
  ::   receiver's sponsorship chain in case the receiver's
  ::   transport address has changed and this lane is no longer
  ::   valid.
  ::
  ::   If .peer is a galaxy, the lane will always remain direct.
  ::
  ?.  ?&  ?=(%dead -.qos.peer-state)
          ?=(^ route.peer-state)
          direct.u.route.peer-state
          !=(%czar (clan:title peer))
      ==
    peer-state
  peer-state(direct.u.route %.n)
::
+|  %atomics
::
+$  private-key    @uwprivatekey
+$  signature      @uwsignature
::
+|  %kinetics
::  $channel: combined sender and receiver identifying data
::
+$  channel
  $:  [our=ship her=ship]
      now=@da
      ::  our data, common to all dyads
      ::
      $:  =our=life
          crypto-core=acru:ames
          =bug
      ==
      ::  her data, specific to this dyad
      ::
      $:  =symmetric-key
          =her=life
          =her=rift
          =her=public-key
          her-sponsor=ship
  ==  ==
::  $open-packet: unencrypted packet payload, for comet self-attestation
::
::    This data structure gets signed and jammed to form the .contents
::    field of a $packet.
::
:: TODO add rift to prevent replay attacks
::
+$  open-packet
  $:  =public-key
      sndr=ship
      =sndr=life
      rcvr=ship
      =rcvr=life
  ==
::  $shut-packet: encrypted packet payload
::
+$  shut-packet
  $:  =bone
      =message-num
      meat=(each fragment-meat ack-meat)
  ==
::  $fragment-meat: contents of a message-fragment packet
::
+$  fragment-meat
  $:  num-fragments=fragment-num
      =fragment-num
      =fragment
  ==
::  $ack-meat: contents of an acknowledgment packet; fragment or message
::
::    Fragment acks reference the $fragment-num of the target packet.
::
::    Message acks contain a success flag .ok, which is %.n in case of
::    negative acknowledgment (nack), along with .lag that describes the
::    time it took to process the message. .lag is zero if the message
::    was processed during a single Arvo event. At the moment, .lag is
::    always zero.
::
+$  ack-meat  (each fragment-num [ok=? lag=@dr])
::  $naxplanation: nack trace; explains which message failed and why
::
+$  naxplanation  [=message-num =error]
::
+|  %statics
::
::  $ames-state: state for entire vane
::
::    peers:       states of connections to other ships
::    unix-duct:   handle to give moves to unix
::    life:        our $life; how many times we've rekeyed
::    crypto-core: interface for encryption and signing
::    bug:         debug printing configuration
::    snub:        blocklist for incoming packets
::    cong:        parameters for marking a flow as clogged
::    dead:        dead flow consolidation timer and recork timer, if set
::
+$  ames-state
  $+  ames-state
  $:  peers=(map ship ship-state)
      =unix=duct
      =life
      =rift
      crypto-core=acru:ames
      =bug
      snub=[form=?(%allow %deny) ships=(set ship)]
      cong=[msg=_5 mem=_100.000]
    ::
      $=  dead
      $:  flow=[%flow (unit dead-timer)]
          cork=[%cork (unit dead-timer)]
  ==  ==
::
+$  dead-timer       [=duct =wire date=@da]
+$  azimuth-state    [=symmetric-key =life =rift =public-key sponsor=ship]
+$  azimuth-state-6  [=symmetric-key =life =public-key sponsor=ship]
+$  ames-state-4   ames-state-5
+$  ames-state-5
  $+  ames-state-5
  $:  peers=(map ship ship-state-5)
      =unix=duct
      =life
      crypto-core=acru-12
      bug=bug-9
  ==
::
+$  ship-state-4  ship-state-5
+$  ship-state-5
  $+  ship-state-5
  $%  [%alien alien-agenda-12]
      [%known peer-state-5]
  ==
::
+$  peer-state-5
  $+  peer-state-5
  $:  azimuth-state-6
      route=(unit [direct=? =lane])
      =qos
      =ossuary
      snd=(map bone message-pump-state-16)
      rcv=(map bone message-sink-state)
      nax=(set [=bone =message-num])
      heeds=(set duct)
  ==
::
+$  bug-9
  $+  bug-9
  $:  veb=_[`?`%.n `?`%.n `?`%.n `?`%.n `?`%.n `?`%.n `?`%.n]
      ships=(set ship)
  ==
::
+$  ames-state-6
  $+  ames-state-6
  $:  peers=(map ship ship-state-6)
      =unix=duct
      =life
      crypto-core=acru-12
      bug=bug-9
  ==
::
+$  ship-state-6
  $+  ship-state-6
  $%  [%alien alien-agenda-12]
      [%known peer-state-6]
  ==
::
+$  peer-state-6
  $+  peer-state-6
  $:  azimuth-state
      route=(unit [direct=? =lane])
      =qos
      =ossuary
      snd=(map bone message-pump-state-16)
      rcv=(map bone message-sink-state)
      nax=(set [=bone =message-num])
      heeds=(set duct)
  ==
::
+$  ames-state-7
  $+  ames-state-7
  $:  peers=(map ship ship-state-7)
      =unix=duct
      =life
      crypto-core=acru-12
      bug=bug-9
  ==
::
+$  ames-state-8
  $+  ames-state-8
  $:  peers=(map ship ship-state-7)
      =unix=duct
      =life
      crypto-core=acru-12
      bug=bug-9
      corks=(set wire)
  ==
::
+$  ames-state-9
  $+  ames-state-9
  $:  peers=(map ship ship-state-7)
      =unix=duct
      =life
      crypto-core=acru-12
      bug=bug-9
      corks=(set wire)
      snub=(set ship)
  ==
::
+$  ames-state-10
  $+  ames-state-10
  $:  peers=(map ship ship-state-7)
      =unix=duct
      =life
      crypto-core=acru-12
      bug=bug-12
      corks=(set wire)
      snub=(set ship)
  ==
::
+$  ship-state-7
  $+  ship-state-7
  $%  [%alien alien-agenda-12]
      [%known peer-state-7]
  ==
::
+$  peer-state-7
  $+  peer-state-7
  $:  azimuth-state
      route=(unit [direct=? =lane])
      =qos
      =ossuary
      snd=(map bone message-pump-state-16)
      rcv=(map bone message-sink-state)
      nax=(set [=bone =message-num])
      heeds=(set duct)
      closing=(set bone)
      corked=(set bone)
      krocs=(set bone)
  ==
::
+$  ames-state-11
  $+  ames-state-11
  $:  peers=(map ship ship-state-7)
      =unix=duct
      =life
      crypto-core=acru-12
      bug=bug-12
      corks=(set wire)
      snub=(set ship)
      cong=[msg=@ud mem=@ud]
  ==
::
+$  queued-event-11
  $+  queued-event-11
  $%  [%call =duct wrapped-task=(hobo task-11)]
      [%take =wire =duct =sign]
  ==
::
+$  task-11
  $+  task-11
  $%  [%snub ships=(list ship)]
      $<(%snub task)
  ==
::
+$  ames-state-12
  $+  ames-state-12
  $:  peers=(map ship ship-state-12)
      =unix=duct
      =life
      crypto-core=acru-12
      bug=bug-12
      snub=[form=?(%allow %deny) ships=(set ship)]
      cong=[msg=@ud mem=@ud]
  ==
::
+$  ship-state-12
  $+  ship-state-12
  $%  [%alien alien-agenda-12]
      [%known peer-state-12]
  ==
::
+$  alien-agenda-12
  $+  alien-agenda-12
  $:  messages=(list [=duct =plea])
      packets=(set =blob)
      heeds=(set duct)
  ==
::
+$  peer-state-12
  $+  peer-state-12
  $:  azimuth-state
      route=(unit [direct=? =lane])
      =qos
      =ossuary
      snd=(map bone message-pump-state-16)
      rcv=(map bone message-sink-state)
      nax=(set [=bone =message-num])
      heeds=(set duct)
      closing=(set bone)
      corked=(set bone)
  ==
::
+$  bug-12
  $:  veb=_[`?`%.n `?`%.n `?`%.n `?`%.n `?`%.n `?`%.n `?`%.n `?`%.n]
      ships=(set ship)
  ==
::
++  acru-12  $_  ^?
  |%
  ++  as  ^?
    |%  ++  seal  |~([a=pass b=@] *@)
        ++  sign  |~(a=@ *@)
        ++  sure  |~(a=@ *(unit @))
        ++  tear  |~([a=pass b=@] *(unit @))
    --
  ++  de  |~([a=@ b=@] *(unit @))
  ++  dy  |~([a=@ b=@] *@)
  ++  en  |~([a=@ b=@] *@)
  ++  ex  ^?
    |%  ++  fig  *@uvH
        ++  pac  *@uvG
        ++  pub  *pass
        ++  sec  *ring
    --
  ++  nu  ^?
    |%  ++  pit  |~([a=@ b=@] ^?(..nu))
        ++  nol  |~(a=ring ^?(..nu))
        ++  com  |~(a=pass ^?(..nu))
    --
  --
::
+$  ames-state-13
  $+  ames-state-13
  $:  peers=(map ship ship-state-13)
      =unix=duct
      =life
      =rift
      crypto-core=acru:ames
      =bug
      snub=[form=?(%allow %deny) ships=(set ship)]
      cong=[msg=@ud mem=@ud]
  ==
::
+$  ship-state-13
  $+  ship-state-13
  $%  [%alien alien-agenda]
      [%known peer-state-13]
  ==
::
+$  peer-state-13
  $+  peer-state-13
  $:  $:  =symmetric-key
          =life
          =rift
          =public-key
          sponsor=ship
      ==
      route=(unit [direct=? =lane])
      =qos
      =ossuary
      snd=(map bone message-pump-state-16)
      rcv=(map bone message-sink-state)
      nax=(set [=bone =message-num])
      heeds=(set duct)
      closing=(set bone)
      corked=(set bone)
      keens=(map path keen-state-13)
  ==
::
++  keen-state-13
  =<  $+  keen-state-13
      $:  wan=(pha want)   ::  request packts, sent
          nex=(list want)  ::  request packets, unsent
          hav=(list have)  ::  response packets, backward
          num-fragments=@ud
          num-received=@ud
          next-wake=(unit @da)
          listeners=(set duct)
          metrics=pump-metrics-16
      ==
  |%
  ::  +afx: polymorphic node type for finger trees
  ::
  ++  afx
    |$  [val]
    $%  [%1 p=val ~]
        [%2 p=val q=val ~]
        [%3 p=val q=val r=val ~]
        [%4 p=val q=val r=val s=val ~]
    ==
  ::  +pha: finger tree
  ::
  ::    DO NOT USE THIS
  ::    It's wrong and only kept around for state migration purposes.
  ::
  ++  pha
    |$  [val]
    $~  [%nul ~]
    $%  [%nul ~]
        [%one p=val]
        [%big p=(afx val) q=(pha val) r=(afx val)]
    ==
  ::  +deq: deque
  ::
  ::    DO NOT USE THIS
  ::    It's wrong and only kept around for state migration purposes.
  ::
  ++  deq
    |*  val=mold
    |%
::    ::
::    ::  +|  %utilities
::    ::
::    ++  make-afx
::      |=  ls=(list val)
::      ?+  ls  ~|(bad-finger/(lent ls) !!)
::        [* ~]        [%1 ls]
::        [* * ~]      [%2 ls]
::        [* * * ~]    [%3 ls]
::        [* * * * ~]  [%4 ls]
::      ==
::    ++  afx-to-pha
::      |=  =(afx val)
::      ^-  (pha val)
::      (apl *(pha val) +.afx)
::    ::
::    ::  +|  %left-biased-operations
::    ::
::    ::  +pop-left: remove leftmost value from tree
::    ::
::    ++  pop-left
::      |=  a=(pha val)
::      ^-  [val=(unit val) pha=(pha val)]
::      ?-  -.a
::        %nul  ~^a
::      ::
::        %one  [`p.a nul/~]
::      ::
::          %big
::        [`p.p.a (big-left +.+.p.a q.a r.a)]
::     ==
::    ++  apl
::      |=  [a=(pha val) vals=(list val)]
::      ^-  (pha val)
::      =.  vals  (flop vals)
::      |-
::      ?~  vals  a
::      $(a (cons a i.vals), vals t.vals)
::    ::
::    ::
::    ++  dip-left
::      |*  state=mold
::      |=  $:  a=(pha val)
::              =state
::              f=$-([state val] [(unit val) ? state])
::          ==
::      ^+  [state a]
::      =/  acc  [stop=`?`%.n state=state]
::      =|  new=(pha val)
::      |-
::      ?:  stop.acc
::        :: cat new and old
::        [state.acc (weld a new)]
::      =^  val=(unit val)  a
::        (pop-left a)
::      ?~  val
::        [state.acc new]
::      =^  res=(unit ^val)  acc
::        (f state.acc u.val)
::      ?~  res  $
::      $(new (snoc new u.res))
::    ::
::    ++  big-left
::      |=  [ls=(list val) a=(pha val) sf=(afx val)]
::      ^-  (pha val)
::      ?.  =(~ ls)
::        [%big (make-afx ls) a sf]
::      =/  [val=(unit val) inner=_a]
::        (pop-left a)
::      ?~  val
::        (afx-to-pha sf)
::      [%big [%1 u.val ~] inner sf]
::    ::
::    ++  cons
::      =|  b=(list val)
::      |=  [a=(pha val) c=val]
::      ^-  (pha val)
::      =.  b  [c b]
::      |-
::      ?~  b  a
::      ?-  -.a
::      ::
::          %nul
::        $(a [%one i.b], b t.b)
::      ::
::          %one
::        %=  $
::           b  t.b
::           a  [%big [%1 i.b ~] [%nul ~] [%1 p.a ~]]
::        ==
::      ::
::          %big
::        ?.  ?=(%4 -.p.a)
::          %=    $
::              b  t.b
::          ::
::              a
::            ?-  -.p.a
::              %1  big/[[%2 i.b p.p.a ~] q.a r.a]
::              %2  big/[[%3 i.b p.p.a q.p.a ~] q.a r.a]
::              %3  big/[[%4 i.b p.p.a q.p.a r.p.a ~] q.a r.a]
::             ==
::          ==
::        =/  inner
::          $(a q.a, b ~[s.p.a r.p.a q.p.a])
::        =.  inner
::          $(a inner, b t.b)
::        big/[[%2 i.b p.p.a ~] inner r.a]
::      ==
::    ::
::    ::  +|  %right-biased-operations
::    ::
::    ::  +snoc: append to end (right) of tree
::    ::
::    ++  snoc
::      |=  [a=(pha val) b=val]
::      ^+  a
::      ?-  -.a
::        %nul  [%one b]
::      ::
::          %one
::        :-  %big
::        :*  [%1 p.a ~]
::            [%nul ~]
::            [%1 b ~]
::        ==
::      ::
::          %big
::        ?-  -.r.a
::        ::
::            %1
::          :-  %big
::          [p.a q.a [%2 p.r.a b ~]]
::        ::
::            %2
::          :-  %big
::          [p.a q.a [%3 p.r.a q.r.a b ~]]
::        ::
::            %3
::          :-  %big
::          [p.a q.a [%4 p.r.a q.r.a r.r.a b ~]]
::        ::
::            %4
::          =/  inner
::            $(a q.a, b p.r.a)
::          =.  inner
::            $(a inner, b q.r.a)
::          =.  inner
::            $(a inner, b r.r.a)
::          :-  %big
::          :*  p.a
::              inner
::              [%2 s.r.a b ~]
::          ==
::        ==
::      ==
::    ::  +apr: append list to end (right) of tree
::    ::
::    ++  apr
::      |=  [a=(pha val) vals=(list val)]
::      ^-  (pha val)
::      ?~  vals  a
::      $(a (snoc a i.vals), vals t.vals)
::    ::  +|  %manipulation
::    ::
::    ::  +weld: concatenate two trees
::    ::
::    ::    O(log n)
::    ++  weld
::      =|  c=(list val)
::      |=  [a=(pha val) b=(pha val)]
::      ^-  (pha val)
::      ?-  -.b
::        %nul  (apr a c)
::        %one  (snoc (apr a c) p.b)
::      ::
::          %big
::        ?-  -.a
::          %nul  (apl b c)
::          %one  (cons (apl b c) p.a)
::        ::
::            %big
::          :-  %big
::          =-  [p.a - r.b]
::          $(a q.a, b q.b, c :(welp +.r.a c +.p.b))
::        ==
::      ==
    ::  +tap: transform tree to list
    ::
    ++  tap
      =|  res=(list val)
      |=  a=(pha val)
      !.
      |^  ^+  res
      ?-  -.a
        %nul  ~
        %one  ~[p.a]
      ::
          %big
        =/  fst=_res
          (tap-afx p.a)
        =/  lst=_res
          (tap-afx r.a)
        =/  mid=_res
          $(a q.a)
        :(welp fst mid lst)
      ==
      ++  tap-afx
        |=  ax=(afx val)
        ^+  res
        ?-  -.ax
          %1  +.ax
          %2  +.ax
          %3  +.ax
          %4  +.ax
        ==
      --
    --
  --
::
+$  ames-state-14  ames-state-16
+$  ames-state-15  ames-state-16
+$  ames-state-16
  $+  ames-state-16
  $:  peers=(map ship ship-state-16)
      =unix=duct
      =life
      =rift
      crypto-core=acru:ames
      =bug
      snub=[form=?(%allow %deny) ships=(set ship)]
      cong=[msg=@ud mem=@ud]
  ==
+$  ship-state-16
  $+  ship-state-16
  $%  [%alien alien-agenda]
      [%known peer-state-16]
  ==
::
+$  peer-state-16
  $+  peer-state-16
  $:  azimuth-state
      route=(unit [direct=? =lane])
      =qos
      =ossuary
      snd=(map bone message-pump-state-16)
      rcv=(map bone message-sink-state)
      nax=(set [=bone =message-num])
      heeds=(set duct)
      closing=(set bone)
      corked=(set bone)
      keens=(map path keen-state-16)
  ==
::
+$  keen-state-14  keen-state-16
+$  keen-state-16
  $+  keen-state-16
  $:  wan=((mop @ud want) lte)
      nex=(list want)
      hav=(list have)
      num-fragments=@ud
      num-received=@ud
      next-wake=(unit @da)
      listeners=(set duct)
      metrics=pump-metrics-16
  ==
::
+$  message-pump-state-16
  $+  message-pump-state-16
  $:  current=_`message-num`1
      next=_`message-num`1
      unsent-messages=(qeu message-blob)
      unsent-fragments=(list static-fragment)
      queued-message-acks=(map message-num ack)
      packet-pump-state=packet-pump-state-16
  ==
::
+$  packet-pump-state-16
  $+  packet-pump-state-16
  $:  next-wake=(unit @da)
      live=((mop live-packet-key live-packet-val) lte-packets)
      metrics=pump-metrics-16
  ==
::
+$  pump-metrics-16
  $+  pump-metrics-16
  $:  rto=_~s1
      rtt=_~s1
      rttvar=_~s1
      ssthresh=_10.000
      cwnd=_1
      num-live=@ud
      counter=@ud
  ==
::
+$  queued-event-11-and-16
  $+  queued-event-11-and-16
  $%  [%call =duct wrapped-task=(hobo task-11-and-16)]
      [%take =wire =duct =sign]
  ==
::
+$  task-11-and-16
  $+  task-11-and-16
  $%  [%kroc dry=?]
      [%snub ships=(list ship)]
      $<(?(%snub %kroc) task)
  ==
::
+$  queued-event-16
  $+  queued-event-16
  $%  [%call =duct wrapped-task=(hobo task-16)]
      [%take =wire =duct =sign]
  ==
::
+$  task-16
  $+  task-16
  $%  [%kroc dry=?]
      $<(%kroc task)
  ==
::  $bug: debug printing configuration
::
::    veb: verbosity toggles
::    ships: identity filter; if ~, print for all
::
+$  bug
  $:  veb=_veb-all-off
      ships=(set ship)
  ==
::
+|  %dialectics
::
::  $move: output effect; either request or response
::
+$  move  [=duct card=(wind note gift)]
::  $queued-event: event to be handled after initial boot completes
::
+$  queued-event
  $+  queued-event
  $%  [%call =duct wrapped-task=(hobo task)]
      [%take =wire =duct =sign]
  ==
::  $note: request to other vane
::
::    Ames passes a %plea note to another vane when it receives a
::    message on a "forward flow" from a peer, originally passed from
::    one of the peer's vanes to the peer's Ames.
::
::    Ames passes a %deep task to itself to handle deferred calls
::    Ames passes a %private-keys to Jael to request our private keys.
::    Ames passes a %public-keys to Jael to request a peer's public
::    keys.
::
+$  note
  $~  [%b %wait *@da]
  $%  $:  %a
          $>(%deep task:ames)
      ==
      $:  %b
          $>(?(%wait %rest) task:behn)
      ==
      $:  %c
          $>(%warp task:clay)
      ==
      $:  %d
          $>(%flog task:dill)
      ==
      $:  %g
          $>(%deal task:gall)
      ==
      $:  %j
          $>  $?  %private-keys
                  %public-keys
                  %turf
                  %ruin
              ==
          task:jael
      ==
      $:  @tas
          $>(%plea vane-task)
  ==  ==
::  $sign: response from other vane
::
+$  sign
  $~  [%behn %wake ~]
  $%  $:  %behn
          $>(%wake gift:behn)
      ==
      $:  %gall
          $>(?(%flub %unto) gift:gall)
      ==
      $:  %jael
          $>  $?  %private-keys
                  %public-keys
                  %turf
              ==
          gift:jael
      ==
      $:  @tas
          $>(?(%boon %done) gift:ames)
  ==  ==
::
::  $message-pump-task: job for |message-pump
::
::    %memo: packetize and send application-level message
::    %hear: handle receipt of ack on fragment or message
::    %near: handle receipt of naxplanation
::    $prod: reset congestion control
::    %wake: handle timer firing
::
+$  message-pump-task
  $%  [%memo =message-blob]
      [%hear =message-num =ack-meat]
      [%near =naxplanation]
      [%prod ~]
      [%wake ~]
  ==
::  $packet-pump-task: job for |packet-pump
::
::    %hear: deal with a packet acknowledgment
::    %done: deal with message acknowledgment
::    %halt: finish event, possibly updating timer
::    %wake: handle timer firing
::    %prod: reset congestion control
::
+$  packet-pump-task
  $%  [%hear =message-num =fragment-num]
      [%done =message-num lag=@dr]
      [%halt ~]
      [%wake current=message-num]
      [%prod ~]
  ==
::  $message-sink-task: job for |message-sink
::
::    %done: receive confirmation from vane of processing or failure
::    %drop: clear .message-num from .nax.state
::    %hear: handle receiving a message fragment packet
::      .ok: %.y unless previous failed attempt
::
+$  message-sink-task
  $%  [%done ok=?]
      [%flub ~]
      [%drop =message-num]
      [%hear =lane =shut-packet ok=?]
  ==
--
::  external vane interface
::
|=  our=ship
::  larval ames, before %born sets .unix-duct; wraps adult ames core
::
=<  =*  adult-gate  .
    =|  queued-events=(qeu queued-event)
    =|  $=  cached-state
        %-  unit
        $%  [%5 ames-state-5]
            [%6 ames-state-6]
            [%7 ames-state-7]
            [%8 ames-state-8]
            [%9 ames-state-9]
            [%10 ames-state-10]
            [%11 ames-state-11]
            [%12 ames-state-12]
            [%13 ames-state-13]
            [%14 ames-state-14]
            [%15 ames-state-15]
            [%16 ames-state-16]
            [%17 ^ames-state]
        ==
    ::
    |=  [now=@da eny=@ rof=roof]
    =*  larval-gate  .
    =*  adult-core   (adult-gate +<)
    =<  |%
        ++  call  ^call
        ++  load  ^load
        ++  scry  ^scry
        ++  stay  ^stay
        ++  take  ^take
        --
    |%
    ++  larval-core  .
    ::  +call: handle request $task
    ::
    ++  call
      |=  [=duct dud=(unit goof) wrapped-task=(hobo task)]
      ::
      =/  =task  ((harden task) wrapped-task)
      ::  reject larval error notifications
      ::
      ?^  dud
        ~|(%ames-larval-call-dud (mean tang.u.dud))
      ::  before processing events, make sure we have state loaded
      ::
      =^  molt-moves  larval-core  molt
      ::
      ?:  &(!=(~ unix-duct.ames-state.adult-gate) =(~ queued-events))
        =^  moves  adult-gate  (call:adult-core duct dud task)
        ~>  %slog.0^leaf/"ames: metamorphosis"
        [(weld molt-moves moves) adult-gate]
      ::  drop incoming packets until we metamorphose
      ::
      ?:  ?=(%hear -.task)
        [~ larval-gate]
      ::  %born: set .unix-duct and start draining .queued-events
      ::
      ?:  ?=(%born -.task)
        ::  process %born using wrapped adult ames
        ::
        =^  moves  adult-gate  (call:adult-core duct dud task)
        =.  moves  (weld molt-moves moves)
        ::  kick off a timer to process the first of .queued-events
        ::
        =.  moves  :_(moves [duct %pass /larva %b %wait now])
        [moves larval-gate]
      ::  any other event: enqueue it until we have a .unix-duct
      ::
      ::    XX what to do with errors?
      ::
      =.  queued-events  (~(put to queued-events) %call duct task)
      [~ larval-gate]
    ::  +take: handle response $sign
    ::
    ++  take
      |=  [=wire =duct dud=(unit goof) =sign]
      ?^  dud
        ~|(%ames-larval-take-dud (mean tang.u.dud))
      ::
      =^  molt-moves  larval-core  molt
      ::
      ?:  &(!=(~ unix-duct.ames-state.adult-gate) =(~ queued-events))
        =^  moves  adult-gate  (take:adult-core wire duct dud sign)
        ~>  %slog.0^leaf/"ames: metamorphosis"
        [(weld molt-moves moves) adult-gate]
      ::  enqueue event if not a larval drainage timer
      ::
      ?.  =(/larva wire)
        =.  queued-events  (~(put to queued-events) %take wire duct sign)
        [~ larval-gate]
      ::  larval event drainage timer; pop and process a queued event
      ::
      ?.  ?=([%behn %wake *] sign)
        ~>  %slog.0^leaf/"ames: larva: strange sign"
        [~ larval-gate]
      ::  if crashed, print, dequeue, and set next drainage timer
      ::
      ?^  error.sign
        ::  .queued-events should never be ~ here, but if it is, don't crash
        ::
        ?:  =(~ queued-events)
          =/  =tang  [leaf/"ames: cursed metamorphosis" u.error.sign]
          =/  moves  [duct %pass /larva-crash %d %flog %crud %larva tang]~
          [moves adult-gate]
        ::  dequeue and discard crashed event
        ::
        =.  queued-events  +:~(get to queued-events)
        ::  .queued-events has been cleared; metamorphose
        ::
        ?~  queued-events
          ~>  %slog.0^leaf/"ames: metamorphosis"
          [~ adult-gate]
        ::  set timer to drain next event
        ::
        =/  moves
          =/  =tang  [leaf/"ames: larva: drain crash" u.error.sign]
          :~  [duct %pass /larva-crash %d %flog %crud %larva tang]
              [duct %pass /larva %b %wait now]
          ==
        [moves larval-gate]
      ::  normal drain timer; dequeue and run event
      ::
      =^  first-event  queued-events  ~(get to queued-events)
      =^  moves  adult-gate
        ?-  -.first-event
          %call  (call:adult-core [duct ~ wrapped-task]:+.first-event)
          %take  (take:adult-core [wire duct ~ sign]:+.first-event)
        ==
      =.  moves  (weld molt-moves moves)
      ::  .queued-events has been cleared; done!
      ::
      ?~  queued-events
        ~>  %slog.0^leaf/"ames: metamorphosis"
        [moves adult-gate]
      ::  set timer to drain next event
      ::
      =.  moves  :_(moves [duct %pass /larva %b %wait now])
      [moves larval-gate]
    ::  lifecycle arms; mostly pass-throughs to the contained adult ames
    ::
    ++  scry  scry:adult-core
    ++  stay  [%17 %larva queued-events ames-state.adult-gate]
    ++  load
      |=  $=  old
          $%  $:  %4
              $%  $:  %larva
                      events=(qeu queued-event)
                      state=ames-state-4
                  ==
                  [%adult state=ames-state-4]
              ==  ==
              $:  %5
              $%  $:  %larva
                      events=(qeu queued-event)
                      state=ames-state-5
                  ==
                  [%adult state=ames-state-5]
              ==  ==
              $:  %6
              $%  $:  %larva
                      events=(qeu queued-event)
                      state=ames-state-6
                  ==
                  [%adult state=ames-state-6]
              ==  ==
              $:  %7
              $%  $:  %larva
                      events=(qeu queued-event)
                      state=ames-state-7
                  ==
                  [%adult state=ames-state-7]
              ==  ==
              $:  %8
              $%  $:  %larva
                      events=(qeu queued-event)
                      state=ames-state-8
                  ==
                  [%adult state=ames-state-8]
              ==  ==
              $:  %9
              $%  $:  %larva
                      events=(qeu queued-event-11)
                      state=ames-state-9
                  ==
                  [%adult state=ames-state-9]
              ==  ==
              $:  %10
              $%  $:  %larva
                      events=(qeu queued-event-11-and-16)
                      state=ames-state-10
                  ==
                  [%adult state=ames-state-10]
              ==  ==
              $:  %11
              $%  $:  %larva
                      events=(qeu queued-event-11-and-16)
                      state=ames-state-11
                  ==
                  [%adult state=ames-state-11]
              ==  ==
              $:  %12
              $%  $:  %larva
                      events=(qeu queued-event-16)
                      state=ames-state-12
                  ==
                  [%adult state=ames-state-12]
              ==  ==
              $:  %13
              $%  $:  %larva
                      events=(qeu queued-event-16)
                      state=ames-state-13
                  ==
                  [%adult state=ames-state-13]
              ==  ==
              $:  %14
              $%  $:  %larva
                      events=(qeu queued-event-16)
                      state=ames-state-14
                  ==
                  [%adult state=ames-state-14]
              ==  ==
              $:  %15
              $%  $:  %larva
                      events=(qeu queued-event-16)
                      state=ames-state-15
                  ==
                  [%adult state=ames-state-15]
              ==  ==
              $:  %16
              $%  $:  %larva
                      events=(qeu queued-event-16)
                      state=ames-state-16
                  ==
                  [%adult state=ames-state-16]
              ==  ==
              $:  %17
              $%  $:  %larva
                      events=(qeu queued-event)
                      state=_ames-state.adult-gate
                  ==
                  [%adult state=_ames-state.adult-gate]
          ==  ==  ==
      |^  ?-  old
          [%4 %adult *]
        $(old [%5 %adult (state-4-to-5:load:adult-core state.old)])
      ::
          [%4 %larva *]
        =.  state.old  (state-4-to-5:load:adult-core state.old)
        $(-.old %5)
      ::
          [%5 %adult *]
        =.  cached-state  `[%5 state.old]
        ~>  %slog.0^leaf/"ames: larva reload"
        larval-gate
      ::
          [%5 %larva *]
        ~>  %slog.0^leaf/"ames: larva: load"
        =.  cached-state  `[%5 state.old]
        =.  queued-events  events.old
        larval-gate
      ::
          [%6 %adult *]
        =.  cached-state  `[%6 state.old]
        ~>  %slog.0^leaf/"ames: larva reload"
        larval-gate
      ::
          [%6 %larva *]
        ~>  %slog.0^leaf/"ames: larva: load"
        =.  cached-state  `[%6 state.old]
        =.  queued-events  events.old
        larval-gate
      ::
          [%7 %adult *]
        =.  cached-state  `[%7 state.old]
        ~>  %slog.0^leaf/"ames: larva reload"
        larval-gate
      ::
          [%7 %larva *]
        ~>  %slog.0^leaf/"ames: larva: load"
        =.  queued-events  events.old
        =.  cached-state  `[%7 state.old]
        larval-gate
      ::
          [%8 %adult *]
        =.  cached-state  `[%8 state.old]
        ~>  %slog.0^leaf/"ames: larva reload"
        larval-gate
      ::
          [%8 %larva *]
        ~>  %slog.0^leaf/"ames: larva: load"
        =.  cached-state  `[%8 state.old]
        =.  queued-events  events.old
        larval-gate
      ::
          [%9 %adult *]
        =.  cached-state  `[%9 state.old]
        ~>  %slog.0^leaf/"ames: larva reload"
        larval-gate
      ::
          [%9 %larva *]
        ~>  %slog.0^leaf/"ames: larva: load"
        =.  cached-state  `[%9 state.old]
        =.  queued-events  (event-11-to-12 events.old)
        larval-gate
      ::
          [%10 %adult *]
        =.  cached-state  `[%10 state.old]
        ~>  %slog.0^leaf/"ames: larva reload"
        larval-gate
      ::
          [%10 %larva *]
        ~>  %slog.1^leaf/"ames: larva: load"
        =.  cached-state  `[%10 state.old]
        =.  queued-events  (event-11-to-17 events.old)
        larval-gate
      ::
          [%11 %adult *]
        =.  cached-state  `[%11 state.old]
        ~>  %slog.0^leaf/"ames: larva reload"
        larval-gate
      ::
          [%11 %larva *]
        ~>  %slog.1^leaf/"ames: larva: load"
        =.  cached-state  `[%11 state.old]
        =.  queued-events  (event-11-to-17 events.old)
        larval-gate
      ::
          [%12 %adult *]
        =.  cached-state  `[%12 state.old]
        ~>  %slog.0^leaf/"ames: larva reload"
        larval-gate
      ::
          [%12 %larva *]
        ~>  %slog.1^leaf/"ames: larva: load"
        =.  cached-state  `[%12 state.old]
        =.  queued-events  (event-16-to-17 events.old)
        larval-gate
      ::
          [%13 %adult *]
        =.  cached-state  `[%13 state.old]
        ~>  %slog.0^leaf/"ames: larva reload"
        larval-gate
      ::
          [%13 %larva *]
        ~>  %slog.1^leaf/"ames: larva: load"
        =.  cached-state  `[%13 state.old]
        =.  queued-events  (event-16-to-17 events.old)
        larval-gate
      ::
          [%14 %adult *]
        =.  cached-state  `[%14 state.old]
        ~>  %slog.0^leaf/"ames: larva reload"
        larval-gate
      ::
          [%14 %larva *]
        ~>  %slog.1^leaf/"ames: larva: load"
        =.  cached-state  `[%14 state.old]
        =.  queued-events  (event-16-to-17 events.old)
        larval-gate
      ::
          [%15 %adult *]
        =.  cached-state  `[%15 state.old]
        ~>  %slog.0^leaf/"ames: larva reload"
        larval-gate
      ::
          [%15 %larva *]
        ~>  %slog.1^leaf/"ames: larva: load"
        =.  cached-state  `[%15 state.old]
        =.  queued-events  (event-16-to-17 events.old)
        larval-gate
      ::
          [%16 %adult *]
        =.  cached-state  `[%16 state.old]
        ~>  %slog.0^leaf/"ames: larva reload"
        larval-gate
      ::
          [%16 %larva *]
        ~>  %slog.1^leaf/"ames: larva: load"
        =.  cached-state  `[%16 state.old]
        =.  queued-events  (event-16-to-17 events.old)
        larval-gate
      ::
          [%17 %adult *]  (load:adult-core %17 state.old)
      ::
          [%17 %larva *]
        ~>  %slog.1^leaf/"ames: larva: load"
        =.  queued-events  events.old
        =.  adult-gate     (load:adult-core %17 state.old)
        larval-gate
      ==
      ::
      ++  event-11-to-12
        |=  events=(qeu queued-event-11)
        ^-  (qeu queued-event)
        ::  "+rep:in on a +qeu looks strange, but works fine."
        ::
        %-  ~(rep in events)
        |=  [e=queued-event-11 q=(qeu queued-event)]
        %-  ~(put to q)  ^-  queued-event
        ?.  ?=(%call -.e)  e
        =/  task=task-11  ((harden task-11) wrapped-task.e)
        %=  e
          wrapped-task  ?.(?=(%snub -.task) task [%snub %deny ships.task])
        ==
      ::
      ++  event-11-to-17
        |=  events=(qeu queued-event-11-and-16)
        ^-  (qeu queued-event)
        %-  ~(rep in events)
        |=  [e=queued-event-11-and-16 q=(qeu queued-event)]
        %-  ~(put to q)  ^-  queued-event
        ?.  ?=(%call -.e)  e
        =/  task=task-11-and-16  ((harden task-11-and-16) wrapped-task.e)
        %=    e
            wrapped-task
          ?+  -.task  task
            %snub  [%snub %deny ships.task]
            %kroc  [%kroc ~]
          ==
        ==
      ::
      ++  event-16-to-17
        |=  events=(qeu queued-event-16)
        ^-  (qeu queued-event)
        %-  ~(rep in events)
        |=  [e=queued-event-16 q=(qeu queued-event)]
        %-  ~(put to q)  ^-  queued-event
        ?.  ?=(%call -.e)  e
        =/  task=task-16  ((harden task-16) wrapped-task.e)
        %=  e
          wrapped-task  ?.(?=(%kroc -.task) task [%kroc ~])
        ==
      --
    ::  +molt: re-evolve to adult-ames
    ::
    ++  molt
      ^-  (quip move _larval-core)
      ?~  cached-state  [~ larval-core]
      ~>  %slog.0^leaf/"ames: molt"
      =?  u.cached-state  ?=(%5 -.u.cached-state)
        6+(state-5-to-6:load:adult-core +.u.cached-state)
      =?  u.cached-state  ?=(%6 -.u.cached-state)
        7+(state-6-to-7:load:adult-core +.u.cached-state)
      =^  moz  u.cached-state
        ?.  ?=(%7 -.u.cached-state)  [~ u.cached-state]
        ~>  %slog.0^leaf/"ames: init daily recork timer"
        :-  [[/ames]~ %pass /recork %b %wait `@da`(add now ~d1)]~
        8+(state-7-to-8:load:adult-core +.u.cached-state)
      =?  u.cached-state  ?=(%8 -.u.cached-state)
        9+(state-8-to-9:load:adult-core +.u.cached-state)
      =?  u.cached-state  ?=(%9 -.u.cached-state)
        10+(state-9-to-10:load:adult-core +.u.cached-state)
      =?  u.cached-state  ?=(%10 -.u.cached-state)
        11+(state-10-to-11:load:adult-core +.u.cached-state)
      =?  u.cached-state  ?=(%11 -.u.cached-state)
        12+(state-11-to-12:load:adult-core +.u.cached-state)
      =?  u.cached-state  ?=(%12 -.u.cached-state)
        13+(state-12-to-13:load:adult-core +.u.cached-state)
      =?  u.cached-state  ?=(%13 -.u.cached-state)
        14+(state-13-to-14:load:adult-core +.u.cached-state)
      =?  u.cached-state  ?=(%14 -.u.cached-state)
        15+(state-14-to-15:load:adult-core +.u.cached-state)
      =?  u.cached-state  ?=(%15 -.u.cached-state)
        16+(state-15-to-16:load:adult-core +.u.cached-state)
      =^  moz  u.cached-state
        ?.  ?=(%16 -.u.cached-state)  [~ u.cached-state]
        :_  17+(state-16-to-17:load:adult-core +.u.cached-state)
        ?^  moz  moz  ::  if we have just added the timer in state-7-to-8, skip
        =;  recork-timer=(list [@da duct])
          ?^  recork-timer  ~
          ~>  %slog.0^leaf/"ames: init daily recork timer"
          [[/ames]~ %pass /recork %b %wait `@da`(add now ~d1)]~
        %+  skim
          ;;  (list [@da duct])
          =<  q.q  %-  need  %-  need
          (rof ~ /ames %bx [[our %$ da+now] /debug/timers])
        |=([@da =duct] ?=([[%ames %recork *] *] duct))
      ::
      ?>  ?=(%17 -.u.cached-state)
      =.  ames-state.adult-gate  +.u.cached-state
      [moz larval-core(cached-state ~)]
    --
::
=>  ::  |ev: inner event-handling core
    ::
    ~%  %per-event  ..trace  ~
    |%
    ++  ev
      =|  moves=(list move)
      ~%  %event-gate  ..ev  ~
      |=  [[now=@da eny=@ rof=roof] =duct =ames-state]
      =*  veb  veb.bug.ames-state
      =|  cork-bone=(unit bone)  ::  modified by +ev-kroc
      ~%  %ev-core  ..$  ~
      |%
      +|  %helpers
      ::
      ++  ev-core  .
      ++  ev-abet  [(flop moves) ames-state]
      ++  ev-emit  |=(=move ev-core(moves [move moves]))
      ++  ev-emil  |=(mos=(list move) ev-core(moves (weld (flop mos) moves)))
      ++  trace-fine  (cury trace %fine)
      ++  trace-ames  (cury trace %ames)
      ++  ev-channel-state  [life crypto-core bug]:ames-state
      ++  ev-trace
        |=  [verb=? =ship print=(trap tape)]
        ^+  same
        (trace-ames verb ship ships.bug.ames-state print)
      ::  +ev-get-peer-state: lookup .her state or ~
      ::
      ++  ev-get-peer-state
        |=  her=ship
        ^-  (unit peer-state)
        ::
        =-  ?.(?=([~ %known *] -) ~ `+.u)
        (~(get by peers.ames-state) her)
      ::  +ev-got-peer-state: lookup .her state or crash
      ::
      ++  ev-got-peer-state
        |=  her=ship
        ^-  peer-state
        ::
        ~|  %freaky-alien^her
        =-  ?>(?=(%known -<) ->)
        (~(got by peers.ames-state) her)
      ::  +ev-gut-peer-state: lookup .her state or default
      ::
      ++  ev-gut-peer-state
        |=  her=ship
        ^-  peer-state
        =/  ship-state  (~(get by peers.ames-state) her)
        ?.  ?=([~ %known *] ship-state)
          *peer-state
        +.u.ship-state
      ::
      +|  %tasks
      ::  +ev-take-flub: vane not ready to process message, pretend it
      ::                 was never delivered
      ::
      ++  ev-take-flub
        |=  =wire
        ^+  ev-core
        ?~  parsed=(parse-bone-wire wire)
          ::  no-op
          ::
          ~>  %slog.0^leaf/"ames: dropping malformed wire: {(spud wire)}"
          ev-core
        ?>  ?=([@ her=ship *] u.parsed)
        =*  her  her.u.parsed
        =/  pe-core  (got:pe-abed:pe her)
        ?:  ?&  ?=([%new *] u.parsed)
                (lth rift.u.parsed rift.peer-state.pe-core)
            ==
          ::  ignore events from an old rift
          ::
          %-  %^  ev-trace  odd.veb  her
              |.("dropping old rift wire: {(spud wire)}")
          ev-core
        =/  =bone
          ?-(u.parsed [%new *] bone.u.parsed, [%old *] bone.u.parsed)
        pe-abet:(pe-flub:pe-core bone)
      ::  +ev-take-done: handle notice from vane that it processed a message
      ::
      ++  ev-take-done
        |=  [=wire error=(unit error)]
        ^+  ev-core
        ?~  parsed=(parse-bone-wire wire)
          ::  no-op
          ::
          ~>  %slog.0^leaf/"ames: dropping malformed wire: {(spud wire)}"
          ev-core
        ?>  ?=([@ her=ship *] u.parsed)
        =*  her      her.u.parsed
        =/  pe-core  (got:pe-abed:pe her)
        |^
        ?:  ?&  ?=([%new *] u.parsed)
                (lth rift.u.parsed rift.peer-state.pe-core)
            ==
          ::  ignore events from an old rift
          ::
          %-  %^  ev-trace  odd.veb  her
              |.("dropping old rift wire: {(spud wire)}")
          ev-core
        =/  =bone
          ?-(u.parsed [%new *] bone.u.parsed, [%old *] bone.u.parsed)
        =?  pe-core  ?=([%old *] u.parsed)
          %-  %^  ev-trace  odd.veb  her
              |.("parsing old wire: {(spud wire)}")
          pe-core
        ::  relay the vane ack to the foreign peer
        ::
        =<  pe-abet
        ::  +pe-handle-cork only deals with bones that are in closing
        ::
        ?~(error (pe-handle-cork:(send-ack bone) bone) (send-nack bone u.error))
        ::
        ::  if processing succeded, send positive ack packet and exit
        ::
        ++  send-ack
          |=  =bone
          ^+  pe-core
          mi-abet:(mi-call:(mi-abed:mi:pe-core bone) %done ok=%.y)
        ::  failed; send message nack packet
        ::
        ++  send-nack
          |=  [=bone =^error]
          ^+  pe-core
          =.  pe-core  mi-abet:(mi-call:(mi-abed:mi:pe-core bone) %done ok=%.n)
          ::  construct nack-trace message, referencing .failed $message-num
          ::
          =/  failed=message-num
            last-acked:(~(got by rcv.peer-state.pe-core) bone)
          =/  =naxplanation  [failed error]
          =/  =message-blob  (jam naxplanation)
          ::  send nack-trace message on associated .nack-bone
          ::
          =/  nack-bone=^bone  (mix 0b10 bone)
          mu-abet:(mu-call:(mu-abed:mu:pe-core nack-bone) %memo message-blob)
        --
      ::  +ev-sift: handle request to filter debug output by ship
      ::
      ++  ev-sift
        |=  ships=(list ship)
        ^+  ev-core
        ev-core(ships.bug.ames-state (sy ships))
      ::  +ev-snub: handle request to change ship blacklist
      ::
      ++  ev-snub
        |=  [form=?(%allow %deny) ships=(list ship)]
        ^+  ev-core
        ev-core(snub.ames-state [form (sy ships)])
      ::  +ev-spew: handle request to set verbosity toggles on debug output
      ::
      ++  ev-spew
        |=  verbs=(list verb)
        ^+  ev-core
        ::  start from all %.n's, then flip requested toggles
        ::
        =.  veb.bug.ames-state
          %+  roll  verbs
          |=  [=verb acc=_veb-all-off]
          ^+  veb.bug.ames-state
          ?-  verb
            %snd  acc(snd %.y)
            %rcv  acc(rcv %.y)
            %odd  acc(odd %.y)
            %msg  acc(msg %.y)
            %ges  acc(ges %.y)
            %for  acc(for %.y)
            %rot  acc(rot %.y)
            %kay  acc(kay %.y)
            %fin  acc(fin %.y)
          ==
        ev-core
      ::  +ev-prod: re-send a packet per flow to each of .ships
      ::
      ++  ev-prod
        |=  ships=(list ship)
        ^+  ev-core
        =?  ships  =(~ ships)  ~(tap in ~(key by peers.ames-state))
        |^  ^+  ev-core
        ?~  ships  ev-core
        $(ships t.ships, ev-core (prod-peer i.ships))
        ::
        ++  prod-peer
          |=  her=ship
          ^+  ev-core
          ?~  par=(ev-get-peer-state her)
            ev-core
          =/  pe-core  (her:pe-abed:pe her u.par)
          =/  bones    ~(tap in ~(key by snd.u.par))
          |-  ^+  ev-core
          ?~  bones    pe-abet:pe-core
          =.  pe-core  mu-abet:(mu-call:(mu-abed:mu:pe-core i.bones) %prod ~)
          $(bones t.bones)
        --
      ::  +ev-cong: adjust congestion control parameters
      ::
      ++  ev-cong
        |=  [msg=@ud mem=@ud]
        ^+  ev-core
        ev-core(cong.ames-state msg^mem)
      ::  +ev-stir: recover from timer desync, setting new timers as needed
      ::
      ::    .arg can be %rift or %dead
      ::
      ++  ev-stir
        |=  arg=@t
        ^+  ev-core
        |^  ?+  arg  do-stir
              %rift  do-rift
              %dead  do-dead
            ==
        ::
        ++  do-dead
          =/  ded=(unit dead-timer)  +.flow.dead.ames-state
          ?^  ded
            %-  (slog leaf+"ames: turning off dead flow consolidation" ~)
            =.  ev-core
              (ev-emit:ev-core duct.u.ded %pass wire.u.ded %b %rest date.u.ded)
            (ev-wake-dead-flows:ev-core ~)
          ::
          %-  (slog leaf+"ames: switching to dead flow consolidation" ~)
          =;  cor=ev-core
            ev-set-dead-flow-timer:(ev-wake-dead-flows:cor ~)
          %-  ~(rep by peers.ames-state:ev-core)
          |=  [[=ship =ship-state] core=_ev-core]
          ^+  ev-core
          ?~  peer-state=(ev-get-peer-state:core ship)
            core
          %-  ~(rep by snd.u.peer-state)
          |=  [[=bone =message-pump-state] cor=_core]
          ^+  ev-core
          =/  next-wake  next-wake.packet-pump-state.message-pump-state
          ?.  ?&  =(~m2 rto.metrics.packet-pump-state.message-pump-state)
                  ?=(^ next-wake)
              ==
            cor
          =/  pe-core       (her:pe-abed:pe:cor ship u.peer-state)
          =/  message-pump  (mu-abed:mu:pe-core bone)
          pe-abet:(pu-emit:pu-core:message-pump %b %rest u.next-wake)
        ::
        ++  do-rift
          =/  =rift
            =-  ~|(%no-rift (,@ q.q:(need (need -))))
            (rof ~ /ames %j `beam`[[our %rift %da now] /(scot %p our)])
          ?:  =(rift rift.ames-state)
            ev-core
          ~&  "ames: fixing rift from {<rift.ames-state>} to {<rift>}"
          ev-core(ames-state ames-state(rift rift))
        ::
        ++  do-stir
          =/  want=(set [@da ^duct])
            %-  ~(rep by peers.ames-state)
            |=  [[who=ship s=ship-state] acc=(set [@da ^duct])]
            ?.  ?=(%known -.s)  acc
            %-  ~(rep by snd.+.s)
            |=  [[b=bone m=message-pump-state] acc=_acc]
            =*  tim  next-wake.packet-pump-state.m
            ?~  tim  acc
            %-  ~(put in acc)
            [u.tim `^duct`~[ames+(make-pump-timer-wire who b) /ames]]
          =.  want
            (~(put in want) (add now ~d1) ~[/ames/recork /ames])
          ::
          =/  have
            %-  ~(gas in *(set [@da ^duct]))
            =/  tim
              ;;  (list [@da ^duct])
              =<  q.q  %-  need  %-  need
              (rof ~ /ames %bx [[our %$ da+now] /debug/timers])
            (skim tim |=([@da d=^duct] ?=([[%ames ?(%pump %recork) *] *] d)))
          ::
          ::  set timers for flows that should have one set but don't
          ::
          =.  ev-core
            %-  ~(rep in (~(dif in want) have))
            |=  [[wen=@da hen=^duct] this=_ev-core]
            ?>  ?=([^ *] hen)
            (ev-emit:this ~[/ames] %pass t.i.hen %b %wait wen)
          ::
          ::  cancel timers for flows that have one set but shouldn't
          ::
          %-  ~(rep in (~(dif in have) want))
          |=  [[wen=@da hen=^duct] this=_ev-core]
          ?>  ?=([^ *] hen)
          (ev-emit:this t.hen %pass t.i.hen %b %rest wen)
        --
      ::  +ev-crud: handle event failure; print to dill
      ::
      ++  ev-crud
        |=  =error
        ^+  ev-core
        (ev-emit duct %pass /crud %d %flog %crud error)
      ::  +ev-heed: handle request to track .ship's responsiveness
      ::
      ++  ev-heed
        |=  =ship
        ^+  ev-core
        =/  ship-state  (~(get by peers.ames-state) ship)
        ?:  ?=([~ %known *] ship-state)
          pe-abet:pe-heed:(her:pe-abed:pe ship +.u.ship-state)
        %^  ev-enqueue-alien-todo  ship  ship-state
        |=  todos=alien-agenda
        todos(heeds (~(put in heeds.todos) duct))
      ::  +ev-jilt: handle request to stop tracking .ship's responsiveness
      ::
      ++  ev-jilt
        |=  =ship
        ^+  ev-core
        =/  ship-state  (~(get by peers.ames-state) ship)
        ?:  ?=([~ %known *] ship-state)
          pe-abet:pe-jilt:(her:pe-abed:pe ship +.u.ship-state)
        %^  ev-enqueue-alien-todo  ship  ship-state
        |=  todos=alien-agenda
        todos(heeds (~(del in heeds.todos) duct))
      :: +ev-dear: handle lane from unix
      ::
      ++  ev-dear
        |=  [=ship =lane]
        ^+  ev-core
        ?:  ?=(%.y -.lane)  ev-core
        =/  ip=@if  (end [0 32] p.lane)
        =/  pt=@ud  (cut 0 [32 16] p.lane)
        ?:  =(%czar (clan:title ship))
          %-  %^  ev-trace  odd.veb  ship
            |.("ignoring %dear lane {(scow %if ip)}:{(scow %ud pt)} for galaxy")
          ev-core
          ?~  peer-state=(ev-get-peer-state ship)
          %-  %^  ev-trace  odd.veb  ship
            |.("no peer-state for ship, ignoring %dear")
          ev-core
        %-  %^  ev-trace  rcv.veb  ship
          |.("incoming %dear lane {(scow %if ip)}:{(scow %ud pt)}")
        pe-abet:(pe-dear:(her:pe-abed:pe ship u.peer-state) lane)
      ::  +ev-hear: handle raw packet receipt
      ::
      ++  ev-hear
        |=  [l=lane b=blob d=(unit goof)]
        ^+  ev-core
        =/  =shot     (sift-shot b)
        ?:  sam.shot  (ev-hear-packet l shot d)
        ?:  req.shot  ~|([%fine %request-events-forbidden] !!)
        ::  TODO no longer true
        ::NOTE  we only send requests to ships we know,
        ::      so we should only get responses from ships we know.
        ::      below we assume sndr.shot is a known peer.
        =*  her  sndr.shot
        =+  ?~  d  ~
            %.  ~
            =*  mot  mote.u.d
            %+  slog  leaf+"ames: fine from {<her>} on {<l>} crashed {<mot>}"
            ?.  msg.veb  ~
            tang.u.d
        pe-abet:(pe-hear-fine:(got:pe-abed:pe her) l shot)
      ::  +ev-hear-packet: handle mildly processed packet receipt
      ::
      ++  ev-hear-packet
        ~/  %ev-hear-packet
        |=  [=lane =shot dud=(unit goof)]
        ^+  ev-core
        %-  (ev-trace rcv.veb sndr.shot |.("received packet"))
        ::
        ?:  =(our sndr.shot)
          ev-core
        ?:  .=  =(%deny form.snub.ames-state)
            (~(has in ships.snub.ames-state) sndr.shot)
          %-  (ev-trace rcv.veb sndr.shot |.("snubbed"))
          ev-core
        ::
        %.  +<
        ::
        ?.  =(our rcvr.shot)
          ev-hear-forward
        ::
        ?:  =(%keys content.shot)
          ev-hear-keys
        ?:  ?&  ?=(%pawn (clan:title sndr.shot))
                !?=([~ %known *] (~(get by peers.ames-state) sndr.shot))
            ==
          ev-hear-open
        ev-hear-shut
      ::  +ev-hear-forward: maybe forward a packet to someone else
      ::
      ::    Note that this performs all forwarding requests without
      ::    filtering.  Any protection against DDoS amplification will be
      ::    provided by Vere.
      ::
      ++  ev-hear-forward
        ~/  %ev-hear-forward
        |=  [=lane =shot dud=(unit goof)]
        ^+  ev-core
        %-  %^  ev-trace  for.veb  sndr.shot
            |.("forward: {<sndr.shot>} -> {<rcvr.shot>}")
        ::  set .origin.shot if it doesn't have one, re-encode, and send
        ::
        =?    origin.shot
            &(?=(~ origin.shot) !=(%czar (clan:title sndr.shot)))
          ?:  ?=(%& -.lane)
            ~
          ?.  (lte (met 3 p.lane) 6)
            ~|  ames-lane-size+p.lane  !!
          `p.lane
        ::
        =/  =blob  (etch-shot shot)
        %-  ev-send-blob
        [for=& rcvr.shot blob (~(get by peers.ames-state) rcvr.shot)]
      ::  +ev-hear-keys: handle receipt of attestion request
      ::
      ++  ev-hear-keys
        ~/  %ev-hear-keys
        |=  [=lane =shot dud=(unit goof)]
        =+  %^  ev-trace  msg.veb  sndr.shot
            |.("requested attestation")
        ?.  =(%pawn (clan:title our))
          ev-core
        =/  =blob  (ev-attestation-packet sndr.shot 1)
        %-  ev-send-blob
        [for=| sndr.shot blob (~(get by peers.ames-state) sndr.shot)]
      ::  +ev-hear-open: handle receipt of plaintext comet self-attestation
      ::
      ++  ev-hear-open
        ~/  %ev-hear-open
        |=  [=lane =shot dud=(unit goof)]
        ^+  ev-core
        =+  %^  ev-trace  msg.veb  sndr.shot
            |.("got attestation")
        ::  assert the comet can't pretend to be a moon or other address
        ::
        ?>  ?=(%pawn (clan:title sndr.shot))
        ::  if we already know .sndr, ignore duplicate attestation
        ::
        =/  ship-state  (~(get by peers.ames-state) sndr.shot)
        ?:  ?=([~ %known *] ship-state)
          ev-core
        ::
        =/  =open-packet  (sift-open-packet shot our life.ames-state)
        ::  add comet as an %alien if we haven't already
        ::
        =?  peers.ames-state  ?=(~ ship-state)
          (~(put by peers.ames-state) sndr.shot %alien *alien-agenda)
        ::  upgrade comet to %known via on-publ-full
        ::
        =.  ev-core
          =/  crypto-suite=@ud  1
          =/  keys
            (my [sndr-life.open-packet crypto-suite public-key.open-packet]~)
          =/  =point
            :*  ^=     rift  0
                ^=     life  sndr-life.open-packet
                ^=     keys  keys
                ^=  sponsor  `(^sein:title sndr.shot)
            ==
          (ev-publ / [%full (my [sndr.shot point]~)])
        ::  manually add the lane to the peer state
        ::
        =.  peers.ames-state
          =/  =peer-state  (ev-gut-peer-state sndr.shot)
          =.  route.peer-state  `[direct=%.n lane]
          (~(put by peers.ames-state) sndr.shot %known peer-state)
        ::
        ev-core
      ::  +ev-hear-shut: handle receipt of encrypted packet
      ::
      ++  ev-hear-shut
        ~/  %ev-hear-shut
        |=  [=lane =shot dud=(unit goof)]
        ^+  ev-core
        =/  sndr-state  (~(get by peers.ames-state) sndr.shot)
        ::  If we don't know them, ask Jael for their keys. If they're a
        ::  comet, this will also cause us to request a self-attestation
        ::  from the sender. The packet itself is dropped; we can assume it
        ::  will be resent.
        ::
        ?.  ?=([~ %known *] sndr-state)
          (ev-enqueue-alien-todo sndr.shot sndr-state |=(alien-agenda +<))
        ::  decrypt packet contents using symmetric-key.channel
        ::
        ::    If we know them, we have a $channel with them, which we've
        ::    populated with a .symmetric-key derived from our private key
        ::    and their public key using elliptic curve Diffie-Hellman.
        ::
        =/  =peer-state   +.u.sndr-state
        =/  =channel      [[our sndr.shot] now ev-channel-state -.peer-state]
        =?  ev-core  !=(sndr-tick.shot (mod her-life.channel 16))
          %-  %^  ev-trace  odd.veb  sndr.shot
              |.  ^-  tape
              =/  sndr  [sndr-tick=sndr-tick.shot her-life=her-life.channel]
              "sndr-tick mismatch {<sndr>}"
          ev-core
        =?  ev-core  !=(rcvr-tick.shot (mod our-life.channel 16))
          %-  %^  ev-trace  odd.veb  sndr.shot
              |.  ^-  tape
              =/  rcvr  [rcvr-tick=rcvr-tick.shot our-life=our-life.channel]
              "rcvr-tick mismatch {<rcvr>}"
          ev-core
        ~|  %ames-crash-on-packet-from^her.channel
        =/  shut-packet=(unit shut-packet)
          (sift-shut-packet shot [symmetric-key her-life our-life]:channel)
        ?~  shut-packet  ev-core
        ::  non-galaxy: update route with heard lane or forwarded lane
        ::
        =?  route.peer-state  !=(%czar (clan:title her.channel))
          ::  if new packet is direct, use that.  otherwise, if the new new
          ::  and old lanes are indirect, use the new one.  if the new lane
          ::  is indirect but the old lane is direct, then if the lanes are
          ::  identical, don't mark it indirect; if they're not identical,
          ::  use the new lane and mark it indirect.
          ::
          ::  if you mark lane as indirect because you got an indirect
          ::  packet even though you already had a direct identical lane,
          ::  then delayed forwarded packets will come later and reset to
          ::  indirect, so you're unlikely to get a stable direct route
          ::  (unless the forwarder goes offline for a while).
          ::
          ::  conversely, if you don't accept indirect routes with different
          ::  lanes, then if your lane is stale and they're trying to talk
          ::  to you, your acks will go to the stale lane, and you'll never
          ::  time it out unless you reach out to them.  this manifests as
          ::  needing to |hi or dotpost to get a response when the other
          ::  ship has changed lanes.
          ::
          ?:  ?=(~ origin.shot)
            `[direct=%.y lane]
          ?:  ?=([~ %& *] route.peer-state)
            ?:  =(lane.u.route.peer-state |+u.origin.shot)
              route.peer-state
            `[direct=%.n |+u.origin.shot]
          `[direct=%.n |+u.origin.shot]
        ::  perform peer-specific handling of packet
        ::
        =<  pe-abet
        (~(pe-hear-shut-packet pe peer-state channel) [lane u.shut-packet dud])
      ::  +ev-take-boon: receive request to give message to peer
      ::
      ++  ev-take-boon
        |=  [=wire payload=*]
        ^+  ev-core
        ?~  parsed=(parse-bone-wire wire)
          ~>  %slog.0^leaf/"ames: dropping malformed wire: {(spud wire)}"
          ev-core
        ::
        ?>  ?=([@ her=ship *] u.parsed)
        =*  her      her.u.parsed
        =/  pe-core  (got:pe-abed:pe her)
        ::
        ?:  ?&  ?=([%new *] u.parsed)
                (lth rift.u.parsed rift.peer-state.pe-core)
            ==
          ::  ignore events from an old rift
          ::
          %-  %^  ev-trace  odd.veb  her
              |.("dropping old rift wire: {(spud wire)}")
          ev-core
        =/  =bone
          ?-(u.parsed [%new *] bone.u.parsed, [%old *] bone.u.parsed)
        =?  pe-core  ?=([%old *] u.parsed)
          %-  %^  ev-trace  odd.veb  her
              |.("parsing old wire: {(spud wire)}")
          pe-core
        pe-abet:(pe-memo:pe-core bone payload %boon)
      ::  +ev-plea: handle request to send message
      ::
      ++  ev-plea
        |=  [=ship =plea]
        ^+  ev-core
        =/  ship-state  (~(get by peers.ames-state) ship)
        ::
        ?.  ?=([~ %known *] ship-state)
          %^  ev-enqueue-alien-todo  ship  ship-state
          |=  todos=alien-agenda
          todos(messages [[duct plea] messages.todos])
        ::
        =+  pe-core=(her:pe-abed:pe ship +.u.ship-state)
        ::  .plea is from local vane to foreign ship
        ::
        =^  =bone  pe-core  (pe-bind-duct:pe-core duct)
        %-  %^  ev-trace  msg.veb  ship
            |.  ^-  tape
            =/  sndr  [our our-life.channel.pe-core]
            =/  rcvr  [ship her-life.channel.pe-core]
            "plea {<sndr rcvr bone=bone vane.plea path.plea>}"
        pe-abet:(pe-memo:pe-core bone plea %plea)
      ::  +ev-tame: handle request to delete a route
      ::
      ++  ev-tame
        |=  =ship
        ^+  ev-core
        ?:  =(%czar (clan:title ship))
          %-  %+  slog
            leaf+"ames: bad idea to %tame galaxy {(scow %p ship)}, ignoring"
          ~
          ev-core
        ?~  peer-state=(ev-get-peer-state ship)
          %-  (slog leaf+"ames: no peer-state for {(scow %p ship)}, ignoring" ~)
          ev-core
        pe-abet:pe-tame:(her:pe-abed:pe ship u.peer-state)
      ::  +ev-cork: handle request to kill a flow
      ::
      ++  ev-cork
        |=  =ship
        ^+  ev-core
        =/  =plea       [%$ /flow [%cork ~]]
        =/  ship-state  (~(get by peers.ames-state) ship)
        ?.  ?=([~ %known *] ship-state)
          %^  ev-enqueue-alien-todo  ship  ship-state
          |=  todos=alien-agenda
          todos(messages [[duct plea] messages.todos])
        ::
        =+  pe-core=(her:pe-abed:pe ship +.u.ship-state)
        =^  =bone  pe-core
          ?^  cork-bone  [u.cork-bone pe-core]
          (pe-bind-duct:pe-core duct)
        ::
        ?.  (~(has by by-bone.ossuary.peer-state.pe-core) bone)
          %-  %^  ev-trace  odd.veb  ship
              |.("trying to cork {<bone=bone>}, not in the ossuary, ignoring")
          ev-core
        ::
        %-  %^  ev-trace  msg.veb  ship
            |.  ^-  tape
            =/  sndr  [our our-life.channel.pe-core]
            =/  rcvr  [ship her-life.channel.pe-core]
            "cork plea {<sndr rcvr bone=bone vane.plea path.plea>}"
        pe-abet:(pe-memo:(pe-cork-flow:pe-core bone) bone plea %plea)
      ::  +ev-kroc: cork all stale flows from failed subscriptions
      ::
      ++  ev-kroc
        |=  bones=(list [ship bone])
        ^+  ev-core
        %+  roll  bones
        |=  [[=ship =bone] co=_ev-core]
        (%*(ev-cork co cork-bone `bone) ship)
      ::  +ev-deep: deferred %ames calls from itself
      ::
      ++  ev-deep
        |=  =deep
        ^+  ev-core
        ::  currently $deep tasks are all focused on a
        ::  particular ship but future ones might not
        ::
        ?>  ?=([@ =ship *] deep)
        =/  ship-state  (~(get by peers.ames-state) ship.deep)
        ?>  ?=([~ %known *] ship-state)
        =+  pe-core=(her:pe-abed:pe ship.deep +.u.ship-state)
        |^  ?-  -.deep
          %nack  pe-abet:(send-nack-trace [nack-bone message-blob]:deep)
          %sink  pe-abet:(sink-naxplanation [target-bone naxplanation]:deep)
          %drop  pe-abet:(clear-nack [nack-bone message-num]:deep)
          %cork  (cork-bone bone.deep)
          %kill  (kill-bone bone.deep)
        ==
        ::
        ++  send-nack-trace
          |=  [=nack=bone =message-blob]
          mu-abet:(mu-call:(mu-abed:mu:pe-core nack-bone) %memo message-blob)
        ::
        ++  sink-naxplanation
          |=  [=target=bone =naxplanation]
          mu-abet:(mu-call:(mu-abed:mu:pe-core target-bone) %near naxplanation)
        ::
        ++  clear-nack
          |=  [=nack=bone =message-num]
          mi-abet:(mi-call:(mi-abed:mi:pe-core nack-bone) %drop message-num)
        ::  client ames [%cork as plea] -> server ames [sinks %cork plea],
        ::                                 pass %deep %cork task to self
        ::                                 put flow in closing (+cork-bone),
        ::                                 and give %done
        ::  sink %ack, pass %deep %kill <- after +ev-take-done, ack %cork plea
        ::  task to self, and delete the   and delete the flow (+pe-handle-cork)
        ::  flow (+kill-bone)
        ::
        ++  cork-bone
          |=  =bone
          =~  pe-abet:(pe-cork-flow:pe-core bone)
              (ev-emit duct %give %done ~)
          ==
        ::
        ++  kill-bone  |=(=bone pe-abet:(pe-kill-flow:pe-core bone))
        --
      :: +ev-set-dead-flow-timer: set dead flow timer and its ames state
      ::
      ++  ev-set-dead-flow-timer
        ^+  ev-core
        =.  flow.dead.ames-state.ev-core
          flow/`[~[/ames] /dead-flow `@da`(add now ~m2)]
        (ev-emit:ev-core ~[/ames] %pass /dead-flow %b %wait `@da`(add now ~m2))
      :: +ev-wake-dead-flows: call pe-wake on all dead flows, discarding any
      :: ames-state changes
      ::
      ++  ev-wake-dead-flows
        |=  [error=(unit tang)]
        ^+  ev-core
        %-  ~(rep by peers.ames-state:ev-core)
        |=  [[=ship =ship-state] core=_ev-core]
        ^+  ev-core
        ?~  peer-state=(ev-get-peer-state:core ship)  core
        =+  pe-core=(her:pe-abed:pe:core ship u.peer-state)
        =<  pe-abort  ^+  pe-core
        %-  ~(rep by snd.u.peer-state)
        |=  [[=bone =message-pump-state] cor=_pe-core]
        ?.  ?&  =(~m2 rto.metrics.packet-pump-state.message-pump-state)
                ?=(^ next-wake.packet-pump-state.message-pump-state)
            ==
          cor
        (pe-wake:cor bone error)
      ::  +ev-take-wake: receive wakeup or error notification from behn
      ::
      ++  ev-take-wake
        |=  [=wire error=(unit tang)]
        ^+  ev-core
        ?:  ?=([%alien @ ~] wire)
          ::  if we haven't received an attestation, ask again
          ::
          ?^  error
            %-  (slog 'ames: attestation timer failed' u.error)
            ev-core
          ?~  ship=`(unit @p)`(slaw %p i.t.wire)
            %-  (slog leaf+"ames: got timer for strange wire: {<wire>}" ~)
            ev-core
          =/  ship-state  (~(get by peers.ames-state) u.ship)
          ?:  ?=([~ %known *] ship-state)
            ev-core
          (ev-request-attestation u.ship)
        ::
        ?:  ?=([%dead-flow ~] wire)
          ev-set-dead-flow-timer:(ev-wake-dead-flows error)
        ::
        ?.  ?=([%recork ~] wire)
          =/  res=(unit ?([%fine her=ship =^wire] [%pump her=ship =bone]))
            ?+  wire  ~
              [%pump ship=@ bone=@ ~]  (parse-pump-wire &2.wire &3.wire)
              [%fine %behn %wake @ *]  (parse-fine-wire &4.wire t.t.t.t.wire)
            ==
          ?~  res
            %-  (slog leaf+"ames: got timer for strange wire: {<wire>}" ~)
            ev-core
          ::
          =/  state=(unit peer-state)  (ev-get-peer-state her.u.res)
          ?~  state
            %-  %-  slog  :_  ~
                leaf/"ames: got timer for strange ship: {<her.u.res>}, ignoring"
            ev-core
          ::
          =/  pe-core  (her:pe-abed:pe her.u.res u.state)
          ?-  -.u.res
            %pump  pe-abet:(pe-wake:pe-core bone.u.res error)
            ::
              %fine
            ?.  (~(has by keens.peer-state.pe-core) wire.u.res)
              ev-core
            pe-abet:fi-abet:fi-take-wake:(fi-abed:fi:pe-core wire.u.res)
          ==
        ::
        =.  ev-core  (ev-emit duct %pass /recork %b %wait `@da`(add now ~d1))
        =.  cork.dead.ames-state
          cork/`[~[/ames] /recork `@da`(add now ~d1)]
        ::
        ?^  error
          %-  (slog 'ames: recork timer failed' u.error)
          ev-core
        ::  recork up to one bone per peer
        ::
        =/  pez  ~(tap by peers.ames-state)
        |-  ^+  ev-core
        ?~  pez  ev-core
        =+  [her sat]=i.pez
        ?.  ?=(%known -.sat)
          $(pez t.pez)
        $(pez t.pez, ev-core pe-abet:pe-recork-one:(her:pe-abed:pe her +.sat))
      ::  +ev-init: first boot; subscribe to our info from jael
      ::
      ++  ev-init
        ^+  ev-core
        ::
        =~  (ev-emit duct %pass /turf %j %turf ~)
            (ev-emit duct %pass /private-keys %j %private-keys ~)
            (ev-emit duct %pass /public-keys %j %public-keys [n=our ~ ~])
        ==
      ::  +ev-priv: set our private key to jael's response
      ::
      ++  ev-priv
        |=  [=life vein=(map life private-key)]
        ^+  ev-core
        ::
        =/  =private-key            (~(got by vein) life)
        =.  life.ames-state         life
        =.  crypto-core.ames-state  (nol:nu:crub:crypto private-key)
        ::  recalculate each peer's symmetric key
        ::
        =/  our-private-key  sec:ex:crypto-core.ames-state
        =.  peers.ames-state
          %-  ~(run by peers.ames-state)
          |=  =ship-state
          ^+  ship-state
          ::
          ?.  ?=(%known -.ship-state)
            ship-state
          ::
          =/  =peer-state  +.ship-state
          =.  symmetric-key.peer-state
            (derive-symmetric-key public-key.+.ship-state our-private-key)
          ::
          [%known peer-state]
        ::
        ev-core
      ::  +ev-publ: update pki data for peer or self
      ::
      ++  ev-publ
        |=  [=wire =public-keys-result]
        ^+  ev-core
        ::
        |^  ^+  ev-core
            ::
            ?-    public-keys-result
                [%diff @ %rift *]
              (on-publ-rift [who to.diff]:public-keys-result)
            ::
                [%diff @ %keys *]
              (on-publ-rekey [who to.diff]:public-keys-result)
            ::
                [%diff @ %spon *]
              (on-publ-sponsor [who to.diff]:public-keys-result)
            ::
                [%full *]
              (on-publ-full points.public-keys-result)
            ::
                [%breach *]
              (on-publ-breach who.public-keys-result)
            ==
        ::  +on-publ-breach: handle continuity breach of .ship; wipe its state
        ::
        ::    Abandon all pretense of continuity and delete all messaging state
        ::    associated with .ship, including sent and unsent messages.
        ::    Also cancel all timers related to .ship.
        ::
        ++  on-publ-breach
          |=  =ship
          ^+  ev-core
          ?:  =(our ship)  ev-core
          ::
          =/  ship-state  (~(get by peers.ames-state) ship)
          ::  we shouldn't be hearing about ships we don't care about
          ::
          ?~  ship-state
            ~>  %slog.0^leaf/"ames: breach unknown {<our ship>}"
            ev-core
          ::  if an alien breached, this doesn't affect us
          ::
          ?:  ?=([~ %alien *] ship-state)
            ~>  %slog.0^leaf/"ames: breach alien {<our ship>}"
            ev-core
          ~>  %slog.0^leaf/"ames: breach peer {<our ship>}"
          ::  a peer breached; drop messaging state
          ::
          =/  =peer-state  +.u.ship-state
          =/  old-qos=qos  qos.peer-state
          ::  cancel all timers related to .ship
          ::
          =.  ev-core
            %+  roll  ~(tap by snd.peer-state)
            |=  [[=snd=bone =message-pump-state] core=_ev-core]
            ^+  core
            ::
            ?~  next-wake=next-wake.packet-pump-state.message-pump-state
              core
            ::  note: copies +on-pump-rest:message-pump
            ::
            =/  wire  (make-pump-timer-wire ship snd-bone)
            (ev-emit:core ~[/ames] %pass wire %b %rest u.next-wake)
          ::  reset all peer state other than pki data
          ::
          =.  +.peer-state  +:*^peer-state
          ::  print change to quality of service, if any
          ::
          =/  text=(unit tape)
            %^  qos-update-text  ship  %ames
            [old-qos qos.peer-state kay.veb ships.bug.ames-state]
          ::
          =?  ev-core  ?=(^ text)
            (ev-emit duct %pass /qos %d %flog %text u.text)
          ::  reinitialize galaxy route if applicable
          ::
          =?  route.peer-state  =(%czar (clan:title ship))
            `[direct=%.y lane=[%& ship]]
          ::
          =.  peers.ames-state
            (~(put by peers.ames-state) ship [%known peer-state])
          ::
          ev-core
        ::  +on-publ-rekey: handle new key for peer
        ::
        ::    TODO: assert .crypto-suite compatibility
        ::
        ++  on-publ-rekey
          |=  $:  =ship
                  =life
                  crypto-suite=@ud
                  =public-key
              ==
          ^+  ev-core
          ?:  =(our ship)  ev-core
          ::
          =/  ship-state  (~(get by peers.ames-state) ship)
          ?.  ?=([~ %known *] ship-state)
            =|  =point
            =.  life.point     life
            =.  keys.point     (my [life crypto-suite public-key]~)
            =.  sponsor.point  `(^^sein:title rof /ames our now ship)
            ::
            (on-publ-full (my [ship point]~))
          ::
          =/  =peer-state   +.u.ship-state
          =/  =private-key  sec:ex:crypto-core.ames-state
          =.  symmetric-key.peer-state
            (derive-symmetric-key public-key private-key)
          ::
          =.  life.peer-state        life
          =.  public-key.peer-state  public-key
          ::
          =.  peers.ames-state
           (~(put by peers.ames-state) ship %known peer-state)
          ev-core
        ::  +on-publ-sponsor: handle new or lost sponsor for peer
        ::
        ::    TODO: really handle sponsor loss
        ::
        ++  on-publ-sponsor
          |=  [=ship sponsor=(unit ship)]
          ^+  ev-core
          ::
          ?:  =(our ship)  ev-core
          ::
          ?~  sponsor
            %-  (slog leaf+"ames: {(scow %p ship)} lost sponsor, ignoring" ~)
            ev-core
          ::
          =/  state=(unit peer-state)  (ev-get-peer-state ship)
          ?~  state
            %-  (slog leaf+"ames: missing peer-state, ignoring" ~)
            ev-core
          =.  sponsor.u.state   u.sponsor
          =.  peers.ames-state  (~(put by peers.ames-state) ship %known u.state)
          ev-core
        ::  +on-publ-full: handle new pki data for peer(s)
        ::
        ++  on-publ-full
          |=  points=(map ship point)
          ^+  ev-core
          ::
          =>  .(points ~(tap by points))
          |^  ^+  ev-core
              ?~  points  ev-core
              ::
              =+  ^-  [=ship =point]  i.points
              ::
              ?:  =(our ship)
                =.  rift.ames-state  rift.point
                $(points t.points)
              ::
              ?.  (~(has by keys.point) life.point)
                $(points t.points)
              ::
              =/  old-ship-state  (~(get by peers.ames-state) ship)
              ::
              =.  ev-core  (insert-peer-state ship point)
              ::
              =?  ev-core  ?=([~ %alien *] old-ship-state)
                (meet-alien ship point +.u.old-ship-state)
              ::
              $(points t.points)
          ::
          ++  meet-alien
            |=  [=ship =point todos=alien-agenda]
            |^  ^+  ev-core
            ::  if we're a comet, send self-attestation packet first
            ::
            =?  ev-core  =(%pawn (clan:title our))
              =/  =blob  (ev-attestation-packet ship life.point)
              (ev-send-blob for=| ship blob (~(get by peers.ames-state) ship))
            ::  save current duct
            ::
            =/  original-duct  duct
            ::  apply heeds
            ::
            =.  ev-core
              %+  roll  ~(tap in heeds.todos)
              |=  [=^duct core=_ev-core]
              (ev-heed:core(duct duct) ship)
            ::  apply outgoing messages, reversing for FIFO order
            ::
            =.  ev-core
              %+  reel  messages.todos
              |=  [[=^duct =plea] core=_ev-core]
              ?:  ?=(%$ -.plea)
                (ev-cork:core(duct duct) ship)
              (ev-plea:core(duct duct) ship plea)
            ::  apply outgoing packet blobs
            ::
            =.  ev-core
              %+  roll  ~(tap in packets.todos)
              |=  [=blob c=_ev-core]
              (ev-send-blob:c for=| ship blob (~(get by peers.ames-state) ship))
            ::  apply remote scry requests
            ::
            =.  ev-core  (meet-alien-fine keens.todos)
            ::
            ev-core(duct original-duct)
            ::
            ++  meet-alien-fine
              |=  peens=(jug path ^duct)
              ^+  ev-core
              =+  pe-core=(gut:pe-abed:pe ship)
              =<   pe-abet  ^+  pe-core
              %-  ~(rep by peens)
              |=  [[=path ducts=(set ^duct)] cor=_pe-core]
              (~(rep in ducts) |=([=^duct c=_cor] (pe-keen:c path duct)))
            --
          --
        ::  on-publ-rift: XX
        ::
        ++  on-publ-rift
          |=  [=ship =rift]
          ^+  ev-core
          ?:  =(our ship)
            =.  rift.ames-state  rift
            ev-core
          ?~  ship-state=(~(get by peers.ames-state) ship)
            ::  print error here? %rift was probably called before %keys
            ::
            ~>  %slog.1^leaf/"ames: missing peer-state on-publ-rift"
            ev-core
          ?:  ?=([%alien *] u.ship-state)
            ::  ignore aliens
            ::
            ev-core
          =/  =peer-state      +.u.ship-state
          =.  rift.peer-state  rift
          =.  peers.ames-state
            (~(put by peers.ames-state) ship %known peer-state)
          ev-core
        ::
        ++  insert-peer-state
          |=  [=ship =point]
          ^+  ev-core
          ::
          =/  =peer-state     (ev-gut-peer-state ship)
          =/  =public-key     pass:(~(got by keys.point) life.point)
          =/  =private-key    sec:ex:crypto-core.ames-state
          =/  =symmetric-key  (derive-symmetric-key public-key private-key)
          ::
          =.  qos.peer-state            [%unborn now]
          =.  life.peer-state           life.point
          =.  rift.peer-state           rift.point
          =.  public-key.peer-state     public-key
          =.  symmetric-key.peer-state  symmetric-key
          =.  sponsor.peer-state
            ?^  sponsor.point
              u.sponsor.point
            (^^sein:title rof /ames our now ship)
          ::  automatically set galaxy route, since unix handles lookup
          ::
          =?  route.peer-state  ?=(%czar (clan:title ship))
            `[direct=%.y lane=[%& ship]]
          ::
          =.  peers.ames-state
            (~(put by peers.ames-state) ship %known peer-state)
          ::
          ev-core
        --
      ::  +ev-take-turf: relay %turf move from jael to unix
      ::
      ++  ev-take-turf
        |=  turfs=(list turf)
        ^+  ev-core
        ::
        (ev-emit unix-duct.ames-state %give %turf turfs)
      ::  +ev-born: handle unix process restart
      ::
      ++  ev-born
        ^+  ev-core
        ::
        =.  unix-duct.ames-state  duct
        ::
        =/  turfs
          ;;  (list turf)
          =<  q.q  %-  need  %-  need
          (rof ~ /ames %j `beam`[[our %turf %da now] /])
        ::
        =*  duct  unix-duct.ames-state
        ::
        =^  cork-moves  cork.dead.ames-state
          ?.  ?=(~ +.cork.dead.ames-state)
            `cork.dead.ames-state
          :-  [~[/ames] %pass /recork %b %wait `@da`(add now ~d1)]~
          cork/`[~[/ames] /recork `@da`(add now ~d1)]
        ::
        %-  ev-emil
        %+  weld
          cork-moves
        ^-  (list move)
        :~  [duct %give %turf turfs]
            [duct %pass /ping %g %deal [our our /a] %ping %poke %noun !>(%kick)]
        ==
      ::  +ev-vega: handle kernel reload
      ::
      ++  ev-vega  ev-core
      ::  +ev-trim: handle request to free memory
      ::
      ::  %ruin comets not seen for six months
      ::
      ++  ev-trim    ::TODO  trim fine parts on high prio
        ^+  ev-core
        =;  rui=(set @p)
          (ev-emit duct %pass /ruin %j %ruin rui)
        =-  (silt (turn - head))
        %+  skim
          ~(tap by peers.ames-state)
        |=  [=ship s=ship-state]
        ?.  &(?=(%known -.s) =(%pawn (clan:title ship)))  %.n
        ?&  (gth (sub now ~d180) last-contact.qos.s)
            ::
            %-  ~(any by snd.s)
            |=  m=message-pump-state
            !=(~ unsent-fragments.m)
        ==
      ::
      +|  %fine-entry-points
      ::
      ++  ev-keen
        |=  spar
        ^+  ev-core
        =+  ~:(spit path)  ::  assert length
        =/  ship-state  (~(get by peers.ames-state) ship)
        ?:  ?=([~ %known *] ship-state)
          pe-abet:(pe-keen:(her:pe-abed:pe ship +.u.ship-state) path duct)
        %^  ev-enqueue-alien-todo  ship  ship-state
        |=  todos=alien-agenda
        todos(keens (~(put ju keens.todos) path duct))
      ::
      ++  ev-cancel-scry
        |=  [all=? spar]
        ^+  ev-core
        ?~  ship-state=(~(get by peers.ames-state) ship)
          ~|(%cancel-scry-missing-peer^ship^path !!)
        ?.  ?=([~ %known *] ship-state)
          :: XX delete from alien agenda?
          %-  %^  trace-fine  fin.veb  ship
              [ships.bug.ames-state |.("peer still alien, skip cancel-scry")]
          ev-core
        =+  peer=(gut:pe-abed:pe ship)
        ?.  (~(has by keens.peer-state.peer) path)
          ev-core
        pe-abet:fi-abet:(fi-unsub:(fi-abed:fi:peer path) duct all)
      ::
      +|  %implementation
      ::  +ev-enqueue-alien-todo: helper to enqueue a pending request
      ::
      ::    Also requests key and life from Jael on first request.
      ::    If talking to a comet, requests attestation packet.
      ::
      ++  ev-enqueue-alien-todo
        |=  $:  =ship
                ship-state=(unit ship-state)
                mutate=$-(alien-agenda alien-agenda)
            ==
        ^+  ev-core
        ::  create a default $alien-agenda on first contact
        ::
        =+  ^-  [already-pending=? todos=alien-agenda]
            ?~  ship-state
              [%.n *alien-agenda]
            [%.y ?>(?=(%alien -.u.ship-state) +.u.ship-state)]
        ::  mutate .todos and apply to permanent state
        ::
        =.  todos             (mutate todos)
        =.  peers.ames-state  (~(put by peers.ames-state) ship %alien todos)
        ?:  already-pending   ev-core
        ?:  =(%pawn (clan:title ship))
          (ev-request-attestation ship)
        ::  NB: we specifically look for this wire in +public-keys-give in
        ::  Jael.  if you change it here, you must change it there.
        ::
        (ev-emit duct %pass /public-keys %j %public-keys [n=ship ~ ~])
      ::  +ev-request-attestation: helper to request attestation from comet
      ::
      ::    Also sets a timer to resend the request every 30s.
      ::
      ++  ev-request-attestation
        |=  =ship
        ^+  ev-core
        =+  (ev-trace msg.veb ship |.("requesting attestion"))
        =.  ev-core
          =/  =blob  (ev-sendkeys-packet ship)
          (ev-send-blob for=| ship blob (~(get by peers.ames-state) ship))
        =/  =wire  /alien/(scot %p ship)
        (ev-emit duct %pass wire %b %wait (add now ~s30))
      ::  +ev-send-blob: fire packet at .ship and maybe sponsors
      ::
      ::    Send to .ship and sponsors until we find a direct lane,
      ::    skipping .our in the sponsorship chain.
      ::
      ::    If we have no PKI data for a recipient, enqueue the packet and
      ::    request the information from Jael if we haven't already.
      ::
      ++  ev-send-blob
        ~/  %ev-send-blob
        |=  [for=? =ship =blob ship-state=(unit ship-state)]
        ::
        =/  final-ship  ship
        %-  (ev-trace rot.veb final-ship |.("send-blob: to {<ship>}"))
        |-
        |^  ^+  ev-core
            ?.  ?=([~ %known *] ship-state)
              ?:  ?=(%pawn (clan:title ship))
                (try-next-sponsor (^sein:title ship))
              %^  ev-enqueue-alien-todo  ship  ship-state
              |=  todos=alien-agenda
              todos(packets (~(put in packets.todos) blob))
            ::
            =/  =peer-state  +.u.ship-state
            ::
            ::  XX  routing hack to mimic old ames.
            ::
            ::    Before removing this, consider: moons when their planet is
            ::    behind a NAT; a planet receiving initial acknowledgment
            ::    from a star; a planet talking to another planet under
            ::    another galaxy.
            ::
            ?:  ?|  =(our ship)
                    ?&  !=(final-ship ship)
                        !=(%czar (clan:title ship))
                    ==
                ==
              (try-next-sponsor sponsor.peer-state)
            ::
            ?:  =(our ship)
              ::  if forwarding, don't send to sponsor to avoid loops
              ::
              ?:  for  ev-core
              (try-next-sponsor sponsor.peer-state)
            ::
            ?~  route=route.peer-state
              %-  (ev-trace rot.veb final-ship |.("no route to:  {<ship>}"))
              (try-next-sponsor sponsor.peer-state)
            ::
            %-  (ev-trace rot.veb final-ship |.("trying route: {<ship>}"))
            =.  ev-core
              (ev-emit unix-duct.ames-state %give %send lane.u.route blob)
            ::
            ?:  direct.u.route  ev-core
            (try-next-sponsor sponsor.peer-state)
        ::
        ++  try-next-sponsor
          |=  sponsor=^ship
          ^+  ev-core
          ::
          ?:  =(ship sponsor)  ev-core
          ^$(ship sponsor, ship-state (~(get by peers.ames-state) sponsor))
        --
      ::  +ev-attestation-packet: generate signed self-attestation for .her
      ::
      ::    Sent by a comet on first contact with a peer.  Not acked.
      ::
      ++  ev-attestation-packet
        |=  [her=ship =her=life]
        ^-  blob
        %-  etch-shot
        %-  etch-open-packet
        :_  crypto-core.ames-state
        :*  ^=  public-key  pub:ex:crypto-core.ames-state
            ^=        sndr  our
            ^=   sndr-life  life.ames-state
            ^=        rcvr  her
            ^=   rcvr-life  her-life
        ==
      ::  +ev-sendkeys-packet: generate a request for a self-attestation.
      ::
      ::    Sent by non-comets to comets.  Not acked.
      ::
      ++  ev-sendkeys-packet
        |=  her=ship
        ^-  blob
        ?>  ?=(%pawn (clan:title her))
        %-  etch-shot
        (encode-keys-packet our her life.ames-state)
      ::
      +|  %internals
      ::  +pe: create nested |pe-core for per-peer processing
      ::
      ++  pe
        |_  [=peer-state =channel]
        +*  veb    veb.bug.channel
            her    her.channel
            keens  keens.peer-state
        ::
        +|  %helpers
        ::
        ++  pe-core  .
        ++  pe-emit  |=(move pe-core(ev-core (ev-emit +<)))
        ++  pe-abed
          |%  ++  gut  |=(=ship (her ship (ev-gut-peer-state ship)))
              ++  got  |=(=ship (her ship (ev-got-peer-state ship)))
              ++  her
                |=  [her=ship peer=^peer-state]
                pe-core(+< [peer [[our her] now ev-channel-state -.peer]])
          --
        ::
        ++  pe-abort  ev-core  :: keeps moves, discards state changes
        ++  pe-abet
          ^+  ev-core
          =.  peers.ames-state
            (~(put by peers.ames-state) her %known peer-state)
          ev-core
        ::
        ++  pe-trace
          |=  [verb=? print=(trap tape)]
          ^+  same
          (ev-trace verb her print)
        ::
        ::  +pe-got-duct: look up $duct by .bone, asserting already bound
        ::
        ++  pe-got-duct
          |=  =bone
          ^-  ^duct
          ~|  %dangling-bone^her^bone
          (~(got by by-bone.ossuary.peer-state) bone)
        ::
        ::  +pe-bind-duct: find or make new $bone for .duct in .ossuary
        ::
        ++  pe-bind-duct
          |=  =^duct
          =*  ossa  ossuary.peer-state
          ^+  [next-bone.ossa pe-core]
          ?^  existing=(~(get by by-duct.ossa) duct)
            [u.existing pe-core]
          :-  next-bone.ossa
          =.  ossa
            :+  (add 4 next-bone.ossa)
              (~(put by by-duct.ossa) duct next-bone.ossa)
            (~(put by by-bone.ossa) next-bone.ossa duct)
          pe-core
        ::
        ++  pe-is-corked
          |=  =bone
          ?|  (~(has in corked.peer-state) bone)
              ?&  =(1 (end 0 bone))
                  =(1 (end 0 (rsh 0 bone)))
                  (~(has in corked.peer-state) (mix 0b10 bone))
          ==  ==
        ::
        +|  %tasks
        ::
        ++  pe-heed
          pe-core(heeds.peer-state (~(put in heeds.peer-state) duct))
        ::
        ++  pe-jilt
          pe-core(heeds.peer-state (~(del in heeds.peer-state) duct))
        ::  +pe-update-qos: update and maybe print connection status
        ::
        ++  pe-update-qos
          |=  [mode=?(%ames %fine) =new=qos]
          ^+  pe-core
          ::
          =^  old-qos  qos.peer-state  [qos.peer-state new-qos]
          ::  if no update worth reporting, we're done
          ::
          =/  text
            %^  qos-update-text  her  mode
            [old-qos new-qos kay.veb ships.bug.ames-state]
          ?~  text
            pe-core
          ::  print message
          ::
          =.  pe-core  (pe-emit duct %pass /qos %d %flog %text u.text)
          ::  if peer has stopped responding, check if %boon's are backing up
          ::
          ?.  ?=(?(%dead %unborn) -.qos.peer-state)
            pe-core
          pe-check-clog
        ::  +pe-hear-shut-packet: handle receipt of ack or message fragment
        ::
        ++  pe-hear-shut-packet
          |=  [=lane =shut-packet dud=(unit goof)]
          ^+  pe-core
          ::  update and print connection status
          ::
          =.  pe-core  (pe-update-qos %ames %live last-contact=now)
          ::
          =/  =bone  bone.shut-packet
          ::
          ?:  ?=(%& -.meat.shut-packet)
            =+  ?.  &(?=(^ dud) msg.veb)  ~
                %.  ~
                %-  slog
                :_  tang.u.dud
                leaf+"ames: {<her>} fragment crashed {<mote.u.dud>}"
            mi-abet:(mi-call:(mi-abed:mi bone) %hear lane shut-packet ?=(~ dud))
          ::  benign ack on corked bone
          ::
          ?:  (pe-is-corked bone)  pe-core
          ::  Just try again on error, printing trace
          ::
          ::    Note this implies that vanes should never crash on %done,
          ::    since we have no way to continue using the flow if they do.
          ::
          =+  ?~  dud  ~
              %.  ~
              %+  slog  leaf+"ames: {<her>} ack crashed {<mote.u.dud>}"
              ?.  msg.veb  ~
              :-  >[bone=bone message-num=message-num meat=meat]:shut-packet<
              tang.u.dud
          =<  mu-abet
          (mu-call:(mu-abed:mu bone) %hear [message-num +.meat]:shut-packet)
        ::
        ++  pe-flub
          |=  =bone
          ^+  pe-core
          mi-abet:(mi-call:(mi-abed:mi:pe-core bone) %flub ~)
        ::  +pe-memo: handle request to send message
        ::
        ++  pe-memo
          |=  [=bone payload=* valence=?(%plea %boon)]
          ^+  pe-core
          ?:  ?&  (~(has in closing.peer-state) bone)
                  !=(payload [%$ /flow %cork ~])
              ==
            ~>  %slog.0^leaf/"ames: ignoring message on closing bone {<bone>}"
            pe-core
          ?:  (~(has in corked.peer-state) bone)
            ~>  %slog.0^leaf/"ames: ignoring message on corked bone {<bone>}"
            pe-core
          ::
          =/  =message-blob  (pe-dedup-message (jim payload))
          =.  pe-core  mu-abet:(mu-call:(mu-abed:mu bone) %memo message-blob)
          ::
          ?:  ?&  =(%boon valence)
                  (gte now (add ~s30 last-contact.qos.peer-state))
              ==
            pe-check-clog
          pe-core
        ::  +pe-wake: handle timer expiration
        ::
        ++  pe-wake
          |=  [=bone error=(unit tang)]
          ^+  pe-core
          ::  if we previously errored out, print and reset timer for later
          ::
          ::    This really shouldn't happen, but if it does, make sure we
          ::    don't brick either this messaging flow or Behn.
          ::
          ?^  error
            =.  pe-core
              (pe-emit duct %pass /wake-fail %d %flog %crud %ames-wake u.error)
            ::
            ?~  message-pump-state=(~(get by snd.peer-state) bone)
              pe-core
            =*  packet-state  packet-pump-state.u.message-pump-state
            ?~  next-wake.packet-state  pe-core
            ::  If we crashed because we woke up too early, assume another
            ::  timer is already set.
            ::
            ?:  (lth now.channel u.next-wake.packet-state)
              pe-core
            ::
            =/  =wire  (make-pump-timer-wire her bone)
            (pe-emit duct %pass wire %b %wait (add now.channel ~s30))
          ::  update and print connection state
          ::
          =.  pe-core   (pe-update-qos %ames qos:(is-peer-dead now peer-state))
          ::  expire direct route if the peer is not responding
          ::
          =.  peer-state  (update-peer-route her peer-state)
          ::  resend comet attestation packet if first message times out
          ::
          ::    The attestation packet doesn't get acked, so if we tried to
          ::    send a packet but it timed out, maybe they didn't get our
          ::    attestation.
          ::
          ::    Only resend on timeout of packets in the first message we
          ::    send them, since they should remember forever.
          ::
          =?    ev-core
              ?&  ?=(%pawn (clan:title our))
                  =(1 current:(~(got by snd.peer-state) bone))
              ==
            =/  =blob  (ev-attestation-packet [her her-life]:channel)
            (ev-send-blob for=| her blob `known/peer-state)
          ?:  (pe-is-corked bone)
            ::  no-op if the bone (or, if a naxplanation, the reference bone)
            ::  was corked, because the flow doesn't exist anymore
            ::  TODO: clean up corked bones?
            ::
            pe-core
          ::  maybe resend some timed out packets
          ::
          mu-abet:(mu-call:(mu-abed:mu bone) %wake ~)
        ::
        ++  pe-hear-fine
          |=  [=lane =shot]
          ^+  pe-core
          ?>  =(sndr-tick.shot (mod life.peer-state 16))
          ::  TODO what if the error happened in sift-purr?
          ::       does vere discard malformed packets?
          =/  [=peep =meow]  (sift-purr `@ux`content.shot)
          =/  =path  (slag 3 path.peep)
          ::
          ?.  (~(has by keens) path)
            %-  (fi-trace:fi odd.veb |.("dead-response {<peep>}"))
            pe-core
          fi-abet:(fi-rcv:(fi-abed:fi path) peep meow lane)
        ::
        ++  pe-keen
          |=  [=path =^duct]
          ^+  pe-core
          ?:  (~(has by keens) path)
            %-  (fi-trace:fi odd.veb |.("dupe {(spud path)}"))
            fi-abet:(fi-sub:(fi-abed:fi path) duct)
          =.  keens  (~(put by keens) path *keen-state)
          fi-abet:(fi-start:(fi-abed:fi path) duct)
        ::
        ++  pe-dear
          |=  =lane
          ^+  pe-core
          pe-core(route.peer-state `[%.y lane])
        ::
        ++  pe-tame
          ^+  pe-core
          pe-core(route.peer-state ~)
        ::  +pe-cork-flow: mark .bone as closing
        ::
        ++  pe-cork-flow
          |=  =bone
          ^+  pe-core
          pe-core(closing.peer-state (~(put in closing.peer-state) bone))
        ::  +pe-kill-flow: delete flow on cork sender side
        ::
        ++  pe-kill-flow
          |=  =bone
          ^+  pe-core
          ?:  (~(has in corked.peer-state) bone)
            ~>  %slog.0^leaf/"ames: ignoring kill on corked bone {<bone>}"
            pe-core
          =.  peer-state
            =,  peer-state
            %_  peer-state
              ::  if the publisher was behind, preemptively remove any nacks
              ::
              rcv              (~(del by (~(del by rcv) bone)) (mix 0b10 bone))
              snd              (~(del by snd) bone)
              corked           (~(put in corked) bone)
              closing          (~(del in closing) bone)
              by-duct.ossuary  (~(del by by-duct.ossuary) (pe-got-duct bone))
              by-bone.ossuary  (~(del by by-bone.ossuary) bone)
            ==
          ::  since we got one cork ack, try the next one
          ::
          pe-recork-one
        ::
        +|  %implementation
        ::  +pe-dedup-message: replace with any existing copy of this message
        ::
        ++  pe-dedup-message
          |=  =message-blob
          ^+  message-blob
          ?:  (lte (met 13 message-blob) 1)
            message-blob
          =/  peers-l=(list [=ship =ship-state])  ~(tap by peers.ames-state)
          |-  ^+  message-blob
          =*  peer-loop  $
          ?~  peers-l
            message-blob
          ?.  ?=(%known -.ship-state.i.peers-l)
            peer-loop(peers-l t.peers-l)
          =/  snd-l=(list [=bone =message-pump-state])
            ~(tap by snd.ship-state.i.peers-l)
          |-  ^+  message-blob
          =*  bone-loop  $
          ?~  snd-l      peer-loop(peers-l t.peers-l)
          =*  unsent-fragments  unsent-fragments.message-pump-state.i.snd-l
          =/  blob-l=(list ^message-blob)
            ~(tap to unsent-messages.message-pump-state.i.snd-l)
          |-  ^+  message-blob
          =*  blob-loop  $
          ?^  blob-l
            ?:  =(i.blob-l message-blob)
              i.blob-l
            blob-loop(blob-l t.blob-l)
          ?~  unsent-fragments  bone-loop(snd-l t.snd-l)
          ?:  =(message-blob fragment.i.unsent-fragments)
            `@`fragment.i.unsent-fragments
          bone-loop(snd-l t.snd-l)
        ::  +pe-check-clog: notify clients if peer has stopped responding
        ::
        ++  pe-check-clog
          ^+  pe-core
          ::
          ::    Only look at response bones.  Request bones are unregulated,
          ::    since requests tend to be much smaller than responses.
          ::
          =/  pumps=(list message-pump-state)
            %+  murn  ~(tap by snd.peer-state)
            |=  [=bone =message-pump-state]
            ?:  =(0 (end 0 bone))
              ~
            `u=message-pump-state
          ::  if clogged, notify client vane
          ::
          |^  ?.  &(nuf-messages nuf-memory)  pe-core
              %+  roll  ~(tap in heeds.peer-state)
              |=([d=^duct core=_pe-core] (pe-emit:core d %give %clog her))
          ::  +nuf-messages: are there enough messages to mark as clogged?
          ::
          ++  nuf-messages
            =|  num=@ud
            |-  ^-  ?
            ?~  pumps  |
            =.  num
              ;:  add  num
                (sub [next current]:i.pumps)
                ~(wyt in unsent-messages.i.pumps)
              ==
            ?:  (gte num msg.cong.ames-state)
              &
            $(pumps t.pumps)
          ::  +nuf-memory: is enough memory used to mark as clogged?
          ::
          ++  nuf-memory
            =|  mem=@ud
            |-  ^-  ?
            ?~  pumps  |
            =.  mem
              %+  add
                %-  ~(rep in unsent-messages.i.pumps)
                |=([a=@ b=_mem] (add b (met 3 a)))
              ?~  unsent-fragments.i.pumps  0
              (met 3 fragment.i.unsent-fragments.i.pumps)
            ?:  (gte mem mem.cong.ames-state)
              &
            $(pumps t.pumps)
          --
        ::  +pe-send-shut-packet: fire encrypted packet at rcvr (maybe sponsors)
        ::
        ++  pe-send-shut-packet
          |=  =shut-packet
          ^+  pe-core
          ::  swizzle last bone bit before sending
          ::
          ::    The peer has the opposite perspective from ours about what
          ::    kind of flow this is (forward/backward), so flip the bit
          ::    here.
          ::
          =.  ev-core
            %:  ev-send-blob  for=|  her
              %-  etch-shot
              %:  etch-shut-packet
                shut-packet(bone (mix 1 bone.shut-packet))
                symmetric-key.channel
                our               her
                our-life.channel  her-life.channel
              ==
            ::
              ship-state=`known/peer-state
            ==
          pe-core
        ::  +pe-recork-one: re-send the next %cork to the peer
        ::
        ++  pe-recork-one
          ^+  pe-core
          =/  boz  (sort ~(tap in closing.peer-state) lte)
          |-  ^+   pe-core
          ?~  boz  pe-core
          =/  pum=message-pump-state  (~(got by snd.peer-state) i.boz)
          ?.  =(next current):pum
            $(boz t.boz)
          ::  sanity check on the message pump state
          ::
          ?.  ?&  =(~ unsent-messages.pum)
                  =(~ unsent-fragments.pum)
                  =(~ live.packet-pump-state.pum)
              ==
            ~>  %slog.0^leaf/"ames: bad pump state {<her i.boz>}"
            $(boz t.boz)
          ::  no outstanding messages, so send a new %cork
          ::
          %-  (pe-trace |(odd.veb snd.veb) |.("recork bone={<i.boz>}"))
          (pe-memo i.boz plea=[%$ /flow [%cork ~]] %plea)
        ::  +pe-handle-cork: handle flow kill after server ames has taken %done
        ::
        ++  pe-handle-cork
          |=  =bone
          |^  ^+  pe-core
          ?.  (~(has in closing.peer-state) bone)  pe-core
          =/  pump=message-pump-state
            (~(gut by snd.peer-state) bone *message-pump-state)
          =?  ev-core  ?=(^ next-wake.packet-pump-state.pump)
            ::  reset-timer for boons
            ::
            (reset-timer her bone u.next-wake.packet-pump-state.pump)
          =/  nax-bone=^bone  (mix 0b10 bone)
          =/  nax-pump=message-pump-state
            (~(gut by snd.peer-state) nax-bone *message-pump-state)
          =?  ev-core  ?=(^ next-wake.packet-pump-state.nax-pump)
            %-  %^  ev-trace  odd.veb  her
                |.("remove naxplanation flow {<[her bone=nax-bone]>}")
            :: reset timer for naxplanations
            ::
            (reset-timer her nax-bone u.next-wake.packet-pump-state.nax-pump)
          =.  peer-state
            =,  peer-state
            %_  peer-state
              ::  preemptively delete nax flows (e.g. nacks for %watches)
              ::
              snd      (~(del by (~(del by snd) bone)) nax-bone)
              rcv      (~(del by rcv) bone)
              corked   (~(put in corked) bone)
              closing  (~(del in closing) bone)
            ==
          pe-core
          ::
          ++  reset-timer
            |=  [=ship =^bone wake=@da]
            %+  ev-emit  [/ames]~
            [%pass (make-pump-timer-wire ship bone) %b %rest wake]
          --
        ::
        +|  %internals
        ::  +mu: constructor for |pump message sender core
        ::
        ++  mu
          |_  [=bone state=message-pump-state]
          ::
          +|  %helpers
          ::
          ++  mu-core  .
          ++  mu-abed
            |=  b=^bone
            mu-core(+< [b (~(gut by snd.peer-state) b *message-pump-state)])
          ::
          ++  mu-abet
            ::  if the bone was corked, it's been removed from the state,
            ::  so we avoid adding it again.
            ::
            =?  snd.peer-state  !mu-corked
              (~(put by snd.peer-state) bone state)
            pe-core
          ::
          ++  pu-core     (pu packet-pump-state.state)
          ++  mu-closing  (~(has in closing.peer-state) bone)
          ++  mu-corked   (~(has in corked.peer-state) bone)
          ::  +mu-is-message-num-in-range: %.y unless duplicate or future ack
          ::
          ++  mu-is-message-num-in-range
            |=  =message-num
            ^-  ?
            ::
            ?:  (gte message-num next.state)
              %.n
            ?:  (lth message-num current.state)
              %.n
            !(~(has by queued-message-acks.state) message-num)
          ::
          +|  %entry-points
          ::  +mu-call: handle a $message-pump-task
          ::
          ++  mu-call
            |=  task=message-pump-task
            ^+  mu-core
            ::
            =.  mu-core  =~((mu-dispatch-task task) mu-feed-packets)
            =+  top=(pry:pu-packet-queue:pu-core live.state:pu-core)
            ::  sanity check to isolate error cases
            ::
            ?.  |(?=(~ top) (lte current.state message-num.key.u.top))
              ~|([%strange-current current=current.state key.u.top] !!)
            ::  maybe trigger a timer based on congestion control calculations
            ::
            pu-abet:(pu-call:pu-core %halt ~)
          ::
          +|  %tasks
          ::  +mu-dispatch-task: perform task-specific processing
          ::
          ++  mu-dispatch-task
            |=  task=message-pump-task
            ^+  mu-core
            ::
            ?-  -.task
              %memo  (mu-memo message-blob.task)
              %prod  pu-abet:(pu-call:pu-core %prod ~)
              %wake  pu-abet:(pu-call:pu-core %wake current.state)
              %near  %-  mu-done
                     [[message-num %naxplanation error]:naxplanation.task %&]
              ::
              %hear
                ?-    -.ack-meat.task
                    %&  (mu-hear [message-num fragment-num=p.ack-meat]:task)
                ::
                    %|
                  =/  cork=?
                    =+  top=(pry:pu-packet-queue:pu-core live.state:pu-core)
                    ::  If we send a %cork and get an ack, we can know by
                    ::  sequence number that the ack is for the %cork message
                    ::
                    ?&  mu-closing
                        ?=(^ top)
                        =(0 ~(wyt in unsent-messages.state))
                        =(0 (lent unsent-fragments.state))
                        =(1 ~(wyt by live.packet-pump-state.state))
                        =(message-num:task message-num.key.u.top)
                    ==
                  =+  [ack msg]=[p.ack-meat message-num]:task
                  =.  mu-core
                    %-  mu-done
                    [[msg ?:(ok.ack [%ok ~] [%nack ~])] cork]
                  ?.  &(!ok.ack cork)  mu-core
                  %-  %+  pe-trace  odd.veb
                      |.("got nack for %cork {<bone=bone message-num=msg>}")
                  mu-core
            ==  ==
          ::  +mu-memo: handle request to send a message
          ::
          ++  mu-memo
            |=  lob=message-blob
            mu-core(unsent-messages.state (~(put to unsent-messages.state) lob))
          ::  +mu-hear: handle packet acknowledgment
          ::
          ++  mu-hear
            |=  [=message-num =fragment-num]
            ^+  mu-core
            ::  pass to |packet-pump unless duplicate or future ack
            ::
            ?.  (mu-is-message-num-in-range message-num)
              %-  (pe-trace snd.veb |.("hear pump out of range"))
              mu-core
            pu-abet:(pu-call:pu-core %hear message-num fragment-num)
          ::  +mu-done: handle message acknowledgment
          ::
          ::    A nack-trace message counts as a valid message nack on the
          ::    original failed message.
          ::
          ::    This prevents us from having to wait for a message nack packet,
          ::    which would mean we couldn't immediately ack the nack-trace
          ::    message, which would in turn violate the semantics of backward
          ::    flows.
          ::
          ++  mu-done
            |=  [[=message-num =ack] cork=?]
            ^+  mu-core
            ::  unsent messages from the future should never get acked
            ::
            ~|  :*  bone=bone
                    mnum=message-num
                    next=next.state
                    unsent-messages=~(wyt in unsent-messages.state)
                    unsent-fragments=(lent unsent-fragments.state)
                    any-live=!=(~ live.packet-pump-state.state)
                ==
            ?>  (lth message-num next.state)
            ::  ignore duplicate message acks
            ::
            ?:  (lth message-num current.state)
              %-  %+  pe-trace  snd.veb  |.
                  "duplicate done {<current=current.state num=message-num>}"
              mu-core
            ::  ignore duplicate and future acks
            ::
            ?.  (mu-is-message-num-in-range message-num)
              mu-core
            ::  clear and print .unsent-fragments if nonempty
            ::
            =?    unsent-fragments.state
                &(=(current next) ?=(^ unsent-fragments)):state
              ::
              ~>  %slog.0^leaf/"ames: early message ack {<her>}"
              ~
            ::  clear all packets from this message from the packet pump
            ::
            =.  mu-core  pu-abet:(pu-call:pu-core %done message-num lag=*@dr)
            ::  enqueue this ack to be sent back to local client vane
            ::
            ::    Don't clobber a naxplanation with just a nack packet.
            ::
            =?    queued-message-acks.state
                =/  old  (~(get by queued-message-acks.state) message-num)
                !?=([~ %naxplanation *] old)
              (~(put by queued-message-acks.state) message-num ack)
            ::  emit local acks from .queued-message-acks until incomplete
            ::
            |-  ^+  mu-core
            ::  if .current hasn't been fully acked, we're done
            ::
            ?~  cur=(~(get by queued-message-acks.state) current.state)
              mu-core
            ::  .current is complete; pop, emit local ack, and try next message
            ::
            =.  queued-message-acks.state
              (~(del by queued-message-acks.state) current.state)
            ::  clear all packets from this message from the packet pump
            ::
            ::    Note we did this when the original packet came in, a few lines
            ::    above.  It's not clear why, but it doesn't always clear the
            ::    packets when it's not the current message.  As a workaround,
            ::    we clear the packets again when we catch up to this packet.
            ::
            ::    This is slightly inefficient because we run this twice for
            ::    each packet and it may emit a few unnecessary packets, but
            ::    it's not incorrect.  pump-metrics are updated only once,
            ::    at the time when we actually delete the packet.
            ::
            =.  mu-core  pu-abet:(pu-call:pu-core %done current.state lag=*@dr)
            ::  give %done to vane if we're ready
            ::
            ?-    -.u.cur
                %ok
              =.  pe-core
                ::  don't give %done for corks
                ::
                ?:  cork  (mu-pump-cork current.state)
                (mu-pump-done current.state ~)
              $(current.state +(current.state))
            ::
                %nack  mu-core
            ::
                %naxplanation
              =.  pe-core  (mu-pump-done current.state `error.u.cur)
              $(current.state +(current.state))
            ==
          ::
          +|  %implementation
          ::  +mu-feed-packets: give packets to |packet-pump until full
          ::
          ++  mu-feed-packets
            ::  if nothing to send, no-op
            ::
            ?:  &(=(~ unsent-messages) =(~ unsent-fragments)):state
              mu-core
            ::  we have unsent fragments of the current message; feed them
            ::
            ?.  =(~ unsent-fragments.state)
              ::  we have unsent fragments of the current message; feed them
              ::
              =^  unsent  mu-core  =<  pu-abut
                (pu-feed:pu-core unsent-fragments.state)
              =.  unsent-fragments.state   unsent
              ::  if it sent all of them, feed it more; otherwise, we're done
              ::
              ?~(unsent mu-feed-packets mu-core)
            ::  .unsent-messages is nonempty; pop a message off and feed it
            ::
            =^  =message-blob  unsent-messages.state
              ~(get to unsent-messages.state)
            ::  break .message into .chunks and set as .unsent-fragments
            ::
            =.  unsent-fragments.state  (split-message next.state message-blob)
            ::  try to feed packets from the next message
            ::
            =.  next.state  +(next.state)
            mu-feed-packets
          ::  +mu-pump-done: handle |message-pump's report of message (n)ack
          ::
          ++  mu-pump-done
            |=  [=message-num error=(unit error)]
            ^+  pe-core
            ?:  ?&  =(1 (end 0 bone))
                    =(1 (end 0 (rsh 0 bone)))
                    %*(mu-corked mu-core bone (mix 0b10 bone))
                ==
              %-  %+  pe-trace  msg.veb
                  =/  dat  [her bone=bone message-num=message-num -.task]
                  |.("remove naxplanation flow {<dat>}")
              ::  we avoid re-adding the bone in mu-abet:mu
              ::
              =.  snd.peer-state  (~(del by snd.peer-state) bone)
              pe-core
            ?:  =(1 (end 0 bone))
              ::  ack is on "subscription update" message; no-op
              ::
              ?:  =(0 (end 0 (rsh 0 bone)))  pe-core
              ::  nack-trace bone; assume .ok, clear nack from |sink
              ::
              %+  pe-emit  duct
              [%pass /clear-nack %a %deep %drop her (mix 0b10 bone) message-num]
            ::  if the bone belongs to a closing flow and we got a
            ::  naxplanation, don't relay ack to the client vane
            ::
            ?:  &(mu-closing ?=(%near -.task))  pe-core
            ::  not a nack-trace bone; relay ack to client vane
            ::
            (pe-emit (pe-got-duct bone) %give %done error)
          ::  +mu-pump-cork: handle %cork on the publisher
          ::
          ++  mu-pump-cork
            |=  =message-num
            ^+  pe-core
            ::  clear all packets from this message from the packet pump
            ::
            =.  mu-core  pu-abet:(pu-call:pu-core %done message-num lag=*@dr)
            ?:  mu-corked
              %-  %+  pe-trace  odd.veb
                  |.("trying to delete a corked bone={<bone>}")
              pe-core
            =/  =wire  (make-bone-wire her her-rift.channel bone)
            (pe-emit duct %pass wire %a %deep %kill her bone)
          ::  +pu: construct |packet-pump core
          ::
          ++  pu
            |=  state=packet-pump-state
            ::
            =|  unsent=(list static-fragment)
            |%
            +|  %helpers
            ++  pu-core  .
            ::  +pu-abut: abet with gifts
            ::
            ++  pu-abut  [unsent pu-abet]
            ++  pu-abet  mu-core(packet-pump-state.state state)
            ++  pu-trace
              |=  [verb=? print=(trap tape)]
              ^+  same
              (trace %ames verb her ships.bug.channel print)
            ::
            ++  pu-wire  (make-pump-timer-wire her bone)
            ++  pu-emit  |=(=note (pe-emit ~[/ames] %pass pu-wire note))
            ::  +pu-packet-queue: type for sent fragments (order: seq number)
            ::
            ++  pu-packet-queue
              %-  (ordered-map live-packet-key live-packet-val)
              lte-packets
            ::  +pu-gauge: inflate a |pump-gauge to track congestion control
            ::
            ++  pu-gauge  (ga metrics.state ~(wyt by live.state))
            ::  +pu-to-static-fragment: convenience function for |packet-pump
            ::
            ++  pu-to-static-fragment
              |=  [live-packet-key live-packet-val]
              ^-  static-fragment
              [message-num num-fragments fragment-num fragment]
            ::            ::
            +|  %entry-points
            ::
            ++  pu-call
              |=  task=packet-pump-task
              ^+  pu-core
              ?-  -.task
                %hear  (pu-hear [message-num fragment-num]:task)
                %done  (pu-done message-num.task)
                %wake  (pu-wake current.task)
                %prod  pu-prod
                %halt  pu-set-wake
              ==
            ::  +feed: try to send a list of packets, returning unsent ones
            ::
            ++  pu-feed
              |=  fragments=(list static-fragment)
              ^+  pu-core
              ::  bite off as many fragments as we can send
              ::
              =/  num-slots  ga-num-slots:pu-gauge
              =/  sent       (scag num-slots fragments)
              =.  unsent     (slag num-slots fragments)
              ::  if nothing to send, we're done
              ::
              ?~  sent  pu-core
              ::  convert $static-fragment's into +ordered-set [key val] pairs
              ::
              =/  send-list
                %+  turn  sent
                |=  static-fragment
                ^-  [key=live-packet-key val=live-packet-val]
                ::
                :-  [message-num fragment-num]
                :-  [sent-date=now.channel tries=1 skips=0]
                [num-fragments fragment]
              ::  update .live and .metrics
              ::
              =.  live.state  (gas:pu-packet-queue live.state send-list)
              ::  TMI
              ::
              =>  .(sent `(list static-fragment)`sent)
              ::  emit a $shut-packet for each packet to send
              ::
              =.  pe-core
                %+  roll  sent
                |=  [packet=static-fragment core=_pe-core]
                (pe-send-shut-packet bone [message-num %& +]:packet)
              pu-core
            ::
            +|  %tasks
            ::  +pu-prod: reset congestion control, re-send packets
            ::
            ++  pu-prod
              ^+  pu-core
              ?:  =(~ next-wake.state)
                pu-core
              ::
              =.  metrics.state
                %*(. *pump-metrics counter counter.metrics.state)
              =.  live.state
                %+  run:pu-packet-queue  live.state
                |=(p=live-packet-val p(- *packet-state))
              ::
              =/  sot  (max 1 ga-num-slots:pu-gauge)
              =/  liv  live.state
              |-  ^+  pu-core
              ?:  =(0 sot)  pu-core
              ?:  =(~ liv)  pu-core
              =^  hed  liv  (pop:pu-packet-queue liv)
              =.  pe-core
                %+  pe-send-shut-packet  bone
                [message-num %& +]:(pu-to-static-fragment hed)
              $(sot (dec sot))
            ::  +pu-wake: handle packet timeout
            ::
            ++  pu-wake
              |=  current=message-num
              ^+  pu-core
              ::  assert temporal coherence
              ::
              ?<  =(~ next-wake.state)
              =.  next-wake.state  ~
              ::  tell congestion control a packet timed out
              ::
              =.  metrics.state  ga-timeout:pu-gauge
              =|  acc=(unit static-fragment)
              ::  re-send first packet and update its state in-place
              ::
              =;  [static-fragment=_acc live=_live.state]
                  =.  live.state   live
                  =?  pe-core  ?=(^ static-fragment)
                    %-  %+  pu-trace  snd.veb
                        =/  nums  [message-num fragment-num]:u.static-fragment
                        |.("dead {<nums show:pu-gauge>}")
                    %+  pe-send-shut-packet  bone
                    [message-num %& +]:u.static-fragment
                  pu-core
              ::
              %^  (dip:pu-packet-queue _acc)  live.state  acc
              |=  $:  acc=_acc
                      key=live-packet-key
                      val=live-packet-val
                  ==
              ^-  [new-val=(unit live-packet-val) stop=? _acc]
              ::  if already acked later message, don't resend
              ::
              ?:  (lth message-num.key current)
                %-  %-  slog  :_  ~  :-  %leaf
                   "ames: strange wake queue, expected {<current>}, got {<key>}"
                [~ stop=%.n ~]
              ::  packet has expired; update it in-place, stop, and produce it
              ::
              =.  last-sent.val  now.channel
              =.  tries.val      +(tries.val)
              ::
              [`val stop=%.y `(pu-to-static-fragment key val)]
            ::  +pu-fast-resend-after-ack: resend timed out packets
            ::
            ::    After we finally receive an ack, we want to resend all the
            ::    live packets that have been building up.
            ::
            ++  pu-fast-resend-after-ack
              |=  [=message-num =fragment-num]
              ^+  pu-core
              =;  res=[resends=(list static-fragment) live=_live.state]
                =.  live.state  live.res
                =.  pe-core
                  %+  reel  resends.res
                  |=  [packet=static-fragment core=_pe-core]
                  (pe-send-shut-packet bone [message-num %& +]:packet)
                pu-core
              ::
              =/  acc
                resends=*(list static-fragment)
              ::
              %^  (dip:pu-packet-queue _acc)  live.state  acc
              |=  $:  acc=_acc
                      key=live-packet-key
                      val=live-packet-val
                  ==
              ^-  [new-val=(unit live-packet-val) stop=? _acc]
              ?:  (lte-packets key [message-num fragment-num])
                [new-val=`val stop=%.n acc]
              ::
              ?:  (gth (ga-next-expiry:pu-gauge -.val) now.channel)
                [new-val=`val stop=%.y acc]
              ::
              =.  last-sent.val  now.channel
              =.  resends.acc  [(pu-to-static-fragment key val) resends.acc]
              [new-val=`val stop=%.n acc]
            ::  +pu-hear: handle ack on a live packet
            ::
            ::    If the packet was in our queue, delete it and update our
            ::    metrics, possibly re-sending skipped packets. Otherwise, no-op
            ::
            ++  pu-hear
              |=  [=message-num =fragment-num]
              ^+  pu-core
              ::
              =-  ::  if no sent packet matches the ack,
                  ::  don't apply mutations or effects
                  ::
                  ?.  found.-
                    %-  (pu-trace snd.veb |.("miss {<show:pu-gauge>}"))
                    pu-core
                  ::
                  =.  metrics.state  metrics.-
                  =.  live.state     live.-
                  %-  ?.  ?|  =(0 fragment-num)
                              =(0 (mod counter.metrics.state 20))
                          ==
                        same
                      %+  pu-trace  snd.veb
                      |.("send: {<fragment=fragment-num show:pu-gauge>}")
                  ::  .resends is backward, so fold backward and emit
                  ::
                  =.  pe-core
                    %+  reel  resends.-
                    |=  [packet=static-fragment core=_pe-core]
                    (pe-send-shut-packet bone [message-num %& +]:packet)
                  (pu-fast-resend-after-ack message-num fragment-num)
              ::
              =/  acc
                :*  found=`?`%.n
                    resends=*(list static-fragment)
                    metrics=metrics.state
                    num-live=~(wyt by live.state)
                ==
              ::
              ^+  [acc live=live.state]
              ::
              %^  (dip:pu-packet-queue _acc)  live.state  acc
              |=  $:  acc=_acc
                      key=live-packet-key
                      val=live-packet-val
                  ==
              ^-  [new-val=(unit live-packet-val) stop=? _acc]
              ::
              =/  ga-core  (ga [metrics num-live]:acc)
              ::  is this the acked packet?
              ::
              ?:  =(key [message-num fragment-num])
                ::  delete acked packet, update metrics, and stop traversal
                ::
                =.     found.acc  %.y
                =.   metrics.acc  (ga-on-ack:ga-core -.val)
                =.  num-live.acc  (dec num-live.acc)
                [new-val=~ stop=%.y acc]
              ::  is this a duplicate ack?
              ::
              ?.  (lte-packets key [message-num fragment-num])
                ::  stop, nothing more to do
                ::
                [new-val=`val stop=%.y acc]
              ::  ack was on later packet; mark skipped, tell gauge, & continue
              ::
              =.  skips.val  +(skips.val)
              =^  resend  metrics.acc  (ga-skipped-packet:ga-core -.val)
              ?.  resend
                [new-val=`val stop=%.n acc]
              ::
              =.  last-sent.val  now.channel
              =.  tries.val      +(tries.val)
              =.  resends.acc    [(pu-to-static-fragment key val) resends.acc]
              [new-val=`val stop=%.n acc]
            ::  +pu-done: apply ack to all packets from .message-num
            ::
            ++  pu-done
              |=  =message-num
              ^+  pu-core
              ::
              =-  =.  metrics.state  metrics.-
                  =.  live.state     live.-
                  ::
                  %-  %+  pu-trace  snd.veb  |.
                      "done {<message-num=message-num show:pu-gauge>}"
                  (pu-fast-resend-after-ack message-num `fragment-num`0)
              ::
              =/  acc  [metrics=metrics.state num-live=~(wyt by live.state)]
              ::
              ^+  [acc live=live.state]
              ::
              %^  (dip:pu-packet-queue _acc)  live.state  acc
              |=  $:  acc=_acc
                      key=live-packet-key
                      val=live-packet-val
                  ==
              ^-  [new-val=(unit live-packet-val) stop=? _acc]
              ::
              =/  ga-core  (ga [metrics num-live]:acc)
              ::  if we get an out-of-order ack for a message, skip until it
              ::
              ?:  (lth message-num.key message-num)
                [new-val=`val stop=%.n acc]
              ::  if packet was from acked message, delete it and continue
              ::
              ?:  =(message-num.key message-num)
                =.   metrics.acc  (ga-on-ack:ga-core -.val)
                =.  num-live.acc  (dec num-live.acc)
                [new-val=~ stop=%.n acc]
              ::  we've gone past the acked message; we're done
              ::
              [new-val=`val stop=%.y acc]
            ::  +set-wake: set, unset, or reset timer, emitting moves
            ::
            ++  pu-set-wake
              ^+  pu-core
              ::  if nonempty .live, pry at head to get next wake time
              ::
              =/  new-wake=(unit @da)
                ?~  head=(pry:pu-packet-queue live.state)
                  ~
                `(ga-next-expiry:pu-gauge -.val.u.head)
              ::  no-op if no change
              ::
              ?:  =(new-wake next-wake.state)  pu-core
              ::  unset old timer if non-null
              ::
              =?  pe-core  !=(~ next-wake.state)
                (pu-emit %b %rest (need next-wake.state))
              ::  set new timer if non-null and not at at max-backoff
              ::
              ::  we are using the ~m2 literal instead of ga-max-backoff:ga-core
              ::  because /app/ping has a special cased maximum backoff of ~s25
              ::  and we don't want to consolidate that
              ::
              =?  pe-core  ?=(^ new-wake)
                ?:  ?&(?=(^ +.flow.dead.ames-state) =(~m2 rto.metrics.state))
                  pe-core
                (pu-emit %b %wait u.new-wake)
              ::
              =?  next-wake.state  !=(~ next-wake.state)   ~  ::  unset
              =?  next-wake.state  ?=(^ new-wake)   new-wake  ::  reset
              ::
              pu-core
            --
          --
        ::  +mi: constructor for |sink message receiver core
        ::
        ++  mi
          |_  [=bone state=message-sink-state]
          ::
          +|  %helpers
          ::
          ++  mi-core  .
          ++  mi-abed
            |=  b=^bone
            mi-core(+< [b (~(gut by rcv.peer-state) b *message-sink-state)])
          ::
          ++  mi-abet
            ::  if the bone was corked, it's been removed from the state,
            ::  so we avoid adding it again.
            ::
            =?  rcv.peer-state  !mi-corked
              (~(put by rcv.peer-state) bone state)
            pe-core
          ::
          ++  mi-closing  (~(has in closing.peer-state) bone)
          ++  mi-corked   (~(has in corked.peer-state) bone)
          ++  mi-received
            |=  =^bone
            ::    odd bone:                %plea request message
            ::    even bone, 0 second bit: %boon response message
            ::    even bone, 1 second bit: nack-trace %boon message
            ::
            ?:  =(1 (end 0 bone))          %plea
            ?:  =(0 (end 0 (rsh 0 bone)))  %boon
            %nack
          ::
          +|  %entry-points
          ::  +call: handle a $message-sink-task
          ::
          ++  mi-call
            |=  task=message-sink-task
            ^+  mi-core
            ?-    -.task
                %drop  mi-core(nax.state (~(del in nax.state) message-num.task))
                %done  (mi-done ok.task)
                %flub
              %_  mi-core
                last-heard.state        (dec last-heard.state)
                pending-vane-ack.state  ~(nap to pending-vane-ack.state)
              ==
            ::
                %hear
              |^  ?:  ?|  mi-corked
                      ?&  %*(mi-corked mi-core bone (mix 0b10 bone))
                          =(%nack (mi-received bone))
                  ==  ==
                ack-on-corked-bone
              ::
              ?>  ?=(%& -.meat.shut-packet.task)
              =+  [num-fragments fragment-num fragment]=+.meat.shut-packet.task
              ?:  &(=(num-fragments 1) =(fragment-num 0))
                (check-pending-acks fragment)
              (mi-hear [lane shut-packet ok]:task)
              ::
              ++  ack-on-corked-bone
                ::  if we %hear a fragment on a corked bone, always ack
                ::
                =.  pe-core
                  %+  pe-send-shut-packet  bone
                  [message-num.shut-packet.task %| %| ok=& lag=*@dr]
                %-  %+  pe-trace  odd.veb
                    |.("hear {<(mi-received bone)>} on corked bone={<bone>}")
                mi-core
              ::
              ++  check-pending-acks
                ::  if this is a %cork %plea and we are still waiting to
                ::  hear %acks for previous naxplanations we sent, no-op
                ::
                |=  frag=@uw
                ^+  mi-core
                =/  blob=*  (cue (rep packet-size [frag]~))
                =+  mu-core=(mu-abed:mu (mix 0b10 bone))
                ?.  ?&  ?=(^ ;;((soft [%$ path %cork ~]) blob))
                        ?=(^ live.packet-pump-state.state.mu-core)
                    ==
                  (mi-hear [lane shut-packet ok]:task)
                %-  %+  pe-trace  odd.veb
                    |.("pending ack for naxplanation, skip %cork bone={<bone>}")
                mi-core
              --
            ==
          ::
          +|  %tasks
          ::  +hear: receive message fragment, possibly completing message
          ::
          ++  mi-hear
            |=  [=lane =shut-packet ok=?]
            ^+  mi-core
            ::  we know this is a fragment, not an ack; expose into namespace
            ::
            ?>  ?=(%& -.meat.shut-packet)
            =+  [num-fragments fragment-num fragment]=+.meat.shut-packet
            ::  seq: message sequence number, for convenience
            ::
            =/  seq  message-num.shut-packet
            ::  ignore messages from far future; limit to 10 in progress
            ::
            ?:  (gte seq (add 10 last-acked.state))
              %-  %+  pe-trace  odd.veb
                  |.("future %hear {<seq=seq last-acked=last-acked.state>}")
              mi-core
            ::
            =/  is-last-fragment=?  =(+(fragment-num) num-fragments)
            ::  always ack a dupe!
            ::
            ?:  (lte seq last-acked.state)
              ?.  is-last-fragment
                ::  single packet ack
                ::
                =.  pe-core  (pe-send-shut-packet bone seq %| %& fragment-num)
                %-  %+  pe-trace  rcv.veb
                    |.("send dupe ack {<seq=seq^fragment-num=fragment-num>}")
                mi-core
              ::  whole message (n)ack
              ::
              =/     ok=?  !(~(has in nax.state) seq)
              =.  pe-core  (pe-send-shut-packet bone seq %| %| ok lag=`@dr`0)
              %-  %+  pe-trace  rcv.veb
                  |.("send dupe message ack {<seq=seq>} ok={<ok>}")
              mi-core
            ::  last-acked<seq<=last-heard; heard message, unprocessed
            ::
            ::    Only true if we've heard some packets we haven't acked, which
            ::    doesn't happen for boons.
            ::
            ?:  (lte seq last-heard.state)
              ?:  &(is-last-fragment !mi-closing)
                ::  if not from a closing bone, drop last packet,
                ::  since we don't know whether to ack or nack
                ::
                %.  mi-core
                %+  pe-trace  rcv.veb
                |.  ^-  tape
                =/  data
                  :*  her  seq=seq  bone=bone.shut-packet
                      fragment-num  num-fragments
                      la=last-acked.state  lh=last-heard.state
                  ==
                "hear last in-progress {<data>}"
              ::  ack all other packets
              ::
              =.  pe-core  (pe-send-shut-packet bone seq %| %& fragment-num)
              %-  %+  pe-trace  rcv.veb  |.
                  =/  data
                    :*  seq=seq  fragment-num=fragment-num
                        num-fragments=num-fragments  closing=mi-closing
                    ==
                  "send ack-1 {<data>}"
              mi-core
            ::  last-heard<seq<10+last-heard; this is a packet in a live message
            ::
            =/  =partial-rcv-message
              ::  create default if first fragment
              ::
              ?~  existing=(~(get by live-messages.state) seq)
                [num-fragments num-received=0 fragments=~]
              ::  we have an existing partial message; check parameters match
              ::
              ?>  (gth num-fragments.u.existing fragment-num)
              ?>  =(num-fragments.u.existing num-fragments)
              ::
              u.existing
            ::
            =/  already-heard-fragment=?
              (~(has by fragments.partial-rcv-message) fragment-num)
            ::  ack dupes except for the last fragment, in which case drop
            ::
            ?:  already-heard-fragment
              ?:  is-last-fragment
                %.  mi-core
                %+  pe-trace  rcv.veb  |.
                =/  data
                  [her seq=seq lh=last-heard.state la=last-acked.state]
                "hear last dupe {<data>}"
              =.  pe-core  (pe-send-shut-packet bone seq %| %& fragment-num)
              %-  %+  pe-trace  rcv.veb
                  |.("send dupe ack {<her^seq=seq^fragment-num=fragment-num>}")
              mi-core
            ::  new fragment; store in state and check if message is done
            ::
            =.  num-received.partial-rcv-message
              +(num-received.partial-rcv-message)
            ::
            =.  fragments.partial-rcv-message
              (~(put by fragments.partial-rcv-message) fragment-num fragment)
            ::
            =.  live-messages.state
              (~(put by live-messages.state) seq partial-rcv-message)
            ::  ack any packet other than the last one, and continue either way
            ::
            =?  pe-core  !is-last-fragment
              %-  %+  pe-trace  rcv.veb  |.
                  =/  data
                    [seq=seq fragment-num=fragment-num fragments=num-fragments]
                  "send ack-2 {<data>}"
              (pe-send-shut-packet bone seq %| %& fragment-num)
            ::  enqueue all completed messages starting at +(last-heard.state)
            ::
            |-  ^+  mi-core
            ::  if this is not the next message to ack, we're done
            ::
            ?.  =(seq +(last-heard.state))
              mi-core
            ::  if we haven't heard anything from this message, we're done
            ::
            ?~  live=(~(get by live-messages.state) seq)
              mi-core
            ::  if the message isn't done yet, we're done
            ::
            ?.  =(num-received num-fragments):u.live
              mi-core
            ::  we have whole message; update state, assemble, and send to vane
            ::
            =.  last-heard.state     +(last-heard.state)
            =.  live-messages.state  (~(del by live-messages.state) seq)
            ::
            %-  %+  pe-trace  msg.veb
                |.("hear {<her>} {<seq=seq>} {<num-fragments.u.live>}kb")
            =/  message=*  (assemble-fragments [num-fragments fragments]:u.live)
            =/  empty=?    =(~ pending-vane-ack.state)
            ::  enqueue message to be sent to local vane
            ::
            =.  pending-vane-ack.state
              (~(put to pending-vane-ack.state) seq message)
            ::
            =?  mi-core  empty  (mi-handle-sink seq message ok)
            ::
            $(seq +(seq))
          ::  +mi-done: handle confirmation of message processing from vane
          ::
          ++  mi-done
            |=  ok=?
            ^+  mi-core
            ::
            =^  pending  pending-vane-ack.state
              ~(get to pending-vane-ack.state)
            =/  =message-num  message-num.p.pending
            ::
            =.  last-acked.state  +(last-acked.state)
            =?  nax.state  !ok  (~(put in nax.state) message-num)
            ::
            =.  pe-core
              (pe-send-shut-packet bone message-num %| %| ok lag=`@dr`0)
            ?~  next=~(top to pending-vane-ack.state)
              mi-core
            (mi-handle-sink message-num.u.next message.u.next ok)
          ::
          +|  %implementation
          ::  +mi-handle-sink: dispatch message
          ::
          ++  mi-handle-sink
            |=  [=message-num message=* ok=?]
            ^+  mi-core
            |^
            ?-((mi-received bone) %plea ha-plea, %boon ha-boon, %nack ha-nack)
            ::
            ++  ha-plea
              ^+  mi-core
              ?:  |(mi-closing mi-corked)  mi-core
              %-  %+  pe-trace  msg.veb
                  =/  dat  [her bone=bone message-num=message-num]
                  |.("sink plea {<dat>}")
              ?.  ok
                =/  nack-bone=^bone  (mix 0b10 bone)
                =/  =message-blob    (jam [message-num *error])
                =/  =wire  (make-bone-wire her her-rift.channel nack-bone)
                ::  send nack-trace with blank .error for security
                ::
                =.  pe-core
                  %+  pe-emit  duct
                  [%pass wire %a %deep %nack her nack-bone message-blob]
                ::
                (mi-done ok=%.n)
              ::
              =/  =wire  (make-bone-wire her her-rift.channel bone)
              =.  pe-core
                =+  ;;  =plea  message
                ?.  =(vane.plea %$)
                  ?+  vane.plea  ~|  %ames-evil-vane^our^her^vane.plea  !!
                    %c  (pe-emit duct %pass wire %c %plea her plea)
                    %e  (pe-emit duct %pass wire %e %plea her plea)
                    %g  (pe-emit duct %pass wire %g %plea her plea)
                    %j  (pe-emit duct %pass wire %j %plea her plea)
                  ==
                ::  a %cork plea is handled using %$ as the recipient vane to
                ::  account for publishers that still handle ames-to-ames %pleas
                ::
                ?>  &(?=([%cork *] payload.plea) ?=(%flow -.path.plea))
                (pe-emit duct %pass wire %a %deep %cork her bone)
              mi-core
            ::
            ::  +ha-boon: handle response message, acking unconditionally
            ::
            ::    .bone must be mapped in .ossuary.peer-state, or we crash.
            ::    This means a malformed message will kill a flow.  We
            ::    could change this to a no-op if we had some sort of security
            ::    reporting.
            ::
            ::    Note that if we had several consecutive packets in the queue
            ::    and crashed while processing any of them, the %hole card
            ::    will turn *all* of them into losts/nacks.
            ::
            ::    TODO: This handles a previous crash in the client vane, but
            ::    not in %ames itself.
            ::
            ++  ha-boon
              ^+  mi-core
              ?:  |(mi-closing mi-corked)  mi-core
              %-  %+  pe-trace  msg.veb  |.
                  ::  XX -.task not visible, FIXME
                  ::
                  =/  dat  [her bone=bone message-num=message-num]
                  ?:(ok "sink boon {<dat>}" "crashed on sink boon {<dat>}")
              =.  pe-core  (pe-emit (pe-got-duct bone) %give %boon message)
              =?  moves  !ok
                ::  we previously crashed on this message; notify client vane
                ::
                %+  turn  moves
                |=  =move
                ?.  ?=([* %give %boon *] move)  move
                [duct.move %give %lost ~]
              ::  send ack unconditionally
              ::
              (mi-done ok=%.y)
            ::
            ++  ha-nack
              ^+  mi-core
              ::  if we get a naxplanation for a %cork, the publisher hasn't
              ::  received the OTA. The /recork timer will retry eventually.
              ::
              %-  %+  pe-trace  msg.veb
                  =/  dat  [her bone=bone message-num=message-num]
                  |.("sink naxplanation {<dat>}")
              ::  flip .bone's second bit to find referenced flow
              ::
              =/  target=^bone  (mix 0b10 bone)
              =.  pe-core
                ::  will notify |message-pump that this message got naxplained
                ::
                =/  =wire  (make-bone-wire her her-rift.channel target)
                %+  pe-emit  duct
                [%pass wire %a %deep %sink her target ;;(naxplanation message)]
              ::  ack nack-trace message (only applied if we don't later crash)
              ::
              (mi-done ok=%.y)
            --
          --
        ::  +fi: constructor for |fine remote scry core
        ::
        ++  fi
          =>  |%
              ::  TODO: move +etch-peep/+etch-wail to %lull?
              ::
              ++  etch-peep
                |=  peep
                ^-  @
                ?>  (lth num ^~((bex 32)))
                =+  (spit path)
                %+  can  3
                :~  4^num       ::  fragment number
                    2^wid       ::  path size
                    wid^`@`pat  ::  namespace path
                ==
              ::
              ++  etch-wail
                |=  w=wail
                ^-  @
                ?-  -.w
                  %0  (lsh 3 (etch-peep +.w))  :: tag byte
                ==
              ::
              ++  make-shot
                |=  w=wail
                ^-  shot
                =/  sic  (mod life.ames-state 16)
                =/  ric  (mod life.peer-state 16)
                [[our her] req=& sam=| sic ric ~ (etch-wail w)]
              ::
              ::
              ++  keys
                |%
                ++  mess
                  |=  [=ship life=@ud =path dat=$@(~ (cask))]
                  (jam +<)
                ::
                ++  sign  sigh:as:crypto-core.ames-state
                ::
                ++  veri-fra
                  |=  [=path fra=@ud dat=@ux sig=@]
                  (veri sig (jam path fra dat))
                ::
                ++  veri
                  |=  [sig=@ dat=@]
                  ^-  ?
                  (safe:as:(com:nu:crub:crypto public-key.peer-state) sig dat)
                ::
                ++  meri
                  |=  [pax=path sig=@ dat=$@(~ (cask))]
                  (veri sig (mess her life.peer-state pax dat))
                --
              --
          ::
          |_  [=path keen=keen-state]
          ::
          +|  %helpers
          ::
          ++  fi-core  .
          ++  fi-abed
            |=  =^path
            ~|(no-keen-for-path/path fi-core(+< [path (~(got by keens) path)]))
          ::
          ++  fi-abet
            ^+  pe-core
            ?.  =,  keen
                ::  num-fragments is 0 when unknown (i.e. no response yet)
                ::  if no-one is listening, kill request
                ::
                ?|  =(~ listeners.keen)
                    &(!=(0 num-fragments) =(num-fragments num-received))
                ==
              =.  fi-core   fi-set-wake
              pe-core(keens.peer-state (~(put by keens) path keen))  :: XX tack.keens
            ::
            =?  fi-core  ?=(^ next-wake.keen)
              (fi-rest u.next-wake.keen)
            pe-core(keens.peer-state (~(del by keens) path))  :: XX tack.keens
          ::
          ++  fi-full-path
            :^    (scot %p her)
                (scot %ud rift.peer-state)
              (scot %ud life.peer-state)
            path
          ::
          ++  fi-show
            =,  keen
            :*  nex=(lent nex)
                hav=(lent hav)
                num-fragments=num-fragments
                num-received=num-received
                next-wake=next-wake
                metrics=metrics
            ==
          ::
          ++  fi-trace
            |=  [verb=? print=(trap tape)]
            ^+  same
            (trace %fine verb her ships.bug.ames-state print)
          ::
          ++  fi-emit   |=(move fi-core(ev-core (ev-emit +<)))
          ++  fi-mop    ((on @ud want) lte)
          ++  fi-gauge  (ga metrics.keen (wyt:fi-mop wan.keen))
          ++  fi-wait   |=(tim=@da (fi-pass-timer %b %wait tim))
          ++  fi-rest   |=(tim=@da (fi-pass-timer %b %rest tim))
          ::
          ++  fi-etch-wail
            |=(frag=@ud `hoot``@`(etch-shot (make-shot %0 fi-full-path frag)))
          ::
          ++  fi-send
            |=  =blob
            fi-core(ev-core (ev-send-blob for=| her blob `known/peer-state))
          ::
          ++  fi-give-tune
            |=  dat=(unit roar)
            |=([=^duct c=_fi-core] (fi-emit:c duct %give %tune [her path] dat))
          ::
          +|  %entry-points
          ::
          ++  fi-start
            |=  =^duct
            %-  (fi-trace fin.veb |.("keen {(spud fi-full-path)}"))
            =.  fi-core  (fi-sub duct)
            ?>  =(num-fragments.keen 0)
            =/  fra=@     1
            =/  req=hoot  (fi-etch-wail fra)
            =/     =want  [fra req last=now tries=1 skips=0]
            =.  wan.keen  (put:fi-mop ~ [fra .]:want)
            (fi-send `@ux`req)
          ::
          ++  fi-rcv
            |=  [[=full=^path num=@ud] =meow =lane:ames]
            ^+  fi-core
            =/  og  fi-core
            =.  pe-core  (pe-update-qos %fine %live last-contact=now)
            ::  handle empty
            ?:  =(0 num.meow)
              ?>  =(~ dat.meow)
              (fi-done sig.meow ~)
            ::  update congestion, or fill details
            ::
            =?  fi-core  =(0 num-fragments.keen)
              ?>  =(num 1)
              (fi-first-rcv meow)
            ::
            ?.  ?=([@ @ @ *] full-path)
              ~|  fine-path-too-short+full-path
              !!
            ?.  =(`her (slaw %p i.full-path))
              ~|  fine-path-bunk-ship+[full-path her]
              !!
            ?.  =(`rift.peer-state (slaw %ud i.t.full-path))
              ~|  fine-path-bunk-rift+[full-path rift.peer-state]
              !!
            ?.  =(`life.peer-state (slaw %ud i.t.t.full-path))
              ~|  fine-path-bunk-life+[full-path life.peer-state]
              !!
            ?.  (veri-fra:keys [full-path num [dat sig]:meow])
              ~|  fine-purr-fail-signature/num^`@ux`sig.meow
              ~|  life.peer-state
              !!
            ::
            =^  found=?  fi-core  (fi-on-ack num)
            ?.  found
              (fi-fast-retransmit:og num)
            =.  num-received.keen  +(num-received.keen)
            =.  hav.keen
              ::  insert in reverse order
              ::
              |-  ^-  (list have)
              ?~  hav.keen
                [num meow]~
              ?:  (lth num fra.i.hav.keen)
                [i.hav.keen $(hav.keen t.hav.keen)]
              [[num meow] hav.keen]
            ?.  =(num-fragments num-received):keen
              fi-continue
            (fi-done [sig dat]:fi-sift-full)
          ::
          ++  fi-sub
            |=(=^duct fi-core(listeners.keen (~(put in listeners.keen) duct)))
          ::  scry is autocancelled in +abet if no more listeners
          ::
          ++  fi-unsub
            |=  [=^duct all=?]
            ^+  fi-core
            ?:  all
              %-  (fi-trace fin.veb |.("unsub all {<fi-full-path>}"))
              =.  fi-core  (~(rep in listeners.keen) (fi-give-tune ~))
              fi-core(listeners.keen ~)
            ::
            ?:  (~(has in listeners.keen) duct)
              %-  (fi-trace fin.veb |.("unsub {<fi-full-path>} on {<duct>}"))
              fi-core(listeners.keen (~(del in listeners.keen) duct))
            ::
            %-  (fi-trace fin.veb |.("unknown {<fi-full-path>} {<duct>}"))
            fi-core
          ::
          +|  %implementation
          ::
          ++  fi-on-ack
            =|  marked=(list want)
            |=  fra=@ud
            ^-  [found=? cor=_fi-core]
            =.  fi-core
              ?~  first=(pry:fi-mop wan.keen)
                fi-core
              ?:  =(fra fra.val.u.first)
                fi-core
              =^  resend=?  metrics.keen
                (ga-skipped-packet:fi-gauge +>.val.u.first)
              ?:  !resend
                fi-core
              =.  tries.val.u.first  +(tries.val.u.first)
              =.  last-sent.val.u.first  now
              =.  wan.keen  (put:fi-mop wan.keen u.first)
              (fi-send `@ux`hoot.val.u.first)
            ::
            ?~  found=(get:fi-mop wan.keen fra)
              [| fi-core]
            =.  metrics.keen  (ga-on-ack:fi-gauge +>.u.found)
            =.  wan.keen      +:(del:fi-mop wan.keen fra)
            [& fi-core]
          ::
          ++  fi-done
            |=  [sig=@ data=$@(~ (cask))]
            =/  ful  fi-full-path
            =/  roar=(unit roar)
              ?.  (meri:keys ful sig data)
                ~
              :+  ~  [ful ?~(data ~ `data)]
              [[her [life.peer-state sig]] ~ ~]
            ::
            %-  (fi-trace fin.veb |.("done {(spud ful)}"))
            (~(rep in listeners.keen) (fi-give-tune roar))
          ::
          ++  fi-first-rcv
            |=  =meow
            ^+  fi-core
            ::
            =;  paz=(list want)
              fi-core(keen keen(num-fragments num.meow, nex (tail paz)))
            %+  turn  (gulf 1 num.meow)
            |=  fra=@ud
            ^-  want
            [fra (fi-etch-wail fra) now 0 0]
          ::  +fi-continue: send packets based on normal congestion flow
          ::
          ++  fi-continue
            =|  inx=@ud
            =|  sent=(list @ud)
            =/  max  ga-num-slots:fi-gauge
            |-  ^+  fi-core
            ?:  |(=(~ nex.keen) =(inx max))
              fi-core
            =^  =want  nex.keen  nex.keen
            =.  last-sent.want   now
            =.      tries.want   +(tries.want)
            =.        wan.keen   (put:fi-mop wan.keen [fra .]:want)
            =.         fi-core   (fi-send `@ux`hoot.want)
            $(inx +(inx))
          ::
          ++  fi-sift-full
            =,  keen
            ?.  ?&  =(num-fragments num-received)
                    =((lent hav) num-received)
                ==
              ~|  :-  %frag-mismatch
                  [have/num-received need/num-fragments path/path]
              !!
            (sift-roar num-fragments hav)
          ::
          ++  fi-fast-retransmit
            |=  fra=@ud
            =;  [cor=_fi-core wants=_wan.keen]
              cor(wan.keen wants)
            %^  (dip:fi-mop ,cor=_fi-core)  wan.keen
              fi-core
            |=  [cor=_fi-core @ud =want]
            ^-  [(unit ^want) stop=? cor=_fi-core]
            ?.  (lte fra.want fra)
              [`want & cor]
            ?:  (gth (ga-next-expiry:fi-gauge:cor +>.want) now)
              [`want & cor]
            [`want(last-sent now) stop=| (fi-send:cor `@ux`hoot.want)]
          ::
          ++  fi-pass-timer
            |=  =note
            =/  =wire  (welp /fine/behn/wake/(scot %p her) path)
            (fi-emit unix-duct.ames-state %pass wire note)
          ::
          ++  fi-set-wake
            ^+  fi-core
            =/  next-wake=(unit @da)
              ?~  want=(pry:fi-mop wan.keen)
                ~
              `(ga-next-expiry:fi-gauge +>:val.u.want)
            ?:  =(next-wake next-wake.keen)
              fi-core
            =?  fi-core  !=(~ next-wake.keen)
              =/  old  (need next-wake.keen)
              =.  next-wake.keen  ~
              (fi-rest old)
            =?  fi-core  ?=(^ next-wake)
              =.  next-wake.keen  next-wake
              (fi-wait u.next-wake)
            fi-core
          ::  +fi-take-wake: handle request packet timeout
          ::
          ++  fi-take-wake
            ^+  fi-core
            =.  next-wake.keen  ~
            =.  pe-core  (pe-update-qos %fine qos:(is-peer-dead now peer-state))
            ::  has the direct route expired?
            ::
            =.  peer-state    (update-peer-route her peer-state)
            =.  metrics.keen  ga-timeout:fi-gauge
            =^  want=(unit want)  wan.keen
              ?~  res=(pry:fi-mop wan.keen)  `wan.keen
              (del:fi-mop wan.keen key.u.res)
            ~|  %took-wake-for-empty-want
            ?>  ?=(^ want)
            =:      tries.u.want  +(tries.u.want)
                last-sent.u.want  now
              ==
            =.  wan.keen  (put:fi-mop wan.keen [fra .]:u.want)
            (fi-send `@ux`hoot.u.want)
          --
        ::  +ga: constructor for |pump-gauge congestion control core
        ::
        ++  ga
          |=  [pump-metrics live-packets=@ud]
          =*  ship     her
          =*  now      now.channel
          =*  metrics  +<-
          |%
          +|  %helpers
          ::
          ++  ga-trace
            |=  [verb=? print=(trap tape)]
            ^+  same
            (trace %ames verb ship ships.bug.channel print)
          ::  +ga-next-expiry: when should a newly sent fresh packet time out?
          ::
          ::    Use rtt + 4*sigma, where sigma is the mean deviation of rtt.
          ::    This should make it unlikely that a packet would time out
          ::    from a delay, as opposed to an actual packet loss.
          ::
          ++  ga-next-expiry
            |=  packet-state
            ^-  @da
            (add last-sent rto)
          ::  +ga-num-slots: how many packets can we send right now?
          ::
          ++  ga-num-slots
            ^-  @ud
            (ga-sub-safe cwnd live-packets)
          ::
          ::  +ga-clamp-rto: apply min and max to an .rto value
          ::
          ++  ga-clamp-rto
            |=  rto=@dr
            ^+  rto
            (min ga-max-backoff (max ^~((div ~s1 5)) rto))
          ::  +ga-max-backoff: calculate highest re-send interval
          ::
          ::    Keeps pinhole to sponsors open by inspecting the duct (hack).
          ::
          ++  ga-max-backoff
            ^-  @dr
            ?:(?=([[%gall %use %ping *] *] duct) ~s25 ~m2)
          ::  +ga-in-slow-start: %.y if we're in "slow-start" mode
          ::
          ++  ga-in-slow-start
            ^-  ?
            (lth cwnd ssthresh)
          ::  +ga-in-recovery: %.y if we're recovering from a skipped packet
          ::
          ::    We finish recovering when .live-packets finally dips back
          ::    down to .cwnd.
          ::
          ++  ga-in-recovery
            ^-  ?
            (gth live-packets cwnd)
          ::  +ga-sub-safe: subtract with underflow protection
          ::
          ++  ga-sub-safe
            |=  [a=@ b=@]
            ^-  @
            ?:((lte a b) 0 (sub a b))
          ::  +show: produce a printable version of .metrics
          ::
          ++  ga-show
            =/  ms  (div ~s1 1.000)
            ::
            :*  rto=(div rto ms)
                rtt=(div rtt ms)
                rttvar=(div rttvar ms)
                ssthresh=ssthresh
                cwnd=cwnd
                num-live=live-packets
                counter=counter
            ==
          ::
          +|  %entry-points
          ::  +ga-on-ack: adjust metrics based on a packet getting acknowledged
          ::
          ++  ga-on-ack
            |=  =packet-state
            ^-  pump-metrics
            ::
            =.  counter  +(counter)
            ::  if below congestion threshold, add 1; else, add avg 1 / cwnd
            ::
            =.  cwnd
              ?:  ga-in-slow-start
                +(cwnd)
              (add cwnd !=(0 (mod (mug now) cwnd)))
            ::  if this was a re-send, don't adjust rtt or downstream state
            ::
            ?:  (gth tries.packet-state 1)
              metrics(rto (ga-clamp-rto (add rtt (mul 4 rttvar))))
            ::  rtt-datum: new rtt measurement based on packet roundtrip
            ::
            =/  rtt-datum=@dr  (ga-sub-safe now last-sent.packet-state)
            ::  rtt-error: difference between this measurement and expected
            ::
            =/  rtt-error=@dr
              ?:  (gte rtt-datum rtt)
                (sub rtt-datum rtt)
              (sub rtt rtt-datum)
            ::  exponential weighting ratio for .rtt and .rttvar
            ::
            =.  rtt     (div (add rtt-datum (mul rtt 7)) 8)
            =.  rttvar  (div (add rtt-error (mul rttvar 7)) 8)
            =.  rto     (ga-clamp-rto (add rtt (mul 4 rttvar)))
            ::
            %-  %+  ga-trace  ges.veb  |.
                "ack update {<ga-show rtt-datum=rtt-datum rtt-error=rtt-error>}"
            metrics
          ::  +ga-skipped-packet: handle misordered ack
          ::
          ++  ga-skipped-packet
            |=  packet-state
            ^-  [resend=? pump-metrics]
            ::
            =/  resend=?  &((lte tries 1) |(ga-in-recovery (gte skips 3)))
            :-  resend
            ::
            =?  cwnd  !ga-in-recovery  (max 2 (div cwnd 2))
            %-  %+  ga-trace  snd.veb
                |.("skip {<resend=resend in-recovery=ga-in-recovery ga-show>}")
            metrics
          ::  +ga-timeout: (re)enter slow-start mode on packet loss
          ::
          ++  ga-timeout
            ^-  pump-metrics
            ::
            %-  (ga-trace ges.veb |.("timeout update {<ga-show>}"))
            =:  ssthresh  (max 1 (div cwnd 2))
                    cwnd  1
                    rto   (ga-clamp-rto (mul rto 2))
              ==
            metrics
          --
        --
      --
    --
::  adult ames, after metamorphosis from larva
::
=|  =ames-state
|=  [now=@da eny=@ rof=roof]
=*  ames-gate  .
=*  veb  veb.bug.ames-state
|%
::  +call: handle request $task
::
++  call
  |=  [=duct dud=(unit goof) wrapped-task=(hobo task)]
  ^-  [(list move) _ames-gate]
  ::
  =/  =task    ((harden task) wrapped-task)
  =/  ev-core  (ev [now eny rof] duct ames-state)
  ::
  =^  moves  ames-state
    =<  ev-abet
    ::  handle error notifications
    ::
    ?^  dud
      ?+  -.task  (ev-crud:ev-core -.task tang.u.dud)
        %hear  (ev-hear:ev-core lane.task blob.task dud)
      ==
    ::
    ?-  -.task
      %born  ev-born:ev-core
      %hear  (ev-hear:ev-core [lane blob ~]:task)
      %dear  (ev-dear:ev-core +.task)
      %heed  (ev-heed:ev-core ship.task)
      %init  ev-init:ev-core
      %jilt  (ev-jilt:ev-core ship.task)
      %prod  (ev-prod:ev-core ships.task)
      %sift  (ev-sift:ev-core ships.task)
      %snub  (ev-snub:ev-core [form ships]:task)
      %spew  (ev-spew:ev-core veb.task)
      %cong  (ev-cong:ev-core [msg mem]:task)
      %stir  (ev-stir:ev-core arg.task)
      %trim  ev-trim:ev-core
      %vega  ev-vega:ev-core
      %plea  (ev-plea:ev-core [ship plea]:task)
      %cork  (ev-cork:ev-core ship.task)
      %tame  (ev-tame:ev-core ship.task)
      %kroc  (ev-kroc:ev-core bones.task)
      %deep  (ev-deep:ev-core deep.task)
    ::
      %keen  (ev-keen:ev-core +.task)
      %yawn  (ev-cancel-scry:ev-core | +.task)
      %wham  (ev-cancel-scry:ev-core & +.task)
    ==
  ::
  [moves ames-gate]
::  +take: handle response $sign
::
++  take
  |=  [=wire =duct dud=(unit goof) =sign]
  ^-  [(list move) _ames-gate]
  ?^  dud
    ~|(%ames-take-dud (mean tang.u.dud))
  ::
  =/  ev-core  (ev [now eny rof] duct ames-state)
  ::
  =^  moves  ames-state
    ?:  ?=([%gall %unto *] sign)
      `ames-state
    ::
    =<  ev-abet
    ?-  sign
      [@ %done *]   (ev-take-done:ev-core wire error.sign)
      [@ %boon *]   (ev-take-boon:ev-core wire payload.sign)
    ::
      [%behn %wake *]  (ev-take-wake:ev-core wire error.sign)
    ::
      [%gall %flub ~]  (ev-take-flub:ev-core wire)
    ::
      [%jael %turf *]          (ev-take-turf:ev-core turf.sign)
      [%jael %private-keys *]  (ev-priv:ev-core [life vein]:sign)
      [%jael %public-keys *]   (ev-publ:ev-core wire public-keys-result.sign)
    ==
  ::
  [moves ames-gate]
::  +stay: extract state before reload
::
++  stay  [%17 %adult ames-state]
::  +load: load in old state after reload
::
++  load
  =<  |=  $=  old-state
          $%  [%17 ^ames-state]
          ==
      ^+  ames-gate
      ?>  ?=(%17 -.old-state)
      ames-gate(ames-state +.old-state)
  ::  all state transitions are called from larval ames
  ::
  |%
  ++  our-beam  `beam`[[our %rift %da now] /(scot %p our)]
  ++  state-4-to-5
    |=  ames-state=ames-state-4
    ^-  ames-state-5
    =.  peers.ames-state
      %-  ~(run by peers.ames-state)
      |=  ship-state=ship-state-4
      ?.  ?=(%known -.ship-state)
        ship-state
      =.  snd.ship-state
        %-  ~(run by snd.ship-state)
        |=  pump=message-pump-state-16
        =.  num-live.metrics.packet-pump-state.pump
          ~(wyt in live.packet-pump-state.pump)
        pump
      ship-state
    ames-state
  ::
  ++  state-5-to-6
    |=  ames-state=ames-state-5
    ^-  ames-state-6
    :_  +.ames-state
    %-  ~(rut by peers.ames-state)
    |=  [=ship ship-state=ship-state-5]
    ^-  ship-state-6
    ?.  ?=(%known -.ship-state)
      ship-state
    =/  peer-state=peer-state-5  +.ship-state
    =/  =rift
      ::  harcoded because %jael doesn't have data about comets
      ::
      ?:  ?=(%pawn (clan:title ship))  0
      ;;  @ud
      =<  q.q  %-  need  %-  need
      (rof ~ /ames %j `beam`[[our %rift %da now] /(scot %p ship)])
    :-   -.ship-state
    :_  +.peer-state
    =,  -.peer-state
    [symmetric-key life rift public-key sponsor]
  ::
  ++  state-6-to-7
    |=  ames-state=ames-state-6
    ^-  ames-state-7
    :_  +.ames-state
    %-  ~(run by peers.ames-state)
    |=  ship-state=ship-state-6
    ^-  ship-state-7
    ?.  ?=(%known -.ship-state)
      ship-state
    :-  %known
    ^-  peer-state-7
    :-  +<.ship-state
    [route qos ossuary snd rcv nax heeds ~ ~ ~]:ship-state
  ::
  ++  state-7-to-8
    |=  ames-state=ames-state-7
    ^-  ames-state-8
    =,  ames-state
    :*  peers  unix-duct  life  crypto-core  bug
        *(set wire)
    ==
  ::
  ++  state-8-to-9
    |=  ames-state=ames-state-8
    ^-  ames-state-9
    =,  ames-state
    :*  peers  unix-duct  life  crypto-core  bug  corks
        *(set ship)
    ==
  ::
  ++  state-9-to-10
    |=  ames-state=ames-state-9
    ^-  ames-state-10
    =,  ames-state
    :*  peers  unix-duct  life  crypto-core
        %=  bug.ames-state
          veb  [&1 &2 &3 &4 &5 &6 |6 %.n]:veb.bug
        ==
        corks  snub
    ==
  ::
  ++  state-10-to-11
    |=  ames-state=ames-state-10
    ^-  ames-state-11
    =,  ames-state
    :*  peers  unix-duct  life  crypto-core  bug  corks  snub
        ::  5 messages and 100Kb of data outstanding
        ::
        [msg=5 mem=100.000]
    ==
  ::
  ++  state-11-to-12
    |=  ames-state=ames-state-11
    ^-  ames-state-12
    :_  =,  ames-state
        :*  unix-duct
            life
            crypto-core
            bug
            [%deny snub]
            cong
        ==
    ^-  (map ship ship-state-12)
    %-  ~(run by peers.ames-state)
    |=  ship-state=ship-state-7
    ^-  ship-state-12
    ?.  ?=(%known -.ship-state)
      ship-state
    %=  ship-state
      +>  [route qos ossuary snd rcv nax heeds closing corked]:+>.ship-state
    ==
  ::
  ++  state-12-to-13
    |=  old=ames-state-12
    ^-  ames-state-13
    =+  !<(=rift q:(need (need (rof ~ /ames %j our-beam))))
    =+  pk=sec:ex:crypto-core.old
    :*  peers=(~(run by peers.old) ship-state-12-to-13)
        unix-duct.old
        life.old
        rift
        ?:(=(*ring pk) *acru:ames (nol:nu:crub:crypto pk))
        %=  bug.old
          veb  [&1 &2 &3 &4 &5 &6 &7 |7 %.n]:veb.bug.old
        ==
        snub.old
        cong.old
    ==
  ::
  ++  ship-state-12-to-13
    |=  old=ship-state-12
    ^-  ship-state-13
    ?:  ?=(%alien -.old)
      old(heeds [heeds.old ~])
    old(corked [corked.old ~])
  ::
  ++  state-13-to-14
    |=  old=ames-state-13
    ^-  ames-state-14
    =-  old(peers -)
    %-  ~(run by peers.old)
    |=  old=ship-state-13
    |^  ?:  ?=(%alien -.old)  old
    old(keens (~(run by keens.old) keen-state-13-to-14))
    ::
    ++  keen-state-13-to-14
      |=  old=keen-state-13
      ^-  keen-state-14
      =-  old(wan -)
      %+  gas:((on @ud want) lte)  ~
      %+  turn  (tap:(deq:keen-state-13 want) wan.old)
      |=  =want  [fra .]:want
    --
  ::
  ++  state-14-to-15
    |=  old=ames-state-14
    ^-  ames-state-15
    old(rift !<(=rift q:(need (need (rof ~ /ames %j our-beam)))))
  ::
  ++  state-15-to-16
    |=  old=ames-state-15
    ^-  ames-state-16
    ::  re-initialize default congestion control values, if bunted
    ::
    old(cong ?.(=(cong.old [0 0]) cong.old [5 100.000]))
  ::
  ++  state-16-to-17
    |=  old=ames-state-16
    ^-  ^ames-state
    %=    old
        cong
      :+  cong.old
        flow/~
      cork/`[~[/ames] /recork `@da`(add now ~d1)]
      ::
        peers
      %-  ~(run by peers.old)
      |=  ship-state=ship-state-16
      ^-  ^ship-state
      ?.  ?=(%known -.ship-state)
        ship-state
      |^
      %=  ship-state
        snd    (~(run by snd.ship-state) message-pump-16-to-17)
        keens  (~(run by keens.ship-state) keen-state-16-to-17)
        rcv    (~(rut by rcv.ship-state) remove-outbound-naxplanations)
      ==
      ::
      ++  message-pump-16-to-17
        |=  pump=message-pump-state-16
        ^-  message-pump-state
        %=    pump
            metrics.packet-pump-state
          [rto rtt rttvar ssthresh cwnd counter]:metrics.packet-pump-state.pump
        ==
      ::
      ++  keen-state-16-to-17
        |=  keen-state=keen-state-16
        ^-  ^keen-state
        %=  keen-state
          metrics  [rto rtt rttvar ssthresh cwnd counter]:metrics.keen-state
        ==
      ::
      ++  remove-outbound-naxplanations
        |=  [=bone sink=message-sink-state]
        ^+  sink
        =/  target=^bone  (mix 0b10 bone)
        ?.  =(%3 (mod target 4))
          sink
        ?~  pump=(~(get by snd.ship-state) target)
          sink
        %_    sink
            nax
          %-  ~(rep in nax.sink)
          |=  [=message-num nax=(set message-num)]
          ::  we keep messages in the queue that have not been acked.
          ::  if the message-num for the naxplanation we sent is
          ::  less than the current message, +mu-done:mu had been called,
          ::  so the message-num can be safely removed
          ::
          =?  nax  (gte message-num current.u.pump)
            (~(put in nax) message-num)
          nax
        ==
      --
    ==
  --
::  +scry: dereference namespace
::
++  scry
  ^-  roon
  |=  [lyc=gang pov=path car=term bem=beam]
  ^-  (unit (unit cage))
  =*  ren  car
  =*  why=shop  &/p.bem
  =*  syd  q.bem
  =*  lot=coin  $/r.bem
  =*  tyl  s.bem
  ::
  ::  only respond for the local identity, %$ desk, current timestamp
  ::
  ?.  ?&  =(&+our why)
          =([%$ %da now] lot)
          =(%$ syd)
      ==
    ?.  for.veb.bug.ames-state  ~
    ~>  %slog.0^leaf/"ames: scry-fail {<why=why lot=lot now=now syd=syd>}"
    ~
  ::  /ax//whey                      (list mass)
  ::  /ax/protocol/version           @
  ::  /ax/peers                      (map ship ?(%alien %known))
  ::  /ax/peers/[ship]               ship-state
  ::  /ax/peers/[ship]/last-contact  (unit @da)
  ::  /ax/peers/[ship]/forward-lane  (list lane)
  ::  /ax/bones/[ship]               [snd=(set bone) rcv=(set bone)]
  ::  /ax/snd-bones/[ship]/[bone]    vase
  ::  /ax/snubbed                    (?(%allow %deny) (list ship))
  ::  /ax/fine/hunk/[path/...]       (list @ux) scry response fragments
  ::  /ax/fine/ducts/[path/]         (list duct)
  ::  /ax/rift                        @
  ::  /ax/corked/[ship]              (set bone)
  ::  /ax/closing/[ship]             (set bone)
  ::
  ?.  ?=(%x ren)  ~
  =>  .(tyl `(pole knot)`tyl)
  ?+    tyl  ~
      [%$ %whey ~]
    =/  maz=(list mass)
      =+  [known alien]=(skid ~(val by peers.ames-state) |=(^ =(%known +<-)))
      :~  peers-known+&+known
          peers-alien+&+alien
      ==
    ``mass+!>(maz)
  ::
      [%protocol %version ~]
    ``noun+!>(protocol-version)
  ::
      [%peers ~]
    :^  ~  ~  %noun
    !>  ^-  (map ship ?(%alien %known))
    (~(run by peers.ames-state) head)
  ::
      [%peers her=@ req=*]
    =/  who  (slaw %p her.tyl)
    ?~  who  [~ ~]
    =/  peer  (~(get by peers.ames-state) u.who)
    ?+    req.tyl  [~ ~]
        ~
      ?~  peer
        [~ ~]
      ``noun+!>(u.peer)
    ::
        [%last-contact ~]
      :^  ~  ~  %noun
      !>  ^-  (unit @da)
      ?.  ?=([~ %known *] peer)
        ~
      `last-contact.qos.u.peer
    ::
        [%forward-lane ~]
      ::
      ::  this duplicates the routing hack from +ev-send-blob:ev-core
      ::  so long as neither the peer nor the peer's sponsoring galaxy is us,
      ::  and the peer has been reached recently:
      ::
      ::    - no route to the peer, or peer has not been contacted recently:
      ::      send to the peer's sponsoring galaxy
      ::    - direct route to the peer: use that
      ::    - indirect route to the peer: send to both that route and the
      ::      the peer's sponsoring galaxy
      ::
      :^  ~  ~  %noun
      !>  ^-  (list lane)
      ?:  =(our u.who)
        ~
      ?.  ?=([~ %known *] peer)
        =/  sax  (rof ~ /ames %j `beam`[[our %saxo %da now] /(scot %p u.who)])
        ?.  ?=([~ ~ *] sax)
          ~
        =/  gal  (rear ;;((list ship) q.q.u.u.sax))
        ?:  =(our gal)
          ~
        [%& gal]~
      =;  zar=(trap (list lane))
        ?~  route.u.peer  $:zar
        =*  rot  u.route.u.peer
        ?:(direct.rot [lane.rot ~] [lane.rot $:zar])
      ::
      |.  ^-  (list lane)
      ?:  ?=(%czar (clan:title sponsor.u.peer))
        ?:  =(our sponsor.u.peer)
          ~
        [%& sponsor.u.peer]~
      =/  next  (~(get by peers.ames-state) sponsor.u.peer)
      ?.  ?=([~ %known *] next)
        ~
      $(peer next)
    ==
  ::
      [%bones her=@ ~]
    =/  who  (slaw %p her.tyl)
    ?~  who  [~ ~]
    =/  per  (~(get by peers.ames-state) u.who)
    ?.  ?=([~ %known *] per)  [~ ~]
    =/  res
      =,  u.per
      [snd=~(key by snd) rcv=~(key by rcv)]
    ``noun+!>(res)
  ::
      [%snd-bones her=@ bon=@ ~]
    =/  who  (slaw %p her.tyl)
    ?~  who  [~ ~]
    =/  ost  (slaw %ud bon.tyl)
    ?~  ost  [~ ~]
    =/  per  (~(get by peers.ames-state) u.who)
    ?.  ?=([~ %known *] per)  [~ ~]
    =/  mps  (~(get by snd.u.per) u.ost)
    ?~  mps  [~ ~]
    =/  res
      u.mps
    ``noun+!>(!>(res))
  ::
      [%snubbed ~]
    ``noun+!>([form.snub.ames-state ~(tap in ships.snub.ames-state)])
  ::
      [%fine %hunk lop=@t len=@t pax=^]
    ::TODO  separate endpoint for the full message (instead of packet list)
    ::  .pax is expected to be a scry path of the shape /vc/desk/rev/etc,
    ::  so we need to give it the right shape
    ::
    ?~  blk=(de-path-soft:balk pax.tyl)  ~
    ::
    ?.  ?&  =(our her.u.blk)
            =(rift.ames-state rif.u.blk)
            =(life.ames-state lyf.u.blk)
        ==
      ~&  [%fine-mismatch our=[rift life]:ames-state her=[her rif lyf]:u.blk]
      ~
    =+  nom=(as-omen:balk u.blk)
    ~|  nom
    |^
    =/  van  ?@(vis.nom (end 3 vis.nom) way.vis.nom)
    ?+    van  ~
        %c
      =+  pem=(rof lyc /ames nom(vis %cp))
      ?.  ?=(^ pem)    ~
      ?.  ?=(^ u.pem)  ~
      ~|  u.u.pem
      =+  per=!<([r=dict:clay w=dict:clay] q.u.u.pem)
      ?.  =([%black ~ ~] rul.r.per)  ~
      (en-hunk (rof ~ /ames nom))
    ::
        %e
      =/  kyr  ?@(vis.nom (rsh 3 vis.nom) car.vis.nom)
      %-  en-hunk
      ?+  kyr  ~
        %x  (rof ~ /ames nom)
      ==
    ::
        %g
      =/  kyr  ?@(vis.nom (rsh 3 vis.nom) car.vis.nom)
      %-  en-hunk
      ?+  kyr  ~
        %x  (rof ~ /ames nom)
      ==
    ==
    ::
    ++  en-hunk
      |=  res=(unit (unit cage))
      ^+  res
      ?~  res  ~
      =/  =hunk  [(slav %ud lop.tyl) (slav %ud len.tyl)]
      ::
      =/  hu-co  (etch-hunk our [life crypto-core]:ames-state)
      ?-  res
        [~ ~]    ``noun+!>((etch:hu-co pax.tyl hunk ~))
        [~ ~ *]  ``noun+!>((etch:hu-co pax.tyl hunk [p q.q]:u.u.res))
      ==
    --
  ::
      [%fine %ducts pax=^]
    ?~  bulk=(de-path-soft:balk pax.tyl)  ~
    ?~  peer=(~(get by peers.ames-state) her.u.bulk)
      [~ ~]
    ?.  ?=([~ %known *] peer)
      [~ ~]  :: TODO handle aliens
    ?~  spr.u.bulk  [~ ~]
    =/  =path  =,(u.bulk [van car (scot cas) spr])
    ?~  keen=(~(get by keens.u.peer) path)
      [~ ~]
    ``noun+!>(listeners:u.keen)
  ::
      [%rift ~]
    ``noun+!>(rift.ames-state)
  ::
      [%corked her=@ ~]
    =/  who  (slaw %p her.tyl)
    ?~  who  [~ ~]
    =/  per  (~(get by peers.ames-state) u.who)
    ?.  ?=([~ %known *] per)  [~ ~]
    ``noun+!>(corked.u.per)
  ::
      [%closing her=@ ~]
    =/  who  (slaw %p her.tyl)
    ?~  who  [~ ~]
    =/  per  (~(get by peers.ames-state) u.who)
    ?.  ?=([~ %known *] per)  [~ ~]
    ``noun+!>(closing.u.per)
  ==
--
