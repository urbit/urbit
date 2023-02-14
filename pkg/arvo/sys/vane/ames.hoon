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
::  protocol-version: current version of the ames wire protocol
::
!:
=/  protocol-version=?(%0 %1 %2 %3 %4 %5 %6 %7)  %0
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
  ==
=>
~%  %ames  ..part  ~
|%
+|  %helpers
::  +trace: print if .verb is set and we're tracking .ship
::
++  trace
  |=  [verb=? =ship ships=(set ship) print=(trap tape)]
  ^+  same
  ?.  verb
    same
  ?.  =>  [ship=ship ships=ships in=in]
      ~+  |(=(~ ships) (~(has in ships) ship))
    same
  (slog leaf/"ames: {(scow %p ship)}: {(print)}" ~)
::  +qos-update-text: notice text for if connection state changes
::
++  qos-update-text
  |=  [=ship old=qos new=qos k=? ships=(set ship)]
  ^-  (unit tape)
  ::
  ?+  [-.old -.new]  ~
    [%unborn %live]  `"; {(scow %p ship)} is your neighbor"
    [%dead %live]    ((trace k ship ships |.("is ok")) ~)
    [%live %dead]    ((trace k ship ships |.("not responding still trying")) ~)
    [%unborn %dead]  ((trace k ship ships |.("not responding still trying")) ~)
    [%live %unborn]  `"; {(scow %p ship)} has sunk"
    [%dead %unborn]  `"; {(scow %p ship)} has sunk"
  ==
::  +lte-packets: yes if a is before b
::
++  lte-packets
  |=  [a=live-packet-key b=live-packet-key]
  ^-  ?
  ::
  ?:  (lth message-num.a message-num.b)
    %.y
  ?:  (gth message-num.a message-num.b)
    %.n
  (lte fragment-num.a fragment-num.b)
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
  =/  num-fragments=fragment-num  (met 13 message-blob)
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
  (cue (rep 13 (flop sorted)))
::  +jim: caching +jam
::
++  jim  |=(n=* ~+((jam n)))
::  +bind-duct: find or make new $bone for .duct in .ossuary
::
++  bind-duct
  |=  [=ossuary =duct]
  ^+  [next-bone.ossuary ossuary]
  ::
  ?^  existing=(~(get by by-duct.ossuary) duct)
    [u.existing ossuary]
  ::
  :-  next-bone.ossuary
  :+  (add 4 next-bone.ossuary)
    (~(put by by-duct.ossuary) duct next-bone.ossuary)
  (~(put by by-bone.ossuary) next-bone.ossuary duct)
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
::  +parse-pump-timer-wire: parse .her and .bone from |packet-pump wire
::
++  parse-pump-timer-wire
  |=  =wire
  ^-  (unit [her=ship =bone])
  ::
  ~|  %ames-wire-timer^wire
  ?.  ?=([%pump @ @ ~] wire)
    ~
  ?~  ship=`(unit @p)`(slaw %p i.t.wire)
    ~
  ?~  bone=`(unit @ud)`(slaw %ud i.t.t.wire)
    ~
  `[u.ship u.bone]
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
::  +encode-packet: serialize a packet into a bytestream
::
++  encode-packet
  ~/  %encode-packet
  |=  packet
  ^-  blob
  ::
  =/  sndr-meta  (encode-ship-metadata sndr)
  =/  rcvr-meta  (encode-ship-metadata rcvr)
  ::
  =/  body=@
    ;:  mix
      sndr-tick
      (lsh 2 rcvr-tick)
      (lsh 3 sndr)
      (lsh [3 +(size.sndr-meta)] rcvr)
      (lsh [3 +((add size.sndr-meta size.rcvr-meta))] content)
    ==
  =/  checksum  (end [0 20] (mug body))
  =?  body  ?=(^ origin)  (mix u.origin (lsh [3 6] body))
  ::
  =/  header=@
    %+  can  0
    :~  [3 reserved=0]
        [1 is-ames=&]
        [3 protocol-version]
        [2 rank.sndr-meta]
        [2 rank.rcvr-meta]
        [20 checksum]
        [1 relayed=.?(origin)]
    ==
  (mix header (lsh 5 body))
::  +decode-packet: deserialize packet from bytestream or crash
::
++  decode-packet
  ~/  %decode-packet
  |=  =blob
  ^-  packet
  ~|  %decode-packet-fail
  ::  first 32 (2^5) bits are header; the rest is body
  ::
  =/  header  (end 5 blob)
  =/  body    (rsh 5 blob)
  ::  read header; first three bits are reserved
  ::
  =/  is-ames  (cut 0 [3 1] header)
  ?.  =(& is-ames)
    ~|  %ames-not-ames  !!
  ::
  =/  version  (cut 0 [4 3] header)
  ?.  =(protocol-version version)
    ~|  ames-protocol-version+version  !!
  ::
  =/  sndr-size  (decode-ship-size (cut 0 [7 2] header))
  =/  rcvr-size  (decode-ship-size (cut 0 [9 2] header))
  =/  checksum   (cut 0 [11 20] header)
  =/  relayed    (cut 0 [31 1] header)
  ::  origin, if present, is 6 octets long, at the end of the body
  ::
  =^  origin=(unit @)  body
    ?:  =(| relayed)
      [~ body]
    =/  len  (sub (met 3 body) 6)
    [`(end [3 6] body) (rsh [3 6] body)]
  ::  .checksum does not apply to the origin
  ::
  ?.  =(checksum (end [0 20] (mug body)))
    ~|  %ames-checksum  !!
  ::  read fixed-length sndr and rcvr life data from body
  ::
  ::    These represent the last four bits of the sender and receiver
  ::    life fields, to be used for quick dropping of honest packets to
  ::    or from the wrong life.
  ::
  =/  sndr-tick  (cut 0 [0 4] body)
  =/  rcvr-tick  (cut 0 [4 4] body)
  ::  read variable-length .sndr and .rcvr addresses
  ::
  =/  off   1
  =^  sndr  off  [(cut 3 [off sndr-size] body) (add off sndr-size)]
  ?.  (is-valid-rank sndr sndr-size)
    ~|  ames-sender-impostor+[sndr sndr-size]  !!
  ::
  =^  rcvr  off  [(cut 3 [off rcvr-size] body) (add off rcvr-size)]
  ?.  (is-valid-rank rcvr rcvr-size)
    ~|  ames-receiver-impostor+[rcvr rcvr-size]  !!
  ::  read variable-length .content from the rest of .body
  ::
  =/  content  (cut 3 [off (sub (met 3 body) off)] body)
  [[sndr rcvr] sndr-tick rcvr-tick origin content]
::  +is-valid-rank: does .ship match its stated .size?
::
++  is-valid-rank
  ~/  %is-valid-rank
  |=  [=ship size=@ubC]
  ^-  ?
  .=  size
  ?-  (clan:title ship)
    %czar  2
    %king  2
    %duke  4
    %earl  8
    %pawn  16
  ==
::  +encode-keys-packet: create key request $packet
::
++  encode-keys-packet
  ~/  %encode-keys-packet
  |=  [sndr=ship rcvr=ship sndr-life=life]
  ^-  packet
  :*  [sndr rcvr]
      (mod sndr-life 16)
      `@`1
      origin=~
      content=`@`%keys
  ==
::  +encode-open-packet: convert $open-packet attestation to $packet
::
++  encode-open-packet
  ~/  %encode-open-packet
  |=  [pac=open-packet =acru:ames]
  ^-  packet
  :*  [sndr rcvr]:pac
      (mod sndr-life.pac 16)
      (mod rcvr-life.pac 16)
      origin=~
      content=`@`(sign:as:acru (jam pac))
  ==
::  +decode-open-packet: decode comet attestation into an $open-packet
::
++  decode-open-packet
  ~/  %decode-open-packet
  |=  [=packet our=ship our-life=@]
  ^-  open-packet
  ::  deserialize and type-check packet contents
  ::
  =+  ;;  [signature=@ signed=@]  (cue content.packet)
  =+  ;;  =open-packet            (cue signed)
  ::  assert .our and .her and lives match
  ::
  ?>  .=       sndr.open-packet  sndr.packet
  ?>  .=       rcvr.open-packet  our
  ?>  .=  sndr-life.open-packet  1
  ?>  .=  rcvr-life.open-packet  our-life
  ::  only a star can sponsor a comet
  ::
  ?>  =(%king (clan:title (^sein:title sndr.packet)))
  ::  comet public-key must hash to its @p address
  ::
  ?>  =(sndr.packet fig:ex:(com:nu:crub:crypto public-key.open-packet))
  ::  verify signature
  ::
  ::    Logic duplicates +com:nu:crub:crypto and +sure:as:crub:crypto.
  ::
  =/  key  (end 8 (rsh 3 public-key.open-packet))
  ?>  (veri:ed:crypto signature signed key)
  open-packet
::  +encode-shut-packet: encrypt and packetize a $shut-packet
::
++  encode-shut-packet
  ~/  %encode-shut-packet
  :: TODO add rift to signed messages to prevent replay attacks?
  ::
  |=  $:  =shut-packet
          =symmetric-key
          sndr=ship
          rcvr=ship
          sndr-life=@
          rcvr-life=@
      ==
  ^-  packet
  ::
  =?    meat.shut-packet
      ?&  ?=(%& -.meat.shut-packet)
          (gth (met 13 fragment.p.meat.shut-packet) 1)
      ==
    %_    meat.shut-packet
        fragment.p
      (cut 13 [[fragment-num 1] fragment]:p.meat.shut-packet)
    ==
  ::
  =/  vec  ~[sndr rcvr sndr-life rcvr-life]
  =/  [siv=@uxH len=@ cyf=@ux]
    (~(en sivc:aes:crypto (shaz symmetric-key) vec) (jam shut-packet))
  =/  content  :(mix siv (lsh 7 len) (lsh [3 18] cyf))
  [[sndr rcvr] (mod sndr-life 16) (mod rcvr-life 16) origin=~ content]
::  +decode-shut-packet: decrypt a $shut-packet from a $packet
::
++  decode-shut-packet
  ~/  %decode-shut-packet
  |=  [=packet =symmetric-key sndr-life=@ rcvr-life=@]
  ^-  shut-packet
  ?.  =(sndr-tick.packet (mod sndr-life 16))
    ~|  ames-sndr-tick+sndr-tick.packet  !!
  ?.  =(rcvr-tick.packet (mod rcvr-life 16))
    ~|  ames-rcvr-tick+rcvr-tick.packet  !!
  =/  siv  (end 7 content.packet)
  =/  len  (end 4 (rsh 7 content.packet))
  =/  cyf  (rsh [3 18] content.packet)
  ~|  ames-decrypt+[[sndr rcvr origin]:packet len siv]
  =/  vec  ~[sndr.packet rcvr.packet sndr-life rcvr-life]
  ;;  shut-packet  %-  cue  %-  need
  (~(de sivc:aes:crypto (shaz symmetric-key) vec) siv len cyf)
::  +decode-ship-size: decode a 2-bit ship type specifier into a byte width
::
::    Type 0: galaxy or star -- 2 bytes
::    Type 1: planet         -- 4 bytes
::    Type 2: moon           -- 8 bytes
::    Type 3: comet          -- 16 bytes
::
++  decode-ship-size
  ~/  %decode-ship-size
  |=  rank=@ubC
  ^-  @
  ::
  ?+  rank  !!
    %0b0   2
    %0b1   4
    %0b10  8
    %0b11  16
  ==
::  +encode-ship-metadata: produce size (in bytes) and address rank for .ship
::
::    0: galaxy or star
::    1: planet
::    2: moon
::    3: comet
::
++  encode-ship-metadata
  ~/  %encode-ship-metadata
  |=  =ship
  ^-  [size=@ =rank]
  ::
  =/  size=@  (met 3 ship)
  ::
  ?:  (lte size 2)  [2 %0b0]
  ?:  (lte size 4)  [4 %0b1]
  ?:  (lte size 8)  [8 %0b10]
  [16 %0b11]
+|  %atomics
::
+$  private-key    @uwprivatekey
+$  signature      @uwsignature
::  $rank: which kind of ship address, by length
::
::    0b0: galaxy or star -- 2  bytes
::    0b1: planet         -- 4  bytes
::    0b10: moon           -- 8  bytes
::    0b11: comet          -- 16 bytes
::
+$  rank  ?(%0b0 %0b1 %0b10 %0b11)
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
::  $dyad: pair of sender and receiver ships
::
+$  dyad  [sndr=ship rcvr=ship]
::  $packet: noun representation of an ames datagram packet
::
::    Roundtrips losslessly through atom encoding and decoding.
::
::    .origin is ~ unless the packet is being forwarded.  If present,
::    it's an atom that encodes a route to another ship, such as an IPv4
::    address.  Routes are opaque to Arvo and only have meaning in the
::    interpreter. This enforces that Ames is transport-agnostic.
::
+$  packet
  $:  dyad
      sndr-tick=@ubC
      rcvr-tick=@ubC
      origin=(unit @uxaddress)
      content=@uxcontent
  ==
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
::
::    Note: .corks is only still present for unreleased migration reasons
::
::
+$  ames-state
  $:  peers=(map ship ship-state)
      =unix=duct
      =life
      crypto-core=acru:ames
      =bug
      corks=(set wire)
      snub=(set ship)
      cong=[msg=@ud mem=@ud]
  ==
+$  ames-state-4  ames-state-5
+$  ames-state-5
  $:  peers=(map ship ship-state-5)
      =unix=duct
      =life
      crypto-core=acru:ames
      bug=bug-9
  ==
::
+$  ship-state-4  ship-state-5
+$  ship-state-5
  $%  [%alien alien-agenda]
      [%known peer-state-5]
  ==
::
+$  peer-state-5
  $:  $:  =symmetric-key
          =life
          =public-key
          sponsor=ship
      ==
      route=(unit [direct=? =lane])
      =qos
      =ossuary
      snd=(map bone message-pump-state)
      rcv=(map bone message-sink-state)
      nax=(set [=bone =message-num])
      heeds=(set duct)
  ==
::
+$  ames-state-6
  $:  peers=(map ship ship-state-6)
      =unix=duct
      =life
      crypto-core=acru:ames
      bug=bug-9
  ==
::
+$  ship-state-6
    $%  [%alien alien-agenda]
        [%known peer-state-6]
    ==
::
+$  peer-state-6
  $:  $:  =symmetric-key
          =life
          =rift
          =public-key
          sponsor=ship
      ==
      route=(unit [direct=? =lane])
      =qos
      =ossuary
      snd=(map bone message-pump-state)
      rcv=(map bone message-sink-state)
      nax=(set [=bone =message-num])
      heeds=(set duct)
  ==
::
+$  ames-state-7
  $:  peers=(map ship ship-state)
      =unix=duct
      =life
      crypto-core=acru:ames
      bug=bug-9
  ==
::
+$  ames-state-8
  $:  peers=(map ship ship-state)
      =unix=duct
      =life
      crypto-core=acru:ames
      bug=bug-9
      corks=(set wire)
  ==
::
+$  bug-9
  $:  veb=_[`?`%.n `?`%.n `?`%.n `?`%.n `?`%.n `?`%.n `?`%.n]
      ships=(set ship)
  ==
::
+$  bug-10
  $:  veb=_[`?`%.n `?`%.n `?`%.n `?`%.n `?`%.n `?`%.n `?`%.n `?`%.n]
      ships=(set ship)
  ==
::
+$  ames-state-9
  $:  peers=(map ship ship-state)
      =unix=duct
      =life
      crypto-core=acru:ames
      bug=bug-9
      corks=(set wire)
      snub=(set ship)
  ==
::
+$  ames-state-10
  $:  peers=(map ship ship-state)
      =unix=duct
      =life
      crypto-core=acru:ames
      bug=bug-10
      corks=(set wire)
      snub=(set ship)
  ==
::
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
  $%  [%call =duct wrapped-task=(hobo task)]
      [%take =wire =duct =sign]
  ==
::  $note: request to other vane
::
::    Ames passes a %plea note to another vane when it receives a
::    message on a "forward flow" from a peer, originally passed from
::    one of the peer's vanes to the peer's Ames.
::
::    Ames passes a %plea to itself to trigger a heartbeat message to
::    our sponsor.
::
::    Ames passes a %private-keys to Jael to request our private keys.
::    Ames passes a %public-keys to Jael to request a peer's public
::    keys.
::
+$  note
  $~  [%b %wait *@da]
  $%  $:  %b
      $%  [%wait date=@da]
          [%rest date=@da]
      ==  ==
      $:  %d
      $%  [%flog flog:dill]
      ==  ==
      $:  %j
      $%  [%private-keys ~]
          [%public-keys ships=(set ship)]
          [%turf ~]
          [%ruin ships=(set ship)]
      ==  ==
      $:  @tas
      $%  [%plea =ship =plea]
  ==  ==  ==
::  $sign: response from other vane
::
+$  sign
  $~  [%behn %wake ~]
  $%  $:  %behn
      $%  $>(%wake gift:behn)
      ==  ==
      $:  %jael
      $%  [%private-keys =life vein=(map life ring)]
          [%public-keys =public-keys-result]
          [%turf turfs=(list turf)]
      ==  ==
      $:  @tas
      $%  [%done error=(unit error)]
          [%boon payload=*]
  ==  ==  ==
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
::  $message-pump-gift: effect from |message-pump
::
::    %done: report message acknowledgment
::    %cork: kill flow
::    %kroc: recork this bone
::    %send: emit message fragment
::    %wait: set a new timer at .date
::    %rest: cancel timer at .date
::
+$  message-pump-gift
  $%  [%done =message-num error=(unit error)]
      [%cork ~]
      [%kroc =bone]
      [%send =static-fragment]
      [%wait date=@da]
      [%rest date=@da]
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
::  $packet-pump-gift: effect from |packet-pump
::
::    %send: emit message fragment
::    %wait: set a new timer at .date
::    %rest: cancel timer at .date
::
+$  packet-pump-gift
  $%  [%send =static-fragment]
      [%wait date=@da]
      [%rest date=@da]
  ==
::  $message-sink-task: job for |message-sink
::
::    %done: receive confirmation from vane of processing or failure
::    %drop: clear .message-num from .nax.state
::    %hear: handle receiving a message fragment packet
::      .ok: %.y unless previous failed attempt
::
+$  message-sink-task
  $%  [%done ok=? cork=?]
      [%drop =message-num]
      [%hear =lane =shut-packet ok=?]
  ==
::  $message-sink-gift: effect from |message-sink
::
::    %memo: assembled from received packets
::    %send: emit an ack packet
::
+$  message-sink-gift
  $%  [%memo =message-num message=*]
      [%send =message-num =ack-meat]
      [%cork ~]
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
            [%11 ^ames-state]
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
    ++  stay  [%11 %larva queued-events ames-state.adult-gate]
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
                      events=(qeu queued-event)
                      state=ames-state-9
                  ==
                  [%adult state=ames-state-9]
              ==  ==
              $:  %10
              $%  $:  %larva
                      events=(qeu queued-event)
                      state=ames-state-10
                  ==
                  [%adult state=ames-state-10]
              ==  ==
              $:  %11
              $%  $:  %larva
                      events=(qeu queued-event)
                      state=_ames-state.adult-gate
                  ==
                  [%adult state=_ames-state.adult-gate]
          ==  ==  ==
      ?-    old
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
        larval-gate
      ::
          [%8 %adult *]
        =.  cached-state  `[%8 state.old]
        ~>  %slog.0^leaf/"ames: larva reload"
        larval-gate
      ::
          [%8 %larva *]
        ~>  %slog.0^leaf/"ames: larva: load"
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
        =.  queued-events  events.old
        larval-gate
      ::
          [%10 %adult *]
        =.  cached-state  `[%10 state.old]
        ~>  %slog.0^leaf/"ames: larva reload"
        larval-gate
      ::
          [%10 %larva *]
        ~>  %slog.1^leaf/"ames: larva: load"
        =.  queued-events  events.old
        larval-gate
      ::
          [%11 %adult *]  (load:adult-core %11 state.old)
      ::
          [%11 %larva *]
        ~>  %slog.1^leaf/"ames: larva: load"
        =.  queued-events  events.old
        =.  adult-gate     (load:adult-core %11 state.old)
        larval-gate
       ::
      ==
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
      ?>  ?=(%11 -.u.cached-state)
      =.  ames-state.adult-gate  +.u.cached-state
      [moz larval-core(cached-state ~)]
    --
::  adult ames, after metamorphosis from larva
::
=<
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
  =/  =task  ((harden task) wrapped-task)
  =/  event-core  (per-event [now eny rof] duct ames-state)
  ::
  =^  moves  ames-state
    =<  abet
    ::  handle error notifications
    ::
    ?^  dud
      ?+  -.task
          (on-crud:event-core -.task tang.u.dud)
        %hear  (on-hear:event-core lane.task blob.task dud)
      ==
    ::
    ?-  -.task
      %born  on-born:event-core
      %hear  (on-hear:event-core [lane blob ~]:task)
      %heed  (on-heed:event-core ship.task)
      %init  on-init:event-core
      %jilt  (on-jilt:event-core ship.task)
      %prod  (on-prod:event-core ships.task)
      %sift  (on-sift:event-core ships.task)
      %snub  (on-snub:event-core ships.task)
      %spew  (on-spew:event-core veb.task)
      %cong  (on-cong:event-core [msg mem]:task)
      %stir  (on-stir:event-core arg.task)
      %trim  on-trim:event-core
      %vega  on-vega:event-core
      %plea  (on-plea:event-core [ship plea]:task)
      %cork  (on-cork:event-core ship.task)
      %kroc  (on-kroc:event-core dry.task)
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
  ::
  =/  event-core  (per-event [now eny rof] duct ames-state)
  ::
  =^  moves  ames-state
    =<  abet
    ?-  sign
      [@ %done *]   (on-take-done:event-core wire error.sign)
      [@ %boon *]   (on-take-boon:event-core wire payload.sign)
    ::
      [%behn %wake *]  (on-take-wake:event-core wire error.sign)
    ::
      [%jael %turf *]          (on-take-turf:event-core turfs.sign)
      [%jael %private-keys *]  (on-priv:event-core [life vein]:sign)
      [%jael %public-keys *]   (on-publ:event-core wire public-keys-result.sign)
    ==
  ::
  [moves ames-gate]
::  +stay: extract state before reload
::
++  stay  [%11 %adult ames-state]
::  +load: load in old state after reload
::
++  load
  =<  |=  $=  old-state
          $%  [%11 ^ames-state]
          ==
      ^+  ames-gate
      ?>  ?=(%11 -.old-state)
      ames-gate(ames-state +.old-state)
  ::  all state transitions are called from larval ames
  ::
  |%
  ::
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
        |=  =message-pump-state
        =.  num-live.metrics.packet-pump-state.message-pump-state
          ~(wyt in live.packet-pump-state.message-pump-state)
        message-pump-state
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
      (rof ~ %j `beam`[[our %rift %da now] /(scot %p ship)])
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
    ^-  ^ship-state
    ?.  ?=(%known -.ship-state)
      ship-state
    :-  %known
    ^-  peer-state
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
    ^-  ^^ames-state
    =,  ames-state
    :*  peers  unix-duct  life  crypto-core  bug  corks  snub
        ::  5 messages and 100Kb of data outstanding
        ::
        [msg=5 mem=100.000]
    ==
  --
::  +scry: dereference namespace
::
++  scry
  ^-  roon
  |=  [lyc=gang car=term bem=beam]
  ^-  (unit (unit cage))
  =*  ren  car
  =*  why=shop  &/p.bem
  =*  syd  q.bem
  =*  lot=coin  $/r.bem
  =*  tyl  s.bem
  ::
  ::TODO  don't special-case whey scry
  ::
  ?:  &(=(%$ ren) =(tyl /whey))
    =/  maz=(list mass)
      =+  [known alien]=(skid ~(val by peers.ames-state) |=(^ =(%known +<-)))
      :~  peers-known+&+known
          peers-alien+&+alien
      ==
    ``mass+!>(maz)
  ::  only respond for the local identity, %$ desk, current timestamp
  ::
  ?.  ?&  =(&+our why)
          =([%$ %da now] lot)
          =(%$ syd)
      ==
    ?.  for.veb.bug.ames-state  ~
    ~>  %slog.0^leaf/"ames: scry-fail {<why=why lot=lot now=now syd=syd>}"
    ~
  ::  /ax/protocol/version           @
  ::  /ax/peers                      (map ship ?(%alien %known))
  ::  /ax/peers/[ship]               ship-state
  ::  /ax/peers/[ship]/forward-lane  (list lane)
  ::  /ax/bones/[ship]               [snd=(set bone) rcv=(set bone)]
  ::  /ax/snd-bones/[ship]/[bone]    vase
  ::  /ax/corks                      (list wire)
  ::
  ?.  ?=(%x ren)  ~
  ?+    tyl  ~
      [%protocol %version ~]
    ``noun+!>(protocol-version)
  ::
      [%peers ~]
    :^  ~  ~  %noun
    !>  ^-  (map ship ?(%alien %known))
    (~(run by peers.ames-state) head)
  ::
      [%peers @ *]
    =/  who  (slaw %p i.t.tyl)
    ?~  who  [~ ~]
    =/  peer  (~(get by peers.ames-state) u.who)
    ?+    t.t.tyl  [~ ~]
        ~
      ?~  peer
        [~ ~]
      ``noun+!>(u.peer)
    ::
        [%forward-lane ~]
      ::
      ::  this duplicates the routing hack from +send-blob:event-core
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
      ?.  ?&  ?=([~ %known *] peer)
              !=(our u.who)
          ==
        ~
      =;  zar=(trap (list lane))
        ?:  (lth last-contact.qos.u.peer (sub now ~h1))
          $:zar
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
      [%bones @ ~]
    =/  who  (slaw %p i.t.tyl)
    ?~  who  [~ ~]
    =/  per  (~(get by peers.ames-state) u.who)
    ?.  ?=([~ %known *] per)  [~ ~]
    =/  res
      =,  u.per
      [snd=~(key by snd) rcv=~(key by rcv)]
    ``noun+!>(res)
  ::
      [%snd-bones @ @ ~]
    =/  who  (slaw %p i.t.tyl)
    ?~  who  [~ ~]
    =/  ost  (slaw %ud i.t.t.tyl)
    ?~  ost  [~ ~]
    =/  per  (~(get by peers.ames-state) u.who)
    ?.  ?=([~ %known *] per)  [~ ~]
    =/  mps  (~(get by snd.u.per) u.ost)
    ?~  mps  [~ ~]
    =/  res
      u.mps
    ``noun+!>(!>(res))
  ::
      [%corks ~]
    ``noun+!>(~(tap in corks.ames-state))
  ==
--
::  |per-event: inner event-handling core
::
~%  %per-event  ..decode-packet  ~
|%
++  per-event
  =|  moves=(list move)
  ~%  %event-gate  ..per-event  ~
  |=  [[now=@da eny=@ rof=roof] =duct =ames-state]
  =*  veb  veb.bug.ames-state
  =|  cork-bone=(unit bone)  ::  modified by +on-kroc
  ~%  %event-core  ..$  ~
  |%
  ++  event-core  .
  ++  abet  [(flop moves) ames-state]
  ++  emit  |=(=move event-core(moves [move moves]))
  ++  channel-state  [life crypto-core bug]:ames-state
  ++  trace
    |=  [verb=? =ship print=(trap tape)]
    ^+  same
    (^trace verb ship ships.bug.ames-state print)
  ::  +on-take-done: handle notice from vane that it processed a message
  ::
  ++  on-take-done
    |=  [=wire error=(unit error)]
    ^+  event-core
    ::  relay the vane ack to the foreign peer
    ::
    ?~  parsed=(parse-bone-wire wire)
      ::  no-op
      ::
      ~>  %slog.0^leaf/"ames: dropping malformed wire: {(spud wire)}"
      event-core
    ?>  ?=([@ her=ship *] u.parsed)
    =*  her          her.u.parsed
    =/  =peer-state  (got-peer-state her)
    =/  =channel     [[our her] now channel-state -.peer-state]
    =/  peer-core    (make-peer-core peer-state channel)
    |^
    ?:  ?&  ?=([%new *] u.parsed)
            (lth rift.u.parsed rift.peer-state)
        ==
      ::  ignore events from an old rift
      ::
      %-  %^  trace  odd.veb  her
          |.("dropping old rift wire: {(spud wire)}")
      event-core
    =/  =bone
      ?-(u.parsed [%new *] bone.u.parsed, [%old *] bone.u.parsed)
    =?  peer-core  ?=([%old *] u.parsed)
      %-  %^  trace  odd.veb  her
          |.("parsing old wire: {(spud wire)}")
      peer-core
    ?~  error
      (send-ack bone)
    (send-nack bone u.error)
    ::
    ::  if processing succeded, send positive ack packet and exit
    ::
    ++  send-ack
      |=  =bone
      ^+  event-core
      =/  cork=?  (~(has in closing.peer-state) bone)
      abet:(run-message-sink:peer-core bone %done ok=%.y cork)
    ::  failed; send message nack packet
    ::
    ++  send-nack
      |=  [=bone =^error]
      ^+  event-core
      =.  event-core
        abet:(run-message-sink:peer-core bone %done ok=%.n cork=%.n)
      =/  =^peer-state  (got-peer-state her)
      =/  =^channel     [[our her] now channel-state -.peer-state]
      ::  construct nack-trace message, referencing .failed $message-num
      ::
      =/  failed=message-num  last-acked:(~(got by rcv.peer-state) bone)
      =/  =naxplanation  [failed error]
      =/  =message-blob  (jam naxplanation)
      ::  send nack-trace message on associated .nack-trace-bone
      ::
      =.  peer-core              (make-peer-core peer-state channel)
      =/  nack-trace-bone=^bone  (mix 0b10 bone)
      ::
      abet:(run-message-pump:peer-core nack-trace-bone %memo message-blob)
    --
  ::  +on-sift: handle request to filter debug output by ship
  ::
  ++  on-sift
    |=  ships=(list ship)
    ^+  event-core
    =.  ships.bug.ames-state  (sy ships)
    event-core
  ::  +on-snub: handle request to change ship blacklist
  ::
  ++  on-snub
    |=  ships=(list ship)
    ^+  event-core
    =.  snub.ames-state  (sy ships)
    event-core
  ::  +on-spew: handle request to set verbosity toggles on debug output
  ::
  ++  on-spew
    |=  verbs=(list verb)
    ^+  event-core
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
      ==
    event-core
  ::  +on-prod: re-send a packet per flow to each of .ships
  ::
  ++  on-prod
    |=  ships=(list ship)
    ^+  event-core
    =?  ships  =(~ ships)  ~(tap in ~(key by peers.ames-state))
    |^  ^+  event-core
    ?~  ships  event-core
    $(ships t.ships, event-core (prod-peer i.ships))
    ::
    ++  prod-peer
      |=  her=ship
      ^+  event-core
      =/  par  (get-peer-state her)
      ?~  par  event-core
      =/  =channel  [[our her] now channel-state -.u.par]
      =/  peer-core  (make-peer-core u.par channel)
      =/  bones  ~(tap in ~(key by snd.u.par))
      |-  ^+  event-core
      ?~  bones  abet:peer-core
      =.  peer-core  (run-message-pump:peer-core i.bones %prod ~)
      $(bones t.bones)
    --
  ::  +on-cong: adjust congestion control parameters
  ::
  ++  on-cong
    |=  [msg=@ud mem=@ud]
    ^+  event-core
    =.  cong.ames-state  msg^mem
    event-core
  ::  +on-stir: recover from timer desync, setting new timers as needed
  ::
  ::    .arg is unused, meant to ease future debug commands
  ::
  ++  on-stir
    |=  arg=@t
    ^+  event-core
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
        (rof ~ %bx [[our %$ da+now] /debug/timers])
      (skim tim |=([@da hen=^duct] ?=([[%ames ?(%pump %recork) *] *] hen)))
    ::
    ::  set timers for flows that should have one set but don't
    ::
    =.  event-core
      %-  ~(rep in (~(dif in want) have))
      |=  [[wen=@da hen=^duct] this=_event-core]
      ?>  ?=([^ *] hen)
      (emit:this ~[/ames] %pass t.i.hen %b %wait wen)
    ::
    ::  cancel timers for flows that have one set but shouldn't
    ::
    %-  ~(rep in (~(dif in have) want))
    |=  [[wen=@da hen=^duct] this=_event-core]
    ?>  ?=([^ *] hen)
    (emit:this t.hen %pass t.i.hen %b %rest wen)
  ::  +on-crud: handle event failure; print to dill
  ::
  ++  on-crud
    |=  =error
    ^+  event-core
    (emit duct %pass /crud %d %flog %crud error)
  ::  +on-heed: handle request to track .ship's responsiveness
  ::
  ++  on-heed
    |=  =ship
    ^+  event-core
    =/  ship-state  (~(get by peers.ames-state) ship)
    ?.  ?=([~ %known *] ship-state)
      %+  enqueue-alien-todo  ship
      |=  todos=alien-agenda
      todos(heeds (~(put in heeds.todos) duct))
    ::
    =/  =peer-state  +.u.ship-state
    =/  =channel     [[our ship] now channel-state -.peer-state]
    abet:on-heed:(make-peer-core peer-state channel)
  ::  +on-jilt: handle request to stop tracking .ship's responsiveness
  ::
  ++  on-jilt
    |=  =ship
    ^+  event-core
    =/  ship-state  (~(get by peers.ames-state) ship)
    ?.  ?=([~ %known *] ship-state)
      %+  enqueue-alien-todo  ship
      |=  todos=alien-agenda
      todos(heeds (~(del in heeds.todos) duct))
    ::
    =/  =peer-state  +.u.ship-state
    =/  =channel     [[our ship] now channel-state -.peer-state]
    abet:on-jilt:(make-peer-core peer-state channel)
  ::  +on-hear: handle raw packet receipt
  ::
  ++  on-hear
    |=  [l=lane b=blob d=(unit goof)]
    (on-hear-packet l (decode-packet b) d)
  ::  +on-hear-packet: handle mildly processed packet receipt
  ::
  ++  on-hear-packet
    ~/  %on-hear-packet
    |=  [=lane =packet dud=(unit goof)]
    ^+  event-core
    %-  (trace odd.veb sndr.packet |.("received packet"))
    ::
    ?:  =(our sndr.packet)
      event-core
    ?:  (~(has in snub.ames-state) sndr.packet)
      %-  (trace rcv.veb sndr.packet |.("snubbed"))
      event-core
    ::
    %.  +<
    ::
    ?.  =(our rcvr.packet)
      on-hear-forward
    ::
    ?:  =(%keys content.packet)
      on-hear-keys
    ?:  ?&  ?=(%pawn (clan:title sndr.packet))
            !?=([~ %known *] (~(get by peers.ames-state) sndr.packet))
        ==
      on-hear-open
    on-hear-shut
  ::  +on-hear-forward: maybe forward a packet to someone else
  ::
  ::    Note that this performs all forwarding requests without
  ::    filtering.  Any protection against DDoS amplification will be
  ::    provided by Vere.
  ::
  ++  on-hear-forward
    ~/  %on-hear-forward
    |=  [=lane =packet dud=(unit goof)]
    ^+  event-core
    %-  %^  trace  for.veb  sndr.packet
        |.("forward: {<sndr.packet>} -> {<rcvr.packet>}")
    ::  set .origin.packet if it doesn't already have one, re-encode, and send
    ::
    =?    origin.packet
        &(?=(~ origin.packet) !=(%czar (clan:title sndr.packet)))
      ?:  ?=(%& -.lane)
        ~
      ?.  (lte (met 3 p.lane) 6)
        ~|  ames-lane-size+p.lane  !!
      `p.lane
    ::
    =/  =blob  (encode-packet packet)
    (send-blob & rcvr.packet blob)
  ::  +on-hear-keys: handle receipt of attestion request
  ::
  ++  on-hear-keys
    ~/  %on-hear-keys
    |=  [=lane =packet dud=(unit goof)]
    =+  %^  trace  msg.veb  sndr.packet
        |.("requested attestation")
    ?.  =(%pawn (clan:title our))
      event-core
    (send-blob | sndr.packet (attestation-packet sndr.packet 1))
  ::  +on-hear-open: handle receipt of plaintext comet self-attestation
  ::
  ++  on-hear-open
    ~/  %on-hear-open
    |=  [=lane =packet dud=(unit goof)]
    ^+  event-core
    =+  %^  trace  msg.veb  sndr.packet
        |.("got attestation")
    ::  assert the comet can't pretend to be a moon or other address
    ::
    ?>  ?=(%pawn (clan:title sndr.packet))
    ::  if we already know .sndr, ignore duplicate attestation
    ::
    =/  ship-state  (~(get by peers.ames-state) sndr.packet)
    ?:  ?=([~ %known *] ship-state)
      event-core
    ::
    =/  =open-packet  (decode-open-packet packet our life.ames-state)
    ::  add comet as an %alien if we haven't already
    ::
    =?  peers.ames-state  ?=(~ ship-state)
      (~(put by peers.ames-state) sndr.packet %alien *alien-agenda)
    ::  upgrade comet to %known via on-publ-full
    ::
    =.  event-core
      =/  crypto-suite=@ud  1
      =/  keys
        (my [sndr-life.open-packet crypto-suite public-key.open-packet]~)
      =/  =point
        :*  ^=     rift  0
            ^=     life  sndr-life.open-packet
            ^=     keys  keys
            ^=  sponsor  `(^sein:title sndr.packet)
        ==
      (on-publ / [%full (my [sndr.packet point]~)])
    ::  manually add the lane to the peer state
    ::
    =.  peers.ames-state
      =/  =peer-state  (gut-peer-state sndr.packet)
      =.  route.peer-state  `[direct=%.n lane]
      (~(put by peers.ames-state) sndr.packet %known peer-state)
    ::
    event-core
  ::  +on-hear-shut: handle receipt of encrypted packet
  ::
  ++  on-hear-shut
    ~/  %on-hear-shut
    |=  [=lane =packet dud=(unit goof)]
    ^+  event-core
    =/  sndr-state  (~(get by peers.ames-state) sndr.packet)
    ::  If we don't know them, ask Jael for their keys. If they're a
    ::  comet, this will also cause us to request a self-attestation
    ::  from the sender. The packet itself is dropped; we can assume it
    ::  will be resent.
    ::
    ?.  ?=([~ %known *] sndr-state)
      (enqueue-alien-todo sndr.packet |=(alien-agenda +<))
    ::  decrypt packet contents using symmetric-key.channel
    ::
    ::    If we know them, we have a $channel with them, which we've
    ::    populated with a .symmetric-key derived from our private key
    ::    and their public key using elliptic curve Diffie-Hellman.
    ::
    =/  =peer-state   +.u.sndr-state
    =/  =channel      [[our sndr.packet] now channel-state -.peer-state]
    ~|  %ames-crash-on-packet-from^her.channel
    =/  =shut-packet
      (decode-shut-packet packet [symmetric-key her-life our-life]:channel)
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
      ?:  ?=(~ origin.packet)
        `[direct=%.y lane]
      ?:  ?=([~ %& *] route.peer-state)
        ?:  =(lane.u.route.peer-state |+u.origin.packet)
          route.peer-state
        `[direct=%.n |+u.origin.packet]
      `[direct=%.n |+u.origin.packet]
    ::  perform peer-specific handling of packet
    ::
    =/  peer-core  (make-peer-core peer-state channel)
    abet:(on-hear-shut-packet:peer-core lane shut-packet dud)
  ::  +on-take-boon: receive request to give message to peer
  ::
  ++  on-take-boon
    |=  [=wire payload=*]
    ^+  event-core
    ?~  parsed=(parse-bone-wire wire)
      ~>  %slog.0^leaf/"ames: dropping malformed wire: {(spud wire)}"
      event-core
    ::
    ?>  ?=([@ her=ship *] u.parsed)
    =*  her          her.u.parsed
    =/  =peer-state  (got-peer-state her)
    =/  =channel     [[our her] now channel-state -.peer-state]
    =/  peer-core    (make-peer-core peer-state channel)
    ::
    ?:  ?&  ?=([%new *] u.parsed)
            (lth rift.u.parsed rift.peer-state)
        ==
      ::  ignore events from an old rift
      ::
      %-  %^  trace  odd.veb  her
          |.("dropping old rift wire: {(spud wire)}")
      event-core
    =/  =bone
      ?-(u.parsed [%new *] bone.u.parsed, [%old *] bone.u.parsed)
    =?  peer-core  ?=([%old *] u.parsed)
      %-  %^  trace  odd.veb  her
          |.("parsing old wire: {(spud wire)}")
      peer-core
    abet:(on-memo:peer-core bone payload %boon)
  ::  +on-plea: handle request to send message
  ::
  ++  on-plea
    |=  [=ship =plea]
    ^+  event-core
    ::  since flow kill goes like:
    ::  client vane cork task -> client ames pass cork as plea ->
    ::  -> server ames sinks plea -> server ames +on-plea (we are here);
    ::  if it's %cork plea passed to ames from its sink,
    ::  give %done and process flow closing after +on-take-done call
    ::
    ?:  =([%a /close ~] plea)
      (emit duct %give %done ~)
    ::  .plea is from local vane to foreign ship
    ::
    =/  ship-state  (~(get by peers.ames-state) ship)
    ::
    ?.  ?=([~ %known *] ship-state)
      %+  enqueue-alien-todo  ship
      |=  todos=alien-agenda
      todos(messages [[duct plea] messages.todos])
    ::
    =/  =peer-state  +.u.ship-state
    =/  =channel     [[our ship] now channel-state -.peer-state]
    ::
    =^  =bone  ossuary.peer-state  (bind-duct ossuary.peer-state duct)
    %-  %^  trace  msg.veb  ship
        |.  ^-  tape
        =/  sndr  [our our-life.channel]
        =/  rcvr  [ship her-life.channel]
        "plea {<sndr rcvr bone=bone vane.plea path.plea>}"
    abet:(on-memo:(make-peer-core peer-state channel) bone plea %plea)
  ::  +on-cork: handle request to kill a flow
  ::
  ++  on-cork
    |=  =ship
    ^+  event-core
    =/  =plea       [%$ /flow [%cork ~]]
    =/  ship-state  (~(get by peers.ames-state) ship)
    ?.  ?=([~ %known *] ship-state)
      %+  enqueue-alien-todo  ship
      |=  todos=alien-agenda
      todos(messages [[duct plea] messages.todos])
    =/  =peer-state  +.u.ship-state
    =/  =channel     [[our ship] now channel-state -.peer-state]
    ::
    =/  [=bone ossuary=_ossuary.peer-state]
      ?^  cork-bone  [u.cork-bone ossuary.peer-state]
      (bind-duct ossuary.peer-state duct)
    =.  ossuary.peer-state  ossuary
    ::
    ?.  (~(has by by-bone.ossuary.peer-state) bone)
      %.  event-core
      %^  trace  odd.veb  ship
      |.("trying to cork {<bone=bone>}, not in the ossuary, ignoring")
    ::
    =.  closing.peer-state  (~(put in closing.peer-state) bone)
    %-  %^  trace  msg.veb  ship
        |.  ^-  tape
        =/  sndr  [our our-life.channel]
        =/  rcvr  [ship her-life.channel]
        "cork plea {<sndr rcvr bone=bone vane.plea path.plea>}"
    abet:(on-memo:(make-peer-core peer-state channel) bone plea %plea)
  ::  +on-kroc: cork all flows from failed subscriptions
  ::
  ++  on-kroc
    |=  dry=?
    ^+  event-core
    ::
    =;  [corks=@ core=_event-core]
      ?.  dry  core
      %.(core (slog leaf/"ames: #{<corks>} flows can be corked" ~))
    ::
    %+  roll  ~(tap by peers.ames-state)
    |=  [[=ship =ship-state] corks=@ core=_event-core]
    ?.  ?=(%known -.ship-state)
      corks^core
    =/  =peer-state:ames  ?>(?=(%known -.ship-state) +.ship-state)
    =/  subs=(jar path [bone sub-nonce=@])
      %+  roll  ~(tap by snd.peer-state)
      |=  [[=forward=bone *] subs=(jar path [bone sub-nonce=@])]
      ?:  (~(has in closing.peer-state) forward-bone)
        subs
      ?~  duct=(~(get by by-bone.ossuary.peer-state) forward-bone)
        subs
      ?.  ?=([* [%gall %use sub=@ @ %out @ @ nonce=@ pub=@ *] *] u.duct)
        subs
      =/  =wire           i.t.u.duct
      =/  nonce=(unit @)  (rush (snag 7 wire) dem)
      %-  ~(add ja subs)
      ::  0 for old pre-nonce subscriptions
      ::
      :_  [forward-bone ?~(nonce 0 u.nonce)]
      ?~  nonce  wire
      ::  don't include the sub-nonce in the key
      ::
      (weld (scag 7 wire) (slag 8 wire))
    %+  roll  ~(tap by subs)
    |=  [[=wire flows=(list [bone sub-nonce=@])] corks=_corks core=_core]
    ::
    %-  tail
    %+  roll  (sort flows |=([[@ n=@] [@ m=@]] (lte n m)))
    |=  [[=bone nonce=@] resubs=_(lent flows) corks=_corks core=_core]
    =/  app=term  ?>(?=([%gall %use sub=@ *] wire) i.t.t.wire)
    =/  =path     (slag 7 wire)
    =/  log=tape  "[bone={<bone>} agent={<app>} nonce={<nonce>}] {<path>}"
    =;  corkable=?
      =?  corks  corkable  +(corks)
      =?  core   &(corkable !dry)  (%*(on-cork core cork-bone `bone) ship)
      (dec resubs)^corks^core
    ::  checks if this is a stale re-subscription
    ::
    ?.  =(resubs 1)
      %.  &
      (trace &(dry odd.veb) ship |.((weld "stale %watch plea " log)))
    ::  the current subscription can be safely corked if there
    ::  is a flow with a naxplanation ack  on a backward bone
    ::
    =+  backward-bone=(mix 0b10 bone)
    ?.  =(2 (mod backward-bone 4))
      |
    ?~  (~(get by rcv.peer-state) backward-bone)
      |
    %.  &
    (trace &(dry odd.veb) ship |.((weld "failed %watch plea " log)))
  ::  +on-take-wake: receive wakeup or error notification from behn
  ::
  ++  on-take-wake
    |=  [=wire error=(unit tang)]
    ^+  event-core
    ::
    ?:  ?=([%alien @ ~] wire)
      ::  if we haven't received an attestation, ask again
      ::
      ?^  error
        %-  (slog 'ames: attestation timer failed' u.error)
        event-core
      ?~  ship=`(unit @p)`(slaw %p i.t.wire)
        %-  (slog leaf+"ames: got timer for strange wire: {<wire>}" ~)
        event-core
      =/  ship-state  (~(get by peers.ames-state) u.ship)
      ?:  ?=([~ %known *] ship-state)
        event-core
      (request-attestation u.ship)
    ::
    ?.  ?=([%recork ~] wire)
      =/  res=(unit [her=ship =bone])  (parse-pump-timer-wire wire)
      ?~  res
        %-  (slog leaf+"ames: got timer for strange wire: {<wire>}" ~)
        event-core
      ::
      =/  state=(unit peer-state)  (get-peer-state her.u.res)
      ?~  state
        %.  event-core
        %-  slog
        [leaf+"ames: got timer for strange ship: {<her.u.res>}, ignoring" ~]
      ::
      =/  =channel  [[our her.u.res] now channel-state -.u.state]
      abet:(on-wake:(make-peer-core u.state channel) bone.u.res error)
    ::
    =.  event-core
      (emit duct %pass /recork %b %wait `@da`(add now ~d1))
    ::
    ?^  error
      %-  (slog 'ames: recork timer failed' u.error)
      event-core
    ::  recork up to one bone per peer
    ::
    =/  pez  ~(tap by peers.ames-state)
    |-  ^+  event-core
    ?~  pez  event-core
    =+  [her sat]=i.pez
    ?.  ?=(%known -.sat)
      $(pez t.pez)
    =*  peer-state  +.sat
    =/  =channel  [[our her] now channel-state -.peer-state]
    =/  peer-core  (make-peer-core peer-state channel)
    $(pez t.pez, event-core abet:recork-one:peer-core)
  ::  +on-init: first boot; subscribe to our info from jael
  ::
  ++  on-init
    ^+  event-core
    ::
    =~  (emit duct %pass /turf %j %turf ~)
        (emit duct %pass /private-keys %j %private-keys ~)
    ==
  ::  +on-priv: set our private key to jael's response
  ::
  ++  on-priv
    |=  [=life vein=(map life private-key)]
    ^+  event-core
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
    event-core
  ::  +on-publ: update pki data for peer or self
  ::
  ++  on-publ
    |=  [=wire =public-keys-result]
    ^+  event-core
    ::
    |^  ^+  event-core
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
      ^+  event-core
      ::
      =/  ship-state  (~(get by peers.ames-state) ship)
      ::  we shouldn't be hearing about ships we don't care about
      ::
      ?~  ship-state
        ~>  %slog.0^leaf/"ames: breach unknown {<our ship>}"
        event-core
      ::  if an alien breached, this doesn't affect us
      ::
      ?:  ?=([~ %alien *] ship-state)
        ~>  %slog.0^leaf/"ames: breach alien {<our ship>}"
        event-core
      ~>  %slog.0^leaf/"ames: breach peer {<our ship>}"
      ::  a peer breached; drop messaging state
      ::
      =/  =peer-state  +.u.ship-state
      =/  old-qos=qos  qos.peer-state
      ::  cancel all timers related to .ship
      ::
      =.  event-core
        %+  roll  ~(tap by snd.peer-state)
        |=  [[=snd=bone =message-pump-state] core=_event-core]
        ^+  core
        ::
        ?~  next-wake=next-wake.packet-pump-state.message-pump-state
          core
        ::  note: copies +on-pump-rest:message-pump
        ::
        =/  wire  (make-pump-timer-wire ship snd-bone)
        =/  duct  ~[/ames]
        (emit:core duct %pass wire %b %rest u.next-wake)
      ::  reset all peer state other than pki data
      ::
      =.  +.peer-state  +:*^peer-state
      ::  print change to quality of service, if any
      ::
      =/  text=(unit tape)
        %^  qos-update-text  ship  old-qos
        [qos.peer-state kay.veb ships.bug.ames-state]
      ::
      =?  event-core  ?=(^ text)
        (emit duct %pass /qos %d %flog %text u.text)
      ::  reinitialize galaxy route if applicable
      ::
      =?  route.peer-state  =(%czar (clan:title ship))
        `[direct=%.y lane=[%& ship]]
      ::
      =.  peers.ames-state
        (~(put by peers.ames-state) ship [%known peer-state])
      ::
      event-core
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
      ^+  event-core
      ::
      =/  ship-state  (~(get by peers.ames-state) ship)
      ?.  ?=([~ %known *] ship-state)
        =|  =point
        =.  life.point     life
        =.  keys.point     (my [life crypto-suite public-key]~)
        =.  sponsor.point  `(^^sein:title rof our now ship)
        ::
        (on-publ-full (my [ship point]~))
      ::
      =/  =peer-state  +.u.ship-state
      ::
      =/  =private-key              sec:ex:crypto-core.ames-state
      =.  symmetric-key.peer-state
        (derive-symmetric-key public-key private-key)
      ::
      =.  life.peer-state           life
      =.  public-key.peer-state     public-key
      ::
      =.  peers.ames-state  (~(put by peers.ames-state) ship %known peer-state)
      event-core
    ::  +on-publ-sponsor: handle new or lost sponsor for peer
    ::
    ::    TODO: really handle sponsor loss
    ::
    ++  on-publ-sponsor
      |=  [=ship sponsor=(unit ship)]
      ^+  event-core
      ::
      ?~  sponsor
        %-  (slog leaf+"ames: {(scow %p ship)} lost sponsor, ignoring" ~)
        event-core
      ::
      =/  state=(unit peer-state)  (get-peer-state ship)
      ?~  state
        %-  (slog leaf+"ames: missing peer-state, ignoring" ~)
        event-core
      =.  sponsor.u.state   u.sponsor
      =.  peers.ames-state  (~(put by peers.ames-state) ship %known u.state)
      event-core
    ::  +on-publ-full: handle new pki data for peer(s)
    ::
    ++  on-publ-full
      |=  points=(map ship point)
      ^+  event-core
      ::
      =>  .(points ~(tap by points))
      |^  ^+  event-core
          ?~  points  event-core
          ::
          =+  ^-  [=ship =point]  i.points
          ::
          ?.  (~(has by keys.point) life.point)
            $(points t.points)
          ::
          =/  old-ship-state  (~(get by peers.ames-state) ship)
          ::
          =.  event-core  (insert-peer-state ship point)
          ::
          =?  event-core  ?=([~ %alien *] old-ship-state)
            (meet-alien ship point +.u.old-ship-state)
          ::
          $(points t.points)
      ::
      ++  meet-alien
        |=  [=ship =point todos=alien-agenda]
        ^+  event-core
        ::  if we're a comet, send self-attestation packet first
        ::
        =?  event-core  =(%pawn (clan:title our))
          (send-blob | ship (attestation-packet ship life.point))
        ::  save current duct
        ::
        =/  original-duct  duct
        ::  apply heeds
        ::
        =.  event-core
          %+  roll  ~(tap in heeds.todos)
          |=  [=^duct core=_event-core]
          (on-heed:core(duct duct) ship)
        ::  apply outgoing messages, reversing for FIFO order
        ::
        =.  event-core
          %+  reel  messages.todos
          |=  [[=^duct =plea] core=_event-core]
          ?:  ?=(%$ -.plea)
            (on-cork:core(duct duct) ship)
          (on-plea:core(duct duct) ship plea)
        ::  apply outgoing packet blobs
        ::
        =.  event-core
          %+  roll  ~(tap in packets.todos)
          |=  [=blob core=_event-core]
          (send-blob:core | ship blob)
        ::
        event-core(duct original-duct)
      --
    ::  on-publ-rift: XX
    ::
    ++  on-publ-rift
      |=  [=ship =rift]
      ^+  event-core
      ?~  ship-state=(~(get by peers.ames-state) ship)
        ::  print error here? %rift was probably called before %keys
        ::
        ~>  %slog.1^leaf/"ames: missing peer-state on-publ-rift"
        event-core
      ?:  ?=([%alien *] u.ship-state)
        ::  ignore aliens
        ::
        event-core
      =/  =peer-state       +.u.ship-state
      =.  rift.peer-state   rift
      =.  peers.ames-state  (~(put by peers.ames-state) ship %known peer-state)
      event-core
    ::
    ++  insert-peer-state
      |=  [=ship =point]
      ^+  event-core
      ::
      =/  =peer-state     (gut-peer-state ship)
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
        (^^sein:title rof our now ship)
      ::  automatically set galaxy route, since unix handles lookup
      ::
      =?  route.peer-state  ?=(%czar (clan:title ship))
        `[direct=%.y lane=[%& ship]]
      ::
      =.  peers.ames-state
        (~(put by peers.ames-state) ship %known peer-state)
      ::
      event-core
    --
  ::  +on-take-turf: relay %turf move from jael to unix
  ::
  ++  on-take-turf
    |=  turfs=(list turf)
    ^+  event-core
    ::
    (emit unix-duct.ames-state %give %turf turfs)
  ::  +on-born: handle unix process restart
  ::
  ++  on-born
    ^+  event-core
    ::
    =.  unix-duct.ames-state  duct
    ::
    =/  turfs
      ;;  (list turf)
      =<  q.q  %-  need  %-  need
      (rof ~ %j `beam`[[our %turf %da now] /])
    ::
    (emit unix-duct.ames-state %give %turf turfs)
  ::  +on-vega: handle kernel reload
  ::
  ++  on-vega  event-core
  ::  +on-trim: handle request to free memory
  ::
  ::  %ruin comets not seen for six months
  ::
  ++  on-trim
    ^+  event-core
    =;  rui=(set @p)
      (emit duct %pass /ruin %j %ruin rui)
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
  ::  +enqueue-alien-todo: helper to enqueue a pending request
  ::
  ::    Also requests key and life from Jael on first request.
  ::    If talking to a comet, requests attestation packet.
  ::
  ++  enqueue-alien-todo
    |=  [=ship mutate=$-(alien-agenda alien-agenda)]
    ^+  event-core
    ::
    =/  ship-state  (~(get by peers.ames-state) ship)
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
    ?:  already-pending
      event-core
    ::
    ?:  =(%pawn (clan:title ship))
      (request-attestation ship)
    ::  NB: we specifically look for this wire in +public-keys-give in
    ::  Jael.  if you change it here, you must change it there.
    ::
    (emit duct %pass /public-keys %j %public-keys [n=ship ~ ~])
  ::  +request-attestation: helper to request attestation from comet
  ::
  ::    Also sets a timer to resend the request every 30s.
  ::
  ++  request-attestation
    |=  =ship
    ^+  event-core
    =+  (trace msg.veb ship |.("requesting attestion"))
    =.  event-core  (send-blob | ship (sendkeys-packet ship))
    =/  =wire  /alien/(scot %p ship)
    (emit duct %pass wire %b %wait (add now ~s30))
  ::  +send-blob: fire packet at .ship and maybe sponsors
  ::
  ::    Send to .ship and sponsors until we find a direct lane,
  ::    skipping .our in the sponsorship chain.
  ::
  ::    If we have no PKI data for a recipient, enqueue the packet and
  ::    request the information from Jael if we haven't already.
  ::
  ++  send-blob
    ~/  %send-blob
    |=  [for=? =ship =blob]
    ::
    =/  final-ship  ship
    %-  (trace rot.veb final-ship |.("send-blob: to {<ship>}"))
    |-
    |^  ^+  event-core
        ::
        =/  ship-state  (~(get by peers.ames-state) ship)
        ::
        ?.  ?=([~ %known *] ship-state)
          ?:  ?=(%pawn (clan:title ship))
            (try-next-sponsor (^sein:title ship))
          %+  enqueue-alien-todo  ship
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
          ?:  for
            event-core
          (try-next-sponsor sponsor.peer-state)
        ::  if forwarding, route must not be stale
        ::
        ?:  &(for (lth last-contact.qos.peer-state (sub now ~h1)))
          (try-next-sponsor sponsor.peer-state)
        ::
        ?~  route=route.peer-state
          %-  (trace rot.veb final-ship |.("no route to:  {<ship>}"))
          (try-next-sponsor sponsor.peer-state)
        ::
        %-  (trace rot.veb final-ship |.("trying route: {<ship>}"))
        =.  event-core
          (emit unix-duct.ames-state %give %send lane.u.route blob)
        ::
        ?:  direct.u.route
          event-core
        (try-next-sponsor sponsor.peer-state)
    ::
    ++  try-next-sponsor
      |=  sponsor=^ship
      ^+  event-core
      ::
      ?:  =(ship sponsor)
        event-core
      ^$(ship sponsor)
    --
  ::  +attestation-packet: generate signed self-attestation for .her
  ::
  ::    Sent by a comet on first contact with a peer.  Not acked.
  ::
  ++  attestation-packet
    |=  [her=ship =her=life]
    ^-  blob
    %-  encode-packet
    %-  encode-open-packet
    :_  crypto-core.ames-state
    :*  ^=  public-key  pub:ex:crypto-core.ames-state
        ^=        sndr  our
        ^=   sndr-life  life.ames-state
        ^=        rcvr  her
        ^=   rcvr-life  her-life
    ==
  ::  +sendkeys-packet: generate a request for a self-attestation.
  ::
  ::    Sent by non-comets to comets.  Not acked.
  ::
  ++  sendkeys-packet
    |=  her=ship
    ^-  blob
    ?>  ?=(%pawn (clan:title her))
    %-  encode-packet
    (encode-keys-packet our her life.ames-state)
  ::  +get-peer-state: lookup .her state or ~
  ::
  ++  get-peer-state
    |=  her=ship
    ^-  (unit peer-state)
    ::
    =-  ?.(?=([~ %known *] -) ~ `+.u)
    (~(get by peers.ames-state) her)
  ::  +got-peer-state: lookup .her state or crash
  ::
  ++  got-peer-state
    |=  her=ship
    ^-  peer-state
    ::
    ~|  %freaky-alien^her
    =-  ?>(?=(%known -<) ->)
    (~(got by peers.ames-state) her)
  ::  +gut-peer-state: lookup .her state or default
  ::
  ++  gut-peer-state
    |=  her=ship
    ^-  peer-state
    =/  ship-state  (~(get by peers.ames-state) her)
    ?.  ?=([~ %known *] ship-state)
      *peer-state
    +.u.ship-state
  ::  +make-peer-core: create nested |peer-core for per-peer processing
  ::
  ++  make-peer-core
    |=  [=peer-state =channel]
    =*  veb  veb.bug.channel
    |%
    ++  peer-core  .
    ++  emit  |=(move peer-core(event-core (^emit +<)))
    ++  abet
      ^+  event-core
      ::
      =.  peers.ames-state
        (~(put by peers.ames-state) her.channel %known peer-state)
      ::
      event-core
    ++  trace
      |=  [verb=? print=(trap tape)]
      ^+  same
      (^trace verb her.channel print)
    ++  on-heed  peer-core(heeds.peer-state (~(put in heeds.peer-state) duct))
    ++  on-jilt  peer-core(heeds.peer-state (~(del in heeds.peer-state) duct))
    ::  +update-qos: update and maybe print connection status
    ::
    ++  update-qos
      |=  =new=qos
      ^+  peer-core
      ::
      =^  old-qos  qos.peer-state  [qos.peer-state new-qos]
      ::  if no update worth reporting, we're done
      ::
      =/  text
        %^  qos-update-text  her.channel  old-qos
        [new-qos kay.veb ships.bug.ames-state]
      ?~  text
        peer-core
      ::  print message
      ::
      =.  peer-core  (emit duct %pass /qos %d %flog %text u.text)
      ::  if peer has stopped responding, check if %boon's are backing up
      ::
      ?.  ?=(?(%dead %unborn) -.qos.peer-state)
        peer-core
      check-clog
    ::  +check-clog: notify clients if peer has stopped responding
    ::
    ++  check-clog
      ^+  peer-core
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
      ::
      =/  clogged=?
        |^  &(nuf-messages nuf-memory)
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
      ::  if clogged, notify client vane
      ::
      ?.  clogged
        peer-core
      %+  roll  ~(tap in heeds.peer-state)
      |=([d=^duct core=_peer-core] (emit:core d %give %clog her.channel))
    ::  +on-hear-shut-packet: handle receipt of ack or message fragment
    ::
    ++  on-hear-shut-packet
      |=  [=lane =shut-packet dud=(unit goof)]
      ^+  peer-core
      ::  update and print connection status
      ::
      =.  peer-core  (update-qos %live last-contact=now)
      ::
      =/  =bone  bone.shut-packet
      ::
      ?:  ?=(%& -.meat.shut-packet)
        =+  ?.  &(?=(^ dud) msg.veb)  ~
            %.  ~
            %-  slog
            :_  tang.u.dud
            leaf+"ames: {<her.channel>} fragment crashed {<mote.u.dud>}"
        (run-message-sink bone %hear lane shut-packet ?=(~ dud))
      ::  benign ack on corked bone
      ::
      ?:  (~(has in corked.peer-state) bone)
        peer-core
      ::  Just try again on error, printing trace
      ::
      ::    Note this implies that vanes should never crash on %done,
      ::    since we have no way to continue using the flow if they do.
      ::
      =+  ?~  dud  ~
          %.  ~
          %+  slog  leaf+"ames: {<her.channel>} ack crashed {<mote.u.dud>}"
          ?.  msg.veb  ~
          :-  >[bone=bone message-num=message-num meat=meat]:shut-packet<
          tang.u.dud
      (run-message-pump bone %hear [message-num +.meat]:shut-packet)
    ::  +on-memo: handle request to send message
    ::
    ++  on-memo
      |=  [=bone payload=* valence=?(%plea %boon)]
      ^+  peer-core
      ?:  ?&  (~(has in closing.peer-state) bone)
              !=(payload [%$ /flow %cork ~])
          ==
        ~>  %slog.0^leaf/"ames: ignoring message on closing bone {<bone>}"
        peer-core
      ?:  (~(has in corked.peer-state) bone)
        ~>  %slog.0^leaf/"ames: ignoring message on corked bone {<bone>}"
        peer-core
      ::
      =/  =message-blob  (dedup-message (jim payload))
      =.  peer-core  (run-message-pump bone %memo message-blob)
      ::
      ?:  ?&  =(%boon valence)
              (gte now (add ~s30 last-contact.qos.peer-state))
          ==
        check-clog
      peer-core
    ::  +dedup-message: replace with any existing copy of this message
    ::
    ++  dedup-message
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
      ?~  snd-l
        peer-loop(peers-l t.peers-l)
      =/  blob-l=(list ^message-blob)
        ~(tap to unsent-messages.message-pump-state.i.snd-l)
      |-  ^+  message-blob
      =*  blob-loop  $
      ?^  blob-l
        ?:  =(i.blob-l message-blob)
          i.blob-l
        blob-loop(blob-l t.blob-l)
      ?~  unsent-fragments.message-pump-state.i.snd-l
        bone-loop(snd-l t.snd-l)
      ?:  =(message-blob fragment.i.unsent-fragments.message-pump-state.i.snd-l)
        `@`fragment.i.unsent-fragments.message-pump-state.i.snd-l
      bone-loop(snd-l t.snd-l)
    ::  +on-wake: handle timer expiration
    ::
    ++  on-wake
      |=  [=bone error=(unit tang)]
      ^+  peer-core
      ::  if we previously errored out, print and reset timer for later
      ::
      ::    This really shouldn't happen, but if it does, make sure we
      ::    don't brick either this messaging flow or Behn.
      ::
      ?^  error
        =.  peer-core
          (emit duct %pass /wake-fail %d %flog %crud %ames-wake u.error)
        ::
        ?~  message-pump-state=(~(get by snd.peer-state) bone)
          peer-core
        ?~  next-wake.packet-pump-state.u.message-pump-state
          peer-core
        ::  If we crashed because we woke up too early, assume another
        ::  timer is already set.
        ::
        ?:  (lth now.channel u.next-wake.packet-pump-state.u.message-pump-state)
          peer-core
        ::
        =/  =wire  (make-pump-timer-wire her.channel bone)
        (emit duct %pass wire %b %wait (add now.channel ~s30))
      ::  update and print connection state
      ::
      =.  peer-core  %-  update-qos
        =/  expiry=@da  (add ~s30 last-contact.qos.peer-state)
        =?    -.qos.peer-state
            (gte now.channel expiry)
          %dead
        qos.peer-state
      ::  expire direct route
      ::
      ::    If the peer is not responding, mark the .lane.route as
      ::    indirect.  The next packets we emit will be sent to the
      ::    receiver's sponsorship chain in case the receiver's
      ::    transport address has changed and this lane is no longer
      ::    valid.
      ::
      ::    If .her is a galaxy, the lane will always remain direct.
      ::
      =?    route.peer-state
          ?&  ?=(%dead -.qos.peer-state)
              ?=(^ route.peer-state)
              direct.u.route.peer-state
              !=(%czar (clan:title her.channel))
          ==
        route.peer-state(direct.u %.n)
      ::  resend comet attestation packet if first message times out
      ::
      ::    The attestation packet doesn't get acked, so if we tried to
      ::    send a packet but it timed out, maybe they didn't get our
      ::    attestation.
      ::
      ::    Only resend on timeout of packets in the first message we
      ::    send them, since they should remember forever.
      ::
      =?    event-core
          ?&  ?=(%pawn (clan:title our))
              =(1 current:(~(got by snd.peer-state) bone))
          ==
        (send-blob | her.channel (attestation-packet [her her-life]:channel))
      ?:  (~(has in corked.peer-state) bone)
        ::  if the bone was corked the flow doesn't exist anymore
        ::  TODO: clean up corked bones in the peer state when it's _safe_?
        ::        (e.g. if this bone is N blocks behind the next one)
        ::
        peer-core
      ::  maybe resend some timed out packets
      ::
      (run-message-pump bone %wake ~)
    ::  +send-shut-packet: fire encrypted packet at rcvr and maybe sponsors
    ::
    ++  send-shut-packet
      |=  =shut-packet
      ^+  peer-core
      ::  swizzle last bone bit before sending
      ::
      ::    The peer has the opposite perspective from ours about what
      ::    kind of flow this is (forward/backward), so flip the bit
      ::    here.
      ::
      =.  event-core
        %^  send-blob  |  her.channel
        %-  encode-packet
        %:  encode-shut-packet
          shut-packet(bone (mix 1 bone.shut-packet))
          symmetric-key.channel
          our               her.channel
          our-life.channel  her-life.channel
        ==
      peer-core
    ::  +recork-one: re-send the next %cork to the peer
    ::
    ++  recork-one
      ^+  peer-core
      =/  boz  (sort ~(tap in closing.peer-state) lte)
      |-  ^+  peer-core
      ?~  boz  peer-core
      =/  pum=message-pump-state  (~(got by snd.peer-state) i.boz)
      ?.  =(next current):pum
        $(boz t.boz)
      ::  sanity check on the message pump state
      ::
      ?.  ?&  =(~ unsent-messages.pum)
              =(~ unsent-fragments.pum)
              =(~ live.packet-pump-state.pum)
          ==
        ~>  %slog.0^leaf/"ames: bad pump state {<her.channel i.boz>}"
        $(boz t.boz)
      ::  no outstanding messages, so send a new %cork
      ::
      ::  TODO use +trace
      ~>  %slog.0^leaf/"ames: recork {<her.channel i.boz>}"
      =/  =plea  [%$ /flow [%cork ~]]
      (on-memo i.boz plea %plea)
    ::  +got-duct: look up $duct by .bone, asserting already bound
    ::
    ++  got-duct
      |=  =bone
      ^-  ^duct
      ~|  %dangling-bone^her.channel^bone
      (~(got by by-bone.ossuary.peer-state) bone)
    ::  +run-message-pump: process $message-pump-task and its effects
    ::
    ++  run-message-pump
      |=  [=bone task=message-pump-task]
      ^+  peer-core
      ::  pass .task to the |message-pump and apply state mutations
      ::
      =/  =message-pump-state
        (~(gut by snd.peer-state) bone *message-pump-state)
      ::
      =/  close=?  (~(has in closing.peer-state) bone)
      =+  message-pump=(make-message-pump message-pump-state channel close bone)
      =^  pump-gifts      message-pump-state  (work:message-pump task)
      =.  snd.peer-state  (~(put by snd.peer-state) bone message-pump-state)
      ::  process effects from |message-pump
      ::
      |^  ^+  peer-core
          ?~  pump-gifts  peer-core
          =*  gift  i.pump-gifts
          =.  peer-core
            ?-  -.gift
              %done  (on-pump-done [message-num error]:gift)
              %cork  (on-pump-cork current.message-pump-state)
              %kroc  (on-pump-kroc bone:gift)
              %send  (on-pump-send static-fragment.gift)
              %wait  (on-pump-wait date.gift)
              %rest  (on-pump-rest date.gift)
            ==
          $(pump-gifts t.pump-gifts)
      ::  +on-pump-done: handle |message-pump's report of message (n)ack
      ::
      ++  on-pump-done
        |=  [=message-num error=(unit error)]
        ^+  peer-core
        ?:  ?&  =(1 (end 0 bone))
                =(1 (end 0 (rsh 0 bone)))
                (~(has in corked.peer-state) (mix 0b10 bone))
            ==
          %-  %+  trace  msg.veb
              =/  dat  [her.channel bone=bone message-num=message-num -.task]
              |.("remove naxplanation flow {<dat>}")
          =.  snd.peer-state
            (~(del by snd.peer-state) bone)
          peer-core
        ::  if odd bone, ack is on "subscription update" message; no-op
        ::
        ?:  =(1 (end 0 bone))
          peer-core
        ::  even bone; is this bone a nack-trace bone?
        ::
        ?:  =(1 (end 0 (rsh 0 bone)))
          ::  nack-trace bone; assume .ok, clear nack from |message-sink
          ::
          =/  target-bone=^bone  (mix 0b10 bone)
          ::
          (run-message-sink target-bone %drop message-num)
        ?:  &(close ?=(%near -.task))
          ::  if the bone belongs to a closing flow and we got a naxplanation,
          ::  don't relay the ack to the client vane, and wait for the next try
          ::
          peer-core
        ::  not a nack-trace bone; relay ack to client vane
        ::
        (emit (got-duct bone) %give %done error)
      ::  +on-pump-cork: kill flow on cork sender side
      ::
      ++  on-pump-cork
        |=  =message-num
        ^+  peer-core
        ::  clear all packets from this message from the packet pump
        ::
        =.  message-pump  (run-packet-pump:message-pump %done message-num *@dr)
        =/  =wire  (make-pump-timer-wire her.channel bone)
        =/  nack-bone=^bone  (mix 0b10 bone)
        =?  rcv.peer-state  (~(has by rcv.peer-state) nack-bone)
          ::  if the publisher was behind we remove nacks received on that bone
          ::
          (~(del by rcv.peer-state) nack-bone)
        =.  peer-state
          =,  peer-state
          %_  peer-state
            snd              (~(del by snd) bone)
            rcv              (~(del by rcv) bone)
            corked           (~(put in corked) bone)
            closing          (~(del in closing) bone)
            krocs            (~(del in krocs) bone)
            by-duct.ossuary  (~(del by by-duct.ossuary) (got-duct bone))
            by-bone.ossuary  (~(del by by-bone.ossuary) bone)
          ==
        ::  since we got one cork ack, try the next one
        ::
        recork-one
      ::  +on-pump-kroc: if we get a nack for a cork, add it to the recork set
      ::
      ++  on-pump-kroc
        |=  =^bone
        ^+  peer-core
        =.  krocs.peer-state  (~(put in krocs.peer-state) bone)
        peer-core
      ::  +on-pump-send: emit message fragment requested by |message-pump
      ::
      ++  on-pump-send
        |=  f=static-fragment
        (send-shut-packet bone [message-num %& +]:f)
      ::  +on-pump-wait: relay |message-pump's set-timer request
      ::
      ++  on-pump-wait
        |=  date=@da
        ^+  peer-core
        ::
        =/  =wire  (make-pump-timer-wire her.channel bone)
        =/  duct   ~[/ames]
        (emit duct %pass wire %b %wait date)
      ::  +on-pump-rest: relay |message-pump's unset-timer request
      ::
      ++  on-pump-rest
        |=  date=@da
        ^+  peer-core
        ::
        =/  =wire  (make-pump-timer-wire her.channel bone)
        =/  duct   ~[/ames]
        (emit duct %pass wire %b %rest date)
      --
    ::  +run-message-sink: process $message-sink-task and its effects
    ::
    ++  run-message-sink
      |=  [=bone task=message-sink-task]
      ^+  peer-core
      ?:  (~(has in corked.peer-state) bone)  peer-core
      ::  pass .task to the |message-sink and apply state mutations
      ::
      =/  =message-sink-state
        (~(gut by rcv.peer-state) bone *message-sink-state)
      ::
      =/  message-sink    (make-message-sink message-sink-state channel)
      =/  closing=?       (~(has in closing.peer-state) bone)
      =^  sink-gifts      message-sink-state  (work:message-sink closing task)
      =.  rcv.peer-state  (~(put by rcv.peer-state) bone message-sink-state)
      ::  process effects from |message-sink
      ::
      |^  ^+  peer-core
          ?~  sink-gifts  peer-core
          =*  gift  i.sink-gifts
          =.  peer-core
            ?-  -.gift
              %memo  (on-sink-memo [message-num message]:gift)
              %send  (on-sink-send [message-num ack-meat]:gift)
              %cork  on-sink-cork
            ==
          $(sink-gifts t.sink-gifts)
      ::  +on-sink-cork: handle flow kill after server ames has taken %done
      ::
      ++  on-sink-cork
        ^+  peer-core
        =/  =message-pump-state
          (~(gut by snd.peer-state) bone *message-pump-state)
        =?  peer-core  ?=(^ next-wake.packet-pump-state.message-pump-state)
          =*  next-wake  u.next-wake.packet-pump-state.message-pump-state
          =/  =wire  (make-pump-timer-wire her.channel bone)
          :: resetting timer for boons
          ::
          (emit [/ames]~ %pass wire %b %rest next-wake)
        =/  nax-bone=^bone  (mix 0b10 bone)
        =?  peer-core  (~(has by snd.peer-state) nax-bone)
          %.  peer-core
          %+  trace  odd.veb
          =/  dat  [her.channel bone=nax-bone message-num=message-num -.task]
          |.("remove naxplanation flow {<dat>}")
        =.  peer-state
          =,  peer-state
          %_  peer-state
            ::  preemptively delete nax flows (e.g. nacks for initial %watches)
            ::
            snd      (~(del by (~(del by snd) bone)) nax-bone)
            rcv      (~(del by rcv) bone)
            corked   (~(put in corked) bone)
            closing  (~(del in closing) bone)
            krocs    (~(del in krocs) bone)
          ==
        peer-core
      ::  +on-sink-send: emit ack packet as requested by |message-sink
      ::
      ++  on-sink-send
        |=([num=message-num ack=ack-meat] (send-shut-packet bone num %| ack))
      ::  +on-sink-memo: dispatch message received by |message-sink
      ::
      ::    odd bone:                %plea request message
      ::    even bone, 0 second bit: %boon response message
      ::    even bone, 1 second bit: nack-trace %boon message
      ::
      ++  on-sink-memo
        ?:  =(1 (end 0 bone))
          on-sink-plea
        ?:  =(0 (end 0 (rsh 0 bone)))
          on-sink-boon
        on-sink-nack-trace
      ::  +on-sink-boon: handle response message received by |message-sink
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
      ::    TODO: This handles a previous crash in the client vane, but not in
      ::    Ames itself.
      ::
      ++  on-sink-boon
        |=  [=message-num message=*]
        ^+  peer-core
        ?:  ?|  (~(has in closing.peer-state) bone)
                (~(has in corked.peer-state) bone)
            ==
          peer-core
        ::  send ack unconditionally
        ::
        =.  peer-core  (emit (got-duct bone) %give %boon message)
        =.  peer-core  (run-message-sink bone %done ok=%.y cork=%.n)
        ::
        ?.  ?=([%hear * * ok=%.n] task)
          ::  fresh boon; give message to client vane
          ::
          %-  %+  trace  msg.veb
              =/  dat  [her.channel bone=bone message-num=message-num -.task]
              |.("sink boon {<dat>}")
          peer-core
        ::  we previously crashed on this message; notify client vane
        ::
        %-  %+  trace  msg.veb
            =/  dat  [her.channel bone=bone message-num=message-num -.task]
            |.("crashed on sink boon {<dat>}")
        boon-to-lost
      ::  +boon-to-lost: convert all boons to losts
      ::
      ++  boon-to-lost
        ^+  peer-core
        =.  moves
          %+  turn  moves
          |=  =move
          ?.  ?=([* %give %boon *] move)
            move
          [duct.move %give %lost ~]
        peer-core
      ::  +on-sink-nack-trace: handle nack-trace received by |message-sink
      ::
      ++  on-sink-nack-trace
        |=  [=message-num message=*]
        ^+  peer-core
        %-  %+  trace  msg.veb
            =/  dat  [her.channel bone=bone message-num=message-num]
            |.("sink naxplanation {<dat>}")
        ::
        =+  ;;  =naxplanation  message
        ::  ack nack-trace message (only applied if we don't later crash)
        ::
        =.  peer-core  (run-message-sink bone %done ok=%.y cork=%.n)
        ::  flip .bone's second bit to find referenced flow
        ::
        =/  target-bone=^bone  (mix 0b10 bone)
        ::  notify |message-pump that this message got naxplained
        ::
        =.  peer-core  (run-message-pump target-bone %near naxplanation)
        ::
        ?.  (~(has in krocs.peer-state) target-bone)
          peer-core
        ::  if we get a naxplanation for a %cork, the publisher is behind
        ::  receiving the OTA.  The /recork timer will retry eventually.
        ::
        %-  %+  trace  msg.veb
            |.("old publisher, %cork nacked on bone={<target-bone>}")
        peer-core
      ::  +on-sink-plea: handle request message received by |message-sink
      ::
      ++  on-sink-plea
        |=  [=message-num message=*]
        ^+  peer-core
        ?:  ?|  (~(has in closing.peer-state) bone)
                (~(has in corked.peer-state) bone)
            ==
          peer-core
        |^
        %-  %+  trace  msg.veb
            =/  dat  [her.channel bone=bone message-num=message-num]
            |.("sink plea {<dat>}")
        ::  is this the first time we're trying to process this message?
        ::
        ?:  ?=([%hear * * ok=%.n] task)
          ::  we previously crashed on this message; send nack
          ::
          nack-plea
        ::  fresh plea; pass to client vane
        ::
        =+  ;;  =plea  message
        =/  =wire  (make-bone-wire her.channel her-rift.channel bone)
        ::
        ?.  =(vane.plea %$)
          ?+  vane.plea  ~|  %ames-evil-vane^our^her.channel^vane.plea  !!
            %c  (emit duct %pass wire %c %plea her.channel plea)
            %g  (emit duct %pass wire %g %plea her.channel plea)
            %j  (emit duct %pass wire %j %plea her.channel plea)
          ==
        ::  a %cork plea is handled using %$ as the recipient vane to
        ::  account for publishers that still handle ames-to-ames %pleas
        ::
        ?>  &(?=([%cork *] payload.plea) ?=(%flow -.path.plea))
        =.  closing.peer-state  (~(put in closing.peer-state) bone)
        (emit duct %pass wire %a %plea her.channel [%a /close ~])
        ::
        ++  nack-plea
          ^+  peer-core
          =.  peer-core   (run-message-sink bone %done ok=%.n cork=%.n)
          ::  send nack-trace with blank .error for security
          ::
          =/  nack-trace-bone=^bone  (mix 0b10 bone)
          =/  =naxplanation  [message-num *error]
          =/  =message-blob  (jam naxplanation)
          ::
          (run-message-pump nack-trace-bone %memo message-blob)
        --
      --
    --
  --
::  +make-message-pump: constructor for |message-pump
::
++  make-message-pump
  |=  [state=message-pump-state =channel closing=? =bone]
  =*  veb  veb.bug.channel
  =|  gifts=(list message-pump-gift)
  ::
  |%
  ++  message-pump  .
  ++  give  |=(gift=message-pump-gift message-pump(gifts [gift gifts]))
  ++  packet-pump  (make-packet-pump packet-pump-state.state channel)
  ++  trace
    |=  [verb=? print=(trap tape)]
    ^+  same
    (^trace verb her.channel ships.bug.channel print)
  ::  +work: handle a $message-pump-task
  ::
  ++  work
    |=  task=message-pump-task
    ^+  [gifts state]
    ::
    =~  (dispatch-task task)
        feed-packets
        (run-packet-pump %halt ~)
        assert
        [(flop gifts) state]
    ==
  ::  +dispatch-task: perform task-specific processing
  ::
  ++  dispatch-task
    |=  task=message-pump-task
    ^+  message-pump
    ::
    ?-  -.task
      %prod  (run-packet-pump %prod ~)
      %memo  (on-memo message-blob.task)
      %wake  (run-packet-pump %wake current.state)
      %hear
        ?-    -.ack-meat.task
            %&
         (on-hear [message-num fragment-num=p.ack-meat]:task)
        ::
            %|
          =/  cork=?
            =/  top-live
              (pry:packet-queue:*make-packet-pump live.packet-pump-state.state)
            ::  If we send a %cork and get an ack, we can know by
            ::  sequence number that the ack is for the %cork message
            ::
            ?&  closing
                ?=(^ top-live)
                =(0 ~(wyt in unsent-messages.state))
                =(0 (lent unsent-fragments.state))
                =(1 ~(wyt by live.packet-pump-state.state))
                =(message-num:task message-num.key.u.top-live)
            ==
          =*  ack  p.ack-meat.task
          =?  message-pump  &(cork !ok.ack)  (give [%kroc bone])
          =.  message-pump
            %-  on-done
            [[message-num:task ?:(ok.ack [%ok ~] [%nack ~])] cork]
          ?.  &(!ok.ack cork)  message-pump
          %.  message-pump
          %+  trace  odd.veb
          |.("got nack for %cork {<bone=bone message-num=message-num:task>}")
        ==
      %near  (on-done [[message-num %naxplanation error]:naxplanation.task %&])
    ==
  ::  +on-memo: handle request to send a message
  ::
  ++  on-memo
    |=  =message-blob
    ^+  message-pump
    ::
    =.  unsent-messages.state  (~(put to unsent-messages.state) message-blob)
    message-pump
  ::  +on-hear: handle packet acknowledgment
  ::
  ++  on-hear
    |=  [=message-num =fragment-num]
    ^+  message-pump
    ::  pass to |packet-pump unless duplicate or future ack
    ::
    ?.  (is-message-num-in-range message-num)
      %-  (trace snd.veb |.("hear pump out of range"))
      message-pump
    (run-packet-pump %hear message-num fragment-num)
  ::  +on-done: handle message acknowledgment
  ::
  ::    A nack-trace message counts as a valid message nack on the
  ::    original failed message.
  ::
  ::    This prevents us from having to wait for a message nack packet,
  ::    which would mean we couldn't immediately ack the nack-trace
  ::    message, which would in turn violate the semantics of backward
  ::    flows.
  ::
  ++  on-done
    |=  [[=message-num =ack] cork=?]
    ^+  message-pump
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
      %-  %+  trace  snd.veb
          |.("duplicate done {<current=current.state message-num=message-num>}")
      message-pump
    ::  ignore duplicate and future acks
    ::
    ?.  (is-message-num-in-range message-num)
      message-pump
    ::  clear and print .unsent-fragments if nonempty
    ::
    =?    unsent-fragments.state
        &(=(current next) ?=(^ unsent-fragments)):state
      ::
      ~>  %slog.0^leaf/"ames: early message ack {<her.channel>}"
      ~
    ::  clear all packets from this message from the packet pump
    ::
    =.  message-pump  (run-packet-pump %done message-num lag=*@dr)
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
    |-  ^+  message-pump
    ::  if .current hasn't been fully acked, we're done
    ::
    ?~  cur=(~(get by queued-message-acks.state) current.state)
      message-pump
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
    ::    but it's not incorrect.  pump-metrics are updated only once,
    ::    at the time when we actually delete the packet.
    ::
    =.  message-pump  (run-packet-pump %done current.state lag=*@dr)
    ::  give %done to vane if we're ready
    ::
    ?-    -.u.cur
        %ok
      =.  message-pump
        ::  don't give %done for corks
        ::
        ?:  cork  (give %cork ~)
        (give %done current.state ~)
      $(current.state +(current.state))
    ::
        %nack
      message-pump
    ::
        %naxplanation
      =.  message-pump  (give %done current.state `error.u.cur)
      $(current.state +(current.state))
    ==
  ::  +is-message-num-in-range: %.y unless duplicate or future ack
  ::
  ++  is-message-num-in-range
    |=  =message-num
    ^-  ?
    ::
    ?:  (gte message-num next.state)
      %.n
    ?:  (lth message-num current.state)
      %.n
    !(~(has by queued-message-acks.state) message-num)
  ::  +feed-packets: give packets to |packet-pump until full
  ::
  ++  feed-packets
    ::  if nothing to send, no-op
    ::
    ?:  &(=(~ unsent-messages) =(~ unsent-fragments)):state
      message-pump
    ::  we have unsent fragments of the current message; feed them
    ::
    ?.  =(~ unsent-fragments.state)
      =/  res  (feed:packet-pump unsent-fragments.state)
      =+  [unsent packet-pump-gifts packet-pump-state]=res
      ::
      =.  unsent-fragments.state   unsent
      =.  packet-pump-state.state  packet-pump-state
      ::
      =.  message-pump  (process-packet-pump-gifts packet-pump-gifts)
      ::  if it sent all of them, feed it more; otherwise, we're done
      ::
      ?~  unsent
        feed-packets
      message-pump
    ::  .unsent-messages is nonempty; pop a message off and feed it
    ::
    =^  =message-blob  unsent-messages.state  ~(get to unsent-messages.state)
    ::  break .message into .chunks and set as .unsent-fragments
    ::
    =.  unsent-fragments.state  (split-message next.state message-blob)
    ::  try to feed packets from the next message
    ::
    =.  next.state  +(next.state)
    feed-packets
  ::  +run-packet-pump: call +work:packet-pump and process results
  ::
  ++  run-packet-pump
    |=  =packet-pump-task
    ^+  message-pump
    ::
    =^  packet-pump-gifts  packet-pump-state.state
      (work:packet-pump packet-pump-task)
    ::
    (process-packet-pump-gifts packet-pump-gifts)
  ::  +process-packet-pump-gifts: pass |packet-pump effects up the chain
  ::
  ++  process-packet-pump-gifts
    |=  packet-pump-gifts=(list packet-pump-gift)
    ^+  message-pump
    ::
    ?~  packet-pump-gifts
      message-pump
    =.  message-pump  (give i.packet-pump-gifts)
    ::
    $(packet-pump-gifts t.packet-pump-gifts)
  ::  +assert: sanity checks to isolate error cases
  ::
  ++  assert
    ^+  message-pump
    =/  top-live
      (pry:packet-queue:*make-packet-pump live.packet-pump-state.state)
    ?.  |(?=(~ top-live) (lte current.state message-num.key.u.top-live))
      ~|  [%strange-current current=current.state key.u.top-live]
      !!
    message-pump
  --
::  +make-packet-pump: construct |packet-pump core
::
++  make-packet-pump
  |=  [state=packet-pump-state =channel]
  =*  veb  veb.bug.channel
  =|  gifts=(list packet-pump-gift)
  |%
  ++  packet-pump  .
  ++  give  |=(packet-pump-gift packet-pump(gifts [+< gifts]))
  ++  trace
    |=  [verb=? print=(trap tape)]
    ^+  same
    (^trace verb her.channel ships.bug.channel print)
  ::  +packet-queue: type for all sent fragments, ordered by sequence number
  ::
  ++  packet-queue
    %-  (ordered-map live-packet-key live-packet-val)
    lte-packets
  ::  +live-packets: number of sent packets awaiting ack
  ::
  ++  live-packets
    ^-  @ud
    ~(wyt by live.state)
  ::  +gauge: inflate a |pump-gauge to track congestion control
  ::
  ++  gauge  (make-pump-gauge metrics.state live-packets [now her bug]:channel)
  ::  +work: handle $packet-pump-task request
  ::
  ++  work
    |=  task=packet-pump-task
    ^+  [gifts state]
    ::
    =-  [(flop gifts) state]
    ::
    ?-  -.task
      %hear  (on-hear [message-num fragment-num]:task)
      %done  (on-done message-num.task)
      %wake  (on-wake current.task)
      %prod  on-prod
      %halt  set-wake
    ==
  ::  +on-prod: reset congestion control, re-send packets
  ::
  ++  on-prod
    ^+  packet-pump
    ?:  =(~ next-wake.state)
      packet-pump
    ::
    =.  metrics.state  %*(. *pump-metrics counter counter.metrics.state)
    =.  live.state
      %+  run:packet-queue  live.state
      |=(p=live-packet-val p(- *packet-state))
    ::
    =/  sot  (max 1 num-slots:gauge)
    =/  liv  live.state
    |-  ^+  packet-pump
    ?:  =(0 sot)  packet-pump
    ?:  =(~ liv)  packet-pump
    =^  hed  liv  (pop:packet-queue liv)
    =.  packet-pump  (give %send (to-static-fragment hed))
    $(sot (dec sot))
  ::  +on-wake: handle packet timeout
  ::
  ++  on-wake
    |=  current=message-num
    ^+  packet-pump
    ::  assert temporal coherence
    ::
    ?<  =(~ next-wake.state)
    =.  next-wake.state  ~
    ::  tell congestion control a packet timed out
    ::
    =.  metrics.state  on-timeout:gauge
    ::  re-send first packet and update its state in-place
    ::
    =-  =*  res  -
        =.  live.state   live.res
        =?  packet-pump  ?=(^ static-fragment)
          %-  %+  trace  snd.veb
              =/  nums  [message-num fragment-num]:u.static-fragment.res
              |.("dead {<nums show:gauge>}")
          (give %send u.static-fragment.res)
        packet-pump
    ::
    =|  acc=(unit static-fragment)
    ^+  [static-fragment=acc live=live.state]
    ::
    %^  (dip:packet-queue _acc)  live.state  acc
    |=  $:  acc=_acc
            key=live-packet-key
            val=live-packet-val
        ==
    ^-  [new-val=(unit live-packet-val) stop=? _acc]
    ::  if already acked later message, don't resend
    ::
    ?:  (lth message-num.key current)
      %-  %-  slog  :_  ~
          leaf+"ames: strange wake queue, expected {<current>}, got {<key>}"
      [~ stop=%.n ~]
    ::  packet has expired; update it in-place, stop, and produce it
    ::
    =.  last-sent.val  now.channel
    =.  retries.val    +(retries.val)
    ::
    [`val stop=%.y `(to-static-fragment key val)]
  ::  +feed: try to send a list of packets, returning unsent and effects
  ::
  ++  feed
    |=  fragments=(list static-fragment)
    ^+  [fragments gifts state]
    ::  return unsent back to caller and reverse effects to finalize
    ::
    =-  [unsent (flop gifts) state]
    ::
    ^+  [unsent=fragments packet-pump]
    ::  bite off as many fragments as we can send
    ::
    =/  num-slots  num-slots:gauge
    =/  sent       (scag num-slots fragments)
    =/  unsent     (slag num-slots fragments)
    ::
    :-  unsent
    ^+  packet-pump
    ::  if nothing to send, we're done
    ::
    ?~  sent  packet-pump
    ::  convert $static-fragment's into +ordered-set [key val] pairs
    ::
    =/  send-list
      %+  turn  sent
      |=  static-fragment
      ^-  [key=live-packet-key val=live-packet-val]
      ::
      :-  [message-num fragment-num]
      :-  [sent-date=now.channel retries=0 skips=0]
      [num-fragments fragment]
    ::  update .live and .metrics
    ::
    =.  live.state     (gas:packet-queue live.state send-list)
    ::  TMI
    ::
    =>  .(sent `(list static-fragment)`sent)
    ::  emit a $packet-pump-gift for each packet to send
    ::
    %+  roll  sent
    |=  [packet=static-fragment core=_packet-pump]
    (give:core %send packet)
  ::  +fast-resend-after-ack: resend timed out packets
  ::
  ::    After we finally receive an ack, we want to resend all the live
  ::    packets that have been building up.
  ::
  ++  fast-resend-after-ack
    |=  [=message-num =fragment-num]
    ^+  packet-pump
    =;  res=[resends=(list static-fragment) live=_live.state]
      =.  live.state  live.res
      %+  reel  resends.res
      |=  [packet=static-fragment core=_packet-pump]
      (give:core %send packet)
    ::
    =/  acc
      resends=*(list static-fragment)
    ::
    %^  (dip:packet-queue _acc)  live.state  acc
    |=  $:  acc=_acc
            key=live-packet-key
            val=live-packet-val
        ==
    ^-  [new-val=(unit live-packet-val) stop=? _acc]
    ?:  (lte-packets key [message-num fragment-num])
      [new-val=`val stop=%.n acc]
    ::
    ?:  (gth (next-expiry:gauge key val) now.channel)
      [new-val=`val stop=%.y acc]
    ::
    =.  last-sent.val  now.channel
    =.  resends.acc  [(to-static-fragment key val) resends.acc]
    [new-val=`val stop=%.n acc]
  ::  +on-hear: handle ack on a live packet
  ::
  ::    If the packet was in our queue, delete it and update our
  ::    metrics, possibly re-sending skipped packets.  Otherwise, no-op.
  ::
  ++  on-hear
    |=  [=message-num =fragment-num]
    ^+  packet-pump
    ::
    =-  ::  if no sent packet matches the ack, don't apply mutations or effects
        ::
        ?.  found.-
          %-  (trace snd.veb |.("miss {<show:gauge>}"))
          packet-pump
        ::
        =.  metrics.state  metrics.-
        =.  live.state     live.-
        %-  ?.  ?|  =(0 fragment-num)
                    =(0 (mod counter.metrics.state 20))
                ==
              same
            (trace snd.veb |.("send: {<fragment=fragment-num show:gauge>}"))
        ::  .resends is backward, so fold backward and emit
        ::
        =.  packet-pump
          %+  reel  resends.-
          |=  [packet=static-fragment core=_packet-pump]
          (give:core %send packet)
        (fast-resend-after-ack message-num fragment-num)
    ::
    =/  acc
      :*  found=`?`%.n
          resends=*(list static-fragment)
          metrics=metrics.state
      ==
    ::
    ^+  [acc live=live.state]
    ::
    %^  (dip:packet-queue _acc)  live.state  acc
    |=  $:  acc=_acc
            key=live-packet-key
            val=live-packet-val
        ==
    ^-  [new-val=(unit live-packet-val) stop=? _acc]
    ::
    =/  gauge  (make-pump-gauge metrics.acc live-packets [now her bug]:channel)
    ::  is this the acked packet?
    ::
    ?:  =(key [message-num fragment-num])
      ::  delete acked packet, update metrics, and stop traversal
      ::
      =.  found.acc    %.y
      =.  metrics.acc  (on-ack:gauge -.val)
      [new-val=~ stop=%.y acc]
    ::  is this a duplicate ack?
    ::
    ?.  (lte-packets key [message-num fragment-num])
      ::  stop, nothing more to do
      ::
      [new-val=`val stop=%.y acc]
    ::  ack was on later packet; mark skipped, tell gauge, and continue
    ::
    =.  skips.val  +(skips.val)
    =^  resend  metrics.acc  (on-skipped-packet:gauge -.val)
    ?.  resend
      [new-val=`val stop=%.n acc]
    ::
    =.  last-sent.val  now.channel
    =.  retries.val    +(retries.val)
    =.  resends.acc    [(to-static-fragment key val) resends.acc]
    [new-val=`val stop=%.n acc]
  ::  +on-done: apply ack to all packets from .message-num
  ::
  ++  on-done
    |=  =message-num
    ^+  packet-pump
    ::
    =-  =.  metrics.state  metrics.-
        =.  live.state     live.-
        ::
        %-  (trace snd.veb |.("done {<message-num=message-num show:gauge>}"))
        (fast-resend-after-ack message-num `fragment-num`0)
    ::
    ^+  [metrics=metrics.state live=live.state]
    ::
    %^  (dip:packet-queue pump-metrics)  live.state  acc=metrics.state
    |=  $:  metrics=pump-metrics
            key=live-packet-key
            val=live-packet-val
        ==
    ^-  [new-val=(unit live-packet-val) stop=? pump-metrics]
    ::
    =/  gauge  (make-pump-gauge metrics live-packets [now her bug]:channel)
    ::  if we get an out-of-order ack for a message, skip until it
    ::
    ?:  (lth message-num.key message-num)
      [new-val=`val stop=%.n metrics]
    ::  if packet was from acked message, delete it and continue
    ::
    ?:  =(message-num.key message-num)
      [new-val=~ stop=%.n metrics=(on-ack:gauge -.val)]
    ::  we've gone past the acked message; we're done
    ::
    [new-val=`val stop=%.y metrics]
  ::  +set-wake: set, unset, or reset timer, emitting moves
  ::
  ++  set-wake
    ^+  packet-pump
    ::  if nonempty .live, pry at head to get next wake time
    ::
    =/  new-wake=(unit @da)
      ?~  head=(pry:packet-queue live.state)
        ~
      `(next-expiry:gauge u.head)
    ::  no-op if no change
    ::
    ?:  =(new-wake next-wake.state)  packet-pump
    ::  unset old timer if non-null
    ::
    =?  packet-pump  !=(~ next-wake.state)
      =/  old  (need next-wake.state)
      =.  next-wake.state  ~
      (give %rest old)
    ::  set new timer if non-null
    ::
    =?  packet-pump  ?=(^ new-wake)
      =.  next-wake.state  new-wake
      (give %wait u.new-wake)
    ::
    packet-pump
  --
::  +to-static-fragment: convenience function for |packet-pump
::
++  to-static-fragment
  |=  [live-packet-key live-packet-val]
  ^-  static-fragment
  [message-num num-fragments fragment-num fragment]
::  +make-pump-gauge: construct |pump-gauge congestion control core
::
++  make-pump-gauge
  |=  [pump-metrics live-packets=@ud now=@da =ship =bug]
  ::  TODO rename live-packets num-live
  =*  veb  veb.bug
  =*  metrics  +<-
  |%
  ++  trace
    |=  [verb=? print=(trap tape)]
    ^+  same
    (^trace verb ship ships.bug print)
  ::  +next-expiry: when should a newly sent fresh packet time out?
  ::
  ::    Use rtt + 4*sigma, where sigma is the mean deviation of rtt.
  ::    This should make it unlikely that a packet would time out from a
  ::    delay, as opposed to an actual packet loss.
  ::
  ++  next-expiry
    |=  [live-packet-key live-packet-val]
    ^-  @da
    (add last-sent rto)
  ::  +num-slots: how many packets can we send right now?
  ::
  ++  num-slots
    ^-  @ud
    (sub-safe cwnd live-packets)
  ::  +on-ack: adjust metrics based on a packet getting acknowledged
  ::
  ++  on-ack
    |=  =packet-state
    ^-  pump-metrics
    ::
    =.  counter  +(counter)
    ::  if below congestion threshold, add 1; else, add avg. 1 / cwnd
    ::
    =.  cwnd
      ?:  in-slow-start
        +(cwnd)
      (add cwnd !=(0 (mod (mug now) cwnd)))
    ::  if this was a re-send, don't adjust rtt or downstream state
    ::
    ?.  =(0 retries.packet-state)
      metrics
    ::  rtt-datum: new rtt measurement based on this packet roundtrip
    ::
    =/  rtt-datum=@dr  (sub-safe now last-sent.packet-state)
    ::  rtt-error: difference between this rtt measurement and expected
    ::
    =/  rtt-error=@dr
      ?:  (gte rtt-datum rtt)
        (sub rtt-datum rtt)
      (sub rtt rtt-datum)
    ::  exponential weighting ratio for .rtt and .rttvar
    ::
    %-  %+  trace  ges.veb
        |.("ack update {<show rtt-datum=rtt-datum rtt-error=rtt-error>}")
    =.  rtt     (div (add rtt-datum (mul rtt 7)) 8)
    =.  rttvar  (div (add rtt-error (mul rttvar 7)) 8)
    =.  rto     (clamp-rto (add rtt (mul 4 rttvar)))
    ::
    metrics
  ::  +on-skipped-packet: handle misordered ack
  ::
  ++  on-skipped-packet
    |=  packet-state
    ^-  [resend=? pump-metrics]
    ::
    =/  resend=?  &(=(0 retries) |(in-recovery (gte skips 3)))
    :-  resend
    ::
    =?  cwnd  !in-recovery  (max 2 (div cwnd 2))
    %-  %+  trace  snd.veb
        |.("skip {<resend=resend in-recovery=in-recovery show>}")
    metrics
  ::  +on-timeout: (re)enter slow-start mode on packet loss
  ::
  ++  on-timeout
    ^-  pump-metrics
    ::
    %-  (trace ges.veb |.("timeout update {<show>}"))
    =:  ssthresh  (max 1 (div cwnd 2))
            cwnd  1
             rto  (clamp-rto (mul rto 2))
      ==
    metrics
  ::  +clamp-rto: apply min and max to an .rto value
  ::
  ++  clamp-rto
    |=  rto=@dr
    ^+  rto
    (min ~m2 (max ^~((div ~s1 5)) rto))
  ::  +in-slow-start: %.y iff we're in "slow-start" mode
  ::
  ++  in-slow-start
    ^-  ?
    (lth cwnd ssthresh)
  ::  +in-recovery: %.y iff we're recovering from a skipped packet
  ::
  ::    We finish recovering when .live-packets finally dips back down to
  ::    .cwnd.
  ::
  ++  in-recovery
    ^-  ?
    (gth live-packets cwnd)
  ::  +sub-safe: subtract with underflow protection
  ::
  ++  sub-safe
    |=  [a=@ b=@]
    ^-  @
    ?:((lte a b) 0 (sub a b))
  ::  +show: produce a printable version of .metrics
  ::
  ++  show
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
  --
::  +make-message-sink: construct |message-sink message receiver core
::
++  make-message-sink
  |=  [state=message-sink-state =channel]
  =*  veb  veb.bug.channel
  =|  gifts=(list message-sink-gift)
  |%
  ++  message-sink  .
  ++  give  |=(message-sink-gift message-sink(gifts [+< gifts]))
  ++  trace
    |=  [verb=? print=(trap tape)]
    ^+  same
    (^trace verb her.channel ships.bug.channel print)
  ::  +work: handle a $message-sink-task
  ::
  ++  work
    |=  [closing=? task=message-sink-task]
    ^+  [gifts state]
    ::
    =-  [(flop gifts) state]
    ::
    ?-  -.task
      %done  (on-done ok.task cork.task)
      %drop  (on-drop message-num.task)
      %hear  (on-hear closing [lane shut-packet ok]:task)
    ==
  ::  +on-hear: receive message fragment, possibly completing message
  ::
  ++  on-hear
    |=  [closing=? =lane =shut-packet ok=?]
    ^+  message-sink
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
      %-  %+  trace  odd.veb
          |.("future %hear {<seq=seq last-acked=last-acked.state>}")
      message-sink
    ::
    =/  is-last-fragment=?  =(+(fragment-num) num-fragments)
    ::  always ack a dupe!
    ::
    ?:  (lte seq last-acked.state)
      ?.  is-last-fragment
        ::  single packet ack
        ::
        %-  %+  trace  rcv.veb
            |.("send dupe ack {<seq=seq fragment-num>}")
        (give %send seq %& fragment-num)
      ::  whole message (n)ack
      ::
      =/  ok=?  !(~(has in nax.state) seq)
      %-  (trace rcv.veb |.("send dupe message ack {<seq=seq>} ok={<ok>}"))
      (give %send seq %| ok lag=`@dr`0)
    ::  last-acked<seq<=last-heard; heard message, unprocessed
    ::
    ::    Only true if we've heard some packets we haven't acked, which
    ::    doesn't happen for boons.
    ::
    ?:  (lte seq last-heard.state)
      ?:  &(is-last-fragment !closing)
        ::  if not from a closing bone, drop last packet,
        ::  since we don't know whether to ack or nack
        ::
        %-  %+  trace  rcv.veb
            |.  ^-  tape
            =/  data
              :*  her.channel  seq=seq  bone=bone.shut-packet
                  fragment-num  num-fragments
                  la=last-acked.state  lh=last-heard.state
              ==
            "hear last in-progress {<data>}"
        message-sink
      ::  ack all other packets
      ::
      %-  %+  trace  rcv.veb  |.
          =/  data
            :*  seq=seq  fragment-num
                num-fragments  closing=closing
            ==
          "send ack-1 {<data>}"
      (give %send seq %& fragment-num)
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
        %-  %+  trace  rcv.veb  |.
            =/  data
              [her.channel seq=seq lh=last-heard.state la=last-acked.state]
            "hear last dupe {<data>}"
        message-sink
      %-  %+  trace  rcv.veb
          |.("send dupe ack {<her.channel seq=seq fragment-num>}")
      (give %send seq %& fragment-num)
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
    =?  message-sink  !is-last-fragment
      %-  %+  trace  rcv.veb  |.
          =/  data
            [seq=seq fragment-num num-fragments]
          "send ack-2 {<data>}"
      (give %send seq %& fragment-num)
    ::  enqueue all completed messages starting at +(last-heard.state)
    ::
    |-  ^+  message-sink
    ::  if this is not the next message to ack, we're done
    ::
    ?.  =(seq +(last-heard.state))
      message-sink
    ::  if we haven't heard anything from this message, we're done
    ::
    ?~  live=(~(get by live-messages.state) seq)
      message-sink
    ::  if the message isn't done yet, we're done
    ::
    ?.  =(num-received num-fragments):u.live
      message-sink
    ::  we have whole message; update state, assemble, and send to vane
    ::
    =.  last-heard.state     +(last-heard.state)
    =.  live-messages.state  (~(del by live-messages.state) seq)
    ::
    %-  %+  trace  msg.veb
        |.("hear {<her.channel>} {<seq=seq>} {<num-fragments.u.live>}kb")
    =/  message=*  (assemble-fragments [num-fragments fragments]:u.live)
    =.  message-sink  (enqueue-to-vane seq message)
    ::
    $(seq +(seq))
  ::  +enqueue-to-vane: enqueue message to be sent to local vane
  ::
  ++  enqueue-to-vane
    |=  [seq=message-num message=*]
    ^+  message-sink
    ::
    =/  empty=?  =(~ pending-vane-ack.state)
    =.  pending-vane-ack.state  (~(put to pending-vane-ack.state) seq message)
    ?.  empty
      message-sink
    (give %memo seq message)
  ::  +on-done: handle confirmation of message processing from vane
  ::
  ++  on-done
    |=  [ok=? cork=?]
    ^+  message-sink
    ::
    =^  pending  pending-vane-ack.state  ~(get to pending-vane-ack.state)
    =/  =message-num  message-num.p.pending
    ::
    =.  last-acked.state  +(last-acked.state)
    =?  nax.state  !ok  (~(put in nax.state) message-num)
    ::
    =.  message-sink  (give %send message-num %| ok lag=`@dr`0)
    =?  message-sink  cork  (give %cork ~)
    =/  next  ~(top to pending-vane-ack.state)
    ?~  next
      message-sink
    (give %memo u.next)
  ::  +on-drop: drop .message-num from our .nax state
  ::
  ++  on-drop
    |=  =message-num
    ^+  message-sink
    ::
    =.  nax.state  (~(del in nax.state) message-num)
    ::
    message-sink
  --
--
