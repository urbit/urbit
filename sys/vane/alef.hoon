::  protocol-version: current version of the ames wire protocol
::
=/  protocol-version=?(%0 %1 %2 %3 %4 %5 %6 %7)  %0
::
|%
+|  %atomics
::
+$  blob           @uxblob
+$  bone           @udbone
+$  lane           @uxlane
+$  message-num    @udmessage
+$  packet-num     @udpacket
+$  public-key     @uwpublickey
+$  symmetric-key  @uwsymmetrickey
::  $rank: which kind of ship address, by length
::
::    0: galaxy or star -- 2  bytes
::    1: planet         -- 4  bytes
::    2: moon           -- 8  bytes
::    3: comet          -- 16 bytes
::
+$  rank  ?(%0 %1 %2 %3)
::
+|  %parts
::
::  $dyad: pair of sender and receiver ships
::
+$  dyad  [sndr=ship rcvr=ship]
::  $message: application-level message
::
::    path: internal route on the receiving ship
::    payload: semantic message contents
::
+$  message  [=path payload=*]
::  $packet: noun representation of an ames datagram packet
::
::    Roundtrips losslessly through atom encoding and decoding.
::
::    .origin is ~ unless the packet is being forwarded.  If present,
::    it's an atom that encodes a route to another ship, such as an IPv4
::    address.  Routes are opaque to Arvo and only have meaning in the
::    interpreter. This enforces that Ames is transport-agnostic.
::
+$  packet  [=dyad encrypted=? origin=(unit lane) content=*]
::
+|  %state
::
::  $channel: combined sender and receiver identifying data
::
+$  channel
  $:  ::  dyad: [our her] if sending; [her our] when receiving
      ::
      dyad
      ::  our data, common to all dyads
      ::
      $:  =our=life
          crypto-core=acru:ames
      ==
      ::  her data, specific to this dyad
      ::
      $:  =symmetric-key
          =her=life
          =her=public-key
          her-sponsors=(list ship)
  ==  ==
::  $ames-state: state for entire vane
::
+$  ames-state
  $:  peers=(map ship ship-state)
      =life
      crypto-core=acru:ames
  ==
::  $ship-state: all we know about a peer
::
::    %known: we know their life and public keys, so we have a channel
::    %alien: no PKI data, so enqueue actions to perform once we learn it
::
+$  ship-state
  $%  [%known peer-state]
      [%alien pending-actions]
  ==
::  $peer-state: state for a peer with known life and keys
::
+$  peer-state
  $:  $:  =symmetric-key
          =life
          =public-key
          sponsors=(list ship)
      ==
      route=(unit [direct=? =lane])
      =ossuary
      snd=(map bone snd-state)
      rcv=(map bone rcv-state)
  ==
::  $ossuary: bone<-->duct bijection and .next bone to map to a duct
::
+$  ossuary
  $:  next=bone
      by-duct=(map duct bone)
      by-bone=(map bone duct)
  ==
::  $pending-actions: what to do when we learn a peer's life and keys
::
+$  pending-actions
  $:  rcv-packets=(list [=lane =packet])
      snd-messages=(list [=duct =message])
  ==
+$  snd-state
  $:  _!!
  ==
+$  rcv-state
  $:  _!!
  ==
--
|%
::  +encode-packet: serialize a packet into a bytestream
::
++  encode-packet
  |=  packet
  ^-  blob
  ::
  =/  sndr-meta  (encode-ship-metadata sndr.dyad)
  =/  rcvr-meta  (encode-ship-metadata rcvr.dyad)
  ::  body: <<sndr rcvr (jam [origin content])>>
  ::
  ::    The .sndr and .rcvr ship addresses are encoded with fixed
  ::    lengths specified by the packet header. They live outside
  ::    the jammed-data section to simplify packet filtering in the
  ::    interpreter.
  ::
  =/  body=@
    ;:  mix
      sndr.dyad
      (lsh 3 size.sndr-meta rcvr.dyad)
      (lsh 3 (add size.sndr-meta size.rcvr-meta) (jam [origin content]))
    ==
  ::  header: 32-bit header assembled from bitstreams of fields
  ::
  ::    <<version checksum sndr-rank rcvr-rank encryption-type unused>>
  ::    4 bits at the end of the header are unused.
  ::
  =/  header=@
    %+  can  0
    :~  [3 protocol-version]
        [20 (mug body)]
        [2 rank.sndr-meta]
        [2 rank.rcvr-meta]
        [5 ?:(encrypted %0 %1)]
    ==
  ::  result is <<header body>>
  ::
  (mix header (lsh 5 1 body))
::  +decode-packet: deserialize packet from bytestream or crash
::
++  decode-packet
  |=  =blob
  ^-  packet
  ::  first 32 (2^5) bits are header; the rest is body
  ::
  =/  header  (end 5 1 blob)
  =/  body    (rsh 5 1 blob)
  ::
  =/  version    (end 0 3 header)
  =/  checksum   (cut 0 [3 20] header)
  =/  sndr-size  (decode-ship-size (cut 0 [23 2] header))
  =/  rcvr-size  (decode-ship-size (cut 0 [25 2] header))
  =/  encrypted  ?+((cut 0 [27 5] header) !! %0 %.y, %1 %.n)
  ::
  ?>  =(protocol-version version)
  ?>  =(checksum (end 0 20 (mug body)))
  ::
  =/  =dyad
    :-  sndr=(end 3 sndr-size body)
    rcvr=(cut 3 [sndr-size rcvr-size] body)
  ::
  =+  ;;  [origin=(unit @uxlane) content=*]
      %-  cue
      (rsh 3 (add rcvr-size sndr-size) body)
  ::
  [dyad encrypted origin content]
::  +decode-ship-size: decode a 2-bit ship type specifier into a byte width
::
::    Type 0: galaxy or star -- 2 bytes
::    Type 1: planet         -- 4 bytes
::    Type 2: moon           -- 8 bytes
::    Type 3: comet          -- 16 bytes
::
++  decode-ship-size
  |=  rank=@
  ^-  @
  ::
  ?+  rank  !!
    %0  2
    %1  4
    %2  8
    %3  16
  ==
::  +encode-ship-metadata: produce size (in bytes) and address rank for .ship
::
::    0: galaxy or star
::    1: planet
::    2: moon
::    3: comet
::
++  encode-ship-metadata
  |=  =ship
  ^-  [size=@ =rank]
  ::
  =/  size=@  (met 3 ship)
  ::
  ?:  (lte size 2)  [2 %0]
  ?:  (lte size 4)  [4 %1]
  ?:  (lte size 8)  [8 %2]
  [16 %3]
--
