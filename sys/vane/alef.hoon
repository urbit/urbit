::  protocol-version: current version of the ames wire protocol
::
=/  protocol-version=?(%0 %1 %2 %3 %4 %5 %6 %7)  %0
::
|%
+|  %generics
::
::  +ordered-set: treap with user-specified horizontal order
::
::    Conceptually smaller items go on the left, so the smallest item
::    can be popped off the head. If $item is `@` and .compare is +lte,
::    then the numerically smallest item is the head.
::
++  ordered-set
  |*  item=mold
  ::  +compare: item comparator for horizontal order
  ::
  |=  compare=$-([item item] ?)
  |%
  ::  +check-balance: verify horizontal and vertical orderings
  ::
  ++  check-balance
    =|  [l=(unit item) r=(unit item)]
    |=  a=(tree item)
    ^-  ?
    ::  empty tree is valid
    ::
    ?~  a  %.y
    ::  nonempty trees must maintain several criteria
    ::
    ?&  ::  if .n.a is left of .u.l, assert horizontal comparator
        ::
        ?~(l %.y (compare n.a u.l))
        ::  if .n.a is right of .u.r, assert horizontal comparator
        ::
        ?~(r %.y (compare u.r n.a))
        ::  if .a is not leftmost element, assert vertical order between
        ::  .l.a and .n.a and recurse to the left with .n.a as right
        ::  neighbor
        ::
        ?~(l.a %.y &((mor n.a n.l.a) $(a l.a, l `n.a)))
        ::  if .a is not rightmost element, assert vertical order
        ::  between .r.a and .n.a and recurse to the right with .n.a as
        ::  left neighbor
        ::
        ?~(r.a %.y &((mor n.a n.r.a) $(a r.a, r `n.a)))
    ==
  ::  +put: ordered item insert
  ::
  ++  put
    |=  [a=(tree item) =item]
    ^-  (tree ^item)
    ::  base case: replace null with single-item tree
    ::
    ?~  a  [n=item l=~ r=~]
    ::  base case: ignore duplicate
    ::
    ?:  =(n.a item)  a
    ::  if item goes on left, recurse left then rebalance vertical order
    ::
    ?:  (compare item n.a)
      =/  l  $(a l.a)
      ?>  ?=(^ l)
      ?:  (mor n.a n.l)
        a(l l)
      l(r a(l r.l))
    ::  item goes on right; recurse right then rebalance vertical order
    ::
    =/  r  $(a r.a)
    ?>  ?=(^ r)
    ?:  (mor n.a n.r)
      a(r r)
    r(l a(r l.r))
  ::  +peek: produce head (smallest item) or null
  ::
  ++  peek
    |=  a=(tree item)
    ^-  (unit item)
    ::
    ?~  a    ~
    ?~  l.a  `n.a
    $(a l.a)
  ::  +pop: produce .head (smallest item) and .rest or crash if empty
  ::
  ++  pop
    |=  a=(tree item)
    ^-  [head=item rest=(tree item)]
    ::
    ?~  a    !!
    ?~  l.a  [n.a r.a]
    ::
    =/  l  $(a l.a)
    :-  head.l
    ::  load .rest.l back into .a and rebalance
    ::
    ?:  |(?=(~ rest.l) (mor n.a n.rest.l))
      a(l rest.l)
    rest.l(r a(r r.rest.l))
  ::  +sift: remove and produce all items matching .reject predicate
  ::
  ::    Unrolls to a list, extracts items, then rolls back into a tree.
  ::    Removed items are produced smallest to largest.
  ::
  ++  sift
    |=  [a=(tree item) reject=$-(item ?)]
    ^-  [lost=(list item) kept=(tree item)]
    ::
    =+  [l k]=(skid (tap a) reject)  [l (gas ~ k)]
  ::  +tap: convert to list, smallest to largest
  ::
  ++  tap
    |=  a=(tree item)
    ^-  (list item)
    ::
    =|  b=(list item)
    |-  ^+  b
    ?~  a  b
    ::
    $(a l.a, b [n.a $(a r.a)])
  ::  +gas: put a list of items
  ::
  ++  gas
    |=  [a=(tree item) b=(list item)]
    ^-  (tree item)
    ::
    ?~  b  a
    $(b t.b, a (put a i.b))
  --
::
+|  %atomics
::
+$  blob           @uxblob
+$  bone           @udbone
+$  fragment       @uwfragment
+$  lane           @uxlane
+$  message-num    @udmessagenum
+$  fragment-num   @udfragmentnum
+$  public-key     @uwpublickey
+$  signature      @uwsignature
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
::  $open-packet: unencrypted packet payload, for comet self-attestation
::
+$  open-packet
  $:  =signature
      =sndr=life
      =rcvr=life
      rcvr=ship
  ==
::  $shut-packet: encrypted packet payload
::
+$  shut-packet
  $:  =sndr=life
      =rcvr=life
      =bone
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
      snd=(map bone message-pump-state)
      rcv=(map bone rcv-state)
      nax=(set [=bone =message-num])
  ==
::  $ossuary: bone<->duct bijection and .next-bone to map to a duct
::
+$  ossuary
  $:  =next=bone
      by-duct=(map duct bone)
      by-bone=(map bone duct)
  ==
::  $pending-actions: what to do when we learn a peer's life and keys
::
+$  pending-actions
  $:  rcv-packets=(list [=lane =packet])
      snd-messages=(list [=duct =message])
  ==
+$  message-pump-state
  $:  =next=message-num
      unsent-messages=(qeu message)
      unsent-fragments=(list static-fragment)
      =packet-pump-state
  ==
+$  packet-pump-state
  $:  next-wake=(unit @da)
      live=(tree live-fragment)
      lost=(tree static-fragment)
      =pump-metrics
  ==
+$  pump-metrics
  $:  ::  empirically observed data
      ::
      $:  num-live=@ud
          num-lost=@ud
          last-sent-at=@da
          last-dead-at=@da
      ==
      ::  derived state
      ::
      $:  rtt=@dr
          max-live=@ud
  ==  ==
+$  live-fragment
  $:  sent-at=@da
      dead-at=@da
      retried=?
      static-fragment
  ==
+$  static-fragment
  $:  =message-num
      num-fragments=fragment-num
      =fragment-num
      =fragment
  ==
+$  rcv-state
  $:  last-acked=message-num
      last-heard=message-num
      pending-vane-ack=(qeu [=message-num =message])
      live-messages=(map message-num partial-rcv-message)
      nax=(set message-num)
  ==
+$  partial-rcv-message
  $:  num-fragments=fragment-num
      num-received=fragment-num
      fragments=(map fragment-num fragment)
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
