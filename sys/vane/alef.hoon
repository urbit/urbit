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
+$  fragment-num   @udfragmentnum
+$  lane           @uxlane
+$  message-num    @udmessagenum
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
+|  %kinetics
::
::  $channel: combined sender and receiver identifying data
::
+$  channel
  $:  [our=ship her=ship]
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
::  $dyad: pair of sender and receiver ships
::
+$  dyad  [sndr=ship rcvr=ship]
::
+$  error  [tag=@tas =tang]
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
+|  %statics
::
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
::  $message-pump-state: persistent state for |message-pump
::
::    Messages queue up in |message-pump's .unsent-messages until they
::    can be packetized and fed into |packet-pump for sending.  When we
::    pop a message off .unsent-messages, we push as many fragments as
::    we can into |packet-pump, then place the remaining in
::    .unsent-fragments.  We also insert an entry in .unacked-fragments
::    initialized with the total number of fragments in the message.
::
::    When we hear a packet ack, we send it to |packet-pump.  If we
::    haven't seen it before, |packet-pump reports the fresh ack.  We
::    then decrement that message's entry in .unacked-fragments.
::
::    When .unacked-fragments goes to zero on a .message-num, that means
::    all fragments have been acked, so delete this entry from
::    .unsent-fragments.  If this message is not .current, then it's a
::    future message and .current has not yet been acked, so we place
::    the message in .queued-acks.
::
::    If it is the current message, emit the message ack, increment
::    .current, and check if this next message is in .queued-acks.  If
::    it is, emit the message (n)ack, increment .current, and check the
::    next message.  Repeat until .current is not fully acked.
::
::    When we hear a message nack, we send it to |packet-pump, which
::    deletes all packets from that message.  If .current gets nacked,
::    clear .unsent-fragments and go into the same flow as when we hear
::    the last packet ack on a message.
::
::    The following equation is always true:
::    .next - .current == number of messages in flight
::
::    At the end of a task, |message-pump sends a %flush task to
::    |packet-pump, which can trigger a timer to be set or cleared based
::    on congestion control calculations. When it fires, the timer will
::    generally cause one or more packets to be resent.
::
::    current: sequence number of message being sent
::    next: sequence number of next message to send
::    unsent-messages: messages to be sent after current message
::    unsent-fragments: fragments of current message waiting for sending
::    unacked-fragments: number of fragments waiting on ack
::    queued-acks: future message acks to be applied after current
::    packet-pump-state: state of corresponding |packet-pump
::
+$  message-pump-state
  $:  current=message-num
      next=message-num
      unsent-messages=(qeu message)
      unsent-fragments=(list static-fragment)
      unacked-fragments=(map message-num fragment-num)
      queued-acks=(map message-num ok=?)
      =packet-pump-state
  ==
::  $packet-pump-state: persistent state for |packet-pump
::
::    next-wake: last timer we've set, or null
::    live: packets in flight; sent but not yet acked
::    lost: packets to retry, since they timed out with no ack
::    pump-metrics: congestion control information
::
+$  packet-pump-state
  $:  next-wake=(unit @da)
      live=(tree live-fragment)
      lost=(tree static-fragment)
      =pump-metrics
  ==
+$  pump-metrics
  $:  num-live=@ud
      num-lost=@ud
      last-sent-at=@da
      last-dead-at=@da
      rtt=@dr
      max-live=@ud
  ==
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
::
+|  %dialectics
::
::  $task: job for ames
::
::    %born: process restart notification
::    %crud: crash report
::    %hear: packet from unix
::    %hole: report that packet handling crashed
::    %init: vane boot
::    %sunk: a ship breached and has a new .rift
::    %vega: kernel reload notification
::    %wegh: request for memory usage report
::    %west: request to send message
::
+$  task
  $%  [%born ~]
      [%crud =error]
      [%hear =lane =blob]
      [%hole =lane =blob]
      [%init =ship]
      [%sunk =ship =rift]
      [%vega ~]
      [%wegh ~]
      [%west =ship =message]
  ==
::  $gift: effect from ames
::
::    %east: message to vane from peer
::    %send: packet to unix
::    %rest: notify vane that peer (n)acked our message
::
+$  gift
  $%  [%east payload=*]
      [%send =lane =blob]
      [%rest error=(unit error)]
  ==
::  $note: request to other vane
::
::    TODO: specialize gall interface for subscription management
::
+$  note
  $%  $:  %b
      $%  [%wait date=@da]
          [%rest date=@da]
      ==  ==
      $:  %c
      $%  [%west =ship =message]
      ==  ==
      $:  %g
      $%  [%west =ship =message]
      ==  ==
      $:  %j
      $%  [%pubs =ship]
          [%turf ~]
          [%west =ship =message]
          [%vein ~]
  ==  ==  ==
::  $sign: response from other vane
::
+$  sign
  $%  $:  %b
      $%  [%wake error=(unit tang)]
      ==  ==
      $:  %j
      $%  [%pubs public:able:jael]
          [%turf turf=(list turf)]
          [%vein =life vein=(map life ring)]
  ==  ==  ==
::  $message-pump-task: job for |message-pump
::
::    %send: packetize and send application-level message
::    %hear-ack: deal with a packet acknowledgment
::    %hear-nack: deal with message negative acknowledgment
::    %wake: handle timer firing
::
+$  message-pump-task
  $%  [%send =message-num =message]
      [%hear-ack =message-num =fragment-num]
      [%hear-nack =message-num lag=@dr]
      [%wake ~]
  ==
::  $message-pump-gift: effect from |message-pump
::
::    %ack-message: report message acknowledgment
::    %send: emit message fragment
::    %set-timer: set a new timer at .date
::    %unset-timer: cancel timer at .date
::
+$  message-pump-gift
  $%  [%ack-message =message-num ok=?]
      [%send =static-fragment]
      [%set-timer date=@da]
      [%unset-timer date=@da]
  ==
::  $packet-pump-task: job for |packet-pump
::
::    %hear-ack: deal with a packet acknowledgment
::    %hear-nack: deal with message negative acknowledgment
::    %flush: finalize this event (i.e. maybe set timer)
::    %send: enqueue or emit message fragments
::    %wake: handle timer firing
::
+$  packet-pump-task
  $%  [%hear-ack =message-num =fragment-num]
      [%hear-nack =message-num lag=@dr]
      [%flush ~]
      [%send fragments=(list static-fragment)]
      [%wake ~]
  ==
::  $packet-pump-gift: effect from |packet-pump
::
::    %ack-fragment: report fresh ack on a message fragment
::    %send: emit message fragment
::    %set-timer: set a new timer at .date
::    %unset-timer: cancel timer at .date
::
+$  packet-pump-gift
  $%  [%ack-fragment =message-num =fragment-num]
      [%send =static-fragment]
      [%set-timer date=@da]
      [%unset-timer date=@da]
  ==
--
::  external vane interface
::
=<
|=  pit=vase
=|  =ames-state
|=  [our=ship eny=@ now=@da scry-gate=sley]
=*  ames-gate  .
|%
::  +call: handle request $task
::
++  call
  |=  [=duct type=* wrapped-task=(hobo task)]
  ^-  [(list move) _ames-gate]
  ::
  !!
::  +take: handle response $sign
::
++  take
  |=  [=wire =duct type=* =sign]
  ^-  [(list move) _ames-gate]
  ::
  !!
::  +stay: extract state before reload
::
++  stay  ames-state
::  +load: load in old state after reload
::
++  load
  |=  old=^ames-state
  ames-gate(ames-state old)
::  +scry: dereference namespace
::
++  scry
  |=  [fur=(unit (set monk)) ren=@tas why=shop syd=desk lot=coin tyl=path]
  ^-  (unit (unit cage))
  ::
  [~ ~]
--
::  helper cores
::
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
