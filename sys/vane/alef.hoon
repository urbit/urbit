::  protocol-version: current version of the ames wire protocol
::
=/  protocol-version=?(%0 %1 %2 %3 %4 %5 %6 %7)  %0
::
|%
+|  %generics
::  $mk-item: constructor for +ordered-map item type
::
+*  mk-item  [key val]  [key=key val=val]
::  +ordered-map: treap with user-specified horizontal order
::
::    Conceptually smaller items go on the left, so the item with the
::    smallest key can be popped off the head. If $key is `@` and
::    .compare is +lte, then the numerically smallest item is the head.
::
++  ordered-map
  |*  [key=mold val=mold]
  =>  |%
      +$  item  (mk-item key val)
      --
  ::  +compare: item comparator for horizontal order
  ::
  |=  compare=$-([key key] ?)
  |%
  ::  +check-balance: verify horizontal and vertical orderings
  ::
  ++  check-balance
    =|  [l=(unit key) r=(unit key)]
    |=  a=(tree item)
    ^-  ?
    ::  empty tree is valid
    ::
    ?~  a  %.y
    ::  nonempty trees must maintain several criteria
    ::
    ?&  ::  if .n.a is left of .u.l, assert horizontal comparator
        ::
        ?~(l %.y (compare key.n.a u.l))
        ::  if .n.a is right of .u.r, assert horizontal comparator
        ::
        ?~(r %.y (compare u.r key.n.a))
        ::  if .a is not leftmost element, assert vertical order between
        ::  .l.a and .n.a and recurse to the left with .n.a as right
        ::  neighbor
        ::
        ?~(l.a %.y &((mor key.n.a key.n.l.a) $(a l.a, l `key.n.a)))
        ::  if .a is not rightmost element, assert vertical order
        ::  between .r.a and .n.a and recurse to the right with .n.a as
        ::  left neighbor
        ::
        ?~(r.a %.y &((mor key.n.a key.n.r.a) $(a r.a, r `key.n.a)))
    ==
  ::  +put: ordered item insert
  ::
  ++  put
    |=  [a=(tree item) =key =val]
    ^-  (tree item)
    ::  base case: replace null with single-item tree
    ::
    ?~  a  [n=[key val] l=~ r=~]
    ::  base case: overwrite existing .key with new .val
    ::
    ?:  =(key.n.a key)  a(val.n val)
    ::  if item goes on left, recurse left then rebalance vertical order
    ::
    ?:  (compare key key.n.a)
      =/  l  $(a l.a)
      ?>  ?=(^ l)
      ?:  (mor key.n.a key.n.l)
        a(l l)
      l(r a(l r.l))
    ::  item goes on right; recurse right then rebalance vertical order
    ::
    =/  r  $(a r.a)
    ?>  ?=(^ r)
    ?:  (mor key.n.a key.n.r)
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
    ?:  |(?=(~ rest.l) (mor key.n.a key.n.rest.l))
      a(l rest.l)
    rest.l(r a(r r.rest.l))
  ::  +nip: remove root; for internal use
  ::
  ++  nip
    |=  a=(tree item)
    ^-  (tree item)
    ::
    ?>  ?=(^ a)
    ::  delete .n.a; merge and balance .l.a and .r.a
    ::
    |-  ^-  (tree item)
    ?~  l.a  r.a
    ?~  r.a  l.a
    ?:  (mor key.n.l.a key.n.r.a)
      l.a(r $(l.a r.l.a))
    r.a(l $(r.a l.r.a))
  ::  +traverse: stateful partial inorder traversal
  ::
  ::    Mutates .state on each run of .f.  Starts at .start key, or if
  ::    .start is ~, starts at the head (item with smallest key).  Stops
  ::    when .f produces .stop=%.y.  Traverses from smaller to larger
  ::    keys.  Each run of .f can replace an item's value or delete the
  ::    item.
  ::
  ++  traverse
    |*  state=mold
    |=  $:  a=(tree item)
            start=(unit key)
            =state
            f=$-([state item] [(unit val) ? state])
        ==
    ^+  [state a]
    ::  acc: accumulator
    ::
    ::    .stop: set to %.y by .f when done traversing
    ::    .state: threaded through each run of .f and produced by +abet
    ::
    =/  acc  [stop=`?`%.n state=state]
    =<  abet  =<  main
    |%
    ++  abet  [state.acc a]
    ::  +main: main recursive loop; performs a partial inorder traversal
    ::
    ++  main
      ^+  .
      ::  stop if empty or we've been told to stop
      ::
      ?~  a  .
      ?:  stop.acc  .
      ::  if nonempty .start, while we're left of .start, move right
      ::
      ?:  &(?=(^ start) !(compare u.start key.n.a))
        right
      ::  inorder traversal: left -> node -> right, until .f sets .stop
      ::
      =>  left
      ?:  stop.acc  .
      =>  node
      ?:  stop.acc  .
      right
    ::  +node: run .f on .n.a, updating .a, .state, and .stop
    ::
    ++  node
      ^+  .
      ::  run .f on node, updating .stop.acc and .state.acc
      ::
      =^  res  acc
        ?>  ?=(^ a)
        (f state.acc n.a)
      ::  apply update to .a from .f's product
      ::
      =.  a
        ::  if .f requested node deletion, merge and balance .l.a and .r.a
        ::
        ?~  res  (nip a)
        ::  we kept the node; replace its .val; order is unchanged
        ::
        ?>  ?=(^ a)
        a(val.n u.res)
      ::
      ..node
    ::  +left: recurse on the left subtree, copying mutant back into .a
    ::
    ++  left
      ^+  .
      ?~  a  .
      =/  lef  main(a l.a)
      lef(a a(l a.lef))
    ::  +right: recurse on the right subtree, copying mutant back into .a
    ::
    ++  right
      ^+  .
      ?~  a  .
      =/  rig  main(a r.a)
      rig(a a(r a.rig))
    --
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
  ::  +uni: unify two ordered maps
  ::
  ::    TODO: document a/b precedence or disjointness constraint
  ::
  ++  uni
    |=  [a=(tree item) b=(tree item)]
    ^-  (tree item)
    ::
    ?~  b  a
    ?~  a  b
    ?:  (mor key.n.a key.n.b)
      ::
      ?:  =(key.n.b key.n.a)
        [n.b $(a l.a, b l.b) $(a r.a, b r.b)]
      ::
      ?:  (compare key.n.b key.n.a)
        $(l.a $(a l.a, r.b ~), b r.b)
      $(r.a $(a r.a, l.b ~), b l.b)
    ::
    ?:  =(key.n.a key.n.b)
      [n.b $(b l.b, a l.a) $(b r.b, a r.a)]
    ::
    ?:  (compare key.n.a key.n.b)
      $(l.b $(b l.b, r.a ~), a r.a)
    $(r.b $(b r.b, l.a ~), a l.a)
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
      now=@da
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
+$  packet  [dyad encrypted=? origin=(unit lane) content=*]
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
      =unix=duct
      =life
      crypto-core=acru:ames
  ==
::  $ship-state: all we know about a peer
::
::    %alien: no PKI data, so enqueue actions to perform once we learn it
::    %known: we know their life and public keys, so we have a channel
::
+$  ship-state
  $%  [%alien pending-requests]
      [%known peer-state]
  ==
::  $pending-requests: what to do when we learn a peer's life and keys
::
::    rcv-packets: packets we've received from unix
::    snd-messages: messages local vanes have asked us to send
::
+$  pending-requests
  $:  rcv-packets=(list [=lane =packet])
      snd-messages=(list [=duct =message])
  ==
::  $peer-state: state for a peer with known life and keys
::
::    route: transport-layer destination for packets to peer
::    ossuary: bone<->duct mapper
::    snd: per-bone message pumps to send messages as fragments
::    rcv: per-bone message stills to assemble messages from fragments
::    nax: unprocessed nacks (negative acknowledgments)
::         Each value is ~ when we've received the ack packet but not a
::         naxplanation, or an error when we've received a naxplanation
::         but not the ack packet.
::
::         When we hear a nack packet or an explanation, if there's no
::         entry in .nax, we make a new entry. Otherwise, if this new
::         information completes the packet+naxplanation, we remove the
::         entry and emit a nack to the local vane that asked us to send
::         the message.
::
::    TODO: should .route be a unit, or do we always need a lane?
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
      rcv=(map bone message-still-state)
      nax=(map [=bone =message-num] (unit error))
  ==
::  $ossuary: bone<->duct bijection and .next-bone to map to a duct
::
+$  ossuary
  $:  =next=bone
      by-duct=(map duct bone)
      by-bone=(map bone duct)
  ==
::  $message-pump-state: persistent state for |message-pump
::
::    Messages queue up in |message-pump's .unsent-messages until they
::    can be packetized and fed into |packet-pump for sending.  When we
::    pop a message off .unsent-messages, we push as many fragments as
::    we can into |packet-pump, then place the remaining in
::    .unsent-fragments.
::
::    When we hear a packet ack, we send it to |packet-pump.  If we
::    haven't seen it before, |packet-pump reports the fresh ack.
::
::    When we hear a message ack (positive or negative), we treat that
::    as though all fragments have been acked.  If this message is not
::    .current, then it's a future message and .current has not yet been
::    acked, so we place the message in .queued-message-acks.
::
::    If we hear a message ack before we've sent all the
::    fragments for that message, clear .unsent-fragments. If the
::    message ack was positive, print it out because it indicates the
::    peer is not behaving properly.
::
::    If the ack is for the current message, emit the message ack,
::    increment .current, and check if this next message is in
::    .queued-message-acks.  If it is, emit the message (n)ack,
::    increment .current, and check the next message.  Repeat until
::    .current is not fully acked.
::
::    When we hear a message nack, we send it to |packet-pump, which
::    deletes all packets from that message.  If .current gets nacked,
::    clear .unsent-fragments and go into the same flow as when we hear
::    the last packet ack on a message.
::
::    The following equation is always true:
::    .next - .current == number of messages in flight
::
::    At the end of a task, |message-pump sends a %finalize task to
::    |packet-pump, which can trigger a timer to be set or cleared based
::    on congestion control calculations. When it fires, the timer will
::    generally cause one or more packets to be resent.
::
::    current: sequence number of message being sent
::    next: sequence number of next message to send
::    unsent-messages: messages to be sent after current message
::    unsent-fragments: fragments of current message waiting for sending
::    queued-message-acks: future message acks to be applied after current
::    packet-pump-state: state of corresponding |packet-pump
::
+$  message-pump-state
  $:  current=message-num
      next=message-num
      unsent-messages=(qeu message)
      unsent-fragments=(list static-fragment)
      queued-message-acks=(map message-num ok=?)
      =packet-pump-state
  ==
::  $packet-pump-state: persistent state for |packet-pump
::
::    next-wake: last timer we've set, or null
::    live: packets in flight; sent but not yet acked
::    lost: packets to retry, since they timed out with no ack
::    metrics: congestion control information
::
+$  packet-pump-state
  $:  next-wake=(unit @da)
      live=(tree [live-packet-key live-packet-val])
      metrics=pump-metrics
  ==
::  $pump-metrics: congestion control statistics for the |pump-gauge
::
::    num-live: number of sent packets in flight
::    num-lost: number of expired packets
::    last-sent-at: last date at which we sent a packet
::    last-dead-at: most recently packet expiry
::    rtt: roundtrip time estimate
::    max-live: current window size
::
+$  pump-metrics
  $:  num-live=@ud
      num-lost=@ud
      last-sent-at=@da
      last-dead-at=@da
      rtt=@dr
      max-live=_7
  ==
+$  live-packet-key  [=message-num =fragment-num]
+$  live-packet-val
  $:  sent-packet-state
      num-fragments=fragment-num
      =fragment
  ==
+$  sent-packet-state
  $:  expiry=@da
      sent-date=@da
      retried=?
  ==
+$  static-fragment
  $:  =message-num
      num-fragments=fragment-num
      =fragment-num
      =fragment
  ==
::  $message-still-state: state of |message-still to assemble messages
::
::    last-acked: highest $message-num we've fully acknowledged
::    last-heard: highest $message-num we've heard all fragments on
::    pending-vane-ack: heard but not processed by local vane
::    live-messages: partially received messages
::
+$  message-still-state
  $:  last-acked=message-num
      last-heard=message-num
      pending-vane-ack=(qeu [=message-num =message])
      live-messages=(map message-num partial-rcv-message)
  ==
::  $partial-rcv-message: message for which we've received some fragments
::
::    num-fragments: total number of fragments in this message
::    num-received: how many fragments we've received so far
::    fragments: fragments we've received, eventually producing a $message
::
+$  partial-rcv-message
  $:  num-fragments=fragment-num
      num-received=fragment-num
      fragments=(map fragment-num fragment)
  ==
::
+|  %dialectics
::
::  $move: output effect; either request or response
::
+$  move  [=duct card=(wind note gift)]
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
::    %hear-fragment-ack: deal with a packet acknowledgment
::    %hear-message-ack: deal with message negative acknowledgment
::    %wake: handle timer firing
::
+$  message-pump-task
  $%  [%send =message]
      [%hear-fragment-ack =message-num =fragment-num]
      [%hear-message-ack =message-num ok=? lag=@dr]
      [%wake ~]
  ==
::  $message-pump-gift: effect from |message-pump
::
::    %ack-message: report message acknowledgment
::    %send: emit message fragment
::    %wait: set a new timer at .date
::    %rest: cancel timer at .date
::
+$  message-pump-gift
  $%  [%ack-message =message-num ok=?]
      [%send =static-fragment]
      [%wait date=@da]
      [%rest date=@da]
  ==
::  $packet-pump-task: job for |packet-pump
::
::    %hear-fragment-ack: deal with a packet acknowledgment
::    %hear-message-ack: deal with message acknowledgment
::    %finalize: finish event, possibly updating timer
::    %wake: handle timer firing
::
+$  packet-pump-task
  $%  [%hear-fragment-ack =message-num =fragment-num]
      [%hear-message-ack =message-num lag=@dr]
      [%finalize ~]
      [%wake ~]
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
::  $message-still-task: job for |message-still
::
::    %hear: handle receiving a message fragment packet
::    %done: receive confirmation from vane of processing completion or
::           failure with diagnostic
::
+$  message-still-task
  $%  [%hear =lane =shut-packet]
      [%done =message-num error=(unit error)]
  ==
::  $message-still-gift: effect from |message-still
::
::    %hear-message: $message assembled from received packets, to be
::                   sent to a local vane for processing
::    %ack-fragment: emit ack in response to heard fragment
::    %ack-message: emit ack in response to message processing
::
+$  message-still-gift
  $%  [%hear-message =message]
      [%ack-fragment =message-num =fragment-num]
      [%ack-message =message-num ok=? lag=@dr]
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
  =/  =task
    ?.  ?=(%soft -.wrapped-task)
      wrapped-task
    ;;(task p.wrapped-task)
  ::
  =/  event-core  (per-event [our eny now scry-gate] duct ames-state)
  ::
  =^  moves  ames-state
    =<  abet
    ?-  -.task
      %born  !!
      %crud  !!
      %hear  (on-hear:event-core [lane blob]:task)
      %hole  !!
      %init  !!
      %sunk  !!
      %vega  !!
      %wegh  !!
      %west  (on-west:event-core [ship message]:task)
    ==
  ::
  [moves ames-gate]
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
::  helpers
::
|%
++  per-event
  =|  moves=(list move)
  |=  [[our=ship eny=@ now=@da scry-gate=sley] =duct =ames-state]
  |%
  ++  event-core  .
  ++  abet  [(flop moves) ames-state]
  ++  emit  |=(=move event-core(moves [move moves]))
  ::
  ::
  ++  on-hear
    |=  [=lane =blob]
    ^+  event-core
    ::
    =/  =packet  (decode-packet blob)
    ::
    %.  [lane packet]
    ::
    ?.  =(our rcvr.packet)
      on-hear-forward
    ::
    ?:  encrypted.packet
      on-hear-shut
    on-hear-open
  ::
  ::
  ++  on-hear-forward
    |=  [=lane =packet]
    ^+  event-core
    ::
    !!
  ::
  ::
  ++  on-hear-open
    |=  [=lane =packet]
    ^+  event-core
    ::
    !!
  ::
  ::
  ++  on-hear-shut
    |=  [=lane =packet]
    ^+  event-core
    ::  encrypted packet content must be an encrypted atom
    ::
    ?>  ?=(@ content.packet)
    ::
    =/  sndr-state  (~(get by peers.ames-state) sndr.packet)
    ::  if we don't know them, enqueue the packet to be handled later
    ::
    ?.  ?=([~ %known *] sndr-state)
      (enqueue-alien-packet lane packet)
    ::  decrypt packet contents using symmetric-key.channel
    ::
    ::    If we know them, we have a $channel with them, which we've
    ::    populated with a .symmetric-key derived from our private key
    ::    and their public key using elliptic curve Diffie-Hellman.
    ::
    =/  =peer-state   +.u.sndr-state
    =/  =channel      [[our sndr.packet] now +>.ames-state -.peer-state]
    =/  =shut-packet  (decrypt symmetric-key.channel content.packet)
    ::  ward against replay attacks
    ::
    ::    We only accept packets from a ship at their known life, and to
    ::    us at our current life.
    ::
    ?>  =(sndr-life.shut-packet her-life.channel)
    ?>  =(rcvr-life.shut-packet our-life.channel)
    ::
    abet:(on-hear-packet:(make-peer-core peer-state channel) lane shut-packet)
  ::  +enqueue-alien-packet: store packet from untrusted source
  ::
  ::    Also requests key and life from Jael on first contact.
  ::
  ++  enqueue-alien-packet
    |=  [=lane =packet]
    ^+  event-core
    ::
    =/  sndr-state  (~(get by peers.ames-state) sndr.packet)
    ::  create a default $pending-requests on first contact
    ::
    =+  ^-  [already-pending=? todos=pending-requests]
        ?~  sndr-state
          [%.n *pending-requests]
        [%.y ?>(?=(%alien -.u.sndr-state) +.u.sndr-state)]
    ::  enqueue unprocessed packet and apply to permanent state
    ::
    =.  rcv-packets.todos  [[lane packet] rcv-packets.todos]
    ::
    =.  peers.ames-state
      (~(put by peers.ames-state) sndr.packet %alien todos)
    ::  ask jael for .sndr life and keys on first contact
    ::
    =?  event-core  !already-pending
      (emit duct %pass /alien %j %pubs sndr.packet)
    ::
    event-core
  ::  +enqueue-alien-message: store message to untrusted source
  ::
  ::    Also requests key and life from Jael on first contact.
  ::
  ++  enqueue-alien-message
    |=  [=ship =message]
    ^+  event-core
    ::
    =/  rcvr-state  (~(get by peers.ames-state) ship)
    ::  create a default $pending-requests on first contact
    ::
    =+  ^-  [already-pending=? todos=pending-requests]
        ?~  rcvr-state
          [%.n *pending-requests]
        [%.y ?>(?=(%alien -.u.rcvr-state) +.u.rcvr-state)]
    ::  enqueue unsent message and apply to permanent state
    ::
    =.  snd-messages.todos  [[duct message] snd-messages.todos]
    ::
    =.  peers.ames-state
      (~(put by peers.ames-state) ship %alien todos)
    ::  ask jael for .ship life and keys on first contact
    ::
    =?  event-core  !already-pending
      (emit duct %pass /alien %j %pubs ship)
    ::
    event-core
  ::  +on-west: handle request to send message
  ::
  ++  on-west
    |=  [=ship =message]
    ^+  event-core
    ::
    =/  rcvr-state  (~(get by peers.ames-state) ship)
    ::
    ?.  ?=([~ %known *] rcvr-state)
      (enqueue-alien-message ship message)
    ::
    =/  =peer-state  +.u.rcvr-state
    =/  =channel     [[our ship] now +>.ames-state -.peer-state]
    ::
    abet:(on-west:(make-peer-core peer-state channel) message)
  ::  +make-peer-core: create nested |peer-core for per-peer processing
  ::
  ++  make-peer-core
    |=  [=peer-state =channel]
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
    ::  +on-hear-packet: handle receipt of ack or message fragment
    ::
    ++  on-hear-packet
      |=  [=lane =shut-packet]
      ^+  peer-core
      ::
      ?:  ?=(%& -.meat.shut-packet)
        %+  on-hear-fragment
          %-  fall  :_  *message-still-state
          (~(get by rcv.peer-state) bone.shut-packet)
        [lane shut-packet]
      ::
      (on-hear-ack lane shut-packet)
    ::  +on-hear-ack: handle receipt of ack on packet or message, from unix
    ::
    ++  on-hear-ack
      |=  [=lane =shut-packet]
      ^+  peer-core
      ::  distinguish ack on single packet from ack on whole message
      ::
      ::    TODO: move conditional to message pump?
      ::
      =/  task=message-pump-task
        ?>  ?=(%| -.meat.shut-packet)
        ?:  ?=(%& -.p.meat.shut-packet)
          [%hear-fragment-ack message-num.shut-packet p.p.meat.shut-packet]
        [%hear-message-ack message-num.shut-packet p.p.meat.shut-packet]
      ::  TODO: is it correct to (mix 1 bone) here?
      ::
      (run-message-pump (mix 1 bone.shut-packet) task)
    ::  +run-message-pump: process a $message-pump-task and its effects
    ::
    ++  run-message-pump
      |=  [=bone task=message-pump-task]
      ^+  peer-core
      ::  pass .task to the |message-pump and apply state mutations
      ::
      =/  =message-pump-state
        (fall (~(get by snd.peer-state) bone) *message-pump-state)
      ::
      =/  message-pump    (make-message-pump message-pump-state channel)
      =^  pump-gifts      message-pump-state  (work:message-pump task)
      =.  snd.peer-state  (~(put by snd.peer-state) bone message-pump-state)
      ::
      =/  client-duct=^duct  (~(got by by-bone.ossuary.peer-state) bone)
      ::  process effects from |message-pump
      ::
      |^  ^+  peer-core
          ?~  pump-gifts  peer-core
          =*  gift  i.pump-gifts
          =.  peer-core
            ?-  -.gift
              %ack-message  (process-ack-message [message-num ok]:gift)
              %send         (process-send static-fragment.gift)
              %wait         (process-wait date.gift)
              %rest         (process-rest date.gift)
            ==
          $(pump-gifts t.pump-gifts)
      ::
      ++  process-ack-message
        |=  [=message-num ok=?]
        ^+  peer-core
        ::  positive ack gets emitted trivially
        ::
        ?:  ok
          (emit client-duct %give %rest ~)
        ::  nack; look up naxplanation or enqueue
        ::
        =/  nax-key  [bone message-num]
        ::
        ?~  naxplanation=(~(get by nax.peer-state) nax-key)
          ::  no naxplanation yet; enqueue
          ::
          =.  nax.peer-state  (~(put by nax.peer-state) nax-key ~)
          peer-core
        ::  |message-pump should never emit duplicate message acks
        ::
        ?>  ?=(^ u.naxplanation)
        ::  we have both nack packet and naxplanation; unqueue and emit
        ::
        =.  nax.peer-state  (~(del by nax.peer-state) nax-key)
        (emit client-duct %give %rest u.naxplanation)
      ::
      ++  process-send
        |=  =static-fragment
        ^+  peer-core
        ::  encrypt and encode .static-fragment to .blob bitstream
        ::
        ::    XOR .bone with 1 just before sending. TODO: bone docs
        ::
        =/  pak=shut-packet
          :*  our-life.channel
              her-life.channel
              (mix 1 bone)
              message-num.static-fragment
              %&  +.static-fragment
          ==
        ::
        =/  content  (encrypt symmetric-key.channel pak)
        =/  =packet  [[our her.channel] encrypted=%.y origin=~ content]
        =/  =blob    (encode-packet packet)
        ::  send to .her and her sponsors until we find a direct lane
        ::
        =/  rcvrs=(list ship)  [her her-sponsors]:channel
        ::
        |-  ^+  peer-core
        ?~  rcvrs  peer-core
        ::
        =/  peer  (~(get by peers.ames-state) i.rcvrs)
        ::
        ?.  ?=([~ %known *] peer)
          $(rcvrs t.rcvrs)
        ::
        ?~  route=route.u.+.peer
          $(rcvrs t.rcvrs)
        ::
        =.  peer-core
          (emit unix-duct.ames-state %give %send lane.u.route blob)
        ::
        ?:  direct.u.route
          peer-core
        $(rcvrs t.rcvrs)
      ::
      ++  process-wait
        |=  date=@da
        ^+  peer-core
        ::
        =/  =wire  (pump-timer-wire her.channel bone)
        (emit client-duct %pass wire %b %wait date)
      ::
      ++  process-rest
        |=  date=@da
        ^+  peer-core
        ::
        =/  =wire  (pump-timer-wire her.channel bone)
        (emit client-duct %pass wire %b %rest date)
      --
    ::  +on-hear-fragment: handle receipt of message fragment, from unix
    ::
    ++  on-hear-fragment
      |=  [=message-still-state =lane =shut-packet]
      ^+  peer-core
      ::  pass fragment to the |message-still for assembly into message
      ::
      =/  message-still  (make-message-still message-still-state channel)
      ::
      =^  still-gifts  message-still-state
        (work:message-still %hear lane shut-packet)
      ::
      =.  rcv.peer-state
        (~(put by rcv.peer-state) bone.shut-packet message-still-state)
      ::
      |-  ^+  peer-core
      ?~  still-gifts  peer-core
      ::
      =*  gift  i.still-gifts
      =.  peer-core
        ?-    -.gift
            %hear-message
          !!
        ::
            %ack-fragment
          !!
        ::
            %ack-message
          !!
        ==
      $(still-gifts t.still-gifts)
    ::  +on-west: handle request to send message
    ::
    ++  on-west
      |=  =message
      ^+  peer-core
      ::
      =^  =bone  ossuary.peer-state  (get-bone ossuary.peer-state duct)
      ::
      (run-message-pump bone %send message)
    --
  --
::  +make-message-pump: constructor for |message-pump
::
++  make-message-pump
  |=  [state=message-pump-state =channel]
  =|  gifts=(list message-pump-gift)
  ::
  |%
  ++  message-pump  .
  ++  give  |=(gift=message-pump-gift message-pump(gifts [gift gifts]))
  ++  packet-pump
    (make-packet-pump packet-pump-state.state channel)
  ::  +work: handle a $message-pump-task
  ::
  ++  work
    |=  task=message-pump-task
    ^+  [gifts state]
    ::
    =~  ?-  -.task
            %send  (on-send message.task)
            %hear-fragment-ack
          (on-hear-fragment-ack [message-num fragment-num]:task)
        ::
            %hear-message-ack
          (on-hear-message-ack [message-num ok lag]:task)
        ::
            *  (run-packet-pump task)
        ==
        feed-packets
        (run-packet-pump %finalize ~)
        [(flop gifts) state]
    ==
  ::  +on-send: handle request to send a message
  ::
  ++  on-send
    |=  =message
    ^+  message-pump
    ::
    =.  unsent-messages.state  (~(put to unsent-messages.state) message)
    message-pump
  ::  +on-hear-fragment-ack: handle packet acknowledgment
  ::
  ++  on-hear-fragment-ack
    |=  [=message-num =fragment-num]
    ^+  message-pump
    ::  pass to |packet-pump unless duplicate or future ack
    ::
    ?.  (is-message-num-in-range message-num)
      message-pump
    (run-packet-pump %hear-fragment-ack message-num fragment-num)
  ::  +on-hear-message-ack: handle message-level acknowledgment
  ::
  ++  on-hear-message-ack
    |=  [=message-num ok=? lag=@dr]
    ^+  message-pump
    ::  ignore duplicate and future acks
    ::
    ?.  (is-message-num-in-range message-num)
      message-pump
    ::  clear and print .unsent-fragments if nonempty
    ::
    =?    unsent-fragments.state
        &(=(current next) ?=(^ unsent-fragments)):state
      ::
      ~&  %early-message-ack^ok^her.channel
      ~
    ::  clear all packets from this message from the packet pump
    ::
    =.  message-pump  (run-packet-pump %hear-message-ack message-num lag)
    ::  enqueue this ack to be sent back to local client vane
    ::
    =.  queued-message-acks.state
      (~(put by queued-message-acks.state) message-num ok)
    ::  emit local acks from .queued-message-acks until incomplete
    ::
    |-  ^+  message-pump
    ::  if .current hasn't been fully acked, we're done
    ::
    ?~  ack=(~(get by queued-message-acks.state) current.state)
      message-pump
    ::  .current is complete; pop, emit local ack, and try next message
    ::
    =.  queued-message-acks.state
      (~(del by queued-message-acks.state) current.state)
    ::
    =.  message-pump  (give %ack-message current.state ok.u.ack)
    ::
    $(current.state +(current.state))
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
    =^  message  unsent-messages.state  ~(get to unsent-messages.state)
    ::  break .message into .chunks and set as .unsent-fragments
    ::
    =.  unsent-fragments.state
      ::
      =/  chunks  (rip 13 (jam message))
      =/  num-fragments=fragment-num  (lent chunks)
      =|  counter=@
      ::
      |-  ^-  (list static-fragment)
      ?~  chunks  ~
      ::
      :-  [message-num=next.state num-fragments counter i.chunks]
      ::
      $(chunks t.chunks, counter +(counter))
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
  --
::  +make-packet-pump: construct |packet-pump core
::
++  make-packet-pump
  |=  [state=packet-pump-state =channel]
  =|  gifts=(list packet-pump-gift)
  |%
  ++  packet-pump  .
  ++  give  |=(packet-pump-gift packet-pump(gifts [+< gifts]))
  ::  +packet-queue: type for all sent fragments, ordered by sequence number
  ::
  ++  packet-queue
    %-  (ordered-map live-packet-key live-packet-val)
    |=  [a=live-packet-key b=live-packet-key]
    ^-  ?
    ::
    ?:  (lth message-num.a message-num.b)
      %.y
    ?:  (gth message-num.a message-num.b)
      %.n
    (lte fragment-num.a fragment-num.b)
  ::  +gauge: inflate a |pump-gauge to track congestion control
  ::
  ++  gauge  (make-pump-gauge now.channel metrics.state)
  ::  +work: handle $packet-pump-task request
  ::
  ++  work
    |=  task=packet-pump-task
    ^+  [gifts state]
    ::
    =-  [(flop gifts) state]
    ::
    ?-  -.task
      %hear-fragment-ack  (on-hear-fragment-ack [message-num fragment-num]:task)
      %hear-message-ack   (on-hear-message-ack message-num.task)
      %wake               resend-lost(next-wake.state ~)
      %finalize           set-wake
    ==
  ::  +resend-lost: resend as many lost packets as .gauge will allow
  ::
  ++  resend-lost
    ^+  packet-pump
    ::
    =-  =.  packet-pump  core.-
        =.  live.state   live.-
        packet-pump
    ::  acc: state to thread through traversal
    ::
    ::    num-slots: start with max retries; decrement on each resend
    ::
    =|  $=  acc
        $:  num-slots=_num-retry-slots:gauge
            core=_packet-pump
        ==
    ::
    ^+  [acc live=live.state]
    ::
    %-  (traverse:packet-queue _acc)
    ::
    :^    live.state
        start=~
      acc
    |=  $:  acc=_acc
            key=live-packet-key
            val=live-packet-val
        ==
    ^-  [new-val=(unit live-packet-val) stop=? _acc]
    ::  load mutant environment
    ::
    =.  packet-pump  core.acc
    ::  if we can't send any more packets, we're done
    ::
    ?:  =(0 num-slots.acc)
      [`val stop=%.y acc]
    ::  if the packet hasn't expired, we're done
    ::
    ?:  (gte expiry.val now.channel)
      [`val stop=%.y acc]
    ::  packet has expired so re-send it
    ::
    =/  =static-fragment
      =>  [key val]
      [message-num num-fragments fragment-num fragment]
    ::
    =.  packet-pump    (give %send static-fragment)
    =.  metrics.state  (on-resent:gauge -.val)
    ::  update $sent-packet-state in .val and continue
    ::
    =.  expiry.val     (next-retry-expiry:gauge -.val)
    =.  sent-date.val  now.channel
    =.  retried.val    %.y
    ::
    [`val stop=%.n (dec num-slots.acc) packet-pump]
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
    ::  resend lost packets first, possibly adjusting congestion control
    ::
    =.  packet-pump  resend-lost
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
      :-  :+  expiry=next-expiry:gauge
            sent-date=now.channel
          retried=%.n
      [num-fragments fragment]
    ::  update .live and .metrics
    ::
    =.  live.state     (gas:packet-queue live.state send-list)
    =.  metrics.state  (on-sent:gauge (lent send-list))
    ::  TMI
    ::
    =>  .(sent `(list static-fragment)`sent)
    ::  emit a $packet-pump-gift for each packet to send
    ::
    |-  ^+  packet-pump
    ?~  sent  packet-pump
    =.  packet-pump  (give %send i.sent)
    $(sent t.sent)
  ::  +on-hear-fragment-ack: handle ack on a live packet
  ::
  ::    Traverse .live from the head, marking packets as lost until we
  ::    find the acked packet. Then delete the acked packet and try to
  ::    resend lost packets.
  ::
  ::    If we don't find the acked packet, no-op: no mutations, effects,
  ::    or resending of lost packets.
  ::
  ++  on-hear-fragment-ack
    |=  [=message-num =fragment-num]
    ^+  packet-pump
    ::
    =-  ::  if no sent packet matches the ack, don't apply mutations or effects
        ::
        ?.  found.-
          packet-pump
        ::
        =.  metrics.state  metrics.-
        =.  live.state     live.-
        ::
        resend-lost
    ::
    ^-  $:  [found=? metrics=pump-metrics]
            live=(tree [live-packet-key live-packet-val])
        ==
    ::
    =/  acc=[found=? metrics=pump-metrics]  [%.n metrics.state]
    ::
    %-  (traverse:packet-queue _acc)
    ::
    :^    live.state
        start=~
      acc
    |=  $:  acc=_acc
            key=live-packet-key
            val=live-packet-val
        ==
    ^-  [new-val=(unit live-packet-val) stop=? _acc]
    ::
    =/  gauge  (make-pump-gauge now.channel metrics.acc)
    ::  is this the acked packet?
    ::
    ?:  =(key [message-num fragment-num])
      ::  delete acked packet, update metrics, and stop traversal
      ::
      :+  new-val=~
        stop=%.y
      [found=%.y metrics=(on-ack:gauge -.val)]
    ::  ack was out of order; mark expired, tell gauge, and continue
    ::
    :+  new-val=`val(expiry `@da`0)
      stop=%.n
    [found=%.n metrics=(on-skipped-packet:gauge -.val)]
  ::  +on-hear-message-ack: apply ack to all packets from .message-num
  ::
  ++  on-hear-message-ack
    |=  =message-num
    ^+  packet-pump
    ::
    =-  =.  metrics.state  metrics.-
        =.  live.state     live.-
        ::
        resend-lost
    ::
    ^-  $:  metrics=pump-metrics
            live=(tree [live-packet-key live-packet-val])
        ==
    ::
    %-  (traverse:packet-queue pump-metrics)
    ::
    :^    live.state
        start=~
      acc=metrics.state
    |=  $:  metrics=pump-metrics
            key=live-packet-key
            val=live-packet-val
        ==
    ^-  [new-val=(unit live-packet-val) stop=? pump-metrics]
    ::
    =/  gauge  (make-pump-gauge now.channel metrics)
    ::  if ack was out of order, mark expired and continue
    ::
    ?:  (lth message-num.key message-num)
      :+  new-val=`val(expiry `@da`0)
        stop=%.n
      metrics=(on-skipped-packet:gauge -.val)
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
    ::  if nonempty .live, peek at head to get next wake time
    ::
    =/  new-wake=(unit @da)
      ?~  head=(peek:packet-queue live.state)
        ~
      `expiry.val.u.head
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
::  +make-pump-gauge: construct |pump-gauge congestion control core
::
::    TODO: actual congestion control
::
++  make-pump-gauge
  |=  [now=@da pump-metrics]
  =*  metrics  +<+
  |%
  ::  +next-expiry: when should a newly sent fresh packet time out?
  ::
  ++  next-expiry
    ^-  @da
    ::
    (add now ~s5)
  ::  +next-retry-expiry: when should a resent packet time out?
  ::
  ++  next-retry-expiry
    |=  sent-packet-state
    ^-  @da
    (add now ~s10)
  ::  +has-slot: can we send a packet right now?
  ::
  ++  has-slot
    ^-  ?
    (gth num-slots 0)
  ::  +num-slots: how many packets can we send right now?
  ::
  ++  num-slots
    ^-  @ud
    ?.  (gth max-live num-live)
      0
    (sub max-live num-live)
  ::  +num-retry-slots: how many lost packets can we resend right now?
  ::
  ++  num-retry-slots
    ^-  @ud
    max-live
  ::  +on-skipped-packet: adjust metrics based on a misordered ack
  ::
  ::    TODO: decrease .max-live
  ::
  ++  on-skipped-packet
    |=  sent-packet-state
    metrics
  ::  +on-ack: adjust metrics based on a packet getting acknowledged
  ::
  ::    TODO: adjust .rtt and .max-live
  ::
  ++  on-ack
    |=  sent-packet-state
    ^-  pump-metrics
    ::
    metrics(num-live (dec num-live))
  ::  +on-sent: adjust metrics based on sending .num-sent fresh packets
  ::
  ++  on-sent
    |=  num-sent=@ud
    ^-  pump-metrics
    ::
    metrics(num-live (add num-sent num-live))
  ::  +on-resent: adjust metrics based on retrying an expired packet
  ::
  ++  on-resent
    |=  sent-packet-state
    ^-  pump-metrics
    metrics
  --
::
::
++  make-message-still
  |=  [=message-still-state =channel]
  =|  gifts=(list message-still-gift)
  |%
  ++  work
    |=  task=message-still-task
    ^+  [gifts message-still-state]
    ::
    =-  [(flop -.-) +.-]
    ::
    ?-  -.task
      %hear  (on-hear [lane shut-packet]:task)
      %done  (on-done [message-num error]:task)
    ==
  ::
  ::
  ++  on-hear
    |=  [=lane =shut-packet]
    ^+  [gifts message-still-state]
    ::
    !!
  ::
  ::
  ++  on-done
    |=  [=message-num error=(unit error)]
    ^+  [gifts message-still-state]
    ::
    !!
  --
::  +get-bone: find or make new bone for .duct in .ossuary
::
++  get-bone
  |=  [=ossuary =duct]
  ^+  [next-bone.ossuary ossuary]
  ::
  ?^  existing=(~(get by by-duct.ossuary) duct)
    [u.existing ossuary]
  ::
  :-  next-bone.ossuary
  :+  (add 2 next-bone.ossuary)
    (~(put by by-duct.ossuary) duct next-bone.ossuary)
  (~(put by by-bone.ossuary) next-bone.ossuary duct)
::
::
++  pump-timer-wire
  |=  [her=ship =bone]
  ^-  wire
  /pump/(scot %p her)/(scot %ud bone)
::  +encrypt: encrypt $shut-packet into atomic packet content
::
++  encrypt
  |=  [=symmetric-key plaintext=shut-packet]
  ^-  @
  ::
  (en:crub:crypto symmetric-key (jam plaintext))
::  +decrypt: decrypt packet content to a $shut-packet or die
::
++  decrypt
  |=  [=symmetric-key ciphertext=@]
  ^-  shut-packet
  ::
  ;;  shut-packet
  %-  cue
  %-  need
  (de:crub:crypto symmetric-key ciphertext)
::  +encode-packet: serialize a packet into a bytestream
::
++  encode-packet
  |=  packet
  ^-  blob
  ::
  =/  sndr-meta  (encode-ship-metadata sndr)
  =/  rcvr-meta  (encode-ship-metadata rcvr)
  ::  body: <<sndr rcvr (jam [origin content])>>
  ::
  ::    The .sndr and .rcvr ship addresses are encoded with fixed
  ::    lengths specified by the packet header. They live outside
  ::    the jammed-data section to simplify packet filtering in the
  ::    interpreter.
  ::
  =/  body=@
    ;:  mix
      sndr
      (lsh 3 size.sndr-meta rcvr)
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
