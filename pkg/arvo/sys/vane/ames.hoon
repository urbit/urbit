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
=,  able
=*  point               point:able:jael
=*  public-keys-result  public-keys-result:able:jael
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
  ==
=>
|%
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
--
=>
~%  %ames-generics  ..is  ~
|%
+|  %generics
::  $mk-item: constructor for +ordered-map item type
::
++  mk-item  |$  [key val]  [key=key val=val]
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
  ::  +del: delete .key from .a if it exists, producing value iff deleted
  ::
  ++  del
    |=  [a=(tree item) =key]
    ^-  [(unit val) (tree item)]
    ::
    ?~  a  [~ ~]
    ::  we found .key at the root; delete and rebalance
    ::
    ?:  =(key key.n.a)
      [`val.n.a (nip a)]
    ::  recurse left or right to find .key
    ::
    ?:  (compare key key.n.a)
      =+  [found lef]=$(a l.a)
      [found a(l lef)]
    =+  [found rig]=$(a r.a)
    [found a(r rig)]
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
    ::  +left: recurse on left subtree, copying mutant back into .l.a
    ::
    ++  left
      ^+  .
      ?~  a  .
      =/  lef  main(a l.a)
      lef(a a(l a.lef))
    ::  +right: recurse on right subtree, copying mutant back into .r.a
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
  ::    .b takes precedence over .a if keys overlap.
  ::
  ++  uni
    |=  [a=(tree item) b=(tree item)]
    ^-  (tree item)
    ::
    ?~  b  a
    ?~  a  b
    ?:  =(key.n.a key.n.b)
      ::
      [n=n.b l=$(a l.a, b l.b) r=$(a r.a, b r.b)]
    ::
    ?:  (mor key.n.a key.n.b)
      ::
      ?:  (compare key.n.b key.n.a)
        $(l.a $(a l.a, r.b ~), b r.b)
      $(r.a $(a r.a, l.b ~), b l.b)
    ::
    ?:  (compare key.n.a key.n.b)
      $(l.b $(b l.b, r.a ~), a r.a)
    $(r.b $(b r.b, l.a ~), a l.a)
  --
::
+|  %atomics
::
+$  bone           @udbone
+$  fragment       @uwfragment
+$  fragment-num   @udfragmentnum
+$  message-blob   @udmessageblob
+$  message-num    @udmessagenum
+$  private-key    @uwprivatekey
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
+$  packet  [dyad encrypted=? origin=(unit lane) content=*]
::  $open-packet: unencrypted packet payload, for comet self-attestation
::
::    This data structure gets signed and jammed to form the .contents
::    field of a $packet.
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
::  $naxplanation: nack trace; explains which message failed and why
::
+$  naxplanation  [=message-num =error]
::  $ack: positive ack, nack packet, or nack trace
::
+$  ack
  $%  [%ok ~]
      [%nack ~]
      [%naxplanation =error]
  ==
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
::
+$  ames-state
  $:  peers=(map ship ship-state)
      =unix=duct
      =life
      crypto-core=acru:ames
      =bug
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
::  $ship-state: all we know about a peer
::
::    %alien: no PKI data, so enqueue actions to perform once we learn it
::    %known: we know their life and public keys, so we have a channel
::
+$  ship-state
  $%  [%alien alien-agenda]
      [%known peer-state]
  ==
::  $alien-agenda: what to do when we learn a peer's life and keys
::
::    messages: pleas local vanes have asked us to send
::    packets: packets we've tried to send
::    heeds: local tracking requests; passed through into $peer-state
::
+$  alien-agenda
  $:  messages=(list [=duct =plea])
      packets=(set =blob)
      heeds=(set duct)
  ==
::  $peer-state: state for a peer with known life and keys
::
::    route: transport-layer destination for packets to peer
::    qos: quality of service; connection status to peer
::    ossuary: bone<->duct mapper
::    snd: per-bone message pumps to send messages as fragments
::    rcv: per-bone message sinks to assemble messages from fragments
::    nax: unprocessed nacks (negative acknowledgments)
::         Each value is ~ when we've received the ack packet but not a
::         nack-trace, or an error when we've received a nack-trace but
::         not the ack packet.
::
::         When we hear a nack packet or an explanation, if there's no
::         entry in .nax, we make a new entry. Otherwise, if this new
::         information completes the packet+nack-trace, we remove the
::         entry and emit a nack to the local vane that asked us to send
::         the message.
::    heeds: listeners for %clog notifications
::
+$  peer-state
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
::  $qos: quality of service; how is our connection to a peer doing?
::
::    .last-contact: last time we heard from peer, or if %unborn, when
::    we first started tracking time
::
+$  qos
  $~  [%unborn *@da]
  [?(%live %dead %unborn) last-contact=@da]
::  $ossuary: bone<->duct bijection and .next-bone to map to a duct
::
::    The first bone is 0. They increment by 4, since each flow includes
::    a bit for each message determining forward vs. backward and a
::    second bit for whether the message is on the normal flow or the
::    associated diagnostic flow (for naxplanations).
::
::    The least significant bit of a $bone is:
::    1 if "forward", i.e. we send %plea's on this flow, or
::    0 if "backward", i.e. we receive %plea's on this flow.
::
::    The second-least significant bit is 1 if the bone is a
::    naxplanation bone, and 0 otherwise.  Only naxplanation
::    messages can be sent on a naxplanation bone, as %boon's.
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
::    we can into |packet-pump, which sends every packet it eats.
::    Packets rejected by |packet-pump are placed in .unsent-fragments.
::
::    When we hear a packet ack, we send it to |packet-pump to be
::    removed from its queue of unacked packets.
::
::    When we hear a message ack (positive or negative), we treat that
::    as though all fragments have been acked.  If this message is not
::    .current, then this ack is for a future message and .current has
::    not yet been acked, so we place the ack in .queued-message-acks.
::
::    If we hear a message ack before we've sent all the fragments for
::    that message, clear .unsent-fragments and have |packet-pump delete
::    all sent fragments from the message. If this early message ack was
::    positive, print it out because it indicates the peer is not
::    behaving properly.
::
::    If the ack is for the current message, have |packet-pump delete
::    all packets from the message, give the message ack back
::    to the client vane, increment .current, and check if this next
::    message is in .queued-message-acks.  If it is, emit the message
::    (n)ack, increment .current, and check the next message.  Repeat
::    until .current is not fully acked.
::
::    The following equation is always true:
::    .next - .current == number of messages in flight
::
::    At the end of a task, |message-pump sends a %halt task to
::    |packet-pump, which can trigger a timer to be set or cleared based
::    on congestion control calculations. When the timer fires, it will
::    generally cause a packet to be re-sent.
::
::    Message sequence numbers start at 1 so that the first message will
::    be greater than .last-acked.message-sink-state on the receiver.
::
::    current: sequence number of earliest message sent or being sent
::    next: sequence number of next message to send
::    unsent-messages: messages to be sent after current message
::    unsent-fragments: fragments of current message waiting for sending
::    queued-message-acks: future message acks to be applied after current
::    packet-pump-state: state of corresponding |packet-pump
::
+$  message-pump-state
  $:  current=_`message-num`1
      next=_`message-num`1
      unsent-messages=(qeu message-blob)
      unsent-fragments=(list static-fragment)
      queued-message-acks=(map message-num ack)
      =packet-pump-state
  ==
+$  static-fragment
  $:  =message-num
      num-fragments=fragment-num
      =fragment-num
      =fragment
  ==
::  $packet-pump-state: persistent state for |packet-pump
::
::    next-wake: last timer we've set, or null
::    live: packets in flight; sent but not yet acked
::    metrics: congestion control information
::
+$  packet-pump-state
  $:  next-wake=(unit @da)
      live=(tree [live-packet-key live-packet-val])
      metrics=pump-metrics
  ==
::  $pump-metrics: congestion control state for a |packet-pump
::
::    This is an Ames adaptation of TCP's Reno congestion control
::    algorithm.  The information signals and their responses are
::    identical to those of the "NewReno" variant of Reno; the
::    implementation differs because Ames acknowledgments differ from
::    TCP's, because this code uses functional data structures, and
::    because TCP's sequence numbers reset when a peer becomes
::    unresponsive, whereas Ames sequence numbers only change when a
::    ship breaches.
::
::    A deviation from Reno is +fast-resend-after-ack, which re-sends
::    timed-out packets when a peer starts responding again after a
::    period of unresponsiveness.
::
::    If .skips reaches 3, we perform a fast retransmit and fast
::    recovery.  This corresponds to Reno's handling of "three duplicate
::    acks".
::
::    rto: retransmission timeout
::    rtt: roundtrip time estimate, low-passed using EWMA
::    rttvar: mean deviation of .rtt, also low-passed with EWMA
::    num-live: how many packets sent, awaiting ack
::    ssthresh: slow-start threshold
::    cwnd: congestion window; max unacked packets
::
+$  pump-metrics
  $:  rto=_~s1
      rtt=_~s1
      rttvar=_~s1
      ssthresh=_10.000
      cwnd=_1
      num-live=@ud
      counter=@ud
  ==
+$  live-packet
  $:  key=live-packet-key
      val=live-packet-val
  ==
+$  live-packet-key
  $:  =message-num
      =fragment-num
  ==
+$  live-packet-val
  $:  packet-state
      num-fragments=fragment-num
      =fragment
  ==
+$  packet-state
  $:  last-sent=@da
      retries=@ud
      skips=@ud
  ==
::  $message-sink-state: state of |message-sink to assemble messages
::
::    last-acked: highest $message-num we've fully acknowledged
::    last-heard: highest $message-num we've heard all fragments on
::    pending-vane-ack: heard but not processed by local vane
::    live-messages: partially received messages
::
+$  message-sink-state
  $:  last-acked=message-num
      last-heard=message-num
      pending-vane-ack=(qeu [=message-num message=*])
      live-messages=(map message-num partial-rcv-message)
      nax=(set message-num)
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
::  $queued-event: event to be handled after initial boot completes
::
+$  queued-event
  $%  [%call =duct type=* wrapped-task=(hobo task)]
      [%take =wire =duct type=* =sign]
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
      ==  ==
      $:  @tas
      $%  [%plea =ship =plea]
  ==  ==  ==
::  $sign: response from other vane
::
+$  sign
  $~  [%b %wake ~]
  $%  $:  %b
      $%  [%wake error=(unit tang)]
      ==  ==
      $:  %j
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
::    %wake: handle timer firing
::
+$  message-pump-task
  $%  [%memo =message-blob]
      [%hear =message-num =ack-meat]
      [%near =naxplanation]
      [%wake ~]
  ==
::  $message-pump-gift: effect from |message-pump
::
::    %done: report message acknowledgment
::    %send: emit message fragment
::    %wait: set a new timer at .date
::    %rest: cancel timer at .date
::
+$  message-pump-gift
  $%  [%done =message-num error=(unit error)]
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
::
+$  packet-pump-task
  $%  [%hear =message-num =fragment-num]
      [%done =message-num lag=@dr]
      [%halt ~]
      [%wake current=message-num]
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
  $%  [%done ok=?]
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
  ==
::  previous state versions, for +stay/+load migrations
::
+|  %plasmonics
::
+$  ames-state-2
  $:  peers=(map ship ship-state)
      =unix=duct
      =life
      crypto-core=acru:ames
      veb=_veb-all-off
  ==
::
+$  ames-state-1
  $:  peers=(map ship ship-state-1)
      =unix=duct
      =life
      crypto-core=acru:ames
  ==
+$  ship-state-1
  $%  [%alien alien-agenda]
      [%known peer-state-1]
  ==
+$  peer-state-1
  $:  $:  =symmetric-key
          =life
          =public-key
          sponsor=ship
      ==
      route=(unit [direct=? =lane])
      qos=qos-1
      =ossuary
      snd=(map bone message-pump-state)
      rcv=(map bone message-sink-state)
      nax=(set [=bone =message-num])
      heeds=(set duct)
  ==
+$  qos-1
  $~  [%unborn ~]
  $%  [%live last-contact=@da]
      [%dead last-contact=@da]
      [%unborn ~]
  ==
--
::  external vane interface
::
|=  pit=vase
::  larval ames, before %born sets .unix-duct; wraps adult ames core
::
=<  =*  adult-gate  .
    =|  queued-events=(qeu queued-event)
    ::
    |=  [our=ship now=@da eny=@ scry-gate=sley]
    =*  larval-gate  .
    =*  adult-core   (adult-gate +<)
    |%
    ::  +call: handle request $task
    ::
    ++  call
      |=  [=duct dud=(unit goof) type=* wrapped-task=(hobo task)]
      ::
      =/  =task  ((harden task) wrapped-task)
      ::
      ::  error notifications "downcast" to %crud or %hole
      ::
      =?  task  ?=(^ dud)
        ?-  -.task
          %crud  ~|(%crud-in-crud !!)
          %hear  [%hole [lane blob]:task]
          *      [%crud -.task tang.u.dud]
        ==
      ::
      ::  %born: set .unix-duct and start draining .queued-events
      ::
      ?:  ?=(%born -.task)
        ::  process %born using wrapped adult ames
        ::
        =^  moves  adult-gate  (call:adult-core duct dud type task)
        ::  if no events were queued up, metamorphose
        ::
        ?~  queued-events
          ~>  %slog.0^leaf/"ames: metamorphosis"
          [moves adult-gate]
        ::  kick off a timer to process the first of .queued-events
        ::
        =.  moves  :_(moves [duct %pass /larva %b %wait now])
        [moves larval-gate]
      ::  any other event: enqueue it until we have a .unix-duct
      ::
      ::    XX what to do with errors?
      ::
      =.  queued-events  (~(put to queued-events) %call duct type task)
      [~ larval-gate]
    ::  +take: handle response $sign
    ::
    ++  take
      |=  [=wire =duct dud=(unit goof) type=* =sign]
      ?^  dud
        ~|(%ames-larval-take-dud (mean tang.u.dud))
      ::  enqueue event if not a larval drainage timer
      ::
      ::    XX what to do with errors?
      ::
      ?.  =(/larva wire)
        =.  queued-events  (~(put to queued-events) %take wire duct type sign)
        [~ larval-gate]
      ::  larval event drainage timer; pop and process a queued event
      ::
      ?.  ?=([%b %wake *] sign)
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
          %call  (call:adult-core [duct ~ type wrapped-task]:+.first-event)
          %take  (take:adult-core [wire duct ~ type sign]:+.first-event)
        ==
      ::  .queued-events has been cleared; metamorphose
      ::
      ?~  queued-events
        ~>  %slog.0^leaf/"ames: metamorphosis"
        [moves adult-gate]
      ~>  %slog.0^leaf/"ames: larva: drain"
      ::  set timer to drain next event
      ::
      =.  moves  :_(moves [duct %pass /larva %b %wait now])
      [moves larval-gate]
    ::  lifecycle arms; mostly pass-throughs to the contained adult ames
    ::
    ++  scry  scry:adult-core
    ++  stay  [%3 %larva queued-events ames-state.adult-gate]
    ++  load
      |=  $=  old
          $%  $:  %3
              $%  [%larva events=_queued-events state=_ames-state.adult-gate]
                  [%adult state=_ames-state.adult-gate]
              ==  ==
          ::
              $:  %2
              $%  [%larva events=_queued-events state=ames-state-2]
                  [%adult state=ames-state-2]
              ==  ==
          ::
              $%  [%larva events=_queued-events state=ames-state-1]
                  [%adult state=ames-state-1]
          ==  ==
      ?-    old
          [%3 %adult *]  (load:adult-core %3 state.old)
          [%2 %adult *]  (load:adult-core %2 state.old)
          [%adult *]     (load:adult-core %1 state.old)
      ::
          [%3 %larva *]
        ~>  %slog.1^leaf/"ames: larva: load"
        =.  queued-events  events.old
        =.  adult-gate     (load:adult-core %3 state.old)
        larval-gate
      ::
          [%2 %larva *]
        ~>  %slog.1^leaf/"ames: larva: load"
        =.  queued-events  events.old
        =.  adult-gate     (load:adult-core %2 state.old)
        larval-gate
      ::
          [%larva *]
        ~>  %slog.0^leaf/"ames: larva: load"
        =.  queued-events  events.old
        =.  adult-gate     (load:adult-core %1 state.old)
        larval-gate
      ==
    --
::  adult ames, after metamorphosis from larva
::
=<
=|  =ames-state
|=  [our=ship now=@da eny=@ scry-gate=sley]
=*  ames-gate  .
=*  veb  veb.bug.ames-state
|%
::  +call: handle request $task
::
++  call
  |=  [=duct dud=(unit goof) type=* wrapped-task=(hobo task)]
  ^-  [(list move) _ames-gate]
  ::
  =/  =task  ((harden task) wrapped-task)
  ::
  ::  error notifications "downcast" to %crud or %hole
  ::
  =?  task  ?=(^ dud)
    ?-  -.task
      %crud  ~|(%crud-in-crud !!)
      %hear  [%hole [lane blob]:task]
      *      [%crud -.task tang.u.dud]
    ==
  ::
  =/  event-core  (per-event [our now eny scry-gate] duct ames-state)
  ::
  =^  moves  ames-state
    =<  abet
    ?-  -.task
      %born  on-born:event-core
      %crud  (on-crud:event-core [p q]:task)
      %hear  (on-hear:event-core [lane blob]:task)
      %heed  (on-heed:event-core ship.task)
      %hole  (on-hole:event-core [lane blob]:task)
      %init  (on-init:event-core ship=p.task)
      %jilt  (on-jilt:event-core ship.task)
      %sift  (on-sift:event-core ships.task)
      %spew  (on-spew:event-core veb.task)
      %vega  on-vega:event-core
      %wegh  on-wegh:event-core
      %plea  (on-plea:event-core [ship plea]:task)
    ==
  ::
  [moves ames-gate]
::  +take: handle response $sign
::
++  take
  |=  [=wire =duct dud=(unit goof) type=* =sign]
  ^-  [(list move) _ames-gate]
  ?^  dud
    ~|(%ames-take-dud (mean tang.u.dud))
  ::
  ::
  =/  event-core  (per-event [our now eny scry-gate] duct ames-state)
  ::
  =^  moves  ames-state
    =<  abet
    ?-  sign
      [@ %done *]   (on-take-done:event-core wire error.sign)
      [@ %boon *]   (on-take-boon:event-core wire payload.sign)
    ::
      [%b %wake *]  (on-take-wake:event-core wire error.sign)
    ::
      [%j %turf *]          (on-take-turf:event-core turfs.sign)
      [%j %private-keys *]  (on-priv:event-core [life vein]:sign)
      [%j %public-keys *]   (on-publ:event-core wire public-keys-result.sign)
    ==
  ::
  [moves ames-gate]
::  +stay: extract state before reload
::
++  stay  [%3 %adult ames-state]
::  +load: load in old state after reload
::
++  load
  |=  $=  old-state
      $%  [%1 ames-state-1]
          [%2 ames-state-2]
          [%3 ^ames-state]
      ==
  |^  ^+  ames-gate
      ::
      =?  old-state  ?=(%1 -.old-state)  %2^(state-1-to-2 +.old-state)
      =?  old-state  ?=(%2 -.old-state)  %3^(state-2-to-3 +.old-state)
      ::
      ?>  ?=(%3 -.old-state)
      ames-gate(ames-state +.old-state)
  ::
  ++  state-1-to-2
    |=  =ames-state-1
    ^-  ames-state-2
    ::
    =|  =ames-state-2
    =.  +.ames-state-2
      :*  unix-duct.ames-state-1
          life.ames-state-1
          crypto-core.ames-state-1
          veb=veb-all-off
      ==
    =.  peers.ames-state-2
      %-  ~(gas by *(map ship ship-state))
      %+  turn  ~(tap by peers.ames-state-1)
      |=  [peer=ship =ship-state-1]
      ^-  [ship ship-state]
      ?:  ?=(%alien -.ship-state-1)
        [peer ship-state-1]
      :+  peer  %known
      %=    +.ship-state-1
          qos
        ?+  -.qos.ship-state-1  qos.ship-state-1
          %unborn  [%unborn now]
        ==
      ==
    ames-state-2
  ::
  ++  state-2-to-3
    |=  =ames-state-2
    ^-  ^ames-state
    ::
    :*  peers.ames-state-2
        unix-duct.ames-state-2
        life.ames-state-2
        crypto-core.ames-state-2
        bug=[veb=veb.ames-state-2 ships=~]
    ==
  --
::  +scry: dereference namespace
::
::    The ones producing vases are expected to be used like this:
::
::    &tang [(sell .^(vase %a /=peer=/~zod)) ~]
::
++  scry
  |=  [fur=(unit (set monk)) ren=@tas why=shop syd=desk lot=coin tyl=path]
  ^-  (unit (unit cage))
  ?.  =(lot [%$ %da now])  ~
  ?.  =(%$ ren)  [~ ~]
  ?.  =([%& our] why)
    [~ ~]
  ?+    syd  ~
      %peer
    ?.  ?=([@ ~] tyl)  [~ ~]
    =/  who  (slaw %p i.tyl)
    ?~  who  [~ ~]
    =/  per  (~(get by peers.ames-state) u.who)
    =/  res
      ?-  per
        ~             %unknown
        [~ %alien *]  %alien
        [~ %known *]
        =,  u.per
        :*  %known
            symkeymug=(mug symmetric-key)
            life=life
            pubkey=public-key
            sponsor=sponsor
            route=route
            qos=qos
            ossuary=ossuary
            snd=~(key by snd)
            rcv=~(key by rcv)
            nax=nax
            heeds=heeds
        ==
      ==
    ``noun+!>(!>(res))
  ::
      %bones
    ?.  ?=([@ ~] tyl)  [~ ~]
    =/  who  (slaw %p i.tyl)
    ?~  who  [~ ~]
    =/  per  (~(get by peers.ames-state) u.who)
    ?.  ?=([~ %known *] per)  [~ ~]
    =/  res
      =,  u.per
      [snd=~(key by snd) rcv=~(key by rcv)]
    ``noun+!>(res)
  ::
      %snd-bone
    ?.  ?=([@ @ ~] tyl)  [~ ~]
    =/  who  (slaw %p i.tyl)
    ?~  who  [~ ~]
    =/  ost  (slaw %ud i.t.tyl)
    ?~  ost  [~ ~]
    =/  per  (~(get by peers.ames-state) u.who)
    ?.  ?=([~ %known *] per)  [~ ~]
    =/  mps  (~(get by snd.u.per) u.ost)
    ?~  mps  [~ ~]
    =/  res
      u.mps
    ``noun+!>(!>(res))
  ==
--
::  helpers
::
~%  %ames-helpers  +>+  ~
|%
++  per-event
  =|  moves=(list move)
  |=  [[our=ship now=@da eny=@ scry-gate=sley] =duct =ames-state]
  =*  veb  veb.bug.ames-state
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
    =+  ^-  [her=ship =bone]  (parse-bone-wire wire)
    ::
    =/  =peer-state  (got-peer-state her)
    =/  =channel     [[our her] now channel-state -.peer-state]
    =/  peer-core    (make-peer-core peer-state channel)
    ::  if processing succeded, send positive ack packet and exit
    ::
    ?~  error
      abet:(run-message-sink:peer-core bone %done ok=%.y)
    ::  failed; send message nack packet
    ::
    =.  event-core  abet:(run-message-sink:peer-core bone %done ok=%.n)
    =/  =^peer-state  (got-peer-state her)
    =/  =^channel     [[our her] now channel-state -.peer-state]
    ::  construct nack-trace message, referencing .failed $message-num
    ::
    =/  failed=message-num  last-acked:(~(got by rcv.peer-state) bone)
    =/  =naxplanation  [failed u.error]
    =/  =message-blob  (jam naxplanation)
    ::  send nack-trace message on associated .nack-trace-bone
    ::
    =.  peer-core              (make-peer-core peer-state channel)
    =/  nack-trace-bone=^bone  (mix 0b10 bone)
    ::
    abet:(run-message-pump:peer-core nack-trace-bone %memo message-blob)
  ::  +on-sift: handle request to filter debug output by ship
  ::
  ++  on-sift
    |=  ships=(list ship)
    ^+  event-core
    =.  ships.bug.ames-state  (sy ships)
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
      ==
    event-core
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
    |=  [=lane =blob]
    ^+  event-core
    (on-hear-packet lane (decode-packet blob) ok=%.y)
  ::  +on-hole: handle packet crash notification
  ::
  ++  on-hole
    |=  [=lane =blob]
    ^+  event-core
    (on-hear-packet lane (decode-packet blob) ok=%.n)
  ::  +on-hear-packet: handle mildly processed packet receipt
  ::
  ++  on-hear-packet
    |=  [=lane =packet ok=?]
    ^+  event-core
    ::
    ?:  =(our sndr.packet)
      event-core
    ::
    %.  +<
    ::
    ?.  =(our rcvr.packet)
      on-hear-forward
    ::
    ?:  encrypted.packet
      on-hear-shut
    on-hear-open
  ::  +on-hear-forward: maybe forward a packet to someone else
  ::
  ::    Note that this performs all forwarding requests without
  ::    filtering.  Any protection against DDoS amplification will be
  ::    provided by Vere.
  ::
  ++  on-hear-forward
    |=  [=lane =packet ok=?]
    ^+  event-core
    %-  %^  trace  for.veb  sndr.packet
        |.("forward: {<sndr.packet>} -> {<rcvr.packet>}")
    ::  set .origin.packet if it doesn't already have one, re-encode, and send
    ::
    =?  origin.packet  ?=(~ origin.packet)  `lane
    =/  =blob  (encode-packet packet)
    (send-blob & rcvr.packet blob)
  ::  +on-hear-open: handle receipt of plaintext comet self-attestation
  ::
  ++  on-hear-open
    |=  [=lane =packet ok=?]
    ^+  event-core
    ::  if we already know .sndr, ignore duplicate attestation
    ::
    =/  ship-state  (~(get by peers.ames-state) sndr.packet)
    ?:  ?=([~ %known *] ship-state)
      event-core
    ::  deserialize and type-check packet contents
    ::
    ?>  ?=(@ content.packet)
    =+  ;;  [signature=@ signed=@]  (cue content.packet)
    =+  ;;  =open-packet            (cue signed)
    ::  assert .our and .her and lives match
    ::
    ?>  .=       sndr.open-packet  sndr.packet
    ?>  .=       rcvr.open-packet  our
    ?>  .=  sndr-life.open-packet  1
    ?>  .=  rcvr-life.open-packet  life.ames-state
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
    =/  key  (end 8 1 (rsh 3 1 public-key.open-packet))
    ?>  (veri:ed:crypto signature signed key)
    ::  store comet as peer in our state
    ::
    =.  peers.ames-state
      %+  ~(put by peers.ames-state)  sndr.packet
      ^-  ^ship-state
      :-  %known
      =|  =peer-state
      =/  our-private-key  sec:ex:crypto-core.ames-state
      =/  =symmetric-key
        (derive-symmetric-key public-key.open-packet our-private-key)
      ::
      %_  peer-state
        qos            [%unborn now]
        symmetric-key  symmetric-key
        life           sndr-life.open-packet
        public-key     public-key.open-packet
        sponsor        (^sein:title sndr.packet)
        route          `[direct=%.n lane]
      ==
    ::
    event-core
  ::  +on-hear-shut: handle receipt of encrypted packet
  ::
  ++  on-hear-shut
    |=  [=lane =packet ok=?]
    ^+  event-core
    ::  encrypted packet content must be an encrypted atom
    ::
    ?>  ?=(@ content.packet)
    ::
    =/  sndr-state  (~(get by peers.ames-state) sndr.packet)
    ::  if we don't know them, maybe enqueue a jael %public-keys request
    ::
    ::    Ignore encrypted packets from alien comets.
    ::    TODO: maybe crash?
    ::
    ?.  ?=([~ %known *] sndr-state)
      ?:  =(%pawn (clan:title sndr.packet))
        event-core
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
    =/  =shut-packet  (decrypt symmetric-key.channel content.packet)
    ::  ward against replay attacks
    ::
    ::    We only accept packets from a ship at their known life, and to
    ::    us at our current life.
    ::
    ~|  our-life=[expected=our-life.channel got=rcvr-life.shut-packet]
    ~|  her-life=[expected=her-life.channel got=sndr-life.shut-packet]
    ?>  =(sndr-life.shut-packet her-life.channel)
    ?>  =(rcvr-life.shut-packet our-life.channel)
    ::  non-galaxy: update route with heard lane or forwarded lane
    ::
    =?    route.peer-state
        ?:  =(%czar (clan:title her.channel))
          %.n
        =/  is-old-direct=?  ?=([~ %& *] route.peer-state)
        =/  is-new-direct=?  ?=(~ origin.packet)
        ::  old direct takes precedence over new indirect
        ::
        |(is-new-direct !is-old-direct)
      ::
      ?~  origin.packet
        `[direct=%.y lane]
      `[direct=%.n u.origin.packet]
    ::  perform peer-specific handling of packet
    ::
    =/  peer-core  (make-peer-core peer-state channel)
    abet:(on-hear-shut-packet:peer-core lane shut-packet ok)
  ::  +on-take-boon: receive request to give message to peer
  ::
  ++  on-take-boon
    |=  [=wire payload=*]
    ^+  event-core
    ::
    =+  ^-  [her=ship =bone]  (parse-bone-wire wire)
    ::
    =/  =peer-state  (got-peer-state her)
    =/  =channel     [[our her] now channel-state -.peer-state]
    ::
    abet:(on-memo:(make-peer-core peer-state channel) bone payload %boon)
  ::  +on-plea: handle request to send message
  ::
  ++  on-plea
    |=  [=ship =plea]
    ^+  event-core
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
        "plea {<sndr^rcvr^bone^vane.plea^path.plea>}"
    ::
    abet:(on-memo:(make-peer-core peer-state channel) bone plea %plea)
  ::  +on-take-wake: receive wakeup or error notification from behn
  ::
  ++  on-take-wake
    |=  [=wire error=(unit tang)]
    ^+  event-core
    ::
    =/  res=(unit [her=ship =bone])  (parse-pump-timer-wire wire)
    ?~  res
      %-  (slog leaf+"ames: got timer for strange wire: {<wire>}" ~)
      event-core
    ::
    =/  state=(unit peer-state)  (get-peer-state her.u.res)
    ?~  state
      %-  (slog leaf+"ames: got timer for strange ship: {<her.u.res>}, ignoring" ~)
      event-core
    ::
    =/  =channel  [[our her.u.res] now channel-state -.u.state]
    ::
    abet:(on-wake:(make-peer-core u.state channel) bone.u.res error)
  ::  +on-init: first boot; subscribe to our info from jael
  ::
  ++  on-init
    |=  our=ship
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
          event-core
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
        ~>  %slog.0^leaf/"ames: breach unknown {<our^ship>}"
        event-core
      ::  if an alien breached, this doesn't affect us
      ::
      ?:  ?=([~ %alien *] ship-state)
        ~>  %slog.0^leaf/"ames: breach alien {<our^ship>}"
        event-core
      ~>  %slog.0^leaf/"ames: breach peer {<our^ship>}"
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
      =/  text=(unit tape)  (qos-update-text ship old-qos qos.peer-state)
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
        =.  sponsor.point  `(scry-for-sponsor ship)
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
    ::    TODO: handle sponsor loss
    ::
    ++  on-publ-sponsor
      |=  [=ship sponsor=(unit ship)]
      ^+  event-core
      ::
      ?~  sponsor
        ~|  %ames-lost-sponsor^our^ship  !!
      ::
      =/  =peer-state         (got-peer-state ship)
      =.  sponsor.peer-state  u.sponsor
      ::
      =.  peers.ames-state  (~(put by peers.ames-state) ship %known peer-state)
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
      =.  public-key.peer-state     public-key
      =.  symmetric-key.peer-state  symmetric-key
      =.  sponsor.peer-state
        ?^  sponsor.point
          u.sponsor.point
        (scry-for-sponsor ship)
      ::  automatically set galaxy route, since unix handles lookup
      ::
      =?  route.peer-state  ?=(%czar (clan:title ship))
        `[direct=%.y lane=[%& ship]]
      ::
      =.  peers.ames-state
        (~(put by peers.ames-state) ship %known peer-state)
      ::
      event-core
    ::  +scry-for-sponsor: ask jael for .who's sponsoring ship
    ::
    ++  scry-for-sponsor
      |=  who=ship
      ^-  ship
      ;;  ship
      =<  q.q  %-  need  %-  need
      %-  scry-gate
      [[%141 %noun] ~ %j `beam`[[our %sein %da now] /(scot %p who)]]
    --
  ::  +on-take-turf: relay %turf move from jael to unix
  ::
  ++  on-take-turf
    |=  turfs=(list turf)
    ^+  event-core
    ::
    (emit unix-duct.ames-state %give %turf turfs)
  ::  +on-wegh: produce memory usage report
  ::
  ++  on-wegh
    ^+  event-core
    ::
    =+  [known alien]=(skid ~(tap by peers.ames-state) |=(^ =(%known +<-)))
    ::
    %-  emit
    :^  duct  %give  %mass
    :+  %ames  %|
    :~  peers-known+&+known
        peers-alien+&+alien
        dot+&+ames-state
    ==
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
      (scry-gate [%141 %noun] ~ %j `beam`[[our %turf %da now] /])
    ::
    (emit unix-duct.ames-state %give %turf turfs)
  ::  +on-vega: handle kernel reload
  ::
  ++  on-vega  event-core
  ::  +enqueue-alien-todo: helper to enqueue a pending request
  ::
  ::    Also requests key and life from Jael on first request.
  ::    On a comet, enqueues self-attestation packet on first request.
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
    ::  ask jael for .sndr life and keys on first contact
    ::
    ?:  already-pending
      event-core
    (emit duct %pass /public-keys %j %public-keys [n=ship ~ ~])
  ::  +send-blob: fire packet at .ship and maybe sponsors
  ::
  ::    Send to .ship and sponsors until we find a direct lane,
  ::    skipping .our in the sponsorship chain.
  ::
  ::    If we have no PKI data for a recipient, enqueue the packet and
  ::    request the information from Jael if we haven't already.
  ::
  ++  send-blob
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
    ::
    =/  =open-packet
      :*  ^=  public-key  pub:ex:crypto-core.ames-state
          ^=        sndr  our
          ^=   sndr-life  life.ames-state
          ^=        rcvr  her
          ^=   rcvr-life  her-life
      ==
    ::
    =/  signed=@  (sign:as:crypto-core.ames-state (jam open-packet))
    =/  =packet   [[our her] encrypted=%.n origin=~ signed]
    ::
    (encode-packet packet)
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
      ?~  text=(qos-update-text her.channel old-qos new-qos)
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
        ?:  =(0 (end 0 1 bone))
          ~
        `u=message-pump-state
      ::  clogged: are five or more response messages unsent to this peer?
      ::
      =/  clogged=?
        =|  acc=@ud
        |-  ^-  ?
        ?~  pumps
          %.n
        =.  acc
          %+  add  acc
          %+  add
            ::  in-flight messages
            ::
            (sub [next current]:i.pumps)
          ::  queued messages
          ::
          ~(wyt in unsent-messages.i.pumps)
        ::
        ?:  (gte acc 5)
          %.y
        $(pumps t.pumps)
      ::  if clogged, notify client vanek
      ::
      ?.  clogged
        peer-core
      %+  roll  ~(tap in heeds.peer-state)
      |=([d=^duct core=_peer-core] (emit:core d %give %clog her.channel))
    ::  +on-hear-shut-packet: handle receipt of ack or message fragment
    ::
    ++  on-hear-shut-packet
      |=  [=lane =shut-packet ok=?]
      ^+  peer-core
      ::  update and print connection status
      ::
      =.  peer-core  (update-qos %live last-contact=now)
      ::
      =/  =bone  bone.shut-packet
      ::
      ?:  ?=(%& -.meat.shut-packet)
        (run-message-sink bone %hear lane shut-packet ok)
      ::  ignore .ok for |message-pump; just try again on error
      ::
      ::    Note this implies that vanes should never crash on %done,
      ::    since we have no way to continue using the flow if they do.
      ::
      (run-message-pump bone %hear [message-num +.meat]:shut-packet)
    ::  +on-memo: handle request to send message
    ::
    ++  on-memo
      |=  [=bone payload=* valence=?(%plea %boon)]
      ^+  peer-core
      ::  if we haven't been trying to talk to %live, reset timer
      ::
      =?    last-contact.qos.peer-state
          ?&  ?=(%live -.qos.peer-state)
              %-  ~(all by snd.peer-state)
              |=  =message-pump-state
              =(~ live.packet-pump-state.message-pump-state)
          ==
        now
      ::
      =/  =message-blob  (jam payload)
      =.  peer-core  (run-message-pump bone %memo message-blob)
      ::
      ?:  &(=(%boon valence) ?=(?(%dead %unborn) -.qos.peer-state))
        check-clog
      peer-core
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
      =.  bone.shut-packet  (mix 1 bone.shut-packet)
      ::
      =/  content  (encrypt symmetric-key.channel shut-packet)
      =/  =packet  [[our her.channel] encrypted=%.y origin=~ content]
      =/  =blob    (encode-packet packet)
      ::
      =.  event-core  (send-blob | her.channel blob)
      peer-core
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
      =/  message-pump    (make-message-pump message-pump-state channel)
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
        ::  if odd bone, ack is on "subscription update" message; no-op
        ::
        ?:  =(1 (end 0 1 bone))
          peer-core
        ::  even bone; is this bone a nack-trace bone?
        ::
        ?:  =(1 (end 0 1 (rsh 0 1 bone)))
          ::  nack-trace bone; assume .ok, clear nack from |message-sink
          ::
          =/  target-bone=^bone  (mix 0b10 bone)
          ::
          (run-message-sink target-bone %drop message-num)
        ::  not a nack-trace bone; relay ack to client vane
        ::
        (emit (got-duct bone) %give %done error)
      ::  +on-pump-send: emit message fragment requested by |message-pump
      ::
      ++  on-pump-send
        |=  =static-fragment
        ^+  peer-core
        ::  encrypt and encode .static-fragment to .blob bitstream
        ::
        %-  send-shut-packet  :*
          our-life.channel
          her-life.channel
          bone
          message-num.static-fragment
          %&  +.static-fragment
        ==
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
      ::  pass .task to the |message-sink and apply state mutations
      ::
      =/  =message-sink-state
        (~(gut by rcv.peer-state) bone *message-sink-state)
      ::
      =/  message-sink    (make-message-sink message-sink-state channel)
      =^  sink-gifts      message-sink-state  (work:message-sink task)
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
            ==
          $(sink-gifts t.sink-gifts)
      ::  +on-sink-send: emit ack packet as requested by |message-sink
      ::
      ++  on-sink-send
        |=  [=message-num =ack-meat]
        ^+  peer-core
        ::
        %-  send-shut-packet  :*
          our-life.channel
          her-life.channel
          bone
          message-num
          %|  ack-meat
        ==
      ::  +on-sink-memo: dispatch message received by |message-sink
      ::
      ::    odd bone:                %plea request message
      ::    even bone, 0 second bit: %boon response message
      ::    even bone, 1 second bit: nack-trace %boon message
      ::
      ++  on-sink-memo
        ?:  =(1 (end 0 1 bone))
          on-sink-plea
        ?:  =(0 (end 0 1 (rsh 0 1 bone)))
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
        ::  send ack unconditionally
        ::
        =.  peer-core  (emit (got-duct bone) %give %boon message)
        =.  peer-core  (run-message-sink bone %done ok=%.y)
        ::
        ?.  ?=([%hear * * ok=%.n] task)
          ::  fresh boon; give message to client vane
          ::
          %-  (trace msg.veb |.("boon {<her.channel^bone -.task>}"))
          peer-core
        ::  we previously crashed on this message; notify client vane
        ::
        %-  (trace msg.veb |.("crashed on boon {<her.channel^bone -.task>}"))
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
        %-  (trace msg.veb |.("nack trace {<her.channel^bone>}"))
        ::
        =+  ;;  =naxplanation  message
        ::  ack nack-trace message (only applied if we don't later crash)
        ::
        =.  peer-core  (run-message-sink bone %done ok=%.y)
        ::  flip .bone's second bit to find referenced flow
        ::
        =/  target-bone=^bone  (mix 0b10 bone)
        ::  notify |message-pump that this message got naxplained
        ::
        (run-message-pump target-bone %near naxplanation)
      ::  +on-sink-plea: handle request message received by |message-sink
      ::
      ++  on-sink-plea
        |=  [=message-num message=*]
        ^+  peer-core
        %-  (trace msg.veb |.("plea {<her.channel^bone>}"))
        ::  is this the first time we're trying to process this message?
        ::
        ?.  ?=([%hear * * ok=%.n] task)
          ::  fresh plea; pass to client vane
          ::
          =+  ;;  =plea  message
          ::
          =/  =wire  (make-bone-wire her.channel bone)
          ::
          ?+  vane.plea  ~|  %ames-evil-vane^our^her.channel^vane.plea  !!
            %a  (emit duct %pass wire %a %plea her.channel plea)
            %c  (emit duct %pass wire %c %plea her.channel plea)
            %g  (emit duct %pass wire %g %plea her.channel plea)
            %j  (emit duct %pass wire %j %plea her.channel plea)
          ==
        ::  we previously crashed on this message; send nack
        ::
        =.  peer-core  (run-message-sink bone %done ok=%.n)
        ::  also send nack-trace with blank .error for security
        ::
        =/  nack-trace-bone=^bone  (mix 0b10 bone)
        =/  =naxplanation  [message-num *error]
        =/  =message-blob  (jam naxplanation)
        ::
        (run-message-pump nack-trace-bone %memo message-blob)
      --
    --
  --
::  +make-message-pump: constructor for |message-pump
::
++  make-message-pump
  |=  [state=message-pump-state =channel]
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
      %memo  (on-memo message-blob.task)
      %wake  (run-packet-pump %wake current.state)
      %hear
        ?-  -.ack-meat.task
          %&  (on-hear [message-num fragment-num=p.ack-meat]:task)
          %|  (on-done [message-num ?:(ok.p.ack-meat [%ok ~] [%nack ~])]:task)
        ==
      %near  (on-done [message-num %naxplanation error]:naxplanation.task)
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
    |=  [=message-num =ack]
    ^+  message-pump
    ::  unsent messages from the future should never get acked
    ::
    ?>  (lth message-num next.state)
    ::  ignore duplicate message acks
    ::
    ?:  (lth message-num current.state)
      %-  (trace snd.veb |.("duplicate done {<current.state message-num>}"))
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
    ::  give %done to vane if we're ready
    ::
    ?-    -.u.cur
        %ok
      =.  message-pump  (give %done current.state ~)
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
      (peek:packet-queue:*make-packet-pump live.packet-pump-state.state)
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
  ::  +gauge: inflate a |pump-gauge to track congestion control
  ::
  ++  gauge  (make-pump-gauge now.channel metrics.state [her bug]:channel)
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
      %halt  set-wake
    ==
  ::  +on-wake: handle packet timeout
  ::
  ++  on-wake
    |=  current=message-num
    ^+  packet-pump
    ::  assert temporal coherence
    ::
    ?<  =(~ next-wake.state)
    ?>  (gte now.channel (need next-wake.state))
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
              |.("dead {<nums^show:gauge>}")
          (give %send u.static-fragment.res)
        packet-pump
    ::
    =|  acc=(unit static-fragment)
    ^+  [static-fragment=acc live=live.state]
    ::
    %^  (traverse:packet-queue _acc)  live.state  acc
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
    =.  metrics.state  (on-sent:gauge (lent send-list))
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
    %^  (traverse:packet-queue _acc)  live.state  acc
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
            (trace snd.veb |.("{<[fragment-num show:gauge]>}"))
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
    %^  (traverse:packet-queue _acc)  live.state  acc
    |=  $:  acc=_acc
            key=live-packet-key
            val=live-packet-val
        ==
    ^-  [new-val=(unit live-packet-val) stop=? _acc]
    ::
    =/  gauge  (make-pump-gauge now.channel metrics.acc [her bug]:channel)
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
        %-  (trace snd.veb |.("done {<message-num^show:gauge>}"))
        (fast-resend-after-ack message-num `fragment-num`0)
    ::
    ^+  [metrics=metrics.state live=live.state]
    ::
    %^  (traverse:packet-queue pump-metrics)  live.state  acc=metrics.state
    |=  $:  metrics=pump-metrics
            key=live-packet-key
            val=live-packet-val
        ==
    ^-  [new-val=(unit live-packet-val) stop=? pump-metrics]
    ::
    =/  gauge  (make-pump-gauge now.channel metrics [her bug]:channel)
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
    ::  if nonempty .live, peek at head to get next wake time
    ::
    =/  new-wake=(unit @da)
      ?~  head=(peek:packet-queue live.state)
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
  |=  [now=@da pump-metrics =ship =bug]
  =*  veb  veb.bug
  =*  metrics  +<+<
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
    (sub-safe cwnd num-live)
  ::  +on-sent: adjust metrics based on sending .num-sent fresh packets
  ::
  ++  on-sent
    |=  num-sent=@ud
    ^-  pump-metrics
    ::
    =.  num-live  (add num-live num-sent)
    metrics
  ::  +on-ack: adjust metrics based on a packet getting acknowledged
  ::
  ++  on-ack
    |=  =packet-state
    ^-  pump-metrics
    ::
    =.  counter  +(counter)
    =.  num-live  (dec num-live)
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
        |.("skip {<[resend=resend in-recovery=in-recovery show]>}")
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
  ::    We finish recovering when .num-live finally dips back down to
  ::    .cwnd.
  ::
  ++  in-recovery
    ^-  ?
    (gth num-live cwnd)
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
        num-live=num-live
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
    |=  task=message-sink-task
    ^+  [gifts state]
    ::
    =-  [(flop gifts) state]
    ::
    ?-  -.task
      %done  (on-done ok.task)
      %drop  (on-drop message-num.task)
      %hear  (on-hear [lane shut-packet ok]:task)
    ==
  ::  +on-hear: receive message fragment, possibly completing message
  ::
  ++  on-hear
    |=  [=lane =shut-packet ok=?]
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
      %-  (trace odd.veb |.("future %hear {<seq^last-acked.state>}"))
      message-sink
    ::
    =/  is-last-fragment=?  =(+(fragment-num) num-fragments)
    ::  always ack a dupe!
    ::
    ?:  (lte seq last-acked.state)
      ?.  is-last-fragment
        ::  single packet ack
        ::
        %-  (trace rcv.veb |.("send dupe ack {<seq^fragment-num>}"))
        (give %send seq %& fragment-num)
      ::  whole message (n)ack
      ::
      =/  ok=?  !(~(has in nax.state) seq)
      %-  (trace rcv.veb |.("send dupe message ack {<seq>} ok={<ok>}"))
      (give %send seq %| ok lag=`@dr`0)
    ::  last-acked<seq<=last-heard; heard message, unprocessed
    ::
    ::    Only true if we've heard some packets we haven't acked, which
    ::    doesn't happen for boons.
    ::
    ?:  (lte seq last-heard.state)
      ?:  is-last-fragment
        ::  drop last packet since we don't know whether to ack or nack
        ::
        %-  %+  trace  rcv.veb
            |.  ^-  tape
            =/  data
              :*  her.channel  seq
                  fragment-num  num-fragments
                  la=last-acked.state  lh=last-heard.state
              ==
            "hear last in-progress {<data>}"
        message-sink
      ::  ack all other packets
      ::
      %-  (trace rcv.veb |.("send ack-1 {<seq^fragment-num^num-fragments>}"))
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
            =/  data  [her.channel seq last-heard.state last-acked.state]
            "hear last dupe {<data>}"
        message-sink
      %-  (trace rcv.veb |.("send dupe ack {<her.channel^seq^fragment-num>}"))
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
      %-  (trace rcv.veb |.("send ack-2 {<seq^fragment-num^num-fragments>}"))
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
        |.("hear {<her.channel>} {<seq>} {<num-fragments.u.live>}kb")
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
    |=  ok=?
    ^+  message-sink
    ::
    =^  pending  pending-vane-ack.state  ~(get to pending-vane-ack.state)
    =/  =message-num  message-num.p.pending
    ::
    =.  last-acked.state  +(last-acked.state)
    =?  nax.state  !ok  (~(put in nax.state) message-num)
    ::
    =.  message-sink  (give %send message-num %| ok lag=`@dr`0)
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
::  +qos-update-text: notice text for if connection state changes
::
++  qos-update-text
  |=  [=ship old=qos new=qos]
  ^-  (unit tape)
  ::
  ?+  [-.old -.new]  ~
    [%unborn %live]  `"; {(scow %p ship)} is your neighbor"
    [%dead %live]    `"; {(scow %p ship)} is ok"
    [%live %dead]    `"; {(scow %p ship)} not responding still trying"
    [%unborn %dead]  `"; {(scow %p ship)} not responding still trying"
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
++  split-message
  |=  [=message-num =message-blob]
  ^-  (list static-fragment)
  ::
  =/  fragments=(list fragment)   (rip 13 message-blob)
  =/  num-fragments=fragment-num  (lent fragments)
  =|  counter=@
  ::
  |-  ^-  (list static-fragment)
  ?~  fragments  ~
  ::
  :-  [message-num num-fragments counter i.fragments]
  ::
  $(fragments t.fragments, counter +(counter))
::  +assemble-fragments: concatenate fragments into a $message
::
++  assemble-fragments
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
  %-  cue
  %+  can   13
  %+  turn  (flop sorted)
  |=(a=@ [1 a])
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
::  +make-bone-wire: encode ship and bone in wire for sending to vane
::
++  make-bone-wire
  |=  [her=ship =bone]
  ^-  wire
  ::
  /bone/(scot %p her)/(scot %ud bone)
::  +parse-bone-wire: decode ship and bone from wire from local vane
::
++  parse-bone-wire
  |=  =wire
  ^-  [her=ship =bone]
  ::
  ~|  %ames-wire-bone^wire
  ?>  ?=([%bone @ @ ~] wire)
  [`@p`(slav %p i.t.wire) `@ud`(slav %ud i.t.t.wire)]
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
  |=  [=public-key =private-key]
  ^-  symmetric-key
  ::
  ?>  =('b' (end 3 1 public-key))
  =.  public-key  (rsh 8 1 (rsh 3 1 public-key))
  ::
  ?>  =('B' (end 3 1 private-key))
  =.  private-key  (rsh 8 1 (rsh 3 1 private-key))
  ::
  `@`(shar:ed:crypto public-key private-key)
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
  =/  =dyad
    :-  sndr=(end 3 sndr-size body)
    rcvr=(cut 3 [sndr-size rcvr-size] body)
  ::
  ?.  =(protocol-version version)
    ~|  %ames-protocol^version^dyad  !!
  ?.  =(checksum (end 0 20 (mug body)))
    ~|  %ames-checksum^dyad  !!
  ::
  =+  ~|  %ames-invalid-packet
      ;;  [origin=(unit lane) content=*]
      ~|  %ames-invalid-noun
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
