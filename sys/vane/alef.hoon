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
    ++  self  .
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
      ?:  ?&  ?=(^ start)
              !(compare u.start key.n.a)
          ==
        right
      ::  inorder traversal: left -> node -> right, recursively
      ::
      =>  left
      ::  only continue rightward if we weren't told to stop
      =<  ?.  stop.acc
            right
          self
      ::  if left told us to stop, don't mutate node
      ::
      ?:  stop.acc  self
      ::  run .f on node, updating .stop.acc and .state.acc
      ::
      ?>  ?=(^ a)
      =^  res  acc  (f state.acc n.a)
      ::  TMI
      ::
      =>  .(a `(tree item)`a)
      =.  a
        ::  if .f requested node deletion, merge and balance .l.a and .r.a
        ::
        ?~  res  (nip a)
        ::  we kept the node; replace its .val; order is unchanged
        ::
        ?>  ?=(^ a)
        a(val.n u.res)
      ::
      self
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
::    .unsent-fragments.  We also insert an entry in .unacked-fragments
::    initialized with the total number of fragments in the message.
::
::    When we hear a packet ack, we send it to |packet-pump.  If we
::    haven't seen it before, |packet-pump reports the fresh ack.  We
::    then decrement that message's entry in .unacked-fragments.
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
::    pump-metrics: congestion control information
::
+$  packet-pump-state
  $:  next-wake=(unit @da)
      live=(tree [live-packet-key live-packet-val])
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
+$  live-packet-key  [=message-num =fragment-num]
+$  live-packet-val
  $:  expiry=@da
      sent-date=@da
      retried=?
      num-fragments=fragment-num
      =fragment
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
::  $message-still-state: state of |message-still to assemble messages
::
::    last-acked: highest $message-num we've fully acknowledged
::    last-heard: highest $message-num we've heard all fragments on
::    pending-vane-ack: heard but not processed by local vane
::    live-messages: partially received messages
::    naxplanations: enqueued nack diagnostics
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
      %west  !!
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
    ::
    ?.  ?=([~ %known *] sndr-state)
      (enqueue-alien-packet lane packet)
    ::
    =/  =peer-state   +.u.sndr-state
    =/  =channel      [[our sndr.packet] now +.ames-state -.peer-state]
    =/  =shut-packet  (decrypt symmetric-key.channel content.packet)
    ::  ward against replay attacks
    ::
    ?>  =(sndr-life.shut-packet her-life.channel)
    ?>  =(rcvr-life.shut-packet our-life.channel)
    ::
    ?:  ?=(%& -.meat.shut-packet)
      %+  on-hear-fragment
        %-  fall  :_  *message-still-state
        (~(get by rcv.peer-state) bone.shut-packet)
      [channel lane shut-packet]
    ::
    %+  on-hear-ack
      %-  fall  :_  *message-pump-state
      (~(get by snd.peer-state) bone.shut-packet)
    [channel lane shut-packet]
  ::
  ::
  ++  on-hear-ack
    |=  [=message-pump-state =channel =lane =shut-packet]
    ^+  event-core
    ::
    =/  pump  (make-message-pump message-pump-state channel)
    ::
    =/  task=message-pump-task
      ?>  ?=(%| -.meat.shut-packet)
      ?:  ?=(%& -.p.meat.shut-packet)
        [%hear-fragment-ack message-num.shut-packet p.p.meat.shut-packet]
      [%hear-message-ack message-num.shut-packet p.p.meat.shut-packet]
    ::
    =^  pump-gifts  message-pump-state  (work:pump task)
    ::
    =.  peers.ames-state
      %+  ~(jab by peers.ames-state)  her.channel
      |=  =ship-state
      ?>  ?=(%known -.ship-state)
      =/  =peer-state  +.ship-state
      =.  snd.peer-state
        (~(put by snd.peer-state) bone.shut-packet message-pump-state)
      [%known peer-state]
    ::
    (process-pump-gifts pump-gifts)
  ::
  ::
  ++  process-pump-gifts
    |=  pump-gifts=(list message-pump-gift)
    ^+  event-core
    ::
    !!
  ::
  ::
  ++  on-hear-fragment
    |=  [=message-still-state =channel =lane =shut-packet]
    ^+  event-core
    ::
    =/  still  (make-message-still message-still-state channel)
    ::
    =^  still-gifts  message-still-state  (work:still %hear lane shut-packet)
    ::
    =.  peers.ames-state
      %+  ~(jab by peers.ames-state)  her.channel
      |=  =ship-state
      ?>  ?=(%known -.ship-state)
      =/  =peer-state  +.ship-state
      =.  rcv.peer-state
        (~(put by rcv.peer-state) bone.shut-packet message-still-state)
      [%known peer-state]
    ::
    (process-still-gifts still-gifts)
  ::
  ::
  ++  process-still-gifts
    |=  still-gifts=(list message-still-gift)
    ^+  event-core
    ::
    !!
  ::
  ++  enqueue-alien-packet
    |=  [=lane =packet]
    ^+  event-core
    ::
    =/  sndr-state  (~(get by peers.ames-state) sndr.packet)
    ::
    =+  ^-  [already-pending=? todos=pending-requests]
        ?~  sndr-state
          [%.n *pending-requests]
        [%.y ?>(?=(%alien -.u.sndr-state) +.u.sndr-state)]
    ::
    =.  rcv-packets.todos  [[lane packet] rcv-packets.todos]
    ::
    =.  peers.ames-state
      (~(put by peers.ames-state) sndr.packet %alien todos)
    ::
    =?  event-core  !already-pending
      (emit duct %pass /alien %j %pubs sndr.packet)
    ::
    event-core
  --
::
::
++  make-message-pump
  |=  [=message-pump-state =channel]
  =|  gifts=(list message-pump-gift)
  ::
  |%
  ++  message-pump  .
  ++  give  |=(gift=message-pump-gift message-pump(gifts [gift gifts]))
  ++  packet-pump
    (make-packet-pump packet-pump-state.message-pump-state channel)
  ::  +work: handle a $message-pump-task
  ::
  ++  work
    |=  task=message-pump-task
    ^+  [gifts message-pump-state]
    ::
    =~  ?-  -.task
          %send              (on-send message.task)
          %hear-message-ack  (on-hear-message-ack [message-num ok lag]:task)
          *                  (run-packet-pump task)
        ==
        feed-packets
        (run-packet-pump %finalize ~)
        [(flop gifts) message-pump-state]
    ==
  ::
  ::
  ++  on-send
    |=  =message
    ^+  message-pump
    ::
    =.  unsent-messages.message-pump-state
      (~(put to unsent-messages.message-pump-state) message)
    ::
    message-pump
  ::
  ::
  ++  on-hear-message-ack
    |=  [=message-num ok=? lag=@dr]
    ^+  message-pump
    ::  ignore acks on already-processed messages
    ::
    ?:  (lth message-num current.message-pump-state)
      message-pump
    ::  ignore duplicate already-processed acks waiting for emission
    ::
    ?:  (~(has by queued-message-acks.message-pump-state) message-num)
      message-pump
    ::  clear and print .unsent-fragments if nonempty
    ::
    =?    unsent-fragments.message-pump-state
        ::
        ?&  =(current next):message-pump-state
            ?=(^ unsent-fragments.message-pump-state)
        ==
      ~&  %early-message-ack^ok^her.channel
      ~
    ::  clear all packets from this message from the packet pump
    ::
    =.  message-pump  (run-packet-pump %hear-message-ack message-num lag)
    ::
    =.  queued-message-acks.message-pump-state
      (~(put by queued-message-acks.message-pump-state) message-num ok)
    ::
    |-  ^+  message-pump
    ::
    =/  ack
      %-  ~(get by queued-message-acks.message-pump-state)
      current.message-pump-state
    ::
    ?~  ack
      message-pump
    ::
    =.  queued-message-acks.message-pump-state
      %-  ~(del by queued-message-acks.message-pump-state)
      current.message-pump-state
    ::
    =.  message-pump  (give %ack-message current.message-pump-state ok.u.ack)
    ::
    $(current.message-pump-state +(current.message-pump-state))
  ::
  ::
  ++  feed-packets
    ::  if nothing to send, no-op
    ::
    ?:  ?&  =(~ unsent-messages.message-pump-state)
            =(~ unsent-fragments.message-pump-state)
        ==
      ::
      message-pump
    ::  we have unsent fragments of the current message; feed them
    ::
    ?.  =(~ unsent-fragments.message-pump-state)
      =/  res  (send:packet-pump unsent-fragments.message-pump-state)
      =+  [unsent packet-pump-gifts state]=res
      ::
      =.  unsent-fragments.message-pump-state   unsent
      =.  packet-pump-state.message-pump-state  state
      ::
      =.  message-pump  (process-packet-pump-gifts packet-pump-gifts)
      ::  if it sent all of them, feed it more; otherwise, we're done
      ::
      ?~  unsent
        feed-packets
      message-pump
    ::  .unsent-messages is nonempty; pop a message off and feed it
    ::
    =^  message  unsent-messages.message-pump-state
      ~(get to unsent-messages.message-pump-state)
    ::
    =.  unsent-fragments.message-pump-state
      ::
      =/  chunks  (rip 13 (jam message))
      =/  num-fragments=fragment-num  (lent chunks)
      =|  counter=@
      ::
      |-  ^-  (list static-fragment)
      ?~  chunks  ~
      ::
      :-  [message-num=next.message-pump-state num-fragments counter i.chunks]
      ::
      $(chunks t.chunks, counter +(counter))
    ::
    =.  next.message-pump-state  +(next.message-pump-state)
    feed-packets
  ::
  ::
  ++  run-packet-pump
    |=  =packet-pump-task
    ^+  message-pump
    ::
    =^  packet-pump-gifts  packet-pump-state.message-pump-state
      (work:packet-pump packet-pump-task)
    ::
    (process-packet-pump-gifts packet-pump-gifts)
  ::
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
::
::
++  make-packet-pump
  |=  [=packet-pump-state =channel]
  =|  gifts=(list packet-pump-gift)
  |%
  ++  packet-pump  .
  ::  +packet-queue: all sent fragments, ordered by sequence number
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
  ++  gauge  (make-pump-gauge now.channel pump-metrics.packet-pump-state)
  ::  +work: handle $packet-pump-task request
  ::
  ++  work
    |=  task=packet-pump-task
    ^+  [gifts packet-pump-state]
    ::
    =-  [(flop gifts) packet-pump-state]
    ::
    ?-  -.task
      %hear-fragment-ack  !!
      %hear-message-ack   (on-hear-message-ack message-num.task)
      %wake               on-wake
      %finalize           on-finalize
    ==
  ::
  ::
  ++  main
    ^+  packet-pump
    !!
  ::  +send: try to send a list of packets, returning unsent and effects
  ::
  ++  send
    |=  fragments=(list static-fragment)
    ^+  [fragments gifts packet-pump-state]
    ::
    !!
  ::  +on-hear-fragment-ack: handle ack on a live packet
  ::
  ++  on-hear-fragment-ack
    |=  [=message-num =fragment-num]
    ^+  packet-pump
    ::
    =-  =.  pump-metrics.packet-pump-state  metrics.-
        =.  live.packet-pump-state     live.-
        main
    ::
    ^+  [metrics=pump-metrics live=live]:packet-pump-state
    ::
    %-  (traverse:packet-queue pump-metrics)
    :^    live.packet-pump-state
        start=~
      acc=pump-metrics.packet-pump-state
    |=  $:  =pump-metrics
            key=live-packet-key
            val=live-packet-val
        ==
    ^-  [new-val=(unit live-packet-val) stop=? ^pump-metrics]
    ::
    =/  gauge  (make-pump-gauge now.channel pump-metrics)
    ::  is this the acked packet?
    ::
    ?:  =(key [message-num fragment-num])
      ::  delete acked packet, update metrics, and stop traversal
      ::
      :+  new-val=~
        stop=%.y
      (on-ack:gauge [sent-date expiry retried]:val)
    ::  ack was out of order; mark expired, tell gauge, and continue
    ::
    :+  new-val=`val(expiry `@da`0)
      stop=%.n
    (on-skipped-packet:gauge [sent-date expiry retried]:val)
  ::  +on-hear-message-ack: apply ack to all packets from .message-num
  ::
  ++  on-hear-message-ack
    |=  =message-num
    ^+  packet-pump
    ::
    !!
  ::
  ::
  ++  on-finalize
    ^+  packet-pump
    ::
    !!
  ::
  ::
  ++  on-wake
    ^+  packet-pump
    ::
    !!
  --
::
::
++  make-pump-gauge
  |=  [now=@da =pump-metrics]
  |%
  ++  on-skipped-packet
    |=  [sent-date=@da expiry=@da retried=?]
    ::
    ::  TODO: decrease .max-live
    ::
    pump-metrics
  ::
  ++  on-ack
    |=  [sent-date=@da expiry=@da retried=?]
    ^+  pump-metrics
    ::
    ::  TODO: adjust .rtt and .max-live
    ::
    pump-metrics(num-live (dec num-live.pump-metrics))
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
