::  /sys/lull
::  %lull: arvo structures
!:
=>  ..part
|%
++  lull  %326
::                                                      ::  ::
::::                                                    ::  ::  (1) models
  ::                                                    ::  ::
::  #  %misc
::
::  miscellaneous systems types
::+|
::  +capped-queue: a +qeu with a maximum number of entries
::
++  capped-queue
  |$  [item-type]
  $:  queue=(qeu item-type)
      size=@ud
      max-size=_64
  ==
::  +clock: polymorphic cache type for use with the clock replacement algorithm
::
::     The +by-clock core wraps interface arms for manipulating a mapping from
::     :key-type to :val-type. Detailed docs for this type can be found there.
::
++  clock
  |$  ::  key-type: mold of keys
      ::  val-type: mold of values
      ::
      [key-type val-type]
  $:  lookup=(map key-type [val=val-type fresh=@ud])
      queue=(qeu key-type)
      size=@ud
      max-size=_2.048
      depth=_1
  ==
::
+$  deco  ?(~ %bl %br %un)                              ::  text decoration
+$  json                                                ::  normal json value
  $@  ~                                                 ::  null
  $%  [%a p=(list json)]                                ::  array
      [%b p=?]                                          ::  boolean
      [%o p=(map @t json)]                              ::  object
      [%n p=@ta]                                        ::  number
      [%s p=@t]                                         ::  string
  ==                                                    ::
+$  life  @ud                                           ::  ship key revision
+$  rift  @ud                                           ::  ship continuity
+$  mime  (pair mite octs)                              ::  mimetyped data
+$  octs  (pair @ud @)                                  ::  octet-stream
+$  sock  (pair ship ship)                              ::  outgoing [our his]
+$  stub  (list (pair stye (list @c)))                  ::  styled unicode
+$  stye  (pair (set deco) (pair tint tint))            ::  decos/bg/fg
+$  styl  %+  pair  (unit deco)                         ::  cascading style
          (pair (unit tint) (unit tint))                ::
+$  styx  (list $@(@t (pair styl styx)))                ::  styled text
+$  tint  $@  ?(%r %g %b %c %m %y %k %w %~)             ::  text color
          [r=@uxD g=@uxD b=@uxD]                        ::  24bit true color
+$  turf  (list @t)                                     ::  domain, tld first
::                                                      ::::
::::                    ++ethereum-types                  ::  eth surs for jael
  ::                                                    ::::
++  ethereum-types
  |%
  ::  ethereum address, 20 bytes.
  ::
  ++  address  @ux
  ::  event location
  ::
  +$  event-id  [block=@ud log=@ud]
  ::
  ++  events  (set event-id)
  --
::                                                      ::::
::::                    ++azimuth-types                   ::  az surs for jael
  ::                                                    ::::
++  azimuth-types
  =,  ethereum-types
  |%
  ++  point
    $:  ::  ownership
        ::
        $=  own
        $:  owner=address
            management-proxy=address
            voting-proxy=address
            transfer-proxy=address
        ==
      ::
        ::  networking
        ::
        $=  net
        %-  unit
        $:  =life
            =pass
            continuity-number=@ud
            sponsor=[has=? who=@p]
            escape=(unit @p)
        ==
      ::
        ::  spawning
        ::
        $=  kid
        %-  unit
        $:  spawn-proxy=address
            spawned=(set @p)  ::TODO  sparse range, pile, see old jael ++py
        ==
    ==
  ::
  +$  dnses  [pri=@t sec=@t ter=@t]
  ::
  ++  diff-azimuth
    $%  [%point who=@p dif=diff-point]
        [%dns dnses]
    ==
  ::
  ++  diff-point
    $%  [%full new=point]                           ::
        [%owner new=address]                        ::  OwnerChanged
        [%activated who=@p]                         ::  Activated
        [%spawned who=@p]                           ::  Spawned
        [%keys =life =pass]                         ::  ChangedKeys
        [%continuity new=@ud]                       ::  BrokeContinuity
        [%sponsor new=[has=? who=@p]]               ::  EscapeAcc/LostSpons
        [%escape new=(unit @p)]                     ::  EscapeReq/Can
        [%management-proxy new=address]             ::  ChangedManagementPro
        [%voting-proxy new=address]                 ::  ChangedVotingProxy
        [%spawn-proxy new=address]                  ::  ChangedSpawnProxy
        [%transfer-proxy new=address]               ::  ChangedTransferProxy
    ==
  --
::  +vane-task: general tasks shared across vanes
::
+$  vane-task
  $~  [%born ~]
  $%  ::  i/o device replaced (reset state)
      ::
      [%born ~]
      ::  boot completed (XX legacy)
      ::
      [%init ~]
      ::  trim state (in response to memory pressure)
      ::
      [%trim p=@ud]
      ::  kernel upgraded
      ::
      [%vega ~]
      ::  receive message via %ames
      ::
      ::    TODO: move .vane from $plea to here
      ::
      [%plea =ship =plea:ames]
  ==
::                                                      ::::
::::                    ++http                          ::
  ::                                                    ::::
::  http: shared representations of http concepts
::
++  http  ^?
  |%
  ::  +header-list: an ordered list of http headers
  ::
  +$  header-list
    (list [key=@t value=@t])
  ::  +method: exhaustive list of http verbs
  ::
  +$  method
    $?  %'CONNECT'
        %'DELETE'
        %'GET'
        %'HEAD'
        %'OPTIONS'
        %'POST'
        %'PUT'
        %'TRACE'
    ==
  ::  +request: a single http request
  ::
  +$  request
    $:  ::  method: http method
        ::
        method=method
        ::  url: the url requested
        ::
        ::    The url is not escaped. There is no escape.
        ::
        url=@t
        ::  header-list: headers to pass with this request
        ::
        =header-list
        ::  body: optionally, data to send with this request
        ::
        body=(unit octs)
    ==
  ::  +response-header: the status code and header list on an http request
  ::
  ::    We separate these away from the body data because we may not wait for
  ::    the entire body before we send a %progress to the caller.
  ::
  +$  response-header
    $:  ::  status: http status code
        ::
        status-code=@ud
        ::  headers: http headers
        ::
        headers=header-list
    ==
  ::  +http-event: packetized http
  ::
  ::    Urbit treats Earth's HTTP servers as pipes, where Urbit sends or
  ::    receives one or more %http-events. The first of these will always be a
  ::    %start or an %error, and the last will always be %cancel or will have
  ::    :complete set to %.y to finish the connection.
  ::
  ::    Calculation of control headers such as 'Content-Length' or
  ::    'Transfer-Encoding' should be performed at a higher level; this structure
  ::    is merely for what gets sent to or received from Earth.
  ::
  +$  http-event
    $%  ::  %start: the first packet in a response
        ::
        $:  %start
            ::  response-header: first event information
            ::
            =response-header
            ::  data: data to pass to the pipe
            ::
            data=(unit octs)
            ::  whether this completes the request
            ::
            complete=?
        ==
        ::  %continue: every subsequent packet
        ::
        $:  %continue
            ::  data: data to pass to the pipe
            ::
            data=(unit octs)
            ::  complete: whether this completes the request
            ::
            complete=?
        ==
        ::  %cancel: represents unsuccessful termination
        ::
        [%cancel ~]
    ==
  ::  +get-header: returns the value for :header, if it exists in :header-list
  ::
  ++  get-header
    |=  [header=@t =header-list]
    ^-  (unit @t)
    ::
    ?~  header-list
      ~
    ::
    ?:  =(key.i.header-list header)
      `value.i.header-list
    ::
    $(header-list t.header-list)
  ::  +set-header: sets the value of an item in the header list
  ::
  ::    This adds to the end if it doesn't exist.
  ::
  ++  set-header
    |=  [header=@t value=@t =header-list]
    ^-  ^header-list
    ::
    ?~  header-list
      ::  we didn't encounter the value, add it to the end
      ::
      [[header value] ~]
    ::
    ?:  =(key.i.header-list header)
      [[header value] t.header-list]
    ::
    [i.header-list $(header-list t.header-list)]
  ::  +delete-header: removes the first instance of a header from the list
  ::
  ++  delete-header
    |=  [header=@t =header-list]
    ^-  ^header-list
    ::
    ?~  header-list
      ~
    ::  if we see it in the list, remove it
    ::
    ?:  =(key.i.header-list header)
      t.header-list
    ::
    [i.header-list $(header-list t.header-list)]
  ::  +unpack-header: parse header field values
  ::
  ++  unpack-header
    |^  |=  value=@t
        ^-  (unit (list (map @t @t)))
        (rust (cass (trip value)) values)
    ::
    ++  values
      %+  more
        (ifix [. .]:(star ;~(pose ace (just '\09'))) com)
      pairs
    ::
    ++  pairs
      %+  cook
        ~(gas by *(map @t @t))
      %+  most  (ifix [. .]:(star ace) mic)
      ;~(plug token ;~(pose ;~(pfix tis value) (easy '')))
    ::
    ++  value
      ;~(pose token quoted-string)
    ::
    ++  token                                         ::  7230 token
      %+  cook  crip
      ::NOTE  this is ptok:de-purl:html, but can't access that here
      %-  plus
      ;~  pose
        aln  zap  hax  buc  cen  pam  soq  tar  lus
        hep  dot  ket  cab  tic  bar  sig
      ==
    ::
    ++  quoted-string                                 ::  7230 quoted string
      %+  cook  crip
      %+  ifix  [. .]:;~(less (jest '\\"') doq)
      %-  star
      ;~  pose
        ;~(pfix bas ;~(pose (just '\09') ace prn))
        ;~(pose (just '\09') ;~(less (mask "\22\5c\7f") (shim 0x20 0xff)))
      ==
    --
  ::  +simple-payload: a simple, one event response used for generators
  ::
  +$  simple-payload
    $:  ::  response-header: status code, etc
        ::
        =response-header
        ::  data: the data returned as the body
        ::
        data=(unit octs)
    ==
  --
::                                                      ::::
::::                    ++ames                            ::  (1a) network
  ::                                                    ::::
++  ames  ^?
  |%
  ::  $task: job for ames
  ::
  ::    Messaging Tasks
  ::
  ::    %hear: packet from unix
  ::    %heed: track peer's responsiveness; gives %clog if slow
  ::    %jilt: stop tracking peer's responsiveness
  ::    %cork: request to delete message flow
  ::    %plea: request to send message
  ::
  ::    System and Lifecycle Tasks
  ::
  ::    %born: process restart notification
  ::    %init: vane boot
  ::    %prod: re-send a packet per flow, to all peers if .ships is ~
  ::    %sift: limit verbosity to .ships
  ::    %snub: set packet blacklist to .ships
  ::    %spew: set verbosity toggles
  ::    %trim: release memory
  ::    %vega: kernel reload notification
  ::
  +$  task
    $%  [%hear =lane =blob]
        [%heed =ship]
        [%jilt =ship]
        [%cork =ship]
        $>(%plea vane-task)
    ::
        $>(%born vane-task)
        $>(%init vane-task)
        [%prod ships=(list ship)]
        [%sift ships=(list ship)]
        [%snub ships=(list ship)]
        [%spew veb=(list verb)]
        [%stir arg=@t]
        $>(%trim vane-task)
        $>(%vega vane-task)
    ==
  ::  $gift: effect from ames
  ::
  ::    Messaging Gifts
  ::
  ::    %boon: response message from remote ship
  ::    %clog: notify vane that %boon's to peer are backing up locally
  ::    %done: notify vane that peer (n)acked our message
  ::    %lost: notify vane that we crashed on %boon
  ::    %send: packet to unix
  ::
  ::    System and Lifecycle Gifts
  ::
  ::    %turf: domain report, relayed from jael
  ::
  +$  gift
    $%  [%boon payload=*]
        [%clog =ship]
        [%done error=(unit error)]
        [%lost ~]
        [%send =lane =blob]
    ::
        [%turf turfs=(list turf)]
    ==
  ::
  ::::                                                  ::  (1a2)
    ::
  ++  acru  $_  ^?                                      ::  asym cryptosuite
    |%                                                  ::  opaque object
    ++  as  ^?                                          ::  asym ops
      |%  ++  seal  |~([a=pass b=@] *@)                 ::  encrypt to a
          ++  sign  |~(a=@ *@)                          ::  certify as us
          ++  sure  |~(a=@ *(unit @))                   ::  authenticate from us
          ++  tear  |~([a=pass b=@] *(unit @))          ::  accept from a
      --  ::as                                          ::
    ++  de  |~([a=@ b=@] *(unit @))                     ::  symmetric de, soft
    ++  dy  |~([a=@ b=@] *@)                            ::  symmetric de, hard
    ++  en  |~([a=@ b=@] *@)                            ::  symmetric en
    ++  ex  ^?                                          ::  export
      |%  ++  fig  *@uvH                                ::  fingerprint
          ++  pac  *@uvG                                ::  default passcode
          ++  pub  *pass                                ::  public key
          ++  sec  *ring                                ::  private key
      --  ::ex                                          ::
    ++  nu  ^?                                          ::  reconstructors
      |%  ++  pit  |~([a=@ b=@] ^?(..nu))               ::  from [width seed]
          ++  nol  |~(a=ring ^?(..nu))                  ::  from ring
          ++  com  |~(a=pass ^?(..nu))                  ::  from pass
      --  ::nu                                          ::
    --  ::acru                                          ::
  ::  $address: opaque atomic transport address to or from unix
  ::
  +$  address  @uxaddress
  ::  $verb: verbosity flag for ames
  ::
  +$  verb  ?(%snd %rcv %odd %msg %ges %for %rot)
  ::  $blob: raw atom to or from unix, representing a packet
  ::
  +$  blob  @uxblob
  ::  $error: tagged diagnostic trace
  ::
  +$  error  [tag=@tas =tang]
  ::  $lane: ship transport address; either opaque $address or galaxy
  ::
  ::    The runtime knows how to look up galaxies, so we don't need to
  ::    know their transport addresses.
  ::
  +$  lane  (each @pC address)
  ::  $plea: application-level message, as a %pass
  ::
  ::    vane: destination vane on remote ship
  ::    path: internal route on the receiving ship
  ::    payload: semantic message contents
  ::
  +$  plea  [vane=@tas =path payload=*]
  ::
  :: +|  %atomics
  ::
  +$  bone           @udbone
  +$  fragment       @uwfragment
  +$  fragment-num   @udfragmentnum
  +$  message-blob   @udmessageblob
  +$  message-num    @udmessagenum
  +$  public-key     @uwpublickey
  +$  symmetric-key  @uwsymmetrickey
  ::
  :: +|  %kinetics
  ::  $ack: positive ack, nack packet, or nack trace
  ::
  +$  ack
    $%  [%ok ~]
        [%nack ~]
        [%naxplanation =error]
    ==
  ::
  :: +|  %statics
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
  ::    closing: bones closed on the sender side
  ::    corked:  bones closed on both sender and receiver
  ::    krocs:   bones that need to be sent again to the publisher
  ::
  +$  peer-state
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
        closing=(set bone)
        corked=(set bone)
        krocs=(set bone)
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
  --  ::ames
::                                                      ::::
::::                    ++behn                            ::  (1b) timekeeping
  ::                                                    ::::
++  behn  ^?
  |%
  +$  gift                                              ::  out result <-$
    $%  [%doze p=(unit @da)]                            ::  next alarm
        [%wake error=(unit tang)]                       ::  wakeup or failed
        [%meta p=vase]
        [%heck syn=sign-arvo]                           ::  response to %huck
    ==
  +$  task                                              ::  in request ->$
    $~  [%vega ~]                                       ::
    $%  $>(%born vane-task)                             ::  new unix process
        [%rest p=@da]                                   ::  cancel alarm
        [%drip p=vase]                                  ::  give in next event
        [%huck syn=sign-arvo]                           ::  give back
        $>(%trim vane-task)                             ::  trim state
        $>(%vega vane-task)                             ::  report upgrade
        [%wait p=@da]                                   ::  set alarm
        [%wake ~]                                       ::  timer activate
    ==
  --  ::behn
::                                                      ::::
::::                    ++clay                            ::  (1c) versioning
  ::                                                    ::::
++  clay  ^?
  |%
  +$  gift                                              ::  out result <-$
    $%  [%boon payload=*]                               ::  ames response
        [%croz rus=(map desk [r=regs w=regs])]          ::  rules for group
        [%cruz cez=(map @ta crew)]                      ::  permission groups
        [%dirk p=@tas]                                  ::  mark mount dirty
        [%ergo p=@tas q=mode]                           ::  version update
        [%hill p=(list @tas)]                           ::  mount points
        [%done error=(unit error:ames)]                 ::  ames message (n)ack
        [%mere p=(each (set path) (pair term tang))]    ::  merge result
        [%ogre p=@tas]                                  ::  delete mount point
        [%rule red=dict wit=dict]                       ::  node r+w permissions
        [%tire p=(each rock:tire wave:tire)]            ::  app state
        [%writ p=riot]                                  ::  response
        [%wris p=[%da p=@da] q=(set (pair care path))]  ::  many changes
    ==                                                  ::
  +$  task                                              ::  in request ->$
    $~  [%vega ~]                                       ::
    $%  [%boat ~]                                       ::  pier rebooted
        [%cred nom=@ta cew=crew]                        ::  set permission group
        [%crew ~]                                       ::  permission groups
        [%crow nom=@ta]                                 ::  group usage
        [%drop des=desk]                                ::  cancel pending merge
        [%info des=desk dit=nori]                       ::  internal edit
        $>(%init vane-task)                             ::  report install
        [%into des=desk all=? fis=mode]                 ::  external edit
        $:  %merg                                       ::  merge desks
            des=desk                                    ::  target
            her=@p  dem=desk  cas=case                  ::  source
            how=germ                                    ::  method
        ==                                              ::
        $:  %fuse                                       ::  merge many
            des=desk                                    ::  target desk
            bas=beak                                    ::  base desk
            con=(list [beak germ])                      ::  merges
        ==                                              ::
        [%mont pot=term bem=beam]                       ::  mount to unix
        [%dirk pot=term]                                ::  mark mount dirty
        [%ogre pot=$@(term beam)]                       ::  delete mount point
        [%park des=desk yok=yoki ran=rang]              ::  synchronous commit
        [%perm des=desk pax=path rit=rite]              ::  change permissions
        [%pork ~]                                       ::  resume commit
        [%rein des=desk ren=rein]                       ::  extra apps
        [%stir arg=*]                                   ::  debug
        [%tire p=(unit ~)]                              ::  app state subscribe
        [%tomb =clue]                                   ::  tombstone specific
        $>(%trim vane-task)                             ::  trim state
        $>(%vega vane-task)                             ::  report upgrade
        [%warp wer=ship rif=riff]                       ::  internal file req
        [%werp who=ship wer=ship rif=riff-any]          ::  external file req
        [%wick ~]                                       ::  try upgrade
        [%zest des=desk liv=zest]                       ::  live
        $>(%plea vane-task)                             ::  ames request
    ==                                                  ::
  ::                                                    ::
  ::::                                                  ::  (1c2)
    ::                                                  ::
  +$  aeon  @ud                                         ::  version number
  +$  beam  [[p=ship q=desk r=case] s=path]             ::  global name
  +$  beak  [p=ship q=desk r=case]                      ::  path prefix
  +$  cable                                             ::  lib/sur/mark ref
    $:  face=(unit term)                                ::
        file-path=term                                  ::
    ==                                                  ::
  +$  care                                              ::  clay submode
    ?(%a %b %c %d %e %f %p %r %s %t %u %v %w %x %y %z)  ::
  +$  case                                              ::  ship desk case spur
    $%  [%da p=@da]                                     ::  date
        [%tas p=@tas]                                   ::  label
        [%ud p=@ud]                                     ::  number
    ==                                                  ::
  +$  cash                                              ::  case or tako
    $%  [%tako p=tako]                                  ::
        case                                            ::
    ==                                                  ::
  +$  cass  [ud=@ud da=@da]                             ::  cases for revision
  +$  clue                                              ::  murder weapon
    $%  [%lobe =lobe]                                   ::  specific lobe
        [%all ~]                                        ::  all safe targets
        [%pick ~]                                       ::  collect garbage
        [%norm =ship =desk =norm]                       ::  set default norm
        [%worn =ship =desk =tako =norm]                 ::  set commit norm
        [%seek =ship =desk =cash]                       ::  fetch source blobs
    ==                                                  ::
  +$  cone  (map [ship desk] foam)                      ::  domes
  +$  foam                                              ::
    $:  dome                                            ::
        tom=(map tako norm)                             ::
        nor=norm                                        ::
        liv=zest                                        ::
        ren=(map dude:gall ?)                           ::
    ==                                                  ::
  +$  crew  (set ship)                                  ::  permissions group
  +$  dict  [src=path rul=real]                         ::  effective permission
  +$  dome                                              ::  project state
    $:  let=@ud                                         ::  top id
        hit=(map @ud tako)                              ::  changes by id
        lab=(map @tas @ud)                              ::  labels
    ==                                                  ::
  +$  germ                                              ::  merge style
    $?  %init                                           ::  new desk
        %fine                                           ::  fast forward
        %meet                                           ::  orthogonal files
        %mate                                           ::  orthogonal changes
        %meld                                           ::  force merge
        %only-this                                      ::  ours with parents
        %only-that                                      ::  hers with parents
        %take-this                                      ::  ours unless absent
        %take-that                                      ::  hers unless absent
        %meet-this                                      ::  ours if conflict
        %meet-that                                      ::  hers if conflict
    ==                                                  ::
  +$  lobe  @uvI                                        ::  blob ref
  +$  miso                                              ::  file delta
    $%  [%del ~]                                        ::  delete
        [%ins p=cage]                                   ::  insert
        [%dif p=cage]                                   ::  mutate from diff
        [%mut p=cage]                                   ::  mutate from raw
    ==                                                  ::
  +$  misu                                              ::  computed delta
    $%  [%del ~]                                        ::  delete
        [%ins p=cage]                                   ::  insert
        [%dif p=lobe q=cage]                            ::  mutate from diff
    ==                                                  ::
  +$  mizu  [p=@u q=(map @ud tako) r=rang]              ::  new state
  +$  moar  [p=@ud q=@ud]                               ::  normal change range
  +$  moat  [from=case to=case =path]                   ::  change range
  +$  mode  (list [path (unit mime)])                   ::  external files
  +$  mood  [=care =case =path]                         ::  request in desk
  +$  mool  [=case paths=(set (pair care path))]        ::  requests in desk
  +$  nori                                              ::  repository action
    $%  [%& p=soba]                                     ::  delta
        [%| p=@tas q=(unit aeon)]                       ::  label
    ==                                                  ::
  +$  nuri                                              ::  repository action
    $%  [%& p=suba]                                     ::  delta
        [%| p=@tas]                                     ::  label
    ==                                                  ::
  +$  norm  (axal ?)                                    ::  tombstone policy
  +$  open  $-(path vase)                               ::  get prelude
  +$  page  ^page                                       ::  export for compat
  +$  rang                                              ::  repository
    $:  hut=(map tako yaki)                             ::  changes
        lat=(map lobe page)                             ::  data
    ==                                                  ::
  +$  rant                                              ::  response to request
    $:  p=[p=care q=case r=desk]                        ::  clade release book
        q=path                                          ::  spur
        r=cage                                          ::  data
    ==                                                  ::
  +$  rave                                              ::  general request
    $%  [%sing =mood]                                   ::  single request
        [%next =mood]                                   ::  await next version
        [%mult =mool]                                   ::  next version of any
        [%many track=? =moat]                           ::  track range
    ==                                                  ::
  +$  real                                              ::  resolved permissions
    $:  mod=?(%black %white)                            ::
        who=(pair (set ship) (map @ta crew))            ::
    ==                                                  ::
  +$  regs  (map path rule)                             ::  rules for paths
  +$  rein  (map dude:gall ?)                           ::  extra apps
  +$  riff  [p=desk q=(unit rave)]                      ::  request+desist
  +$  riff-any                                          ::
    $%  [%1 =riff]                                      ::
    ==                                                  ::
  +$  rite                                              ::  new permissions
    $%  [%r red=(unit rule)]                            ::  for read
        [%w wit=(unit rule)]                            ::  for write
        [%rw red=(unit rule) wit=(unit rule)]           ::  for read and write
    ==                                                  ::
  +$  riot  (unit rant)                                 ::  response+complete
  +$  rule  [mod=?(%black %white) who=(set whom)]       ::  node permission
  +$  rump  [p=care q=case r=@tas s=path]               ::  relative path
  +$  saba  [p=ship q=@tas r=moar s=dome]               ::  patch+merge
  +$  soba  (list [p=path q=miso])                      ::  delta
  +$  suba  (list [p=path q=misu])                      ::  delta
  +$  tako  @uvI                                        ::  yaki ref
  +$  toro  [p=@ta q=nori]                              ::  general change
  ++  unce                                              ::  change part
    |*  a=mold                                          ::
    $%  [%& p=@ud]                                      ::  skip[copy]
        [%| p=(list a) q=(list a)]                      ::  p -> q[chunk]
    ==                                                  ::
  ++  urge  |*(a=mold (list (unce a)))                  ::  list change
  +$  waft                                              ::  kelvin range
    $^  [[%1 ~] p=(set weft)]                           ::
    weft                                                ::
  +$  whom  (each ship @ta)                             ::  ship or named crew
  +$  yoki  (each yuki yaki)                            ::  commit
  +$  yuki                                              ::  proto-commit
    $:  p=(list tako)                                   ::  parents
        q=(map path (each page lobe))                   ::  namespace
    ==                                                  ::
  +$  yaki                                              ::  commit
    $:  p=(list tako)                                   ::  parents
        q=(map path lobe)                               ::  namespace
        r=tako                                          ::  self-reference
        t=@da                                           ::  date
    ==                                                  ::
  +$  zest  $~(%dead ?(%dead %live %held))              ::  how live
  ::                                                    ::
  ++  tire                                              ::  app state
    |%                                                  ::
    +$  rock  (map desk [=zest wic=(set weft)])         ::
    +$  wave                                            ::
      $%  [%wait =desk =weft]                           ::  blocked
          [%warp =desk =weft]                           ::  unblocked
          [%zest =desk =zest]                           ::  running
      ==                                                ::
    ::
    ++  wash                                            ::  patch
      |=  [=rock =wave]
      ^+  rock
      ?-    -.wave
          %wait
        =/  got=[=zest wic=(set weft)]
          (~(gut by rock) desk.wave *zest ~)
        (~(put by rock) desk.wave got(wic (~(put in wic.got) weft.wave)))
      ::
          %warp
        %-  ~(run by rock)
        |=  [=zest wic=(set weft)]
        [zest (~(del in wic) weft.wave)]
      ::
          %zest
        ?:  ?=(%dead zest.wave)
          (~(del by rock) desk.wave)
        =/  got=[=zest wic=(set weft)]
          (~(gut by rock) desk.wave *zest ~)
        (~(put by rock) desk.wave got(zest zest.wave))
      ==
    ::
    ++  walk                                            ::  diff
      |=  [a=rock b=rock]
      ^-  (list wave)
      =/  adds  (~(dif by b) a)
      =/  dels  (~(dif by a) b)
      =/  bots  (~(int by a) b)
      ;:  welp
        ^-  (list wave)
        %-  zing
        %+  turn  ~(tap by adds)
        |=  [=desk =zest wic=(set weft)]
        ^-  (list wave)
        :-  [%zest desk zest]
        %+  turn  ~(tap in wic)
        |=  =weft
        [%wait desk weft]
      ::
        ^-  (list wave)
        %+  turn  ~(tap by dels)
        |=  [=desk =zest wic=(set weft)]
        ^-  wave
        [%zest desk %dead]
      ::
        ^-  (list wave)
        %-  zing
        %+  turn  ~(tap by bots)
        |=  [=desk * *]
        ^-  (list wave)
        =/  aa  (~(got by a) desk)
        =/  bb  (~(got by b) desk)
        =/  wadds  (~(dif in wic.bb) wic.aa)
        =/  wdels  (~(dif in wic.aa) wic.bb)
        ;:  welp
          ?:  =(zest.aa zest.bb)
            ~
          [%zest desk zest.bb]~
        ::
          %+  turn  ~(tap by wadds)
          |=  =weft
          ^-  wave
          [%wait desk weft]
        ::
          %+  turn  ~(tap by wdels)
          |=  =weft
          ^-  wave
          [%warp desk weft]
        ==
      ==
    --
  ::
  ::  +page-to-lobe: hash a page to get a lobe.
  ::
  ++  page-to-lobe  |=(page (shax (jam +<)))
  ::
  ++  cord-to-waft
    |=  =cord
    ^-  waft
    =/  wefts=(list weft)
      %+  turn  (rash cord (star (ifix [gay gay] tall:vast)))
      |=  =hoon
      !<(weft (slap !>(~) hoon))
    ?:  ?=([* ~] wefts)
      i.wefts
    [[%1 ~] (sy wefts)]
  ::
  ++  waft-to-wefts
    |=  kal=waft
    ^-  (set weft)
    ?^  -.kal
      p.kal
    [kal ~ ~]
  ::
  ::  +make-yaki: make commit out of a list of parents, content, and date.
  ::
  ++  make-yaki
    |=  [p=(list tako) q=(map path lobe) t=@da]
    ^-  yaki
    =+  ^=  has
        %^  cat  7  (sham [%yaki (roll p add) q t])
        (sham [%tako (roll p add) q t])
    [p q has t]
  ::  $pile: preprocessed hoon source file
  ::
  ::    /-  sur-file            ::  surface imports from /sur
  ::    /+  lib-file            ::  library imports from /lib
  ::    /=  face  /path         ::  imports built hoon file at path
  ::    /~  face  type   /path  ::  imports built hoon files from directory
  ::    /%  face  %mark         ::  imports mark definition from /mar
  ::    /$  face  %from  %to    ::  imports mark converter from /mar
  ::    /*  face  %mark  /path  ::  unbuilt file imports, as mark
  ::
  +$  pile
    $:  sur=(list taut)
        lib=(list taut)
        raw=(list [face=term =path])
        raz=(list [face=term =spec =path])
        maz=(list [face=term =mark])
        caz=(list [face=term =mars])
        bar=(list [face=term =mark =path])
        =hoon
    ==
  ::  $taut: file import from /lib or /sur
  ::
  +$  taut  [face=(unit term) pax=term]
  ::  $mars: mark conversion request
  ::  $tube: mark conversion gate
  ::  $nave: typed mark core
  ::
  +$  mars  [a=mark b=mark]
  +$  tube  $-(vase vase)
  ++  nave
    |$  [typ dif]
    $_
    ^?
    |%
    ++  diff  |~([old=typ new=typ] *dif)
    ++  form  *mark
    ++  join  |~([a=dif b=dif] *(unit (unit dif)))
    ++  mash
      |~  [a=[ship desk dif] b=[ship desk dif]]
      *(unit dif)
    ++  pact  |~([typ dif] *typ)
    ++  vale  |~(noun *typ)
    --
  ::  $dais: processed mark core
  ::
  +$  dais
    $_  ^|
    |_  sam=vase
    ++  diff  |~(new=_sam *vase)
    ++  form  *mark
    ++  join  |~([a=vase b=vase] *(unit (unit vase)))
    ++  mash
      |~  [a=[ship desk diff=vase] b=[ship desk diff=vase]]
      *(unit vase)
    ++  pact  |~(diff=vase sam)
    ++  vale  |~(noun sam)
    --
  ::
  ++  get-fit
    |=  [bek=beak pre=@tas pax=@tas]
    ^-  (unit path)
    =/  paz  (segments pax)
    |-  ^-  (unit path)
    ?~  paz
      ~
    =/  puz=path  (snoc `path`[pre i.paz] %hoon)
    =+  .^(=arch cy+[(scot %p p.bek) q.bek (scot r.bek) puz])
    ?^  fil.arch
      `puz
    $(paz t.paz)
  ::  +segments: compute all paths from :path-part, replacing some `/`s with `-`s
  ::
  ::    For example, when passed a :path-part of 'foo-bar-baz',
  ::    the product will contain:
  ::    ```
  ::    dojo> (segments 'foo-bar-baz')
  ::    ~[/foo-bar-baz /foo-bar/baz /foo/bar-baz /foo/bar/baz]
  ::    ```
  ::
  ++  segments
    |=  suffix=@tas
    ^-  (list path)
    =/  parser
      (most hep (cook crip ;~(plug ;~(pose low nud) (star ;~(pose low nud)))))
    =/  torn=(list @tas)  (fall (rush suffix parser) ~[suffix])
    %-  flop
    |-  ^-  (list (list @tas))
    ?<  ?=(~ torn)
    ?:  ?=([@ ~] torn)
      ~[torn]
    %-  zing
    %+  turn  $(torn t.torn)
    |=  s=(list @tas)
    ^-  (list (list @tas))
    ?>  ?=(^ s)
    ~[[i.torn s] [(crip "{(trip i.torn)}-{(trip i.s)}") t.s]]
  --  ::clay
::                                                      ::::
::::                    ++dill                            ::  (1d) console
  ::                                                    ::::
++  dill  ^?
  |%
  +$  gift                                              ::  out result <-$
    $%  [%blit p=(list blit)]                           ::  terminal output
        [%logo ~]                                       ::  logout
        [%meld ~]                                       ::  unify memory
        [%pack ~]                                       ::  compact memory
        [%trim p=@ud]                                   ::  trim kernel state
    ==                                                  ::
  +$  task                                              ::  in request ->$
    $~  [%vega ~]                                       ::
    $%  [%boot lit=? p=*]                               ::  weird %dill boot
        [%crop p=@ud]                                   ::  trim kernel state
        [%crud p=@tas q=(list tank)]                    ::  print error
        [%flog p=flog]                                  ::  wrapped error
        [%heft ~]                                       ::  memory report
        $>(%init vane-task)                             ::  after gall ready
        [%meld ~]                                       ::  unify memory
        [%pack ~]                                       ::  compact memory
        [%shot ses=@tas task=session-task]              ::  task for session
        [%talk p=(list tank)]                           ::  print tanks
        [%text p=tape]                                  ::  print tape
        $>(%trim vane-task)                             ::  trim state
        $>(%vega vane-task)                             ::  report upgrade
        [%verb ~]                                       ::  verbose mode
        [%knob tag=term level=?(%hush %soft %loud)]     ::  error verbosity
        session-task                                    ::  for default session
    ==                                                  ::
  ::                                                    ::
  +$  session-task                                      ::  session request
    $%  [%belt p=belt]                                  ::  terminal input
        [%blew p=blew]                                  ::  terminal config
        [%flee ~]                                       ::  unwatch session
        [%hail ~]                                       ::  terminal refresh
        [%open p=dude:gall q=(list gill:gall)]          ::  setup session
        [%shut ~]                                       ::  close session
        [%view ~]                                       ::  watch session blits
    ==                                                  ::
  ::
  ::::                                                  ::  (1d2)
    ::
  +$  blew  [p=@ud q=@ud]                               ::  columns rows
  +$  belt                                              ::  client input
    $?  bolt                                            ::  simple input
        [%mod mod=?(%ctl %met %hyp) key=bolt]           ::  w/ modifier
        [%txt p=(list @c)]                              ::  utf32 text
        ::TODO  consider moving %hey, %rez, %yow here   ::
    ==                                                  ::
  +$  bolt                                              ::  simple input
    $@  @c                                              ::  simple keystroke
    $%  [%aro p=?(%d %l %r %u)]                         ::  arrow key
        [%bac ~]                                        ::  true backspace
        [%del ~]                                        ::  true delete
        [%hit x=@ud y=@ud]                              ::  mouse click
        [%ret ~]                                        ::  return
    ==                                                  ::
  +$  blit                                              ::  client output
    $%  [%bel ~]                                        ::  make a noise
        [%clr ~]                                        ::  clear the screen
        [%hop p=$@(@ud [x=@ud y=@ud])]                  ::  set cursor col/pos
        [%klr p=stub]                                   ::  put styled
        [%mor p=(list blit)]                            ::  multiple blits
        [%nel ~]                                        ::  newline
        [%put p=(list @c)]                              ::  put text at cursor
        [%sag p=path q=*]                               ::  save to jamfile
        [%sav p=path q=@]                               ::  save to file
        [%url p=@t]                                     ::  activate url
        [%wyp ~]                                        ::  wipe cursor line
    ==                                                  ::
  +$  dill-belt                                         ::  arvo input
    $%  belt                                            ::  client input
        [%cru p=@tas q=(list tank)]                     ::  echo error
        [%hey ~]                                        ::  refresh
        [%rez p=@ud q=@ud]                              ::  resize, cols, rows
        [%yow p=gill:gall]                              ::  connect to app
    ==                                                  ::
  +$  dill-blit                                         ::  arvo output
    $%  blit                                            ::  client output
        [%qit ~]                                        ::  close console
    ==                                                  ::
  +$  flog                                              ::  sent to %dill
    $%  [%crop p=@ud]                                   ::  trim kernel state
        [%crud p=@tas q=(list tank)]                    ::
        [%heft ~]                                       ::
        [%meld ~]                                       ::  unify memory
        [%pack ~]                                       ::  compact memory
        [%text p=tape]                                  ::
        [%verb ~]                                       ::  verbose mode
    ==                                                  ::
  ::                                                    ::
  +$  poke                                              ::  dill to userspace
    $:  ses=@tas                                        ::  target session
        dill-belt                                       ::  input
    ==                                                  ::
  --  ::dill
::                                                      ::::
::::                    ++eyre                            ::  (1e) http-server
  ::                                                    ::::
++  eyre  ^?
  |%
  +$  gift
    $%  ::  set-config: configures the external http server
        ::
        ::    TODO: We need to actually return a (map (unit @t) http-config)
        ::    so we can apply configurations on a per-site basis
        ::
        [%set-config =http-config]
        ::  response: response to an event from earth
        ::
        [%response =http-event:http]
        ::  response to a %connect or %serve
        ::
        ::    :accepted is whether :binding was valid. Duplicate bindings are
        ::    not allowed.
        ::
        [%bound accepted=? =binding]
    ==
  ::
  +$  task
    $~  [%vega ~]
    $%  ::  initializes ourselves with an identity
        ::
        $>(%init vane-task)
        ::  new unix process
        ::
        $>(%born vane-task)
        ::  trim state (in response to memory pressure)
        ::
        $>(%trim vane-task)
        ::  report upgrade
        ::
        $>(%vega vane-task)
        ::  notifies us of the ports of our live http servers
        ::
        [%live insecure=@ud secure=(unit @ud)]
        ::  update http configuration
        ::
        [%rule =http-rule]
        ::  starts handling an inbound http request
        ::
        [%request secure=? =address =request:http]
        ::  starts handling an backdoor http request
        ::
        [%request-local secure=? =address =request:http]
        ::  cancels a previous request
        ::
        [%cancel-request ~]
        ::  connects a binding to an app
        ::
        [%connect =binding app=term]
        ::  connects a binding to a generator
        ::
        [%serve =binding =generator]
        ::  disconnects a binding
        ::
        ::    This must be called with the same duct that made the binding in
        ::    the first place.
        ::
        [%disconnect =binding]
        ::  notifies us that web login code changed
        ::
        [%code-changed ~]
        ::  start responding positively to cors requests from origin
        ::
        [%approve-origin =origin]
        ::  start responding negatively to cors requests from origin
        ::
        [%reject-origin =origin]
    ==
  ::  +origin: request origin as specified in an Origin header
  ::
  +$  origin  @torigin
  ::  +cors-registry: origins categorized by approval status
  ::
  +$  cors-registry
    $:  requests=(set origin)
        approved=(set origin)
        rejected=(set origin)
    ==
  ::  +outstanding-connection: open http connections not fully complete:
  ::
  ::    This refers to outstanding connections where the connection to
  ::    outside is opened and we are currently waiting on an app to
  ::    produce the results.
  ::
  +$  outstanding-connection
    $:  ::  action: the action that had matched
        ::
        =action
        ::  inbound-request: the original request which caused this connection
        ::
        =inbound-request
        ::  response-header: set when we get our first %start
        ::
        response-header=(unit response-header:http)
        ::  bytes-sent: the total bytes sent in response
        ::
        bytes-sent=@ud
    ==
  ::  +authentication-state: state used in the login system
  ::
  +$  authentication-state
    $:  ::  sessions: a mapping of session cookies to session information
        ::
        sessions=(map @uv session)
    ==
  ::  +session: server side data about a session
  ::
  +$  session
    $:  ::  expiry-time: when this session expires
        ::
        ::    We check this server side, too, so we aren't relying on the browser
        ::    to properly handle cookie expiration as a security mechanism.
        ::
        expiry-time=@da
        ::  channels: channels opened by this session
        ::
        channels=(set @t)
        ::
        ::  TODO: We should add a system for individual capabilities; we should
        ::  mint some sort of long lived cookie for mobile apps which only has
        ::  access to a single application path.
    ==
  ::  channel-state: state used in the channel system
  ::
  +$  channel-state
    $:  ::  session: mapping between an arbitrary key to a channel
        ::
        session=(map @t channel)
        ::  by-duct: mapping from ducts to session key
        ::
        duct-to-key=(map duct @t)
    ==
  ::  +timer: a reference to a timer so we can cancel or update it.
  ::
  +$  timer
    $:  ::  date: time when the timer will fire
        ::
        date=@da
        ::  duct: duct that set the timer so we can cancel
        ::
        =duct
    ==
  ::  channel-event: unacknowledged channel event, vaseless sign
  ::
  +$  channel-event
    $%  $>(%poke-ack sign:agent:gall)
        $>(%watch-ack sign:agent:gall)
        $>(%kick sign:agent:gall)
        [%fact =mark =noun]
    ==
  ::  channel: connection to the browser
  ::
  ::    Channels are the main method where a webpage communicates with Gall
  ::    apps. Subscriptions and pokes are issues with PUT requests on a path,
  ::    while GET requests on that same path open a persistent EventSource
  ::    channel.
  ::
  ::    The EventSource API is a sequence number based API that browser provide
  ::    which allow the server to push individual events to the browser over a
  ::    connection held open. In case of reconnection, the browser will send a
  ::    'Last-Event-Id: ' header to the server; the server then resends all
  ::    events since then.
  ::
  +$  channel
    $:  ::  channel-state: expiration time or the duct currently listening
        ::
        ::    For each channel, there is at most one open EventSource
        ::    connection. A 400 is issues on duplicate attempts to connect to the
        ::    same channel. When an EventSource isn't connected, we set a timer
        ::    to reap the subscriptions. This timer shouldn't be too short
        ::    because the
        ::
        state=(each timer duct)
        ::  next-id: next sequence number to use
        ::
        next-id=@ud
        ::  last-ack: time of last client ack
        ::
        ::    used for clog calculations, in combination with :unacked
        ::
        last-ack=@da
        ::  events: unacknowledged events
        ::
        ::    We keep track of all events where we haven't received a
        ::    'Last-Event-Id: ' response from the client or a per-poke {'ack':
        ::    ...} call. When there's an active EventSource connection on this
        ::    channel, we send the event but we still add it to events because we
        ::    can't assume it got received until we get an acknowledgment.
        ::
        events=(qeu [id=@ud request-id=@ud =channel-event])
        ::  unacked: unacknowledged event counts by request-id
        ::
        ::    used for clog calculations, in combination with :last-ack
        ::
        unacked=(map @ud @ud)
        ::  subscriptions: gall subscriptions by request-id
        ::
        ::    We maintain a list of subscriptions so if a channel times out, we
        ::    can cancel all the subscriptions we've made.
        ::
        subscriptions=(map @ud [ship=@p app=term =path duc=duct])
        ::  heartbeat: sse heartbeat timer
        ::
        heartbeat=(unit timer)
    ==
  ::  +binding: A rule to match a path.
  ::
  ::    A +binding is a system unique mapping for a path to match. A +binding
  ::    must be system unique because we don't want two handlers for a path;
  ::    what happens if there are two different actions for [~ /]?
  ::
  +$  binding
    $:  ::  site: the site to match.
        ::
        ::    A ~ will match the Urbit's identity site (your.urbit.org). Any
        ::    other value will match a domain literal.
        ::
        site=(unit @t)
        ::  path: matches this prefix path
        ::
        ::    /~myapp will match /~myapp or /~myapp/longer/path
        ::
        path=(list @t)
    ==
  ::  +action: the action to take when a binding matches an incoming request
  ::
  +$  action
    $%  ::  dispatch to a generator
        ::
        [%gen =generator]
        ::  dispatch to an application
        ::
        [%app app=term]
        ::  internal authentication page
        ::
        [%authentication ~]
        ::  internal logout page
        ::
        [%logout ~]
        ::  gall channel system
        ::
        [%channel ~]
        ::  gall scry endpoint
        ::
        [%scry ~]
        ::  respond with the default file not found page
        ::
        [%four-oh-four ~]
    ==
  ::  +generator: a generator on the local ship that handles requests
  ::
  ::    This refers to a generator on the local ship, run with a set of
  ::    arguments. Since http requests are time sensitive, we require that the
  ::    generator be on the current ship.
  ::
  +$  generator
    $:  ::  desk: desk on current ship that contains the generator
        ::
        =desk
        ::  path: path on :desk to the generator's hoon file
        ::
        path=(list @t)
        ::  args: arguments passed to the gate
        ::
        args=*
    ==
  :: +http-config: full http-server configuration
  ::
  +$  http-config
    $:  :: secure: PEM-encoded RSA private key and cert or cert chain
        ::
        secure=(unit [key=wain cert=wain])
        :: proxy: reverse TCP proxy HTTP(s)
        ::
        proxy=_|
        :: log: keep HTTP(s) access logs
        ::
        log=?
        :: redirect: send 301 redirects to upgrade HTTP to HTTPS
        ::
        ::   Note: requires certificate.
        ::
        redirect=?
    ==
  :: +http-rule: update configuration
  ::
  +$  http-rule
    $%  :: %cert: set or clear certificate and keypair
        ::
        [%cert cert=(unit [key=wain cert=wain])]
        :: %turf: add or remove established dns binding
        ::
        [%turf action=?(%put %del) =turf]
    ==
  ::  +address: client IP address
  ::
  +$  address
    $%  [%ipv4 @if]
        [%ipv6 @is]
        ::  [%ames @p]
    ==
  ::  +inbound-request: +http-request and metadata
  ::
  +$  inbound-request
    $:  ::  authenticated: has a valid session cookie
        ::
        authenticated=?
        ::  secure: whether this request was encrypted (https)
        ::
        secure=?
        ::  address: the source address of this request
        ::
        =address
        ::  request: the http-request itself
        ::
        =request:http
    ==
  ::
  +$  cred                                              ::  credential
    $:  hut=hart                                        ::  client host
        aut=(jug @tas @t)                               ::  client identities
        orx=oryx                                        ::  CSRF secret
        acl=(unit @t)                                   ::  accept-language
        cip=(each @if @is)                              ::  client IP
        cum=(map @tas *)                                ::  custom dirt
    ==                                                  ::
  +$  epic                                              ::  FCGI parameters
    $:  qix=(map @t @t)                                 ::  query
        ced=cred                                        ::  client credentials
        bem=beam                                        ::  original path
    ==                                                  ::
  ::
  +$  hart  [p=? q=(unit @ud) r=host]                   ::  http sec+port+host
  +$  hate  [p=purl q=@p r=moth]                        ::  semi-cooked request
  +$  hiss  [p=purl q=moth]                             ::  outbound request
  +$  host  (each turf @if)                             ::  http host
  +$  hoke  %+  each  [%localhost ~]                    ::  local host
            ?(%.0.0.0.0 %.127.0.0.1)                    ::
  +$  httq                                              ::  raw http request
    $:  p=meth                                          ::  method
        q=@t                                            ::  unparsed url
        r=(list [p=@t q=@t])                            ::  headers
        s=(unit octs)                                   ::  body
    ==                                                  ::
  +$  httr  [p=@ud q=mess r=(unit octs)]                ::  raw http response
  +$  math  (map @t (list @t))                          ::  semiparsed headers
  +$  mess  (list [p=@t q=@t])                          ::  raw http headers
  +$  meth                                              ::  http methods
    $?  %conn                                           ::  CONNECT
        %delt                                           ::  DELETE
        %get                                            ::  GET
        %head                                           ::  HEAD
        %opts                                           ::  OPTIONS
        %post                                           ::  POST
        %put                                            ::  PUT
        %trac                                           ::  TRACE
    ==                                                  ::
  +$  moth  [p=meth q=math r=(unit octs)]               ::  http operation
  +$  oryx  @t                                          ::  CSRF secret
  +$  pork  [p=(unit @ta) q=(list @t)]                  ::  fully parsed url
  :: +prox: proxy notification
  ::
  ::   Used on both the proxy (ward) and upstream sides for
  ::   sending/receiving proxied-request notifications.
  ::
  +$  prox
    $:  :: por: tcp port
        ::
        por=@ud
        :: sek: secure?
        ::
        sek=?
        :: non: authentication nonce
        ::
        non=@uvJ
    ==
  +$  purf  (pair purl (unit @t))                       ::  url with fragment
  +$  purl  [p=hart q=pork r=quay]                      ::  parsed url
  +$  quay  (list [p=@t q=@t])                          ::  parsed url query
  ++  quer  |-($@(~ [p=@t q=@t t=$]))                   ::  query tree
  +$  quri                                              ::  request-uri
    $%  [%& p=purl]                                     ::  absolute
        [%| p=pork q=quay]                              ::  relative
    ==                                                  ::
  ::  +reserved: check if an ipv4 address is in a reserved range
  ::
  ++  reserved
    |=  a=@if
    ^-  ?
    =/  b  (flop (rip 3 a))
    ::  0.0.0.0/8 (software)
    ::
    ?.  ?=([@ @ @ @ ~] b)  &
    ?|  ::  10.0.0.0/8 (private)
        ::
        =(10 i.b)
        ::  100.64.0.0/10 (carrier-grade NAT)
        ::
        &(=(100 i.b) (gte i.t.b 64) (lte i.t.b 127))
        ::  127.0.0.0/8 (localhost)
        ::
        =(127 i.b)
        ::  169.254.0.0/16 (link-local)
        ::
        &(=(169 i.b) =(254 i.t.b))
        ::  172.16.0.0/12 (private)
        ::
        &(=(172 i.b) (gte i.t.b 16) (lte i.t.b 31))
        ::  192.0.0.0/24 (protocol assignment)
        ::
        &(=(192 i.b) =(0 i.t.b) =(0 i.t.t.b))
        ::  192.0.2.0/24 (documentation)
        ::
        &(=(192 i.b) =(0 i.t.b) =(2 i.t.t.b))
        ::  192.18.0.0/15 (reserved, benchmark)
        ::
        &(=(192 i.b) |(=(18 i.t.b) =(19 i.t.b)))
        ::  192.51.100.0/24 (documentation)
        ::
        &(=(192 i.b) =(51 i.t.b) =(100 i.t.t.b))
        ::  192.88.99.0/24 (reserved, ex-anycast)
        ::
        &(=(192 i.b) =(88 i.t.b) =(99 i.t.t.b))
        ::  192.168.0.0/16 (private)
        ::
        &(=(192 i.b) =(168 i.t.b))
        ::  203.0.113/24 (documentation)
        ::
        &(=(203 i.b) =(0 i.t.b) =(113 i.t.t.b))
        ::  224.0.0.0/8 (multicast)
        ::  240.0.0.0/4 (reserved, future)
        ::  255.255.255.255/32 (broadcast)
        ::
        (gte i.b 224)
    ==
  ::  +ipa: parse ip address
  ::
  ++  ipa
    ;~(pose (stag %ipv4 ip4) (stag %ipv6 ip6))
  ::  +ip4: parse ipv4 address
  ::
  ++  ip4
    =+  byt=(ape:ag ted:ab)
    (bass 256 ;~(plug byt (stun [3 3] ;~(pfix dot byt))))
  ::  +ip6: parse ipv6 address
  ::
  ++  ip6
    %+  bass  0x1.0000
    %+  sear
      |=  hexts=(list $@(@ [~ %zeros]))
      ^-  (unit (list @))
      ::  not every list of hextets is an ipv6 address
      ::
      =/  legit=?
        =+  l=(lent hexts)
        =+  c=|=(a=* ?=([~ %zeros] a))
        ?|  &((lth l 8) ?=([* ~] (skim hexts c)))
            &(=(8 l) !(lien hexts c))
        ==
      ?.  legit  ~
      %-  some
      ::  expand zeros
      ::
      %-  zing
      %+  turn  hexts
      |=  hext=$@(@ [~ %zeros])
      ?@  hext  [hext]~
      (reap (sub 9 (lent hexts)) 0)
    ::  parse hextets, producing cell for shorthand zeroes
    ::
    |^  %+  cook
          |=  [a=(list @) b=(list [~ %zeros]) c=(list @)]
          :(welp a b c)
        ;~  plug
          (more col het)
          (stun [0 1] cel)
          (more col het)
        ==
    ++  cel  (cold `%zeros ;~(plug col col))
    ++  het  (bass 16 (stun [1 4] six:ab))
    --
  ::
  +$  rout  [p=(list host) q=path r=oryx s=path]        ::  http route (new)
  +$  user  knot                                        ::  username
  --  ::eyre
::                                                      ::::
::::                    ++gall                            ::  (1g) extensions
  ::                                                    ::::
++  gall  ^?
  |%
  +$  gift                                              ::  outgoing result
    $%  [%boon payload=*]                               ::  ames response
        [%done error=(unit error:ames)]                 ::  ames message (n)ack
        [%unto p=unto]                                  ::
    ==                                                  ::
  +$  task                                              ::  incoming request
    $~  [%vega ~]                                       ::
    $%  [%deal p=sock q=term r=deal]                    ::  full transmission
        [%sear =ship]                                   ::  clear pending queues
        [%jolt =desk =dude]                             ::  (re)start agent
        [%idle =dude]                                   ::  suspend agent
        [%load =load]                                   ::  load agent
        [%nuke =dude]                                   ::  delete agent
        [%doff dude=(unit dude) ship=(unit ship)]       ::  kill subscriptions
        [%rake dude=(unit dude) all=?]                  ::  reclaim old subs
        $>(%init vane-task)                             ::  set owner
        $>(%trim vane-task)                             ::  trim state
        $>(%vega vane-task)                             ::  report upgrade
        $>(%plea vane-task)                             ::  network request
        [%spew veb=(list verb)]                         ::  set verbosity
        [%sift dudes=(list dude)]                       ::  per agent
    ==                                                  ::
  +$  bitt  (map duct (pair ship path))                 ::  incoming subs
  +$  boat  (map [=wire =ship =term] [acked=? =path])   ::  outgoing subs
  +$  boar  (map [=wire =ship =term] nonce=@)           ::  and their nonces
  +$  bowl                                              ::  standard app state
          $:  $:  our=ship                              ::  host
                  src=ship                              ::  guest
                  dap=term                              ::  agent
              ==                                        ::
              $:  wex=boat                              ::  outgoing subs
                  sup=bitt                              ::  incoming subs
              ==                                        ::
              $:  act=@ud                               ::  change number
                  eny=@uvJ                              ::  entropy
                  now=@da                               ::  current time
                  byk=beak                              ::  load source
          ==  ==                                        ::
  +$  dude  term                                        ::  server identity
  +$  gill  (pair ship term)                            ::  general contact
  +$  load  (list [=dude =beak =agent])                 ::  loadout
  +$  scar                                              ::  opaque duct
    $:  p=@ud                                           ::  bone sequence
        q=(map duct bone)                               ::  by duct
        r=(map bone duct)                               ::  by bone
    ==                                                  ::
  +$  suss  (trel dude @tas @da)                        ::  config report
  +$  well  (pair desk term)                            ::
  +$  neat
    $%  [%arvo =note-arvo]
        [%agent [=ship name=term] =deal]
        [%pyre =tang]
    ==
  +$  deal
    $%  [%raw-poke =mark =noun]
        task:agent
    ==
  +$  unto
    $%  [%raw-fact =mark =noun]
        sign:agent
    ==
  ::  TODO: add more flags?
  ::
  +$  verb  ?(%odd)
  ::
  ::  +agent: app core
  ::
  ++  agent
    =<  form
    |%
    +$  step  (quip card form)
    +$  card  (wind note gift)
    +$  note
      $%  [%agent [=ship name=term] =task]
          [%arvo note-arvo]
          [%pyre =tang]
      ==
    +$  task
      $%  [%watch =path]
          [%watch-as =mark =path]
          [%leave ~]
          [%poke =cage]
          [%poke-as =mark =cage]
      ==
    +$  gift
      $%  [%fact paths=(list path) =cage]
          [%kick paths=(list path) ship=(unit ship)]
          [%watch-ack p=(unit tang)]
          [%poke-ack p=(unit tang)]
      ==
    +$  sign
      $%  [%poke-ack p=(unit tang)]
          [%watch-ack p=(unit tang)]
          [%fact =cage]
          [%kick ~]
      ==
    ++  form
      $_  ^|
      |_  bowl
      ++  on-init
        *(quip card _^|(..on-init))
      ::
      ++  on-save
        *vase
      ::
      ++  on-load
        |~  old-state=vase
        *(quip card _^|(..on-init))
      ::
      ++  on-poke
        |~  [mark vase]
        *(quip card _^|(..on-init))
      ::
      ++  on-watch
        |~  path
        *(quip card _^|(..on-init))
      ::
      ++  on-leave
        |~  path
        *(quip card _^|(..on-init))
      ::
      ++  on-peek
        |~  path
        *(unit (unit cage))
      ::
      ++  on-agent
        |~  [wire sign]
        *(quip card _^|(..on-init))
      ::
      ++  on-arvo
        |~  [wire sign-arvo]
        *(quip card _^|(..on-init))
      ::
      ++  on-fail
        |~  [term tang]
        *(quip card _^|(..on-init))
      --
    --
  --  ::gall
::  %iris http-client interface
::
++  iris  ^?
  |%
  ::  +gift: effects the client can emit
  ::
  +$  gift
    $%  ::  %request: outbound http-request to earth
        ::
        ::    TODO: id is sort of wrong for this interface; the duct should
        ::    be enough to identify which request we're talking about?
        ::
        [%request id=@ud request=request:http]
        ::  %cancel-request: tell earth to cancel a previous %request
        ::
        [%cancel-request id=@ud]
        ::  %response: response to the caller
        ::
        [%http-response =client-response]
    ==
  ::
  +$  task
    $~  [%vega ~]
    $%  ::  system started up; reset open connections
        ::
        $>(%born vane-task)
        ::  trim state (in response to memory pressure)
        ::
        $>(%trim vane-task)
        ::  report upgrade
        ::
        $>(%vega vane-task)
        ::  fetches a remote resource
        ::
        [%request =request:http =outbound-config]
        ::  cancels a previous fetch
        ::
        [%cancel-request ~]
        ::  receives http data from outside
        ::
        [%receive id=@ud =http-event:http]
    ==
  ::  +client-response: one or more client responses given to the caller
  ::
  +$  client-response
    $%  ::  periodically sent as an update on the duct that sent %fetch
        ::
        $:  %progress
            ::  http-response-header: full transaction header
            ::
            ::    In case of a redirect chain, this is the target of the
            ::    final redirect.
            ::
            =response-header:http
            ::  bytes-read: bytes fetched so far
            ::
            bytes-read=@ud
            ::  expected-size: the total size if response had a content-length
            ::
            expected-size=(unit @ud)
            ::  incremental: data received since the last %http-progress
            ::
            incremental=(unit octs)
        ==
        ::  final response of a download, parsed as mime-data if successful
        ::
        [%finished =response-header:http full-file=(unit mime-data)]
        ::  canceled by the runtime system
        ::
        [%cancel ~]
    ==
  ::  mime-data: externally received but unvalidated mimed data
  ::
  +$  mime-data
    [type=@t data=octs]
  ::  +outbound-config: configuration for outbound http requests
  ::
  +$  outbound-config
    $:  ::  number of times to follow a 300 redirect before erroring
        ::
        ::    Common values for this will be 3 (the limit most browsers use), 5
        ::    (the limit recommended by the http standard), or 0 (let the
        ::    requester deal with 300 redirects).
        ::
        redirects=_5
        ::  number of times to retry before failing
        ::
        ::    When we retry, we'll automatically try to use the 'Range' header
        ::    to resume the download where we left off if we have the
        ::    'Accept-Range: bytes' in the original response.
        ::
        retries=_3
    ==
  ::  +to-httr: adapts to old eyre interface
  ::
  ++  to-httr
    |=  [header=response-header:http full-file=(unit mime-data)]
    ^-  httr:eyre
    ::
    =/  data=(unit octs)
      ?~(full-file ~ `data.u.full-file)
    ::
    [status-code.header headers.header data]
  --
::                                                      ::::
::::                    ++jael                          ::  (1h) security
  ::                                                    ::::
++  jael  ^?
  |%
  +$  public-keys-result
    $%  [%full points=(map ship point)]
        [%diff who=ship =diff:point]
        [%breach who=ship]
    ==
  ::                                                  ::
  +$  gift                                            ::  out result <-$
    $%  [%done error=(unit error:ames)]               ::  ames message (n)ack
        [%boon payload=*]                             ::  ames response
        [%private-keys =life vein=(map life ring)]    ::  private keys
        [%public-keys =public-keys-result]            ::  ethereum changes
        [%turf turf=(list turf)]                      ::  domains
    ==                                                ::
  ::  +feed: potential boot parameters
  ::
  +$  feed
    $^  [[%1 ~] who=ship kyz=(list [lyf=life key=ring])]
    seed
  ::  +seed: individual boot parameters
  ::
  +$  seed  [who=ship lyf=life key=ring sig=(unit oath:pki)]
  ::
  +$  task                                            ::  in request ->$
    $~  [%vega ~]                                     ::
    $%  [%dawn dawn-event]                            ::  boot from keys
        [%fake =ship]                                 ::  fake boot
        [%listen whos=(set ship) =source]             ::  set ethereum source
        ::TODO  %next for generating/putting new private key
        [%meet =ship =life =pass]                     ::  met after breach
        [%moon =ship =udiff:point]                    ::  register moon keys
        [%nuke whos=(set ship)]                       ::  cancel tracker from
        [%private-keys ~]                             ::  sub to privates
        [%public-keys ships=(set ship)]               ::  sub to publics
        [%rekey =life =ring]                          ::  update private keys
        [%resend ~]                                   ::  resend private key
        [%ruin ships=(set ship)]                      ::  pretend breach
        $>(%trim vane-task)                           ::  trim state
        [%turf ~]                                     ::  view domains
        $>(%vega vane-task)                           ::  report upgrade
        $>(%plea vane-task)                           ::  ames request
        [%step ~]                                     ::  reset web login code
    ==                                                ::
  ::
  +$  dawn-event
    $:  =seed
        spon=(list [=ship point:azimuth-types])
        czar=(map ship [=rift =life =pass])
        turf=(list turf)
        bloq=@ud
        node=(unit purl:eyre)
    ==
  ::
  ++  block
    =<  block
    |%
    +$  hash    @uxblockhash
    +$  number  @udblocknumber
    +$  id      [=hash =number]
    +$  block   [=id =parent=hash]
    --
  ::
  ::  Azimuth points form a groupoid, where the objects are all the
  ::  possible values of +point and the arrows are the possible values
  ::  of (list point-diff).  Composition of arrows is concatenation,
  ::  and you can apply the diffs to a +point with +apply.
  ::
  ::  It's simplest to consider +point as the coproduct of three
  ::  groupoids, Rift, Keys, and Sponsor.  Recall that the coproduct
  ::  of monoids is the free monoid (Kleene star) of the coproduct of
  ::  the underlying sets of the monoids.  The construction for
  ::  groupoids is similar.  Thus, the objects of the coproduct are
  ::  the product of the objects of the underlying groupoids.  The
  ::  arrows are a list of a sum of the diff types of the underlying
  ::  groupoids.  Given an arrow=(list diff), you can project to the
  ::  underlying arrows with +skim filtering on the head of each diff.
  ::
  ::  The identity element is ~.  Clearly, composing this with any
  ::  +diff gives the original +diff.  Since this is a category,
  ::  +compose must be associative (true, because concatenation is
  ::  associative).  This is a groupoid, so we must further have that
  ::  every +point-diff has an inverse.  These are given by the
  ::  +inverse operation.
  ::
  ++  point
    =<  point
    |%
    +$  point
      $:  =rift
          =life
          keys=(map life [crypto-suite=@ud =pass])
          sponsor=(unit @p)
      ==
    ::
    +$  key-update  [=life crypto-suite=@ud =pass]
    ::
    ::  Invertible diffs
    ::
    +$  diffs  (list diff)
    +$  diff
      $%  [%rift from=rift to=rift]
          [%keys from=key-update to=key-update]
          [%spon from=(unit @p) to=(unit @p)]
      ==
    ::
    ::  Non-invertible diffs
    ::
    +$  udiffs  (list [=ship =udiff])
    +$  udiff
      $:  =id:block
      $%  [%rift =rift boot=?]
          [%keys key-update boot=?]
          [%spon sponsor=(unit @p)]
          [%disavow ~]
      ==  ==
    ::
    ++  udiff-to-diff
      |=  [=a=udiff =a=point]
      ^-  (unit diff)
      ?-    +<.a-udiff
          %disavow  ~|(%udiff-to-diff-disavow !!)
          %spon     `[%spon sponsor.a-point sponsor.a-udiff]
          %rift
        ?.  (gth rift.a-udiff rift.a-point)
          ~
        ~?  &(!=(rift.a-udiff +(rift.a-point)) !boot.a-udiff)
          [%udiff-to-diff-skipped-rift a-udiff a-point]
        `[%rift rift.a-point rift.a-udiff]
      ::
          %keys
        ?.  (gth life.a-udiff life.a-point)
          ~
        ~?  &(!=(life.a-udiff +(life.a-point)) !boot.a-udiff)
          [%udiff-to-diff-skipped-life a-udiff a-point]
        :^  ~  %keys
          [life.a-point (~(gut by keys.a-point) life.a-point *[@ud pass])]
        [life crypto-suite pass]:a-udiff
      ==
    ::
    ++  inverse
      |=  diffs=(list diff)
      ^-  (list diff)
      %-  flop
      %+  turn  diffs
      |=  =diff
      ^-  ^diff
      ?-  -.diff
        %rift  [%rift to from]:diff
        %keys  [%keys to from]:diff
        %spon  [%spon to from]:diff
      ==
    ::
    ++  compose
      (bake weld ,[(list diff) (list diff)])
    ::
    ++  apply
      |=  [diffs=(list diff) =a=point]
      (roll diffs (apply-diff a-point))
    ::
    ++  apply-diff
      |=  a=point
      |:  [*=diff a-point=a]
      ^-  point
      ?-    -.diff
          %rift
        ?>  =(rift.a-point from.diff)
        a-point(rift to.diff)
      ::
          %keys
        ?>  =(life.a-point life.from.diff)
        ?>  =((~(get by keys.a-point) life.a-point) `+.from.diff)
        %_  a-point
          life  life.to.diff
          keys  (~(put by keys.a-point) life.to.diff +.to.diff)
        ==
      ::
          %spon
        ?>  =(sponsor.a-point from.diff)
        a-point(sponsor to.diff)
      ==
    --
  ::                                                    ::
  ::::                                                  ::
    ::                                                  ::
  +$  source  (each ship term)
  +$  source-id  @udsourceid
  ::
  ::  +state-eth-node: state of a connection to an ethereum node
  ::
  +$  state-eth-node                                    ::  node config + meta
    $:  top-source-id=source-id
        sources=(map source-id source)
        sources-reverse=(map source source-id)
        default-source=source-id
        ship-sources=(map ship source-id)
        ship-sources-reverse=(jug source-id ship)
    ==                                                  ::
  ::                                                    ::
  ::::                  ++pki:jael                      ::  (1h2) certificates
    ::                                                  ::::
  ++  pki  ^?
    |%
    ::TODO  update to fit azimuth-style keys
    ::  the urbit meta-certificate (++will) is a sequence
    ::  of certificates (++cert).  each cert in a will
    ::  revokes and replaces the previous cert.  the
    ::  version number of a ship is a ++life.
    ::
    ::  the deed contains an ++arms, a definition
    ::  of cosmetic identity; a semi-trusted parent,
    ::  which signs the initial certificate and provides
    ::  routing services; and a dirty bit.  if the dirty
    ::  bit is set, the new life of this ship may have
    ::  lost information that the old life had.
    ::
    +$  hand  @uvH                                      ::  128-bit hash
    +$  mind  [who=ship lyf=life]                       ::  key identifier
    +$  name  (pair @ta @t)                             ::  ascii / unicode
    +$  oath  @                                         ::  signature
    --  ::  pki
  --  ::  jael
::                                                      ::::
::::                    ++khan                            ::  (1i) threads
  ::                                                    ::::
++  khan  ^?
  |%
  +$  gift                                              ::  out result <-$
    $%  [%arow p=(avow cage)]                           ::  in-arvo result
        [%avow p=(avow page)]                           ::  external result
    ==                                                  ::
  +$  task                                              ::  in request ->$
    $~  [%vega ~]                                       ::
    $%  $>(%born vane-task)                             ::  new unix process
        [%done ~]                                       ::  socket closed
        ::  TODO  mark ignored                          ::
        ::                                              ::
        [%fard p=(fyrd cage)]                           ::  in-arvo thread
        [%fyrd p=(fyrd cast)]                           ::  external thread
        [%lard =bear =shed]                             ::  inline thread
        $>(%trim vane-task)                             ::  trim state
        $>(%vega vane-task)                             ::  report upgrade
    ==                                                  ::
  ::                                                    ::
  ++  avow  |$  [a]  (each a goof)                      ::  $fyrd result
  +$  bear  $@(desk beak)                               ::  partial $beak
  +$  cast  (pair mark page)                            ::  output mark + input
  ++  fyrd  |$  [a]  [=bear name=term args=a]           ::  thread run request
  ::                                                    ::
  +$  shed  _*form:(strand:rand ,vase)                  ::  compute vase
  --  ::khan
::
++  rand                                                ::  computation
  |%
  +$  card  card:agent:gall
  +$  input
    $%  [%poke =cage]
        [%sign =wire =sign-arvo]
        [%agent =wire =sign:agent:gall]
        [%watch =path]
    ==
  +$  strand-input  [=bowl in=(unit input)]
  +$  tid   @tatid
  +$  bowl
    $:  our=ship
        src=ship
        tid=tid
        mom=(unit tid)
        wex=boat:gall
        sup=bitt:gall
        eny=@uvJ
        now=@da
        byk=beak
    ==
  ::
  ::  cards:  cards to send immediately.  These will go out even if a
  ::          later stage of the computation fails, so they shouldn't have
  ::          any semantic effect on the rest of the system.
  ::          Alternately, they may record an entry in contracts with
  ::          enough information to undo the effect if the computation
  ::          fails.
  ::  wait:   don't move on, stay here.  The next sign should come back
  ::          to this same callback.
  ::  skip:   didn't expect this input; drop it down to be handled
  ::          elsewhere
  ::  cont:   continue computation with new callback.
  ::  fail:   abort computation; don't send effects
  ::  done:   finish computation; send effects
  ::
  ++  strand-output-raw
    |*  a=mold
    $~  [~ %done *a]
    $:  cards=(list card)
        $=  next
        $%  [%wait ~]
            [%skip ~]
            [%cont self=(strand-form-raw a)]
            [%fail err=(pair term tang)]
            [%done value=a]
        ==
    ==
  ::
  ++  strand-form-raw
    |*  a=mold
    $-(strand-input (strand-output-raw a))
  ::
  ::  Abort strand computation with error message
  ::
  ++  strand-fail
    |=  err=(pair term tang)
    |=  strand-input
    [~ %fail err]
  ::
  ::  Asynchronous transcaction monad.
  ::
  ::  Combo of four monads:
  ::  - Reader on input
  ::  - Writer on card
  ::  - Continuation
  ::  - Exception
  ::
  ++  strand
    |*  a=mold
    |%
    ++  output  (strand-output-raw a)
    ::
    ::  Type of an strand computation.
    ::
    ++  form  (strand-form-raw a)
    ::
    ::  Monadic pure.  Identity computation for bind.
    ::
    ++  pure
      |=  arg=a
      ^-  form
      |=  strand-input
      [~ %done arg]
    ::
    ::  Monadic bind.  Combines two computations, associatively.
    ::
    ++  bind
      |*  b=mold
      |=  [m-b=(strand-form-raw b) fun=$-(b form)]
      ^-  form
      |=  input=strand-input
      =/  b-res=(strand-output-raw b)
        (m-b input)
      ^-  output
      :-  cards.b-res
      ?-    -.next.b-res
        %wait  [%wait ~]
        %skip  [%skip ~]
        %cont  [%cont ..$(m-b self.next.b-res)]
        %fail  [%fail err.next.b-res]
        %done  [%cont (fun value.next.b-res)]
      ==
    ::
    ::  The strand monad must be evaluted in a particular way to maintain
    ::  its monadic character.  +take:eval implements this.
    ::
    ++  eval
      |%
      ::  Indelible state of a strand
      ::
      +$  eval-form
        $:  =form
        ==
      ::
      ::  Convert initial form to eval-form
      ::
      ++  from-form
        |=  =form
        ^-  eval-form
        form
      ::
      ::  The cases of results of +take
      ::
      +$  eval-result
        $%  [%next ~]
            [%fail err=(pair term tang)]
            [%done value=a]
        ==
      ::
      ++  validate-mark
        |=  [in=* =mark =bowl]
        ^-  cage
        =+  .^  =dais:clay  %cb
                /(scot %p our.bowl)/[q.byk.bowl]/(scot %da now.bowl)/[mark]
            ==
        =/  res  (mule |.((vale.dais in)))
        ?:  ?=(%| -.res)
          ~|  %spider-mark-fail
          (mean leaf+"spider: ames vale fail {<mark>}" p.res)
        [mark p.res]
      ::
      ::  Take a new sign and run the strand against it
      ::
      ++  take
        ::  cards: accumulate throughout recursion the cards to be
        ::         produced now
        =|  cards=(list card)
        |=  [=eval-form =strand-input]
        ^-  [[(list card) =eval-result] _eval-form]
        =*  take-loop  $
        =.  in.strand-input
          ?~  in.strand-input  ~
          =/  in  u.in.strand-input
          ?.  ?=(%agent -.in)      `in
          ?.  ?=(%fact -.sign.in)  `in
          ::
          :-  ~
          :^  %agent  wire.in  %fact
          (validate-mark q.q.cage.sign.in p.cage.sign.in bowl.strand-input)
        ::  run the strand callback
        ::
        =/  =output  (form.eval-form strand-input)
        ::  add cards to cards
        ::
        =.  cards
          %+  welp
            cards
          ::  XX add tag to wires?
          cards.output
        ::  case-wise handle next steps
        ::
        ?-  -.next.output
            %wait  [[cards %next ~] eval-form]
            %skip  [[cards %next ~] eval-form]
            %fail  [[cards %fail err.next.output] eval-form]
            %done  [[cards %done value.next.output] eval-form]
            %cont
          ::  recurse to run continuation with initialization input
          ::
          %_  take-loop
            form.eval-form  self.next.output
            strand-input    [bowl.strand-input ~]
          ==
        ==
      --
    --
  --  ::strand
::
+$  gift-arvo                                           ::  out result <-$
  $~  [%doze ~]
  $%  gift:ames
      gift:behn
      gift:clay
      gift:dill
      gift:eyre
      gift:gall
      gift:iris
      gift:jael
      gift:khan
  ==
+$  task-arvo                                           ::  in request ->$
  $%  task:ames
      task:clay
      task:behn
      task:dill
      task:eyre
      task:gall
      task:iris
      task:jael
      task:khan
  ==
+$  note-arvo                                           ::  out request $->
  $~  [%b %wake ~]
  $%  [%a task:ames]
      [%b task:behn]
      [%c task:clay]
      [%d task:dill]
      [%e task:eyre]
      [%g task:gall]
      [%i task:iris]
      [%j task:jael]
      [%k task:khan]
      [%$ %whiz ~]
      [@tas %meta vase]
  ==
::  full vane names are required in vanes
::
+$  sign-arvo                                           ::  in result $<-
  $%  [%ames gift:ames]
      $:  %behn
          $%  gift:behn
              $>(%wris gift:clay)
              $>(%writ gift:clay)
              $>(%mere gift:clay)
              $>(%unto gift:gall)
          ==
      ==
      [%clay gift:clay]
      [%dill gift:dill]
      [%eyre gift:eyre]
      [%gall gift:gall]
      [%iris gift:iris]
      [%jael gift:jael]
      [%khan gift:khan]
  ==
::  $unix-task: input from unix
::
+$  unix-task                                           ::  input from unix
  $~  [%wake ~]
  $%  ::  %dill: keyboard input
      ::
      $>(%belt task:dill)
      ::  %dill: configure terminal (resized)
      ::
      $>(%blew task:dill)
      ::  %clay: new process
      ::
      $>(%boat task:clay)
      ::  %behn/%eyre/%iris: new process
      ::
      $>(%born vane-task)
      ::  %eyre: cancel request
      ::
      [%cancel-request ~]
      ::  %dill: reset terminal configuration
      ::
      $>(%hail task:dill)
      ::  %ames: hear packet
      ::
      $>(%hear task:ames)
      ::  %clay: external edit
      ::
      $>(%into task:clay)
      ::  %clay: synchronous commit
      ::
      ::    TODO: make $yuki an option for %into?
      ::
      $>(%park task:clay)
      ::  %eyre: learn ports of live http servers
      ::
      $>(%live task:eyre)
      ::  %iris: hear (partial) http response
      ::
      $>(%receive task:iris)
      ::  %eyre: starts handling an inbound http request
      ::
      $>(%request task:eyre)
      ::  %eyre: starts handling an backdoor http request
      ::
      $>(%request-local task:eyre)
      ::  %dill: close session
      ::
      $>(%shut task:dill)
      ::  %behn: wakeup
      ::
      $>(%wake task:behn)
  ==
--  ::
