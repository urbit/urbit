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
::    Debug output can be adjusted using %sift and %spew $task's.
::
!:
=,  ames
=*  point               point:jael
=*  public-keys-result  public-keys-result:jael
=/  packet-size  13
::
=>  ::  common helpers
    ~%  %ames  ..part  ~
    |%
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
    ::
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
    ::
    --
::
=>  ::  vane IO
    ::
    |%
    +$  sign
      $~  [%behn %wake ~]
      $%  [%ames $>(?(%tune %mess-response) gift)]
          [%behn $>(%wake gift:behn)]
          [%gall $>(?(%flub %unto) gift:gall)]
          [%jael $>(?(%private-keys %public-keys %turf) gift:jael)]
          $:  @tas
              $>(?(%noon %boon %done) gift)
      ==  ==
    ::  $move: output effect; either request or response
    ::
    +$  move  [=duct card=(wind note gift)]
    ::  $note: request to other vane
    ::
    +$  note
      $~  [%b %wait *@da]
      $%  $:  %a
              $>(?(%deep %keen %meek %moke %prod) task)
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
              $>(?(%clog %deal) task:gall)
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
    ::
    --
::
=>  ::  ames helpers
    ::
    ~%  %ames  ..trace  ~
    |%
    ::
    +|  %helpers
    :: +get-forward-lanes: get all lanes to send to when forwarding to peer
    ::
    ++  get-forward-lanes
      |=  [our=@p peer=peer-state peers=(map ship ship-state)]
      ^-  (list lane)
      =;  zar=(trap (list lane))
        ?~  route.peer  $:zar
        =*  rot  u.route.peer
        ?:(direct.rot [lane.rot ~] [lane.rot $:zar])
      ::
      |.  ^-  (list lane)
      ?:  ?=(%czar (clan:title sponsor.peer))
        ?:  =(our sponsor.peer)
          ~
        [%& sponsor.peer]~
      =/  next  (~(get by peers) sponsor.peer)
      ?.  ?=([~ %known *] next)
        ~
      $(peer +.u.next)
    ::
    ++  chain
      =<  mop
      |%
      ++  on   ((^on ,@ ,[key=@ =path]) lte)
      +$  mop  ^chain
      --
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
    ++  jim  |=(n=* ~>(%memo./ames/jam (jam n)))
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
      ^-  (unit parsed-bone-wire)
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
    ::
    +$  parsed-bone-wire
      $%  [%old her=ship =bone]
          [%new her=ship =rift =bone]
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
      :: ~/  %encode-keys-packet
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
      |=  [=ship =life =acru]
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
      ++  etch-data
        |=  [=path data=$@(~ (cask))]
        =/  sig=@  (full path data)
        ?~  data  sig
        (mix sig (lsh 9 (jam data)))
      ++  etch-open
        |=  [=path =hunk data=$@(~ (cask))]
        (etch path hunk (etch-data path data))
      ::
      ++  etch
        |=  [=path =hunk mes=@]
        ^-  (list yowl)
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
      |=  [pac=open-packet =acru]
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
      :: ~/  %etch-shut-packet
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
      :: ~/  %sift-shut-packet
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
    ++  is-our-bulk
      |=  [our=ship ames-state=axle =balk]
      ^-  ?
      =-  ~?  =(| -)
            [%fine-mismatch our=[rift life]:ames-state her=[her rif lyf]:balk]
          -
      ?&  =(our her.balk)
          =(rift.ames-state rif.balk)
          =(life.ames-state lyf.balk)
      ==
    ::
    ++  check-fine-key
      |=  [ames-state=axle =balk key-idx=@]
      ^-  ?
      ?~  link=(get:on:chain server-chain.ames-state key-idx)
        |
      =/  gol  path.u.link
      =/  =path  [van.balk car.balk spr.balk]
      |-  ^-  ?
      ?~  gol   &
      ?~  path  |
      ?.  =(i.path i.gol)
        |
      $(path t.path, gol t.gol)
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
    ++  poke-ping-app
      |=  [=duct our=ship poke=?(%stop %once [%kick fail=?])]
      ^-  move
      [duct %pass /ping %g %deal [our our /ames] %ping %poke noun+!>(poke)]
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
              crypto-core=acru
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
    +$  ames-state-21
      $+  ames-state-21
      $:  peers=(map ship ship-state)
          =unix=duct
          =life
          =rift
          crypto-core=acru
          =bug
          snub=[form=?(%allow %deny) ships=(set ship)]
          cong=[msg=_5 mem=_100.000]
        ::
          $=  dead
          $:  flow=[%flow (unit dead-timer)]
              cork=[%cork (unit dead-timer)]
          ==
        ::
          =chain
        ==
    ::
    +$  dead-timer       [=duct =wire date=@da]
    +$  azimuth-state    [=symmetric-key =life =rift =public-key sponsor=ship]
    +$  azimuth-state-6  [=symmetric-key =life =public-key sponsor=ship]
    +$  ames-state-4     ames-state-5
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
          crypto-core=acru
          bug=bug-19
          snub=[form=?(%allow %deny) ships=(set ship)]
          cong=[msg=@ud mem=@ud]
      ==
    ::
    +$  ship-state-13
      $+  ship-state-13
      $%  [%alien alien-agenda-17]
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
    +$  ames-state-18  ames-state-17
    +$  ames-state-17
      $+  ames-state-17
      $:  peers=(map ship ship-state-17)
          =unix=duct
          =life
          =rift
          crypto-core=acru
          bug=bug-19
          snub=[form=?(%allow %deny) ships=(set ship)]
          cong=[msg=_5 mem=_100.000]
        ::
          $=  dead
          $:  flow=[%flow (unit dead-timer)]
              cork=[%cork (unit dead-timer)]
      ==  ==
    ::
    +$  ship-state-17
      $+  ship-state-17
      $%  [%alien alien-agenda-17]
          [%known peer-state-17]
      ==
    ::
    +$  peer-state-17
      $+  peer-state-17
      $:  $:  =symmetric-key
              =life
              =rift
              =public-key
              sponsor=ship
          ==
          route=(unit [direct=? =lane])
          =qos
          =ossuary
          snd=(map bone message-pump-state-17)
          rcv=(map bone message-sink-state)
          nax=(set [=bone =message-num])
          heeds=(set duct)
          closing=(set bone)
          corked=(set bone)
          keens=(map path keen-state)
      ==
    ::
    +$  message-pump-state-17
      $+  message-pump-state-17
      $:  current=_`message-num`1
          next=_`message-num`1
          unsent-messages=(qeu message-blob)
          unsent-fragments=(list static-fragment)
          queued-message-acks=(map message-num ack)
          =packet-pump-state
      ==
    ::
    +$  ames-state-14  ames-state-16
    +$  ames-state-15  ames-state-16
    +$  ames-state-16
      $+  ames-state-16
      $:  peers=(map ship ship-state-16)
          =unix=duct
          =life
          =rift
          crypto-core=acru
          bug=bug-19
          snub=[form=?(%allow %deny) ships=(set ship)]
          cong=[msg=@ud mem=@ud]
      ==
    ::
    +$  alien-agenda-17
      $+  alien-agenda
      $:  messages=(list [=duct =plea])
          packets=(set =blob)
          heeds=(set duct)
          keens=(jug path duct)
      ==
    ::
    +$  ship-state-16
      $+  ship-state-16
      $%  [%alien alien-agenda-17]
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
    +$  task-4-til-8
      $+  task-4-til-8
      $%  [?(%heed %jilt) =ship]              ::  introduced in state %4, removed in %21
          $<(?(%snub %kroc %deep %keen) task) ::  tasks introduced later
      ==
    +$  queued-event-4-til-8
      $+  queued-event-4-til-8
      $%  [%call =duct wrapped-task=(hobo task-4-til-8)]
          [%take =wire =duct =sign]
      ==
    ::
    +$  queued-event-9-til-11
      $+  queued-event-9-til-11
      $%  [%call =duct wrapped-task=(hobo task-9-til-11)]
          [%take =wire =duct =sign]
      ==
    ::
    +$  task-9-til-11
      $+  task-9-til-11
      $%  [%kroc dry=?]                 ::  introduced in state %10, modified in %17
          [%snub ships=(list ship)]     ::  introduced in state %9,  modified in %11
          [?(%heed %jilt) =ship]        ::  introduced in state %4, removed in %21
          $<(?(%snub %kroc %deep %keen) task) ::  %deep/%keen introduced later
      ==
    ::
    +$  queued-event-12-til-16
      $+  queued-event-12-til-16
      $%  [%call =duct wrapped-task=(hobo task-12-til-16)]
          [%take =wire =duct =sign]
      ==
    ::
    +$  task-12-til-16
      $+  task-12-til-16
      $%  [%kroc dry=?]                 ::  introduced in state %10, modified in %17
          [%keen spar]                  ::  introduced in state %13, modified in %19
          deep-task-14                  ::  introduced in state %14, modified in %19
          [?(%heed %jilt) =ship]        ::  introduced in state %4, removed in %21
          $<(?(%kroc %keen %deep) task)
      ==
    ::
    +$  deep-task-14
      $:  %deep
          $%  [%nack =ship =nack=bone =message-blob]
              [%sink =ship =target=bone naxplanation=[=message-num =error]]
              [%drop =ship =nack=bone =message-num]
              [%cork =ship =bone]
              [%kill =ship =bone]
      ==  ==
    ::
    +$  queued-event-17-and-18
      $+  queued-event-17-and-18
      $%  [%call =duct wrapped-task=(hobo task-16-and-18)]
          [%take =wire =duct =sign]
      ==
    ::
    +$  task-16-and-18
      $+  task-16-and-18
      $%  [%keen spar]                  ::  introduced in state %13, modified in %19
          deep-task-14                  ::  introduced in state %14, modified in %19
          [?(%heed %jilt) =ship]        ::  introduced in state %4, removed in %21
          $<(?(%keen %deep) task)
      ==
    ::
    +$  bug-19
      $:  veb=_[`?`%.n `?`%.n `?`%.n `?`%.n `?`%.n `?`%.n `?`%.n `?`%.n `?`%.n]
          ships=(set ship)
      ==
    ::
    +$  ames-state-19
      $+  ames-state-19
      $:  peers=(map ship ship-state-20)
          =unix=duct
          =life
          =rift
          crypto-core=acru
          bug=bug-19
          snub=[form=?(%allow %deny) ships=(set ship)]
          cong=[msg=@ud mem=@ud]
          $=  dead
          $:  flow=[%flow (unit dead-timer)]
              cork=[%cork (unit dead-timer)]
          ==
        ::
          =chain
      ==
    ::
    +$  task-19-and-20
      $+  task-19-and-20
      $%  [?(%heed %jilt) =ship]       ::  introduced in state %4, removed in %21
          task
      ==
    ::
    +$  queued-event-19-and-20
      $+  queued-event-19-and-20
      $%  [%call =duct wrapped-task=(hobo task-19-and-20)]
          [%take =wire =duct =sign]
      ==
    ::
    +$  peer-state-20
      $+  peer-state-20
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
          keens=(map path keen-state)
          =chain
      ==
    ::
    +$  alien-agenda-20
      $+  alien-agenda-20
      $:  messages=(list [=duct =plea])
          packets=(set =blob)
          heeds=(set duct)
          keens=(jug path duct)
          chums=(jug path duct)
      ==
    ::
    +$  ship-state-20
      $+  ship-state-20
      $%  [%alien alien-agenda-20]
          [%known peer-state-20]
      ==
    ::
    +$  ames-state-20
      $+  ames-state-20
      $:  peers=(map ship ship-state-20)
          =unix=duct
          =life
          =rift
          crypto-core=acru
          =bug
          snub=[form=?(%allow %deny) ships=(set ship)]
          cong=[msg=_5 mem=_100.000]
        ::
          $=  dead
          $:  flow=[%flow (unit dead-timer)]
              cork=[%cork (unit dead-timer)]
          ==
        ::
          =chain
      ==
    ::
    +|  %dialectics
    ::  $queued-event: event to be handled after initial boot completes
    ::
    +$  queued-event
      $+  queued-event
      $%  [%call =duct wrapped-task=(hobo task)]
          [%take =wire =duct =sign]
      ==
    ::  $message-pump-task: job for |message-pump
    ::
    ::    %memo: packetize and send application-level message
    ::    %hear: handle receipt of ack on fragment or message
    ::    %near: handle receipt of naxplanation
    ::    $prod: reset congestion control
    ::    %wake: handle timer firing
    ::
    +$  message-pump-task
      $%  [%memo =message]
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
    ::
    --
::
=>  ::  mesa helpers
    ::
    |%
    ::
    +|  %helpers
    ::
    ++  get-forward-lanes-mesa
      |=  [our=@p fren=fren-state chums=(map ship chum-state)]
      ^-  (list lane:pact)
      ?^  lane.fren
        (drop lane.fren)
      ?:  ?=(%czar (clan:title sponsor.fren))
        ?:  =(our sponsor.fren)
          ~
        [`@ux`sponsor.fren]~
      =/  next  (~(get by chums) sponsor.fren)
      ?.  ?=([~ %known *] next)
        ~
      $(fren +.u.next)
    ::
    ++  key-chain  ((on ,@ ,[key=@ =path]) lte)
    ::
    ++  parse-packet  |=(a=@ -:($:de:pact a))
    ++  inner-path-to-beam
      |=  [her=ship pat=(pole knot)]
      ^-  (unit [vew=view bem=beam])
      ::  /vane/care/case/desk/[spur]
      ::
      ?.  ?=([van=@ car=@ cas=@ des=@ pur=*] pat)
        ~
      ?~  cas=(de-case cas.pat)
        ~
      `[[van car]:pat [her des.pat u.cas] pur.pat]  :: XX
    ::
    +|  %encryption
    ::
    +$  binding  [=path root=@uxI]
    ::  +get-key-for : eddh with our key
    ::
    ++  get-key-for
      |=  [=ship =life chums=(map ship chum-state)]
      ^-  (unit pass)
      =+  chum=(~(get by chums) ship)
      ?.  ?=([~ %known *] chum)
        =<  `pass  :: XX check suite?
        .^([suite=@ud =pass] %j /=puby=/[(scot %p ship)]/[(scot %ud life)])  :: XX rof
      ?.  =(life life.+.u.chum)
         ~  :: XX  log?
      `symmetric-key.+.u.chum
    ::
    ++  get-group-key-for  |=(@ud `(unit @uxI)`(some `@uxI`0))  ::  XX implement?
    ++  crypt
      |%
      ::
      ++  const-cmp
        |=  [a=@ b=@]
        ^-  ?
        =(0 (~(dif fe 7) a b))  :: XX jet for constant-time
      ::
      ++  etch-path
        |=  =path
        ^-  octs
        =/  pax  (spat path)
        (met 3 pax)^pax
      ::
      ++  sift-path
        |=  =octs
        ^-  path
        (stab q.octs)
      ::
      ++  etch-binding
        |=  =binding
        ^-  octs
        =/  pax  (etch-path path.binding)
        [(add p.pax 32) (can 3 pax [32 root.binding] ~)]
      ::
      ++  kdf  kdf:blake3:blake:crypto
      ::
      ++  crypt
        |=  [key=@uxI iv=octs msg=octs]
        ^-  octs
        =/  x  (xchacha:chacha:crypto 8 key (kdf 24 "mesa-crypt-iv" iv))
        (chacha:crypto 8 key.x nonce.x 0 msg)
      ::
      ++  sign
        |=  [sek=@uxI =binding]
        ^-  @uxJ
        (sign-octs:ed:crypto (etch-binding binding) sek)
      ::
      ++  verify-sig
        |=  [pub=@uxI sig=@uxJ =binding]
        ^-  ?
        (veri-octs:ed:crypto sig (etch-binding binding) pub)
      ::
      ++  mac
        |=  [key=@uxI =binding]
        ^-  @uxH
        (keyed-hash key 16 (etch-binding binding))
      ::
      ++  verify-mac
        |=  [key=@uxI tag=@uxH =binding]
        ^-  ?
        (const-cmp tag (mac key binding))
      ::
      ++  encrypt
        |=  [key=@uxI iv=@ msg=@]
        ^-  @
        =/  =octs  (crypt key (met 3 iv)^iv (met 3 msg)^msg)
        (can 3 octs [1 0x1] ~)
      ::
      ++  decrypt
        |=  [key=@uxI iv=@ cyf=@]
        ^-  @
        =/  wid  (dec (met 3 cyf))
        ?>  =(0x1 (cut 3 [wid 1] cyf))
        q:(crypt key (met 3 iv)^iv wid^cyf)
      ::
      ++  seal-path
        |=  [key=@uxI =path]
        ^-  @
        =/  keys  (kdf 64 "mesa-aead" 32^key)
        =/  pat  (etch-path path)
        =/  tag  (keyed-hash (rsh 8 keys) 16 pat)
        =/  cyf  (crypt (end 8 keys) 16^tag pat)
        (can 3 [16 tag] cyf [1 0x1] ~)
      ::
      ++  open-path
        |=  [key=@uxI sealed=@]
        ^-  path
        =/  keys  (kdf 64 "mesa-aead" 32^key)
        =/  wid  (dec (met 3 sealed))
        ?>  =(0x1 (cut 3 [wid 1] sealed))
        =/  [tag=@ux cyf=@ux]  [(end [3 16] sealed) (rsh [3 16] sealed)]
        =/  pat  (crypt (end 8 keys) 16^tag (sub wid 16)^cyf)
        ?>  (const-cmp tag (keyed-hash (rsh 8 keys) 16 pat))
        (sift-path pat)
      ::
      --
    ::
    ++  keyed-hash
      |=  [key=@uxI out=@ud msg=octs]
      ((keyed:blake3:blake:crypto 32^key) out msg)
    ::
    +|  %flow-gifts
    ::
    +$  flow-sign
      $%  $>(?(%flub %done) gift:gall)
          [%mess-response seq=@ud sage:mess] :: added seq number to %response
      ==
    ::
    --
::  external vane interface
::
|=  our=ship
=|  ames-state=axle
=*  unix-duct  unix-duct.ames-state
::
|=  [now=@da eny=@uvJ rof=roof]
=*  vane-gate  .
::
=>  ::  pre/post migrated core handler
    ::
    |%
    ++  ames
      ::
      =>  ~%  %per-event  ..trace  ~
          |%
          ::  |ev: inner event-handling core
          ::
          ++  ev
            =|  moves=(list move)
            ~%  %event-gate  ..ev  ~
            |=  [=duct ames-state=axle]
            =*  veb  veb.bug.ames-state
            =|  cork-bone=(unit bone)  ::  modified by +on-kroc
            =|  vane=?(%fine %ames)
            ~%  %event-core  ..$  ~
            |%
            +|  %helpers
            ::
            ++  event-core  .
            ++  abet  [(flop moves) ames-state]
            ++  emit  |=  =move
                      event-core(moves [move moves])
            ++  emil  |=(mos=_moves event-core(moves (weld (flop mos) moves)))
            ++  channel-state  [life crypto-core bug]:ames-state
            ++  ev-trace
              |=  [verb=? =ship print=(trap tape)]
              ^+  same
              (trace vane verb ship ships.bug.ames-state print)
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
            ::
            ++  get-sponsors
              ;;  (list ship)
              =<  q.q  %-  need  %-  need
              (rof [~ ~] /ames %j `beam`[[our %saxo %da now] /(scot %p our)])
            ::
            +|  %tasks
            ::  +on-take-flub: vane not ready to process message, pretend it
            ::                 was never delivered
            ::
            ++  on-take-flub
              |=  =wire
              ^+  event-core
              ?~  parsed=(parse-bone-wire wire)
                ::  no-op
                ::
                ~>  %slog.0^leaf/"ames: dropping malformed wire: {(spud wire)}"
                event-core
              ?>  ?=([@ her=ship *] u.parsed)
              =*  her  her.u.parsed
              =/  peer-core  (abed-got:pe her)
              ?:  ?&  ?=([%new *] u.parsed)
                      (lth rift.u.parsed rift.peer-state.peer-core)
                  ==
                ::  ignore events from an old rift
                ::
                %-  %^  ev-trace  odd.veb  her
                    |.("dropping old rift wire: {(spud wire)}")
                event-core
              =/  =bone
                ?-(u.parsed [%new *] bone.u.parsed, [%old *] bone.u.parsed)
              abet:(on-flub:peer-core bone)
            ::  +on-take-done: handle notice from vane that it processed a message
            ::
            ++  on-take-done
              |=  [=wire error=(unit error)]
              ^+  event-core
              ?~  parsed=(parse-bone-wire wire)
                ::  no-op
                ::
                ~>  %slog.0^leaf/"ames: dropping malformed wire: {(spud wire)}"
                event-core
              ?>  ?=([@ her=ship *] u.parsed)
              =/  peer-core  (abed-got:pe her.u.parsed)
              ?~  bon=(bone-ok u.parsed wire rift.peer-state.peer-core)
                event-core
              ::  relay the vane ack to the foreign peer
              ::
              |^  =<  abet
              ?~(error (send-ack u.bon) (send-nack u.bon u.error))
              ::
              ::  if processing succeded, send positive ack packet and exit
              ::
              ++  send-ack
                |=  =bone
                ^+  peer-core
                ::  handle cork only deals with bones that are in closing
                ::
                %.  bone
                handle-cork:abet:(call:(abed:mi:peer-core bone) %done ok=%.y)
              ::  failed; send message nack packet
              ::
              ++  send-nack
                |=  [=bone =^error]
                ^+  peer-core
                =.  peer-core  abet:(call:(abed:mi:peer-core bone) %done ok=%.n)
                ::  construct nack-trace message, referencing .failed $message-num
                ::
                =/  failed=message-num
                  last-acked:(~(got by rcv.peer-state.peer-core) bone)
                =/  =message  [%naxplanation failed error]
                ::  send nack-trace message on associated .nack-bone
                ::
                =/  nack-bone=^bone  (mix 0b10 bone)
                abet:(call:(abed:mu:peer-core nack-bone) %memo message)
              --
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
                ?~  par
                  ::  XX  this shouldn't be needed
                  ::  XX  only if %alien
                  ~&  >>  rettrieve-keys-again/her
                  (emit [//keys]~ %pass /public-keys %j %public-keys [n=her ~ ~])
                  :: event-core
                =/  peer-core  (abed-peer:pe her u.par)
                =/  bones  ~(tap in ~(key by snd.u.par))
                |-  ^+  event-core
                ?~  bones      abet:peer-core
                =.  peer-core  abet:(call:(abed:mu:peer-core i.bones) %prod ~)
                $(bones t.bones)
              ::
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
            ::    .arg can be %rift or %dead
            ::
            ++  on-stir
              |=  arg=@t
              ^+  event-core
              |^  ?+  arg  do-stir
                    %rift  do-rift
                    %dead  do-dead
                  ==
              ::
              ++  do-dead
                =/  ded=(unit dead-timer)  +.flow.dead.ames-state
                ?^  ded
                  %-  (slog leaf+"ames: turning off dead flow consolidation" ~)
                  =.  event-core
                    (emit:event-core duct.u.ded %pass wire.u.ded %b %rest date.u.ded)
                  =.  flow.dead.ames-state.event-core  [%flow ~]
                  (wake-dead-flows:event-core ~)
                ::
                %-  (slog leaf+"ames: switching to dead flow consolidation" ~)
                =;  cor=event-core
                  set-dead-flow-timer:cor
                %-  ~(rep by peers.ames-state:event-core)
                |=  [[=ship =ship-state] core=_event-core]
                ^+  event-core
                =/  peer-state=(unit peer-state)  (get-peer-state:core ship)
                ?~  peer-state  core
                %-  ~(rep by snd.u.peer-state)
                |=  [[=bone =message-pump-state] cor=_core]
                ^+  event-core
                =/  next-wake  next-wake.packet-pump-state.message-pump-state
                ?.  ?&  =(~m2 rto.metrics.packet-pump-state.message-pump-state)
                        ?=(^ next-wake)
                    ==
                  cor
                =/  peer-core  (abed-peer:pe:cor ship u.peer-state)
                =/  message-pump  (abed:mu:peer-core bone)
                abet:(pu-emit:packet-pump:message-pump %b %rest u.next-wake)
              ::
              ++  do-rift
                =/  =rift
                  =-  ~|(%no-rift (,@ q.q:(need (need -))))
                  (rof [~ ~] /ames %j `beam`[[our %rift %da now] /(scot %p our)])
                ?:  =(rift rift.ames-state)
                  event-core
                ~&  "ames: fixing rift from {<rift.ames-state>} to {<rift>}"
                event-core(ames-state ames-state(rift rift))
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
                    (rof [~ ~] /ames %bx [[our %$ da+now] /debug/timers])
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
              --
            ::  +on-crud: handle event failure; print to dill
            ::
            ++  on-crud
              |=  =error
              ^+  event-core
              (emit duct %pass /crud %d %flog %crud error)
            ::  +on-hear: handle raw packet receipt
            ::
            ++  on-hear
              |=  [l=lane b=blob d=(unit goof)]
              ^+  event-core
              =/  =shot     (sift-shot b)
              ?:  sam.shot  (on-hear-packet l shot d)
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
              abet:(on-hear-fine:(abed-got:pe her) l shot)
            ::  +on-hear-packet: handle mildly processed packet receipt
            ::
            ++  on-hear-packet
              :: ~/  %on-hear-packet
              |=  [=lane =shot dud=(unit goof)]
              ^+  event-core
              %-  (ev-trace rcv.veb sndr.shot |.("received packet"))
              ::
              ?:  =(our sndr.shot)
                event-core
              ?:  .=  =(%deny form.snub.ames-state)
                  (~(has in ships.snub.ames-state) sndr.shot)
                %-  (ev-trace rcv.veb sndr.shot |.("snubbed"))
                event-core
              ::
              %.  +<
              ::
              ?.  =(our rcvr.shot)
                on-hear-forward
              ::
              ?:  =(%keys content.shot)
                on-hear-keys
              ?:  ?&  ?=(%pawn (clan:title sndr.shot))
                      !?=([~ %known *] (~(get by peers.ames-state) sndr.shot))
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
              :: ~/  %on-hear-forward
              |=  [=lane =shot dud=(unit goof)]
              ^+  event-core
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
              (send-blob for=& rcvr.shot blob (~(get by peers.ames-state) rcvr.shot))
            ::  +on-hear-keys: handle receipt of attestion request
            ::
            ++  on-hear-keys
              :: ~/  %on-hear-keys
              |=  [=lane =shot dud=(unit goof)]
              =+  %^  ev-trace  msg.veb  sndr.shot
                  |.("requested attestation")
              ?.  =(%pawn (clan:title our))
                event-core
              =/  =blob  (attestation-packet sndr.shot 1)
              (send-blob for=| sndr.shot blob (~(get by peers.ames-state) sndr.shot))
            ::  +on-hear-open: handle receipt of plaintext comet self-attestation
            ::
            ++  on-hear-open
              :: ~/  %on-hear-open
              |=  [=lane =shot dud=(unit goof)]
              ^+  event-core
              =+  %^  ev-trace  msg.veb  sndr.shot
                  |.("got attestation")
              ::  assert the comet can't pretend to be a moon or other address
              ::
              ?>  ?=(%pawn (clan:title sndr.shot))
              ::  if we already know .sndr, ignore duplicate attestation
              ::
              =/  ship-state  (~(get by peers.ames-state) sndr.shot)
              ?:  ?=([~ %known *] ship-state)
                event-core
              ::
              =/  =open-packet  (sift-open-packet shot our life.ames-state)
              ::  add comet as an %alien if we haven't already
              ::
              =?  peers.ames-state  ?=(~ ship-state)
                (~(put by peers.ames-state) sndr.shot %alien *alien-agenda)
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
                      ^=  sponsor  `(^sein:title sndr.shot)
                  ==
                =+  ev-core=~(ev-core mesa [duct sndr.shot^*fren-state])
                =^  moves  ames-state
                  =<  ev-abet
                  sy-abet:(sy-publ:sy:ev-core / [%full (my [sndr.shot point]~)])
                (emil moves)
              ::  manually add the lane to the peer state
              ::
              =/  =peer-state  (gut-peer-state sndr.shot)
              =.  route.peer-state  `[direct=%.n lane]
              =.  peers.ames-state
                (~(put by peers.ames-state) sndr.shot %known peer-state)
              ::
              =.  event-core
                %-  emit
                :*  unix-duct.ames-state  %give  %nail  sndr.shot
                    (get-forward-lanes our peer-state peers.ames-state)
                ==
              ::
              event-core
            ::  +on-hear-shut: handle receipt of encrypted packet
            ::
            ++  on-hear-shut
              ~/  %on-hear-shut
              |=  [=lane =shot dud=(unit goof)]
              ^+  event-core
              =/  sndr-state  (~(get by peers.ames-state) sndr.shot)
              ::  If we don't know them, ask Jael for their keys. If they're a
              ::  comet, this will also cause us to request a self-attestation
              ::  from the sender. The packet itself is dropped; we can assume it
              ::  will be resent.
              ::
              ?.  ?=([~ %known *] sndr-state)
                (enqueue-alien-todo sndr.shot sndr-state |=(alien-agenda +<))
              ::  decrypt packet contents using symmetric-key.channel
              ::
              ::    If we know them, we have a $channel with them, which we've
              ::    populated with a .symmetric-key derived from our private key
              ::    and their public key using elliptic curve Diffie-Hellman.
              ::
              =/  =peer-state   +.u.sndr-state
              =/  =channel      [[our sndr.shot] now channel-state -.peer-state]
              =?  event-core  !=(sndr-tick.shot (mod her-life.channel 16))
                %.  event-core
                %^  ev-trace  odd.veb  sndr.shot
                |.  ^-  tape
                =/  sndr  [sndr-tick=sndr-tick.shot her-life=her-life.channel]
                "sndr-tick mismatch {<sndr>}"
              =?  event-core  !=(rcvr-tick.shot (mod our-life.channel 16))
                %.  event-core
                %^  ev-trace  odd.veb  sndr.shot
                |.  ^-  tape
                =/  rcvr  [rcvr-tick=rcvr-tick.shot our-life=our-life.channel]
                "rcvr-tick mismatch {<rcvr>}"
              ~|  %ames-crash-on-packet-from^her.channel
              =/  shut-packet=(unit shut-packet)
                (sift-shut-packet shot [symmetric-key her-life our-life]:channel)
              ?~  shut-packet
                event-core
              =/  old-route  route.peer-state
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
              ::
              =?  event-core  !=(old-route route.peer-state)
                %-  emit
                :*  unix-duct.ames-state  %give  %nail  sndr.shot
                    (get-forward-lanes our peer-state peers.ames-state)
                ==
              ::  perform peer-specific handling of packet
              ::
              =<  abet
              (~(on-hear-shut-packet pe peer-state channel) [lane u.shut-packet dud])
            ::
            ++  bone-ok
              |=  [parsed=parsed-bone-wire =wire =rift]
              ^-  (unit bone)
              =*  her        her.parsed
              ::
              ?:  ?&  ?=([%new *] parsed)
                      (lth rift.parsed rift)
                  ==
                ::  ignore events from an old rift
                ::
                %-  %^  ev-trace  odd.veb  her
                    |.("dropping old rift wire: {(spud wire)}")
                ~
              =/  =bone
                ?-(parsed [%new *] bone.parsed, [%old *] bone.parsed)
              =+  ?.  ?=([%old *] parsed)  ~
                %-  %^  ev-trace  odd.veb  her
                    |.("parsing old wire: {(spud wire)}")
                ~
              `bone
            ::
            ++  on-take-noon
              |=  [=wire payload=* id=*]
              ^+  event-core
              ?~  parsed=(parse-bone-wire wire)
                ~>  %slog.0^leaf/"ames: dropping malformed wire: {(spud wire)}"
                event-core
              ?>  ?=([@ her=ship *] u.parsed)
              =/  peer-core  (abed-got:pe her.u.parsed)
              ?~  bone=(bone-ok u.parsed wire rift.peer-state.peer-core)
                event-core
              ::
              abet:(check-clog:(on-memo:peer-core u.bone [%boon payload]) u.bone id)
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
              =/  peer-core  (abed-got:pe her.u.parsed)
              ?~  bone=(bone-ok u.parsed wire rift.peer-state.peer-core)
                event-core
              abet:(on-memo:peer-core u.bone [%boon payload])
            ::  +on-plea: handle request to send message
            ::
            ++  on-plea
              |=  [=ship =plea]
              ^+  event-core
              =/  ship-state  (~(get by peers.ames-state) ship)
              ::
              ?.  ?=([~ %known *] ship-state)
                %^  enqueue-alien-todo  ship  ship-state
                |=  todos=alien-agenda
                todos(messages [[duct plea] messages.todos])
              ::
              =+  peer-core=(abed-peer:pe ship +.u.ship-state)
              ::  .plea is from local vane to foreign ship
              ::
              =^  =bone  peer-core  (bind-duct:peer-core duct)
              %-  %^  ev-trace  msg.veb  ship
                  |.  ^-  tape
                  =/  sndr  [our our-life.channel.peer-core]
                  =/  rcvr  [ship her-life.channel.peer-core]
                  "plea {<sndr rcvr bone=bone vane.plea path.plea>}"
              abet:(on-memo:peer-core bone [%plea plea])
            ::
            ++  on-tune
              |=  [=wire s=[=ship path=(pole knot)] roar=(unit roar)]
              ^+  event-core
              :: XX save or decrypt path?
              :: XX crash in decryption/cue indicates misbehaving peer
              ::
              =/  per  (~(get by peers.ames-state) ship.s)
              ?>  ?=([~ %known *] per)
              ?>  ?=([%a %x @ %$ rest=*] path.s)
              ?.  ?=([%chum her=@ lyf=@ cyf=@ ~] rest.path.s)
                =>  .(wire `(pole knot)`wire)
                ~|  bad-wire/wire
                ?>  ?=([%fine %shut idx=@ ~] wire)
                ~|  bad-path/rest.path.s
                ?>  ?=([%fine %shut kef=@ cyf=@ ~] rest.path.s)
                =/  [key=@ ,path]  (~(got by chain.u.per) (slav %ud idx.wire))
                =/  raw=@t
                  (dy:crub:crypto key (slav %uv cyf.rest.path.s))
                =/  pax=path
                  (stab raw)
                =;  dat=(unit (unit page))
                  (emit duct [%give %near [ship.s pax] dat])
                ?:  ?|  ?=(~ roar)
                        ?=(~ q.dat.u.roar)
                    ==
                  ~  :: XX weird
                ?>  ?=([%atom @] u.q.dat.u.roar)
                =-  ``;;(page (cue -))
                (dy:crub:crypto key q.u.q.dat.u.roar)
              ?>  ?=([%chum *] wire)
              =/  pax
                %-  stab
                (dy:crub:crypto symmetric-key.u.per (slav %uv cyf.rest.path.s))
              =/  dat=(unit (unit page))
                ?:  ?|  ?=(~ roar)
                        ?=(~ q.dat.u.roar)
                    ==
                  ~  :: XX weird
                ?>  ?=([%atom @] u.q.dat.u.roar)
                =-  `?~(- ~ `(,page (cue -)))
                (dy:crub:crypto symmetric-key.u.per q.u.q.dat.u.roar)
              (emit duct [%give %near [ship.s pax] dat])
            ::  +on-cork: handle request to kill a flow
            ::
            ++  on-cork
              |=  =ship
              ^+  event-core
              =/  =plea       [%$ /flow [%cork ~]]
              =/  ship-state  (~(get by peers.ames-state) ship)
              ?.  ?=([~ %known *] ship-state)
                %^  enqueue-alien-todo  ship  ship-state
                |=  todos=alien-agenda
                todos(messages [[duct plea] messages.todos])
              ::
              =+  peer-core=(abed-peer:pe ship +.u.ship-state)
              =^  =bone  peer-core
                ?^  cork-bone  [u.cork-bone peer-core]
                (bind-duct:peer-core duct)
              ::
              ?.  (~(has by by-bone.ossuary.peer-state.peer-core) bone)
                %.  event-core
                %^  ev-trace  odd.veb  ship
                |.("trying to cork {<bone=bone>}, not in the ossuary, ignoring")
              ::
              %-  %^  ev-trace  msg.veb  ship
                  |.  ^-  tape
                  =/  sndr  [our our-life.channel.peer-core]
                  =/  rcvr  [ship her-life.channel.peer-core]
                  "cork plea {<sndr rcvr bone=bone vane.plea path.plea>}"
              abet:(on-memo:(on-cork-flow:peer-core bone) bone [%plea plea])
            ::  +on-kroc: cork all stale flows from failed subscriptions
            ::
            ++  on-kroc
              |=  bones=(list [ship bone])
              ^+  event-core
              %+  roll  bones
              |=  [[=ship =bone] co=_event-core]
              (%*(on-cork co cork-bone `bone) ship)
            ::  +on-deep: deferred %ames calls from itself
            ::
            ++  on-deep
              |=  =deep
              ^+  event-core
              ::  currently $deep tasks are all focused on a
              ::  particular ship but future ones might not
              ::
              ?>  ?=([@ =ship *] deep)
              =/  ship-state  (~(get by peers.ames-state) ship.deep)
              ?>  ?=([~ %known *] ship-state)
              =+  peer-core=(abed-peer:pe ship.deep +.u.ship-state)
              |^  ?-  -.deep
                %nack  abet:(send-nack-trace [nack-bone message]:deep)
                %sink  abet:(sink-naxplanation [target-bone naxplanation]:deep)
                %drop  abet:(clear-nack [nack-bone message-num]:deep)
                %cork  (cork-bone bone.deep)
                %kill  (kill-bone bone.deep)
                %ahoy  (migrate-peer bone.deep)  :: XX remove bone; it's just next-bone.ossuary
              ==
              ::
              ++  migrate-peer
                |=  =bone
                =.  peer-core  (on-migrate:peer-core bone)
                ::  XX  defer migrating the peer until we can read from their
                ::  namespace that they have migrated us?
                ::  XX  requires a namespace for migrated peers
                ::
                =<  abut
                =<  abet
                (call:(abed:mi:peer-core bone) %done ok=%.y)
              ::
              ++  send-nack-trace
                |=  [=nack=bone =message]
                abet:(call:(abed:mu:peer-core nack-bone) %memo message)
              ::
              ++  sink-naxplanation
                |=  [=target=bone =naxplanation]
                abet:(call:(abed:mu:peer-core target-bone) %near naxplanation)
              ::
              ++  clear-nack
                |=  [=nack=bone =message-num]
                abet:(call:(abed:mi:peer-core nack-bone) %drop message-num)
              ::  client ames [%cork as plea] ->  server ames [sinks %cork plea],
              ::                                  pass %deep %cork task to self
              ::                                  put flow in closing (+cork-bone),
              ::                                  and give %done
              ::  sink %ack, pass %deep %kill <-  after +on-take-done, ack %cork plea
              ::  task to self, and delete the    and delete the flow in +handle-cork
              ::  flow (+kill-bone)
              ::
              ::
              ++  cork-bone
                |=  =bone
                =~(abet:(on-cork-flow:peer-core bone) (emit duct %give %done ~))
              ::
              ++  kill-bone  |=(=bone abet:(on-kill-flow:peer-core bone))
              --
            ::  +on-stun: poke %ping app when hearing a STUN response
            ::
            ++  on-stun
              |=  =stun
              ^+  event-core
              %-  %^  ev-trace  sun.veb  ship.stun
                  =/  lane=tape
                    ?:  &
                      ::  turn off until correct parsing ip/port in ames.c
                      ::  (see https://github.com/urbit/vere/pull/623)
                      ""
                    ?:  ?=(%& -.lane.stun)
                      "from {<p.lane.stun>}"
                    =,  lane.stun
                    =/  ip=@if  (end [0 32] p)
                    =/  pt=@ud  (cut 0 [32 16] p)
                    "lane {(scow %if ip)}:{((d-co:co 1) pt)} ({(scow %ux p)})"
                  |.("inject %stun {<-.stun>} {lane}")
              %-  emit
              %^  poke-ping-app  unix-duct.ames-state  our
              ?.  ?=(%fail -.stun)  -.stun
              [%kick fail=%.y]
            :: +set-dead-flow-timer: set dead flow timer and corresponding ames state
            ::
            ++  set-dead-flow-timer
              ^+  event-core
              =.  flow.dead.ames-state.event-core
                flow/`[~[/ames] /dead-flow `@da`(add now ~m2)]
              (emit ~[/ames] %pass /dead-flow %b %wait `@da`(add now ~m2))
            :: +wake-dead-flows: call on-wake on all dead flows, discarding any
            ::                   ames-state changes
            ::
            ++  wake-dead-flows
              |=  error=(unit tang)
              ^+  event-core
              %-  ~(rep by peers.ames-state:event-core)
              |=  [[=ship =ship-state] core=_event-core]
              ^+  event-core
              =/  peer-state=(unit peer-state)  (get-peer-state:core ship)
              ?~  peer-state  core
              =/  peer-core  (abed-peer:pe:core ship u.peer-state)
              =<  abort
              ^+  peer-core
              %-  ~(rep by snd.u.peer-state)
              |=  [[=bone =message-pump-state] cor=_peer-core]
              ?.  ?&  =(~m2 rto.metrics.packet-pump-state.message-pump-state)
                      ?=(^ next-wake.packet-pump-state.message-pump-state)
                  ==
                cor
              (on-wake:cor bone error)
            ::  +on-take-wake: receive wakeup or error notification from behn
            ::
            ++  on-take-wake
              |=  [=wire error=(unit tang)]
              ^+  event-core
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
              ?:  ?=([%dead-flow ~] wire)
                set-dead-flow-timer:(wake-dead-flows error)
              ::
              ?.  ?=([%recork ~] wire)
                =/  res=(unit ?([%fine her=ship =^wire] [%pump her=ship =bone]))
                  ?+  wire  ~
                    [%pump ship=@ bone=@ ~]  (parse-pump-wire &2.wire &3.wire)
                    [%fine %behn %wake @ *]  (parse-fine-wire &4.wire t.t.t.t.wire)
                  ==
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
                =/  peer-core  (abed-peer:pe her.u.res u.state)
                ?-  -.u.res
                  %pump  abet:(on-wake:peer-core bone.u.res error)
                  ::
                    %fine
                  ?.  (~(has by keens.peer-state.peer-core) wire.u.res)
                    event-core
                  abet:fi-abet:fi-take-wake:(abed:fi:peer-core wire.u.res)
                ==
              ::
              =.  event-core  (emit duct %pass /recork %b %wait `@da`(add now ~d1))
              =.  cork.dead.ames-state
                cork/`[~[/ames] /recork `@da`(add now ~d1)]
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
              $(pez t.pez, event-core abet:recork-one:(abed-peer:pe her +.sat))
            ::  XX  only call sy-init?
            ::  mesa touches chums=(map ship chum-state) for the public keys of the ships;
            ::  ames touches peers=(map ship peer-state). Each core is called based
            ::  on the wire (mesa tags wires with /mesa)
            ::  If mesa is always call for newly booted ships, and chums is always used
            ::  we need a way to deal with version negotiation
            ::
            ::  +on-trim: handle request to free memory
            ::
            ::  %ruin comets not seen for six months
            ::
            ++  on-trim    ::TODO  trim fine parts on high prio
              ^+  event-core
              =;  rui=(set @p)
                (emit duct %pass /ruin %j %ruin rui)
              =-  (silt (turn - ^head))  ::  XX
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
            ++  on-keen
              |=  [sec=(unit [idx=@ key=@]) spar]
              ^+  event-core
              =+  ~:(spit path)  ::  assert length
              =/  ship-state  (~(get by peers.ames-state) ship)
              ?:  ?=([~ %known *] ship-state)
                ?~  sec
                  abet:(on-keen:(abed-peer:pe ship +.u.ship-state) path duct)
                =.  chain.u.ship-state  (put:on:chain chain.u.ship-state [idx key /]:u.sec)
                =.  peers.ames-state  (~(put by peers.ames-state) ship u.ship-state)
                =/  enc
                  (scot %uv (en:crub:crypto key.u.sec (spat path)))
                =/  lav  /a/x/1//fine/shut/(scot %ud idx.u.sec)/[enc]
                =/  wir  /fine/shut/(scot %ud idx.u.sec)
                (emit duct %pass wir %a %keen ~ ship lav)
              :: XX: key exchange over ames forces all encrypted scries to be
              :: to a known peer
              ?>  ?=(~ sec)
              %^  enqueue-alien-todo  ship  ship-state
              |=  todos=alien-agenda
              todos(keens (~(put ju keens.todos) path duct))
            ::
            ++  on-chum
              |=  spar
              ^+  event-core
              =/  ship-state  (~(get by peers.ames-state) ship)
              ?.  ?=([~ %known *] ship-state)
                %^  enqueue-alien-todo  ship  ship-state
                |=  todos=alien-agenda
                todos(chums (~(put ju chums.todos) path duct))
              =/  cyf
                (scot %uv (en:crub:crypto symmetric-key.u.ship-state (spat path)))
              =/  lav
                /a/x/1//chum/(scot %p our)/(scot %ud life.ames-state)/[cyf]
              (emit duct [%pass /chum %a %keen ~ ship lav])
            ::
            ++  on-cancel-scry
              |=  [all=? spar]
              ^+  event-core
              ?~  ship-state=(~(get by peers.ames-state) ship)
                ~&(%cancel-scry-missing-peer^ship^path event-core)
              ?.  ?=([~ %known *] ship-state)
                :: XX delete from alien agenda?
                %.  event-core
                %^  trace  vane  fin.veb
                [ship ships.bug.ames-state |.("peer still alien, skip cancel-scry")]
              =+  peer=(abed:pe ship)
              ?.  (~(has by keens.peer-state.peer) path)
                event-core
              abet:fi-abet:(fi-unsub:(abed:fi:peer path) duct all)
            ::
            +|  %migration-entry-points
            ::
            ++  on-mate
              |=  ship=(unit ship)
              |^  ^+  event-core
              ?^  ship
                ?~  peer=(~(get by peers.ames-state) u.ship)
                  event-core
                (migrate-peer u.ship u.peer)
              %-  ~(rep by peers.ames-state)
              |=  [[=^ship state=ship-state] core=_event-core]
              ?:  ?=(%alien -.state)  core
              (migrate-peer ship state)
              ::
              ++  migrate-peer
                |=  [=^ship =ship-state]
                ^+  event-core
                ?>  ?=([%known *] ship-state)
                =+  pe-core=(abed-peer:pe ship +.ship-state)
                abut:(on-migrate:pe-core next-bone.ossuary.peer-state.pe-core)  :: XX (dec next-bone)
              ::
              --
            ::
            +|  %implementation
            ::  +enqueue-alien-todo: helper to enqueue a pending request
            ::
            ::    Also requests key and life from Jael on first request.
            ::    If talking to a comet, requests attestation packet.
            ::
            ++  enqueue-alien-todo
              |=  $:  =ship
                      ship-state=(unit ship-state)
                      mutate=$-(alien-agenda alien-agenda)
                  ==
              ^+  event-core
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
              =+  (ev-trace msg.veb ship |.("requesting attestion"))
              =.  event-core
                =/  =blob  (sendkeys-packet ship)
                (send-blob for=| ship blob (~(get by peers.ames-state) ship))
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
              |=  [for=? =ship =blob ship-state=(unit ship-state)]
              ::
              =/  final-ship  ship
              %-  (ev-trace rot.veb final-ship |.("send-blob: to {<ship>}"))
              |-
              |^  ^+  event-core
                  ?.  ?=([~ %known *] ship-state)
                    ?:  ?=(%pawn (clan:title ship))
                      (try-next-sponsor (^sein:title ship))
                    %^  enqueue-alien-todo  ship  ship-state
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
                    %-  (ev-trace rot.veb final-ship |.("no route to:  {<ship>}"))
                    (try-next-sponsor sponsor.peer-state)
                  ::
                  %-  (ev-trace rot.veb final-ship |.("trying route: {<ship>}"))
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
                ^$(ship sponsor, ship-state (~(get by peers.ames-state) sponsor))
              --
            ::  +attestation-packet: generate signed self-attestation for .her
            ::
            ::    Sent by a comet on first contact with a peer.  Not acked.
            ::
            ++  attestation-packet
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
            ::  +sendkeys-packet: generate a request for a self-attestation.
            ::
            ::    Sent by non-comets to comets.  Not acked.
            ::
            ++  sendkeys-packet
              |=  her=ship
              ^-  blob
              ?>  ?=(%pawn (clan:title her))
              %-  etch-shot
              (encode-keys-packet our her life.ames-state)
            ::
            +|  %internals
            ::  +pe: create nested |peer-core for per-peer processing
            ::
            ++  pe
              |_  [=peer-state =channel]
              +*  veb    veb.bug.channel
                  her    her.channel
                  keens  keens.peer-state
              ::
              +|  %helpers
              ::
              ++  peer-core  .
              ++  pe-emit   |=(move peer-core(event-core (emit +<)))
              ++  abed      |=(=ship (abed-peer ship (gut-peer-state ship)))
              ++  abed-got  |=(=ship (abed-peer ship (got-peer-state ship)))
              ++  abed-peer
                |=  [=ship peer=^peer-state]
                %_  peer-core
                  peer-state  peer
                    channel  [[our ship] now channel-state -.peer]
                ==
              ::
              ++  abort  event-core  :: keeps moves, discards state changes
              ++  abet
                ^+  event-core
                =.  peers.ames-state
                  (~(put by peers.ames-state) her %known peer-state)
                event-core
              ::
              ++  abut
                ^+  event-core
                =.  peers.ames-state  (~(del by peers.ames-state) her)
                event-core
              ::
              ++  pe-trace
                |=  [verb=? print=(trap tape)]
                ^+  same
                (ev-trace verb her print)
              ::
              ::  +got-duct: look up $duct by .bone, asserting already bound
              ::
              ++  got-duct
                |=  =bone
                ^-  ^duct
                ~|  %dangling-bone^her^bone
                (~(got by by-bone.ossuary.peer-state) bone)
              ::
              ::  +bind-duct: find or make new $bone for .duct in .ossuary
              ::
              ++  bind-duct
                |=  =^duct
                =*  ossa  ossuary.peer-state
                ^+  [next-bone.ossa peer-core]
                ?^  existing=(~(get by by-duct.ossa) duct)
                  [u.existing peer-core]
                :-  next-bone.ossa
                =.  ossa
                  :+  (add 4 next-bone.ossa)
                    (~(put by by-duct.ossa) duct next-bone.ossa)
                  (~(put by by-bone.ossa) next-bone.ossa duct)
                peer-core
              ::
              ++  is-corked
                |=  =bone
                ?|  (~(has in corked.peer-state) bone)
                    ?&  =(1 (end 0 bone))
                        =(1 (end 0 (rsh 0 bone)))
                        (~(has in corked.peer-state) (mix 0b10 bone))
                ==  ==
              ::
              +|  %tasks
              ::  +update-qos: update and maybe print connection status
              ::
              ++  update-qos
                |=  [mode=?(%ames %fine) =new=qos]
                ^+  peer-core
                ::
                =^  old-qos  qos.peer-state  [qos.peer-state new-qos]
                ::  if no update worth reporting, we're done
                ::
                =/  text
                  %^  qos-update-text  her  mode
                  [old-qos new-qos kay.veb ships.bug.ames-state]
                ?~  text
                  peer-core
                ::  print message
                ::
                (pe-emit duct %pass /qos %d %flog %text u.text)
              ::  +on-hear-shut-packet: handle receipt of ack or message fragment
              ::
              ++  on-hear-shut-packet
                |=  [=lane =shut-packet dud=(unit goof)]
                ^+  peer-core
                ::  update and print connection status
                ::
                =.  peer-core  (update-qos %ames %live last-contact=now)
                ::
                =/  =bone  bone.shut-packet
                ::
                ?:  ?=(%& -.meat.shut-packet)
                  =+  ?.  &(?=(^ dud) msg.veb)  ~
                      %.  ~
                      %-  slog
                      :_  tang.u.dud
                      leaf+"ames: {<her>} fragment crashed {<mote.u.dud>}"
                  abet:(call:(abed:mi bone) %hear lane shut-packet ?=(~ dud))
                ::  benign ack on corked bone
                ::
                ?:  (is-corked bone)  peer-core
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
                abet:(call:(abed:mu bone) %hear [message-num +.meat]:shut-packet)
              ::
              ++  on-flub
                |=  =bone
                ^+  peer-core
                abet:(call:(abed:mi:peer-core bone) %flub ~)
              ::
              ++  check-clog
                |=  [=bone id=*]
                ^+  peer-core
                =/  =message-pump-state  (~(got by snd.peer-state) bone)
                ?:  (gth ~(wyt in unsent-messages.message-pump-state) msg.cong.ames-state)
                  (pe-emit [/ames]~ %pass /clog %g %clog id)
                peer-core
              ::  +on-memo: handle request to send message
              ::
              ++  on-memo
                |=  [=bone =message]
                ^+  peer-core
                ?:  ?&  (~(has in closing.peer-state) bone)
                        !=(message [%plea %$ /flow %cork ~])
                    ==
                  ~>  %slog.0^leaf/"ames: ignoring message on closing bone {<bone>}"
                  peer-core
                ?:  (~(has in corked.peer-state) bone)
                  ~>  %slog.0^leaf/"ames: ignoring message on corked bone {<bone>}"
                  peer-core
                ::
                abet:(call:(abed:mu bone) %memo message)
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
                    (pe-emit duct %pass /wake-fail %d %flog %crud %ames-wake u.error)
                  ::
                  ?~  message-pump-state=(~(get by snd.peer-state) bone)
                    peer-core
                  =*  packet-state  packet-pump-state.u.message-pump-state
                  ?~  next-wake.packet-state  peer-core
                  ::  If we crashed because we woke up too early, assume another
                  ::  timer is already set.
                  ::
                  ?:  (lth now.channel u.next-wake.packet-state)
                    peer-core
                  ::
                  =/  =wire  (make-pump-timer-wire her bone)
                  (pe-emit duct %pass wire %b %wait (add now.channel ~s30))
                ::  update and print connection state
                ::
                =.  peer-core   (update-qos %ames qos:(is-peer-dead now peer-state))
                ::  expire direct route if the peer is not responding
                ::
                =/  old-route  route.peer-state
                =.  peer-state  (update-peer-route her peer-state)
                =?  peer-core  !=(old-route route.peer-state)
                  %-  pe-emit
                  :*  unix-duct.ames-state  %give  %nail  her
                      (get-forward-lanes our peer-state peers.ames-state)
                  ==
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
                  =/  =blob  (attestation-packet [her her-life]:channel)
                  (send-blob for=| her blob `known/peer-state)
                ?:  (is-corked bone)
                  ::  no-op if the bone (or, if a naxplanation, the reference bone)
                  ::  was corked, because the flow doesn't exist anymore
                  ::  TODO: clean up corked bones?
                  ::
                  peer-core
                ::  maybe resend some timed out packets
                ::
                abet:(call:(abed:mu bone) %wake ~)
              ::
              ++  on-hear-fine
                |=  [=lane =shot]
                ^+  peer-core
                ?>  =(sndr-tick.shot (mod life.peer-state 16))
                ::  TODO what if the error happened in sift-purr?
                ::       does vere discard malformed packets?
                =/  [=peep =meow]  (sift-purr `@ux`content.shot)
                =/  =path  (slag 3 path.peep)
                ::
                ?.  (~(has by keens) path)
                  %-  (fi-trace:fi fin.veb |.("dead-response {path}"))
                  peer-core
                fi-abet:(fi-rcv:(abed:fi path) peep meow lane)
              ::
              ++  on-keen
                |=  [=path =^duct]
                ^+  peer-core
                ?:  (~(has by keens) path)
                  ::  TODO use fi-trace
                  ~>  %slog.0^leaf/"fine: dupe {(spud path)}"
                  fi-abet:(fi-sub:(abed:fi path) duct)
                =.  keens  (~(put by keens) path *keen-state)
                fi-abet:(fi-start:(abed:fi path) duct)
              ::
              ::  +on-cork-flow: mark .bone as closing
              ::
              ++  on-cork-flow
                |=  =bone
                ^+  peer-core
                peer-core(closing.peer-state (~(put in closing.peer-state) bone))
              ::  +on-kill-flow: delete flow on cork sender side
              ::
              ++  on-kill-flow
                |=  =bone
                ^+  peer-core
                ?:  (~(has in corked.peer-state) bone)
                  ~>  %slog.0^leaf/"ames: ignoring kill on corked bone {<bone>}"
                  peer-core
                =.  peer-state
                  =,  peer-state
                  %_  peer-state
                    ::  if the publisher was behind, preemptively remove any nacks
                    ::
                    rcv              (~(del by (~(del by rcv) bone)) (mix 0b10 bone))
                    snd              (~(del by snd) bone)
                    corked           (~(put in corked) bone)
                    closing          (~(del in closing) bone)
                    by-duct.ossuary  (~(del by by-duct.ossuary) (got-duct bone))
                    by-bone.ossuary  (~(del by by-bone.ossuary) bone)
                  ==
                ::  since we got one cork ack, try the next one
                ::
                recork-one
              ::
              ++  on-migrate
                |=  =bone  :: XX do something with bone? save high water mark
                           :: XX remove bone; it's just next-bone.ossuary
                ^+  peer-core
                =|  fren=fren-state
                |^  =:        -.fren  azimuth-state=-.peer-state
                           lane.fren  get-lane
                            qos.fren  qos.peer-state
                         corked.fren  divide-bones
                        ossuary.fren  align-bones
                   client-chain.fren  chain.peer-state
                  ==
                =^  poke-moves  flows.fren  (make-flows fren)
                =^  peek-moves  ames-state  (make-peeks fren)
                ::  XX  needed?  peek/poke-moves will have %send moves already
                ::  enqueue a %prod to start sending unsent messages, after
                ::  all the %mokes (which trigger +peeks for %acks) have been
                ::  processed
                ::
                =/  prod-move=(list move)  [[/ames]~ %pass /mate %a %prod ~]~
                ::  .her is fully migrated, +abul will delete it from peers.
                ::
                peer-core(event-core (emil (weld poke-moves peek-moves)))
                ::
                ++  align-bones
                  ^+  ossuary.peer-state
                  ::  XX update ossuary in terms of [bone=@ud dire=?(%for %bak)]
                  ::
                  ossuary.peer-state
                ::
                ++  divide-bones
                  ^-  (set side)
                  %-  ~(rep in corked.peer-state)
                  |=  [=^bone corked=_corked.fren]
                  (~(put in corked) bone ?:(=(%0 (mod bone 4)) %for %bak))
                ::
                ++  make-flows
                  |=  fren=fren-state
                  ^-  (quip move (map side flow-state))
                  ::  forward flows
                  ::
                  =^  moves  flows.fren
                    %-  ~(rep by snd.peer-state)
                    |=  $:  [=^bone pump=message-pump-state]
                            moves=(list move)
                            flows=_flows.fren
                        ==
                    =|  flow=flow-state
                    =/  =dire
                       ?:  =(%0 (mod bone 4))  %for  :: sending %plea(s)
                       %bak  ::  sending boon(s) and naxplanation(s)
                    ::
                    =?  bone  =(%1 (mod bone 4))
                      (mix 0b1 bone)
                    =.    closing.flow  (~(has in closing.peer-state) bone)
                    =.  next-load.flow  next.pump
                    ::  initialize fo-core
                    ::
                    =/  fo-core
                      =/  =^duct
                        (~(gut by by-bone.ossuary.peer-state) bone [/ames]~)
                      =/  core
                        %*  .  fo:~(ev-core mesa [duct her^fren])
                          flows.sat.per  (~(put by flows) bone^dire flow)  :: XX check that we don't add naxplanation flows here
                        ==
                      (fo-abed:core duct bone dire)
                    =?  moves  !=(current.pump next.pump)
                      =*  live  live.packet-pump-state.pump
                      =/  current-live=?
                        %-  ~(rep by live)
                        |=  [[live-packet-key *] has=_|]
                        =(message-num current.pump)
                      ?:  current-live  moves
                      ::  we are still expecting an ack or a naxplanation for
                      ::  the current message. if there packet-pump has not
                      ::  state about current.pump, it means that we have heard
                      ::  the %nack, and clear everything, but deffered
                      ::  incrementing current until the naxplanation arrives.
                      ::
                      ::  the sender of the naxplanation will have bind it in
                      ::  their namespace, so we start +peeking it
                      ::
                      %+  weld  moves
                      moves:(fo-peek-naxplanation:fo-core current.pump)
                    ::
                    ::  live packets in packet-pump-state are reconstructed; the
                    ::  receiver will droppped any partially received fragments
                    ::  so the full message will need to be resent.
                    ::
                    =/  live=(list message)
                      =+  queue=((on ,@ud partial-rcv-message) lte)
                      =;  fragments
                        %-  flop
                        %+  roll  (tap:queue fragments)
                        |=  [[* partial-rcv-message] total=(list message)]
                        :_  total
                        ;;  message
                        :_  (assemble-fragments num-fragments fragments)
                        ?:  =(%0 (mod bone 4))  %plea
                        ?:  =(%1 (mod bone 4))  %boon
                        ?>  =(%3 (mod bone 4))  %naxplanation
                      |^
                      =/  unsent=((mop ,@ud partial-rcv-message) lte)
                        %+  roll  unsent-fragments.pump
                        |=  $:  static-fragment
                                live=((mop ,@ud partial-rcv-message) lte)
                            ==
                        ~|  [seq=message-num current=current.pump]
                        ?>  =(message-num current.pump)  :: XX
                        %-  acc-fragments
                        [message-num num-fragments fragment-num fragment live]
                      ::  fragments of the current message could be in unsent
                      ::  (rejected by the packet pump due to congestion) but
                      ::  some could be live, so we need to get those before
                      ::  assembling.
                      ::
                      %+  roll
                        (tap:packet-queue:$:pu:mu live.packet-pump-state.pump)
                      |=  [[live-packet-key live-packet-val] acc=_unsent]
                      %-  acc-fragments
                      [message-num num-fragments fragment-num fragment acc]
                      ::
                      ++  acc-fragments
                        |=  $:  seq=@ud  n-fags=@ud  n-fag=@ud  fag=fragment
                                acc=((mop ,@ud partial-rcv-message) lte)
                            ==
                        =/  fags  ?~(val=(get:queue acc seq) ~ fragments:u.val)
                        %+  put:queue  acc
                        [seq n-fags recv=*@ud (~(put by fags) n-fag fag)]
                      ::
                      --
                    ::
                    =^  forward-moves  flow
                      =;  [* core=_fo-core]
                        [moves state]:core
                      %+  roll  (weld live ~(tap to unsent-messages.pump))
                      ::
                      ::  XX remove current?
                      ::
                      |=  [=message current=_current.pump core=_fo-core]
                      :-  +(current)
                      ?.  ?=(%naxplanation -.message)
                        (fo-call:core message)
                      ::  if we are still sending a %naxplanation, we need to
                      ::  put it in our namespace so the other ship reads it
                      ::
                      %_    fo-core
                          nax.state
                        %-  ~(put by nax.state.core)
                        [message-num error]:message
                      ==
                    ::
                    :_  (~(put by flows) [bone dire] flow)
                    =.  moves  (weld forward-moves moves)
                    =?  moves  ?=(^ next-wake.packet-pump-state.pump)
                      =*  wake  u.next-wake.packet-pump-state.pump
                      :_  moves  ^-  move
                      :-  [/ames]~
                      [%pass (make-pump-timer-wire her bone) %b %rest wake]
                    moves
                  ::  backward flows
                  ::
                  =.  flows.fren
                    ::  XX the %ahoy flow is not migrated properly since
                    ::     at this point we have not acked it yet
                    ::
                    %-  ~(rep by rcv.peer-state)
                    |=  [[=^bone sink=message-sink-state] flows=_flows.fren]
                    ::  drop any partially received messages in live-messages
                    ::
                    ::  if this was a naxplanation bone but we haven't finished
                    ::  sink it, also drop it. the message pump has enough
                    ::  information to know that we need to start +peeking it.
                    ::
                    ?:  =(%2 (mod bone 4))
                      ::  %naxplanation %ack on receiver; skip bone
                      flows
                    =/  =dire
                       ?:  =(%0 (mod bone 4))  %for  :: receiving %boon(s)
                       %bak  ::  receiving plea(s)
                    =/  flow=flow-state
                      =?  bone  =(%1 (mod bone 4))
                        (mix 0b1 bone)
                      (~(gut by flows) bone^dire *flow-state)
                    =:      closing.flow  (~(has in closing.peer-state) bone)
                               line.flow  last-acked.sink
                         last-acked.flow  last-acked.sink
                        ::  XX if there's a pending-vane ack it should have
                        ::  been sent to the vane already?
                        ::  drop any pending ack
                        ::
                        pending-ack.flow  %.n  ::  ?=(^ pending-vane-ack.sink)
                      ::
                          nax.flow
                        %-  ~(gas by *_nax.flow)
                        ::  XX  when a message in nacked (e.g. 25), we add it to
                        ::  nax.sink and create a %naxplanation message (e.g. 1
                        ::  first ever naxplanation sent) that contains the
                        ::  nacked sequence number (25), and is sent to the
                        ::  original sender on the naxplanation flow.
                        ::
                        ::  when the ack for the naxplanation (1) comes back, we
                        ::  know that this was an ack for a naxplanation bone
                        ::  and then drop the sequence number of _this_
                        ::  naxplanation message (1) instead of the original
                        ::  message (25) that was nacked.
                        ::
                        ::  this causes a gap or buffer of messages in nax.sink
                        ::  that will remain there until naxplanations catch
                        ::  up with the actual message numbers from the
                        ::  reference flow.
                        ::
                        ::  we are removing messages that might not have been
                        ::  nacked, but that seems ok since the message won't be
                        ::  there anyway.
                        ::
                        ::  for the migration, this could mean that there are
                        ::  gaps in the sequence of messages that we have nacked
                        ::  so the fact that there are messages in nax.sink
                        ::  doesn't mean that we can say: "if a sequence number
                        ::  higher than what's in the queue is not in nax.sink,
                        ::  we can we assume that it has not been nacked?".
                        ::
                        ::  Because of this is better to draw a "line" by adding
                        ::  last-acked.sink to line.flow and dropping any reads
                        ::  to acks older than that sequence number, and not
                        ::  migrate anything in nax.sink.
                        ::
                        =-  ~
                        ::  if there are entries in nax/sink, we have nacked a
                        ::  plea/boon, but we were waiting on the naxplanation.
                        ::
                        ::  naxplanations are not sent anymore, just exposed
                        ::  in the namespace.
                        ::
                        ::  peeks for naxplanation won't be triggered in the
                        ::  migration but rather from the re-send of the orginal
                        ::  messages in the message pump
                        ::
                        (turn ~(tap in nax.sink) (late *error))
                      ==
                    =?  bone  =(%1 (mod bone 4))
                      (mix 0b1 bone)
                    (~(put by flows) bone^dire flow)
                  ::  naxplanations
                  ::  XX  check that this is true
                  ::  XX entries in nax.peer-state have not been used
                  ::
                  moves^flows.fren
                ::
                ++  make-peeks
                  |=  fren=fren-state
                  ^-  (quip move axle)
                  =/  ev-core
                    =/  chums  (~(put by chums.ames-state) her known/fren)
                    %*  ev-core  mesa
                      sat.per           fren
                      chums.ames-state  chums
                    ==
                  =*  per  peer-state
                  =<  ev-abet
                  ^+  ev-core
                  %-  ~(rep by keens.per)
                  |=  [[=path keen=keen-state] core=_ev-core]
                  =|  req=request-state
                  =>  .(path `(pole knot)`path)
                  ~|  make-peeks-crashed/path
                  ?.  ?=([van=@ car=@ cas=@ desk=@ pat=*] path)
                    :: XX validate van, car, cas, desk ?
                    ::
                    ~&  skip-weird-path/path  core
                  =;  [pax=^path =space]
                    =.  pax  (ev-make-path:core space pax)
                    %-  ~(rep in listeners.keen)
                    |=  [=^duct core=_core]
                    (ev-make-peek:core(hen duct) space her pax)
                  ::  XX unitize this and no-op if failure to convert
                  ::
                  ?+    pat.path  [pat.path [%publ life.per]]
                    ::
                      [%fine %shut idx=@ cyf=@]
                    =/  idx=@ud    (slav %ud idx.pat.path)
                    =/  cyf=@      (slav %ud cyf.pat.path)
                    =/  key=@      key:(got:on:chain server-chain.ames-state idx)
                    =/  pax=^path  (rash `@t`(dy:crub:crypto key cyf) stap)
                    [pax %shut idx key]
                    ::
                      [%chum her=@ lyf=@ cyf=@]
                    =/  cyf=@      (slav %ud cyf.pat.path)
                    =*  key        symmetric-key.per
                    =/  pax=^path  (rash `@t`(dy:crub:crypto key cyf) stap)
                    [pax %chum life.ames-state her life.per key]
                  ==
                ::
                ++  get-lane
                  ^-  (unit lane:pact)
                  ?~  route.peer-state  ~
                  ?.  direct.u.route.peer-state  ~
                  =*  lane  lane.u.route.peer-state
                  %-  some
                  ?-  -.lane
                    %&  `@ux`p.lane  ::  galaxy
                  ::
                      %|
                    :+    %if
                      ip=`@if`(end [0 32] p.lane)
                    pt=`@ud`(cut 0 [32 16] p.lane)
                  ==
                ::
                --
              ::
              +|  %implementation
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
                  %:  send-blob  for=|  her
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
                  ~>  %slog.0^leaf/"ames: bad pump state {<her i.boz>}"
                  $(boz t.boz)
                ::  no outstanding messages, so send a new %cork
                ::
                ::  TODO use +trace
                ~>  %slog.0^leaf/"ames: recork {<her i.boz>}"
                =/  =plea     [%$ /flow [%cork ~]]
                =/  =message  [%plea plea]
                (on-memo i.boz message)
              ::  +handle-cork: handle flow kill after server ames has taken %done
              ::
              ++  handle-cork
                |=  =bone
                |^  ^+  peer-core
                ?.  (~(has in closing.peer-state) bone)  peer-core
                =/  pump=message-pump-state
                  (~(gut by snd.peer-state) bone *message-pump-state)
                =?  event-core  ?=(^ next-wake.packet-pump-state.pump)
                  ::  reset-timer for boons
                  ::
                  (reset-timer her bone u.next-wake.packet-pump-state.pump)
                =/  nax-bone=^bone  (mix 0b10 bone)
                =/  nax-pump=message-pump-state
                  (~(gut by snd.peer-state) nax-bone *message-pump-state)
                =?  event-core  ?=(^ next-wake.packet-pump-state.nax-pump)
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
                peer-core
                ::
                ++  reset-timer
                  |=  [=ship =^bone wake=@da]
                  (emit [/ames]~ %pass (make-pump-timer-wire ship bone) %b %rest wake)
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
                ++  pump  .
                ++  abed
                  |=  b=^bone
                  pump(bone b, state (~(gut by snd.peer-state) b *message-pump-state))
                ++  abet
                  ::  if the bone was corked, it's been removed from the state,
                  ::  so we avoid adding it again.
                  ::
                  =?  snd.peer-state  !corked  (~(put by snd.peer-state) bone state)
                  peer-core
                ::
                ++  packet-pump  (pu packet-pump-state.state)
                ++  closing      (~(has in closing.peer-state) bone)
                ++  corked       (~(has in corked.peer-state) bone)
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
                ::
                +|  %entry-points
                ::  +call: handle a $message-pump-task
                ::
                ++  call
                  |=  task=message-pump-task
                  ^+  pump
                  ::
                  =.  pump  =~((dispatch-task task) feed-packets)
                  =+  top=top-live:packet-pump
                  ::  sanity check to isolate error cases
                  ::
                  ?.  |(?=(~ top) (lte current.state message-num.key.u.top))
                    ~|([%strange-current current=current.state key.u.top] !!)
                  ::  maybe trigger a timer based on congestion control calculations
                  ::
                  abet:(call:packet-pump %halt ~)
                ::
                +|  %tasks
                ::  +dispatch-task: perform task-specific processing
                ::
                ++  dispatch-task
                  |=  task=message-pump-task
                  ^+  pump
                  ::
                  ?-  -.task
                    %memo  (on-memo message.task)
                    %prod  abet:(call:packet-pump %prod ~)
                    %wake  abet:(call:packet-pump %wake current.state)
                    %near  %-  on-done
                          [[message-num %naxplanation error]:naxplanation.task %&]
                    %hear
                      ?-    -.ack-meat.task
                          %&
                      (on-hear [message-num fragment-num=p.ack-meat]:task)
                      ::
                          %|
                        =/  cork=?
                          =+  top=top-live:packet-pump
                          ::  If we send a %cork and get an ack, we can know by
                          ::  sequence number that the ack is for the %cork message
                          ::
                          ?&  closing
                              ?=(^ top)
                              =(0 ~(wyt in unsent-messages.state))
                              =(0 (lent unsent-fragments.state))
                              =(1 ~(wyt by live.packet-pump-state.state))
                              =(message-num:task message-num.key.u.top)
                          ==
                        =+  [ack msg]=[p.ack-meat message-num]:task
                        =.  pump
                          %-  on-done
                          [[msg ?:(ok.ack [%ok ~] [%nack ~])] cork]
                        ?.  &(!ok.ack cork)  pump
                        %.  pump
                        %+  pe-trace  odd.veb
                        |.("got nack for %cork {<bone=bone message-num=msg>}")
                  ==  ==
                ::  +on-memo: handle request to send a message
                ::
                ++  on-memo
                  |=  =message
                  pump(unsent-messages.state (~(put to unsent-messages.state) message))
                ::  +on-hear: handle packet acknowledgment
                ::
                ++  on-hear
                  |=  [=message-num =fragment-num]
                  ^+  pump
                  ::  pass to |packet-pump unless duplicate or future ack
                  ::
                  ?.  (is-message-num-in-range message-num)
                    %.  pump
                    (pe-trace snd.veb |.("hear pump out of range"))
                  abet:(call:packet-pump %hear message-num fragment-num)
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
                  ^+  pump
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
                    %.  pump
                    %+  pe-trace  snd.veb  |.
                    "duplicate done {<current=current.state message-num=message-num>}"
                  ::  ignore duplicate and future acks
                  ::
                  ?.  (is-message-num-in-range message-num)
                    pump
                  ::  clear and print .unsent-fragments if nonempty
                  ::
                  =?    unsent-fragments.state
                      &(=(current next) ?=(^ unsent-fragments)):state
                    ::
                    ~>  %slog.0^leaf/"ames: early message ack {<her>}"
                    ~
                  ::  clear all packets from this message from the packet pump
                  ::
                  =.  pump  abet:(call:packet-pump %done message-num lag=*@dr)
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
                  |-  ^+  pump
                  ::  if .current hasn't been fully acked, we're done
                  ::
                  ?~  cur=(~(get by queued-message-acks.state) current.state)
                    pump
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
                  =.  pump  abet:(call:packet-pump %done current.state lag=*@dr)
                  ::  give %done to vane if we're ready
                  ::
                  ?-    -.u.cur
                      %ok
                    =.  peer-core
                      ::  don't give %done for corks
                      ::
                      ?:  cork  (pump-cork current.state)
                      (pump-done current.state ~)
                    $(current.state +(current.state))
                  ::
                      %nack  pump
                  ::
                      %naxplanation
                    =.  peer-core  (pump-done current.state `error.u.cur)
                    $(current.state +(current.state))
                  ==
                ::
                +|  %implementation
                ::  +feed-packets: give packets to |packet-pump until full
                ::
                ++  feed-packets
                  ::  if nothing to send, no-op
                  ::
                  ?:  &(=(~ unsent-messages) =(~ unsent-fragments)):state
                    pump
                  ::  we have unsent fragments of the current message; feed them
                  ::
                  ?.  =(~ unsent-fragments.state)
                    ::  we have unsent fragments of the current message; feed them
                    ::
                    =^  unsent  pump  abut:(feed:packet-pump unsent-fragments.state)
                    =.  unsent-fragments.state   unsent
                    ::  if it sent all of them, feed it more; otherwise, we're done
                    ::
                    ?~(unsent feed-packets pump)
                  ::  .unsent-messages is nonempty; pop a message off and feed it
                  ::
                  =^  =message  unsent-messages.state
                    ~(get to unsent-messages.state)
                  ::  break .message into .chunks and set as .unsent-fragments
                  ::
                  =.  unsent-fragments.state  (split-message next.state (jim +.message))
                  ::  try to feed packets from the next message
                  ::
                  =.  next.state  +(next.state)
                  feed-packets
                ::  +pump-done: handle |message-pump's report of message (n)ack
                ::
                ++  pump-done
                  |=  [=message-num error=(unit error)]
                  ^+  peer-core
                  ?:  ?&  =(1 (end 0 bone))
                          =(1 (end 0 (rsh 0 bone)))
                          (~(has in corked.peer-state) (mix 0b10 bone))
                      ==
                    %-  %+  pe-trace  msg.veb
                        =/  dat  [her bone=bone message-num=message-num -.task]
                        |.("remove naxplanation flow {<dat>}")
                    ::  we avoid re-adding the bone in abet:mu
                    ::
                    =.  snd.peer-state  (~(del by snd.peer-state) bone)
                    peer-core
                  ?:  =(1 (end 0 bone))
                    ::  ack is on "subscription update" message; no-op
                    ::
                    ?:  =(0 (end 0 (rsh 0 bone)))  peer-core
                    ::  nack-trace bone; assume .ok, clear nack from |sink
                    ::
                    %+  pe-emit  duct
                    [%pass /clear-nack %a %deep %drop her (mix 0b10 bone) message-num]
                  ::  if the bone belongs to a closing flow and we got a
                  ::  naxplanation, don't relay ack to the client vane
                  ::
                  ?:  &(closing ?=(%near -.task))  peer-core
                  ::  not a nack-trace bone; relay ack to client vane
                  ::
                  (pe-emit (got-duct bone) %give %done error)
                ::  +pump-cork: handle %cork on the publisher
                ::
                ++  pump-cork
                  |=  =message-num
                  ^+  peer-core
                  ::  clear all packets from this message from the packet pump
                  ::
                  =.  pump  abet:(call:packet-pump %done message-num lag=*@dr)
                  ?:  corked
                    %-  %+  pe-trace  odd.veb
                        |.("trying to delete a corked bone={<bone>}")
                    peer-core
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
                  ++  pack  .
                  ::  +abut: abet with gifts
                  ::
                  ++  abut  [unsent abet]
                  ++  abet  pump(packet-pump-state.state state)
                  ++  pu-trace
                    |=  [verb=? print=(trap tape)]
                    ^+  same
                    (trace %ames verb her ships.bug.channel print)
                  ::
                  ++  pu-wire  (make-pump-timer-wire her bone)
                  ++  pu-emit  |=(=note (pe-emit pump-duct %pass pu-wire note))
                  ::  +packet-queue: type for all sent fragments (order: seq number)
                  ::
                  ++  packet-queue
                    %-  (ordered-map live-packet-key live-packet-val)
                    lte-packets
                  ::  +gauge: inflate a |pump-gauge to track congestion control
                  ::
                  ++  gauge  (ga metrics.state ~(wyt by live.state))
                  ::  +to-static-fragment: convenience function for |packet-pump
                  ::
                  ++  to-static-fragment
                    |=  [live-packet-key live-packet-val]
                    ^-  static-fragment
                    [message-num num-fragments fragment-num fragment]
                  ::
                  ++  pump-duct  ~[/ames]
                  ++  top-live   (pry:packet-queue live.state)
                  ::
                  +|  %entry-points
                  ::
                  ++  call
                    |=  task=packet-pump-task
                    ^+  pack
                    ?-  -.task
                      %hear  (on-hear [message-num fragment-num]:task)
                      %done  (on-done message-num.task)
                      %wake  (on-wake current.task)
                      %prod  on-prod
                      %halt  set-wake
                    ==
                  ::  +feed: try to send a list of packets, returning unsent ones
                  ::
                  ++  feed
                    |=  fragments=(list static-fragment)
                    ^+  pack
                    ::  bite off as many fragments as we can send
                    ::
                    =/  num-slots  num-slots:gauge
                    =/  sent       (scag num-slots fragments)
                    =.  unsent     (slag num-slots fragments)
                    ::  if nothing to send, we're done
                    ::
                    ?~  sent  pack
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
                    =.  live.state  (gas:packet-queue live.state send-list)
                    ::  TMI
                    ::
                    =>  .(sent `(list static-fragment)`sent)
                    ::  emit a $shut-packet for each packet to send
                    ::
                    =.  peer-core
                      %+  roll  sent
                      |=  [packet=static-fragment core=_peer-core]
                      (send-shut-packet bone [message-num %& +]:packet)
                    pack
                  ::
                  +|  %tasks
                  ::  +on-prod: reset congestion control, re-send packets
                  ::
                  ++  on-prod
                    ^+  pack
                    ?:  =(~ next-wake.state)
                      pack
                    ::
                    =.  metrics.state
                      %*(. *pump-metrics counter counter.metrics.state)
                    =.  live.state
                      %+  run:packet-queue  live.state
                      |=(p=live-packet-val p(- *packet-state))
                    ::
                    =/  sot  (max 1 num-slots:gauge)
                    =/  liv  live.state
                    |-  ^+  pack
                    ?:  =(0 sot)  pack
                    ?:  =(~ liv)  pack
                    =^  hed  liv  (pop:packet-queue liv)
                    =.  peer-core
                      %+  send-shut-packet  bone
                      [message-num %& +]:(to-static-fragment hed)
                    $(sot (dec sot))
                  ::  +on-wake: handle packet timeout
                  ::
                  ++  on-wake
                    |=  current=message-num
                    ^+  pack
                    ::  assert temporal coherence
                    ::
                    ?<  =(~ next-wake.state)
                    =.  next-wake.state  ~
                    ::  tell congestion control a packet timed out
                    ::
                    =.  metrics.state  on-timeout:gauge
                    =|  acc=(unit static-fragment)
                    ::  re-send first packet and update its state in-place
                    ::
                    =;  [static-fragment=_acc live=_live.state]
                        =.  live.state   live
                        =?  peer-core  ?=(^ static-fragment)
                          %-  %+  pu-trace  snd.veb
                              =/  nums  [message-num fragment-num]:u.static-fragment
                              |.("dead {<nums show:gauge>}")
                          (send-shut-packet bone [message-num %& +]:u.static-fragment)
                        pack
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
                      %.  [~ stop=%.n ~]
                      %-  slog  :_  ~  :-  %leaf
                      "ames: strange wake queue, expected {<current>}, got {<key>}"
                    ::  packet has expired; update it in-place, stop, and produce it
                    ::
                    =.  last-sent.val  now.channel
                    =.  tries.val      +(tries.val)
                    ::
                    [`val stop=%.y `(to-static-fragment key val)]
                  ::  +fast-resend-after-ack: resend timed out packets
                  ::
                  ::    After we finally receive an ack, we want to resend all the
                  ::    live packets that have been building up.
                  ::
                  ++  fast-resend-after-ack
                    |=  [=message-num =fragment-num]
                    ^+  pack
                    =;  res=[resends=(list static-fragment) live=_live.state]
                      =.  live.state  live.res
                      =.  peer-core
                        %+  reel  resends.res
                        |=  [packet=static-fragment core=_peer-core]
                        (send-shut-packet bone [message-num %& +]:packet)
                      pack
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
                    ?:  (gth (next-expiry:gauge -.val) now.channel)
                      [new-val=`val stop=%.y acc]
                    ::
                    =.  last-sent.val  now.channel
                    =.  resends.acc  [(to-static-fragment key val) resends.acc]
                    [new-val=`val stop=%.n acc]
                  ::  +on-hear: handle ack on a live packet
                  ::
                  ::    If the packet was in our queue, delete it and update our
                  ::    metrics, possibly re-sending skipped packets. Otherwise, no-op
                  ::
                  ++  on-hear
                    |=  [=message-num =fragment-num]
                    ^+  pack
                    ::
                    =-  ::  if no sent packet matches the ack,
                        ::  don't apply mutations or effects
                        ::
                        ?.  found.-
                          %-  (pu-trace snd.veb |.("miss {<show:gauge>}"))
                          pack
                        ::
                        =.  metrics.state  metrics.-
                        =.  live.state     live.-
                        %-  ?.  ?|  =(0 fragment-num)
                                    =(0 (mod counter.metrics.state 20))
                                ==
                              same
                            %+  pu-trace  snd.veb
                            |.("send: {<fragment=fragment-num show:gauge>}")
                        ::  .resends is backward, so fold backward and emit
                        ::
                        =.  peer-core
                          %+  reel  resends.-
                          |=  [packet=static-fragment core=_peer-core]
                          (send-shut-packet bone [message-num %& +]:packet)
                        (fast-resend-after-ack message-num fragment-num)
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
                    %^  (dip:packet-queue _acc)  live.state  acc
                    |=  $:  acc=_acc
                            key=live-packet-key
                            val=live-packet-val
                        ==
                    ^-  [new-val=(unit live-packet-val) stop=? _acc]
                    ::
                    =/  gauge  (ga [metrics num-live]:acc)
                    ::  is this the acked packet?
                    ::
                    ?:  =(key [message-num fragment-num])
                      ::  delete acked packet, update metrics, and stop traversal
                      ::
                      =.     found.acc  %.y
                      =.   metrics.acc  (on-ack:gauge -.val)
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
                    =^  resend  metrics.acc  (on-skipped-packet:gauge -.val)
                    ?.  resend
                      [new-val=`val stop=%.n acc]
                    ::
                    =.  last-sent.val  now.channel
                    =.  tries.val      +(tries.val)
                    =.  resends.acc    [(to-static-fragment key val) resends.acc]
                    [new-val=`val stop=%.n acc]
                  ::  +on-done: apply ack to all packets from .message-num
                  ::
                  ++  on-done
                    |=  =message-num
                    ^+  pack
                    ::
                    =-  =.  metrics.state  metrics.-
                        =.  live.state     live.-
                        ::
                        %.  (fast-resend-after-ack message-num `fragment-num`0)
                        (pu-trace snd.veb |.("done {<num=message-num show:gauge>}"))
                    ::
                    =/  acc  [metrics=metrics.state num-live=~(wyt by live.state)]
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
                    =/  gauge  (ga [metrics num-live]:acc)
                    ::  if we get an out-of-order ack for a message, skip until it
                    ::
                    ?:  (lth message-num.key message-num)
                      [new-val=`val stop=%.n acc]
                    ::  if packet was from acked message, delete it and continue
                    ::
                    ?:  =(message-num.key message-num)
                      =.   metrics.acc  (on-ack:gauge -.val)
                      =.  num-live.acc  (dec num-live.acc)
                      [new-val=~ stop=%.n acc]
                    ::  we've gone past the acked message; we're done
                    ::
                    [new-val=`val stop=%.y acc]
                  ::  +set-wake: set, unset, or reset timer, emitting moves
                  ::
                  ++  set-wake
                    ^+  pack
                    ::  if nonempty .live, pry at head to get next wake time
                    ::
                    =/  new-wake=(unit @da)
                      ?~  head=(pry:packet-queue live.state)
                        ~
                      `(next-expiry:gauge -.val.u.head)
                    ::  no-op if no change
                    ::
                    ?:  =(new-wake next-wake.state)  pack
                    ::  unset old timer if non-null
                    ::
                    =?  peer-core  !=(~ next-wake.state)
                      (pu-emit %b %rest (need next-wake.state))
                    ::  set new timer if non-null and not at at max-backoff
                    ::
                    ::  we are using the ~m2 literal instead of max-backoff:gauge
                    ::  because /app/ping has a special cased maximum backoff of ~s25
                    ::  and we don't want to consolidate that
                    ::
                    =?  peer-core  ?=(^ new-wake)
                      ?:  ?&(?=(^ +.flow.dead.ames-state) =(~m2 rto.metrics.state))
                        peer-core
                      (pu-emit %b %wait u.new-wake)
                    ::
                    =?  next-wake.state  !=(~ next-wake.state)   ~  ::  unset
                    =?  next-wake.state  ?=(^ new-wake)   new-wake  ::  reset
                    ::
                    pack
                  --
                --
              ::  +mi: constructor for |sink message receiver core
              ::
              ++  mi
                |_  [=bone state=message-sink-state]
                ::
                +|  %helpers
                ::
                ++  sink  .
                ++  abed
                  |=  b=^bone
                  sink(bone b, state (~(gut by rcv.peer-state) b *message-sink-state))
                ++  abet
                  ::  if the bone was corked, it's been removed from the state,
                  ::  so we avoid adding it again.
                  ::
                  =?  rcv.peer-state  !corked  (~(put by rcv.peer-state) bone state)
                  peer-core
                ::
                ++  closing  (~(has in closing.peer-state) bone)
                ++  corked   (~(has in corked.peer-state) bone)
                ++  received
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
                ++  call
                  |=  task=message-sink-task
                  ^+  sink
                  ?-    -.task
                      %drop  sink(nax.state (~(del in nax.state) message-num.task))
                      %done  (done ok.task)
                      %flub
                    %=  sink
                      last-heard.state        (dec last-heard.state)
                      pending-vane-ack.state  ~(nap to pending-vane-ack.state)
                    ==
                  ::
                      %hear
                    |^  ?:  ?|  corked
                            ?&  %*(corked sink bone (mix 0b10 bone))
                                =(%nack (received bone))
                        ==  ==
                      ack-on-corked-bone
                    ::
                    ?>  ?=(%& -.meat.shut-packet.task)
                    =+  [num-fragments fragment-num fragment]=+.meat.shut-packet.task
                    ?:  &(=(num-fragments 1) =(fragment-num 0))
                      (check-pending-acks fragment)
                    (hear [lane shut-packet ok]:task)
                    ::
                    ++  ack-on-corked-bone
                      ::  if we %hear a fragment on a corked bone, always ack
                      ::
                      =.  peer-core
                        %+  send-shut-packet  bone
                        [message-num.shut-packet.task %| %| ok=& lag=*@dr]
                      %.  sink
                      %+  pe-trace  odd.veb
                      |.("hear {<(received bone)>} on corked bone={<bone>}")
                    ::
                    ++  check-pending-acks
                      ::  if this is a %cork %plea and we are still waiting to
                      ::  hear %acks for previous naxplanations we sent, no-op
                      ::
                      |=  frag=@uw
                      ^+  sink
                      =/  blob=*  (cue (rep packet-size [frag]~))
                      =+  pump=(abed:mu (mix 0b10 bone))
                      ?.  ?&  ?=(^ ;;((soft [%$ path %cork ~]) blob))
                              ?=(^ live.packet-pump-state.state.pump)
                          ==
                        (hear [lane shut-packet ok]:task)
                      %.  sink
                      %+  pe-trace  odd.veb
                      |.("pending ack for naxplanation, skip %cork bone={<bone>}")
                    --
                  ==
                ::
                +|  %tasks
                ::  +hear: receive message fragment, possibly completing message
                ::
                ++  hear
                  |=  [=lane =shut-packet ok=?]
                  ^+  sink
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
                    sink
                  ::
                  =/  is-last-fragment=?  =(+(fragment-num) num-fragments)
                  ::  always ack a dupe!
                  ::
                  ?:  (lte seq last-acked.state)
                    ?.  is-last-fragment
                      ::  single packet ack
                      ::
                      =.  peer-core  (send-shut-packet bone seq %| %& fragment-num)
                      %.  sink
                      %+  pe-trace  rcv.veb
                      |.("send dupe ack {<seq=seq^fragment-num=fragment-num>}")
                    ::  whole message (n)ack
                    ::
                    =/       ok=?  !(~(has in nax.state) seq)
                    =.  peer-core  (send-shut-packet bone seq %| %| ok lag=`@dr`0)
                    %.  sink
                    %+  pe-trace  rcv.veb
                    |.("send dupe message ack {<seq=seq>} ok={<ok>}")
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
                      %-  %+  pe-trace  rcv.veb
                          |.  ^-  tape
                          =/  data
                            :*  her  seq=seq  bone=bone.shut-packet
                                fragment-num  num-fragments
                                la=last-acked.state  lh=last-heard.state
                            ==
                          "hear last in-progress {<data>}"
                      sink
                    ::  ack all other packets
                    ::
                    =.  peer-core  (send-shut-packet bone seq %| %& fragment-num)
                    %-  %+  pe-trace  rcv.veb  |.
                        =/  data
                          :*  seq=seq  fragment-num=fragment-num
                              num-fragments=num-fragments  closing=closing
                          ==
                        "send ack-1 {<data>}"
                    sink
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
                      %-  %+  pe-trace  rcv.veb  |.
                          =/  data
                            [her seq=seq lh=last-heard.state la=last-acked.state]
                          "hear last dupe {<data>}"
                      sink
                    =.  peer-core  (send-shut-packet bone seq %| %& fragment-num)
                    %.  sink
                    %+  pe-trace  rcv.veb
                    |.("send dupe ack {<her^seq=seq^fragment-num=fragment-num>}")
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
                  =?  peer-core  !is-last-fragment
                    %-  %+  pe-trace  rcv.veb  |.
                        =/  data
                          [seq=seq fragment-num=fragment-num fragments=num-fragments]
                        "send ack-2 {<data>}"
                    (send-shut-packet bone seq %| %& fragment-num)
                  ::  enqueue all completed messages starting at +(last-heard.state)
                  ::
                  |-  ^+  sink
                  ::  if this is not the next message to ack, we're done
                  ::
                  ?.  =(seq +(last-heard.state))
                    sink
                  ::  if we haven't heard anything from this message, we're done
                  ::
                  ?~  live=(~(get by live-messages.state) seq)
                    sink
                  ::  if the message isn't done yet, we're done
                  ::
                  ?.  =(num-received num-fragments):u.live
                    sink
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
                  =?  sink  empty  (handle-sink seq message ok)
                  ::
                  $(seq +(seq))
                ::  +done: handle confirmation of message processing from vane
                ::
                ++  done
                  |=  ok=?
                  ^+  sink
                  ::
                  =^  pending  pending-vane-ack.state
                    ~(get to pending-vane-ack.state)
                  =/  =message-num  message-num.p.pending
                  ::
                  =.  last-acked.state  +(last-acked.state)
                  =?  nax.state  !ok  (~(put in nax.state) message-num)
                  ::
                  =.  peer-core
                    (send-shut-packet bone message-num %| %| ok lag=`@dr`0)
                  ?~  next=~(top to pending-vane-ack.state)  sink
                  (handle-sink message-num.u.next message.u.next ok)
                ::
                +|  %implementation
                ::  +handle-sink: dispatch message
                ::
                ++  handle-sink
                  |=  [=message-num message=* ok=?]
                  ^+  sink
                  |^  ?-((received bone) %plea ha-plea, %boon ha-boon, %nack ha-nack)
                  ::
                  ++  ha-plea
                    ^+  sink
                    ?:  |(closing corked)  sink
                    %-  %+  pe-trace  msg.veb
                        =/  dat  [her bone=bone message-num=message-num]
                        |.("sink plea {<dat>}")
                    ?.  ok
                      =/  nack-bone=^bone  (mix 0b10 bone)
                      =/  =^message        [%naxplanation message-num *error]
                      =/  =wire  (make-bone-wire her her-rift.channel nack-bone)
                      ::  send nack-trace with blank .error for security
                      ::
                      =.  peer-core
                        %+  pe-emit  duct
                        [%pass wire %a %deep %nack her nack-bone message]
                      ::
                      (done ok=%.n)
                    ::
                    =/  =wire  (make-bone-wire her her-rift.channel bone)
                    =.  peer-core
                      =+  ;;  =plea  message
                      ?.  =(vane.plea %$)
                        ?+    vane.plea  ~|(ames-evil-vane/our^her^vane.plea !!)
                            ?(%c %e %g %j)
                          (pe-emit duct %pass wire vane.plea %plea her plea)
                        ==
                      ::  a %cork and %ahoy pleas (both introduced to account
                      ::  for checking per-peer protocol updates) are handled
                      ::  using %$ as the recipient vane to handle peers
                      ::  that have not migrated into the new protocol
                      ::
                      ?+    -.payload.plea  !!
                          %ahoy
                        ?>  ?=(%mesa -.path.plea)
                        (pe-emit duct %pass wire %a %deep %ahoy her bone)
                      ::
                          %cork
                        ?>  ?=(%flow -.path.plea)
                        (pe-emit duct %pass wire %a %deep %cork her bone)
                      ==
                    sink
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
                    ^+  sink
                    ?:  |(closing corked)  sink
                    %-  %+  pe-trace  msg.veb  |.
                        ::  XX -.task not visible, FIXME
                        ::
                        =/  dat  [her bone=bone message-num=message-num]
                        ?:(ok "sink boon {<dat>}" "crashed on sink boon {<dat>}")
                    =.  peer-core  (pe-emit (got-duct bone) %give %boon message)
                    =?  moves  !ok
                      ::  we previously crashed on this message; notify client vane
                      ::
                      %+  turn  moves
                      |=  =move
                      ?.  ?=([* %give %boon *] move)  move
                      [duct.move %give %lost ~]
                    ::  send ack unconditionally
                    ::
                    (done ok=%.y)
                  ::
                  ++  ha-nack
                    ^+  sink
                    ::  if we get a naxplanation for a %cork, the publisher hasn't
                    ::  received the OTA. The /recork timer will retry eventually.
                    ::
                    %-  %+  pe-trace  msg.veb
                        =/  dat  [her bone=bone message-num=message-num]
                        |.("sink naxplanation {<dat>}")
                    ::  flip .bone's second bit to find referenced flow
                    ::
                    =/  target=^bone  (mix 0b10 bone)
                    =.  peer-core
                      ::  will notify |message-pump that this message got naxplained
                      ::
                      =/  =wire  (make-bone-wire her her-rift.channel target)
                      %+  pe-emit  duct
                      [%pass wire %a %deep %sink her target ;;(naxplanation message)]
                    ::  ack nack-trace message (only applied if we don't later crash)
                    ::
                    (done ok=%.y)
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
                ++  fine  .
                ++  abed
                  |=  p=^path
                  ~|  no-keen-for-path/p
                  fine(path p, keen (~(got by keens) p))
                ::
                ++  fi-abet
                  ^+  peer-core
                  ?.  =,  keen
                      ::  num-fragments is 0 when unknown (i.e. no response yet)
                      ::  if no-one is listening, kill request
                      ::
                      ?|  =(~ listeners.keen)
                          &(!=(0 num-fragments) =(num-fragments num-received))
                      ==
                    =.  fine   fi-set-wake
                    peer-core(keens.peer-state (~(put by keens) path keen))  :: XX tack.keens
                  ::
                  =?  fine  ?=(^ next-wake.keen)
                    (fi-rest u.next-wake.keen)
                  peer-core(keens.peer-state (~(del by keens) path))  :: XX tack.keens
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
                ++  fi-emit   |=(move fine(event-core (emit +<)))
                ++  fi-emil   |=((list move) fine(event-core (emil +<)))
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
                  =.  event-core
                    %-  %*(send-blob event-core vane %fine)
                    [for=| her blob `known/peer-state]
                  fine
                ::
                ++  fi-give-tune
                  |=  dat=(unit roar)
                  |=([=^duct =_fine] (fi-emit:fine duct %give %tune [her path] dat))
                ::
                +|  %entry-points
                ::
                ++  fi-start
                  |=  =^duct
                  %-  (fi-trace fin.veb |.("keen {(spud fi-full-path)}"))
                  =.  fine  (fi-sub duct)
                  ?>  =(num-fragments.keen 0)
                  =/  fra=@     1
                  =/  req=hoot  (fi-etch-wail fra)
                  =/     =want  [fra req last=now tries=1 skips=0]
                  =.  wan.keen  (put:fi-mop ~ [fra .]:want)
                  (fi-send `@ux`req)
                ::
                ++  fi-rcv
                  |=  [[=full=^path num=@ud] =meow =lane]
                  ^+  fine
                  =/  og  fine
                  =.  peer-core  (update-qos %fine %live last-contact=now)
                  ::  handle empty
                  ?:  =(0 num.meow)
                    ?>  =(~ dat.meow)
                    (fi-done sig.meow ~)
                  ::  update congestion, or fill details
                  ::
                  =?  fine  =(0 num-fragments.keen)
                    ?>  =(num 1) :: XX no-op instead?
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
                  =^  found=?  fine  (fi-on-ack num)
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
                  |=(=^duct fine(listeners.keen (~(put in listeners.keen) duct)))
                ::  scry is autocancelled in +abet if no more listeners
                ::
                ++  fi-unsub
                  |=  [=^duct all=?]
                  ^+  fine
                  ?:  all
                    %-  (fi-trace fin.veb |.("unsub all {<fi-full-path>}"))
                    =.  fine  (~(rep in listeners.keen) (fi-give-tune ~))
                    fine(listeners.keen ~)
                  ::
                  ?:  (~(has in listeners.keen) duct)
                    %-  (fi-trace fin.veb |.("unsub {<fi-full-path>} on {<duct>}"))
                    fine(listeners.keen (~(del in listeners.keen) duct))
                  ::
                  %.  fine
                  (fi-trace fin.veb |.("unknown {<fi-full-path>} {<duct>}"))
                ::
                +|  %implementation
                ::
                ++  fi-on-ack
                  =|  marked=(list want)
                  |=  fra=@ud
                  ^-  [found=? cor=_fine]
                  =.  fine
                    =/  first  (pry:fi-mop wan.keen)
                    ?~  first
                      fine
                    ?:  =(fra fra.val.u.first)
                      fine
                    =^  resend=?  metrics.keen
                      (%*(on-skipped-packet fi-gauge vane %fine) +>.val.u.first)
                    ?:  !resend
                      fine
                    =.  tries.val.u.first  +(tries.val.u.first)
                    =.  last-sent.val.u.first  now
                    =.  wan.keen  (put:fi-mop wan.keen u.first)
                    =.  fine  (fi-send `@ux`hoot.val.u.first)
                    fine
                  ::
                  =/  found  (get:fi-mop wan.keen fra)
                  ?~  found
                    [| fine]
                  =.  metrics.keen  (%*(on-ack fi-gauge vane %fine) +>.u.found)
                  =.  wan.keen  +:(del:fi-mop wan.keen fra)
                  [& fine]
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
                  ^+  fine
                  ::
                  =;  paz=(list want)
                    fine(keen keen(num-fragments num.meow, nex (tail paz)))
                  %+  turn  (gulf 1 num.meow)
                  |=  fra=@ud
                  ^-  want
                  [fra (fi-etch-wail fra) now 0 0]
                ::  +fi-continue: send packets based on normal congestion flow
                ::
                ++  fi-continue
                  =|  inx=@ud
                  =|  sent=(list @ud)
                  =/  max  num-slots:fi-gauge
                  |-  ^+  fine
                  ?:  |(=(~ nex.keen) =(inx max))
                    fine
                  =^  =want  nex.keen  nex.keen
                  =.  last-sent.want   now
                  =.      tries.want   +(tries.want)
                  =.        wan.keen   (put:fi-mop wan.keen [fra .]:want)
                  =.            fine   (fi-send `@ux`hoot.want)
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
                  =;  [cor=_fine wants=_wan.keen]
                    cor(wan.keen wants)
                  %^  (dip:fi-mop ,cor=_fine)  wan.keen
                    fine
                  |=  [cor=_fine @ud =want]
                  ^-  [(unit ^want) stop=? cor=_fine]
                  ?.  (lte fra.want fra)
                    [`want & cor]
                  ?:  (gth (next-expiry:fi-gauge:cor +>.want) now)
                    [`want & cor]
                  =.  last-sent.want  now
                  =.  cor  (fi-send:cor `@ux`hoot.want)
                  [`want | cor]
                ::
                ++  fi-pass-timer
                  |=  =note
                  =/  =wire  (welp /fine/behn/wake/(scot %p her) path)
                  (fi-emit unix-duct.ames-state %pass wire note)
                ::
                ++  fi-set-wake
                  ^+  fine
                  =/  next-wake=(unit @da)
                    ?~  want=(pry:fi-mop wan.keen)
                      ~
                    `(next-expiry:fi-gauge +>:val.u.want)
                  ?:  =(next-wake next-wake.keen)
                    fine
                  =?  fine  !=(~ next-wake.keen)
                    =/  old  (need next-wake.keen)
                    =.  next-wake.keen  ~
                    (fi-rest old)
                  =?  fine  ?=(^ next-wake)
                    =.  next-wake.keen  next-wake
                    (fi-wait u.next-wake)
                  fine
                ::  +fi-take-wake: handle request packet timeout
                ::
                ++  fi-take-wake
                  ^+  fine
                  =.  next-wake.keen  ~
                  =.  peer-core   (update-qos %fine qos:(is-peer-dead now peer-state))
                  ::  has the direct route expired?
                  ::
                  =/  old-route  route.peer-state
                  =.  peer-state  (update-peer-route her peer-state)
                  =?  peer-core  !=(old-route route.peer-state)
                    %-  pe-emit
                    :*  unix-duct.ames-state  %give  %nail  her
                        (get-forward-lanes our peer-state peers.ames-state)
                    ==
                  =.  metrics.keen  %*(on-timeout fi-gauge vane %fine)
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
                  (trace vane verb ship ships.bug.channel print)
                ::  +next-expiry: when should a newly sent fresh packet time out?
                ::
                ::    Use rtt + 4*sigma, where sigma is the mean deviation of rtt.
                ::    This should make it unlikely that a packet would time out
                ::    from a delay, as opposed to an actual packet loss.
                ::
                ++  next-expiry
                  |=  packet-state
                  ^-  @da
                  (add last-sent rto)
                ::  +num-slots: how many packets can we send right now?
                ::
                ++  num-slots
                  ^-  @ud
                  (sub-safe cwnd live-packets)
                ::
                ::  +clamp-rto: apply min and max to an .rto value
                ::
                ++  clamp-rto
                  |=  rto=@dr
                  ^+  rto
                  (min max-backoff (max ^~((div ~s1 5)) rto))
                ::  +max-backoff: calculate highest re-send interval
                ::
                ::    Keeps pinhole to sponsors open by inspecting the duct (hack).
                ::
                ++  max-backoff
                  ^-  @dr
                  ?:(?=([[%gall %use %ping *] *] duct) ~s25 ~m2)
                ::  +in-slow-start: %.y if we're in "slow-start" mode
                ::
                ++  in-slow-start
                  ^-  ?
                  (lth cwnd ssthresh)
                ::  +in-recovery: %.y if we're recovering from a skipped packet
                ::
                ::    We finish recovering when .live-packets finally dips back
                ::    down to .cwnd.
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
                ::
                +|  %entry-points
                ::  +on-ack: adjust metrics based on a packet getting acknowledged
                ::
                ++  on-ack
                  |=  =packet-state
                  ^-  pump-metrics
                  ::
                  =.  counter  +(counter)
                  ::  if below congestion threshold, add 1; else, add avg 1 / cwnd
                  ::
                  =.  cwnd
                    ?:  in-slow-start
                      +(cwnd)
                    (add cwnd !=(0 (mod (mug now) cwnd)))
                  ::  if this was a re-send, don't adjust rtt or downstream state
                  ::
                  ?:  (gth tries.packet-state 1)
                    metrics(rto (clamp-rto (add rtt (mul 4 rttvar))))
                  ::  rtt-datum: new rtt measurement based on packet roundtrip
                  ::
                  =/  rtt-datum=@dr  (sub-safe now last-sent.packet-state)
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
                  =.  rto     (clamp-rto (add rtt (mul 4 rttvar)))
                  ::
                  %.  metrics
                  %+  ga-trace  ges.veb  |.
                  "ack update {<show rtt-datum=rtt-datum rtt-error=rtt-error>}"
                ::  +on-skipped-packet: handle misordered ack
                ::
                ++  on-skipped-packet
                  |=  packet-state
                  ^-  [resend=? pump-metrics]
                  ::
                  =/  resend=?  &((lte tries 1) |(in-recovery (gte skips 3)))
                  :-  resend
                  ::
                  =?  cwnd  !in-recovery  (max 2 (div cwnd 2))
                  %-  %+  ga-trace  snd.veb
                      |.("skip {<resend=resend in-recovery=in-recovery show>}")
                  metrics
                ::  +on-timeout: (re)enter slow-start mode on packet loss
                ::
                ++  on-timeout
                  ^-  pump-metrics
                  ::
                  %-  (ga-trace ges.veb |.("timeout update {<show>}"))
                  =:  ssthresh  (max 1 (div cwnd 2))
                          cwnd  1
                          rto   (clamp-rto (mul rto 2))
                    ==
                  metrics
                --
              --
            ::
            --
          ::
          --
      ::  adult ames, after metamorphosis from larva
      ::
      =*  veb  veb.bug.ames-state
      |%
      ::  +call: handle request $task
      ::
      ++  call
        |=  [=duct dud=(unit goof) wrapped-task=(hobo task)]
        ^-  [(list move) _vane-gate]
        ::
        =/  =task       ((harden task) wrapped-task)
        =/  event-core  (ev duct ames-state)
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
          ?+  -.task  !!  ::  XX mesa tasks; no-op?
            %hear  (on-hear:event-core [lane blob ~]:task)
            %prod  (on-prod:event-core ships.task)
            %cong  (on-cong:event-core [msg mem]:task)
            %stir  (on-stir:event-core arg.task)
            %trim  on-trim:event-core
            %plea  (on-plea:event-core [ship plea]:task)
            %cork  (on-cork:event-core ship.task)
            %kroc  (on-kroc:event-core bones.task)
            %deep  (on-deep:event-core deep.task)
          ::
            %keen  (on-keen:event-core +.task)
            %chum  (on-chum:event-core +.task)
            %yawn  (on-cancel-scry:event-core | +.task)
            %wham  (on-cancel-scry:event-core & +.task)
          ::
            %mate  (on-mate:event-core +.task)
          ==
        ::
        [moves vane-gate]
      ::  +take: handle response $sign
      ::
      ++  take
        |=  [=wire =duct dud=(unit goof) =sign]
        ^-  [(list move) _vane-gate]
        ?^  dud
          ~|(%ames-take-dud (mean tang.u.dud))
        ::
        =/  event-core  (ev duct ames-state)
        ::
        =^  moves  ames-state
          ?:  ?=([%gall %unto *] sign)
            `ames-state
          ::
          =<  abet
          ?+  sign  ~&(ames-take-sign/[&1^&2]:sign event-core)
            [@ %done *]  (on-take-done:event-core wire error.sign)
            [@ %boon *]  (on-take-boon:event-core wire payload.sign)
            [@ %noon *]  (on-take-noon:event-core wire id.sign payload.sign)
          ::
            [%ames %tune *]  (on-tune:event-core wire [[ship path] roar]:sign)
          ::
            [%behn %wake *]  (on-take-wake:event-core wire error.sign)
          ::
            [%gall %flub ~]  (on-take-flub:event-core wire)
          ==
        ::
        [moves vane-gate]
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
        ?:  ?&  =(&+our why)
                =([%ud 1] r.bem)
                =(%$ syd)
                =(%x ren)
            ==
          =>  .(tyl `(pole knot)`tyl)
          ?+    tyl  ~
          ::
              [%fine %shut kef=@ enc=@ ~]
            =/  key-idx  (slav %ud kef.tyl)
            =/  key  (got:on:chain server-chain.ames-state (slav %ud kef.tyl))
            =/  pat=(unit path)
              (rush `@t`(dy:crub:crypto key.key (slav %uv enc.tyl)) stap)
            ?~  pat
              [~ ~]
            ?~  blk=(de-part:balk our rift.ames-state life.ames-state u.pat)
              [~ ~]
            ?.  (check-fine-key ames-state u.blk key-idx)
              ~&  key-validation-failed/[u.pat key-idx server-chain.ames-state]
              [~ ~]
            =/  res  (rof [~ ~] /ames (as-omen:balk u.blk))
            ?~  res
              ~&  %bailing-close
              [~ ~]
            ?~  u.res
              ``atom+!>(~)
            ?~  key=(get:on:chain server-chain.ames-state key-idx)
              ~
            =-  ``atom+!>(-)
            `@uv`(en:crub:crypto -.u.key (jam [p q.q]:u.u.res))
          ::
              [%chum her=@ lyf=@ cyf=@ ~]
            =/  who  (slaw %p her.tyl)
            =/  lyf  (slaw %ud lyf.tyl)
            =/  cyf  (slaw %uv cyf.tyl)
            ?:  |(?=(~ who) ?=(~ lyf) ?=(~ cyf))
              [~ ~]
            =/  per  (~(get by peers.ames-state) u.who)
            ?.  &(?=([~ %known *] per) =(life.u.per u.lyf))
              ~
            =/  bal=(unit balk)
              ?~  tex=(de:crub:crypto symmetric-key.u.per u.cyf)  ~
              ?~  pax=(rush u.tex stap)                           ~
              (de-part:balk our 0 0 u.pax)
            ?~  bal
              [~ ~]
            ?~  res=(rof `[u.who ~ ~] /ames (as-omen:balk u.bal))
              ~
            =-  ``atom+!>(`@ux`-)
            %+  en:crub:crypto  symmetric-key.u.per
            ?~(u.res ~ (jam [p q.q]:u.u.res))
          ==
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
        ::  /ax/chain/[idx]                [idx=@ud key=@uvJ]
        ::  /ax/chain/latest               [idx=@ud key=@uvJ]
        ::  /ax/peers                      (map ship ?(%alien %known))
        ::  /ax/peers/[ship]               ship-state
        ::  /ax/peers/[ship]/last-contact  (unit @da)
        ::  /ax/peers/[ship]/forward-lane  (list lane)
        ::  /ax/bones/[ship]               [snd=(set bone) rcv=(set bone)]
        ::  /ax/snd-bones/[ship]/[bone]    vase
        ::  /ax/snubbed                    (?(%allow %deny) (list ship))
        ::  /ax/fine/hunk/[path/...]       (list @ux) scry response fragments
        ::  /ax/fine/ducts/[path/]         (list duct)
        ::  /ax/fine/shut/[path/]          @ux encrypted response
        ::  /ax/rift                        @
        ::  /ax/corked/[ship]              (set bone)
        ::  /ax/closing/[ship]             (set bone)
        ::
        ?.  ?=(%x ren)  ~
        =>  .(tyl `(pole knot)`tyl)
        ::  public endpoints
        ?:  ?=([%fine %hunk lop=@t len=@t pax=^] tyl)
          ::TODO  separate endpoint for the full message (instead of packet list)
          ::  .pax is expected to be a scry path of the shape /vc/desk/rev/etc,
          ::  so we need to give it the right shape
          ::
          ?~  blk=(de-path-soft:balk pax.tyl)  ~
          ::
          ?.  (is-our-bulk our ames-state u.blk)
            ~
          =+  nom=(as-omen:balk u.blk)
          ~|  nom
          |^
          =/  van  ?@(vis.nom (end 3 vis.nom) way.vis.nom)
          =/  kyr  ?@(vis.nom (rsh 3 vis.nom) car.vis.nom)
          (en-hunk (rof ~ /ames nom))
          ::
          ++  en-hunk
            |=  res=(unit (unit cage))
            ^+  res
            ?~  res  ~
            =/  =hunk  [(slav %ud lop.tyl) (slav %ud len.tyl)]
            ::
            =/  hu-co
              (etch-hunk our life.ames-state (nol:nu:crub:crypto priv.ames-state))
            ?-  res
              [~ ~]    ``noun+!>((etch-open:hu-co pax.tyl hunk ~))
              [~ ~ *]  ``noun+!>((etch-open:hu-co pax.tyl hunk [p q.q]:u.u.res))
            ==
          --
        ::  private endpoints
        ?.  =([~ ~] lyc)  ~
          ?+    tyl  ~
              [%$ %whey ~]
            =/  maz=(list mass)
              =+  [known alien]=(skid ~(val by peers.ames-state) |=(^ =(%known +<-)))
              :~  peers-known+&+known
                  peers-alien+&+alien
              ==
            ``mass+!>(maz)
          ::
              [%chain %latest ~]
            ``noun+!>(`[idx=@ key=@ =path]`(need (ram:on:chain server-chain.ames-state)))
          ::
              [%chain idx=@ ~]
            ?~  idx=(slaw %ud idx.tyl)
              [~ ~]
            ?~  key=(get:on:chain server-chain.ames-state u.idx)
              [~ ~]
            ``noun+!>(`[idx=@ key=@]`[u.idx key.u.key])
          ::
              [%peers ~]
            :^  ~  ~  %noun
            !>  ^-  (map ship ?(%alien %known))
            (~(run by peers.ames-state) ^head)  :: XX
          ::
              [%peers her=@ req=*]
            =/  who  (slaw %p her.tyl)
            ?~  who  [~ ~]
            =/  peer  (~(get by peers.ames-state) u.who)
            =/  chum  (~(get by chums.ames-state) u.who)
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
              ?:  =(our u.who)
                ~
              ?:  ?=([~ %known *] peer)
                (get-forward-lanes our +.u.peer peers.ames-state)
              ?:  ?=([~ %known *] chum)
                =/  lanes=(list lane:pact)
                  (get-forward-lanes-mesa our +.u.chum chums.ames-state)
                %+  turn  lanes
                |=  =lane:pact
                ^-  ^lane
                ?@  lane
                  [%.y `@p`lane]
                :-  %.n
                %+  can  3
                :~  4^p.lane
                    2^q.lane
                ==
              =/  sax  (rof [~ ~] /ames %j `beam`[[our %saxo %da now] /(scot %p u.who)])
              ?.  ?=([~ ~ *] sax)
                ~
              =/  gal  (rear ;;((list ship) q.q.u.u.sax))
              ?:  =(our gal)
                ~
              [%& gal]~
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
          ::
              [%protocol %version ~]
            ``noun+!>(protocol-version)
          ::
              [%boot ~]
            =/  who
              =/  ship  our
              |-
              ^-  @p
              =/  next  (^^sein:title rof /ames our now ship)
              ?:  ?=(%czar (clan:title next))
                next
              $(ship next)
            =/  per  (~(get by peers.ames-state) who)
            ?.  ?=([~ %known *] per)  ``noun+!>(~)
            =,  u.per
            =/  ducs
              %+  skim
                ~(tap in ~(key by by-duct.ossuary))
              |=  =duct
              =(-.duct /gall/sys/way/(scot %p who)/ping)
            ?~  ducs  ``noun+!>(~)
            =/  ping-bone
              (~(got by by-duct.ossuary) -.ducs)
            =/  ping-snd=message-pump-state
              (~(got by snd) ping-bone)
            :^  ~  ~  %noun
            !>  :*  ~  who  rift.ames-state  life.ames-state
                    ping-bone  current.ping-snd  next.ping-snd
                ==
          ::
          ==
      ::
      --
    ::
    ++  mesa
      ::
      =>  ::  inner event-handling
          ::
          =|  moves=(list move)
          ::
          |_  [hen=duct per=[=ship sat=fren-state]]
          ::
          +|  %helpers
          ::
          ++  ev-core  .
          ++  ev-abet
            =.  chums.ames-state
              (~(put by chums.ames-state) per(sat known/sat.per))
            ev-abut
          ::
          ++  ev-abut  moves^ames-state
          ++  ev-abed  |=(=duct ev-core(hen duct))
          ++  ev-foco  |=(=_per ev-core(per per))
          ++  ev-emit  |=(=move ev-core(moves [move moves]))
          ++  ev-emil  |=(mos=(list move) ev-core(moves (weld mos moves)))
          ::
          +|  %flow-wires
          ::
          +$  ev-flow-wire
            $:  %mesa
                %flow
                were=?(%van %ext %int %cor %pok)  ::  XX revisit names
                =dire
                [%p her=@p]
                [%ud rift=@ud]
                [%ud bone=@ud]
                ~
            ==
          ::
          +$  ev-timer-wire  :: XX revisit
            $:  %mesa
                :: ?(%poke %dead %alien ...)  :: XX add tag for each timer flow
                [%p her=@p]
                [%ud bone=@ud]
                [%ud rift=@ud]
                ~
            ==
          ::
          ++  ev-pave
            |=  =path
            ^-  pith
            %+  turn  path
            |=  i=@ta
            (fall (rush i spot:stip) i)
          ::
          +|  %top-level-paths
          ::
          ::  /ax/~ship//ver=1/mess/rift=1//[...]
          ::
          ::  /ax/~ship//ver=1/mess/rift=1/pact/bloq=13/ init/[...]
          ::
          ::  /ax/~ship//ver=1/mess/rift=1/pact/bloq=13/pure/auth/frag=1/[...]
          ::  /ax/~ship//ver=1/mess/rift=1/pact/bloq=13/pure/data/frag=1/[...]
          ::
          +$  res-scry-head  [%ax [%p her=@p] %'1' res=*]
          +$  res-mess-head  [%mess [%ud ryf=@ud] res=*]
          +$  res-pact-head  [%pact [%ud boq=@ud] ser=?(%etch %pure) pat=*]
          +$  res-pure-pith  [typ=?(%auth %data) [%ud fag=@ud] pat=*]
          ::
          +|  %namespace-paths
          ::
          ::  /[..]/publ/life=1/[...]
          ::  /[..]/chum/life=1/her=@p/hyf=@ud/encrypted-path=@uv
          ::  /[..]/shut/key=1/encrypted-path=@uv
          ::
          +$  publ-pith  [%publ [%ud lyf=@ud] pat=*]
          +$  chum-pith  [%chum [%ud lyf=@ud] [%p her=@p] [%ud hyf=@ud] [%uv cyf=@uv] ~]
          +$  shut-pith  [%shut [%ud kid=@ud] [%uv cyf=@uv] ~]
          ::
          +|  %message-flow-paths
          ::
          +$  flow-pith
            $:  %flow
                [%ud bone=@ud]
                load=?(%plea %boon %ack-plea %ack-boon %nax)  ::  XX to %lul
                [%p rcvr=@p]
                [%ud mess=@ud]
                ~
            ==
          ::
          +$  cork-pith
            $:  %flow
                [%ud bone=@ud]
                %cork             :: XX allow to read "server" corks
                [%p rcvr=@p]
                ~
            ==
          ::
          +|  %attestation-path
          ::
          +$  poof-pith
            $:  %comet
                %proof
                [%p rcvr=@p]
                [%ud life=@ud]
                ~
            ==
          ::
          +|  %validation
          ::
          ++  ev-validate-wire
            |=  =wire
            ^-  (unit ev-flow-wire)
            =>  .(wire `(pole iota)`(ev-pave wire))
            ?.   ?=(ev-flow-wire wire)
              ~>  %slog.0^leaf/"mesa: malformed wire: {(spud (pout wire))}"  ~
            `wire
          ::
          ++  ev-validate-path
            |=  =path
            ^-  (pole iota)
            ~|  path
            %-  ev-pave
            ?~  inn=(inner-path-to-beam *@p path)  ~
            ~|  u.inn
            ?>  =([[%a %x] *@p %$ ud+1] [vew -.bem]:u.inn)
            s.bem.u.inn
          ::
          ++  ev-decrypt-load
            |=  [[=ship =path] ser=@]
            ^-  @
            =/  tyl=(pole knot)  path
            ?+    tyl  !!
                [%publ *]  :: unencrypted
              ser
            ::
                [%chum lyf=@ her=@ hyf=@ cyf=@ ~]  :: encrypted with eddh key
              =/  lyf  (slaw %ud lyf.tyl)
              =/  her  (slaw %p her.tyl)
              =/  hyf  (slaw %ud hyf.tyl)
              =/  cyf  (slaw %uv cyf.tyl)
              ?>  &(?=(^ lyf) ?=(^ her) ?=(^ hyf) ?=(^ cyf))
              =/  key  ::  (get-key-for u.her u.hyf)
                =/  her=@p  ?:(=(u.her our) ship u.her)  :: %poke payload are for us
                =+  per=(ev-got-per her)        :: XX ev-get-per
                :: ::  ?>  ?=(%known -.sat.per)        :: XX wat if %alien?
                ?.  =(u.hyf life.sat.per)   !!  :: XX
                symmetric-key.sat.per
              =*  iv  u.pyf  :: XX
              (decrypt:crypt `@`key u.cyf ser)
            ::
                [%shut kid=@ cyf=@ ~]  :: encrypted with group key
              =/  kid  (slaw %ud kid.tyl)
              =/  cyf  (slaw %uv cyf.tyl)
              ?>  &(?=(^ kid) ?=(^ cyf))
              ::  ?>  ?=(%known -.sat.per)
              ?~  key=(get:key-chain client-chain.sat.per u.kid)
                !!  ::  XX handle
              (decrypt:crypt -.u.key u.cyf ser)
            ==
          ::
          ++  ev-decrypt-spac
            |=  [=space ser=@ cyf=(unit @)]
            ^+  ser
            ?-  -.space
              %publ  ser
              %shut  (decrypt:crypt key.space (need cyf) ser)
              %chum  (decrypt:crypt key.space (need cyf) ser)
            ==
          ::
          ++  ev-sig-key
            |=  [=path =ship]
            ^-  @uxI
            =/  tyl=(pole knot)  path
            ?>  ?=([%publ lyf=@ pat=*] tyl)
            =/  lyf  (slaw %ud lyf.tyl)
            ?>  ?=(^ lyf)
            =/  sat=fren-state  sat:(ev-got-per ship)
            ?>  =(life.sat u.lyf)
            (end 8 (rsh 3 public-key.sat))
          ::
          ++  ev-mac-key
            |=  [=path =ship]
            ^-  @uxI
            =/  tyl=(pole knot)  path
            ?+    tyl  !!
                [%chum lyf=@ her=@ hyf=@ pat=[cyf=@ ~]]
              =/  her  (slaw %p her.tyl)
              ?>  ?=(^ her)
              =/  her=@p  ?:(=(u.her our) ship u.her)
              =/  sat=fren-state  sat:(ev-got-per her)
              ?>  (lte (met 3 symmetric-key.sat) 32)
              `@uxI`symmetric-key.sat
            ::
                [%shut kid=@ pat=[cyf=@ ~]]
              =/  kid  (slaw %ud kid.tyl)
              ?>  ?=(^ kid)
              =/  sat=fren-state  sat:(ev-got-per ship)
              ?~  key=(get:key-chain client-chain.sat u.kid)
                !!  :: XX handle
              ?>  (lte (met 3 -.u.key) 32)
              `@uxI`-.u.key
            ==
          ::
          ++  ev-decrypt-path
            |=  [=path =ship]
            ^-  [=space (unit cyf=@) inner=^path]
            =/  tyl=(pole knot)  path
            ?+    tyl  !!
                [%publ lyf=@ pat=*]  :: unencrypted
              [publ/(slav %ud lyf.tyl) ~ pat.tyl]
            ::
                [%chum lyf=@ her=@ hyf=@ pat=[cyf=@ ~]]  :: encrypted with eddh key
              =/  lyf  (slaw %ud lyf.tyl)
              =/  her  (slaw %p her.tyl)
              =/  hyf  (slaw %ud hyf.tyl)
              =/  cyf  (slaw %uv cyf.pat.tyl)
              ?>  &(?=(^ lyf) ?=(^ her) ?=(^ hyf) ?=(^ cyf))
              ::  XX check =(ship u.her)
              =/  her=@p  ?:(=(u.her our) ship u.her)  :: %poke payloads are for us
              =+  per=(ev-got-per her)         :: XX ev-get-per
              :: ::  ?>  ?=(%known -.sat.per)  :: XX wat if %alien?
              :: ?.  =(u.hyf life.sat.per)   !!  :: XX
              =*  key  `@uxI`symmetric-key.sat.per
              :+  [%chum life.ames-state her life.sat.per key]
                cyf
              (open-path:crypt key u.cyf)
            ::
                [%shut kid=@ pat=[cyf=@ ~]]  :: encrypted with group key
              =/  kid  (slaw %ud kid.tyl)
              =/  cyf  (slaw %uv cyf.pat.tyl)
              ?>  &(?=(^ kid) ?=(^ cyf))
              =+  per=(ev-got-per ship)        :: XX ev-get-per
              :: ::  ?>  ?=(%known -.sat.per)  :: XX wat if %alien?
              ?~  key=(get:key-chain client-chain.sat.per u.kid)
                !!  :: XX handle
              :^    [%shut u.kid -.u.key]
                  cyf
                -.u.key
              (open-path:crypt -.u.key u.cyf)
            ==
          ::
          ++  ev-authenticate
            |=  [rut=@uxI aut=auth:pact =name:pact]
            ^-  ?
            ?>  ?=([%& *] aut)
            =/  ful  (en-beam [[her.name %$ ud+1] pat.name])
            ?-  -.p.aut
              %&  (verify-sig:crypt (ev-sig-key [pat her]:name) p.p.aut ful rut)
              %|  (verify-mac:crypt (ev-mac-key [pat her]:name) p.p.aut ful rut)
            ==
          ::
          +|  %entry-points
          ::
          ++  ev-call
            =>  |%
                 +$  req-task
                   ::  ?(%plea %keen %cork) request tasks are called directly
                   ::
                   $%  $<(%mess $>(%heer task))  ::  XX common tasks
                       [%mess =mess dud=(unit goof)]
                   ==
                --
            ::
            |=  task=req-task
            ^+  ev-core
            ?-  -.task
            ::  %packet-response-entry-point
            ::
                %heer
              =/  =pact:pact  (parse-packet p.task)
              ?-  +<.pact
                %page  (ev-pact-page lane.task hop.pact +>.pact)
                %peek  (ev-pact-peek +>.pact)
                %poke  (ev-pact-poke lane.task hop.pact +>.pact)
              ==
            ::  %message-response-entry-point
            ::
                %mess
              ?-  -.mess.task
                %page  (ev-mess-page +.mess.task)
                %peek  (ev-mess-peek +.mess.task)
                %poke  (ev-mess-poke [dud +.mess]:task)
              ==
            ::
            ==
          ::
          +|  %request-flow
          ::
          ++  ev-req-plea
            |=  [vane=@tas =wire payload=*]
            ^+  ev-core
            ::  ?>  ?=(%known -.sat.per)
            =^  bone  ossuary.sat.per  ::  XX  to arm?
              =,  ossuary.sat.per
              ?^  bone=(~(get by by-duct) hen)
                [u.bone ossuary.sat.per]
              :-  next-bone  ^+  ossuary.sat.per
              :+  (add 4 next-bone)
                (~(put by by-duct) hen next-bone)
              (~(put by by-bone) next-bone hen)
            ::  handle cork
            ::
            =/  cork=?  =([%$ /cork %cork ~] vane^wire^payload)
            ?:  &(cork !(~(has by by-bone.ossuary.sat.per) bone))
              ~&  "trying to cork {<bone=bone>}, not in the ossuary, ignoring"
              ev-core
            =+  fo-core=(fo-abed:fo hen bone dire=%for)
            =<  fo-abet
            %.  plea/[vane wire payload]
            fo-call:fo-core(closing.state cork)
          ::
          ++  ev-req-boon
            |=  [=bone id=(unit *) load=*]
            ^+  ev-core
            ::  XX handle corked/closing bones
            ::
            =+  fo-core=(fo-abed:fo hen bone dire=%bak)
            =.  ev-core  fo-abet:(fo-call:fo-core boon/load)
            ?~  id
              ev-core
            ?.  %+  gth  (wyt:fo-mop:fo-core loads.state.fo-core)
                msg.cong.ames-state
              ev-core
            (ev-emit:ev-core [/ames]~ %pass /clog %g clog/u.id)
          ::
          ++  ev-req-peek
            |=  [sec=(unit [kid=@ key=@]) =path]
            ^+  ev-core
            ::  +sy-plug should have already stored [kid key path] in chain.ames-state
            ::  on the server, and the client would have retrieved the key via
            ::  the %ames key exchange. here we store it in their peer state
            ::
            =/  =space  ?~(sec publ/life.sat.per shut/[kid key]:u.sec)
            ::
            =?  chums.ames-state  ?=(%shut -.space)
              %+  ~(put by chums.ames-state)  ship.per
              :-  %known
              %_    sat.per
                  client-chain
                (put:key-chain client-chain.sat.per kid.space key.space path)
              ==
            (ev-make-peek space ship.per (ev-make-path space path))
          ::
          +|  %packet-entry-points
          ::
          ++  ev-pact-poke
            |=  [=lane:pact hop=@ud =ack=name:pact =poke=name:pact =data:pact]
            ^+  ev-core
            ::  XX dispatch/hairpin &c
            ::
            ::  - pre-check that we want to process this poke (recognize ack path, ship not blacklisted, &c)
            ::  - initialize our own outbound request for the poke payload
            ::  - start processing the part of the poke payload we already have
            ::    - validation should crash event or ensure that no state is changed
            ::  XX  parse path to get: requester, rift, bone, message
            ::
            ::  path validation/decryption
            ::
            =/  ack=(pole iota)
              ~|  ack-path/pat.ack-name^pat.poke-name
              (ev-validate-path inner:(ev-decrypt-path pat.ack-name her.poke-name))
            =/  [=space cyf=(unit @) =inner=path]
              ~|  inner-path/pat.ack-name^pat.poke-name
              (ev-decrypt-path [pat her]:poke-name)
            =/  pok=(pole iota)
              ~|  pok/pat.ack-name^pat.poke-name
              (ev-validate-path inner-path)
            ::
            ~|  path-validation-failed/ack^pok
            ?>  &(?=(flow-pith ack) ?=(flow-pith pok))
            ::
            ?.  =(her.ack-name our)  ::  do we need to respond to this ack?
              ~&  >>  %not-our-ack^her.ack-name^our
              ev-core  :: XX TODO log
            ?.  =(rcvr.pok our)  ::  are we the receiver of the poke?
              ~&  >  %poke-for-other^[rcvr.pok our]
              ev-core  :: XX TODO log
            ::
            =.  per  (ev-update-lane lane hop ~)
            ::  update and print connection status
            ::  XX  this is implicitly updating chums.state;
            =.  ev-core  (ev-update-qos %live last-contact=now)
            ?.  =(1 (div (add tob.data 1.023) 1.024))
              =/  =dire  :: flow swtiching
                %*(fo-flip-dire fo side *@ud^(fo-infer-dire:fo load.pok))  :: XX assert load is plea/boon
              ::
              =+  fo-core=(fo-abed:fo hen bone.pok dire)
              ?:  (fo-message-is-acked:fo-core mess.pok)
                ::  don't peek if the message havs been already acked
                ~&  >>  "fo-message-is-acked"
                ::
                fo-abet:(fo-send-ack:fo-core mess.pok)
              =/  =^space
                chum/[life.sat.per our life.ames-state symmetric-key.sat.per]
              %+  ev-emit  hen
              [%pass (fo-wire:fo-core %pok) %a meek/[space [her pat]:poke-name]]
            ::  authenticate one-fragment message
            ::
            ~|  data=data
            ?>  %^    ev-authenticate
                    (root:lss (met 3 dat.data)^dat.data)
                  aut.data
                poke-name
            ::
            %:  ev-mess-poke
              ~   :: XX refactor function signature
              her.ack-name^(pout ack)  ::  XX not used
              her.poke-name^(pout pok)
              ;;(gage:mess (cue (ev-decrypt-spac space dat.data cyf)))
            ==
          ::
          ++  ev-pact-peek
            |=  =name:pact
            ?.  =(our her.name)
              ev-core
            =/  res=(unit (unit cage))  (ev-peek ~ /ames %x (name-to-beam name))
            ?.  ?=([~ ~ ^] res)
              ev-core
            (ev-emit hen %give %push ~ !<(@ q.u.u.res))
          ::
          ++  ev-pact-page
            |=  [=lane:pact hop=@ud =name:pact =data:pact =next:pact]
            ^+  ev-core
            ::  check for pending request (peek|poke)
            ::
            =*  pit  pit.sat.per
            =/  [=space cyf=(unit @) =inner=path]
              (ev-decrypt-path pat.name ship.per)
            =*  sealed-path  pat.name
            ?~  res=(~(get by pit) sealed-path)
              ev-core
            ::
            =.  per  (ev-update-lane lane hop next)
            ::  update and print connection status
            ::  XX  this is implicitly updating chums.state;
            =.  ev-core  (ev-update-qos %live last-contact=now)
            ::
            =/  tof  (div (add tob.data 1.023) 1.024)
            ::
            =/  [typ=?(%auth %data) fag=@ud]
              ?~  wan.name
                [?:((gth tof 1) %auth %data) 0]
              [typ fag]:wan.name
            ::
            ?-    typ
                %auth
              ?.  ?|  ?=(~ ps.u.res)
                      =(0 fag)
                      (gth tof 1)
                  ==
                ev-core
              =/  proof=(list @ux)  (rip 8 dat.data)
              ~&  >>>  auth/proof
              ?>  (ev-authenticate (recover-root:verifier:lss proof) aut.data name)
              =/  state  (init:verifier:lss tof proof)
              =.  chums.ames-state
                %+  ~(put by chums.ames-state)  her.name
                =-  known/sat.per(pit -)
                %+  ~(put by pit)  sealed-path
                u.res(ps `[state ~])
              ::
              ~&  >>  "request next fragment"^fag
              ::  request next fragment
              ::
              (ev-push-pact [hop=0 %peek name(wan [%data 0])] lane.sat.per)
            ::
                %data
              ::  do we have packet state already?
              ::
              ?~  ps.u.res
                ::  is this this a standalone (jumbo or 1-frag) message?
                ::
                =/  mod  (bex (dec boq.name))  :: XX unguarded
                ?:  =(1 (div (add tob.data (dec mod)) mod))
                  ~&  [tob=tob.data met=(met 3 dat.data)]
                  ?>  ?=(%& -.aut.data)
                  ?>  (ev-authenticate (root:lss tob.data^dat.data) aut.data name)
                  =/  =spar  [her.name inner-path]
                  =/  =auth:mess  p.aut.data
                  =/  res=@  (ev-decrypt-spac space dat.data cyf)
                  ::  if %chum/%shut, we need to pass the sealed-path to find it
                  ::  in the pit.fren-state and then remove it
                  ::
                  %*($ ev-mess-page sealed-path `sealed-path, +< spar^auth^res)
                ::  XX handle out-of-order packet
                ::
                !!
              ::  yes, we do have packet state already
              ::
              =*  ps  u.ps.u.res
              ?.  =(counter.los.ps fag)
                ev-core
              ::  extract the pair (if present) and verify
              ::
              ?>  ?=(%| -.aut.data)
              =/  pair=(unit [l=@ux r=@ux])  p.aut.data
              ::  update packet state
              ::
              =/  leaf=octs
                ?.  =(+(fag) leaves.los.ps)
                  1.024^dat.data
                (met 3 dat.data)^dat.data
              =.  los.ps   (verify-msg:verifier:lss los.ps [leaf pair])
              =.  fags.ps  [dat.data fags.ps]
              =.  chums.ames-state
                %+  ~(put by chums.ames-state)  her.name
                =-  known/sat.per(pit -)
                %+  ~(put by pit)  sealed-path
                u.res
              ::  is the message incomplete?
              ::
              ?.  =(+(fag) leaves.los.ps)
                ::  request next fragment
                ::
                %+  ev-push-pact
                  [hop=0 %peek name(wan [%data counter.los.ps])]
                lane.sat.per
              ::  yield complete message
              ::
              ~&  >>  "yield complete message"^fag
              =/  =spar  [her.name inner-path]
              =/  =auth:mess  [%| *@uxH] :: XX should be stored in ps?
              =/  res=@  (ev-decrypt-spac space (rep 13 (flop fags.ps)) cyf)
              ::  if %chum/%shut, we need to pass the sealed-path to find it
              ::  in the pit.fren-state and then remove it
              ::
              %*($ ev-mess-page sealed-path `sealed-path, +< [spar auth res])
            ==
          ::
          +|  %messages-entry-point
          ::
          ::  XX call +ev-update-qos again in the message layer?
          ::
          ++  ev-mess-page
            =|  sealed-path=(unit path)   ::  XX set if coming from the packet layer
            |=  [=spar =auth:mess res=@]  ::  XX res and path.spar have been decrypted
            ^+  ev-core
            =*  ship  ship.spar
            ?>  =(ship.per ship.spar)
            :: ?~  rs=(~(get by chums.ames-state) ship)
            ::   ev-core
            :: ?>  ?=([~ %known *] rs)  ::  XX alien agenda
            =+  path=?~(sealed-path path.spar u.sealed-path)
            ?~  ms=(~(get by pit.sat.per) path)
              ev-core
            :: =.  per  ship^+.u.rs
            ::
            ::  XX validate response
            =.  pit.sat.per  (~(del by pit.sat.per) path)
            ~|   gage-res-failed/`@ux`res
            =+  ;;(=gage:mess (cue res))
            ?>  ?=(^ gage)
            (ev-give-response for.u.ms path.spar gage)
          ::
          ++  ev-mess-poke  :: XX refactor function signature; ack-spar not used
            |=  [dud=(unit goof) =ack=spar =pok=spar =gage:mess]
            ^+  ev-core
            =+  ?~  dud  ~
                %-  %+  slog  leaf+"mesa: message crashed {<mote.u.dud>}"
                    tang.u.dud
                ::  XX what if the crash is due to path validation
                ::  and we can't infer the sequence number?
                ~
            =/  pok=(pole iota)  (ev-pave path.pok-spar)
            ~|  poke-path-failed/path.pok-spar
            ?>  ?=(flow-pith pok)
            ::
            ::  the packet layer has already validated that this is a valid %poke
            ::
            ::  XX ev-got-per; assumes that %aliens are checked in the packet layer
            :: =.  per  (ev-got-per ship.pok-spar)
            :: ::  ?>  ?=(%known -.sat.per)
            ::
            =/  =dire  :: flow swtiching
              %*(fo-flip-dire fo side *@ud^(fo-infer-dire:fo load.pok))  :: XX assert load is plea/boon
            ::
            =/  req=mesa-message
              ~|  gage-parsing-failed/gage
              ?>  ?=([%message *] gage)  :: XX [%message %mark *] ??
              ?:  =(%for dire)  ::  %boon(s) sink forward (reversed %plea direction)
                ?>(?=([%boon *] +.gage) +.gage)
              ?>  =(%bak dire)  ::  %pleas(s) and %corks sink backward
              ?>  ?=([%plea *] +.gage)
              ;;([%plea plea] +.gage)
            ::
            =<  fo-abet
            %.  [%sink mess.pok req ?=(~ dud)]
            fo-call:(fo-abed:fo hen bone.pok dire)
          ::
          ++  ev-mess-peek
            |=  =spar
            ?.  =(our ship.spar)
              ev-core
            =/  res=(unit (unit cage))
              !!  :: scry for path
            ?.  ?=([~ ~ ^] res)
              ev-core
            ::  XX [%give %response %page p.mess [p q.q]:u.u.res]
            ev-core
          ::
          +|  %take-responses
          ::
          ++  ev-take-wake
            |=  [=wire error=(unit tang)]
            ^+  ev-core
            =>  .(wire `(pole iota)`(ev-pave wire))
            ?.  ?=([%mesa %dead-flow ~] wire)
              ~&  %evil-behn-timer^wire  ev-core
            ::  XX log if error
            ::  XX if we wake up too early, no-op, otherwise set new timer
            ::  XX if timed-out, update qos
            ::  XX expire direct route if the peer is not responding (%nail)
            ::  XX re-send comet attestation?
            ::  XX only timed-out (dead) outgoing %poke requests
            ::
            =.  flow.dead.ames-state
              flow/`[~[/ames] /mesa/dead-flow `@da`(add now ~m2)]
            =.  ev-core
              %+  ev-emit  ~[/ames]
              [%pass /mesa/dead-flow %b %wait `@da`(add now ~m2)]
            =+  ames-core=(ev:ames hen ames-state)
            =^  moves  ames-state  abet:(wake-dead-flows:ames-core error)
            =.  ev-core  sy-abet:sy-prod:sy
            (ev-emil moves)
          ::
          ++  ev-take-flub
            |=  =wire
            ^+  ev-core
            ?~  flow-wire=(ev-validate-wire wire)
              ev-core
            =,  u.flow-wire
            =.  per  (ev-got-per her)
            :: ::  ?>  ?=(%known -.sat.per)
            ?:  (lth rift rift.sat.per)
              :: XX log
              ev-core  ::  ignore events from an old rift
            ?>  ?=([%van %bak] [were dire])
            fo-abet:(fo-take:(fo-abed:fo hen bone dire=%bak) %van flub/~)
          ::  +ev-take-response: receive remote responses
          ::
          ++  ev-take-response
            |=  [=wire =sage:mess]
            ^+  ev-core
            ?~  flow-wire=(ev-validate-wire wire)
              ::  check if attestation proof response
              ::
              ?.  ?=([%mesa %comet %proof ~] wire)
                ev-core
              ::
              ?>  ?=(%pawn (clan:title ship.p.sage))
              =+  path=`(pole iota)`(ev-pave path.p.sage)
              ?>  ?=(poof-pith path)
              ::  this is an attestation for us, at our current life
              ::
              ?>  &(=(our rcvr.path) =(life.path life.ames-state))
              ?>  ?=([%message *] q.sage)
              ::
              =+  ;;([signature=@ signed=@] (cue ;;(@ +.q.sage)))
              =+  ;;(comet-proof=open-packet (cue signed))
              ::  update comet's keys
              ::
              (ev-register-comet ship.p.sage comet-proof signature signed)
            =,  u.flow-wire
            =.  per  (ev-got-per her)
            ?:  (lth rift rift.sat.per)
              :: XX log
              ev-core  ::  ignore events from an old rift
            ::
            =/  message-path=(pole iota)  (ev-validate-path path.p.sage)
            ::
            ?:  =(were %cor)
              ::  validate %cork path
              ::
              ?>  ?=(cork-pith message-path)
              ::  if we don't crash, the client has removed the flow,
              ::  and have succesfully +peek'ed the %cork
              ::
              =<  fo-abel
              %.(sage fo-take-client-cork:(fo-abed:fo hen bone dire=%bak))
            ::
            ::  XX  validate thath wire and path match?
            ::
            ?>  ?=(flow-pith message-path)
            ::
            ::  XX replaced by the flow "dire"ction ?(%for %bak)
            ::  based on the bone we can know if this payload is an ack?
            ::  bone=0                                   bone=1
            ::  response   <=  ack payloads        =>       response
            ::             <=  boon/poke payloads  =>
            ::
            ::  bones for acks are "internal", -- triggered by internal requests
            ::  for %poke payloads "external" -- triggered by hearing a request
            ::
            ?:  =(%pok were)
              ::  XX ack-path not used
              (ev-mess-poke ~ ack-path=our^/ her^(pout message-path) q.sage)
            ::  wires are tagged ?(%int %ext) so we can diferentiate if we are
            ::  proessing an ack or a naxplanation payload
            ::
            =/  fo-core
              ::  XX parse $ack payload in here, and call task instead?
              %-  fo-take:(fo-abed:fo hen bone dire)
              [were mess-response/[mess.message-path sage]]
            ::
            ?.  can-be-corked.fo-core
              fo-abet:fo-core
            ::  we received the %ack for the %cork %plea;
            ::  remove the flow and it's associated bone in the ossuary;
            ::  expose %cork flow in the namespace "~(put in corked)"
            ::
            ::  XX to arm
            =.  sat.per
              =,  sat.per
              %_  sat.per
                flows            (~(del by flows) bone^dire)
                corked           (~(put in corked) bone^dire)
                by-duct.ossuary  (~(del by by-duct.ossuary) (ev-got-duct bone))   ::  XX bone^side=%for
                by-bone.ossuary  (~(del by by-bone.ossuary) bone)                 ::  XX bone^side=%for
              ==
            :: XX  call fo-abet:fo-core instead
            %_    ev-core
                ames-state
              ames-state(chums (~(put by chums.ames-state) [ship known/sat]:per))
            ==
          ::  +ev-take-done: vane responses
          ::
          ++  ev-take-boon
            |=  [=wire =gift]
            ^+  ev-core
            ?~  flow-wire=(ev-validate-wire wire)
              ev-core
            =,  u.flow-wire
            =.  per  (ev-got-per her)
            ::  ?>  ?=(%known -.sat.per)
            ?:  (lth rift rift.sat.per)
              :: XX log
              ev-core  ::  ignore events from an old rift
            ?>  ?=([%van %bak] [were dire])  ::  vane acks happen on backward flows
            %+  ev-req-boon  bone
            ?+  -.gift  !!
              %boon  `payload.gift
              %noon  [`id payload]:gift
            ==
          ::  +ev-poke-done: vane acks
          ::
          ++  ev-poke-done
            |=  [=wire error=(unit error)]
            ^+  ev-core
            ?~  flow-wire=(ev-validate-wire wire)
              ev-core
            =,  u.flow-wire
            =.  per  (ev-got-per her)
            ::  ?>  ?=(%known -.sat.per)
            ?:  (lth rift rift.sat.per)
              :: XX log
              ev-core  ::  ignore events from an old rift
            ?>  ?=([%van %bak] [were dire])  ::  vane acks happen on backward flows
            ::
            =<  fo-abet
            ::  XX since we ack one message at at time, seq is not needed?
            ::  XX use it as an assurance check?
            ::
            %.  [%van done/error]
            fo-take:(fo-abed:fo hen bone dire=%bak)
          ::
          +|  %message-constructor
          ::
          ::  XX remove all spaces from the task, and make the paths at the callsite?
          ::
          ++  ev-make-peek  |=([=space p=spar] (ev-make-mess p ~))
          ++  ev-make-poke
            |=  [=space =ack=spar =poke=path]
            ::  XX  make all paths when the %mako task is sent?
            ::
            =.  path.ack-spar   (ev-make-path space path.ack-spar)
            =.  poke-path
              =?  space  ?=(?(%publ %chum) -.space)
                ::  switch life(s) and ship, for payloads
                ::  XX  test that these lifes are correctly checked in the +scry handler
                ::
                ::  lifes need to be switched since for %pokes,
                ::  this is a payload in our namespace
                ::
                ?:  ?=(%publ -.space)
                  space(life life.ames-state)
                space(our-life her-life.space, her-life our-life.space, her ship.ack-spar)
              (ev-make-path space poke-path)
            ::
            ::  ack and poke paths are already encrypted at this point
            ::
            (ev-make-mess ack-spar `poke-path)
          ::  +ev-make-mess: this arm doesn't use the peer from the door's
          ::  sample sat.per, due to the way comets are handled in when reading
          ::  attestation proofs.and therefore don't use the +abed/+abet pattern
          ::
          ++  ev-make-mess
            |=  [p=spar q=(unit path)]
            ^+  ev-core
            =/  [=rift her=chum-state]
              =/  her  (~(get by chums.ames-state) ship.p)
              ?:  ?=([~ %known *] her)
                [rift .]:u.her
              ::  only an %alien %comet is allowed to use the pit
              ::
              ?>  ?=([~ %alien *] her)
              ?>  ?=(%pawn (clan:title ship.p))
              [0 u.her]
            =/  pit
              ?:(?=(%known -.her) pit.her pit.her)          ::  XX find-fork
            ?^  res=(~(get by pit) path.p)
              ?>  =(q pay.u.res)  ::  prevent overriding payload
              =-  ev-core(chums.ames-state -)
              %+  ~(put by chums.ames-state)  ship.p
              =.  pit
                %+  ~(put by pit)  path.p
                u.res(for (~(put in for.u.res) hen))
              ?:(?=(%known -.her) her(pit pit) her(pit pit))   ::  XX find-fork
            ::
            =/  =pact:pact  (ev-make-pact p q rift)
            =|  new=request-state
            =.  for.new   (~(put in for.new) hen)
            =.  pay.new   q
            =.  chums.ames-state
              %+  ~(put by chums.ames-state)  ship.p
              =.  pit  (~(put by pit) path.p new)
              ?:(?=(%known -.her) her(pit pit) her(pit pit))   ::  XX find-fork
            ::
            (ev-push-pact pact ?:(?=(%alien -.her) ~ lane.her))
          ::
          ++  ev-make-pact
            |=  [p=spar q=(unit path) =per=rift]
            ^-  pact:pact
            =/  nam  [[ship.p per-rift] [13 ~] path.p]
            ?~  q
              [hop=0 %peek nam]
            ::  XX if path will be too long, put in [tmp] and use that path
            :: (mes:plot:d (en:name:d [[her=~nec rif=40] [boq=0 wan=~] pat=['c~_h' ~]]))
            :: [bloq=q=3 step=r=12]
            ::  =/  has  (shax u.u.res)
            ::  =.  tmchums.ames-state  (~(put by tmchums.ames-state) has [%some-envelope original-path u.u.res])
            ::  //ax/[$ship]//1/temp/[hash]
            ::
            =/  man=name:pact  [[our rift.ames-state] [13 ~] u.q]
            ::
            ~|  [%no-page man=man]
            [hop=0 %poke nam man (need (ev-get-page man))]
          ::
          ++  ev-make-path
            |=  [=space =path]
            ^+  path
            =>  [space=space path=path ..crypt]
            :: ~>  %memo./ames/mess-spac
            ?-    -.space
                %publ  `^path`[%publ (scot %ud life.space) path]  :: unencrypted
            ::
                %chum  :: encrypted with eddh key
              :-  %chum
              ^+  path
              :~  (scot %ud our-life.space)
                  (scot %p her.space)
                  (scot %ud her-life.space)
                  (scot %uv (seal-path:crypt `@`key.space path))
              ==
            ::
                %shut  :: encrypted with group key
              :: key provided by the %keen task, or retrieved from client-chain.per.sat
              ::
              =/  cyf  (seal-path:crypt key.space path)
              /shut/[(scot %ud kid.space)]/[(scot %uv cyf)]
            ==
          ::
          ++  ev-get-page
            |=  =name:pact
            ^-  (unit data:pact)
            =/  res=(unit (unit cage))
              (ev-peek ~ /ames-get-page %x (name-to-beam name))  :: XX
            ?.  ?=([~ ~ %atom *] res)
              ~
            =;  page=pact:pact
              ?>(?=(%page +<.page) `q.page)
            =>  [res=res de=de:pact]
            :: ~>  %memo./ames/get-page  :: XX unnecessary?
            =+  ;;([pac=@ *] q.q.u.u.res)
            -:($:de pac)
          ::
          +|  %fren-helpers
          ::
          ++  ev-gut-per
            |=  =ship
            ^+  per
            :-  ship
            =/  chum-state  (~(get by chums.ames-state) ship)
            ?.(?=([~ %known *] chum-state) *fren-state +.u.chum-state)
          ::
          ++  ev-got-per
            |=  =ship
            ^+  per
            :-  ship
            ~|  %freaky-alien^ship
            =-  ?>(?=([%known *] -) ->)
            (~(got by chums.ames-state) ship)
          ::  +ev-get-per: lookup .her state, ~ if missing, [~ ~] if %alien
          ::
          ++  ev-get-per
            |=  her=ship
            ^-  (unit (unit chum-state))
            ::
            ?~  per=(~(get by chums.ames-state) her)  ~
            `per
          ::
          ++  ev-put-per
            |=  =ship
            ^+  ames-state
            ames-state(chums (~(put by chums.ames-state) ship known/*fren-state))
          ::
          ++  ev-got-duct
            |=  =bone
            ^-  duct
            ::  ?>  ?=(%known -.sat.per)
            ~|(%dangling-bone^ship.per^bone (~(got by by-bone.ossuary.sat.per) bone))
          ::
          +|  %peek-subscribers
          ::
          ++  ev-give-response
            |=  [listeners=(set duct) =path =gage:mess]
            ^+  ev-core
            %-  ~(rep in listeners)
            |=  [hen=duct c=_ev-core]
            (ev-emit:c hen %give %mess-response ship.per^path gage)
          ::
          ++  ev-cancel-peek
            |=  [all=? =path]  :: XX namespace?
            ^+  ev-core
            ?~  ms=(~(get by pit.sat.per) path)
              ev-core
            ?:  all
              =.  pit.sat.per  (~(del by pit.sat.per) path)
              (ev-give-response for.u.ms path ~)
            =.  for.u.ms  (~(del in for.u.ms) hen)
            =.  pit.sat.per
              ?~  for.u.ms
                (~(del by pit.sat.per) path)
              (~(put by pit.sat.per) path u.ms)
            ev-core
          ::
          +|  %flows
          ::
          ++  fo
            ::  flows exist only for known peers
            ::
            :: =>  .(sat.per ?>(?=(%known -.sat.per) sat.per))
            ::
            =|  can-be-corked=?(%.y %.n)
            ::
            |_  [[hen=duct =side] state=flow-state]
            ::
            +*  ::veb   veb.bug.channel  ::  XX TODO
                bone  bone.side
                dire  dire.side
                her   ship.per
            ::
            +|  %helpers
            ::
            ++  fo-core  .
            ++  fo-abed
              |=  [=duct =^side]
              ::  XX use got by in another arm to assert when the flow should exist
              =.  state  (~(gut by flows.sat.per) side *flow-state)
              fo-core(hen duct, side side)
            ::
            ++  fo-abet
              ^+  ev-core
              =.  flows.sat.per  (~(put by flows.sat.per) bone^dire state)
              %_    ev-core
                  ames-state
                ames-state(chums (~(put by chums.ames-state) her known/sat.per))
              ==
            ::
            ++  fo-abel
              ^+  ev-core
              ::
              =:  flows.sat.per   (~(del by flows.sat.per) bone^dire)
                  corked.sat.per  (~(put in corked.sat.per) bone^dire)
                ==
              =.  ames-state
                ames-state(chums (~(put by chums.ames-state) her known/sat.per))
              ev-core
            ::
            ++  fo-emit      |=(=move fo-core(moves [move moves]))
            ++  fo-emil      |=(mos=(list move) fo-core(moves (weld mos moves)))
            ++  fo-to-close
              ::  if the flow is in closing, only allow sending the %cork %plea
              ::
              |=  poke=mesa-message
              ?&(closing.state !=(poke [%plea %$ /cork %cork ~]))
            ::
            ++  fo-flip-dire  ?:(=(dire %for) %bak %for)
            ::  +fo-infer-dire: infer the side that's producing this payload
            ::  (e.g. when hearing a +peek request for this path, if the payload
            ::  is a %plea, is always produced on the %for side)
            ::
            ++  fo-infer-dire
              |=  command=?(%plea %boon %ack-plea %ack-boon %nax)  ::  to %lull
              ?-  command
                %plea      %for
                %boon      %bak
                %nax       %bak  ::  XX naxplanation read only by plea sender?
                %ack-plea  %bak
                %ack-boon  %for
              ==
            ::  +fo-add-command: when binding a payload we produce
            ::
            ++  fo-infer-load
              |=  command=?(%ack %poke)
              ?:  &(?=(%poke command) ?=(%for dire.side))  %plea        ::  /~a/poke/~b/for
              ?:  &(?=(%poke command) ?=(%bak dire.side))  %boon        ::  /~a/poke/~b/bak
              ?:  &(?=(%ack command) ?=(%for dire.side))   %ack-boon    ::  /~b/ack/~a/bak
              ?>  &(?=(%ack command) ?=(%bak dire.side))   %ack-plea    ::  /~b/ack/~a/for
            ::
            ++  fo-message-is-acked  |=(seq=@ud =(seq last-acked.state))
            ++  fo-message-not-in-range
              |=  seq=@ud
              ^-  ?
              ?&  (gth seq +(last-acked.state))           ::  future ack
                  ?|  (lte seq last-acked.state)
                      (gth (sub last-acked.state seq) 10) ::  too far ack
              ==  ==
            ::
            +|  %builders
            ::
            ++  fo-mop  ((on ,@ud mesa-message) lte)
            ++  fo-cac  ((on ,@ud ?) lte)
            ++  fo-ack-path  |=([s=@ r=@p] (fo-path s (fo-infer-load %ack) r))
            ++  fo-pok-path  |=([s=@ r=@p] (fo-path s (fo-infer-load %poke) r))
            ++  fo-nax-path  |=([s=@ r=@p] (fo-path s %nax r))
            ++  fo-cor-path  |=([s=@ r=@p] (fo-path s %cork r))
            ++  fo-path
              |=  [seq=@ud =load rcvr=@p]  :: XX lead from lull
              ^-  path
              :*  vane=%a  care=%x  case='1'  desk=%$
                ::
                  %flow  (scot %ud bone)  load  rcvr=(scot %p rcvr)
                ::  %corks refers to the whole flow; skip the sequence number
                ::
                  ?:(=(%cork load) ~ [(scot %ud seq) ~])
              ==
            ::
            ++  fo-wire
              ::  XX better names
              ::  $?  for-acks=%int
              ::      for-nax-payloads=%ext
              ::      to/from-vane=%van
              ::      for-corks=%cor
              ::      for-poke-payloads=%pok
              ::  ==
              ::
              |=  were=?(%int %ext %van %cor %pok)
              ^-  wire
              ::  %for: %plea(s) are always sent forward, %boon(s) %bak
              ::  both .to-vane and .dire are asserted when receiving the vane %ack
              ::  since they will always be %van and %bak
              ::
              :~  %mesa  %flow  were  dire
                  rcvr=[(scot %p her)]
                :: add rift to avoid dangling bones from previous eras
                ::
                  rift=[(scot %ud rift.sat.per)]
                  bone=[(scot %ud bone)]
              ==
            ::
            +|  %entry-points
            ::
            ++  fo-call
              =>  |%
                  +$  poke-task
                    $%  [%sink seq=@ud mess=mesa-message ok=?]
                        ::  XX remove %fo-planation from lull
                        mesa-message
                    ==
                  --
              ::
              |=  poke=poke-task
              ^+  fo-core
              ::
              ?-    -.poke
                  ?(%plea %boon)
                ?:  |((fo-to-close poke) (~(has in corked.sat.per) side))
                  ~&  >>>  %to-close-corked
                  ::  XX log
                  fo-core
                =:  next-load.state  +(next-load.state)
                    loads.state      (put:fo-mop loads.state next-load.state poke)
                  ==
                fo-send
                ::
                  %sink
                ~|  mess.poke
                ::  a %plea sinks on the backward receiver (from a forward flow)
                ::  a %boon sinks on the forward receiver (from a backward flow)
                ::
                ?-  dire
                  %bak  ?>(?=(%plea -.mess.poke) (fo-sink-plea [seq +.mess ok]:poke))
                  %for  ?>(?=(%boon -.mess.poke) (fo-sink-boon [seq +.mess ok]:poke))
                ==
              ==
            ::
            ++  fo-take
              |=  [were=?(%ext %int %van %cor %pok) sign=flow-sign]
              ^+  fo-core
              ?-  -.sign
                  %done  ?>(?=(%van were) (fo-take-done +.sign))  :: ack from client vane
                  %flub  ?>(?=(%van were) fo-core(pending-ack.state %.n))
              ::
                  %mess-response
                ?+  were  !!  :: %pok is handle outside
                  :: XX payload given by the packet layer
                  :: via the wire used when %pass %a peek-for-poke
                  :: and only handled there?
                  %ext  (fo-take-naxplanation +.sign)
                  %int  (fo-take-ack +.sign)
                  %cor  (fo-take-client-cork +>.sign)
                ==
              ==
            ::
            ++  fo-peek
              |=  [=load seq=@ud]
              ^-  (unit page)
              ::  XX assert flow direction?
              ::  %ack and %nax can be both %for (%plea) and %bak (%boon)
              ::
              ?-    load
                  %nax
                ?~(nax=(~(get by nax.state) seq) ~ `nax/u.nax)
              ::
                  ?(%ack-plea %ack-boon)
                ?:  (lte seq line.state)
                  ::  refuse to answer for pre-migration messages
                  ::
                  ~
                ?:  ?&  (lth seq last-acked.state)
                        (gth (sub last-acked.state seq) 10)
                    ==
                  :: if seq > gth 10, refuse to answer
                  ::
                  ~
                ?.  =(seq last-acked.state)
                  ::  refuse to answer for future acks
                  ::
                  ~
                `ack/(~(has by nax.state) seq)
              ::
                  %cork
                ?.  (~(has in corked.sat.per) side)  ~
                ~&  >>  corked/corked.sat.per
                `gone/~
              ::
                  ?(%plea %boon)
                ?~  v=(get:fo-mop loads.state seq)   ~
                ?>  =(load -.u.v)
                ?-  -.u.v
                  %plea  `plea/[vane path payload]:u.v
                  %boon  `boon/payload.u.v
                ==
              ==
            ::
            +|  %request-sender
            ::
            ++  fo-send
              ^+  fo-core
              =+  loads=loads.state ::  cache
              |-  ^+  fo-core
              =*  loop  $
              =+  num=(wyt:fo-mop loads)
              ?:  =(0 num)
                fo-core
              ?.  (gth send-window.state 0)
                fo-core
              ::
              =^  [seq=@ud request=mesa-message]  loads  (pop:fo-mop loads)
              =.  send-window.state  (dec send-window.state)
              ::  XX %ames call itself with a %moke task
              ::  on a wire used to infer the listener (the %poke %plea request; this)
              ::  when getting the %response $page with the %ack (tagged with %int)
              ::  and similarly for %naxplanation payloads (tagged with %ext)
              ::
              ::  XX  namespace encoding here, or inside +ev-make-poke?
              :: =/  paths=[spar path]
              ::   :-  =/  =ack=space
              ::         chum/[life.sat.per our life.ames-state symmetric-key.sat.per]
              ::       her^(ev-make-path ack-space (fo-ack-path seq her our))
              ::   =/  =poke=space
              ::     chum/[life.ames-state ship.per [life symmetric-key]:sat:per]
              ::   (ev-make-path poke-space (fo-pok-path seq our her))
              =/  paths=[spar path]
                :_  (fo-pok-path seq her)
                :-  her
                ::  an %ack is on the other side
                ::
                (%*(fo-ack-path fo-core dire.side fo-flip-dire) seq our)
              =/  =space   chum/[life.sat.per our life.ames-state symmetric-key.sat.per]
              =/  =wire    (fo-wire %int)
              =.  fo-core  (fo-emit hen %pass wire %a moke/[space paths])
              loop
            ::
            +|  %request-receiver
            ::
            ++  fo-sink-boon
              |=  [seq=@ud message=* ok=?]
              ^+  fo-core
              ::  XX check that the message can be acked (not in future, or far back past)
              ::
              ?:  (gth seq +(last-acked.state))
                ::  no-op if future message
                ~&  future-ack/seq=seq^last-acked=last-acked.state
                fo-core
              ?:  (lte seq last-acked.state)
                ?:  (gth (sub last-acked.state seq) 10)
                  ~&  %drop-old-seq  ::  XX  use verbosity logs
                  fo-core
                ~&  %already-acked  ::  XX  use verbosity logs
                (fo-send-ack seq)
              =.  fo-core  (fo-emit (ev-got-duct bone) %give %boon message)
              ::  handle a previous crash
              ::  XX revisit
              ::
              =?  moves  !ok
                ::  we previously crashed on this message; notify client vane
                ::
                %+  turn  moves
                |=  =move
                ?.  ?=([* %give %boon *] move)  move
                [duct.move %give %lost ~]
              ::  ack unconditionally
              ::
              =.  last-acked.state  +(last-acked.state)
              (fo-send-ack last-acked.state)
            ::
            ++  fo-sink-plea
              |=  [seq=@ud =plea ok=?]
              ^+  fo-core
              ::  receiver of a %plea request
              ::
              ::  XX check that the message can be acked (not in future, or far back past)
              ::
              ?:  (gth seq +(last-acked.state))
                ::  no-op if future message
                ~&  %future-ack  ::  XX  use verbosity logs
                fo-core
              ?:  (lte seq last-acked.state)
                ?:  (gth (sub last-acked.state seq) 10)
                  ~&  %drop-old-seq  ::  XX  use verbosity logs
                  fo-core
                ::  XX  use verbosity logs
                (fo-send-ack seq)
              ?.  ok
                %.  `*error
                fo-take-done:fo-core(pending-ack.state %.y)
              ::
              =/  =wire  (fo-wire %van)
              ?:  &(=(vane %$) ?=([%ahoy ~] payload) ?=([%mesa ~] path)):plea
                ::  migrated %ahoy pleas are always acked
                ::
                (fo-take-done:fo-core(pending-ack.state %.y) ~)
              ?.  &(=(vane %$) ?=([%cork ~] payload) ?=([%cork ~] path)):plea
                =.  fo-core
                  %-  fo-emit
                  ?+  vane.plea  ~|  %mesa-evil-vane^our^her^vane.plea  !!
                    ?(%c %e %g %j)  [hen %pass wire vane.plea %plea her plea]
                  ==
                ::
                fo-core(pending-ack.state %.y)
              ::  publisher receives %cork
              ::  mark flow as closing
              ::  publish %cork %ack (in +ev-mess-poke) in corked.sat.per
              ::
              =.  fo-core
                %-  fo-emit
                ::  start %peek request to check if they have corked the flow
                ::  after reading the ack from our namespace
                ::
                =/  =space  chum/[life.sat.per our life.ames-state symmetric-key.sat.per]
                =/  =path   (ev-make-path space (fo-cor-path seq our))
                [hen %pass wire=(fo-wire %cor) %a meek/space^her^path]
              ::  XX just fo-core(closing.state %.y)?
              (fo-take-done:fo-core(closing.state %.y, pending-ack.state %.y) ~)
            ::
            +|  %from-vane
            ::
            ++  fo-take-done
              |=  error=(unit error)
              ^+  fo-core
              ::  if there's a pending-vane ack, is always +(last-acked)
              ::
              ?>  =(%.y pending-ack.state)
              =/  seq=@ud  +(last-acked.state)
              =:  last-acked.state   seq
                  pending-ack.state  %.n
                ==
              =?  fo-core  ?=(^ error)   :: XX use verbosity flag
                (fo-emit hen %pass /crud %d %flog %crud u.error)
              =?  nax.state  ?=(^ error)
                =?  nax.state  (gth seq 10)
                  ::  only keep the last 10 nacks
                  ::
                  (~(del by nax.state) (sub seq 10))
                (~(put by nax.state) seq u.error)
              (fo-send-ack seq)
            ::
            +|  %from-network
            ::
            ++  fo-take-ack
              |=  [seq=@ud =spar =gage:mess]
              ^+  fo-core
              ::  only handle acks for %pokes that have been sent
              ::
              ?.  (lth seq next-load.state)
                :: XX log?
                fo-core
              ::  if all pokes have been processed no-op
              ::
              ?~  first=(pry:fo-mop loads.state)
                fo-core
              ?>  ?=([%message *] gage)
              =+  ;;([%ack error=?] +.gage)  ::  XX
              ?.  =(key.u.first seq)
                ::  XX we shouldn't see this since send-window is always 1
                ::
                ~&  >>>  out-of-order-ack/seq=seq^first=key.u.first
                :: if the ack we receive is not for the first, save it
                ::  XX if error, start +peeking right away?
                ::
                fo-core(cache.state (put:fo-cac cache.state seq error))
              |-  ^+  fo-core
              ?:  error
                ~&  >>  "error: start %peek for naxplanation "^gage
                ::  if error start %peek for naxplanation
                ::
                (fo-peek-naxplanation seq)
              ::  ack is for the first, oldest pending-ack sent message;
              ::  remove it and XX start processing cached acks
              ::
              =^  *  loads.state  (del:fo-mop loads.state seq)
              ::  increase the send-window so we can send the next message
              ::
              =.  send-window.state  +(send-window.state)
              =.  can-be-corked
                ?&  closing.state      ::  we sent a %cork %plea
                    ?=(~ loads.state)  ::  nothing else is pending
                ==
              =.  fo-core
                =~  fo-send  ::  send next messages
                  ::
                    ?:  ?|  ?=(%bak dire)  ::  %boon %ack; assumed %acked from vane
                            can-be-corked  ::  %cork %ack; implicit ack
                        ==
                      fo-core
                    ::  don't give %done for %boon and %cork; implicit %ack
                    ::
                    (fo-emit (ev-got-duct bone) %give %done ~)
                ==
              ::  are there any cached acks?
              ::
              ?~  cack=(pry:fo-cac cache.state)  fo-core
              ?.  =(key.u.cack +(seq))           fo-core
              ::  first ack in the cache is the next sent %poke; process
              ::
              =^  *  cache.state  (del:fo-cac cache.state key.u.cack)
              $(error val.u.cack, seq key.u.cack)
            ::
            ++  fo-take-naxplanation
              |=  [seq=@ud =spar =gage:mess]
              ^+  fo-core
              ::  XX same as fo-take-ack refactor
              ::
              =/  next-load=@ud  ?~(next=(ram:fo-mop loads.state) 1 key.u.next)
              ?:  (gth seq next-load)
                :: XX log?
                ~&  >>  "fo-take-naxplanation: future message, no-op"
                fo-core
              ::  if all pokes have been processed no-op
              ::
              ?~  first=(pry:fo-mop loads.state)
                ~&  >>  "fo-take-naxplanation: all pokes have been processed, no-op"
                fo-core
              :: XX  if the ack we receive is not for the first, no-op
              :: XX as currently implemented we only hear for the naxplanation of the
              ::  oldest message
              ::
              ?.  =(key.u.first seq)
                ~&  >>  "fo-take-naxplanation: naxplanation is not for the first load, no-op"
                fo-core
              ~&  >  "fo-take-naxplanation: seq={<seq>}"
              ::  ack is for the first, oldest pending-ack set message, remove it
              ::
              =^  *  loads.state  (del:fo-mop loads.state seq)
              ::  increase the send-window so we can send the next message
              ::
              =.  send-window.state  +(send-window.state)
              =.  fo-core            fo-send  ::  send next messages
              ::  XX check path.spar
              ::  XX path.spar will be the full namespace path, peel off before?
              ::  XX clear timer for the failed %poke
              ::
              ?>  ?=([%message *] gage)
              =+  ;;([%nax =error] +.gage)  ::  XX
              (fo-emit (ev-got-duct bone) %give %done `error)
            ::
            ++  fo-take-client-cork
              |=  [=spar =gage:mess]
              ^+  fo-core
              ::  sanity checks on the state of the flow
              ::
              ~|  [gage/gage state]
              ?>
              ?&  ?=([%message %gone ~] gage)             :: client corked the flow
                  !pending-ack.state                      :: there are no pending acks
                  closing.state                           :: the flow is in closing
                  !(~(has by nax.state) last-acked.state) :: the %cork was not nacked
              ==
              fo-core
            ::
            +|  %internals
            ::
            ++  fo-send-ack
              |=  seq=@ud
              ^+  fo-core
              ::  emit (n)ack to unix; see +fo-peek where the (n)ack is produced
              ::
              =/  =path   (fo-ack-path seq her)
              =/  =space  chum/[life.ames-state her [life symmetric-key]:sat.per]
              =/  =name:pact
                [[our rift.ames-state] [13 ~] (ev-make-path space path)]
              ?~  page=(ev-get-page name)
                :: XX !!? log?
                fo-core
              =;  core=_ev-core
                fo-core(ev-core core)
              %+  ev-push-pact
                [hop=0 page/[name u.page ~]]
              ::  XX  check here if we have a lane, and if not, assume that it came
              ::  via a sponsor, to avoid breaking symmetric routing
              ::  XX  unnecessary? vere wil probably ignore this lane
              ?^  lane.sat.per
                lane.sat.per
              [~ `@ux`(^^sein:title rof /ames our now ship.per)]
            ::
            ++  fo-peek-naxplanation
              |=  seq=@ud
              ::  XX %ames call itself with a %meek task
              ::  on a wire used to infer the listener (the %poke %nax request; us)
              ::  when getting the %response $page with or %naxplanation payloads
              ::  (tagged with %ext)
              =/  =wire  (fo-wire %ext)
              =/  =space
                chum/[life.sat.per our life.ames-state symmetric-key.sat.per]
              =/  =path   (ev-make-path space (fo-nax-path seq our))
              (fo-emit hen %pass wire %a meek/[space her^path])
            ::
            --
          ::
          +|  %aliens-and-comets
          ::  +ev-enqueue-alien-todo: helper to enqueue a pending request
          ::
          ::    Also requests key and life from Jael on first request.
          ::    If talking to a comet, requests attestation packet.
          ::
          ++  ev-enqueue-alien-todo
            |=  $:  =ship
                    chum-state=(unit chum-state)
                    mutate=$-(ovni-state ovni-state)
                ==
            ^+  ev-core
            ::  create a default $ovni-state on first contact
            ::
            =/  [already-pending=? todos=ovni-state]
              ?~  chum-state
                [%.n *ovni-state]
              [%.y ?>(?=(%alien -.u.chum-state) +.u.chum-state)]
            ::  mutate .todos and apply to permanent state
            ::
            =.  todos  (mutate todos)
            =.  chums.ames-state  (~(put by chums.ames-state) ship %alien todos)
            ?:  already-pending
              ev-core
            ::
            ?:  =(%pawn (clan:title ship))
               (ev-read-proof ship)
            ::  NB: we specifically look for this wire in +public-keys-give in
            ::  Jael.  if you change it here, you must change it there.
            ::
            (ev-emit hen %pass /public-keys %j %public-keys [n=ship ~ ~])
          ::
          ++  ev-register-comet
            |=  [comet=@p open-packet signature=@ signed=@]  :: XX to %lull
            ^+  ev-core
            =/  crub  (com:nu:crub:crypto public-key)
            ::  verify signature
            ::
            ?>  (safe:as:crub signature signed)
            ::  assert the contents of the proof match those of a comet
            ::
            ?>  &(=(sndr comet) =(sndr-life 1))
            ::  assert the contents of the proof match ours
            ::
            ?>  &(=(rcvr our) =(rcvr-life life.ames-state))
            ::  only a star can sponsor a comet
            ::
            ?>  =(%king (clan:title (^sein:title comet)))
            ::  comet public-key must hash to its @p address
            ::
            ?>  =(comet fig:ex:crub)
            =/  keys
              (~(put by *(map life [suite=@ud pass])) life=1 suite=1 public-key)
            ::  insert comet
            ::
            =|  =point:jael
            =>  %^  ~(sy-publ sy hen)  /comet  %full
                %+  ~(put by *(map ship point:jael))  comet
                point(rift 0, life 1, keys keys, sponsor `(^sein:title comet))
            sy-abet
          ::
          ++  ev-read-proof
            |=  comet=ship
            =/  =wire   /mesa/comet/pof
            =/  =space  [%publ 1]
            =/  =path
              %+  ev-make-path  space
              /comet/proof/[(scot %p our)]/[(scot %p life.ames-state)]
            (ev-emit hen %pass wire %a meek/[space comet path])
          ::
          +|  %system
          ::
          ++  sy  ::  system/internal: %born, %heed, %kroc, %prod...
            |_  hen=duct
            ::
            +|  %helpers
            ::
            ++  sy-core  .
            ++  sy-abet  ev-core
            ::
            +|  %entry-points
            ::
            ++  sy-born
              =/  turfs
                ;;  (list turf)
                =<  q.q  %-  need  %-  need
                (rof [~ ~] /ames %j `beam`[[our %turf %da now] /])
              ::
              =*  duct  unix-duct.ames-state
              =?  ev-core  ?=(~ +.flow.dead.ames-state)
                (ev-emit ~[/ames] %pass /mesa/dead-flow %b %wait `@da`(add now ~m2))
              =?  flow.dead.ames-state  ?=(~ +.flow.dead.ames-state)
                flow/`[~[/ames] /mesa/dead-flow `@da`(add now ~m2)]
              =.  ev-core
                %-  ev-emil
                ^-  (list move)
                :~  [hen %give %turf turfs]
                    [hen %give %saxo sy-get-sponsors]
                    (poke-ping-app hen our %kick fail=%.n)
                ==
              sy-core(ames-state ames-state(unix-duct hen))
            ::  +sy-init: first boot; subscribe to our info from jael
            ::
            ++  sy-init
              ^+  sy-core
              =.  ev-core
                %-  ev-emil
                :~  [hen %pass /turf %j %turf ~]
                    [hen %pass /private-keys %j %private-keys ~]
                    [hen %pass /public-keys %j %public-keys [n=our ~ ~]]
                ==
              sy-core
            ::
            ++  sy-crud
              |=  =error
              ^+  sy-core
              =.  ev-core  (ev-emit hen %pass /crud %d %flog %crud error)
              sy-core
            ::  +sy-plug: handle key reservation
            ::
            ++  sy-plug
              |=  =path
              ^+  sy-core
              =/  key=@  (kdf:crypt 32 "mesa-chum-key" 32^eny)
              =/  kid=@ud
                ?~  latest=(ram:key-chain server-chain.ames-state)
                  1
                .+(key.u.latest)
              =.  server-chain.ames-state
                (put:key-chain server-chain.ames-state kid [key path])
              ~&  >  plug/[kid key path]
              ::  kid^key kill be used by remote %keen task when sending $peek
              ::
              sy-core(ev-core (ev-emit hen %give %stub kid key))
            ::
            ++  sy-publ
              |=  [=wire =public-keys-result:jael]
              |^  ^+  sy-core
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
                ^+  sy-core
                ?:  =(our ship)
                  sy-core
                ::
                =/  peer  (sy-find-peer ship)
                ::  we shouldn't be hearing about ships we don't care about
                ::
                ?~  +.peer
                  ~>  %slog.0^leaf/"ames: breach unknown {<our ship>}"
                  sy-core
                ::  if an alien breached, this doesn't affect us
                ::
                ?:  ?=([?(%ship %chum) ~ %alien *] peer)
                  ~>  %slog.0^leaf/"ames: breach alien {<our ship>}"
                  sy-core
                ~>  %slog.0^leaf/"ames: breach peer {<our ship>}"
                ::  print change to quality of service, if any
                ::
                =.  ev-core
                  =/  old-qos=qos  qos.+.u.peer
                  =/  text=(unit tape)
                    %^  qos-update-text  ship  %ames
                    [old-qos *qos [kay.veb ships]:bug.ames-state]
                  ?~  text  ev-core
                  (ev-emit hen %pass /qos %d %flog %text u.text)
                ::  a peer breached; drop all peer state other than pki data
                ::
                =?  chums.ames-state  ?=(%chum -.peer)
                  =.  +>.u.peer  +:*fren-state
                  ::  XX  reinitialize galaxy route if applicable
                  ::
                  =?  lane.+.u.peer  =(%czar (clan:title ship))
                    (some `@ux`ship)
                  (~(put by chums.ames-state) ship u.peer)
                =?  peers.ames-state  ?=(%ship -.peer)
                  ::  XX  reinitialize galaxy route if applicable
                  ::
                  =?  route.+.u.peer  =(%czar (clan:title ship))
                    `[direct=%.y %& ship]
                  =.  +>.u.peer  +:*peer-state
                  (~(put by peers.ames-state) ship u.peer)
                ::
                =.  ev-core
                  %-  ev-emit
                  :*  unix-duct.ames-state  %give  %nail  ship
                      ?.  ?=(%chum -.peer)
                        (get-forward-lanes our +.u.peer peers.ames-state)
                      ^-  (list lane)
                      ::  XX refactor
                      %+  turn
                        (get-forward-lanes-mesa our +.u.peer chums.ames-state)
                      |=  =lane:pact
                      ^-  (each @pC address)
                      ?>  ?=(@ lane)
                      [%.y `@pC`lane]
                  ==
                ::  if one of our sponsors breached, give the updated list to vere
                ::
                =/  sponsors  (~(gas in *(set ^ship)) sy-get-sponsors)
                =?  ev-core  (~(has in sponsors) ship)
                  (ev-emit unix-duct.ames-state %give %saxo ~(tap in sponsors))
                :: ::
                sy-core
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
                ^+  sy-core
                ?:  =(our ship)  sy-core
                ::
                =/  peer  (sy-find-peer ship)
                ?.  ?=([?(%ship %chum) ~ %known *] peer)
                  =|  =point:jael
                  =.  life.point     life
                  =.  keys.point     (my [life crypto-suite public-key]~)
                  =.  sponsor.point  `(^^sein:title rof /ames our now ship)
                  ::
                  (on-publ-full (my [ship point]~))
                ::
                =/  crypto-core   (nol:nu:crub:crypto priv.ames-state)
                =/  =private-key  sec:ex:crypto-core
                ::
                =.  symmetric-key.+.u.peer
                  (derive-symmetric-key public-key private-key)
                =.  life.+.u.peer        life
                =.  public-key.+.u.peer  public-key
                ::
                =?  chums.ames-state  ?=(%chum -.peer)
                  (~(put by chums.ames-state) ship u.peer)
                =?  peers.ames-state  ?=(%ship -.peer)
                  (~(put by peers.ames-state) ship u.peer)
                sy-core
              ::  +on-publ-sponsor: handle new or lost sponsor for peer
              ::
              ::    TODO: really handle sponsor loss
              ::
              ++  on-publ-sponsor
                |=  [=ship sponsor=(unit ship)]
                ^+  sy-core
                ::
                ?:  =(our ship)
                  =.  ev-core
                    (ev-emit unix-duct.ames-state %give %saxo sy-get-sponsors)
                  sy-core
                ?~  sponsor
                  %-  (slog leaf+"ames: {(scow %p ship)} lost sponsor, ignoring" ~)
                  sy-core
                ::
                =/  peer  (sy-find-peer ship)
                ?.  ?=([?(%ship %chum) ~ %known *] peer)
                  %-  (slog leaf+"ames: missing peer-state, ignoring" ~)
                  sy-core
                =.  sponsor.+.u.peer   u.sponsor
                =?  chums.ames-state  ?=(%chum -.peer)
                  (~(put by chums.ames-state) ship u.peer)
                =?  peers.ames-state  ?=(%ship -.peer)
                  (~(put by peers.ames-state) ship u.peer)
                =.  ev-core
                  %-  ev-emit
                  :*  unix-duct.ames-state  %give  %nail  ship
                      ?.  ?=(%chum -.peer)
                        (get-forward-lanes our +.u.peer peers.ames-state)
                      ^-  (list lane)
                      ::  XX refactor
                      %+  turn
                        (get-forward-lanes-mesa our +.u.peer chums.ames-state)
                      |=  =lane:pact
                      ^-  (each @pC address)
                      ?>  ?=(@ lane)  :: XX FIXME
                      [%.y `@pC`lane]
                  ==
                ::
                sy-core
              ::  +on-publ-full: handle new pki data for peer(s)
              ::
              ++  on-publ-full
                |=  points=(map ship point:jael)
                ^+  sy-core
                ::
                =>  .(points ~(tap by points))
                |^  ^+  sy-core
                    ?~  points  sy-core
                    ::
                    =/  [=ship =point:jael]  i.points
                    ::
                    =?  rift.ames-state  =(our ship)
                      rift.point
                    ::
                    ::  XX not needed?
                    :: =?  sy-core  =(our ship)
                    ::   (ev-emit unix-duct.ames-state %give %saxo get-sponsors)
                    ?.  (~(has by keys.point) life.point)
                      $(points t.points)
                    ::
                    =/  old-peer-state  (sy-find-peer ship)
                    ::
                    =^  new-state  sy-core
                      (insert-ship-state -.old-peer-state ship point)
                    ::
                    =?  sy-core  ?=([?(%ship %chum) ~ %alien *] old-peer-state)
                      ?:  ?=(%ship -.old-peer-state)
                        (meet-alien-ship ship point +.u.old-peer-state)
                      ?>  ?=(%chum -.new-state)
                      (meet-alien-chum ship point +.u.old-peer-state +.new-state)
                    ::
                    $(points t.points)
                ::
                ++  meet-alien-ship
                  |=  [=ship =point todos=alien-agenda]
                  ^+  sy-core
                  ::  init event-core:ames
                  ::
                  =/  ames-core  (ev:ames hen ames-state)
                  ::  if we're a comet, send self-attestation packet first
                  ::
                  =?  ames-core  =(%pawn (clan:title our))
                    =/  =blob  (attestation-packet:ames-core ship life.point)
                    %-  send-blob:ames-core
                    [for=| ship blob (~(get by peers.ames-state) ship)]
                  ::  save current duct
                  ::
                  =/  original-duct  hen
                  ::  apply outgoing messages, reversing for FIFO order
                  ::
                  =.  ames-core
                    %+  reel  messages.todos
                    |=  [[=duct =plea] core=_ames-core]
                    ?:  ?=(%$ -.plea)
                      (on-cork:core(duct duct) ship)
                    (on-plea:core(duct duct) ship plea)
                  ::  apply outgoing packet blobs
                  ::
                  =.  ames-core
                    %+  roll  ~(tap in packets.todos)
                    |=  [b=blob c=_ames-core]
                    (send-blob:c for=| ship b (~(get by peers.ames-state) ship))
                  ::  apply remote scry requests
                  ::
                  =^  moves  ames-state
                    =+  peer-core=(abed:pe:ames-core ship)
                    =<  abet  ^+  ames-core
                    =<  abet  ^+  peer-core
                    %-  ~(rep by keens.todos)
                    |=  [[=path ducts=(set duct)] cor=_peer-core]
                    (~(rep in ducts) |=([=duct c=_cor] (on-keen:c path duct)))
                  ::
                  sy-core(ev-core (ev-emil moves))
                ::
                ++  meet-alien-chum
                  |=  [=ship =point:jael todos=ovni-state =chum-state]
                  ^+  sy-core
                  ?.  ?=([%known *] chum-state)
                    ::  +insert-peer should have made this peer %known
                    ::
                    sy-core
                  ::  init ev-core with provided chum-state
                  ::
                  =.  ev-core  %*(. ev-core per ship^+.chum-state)
                  ::
                  =.  ev-core
                    ::  apply outgoing messages
                    ::
                    %+  reel  pokes.todos  ::  reversing for FIFO order
                    |=  [[=duct mess=mesa-message] c=_ev-core]
                    ?+    -.mess  !!  :: XX log alien peer %boon?
                        %plea
                      (ev-req-plea:(ev-abed:c duct) +.mess)
                    ==
                  =.  ev-core
                    ::  apply remote scry requests
                    ::
                    %-  ~(rep by peeks.todos)
                    |=  [[=path ducts=(set duct)] core=_ev-core]
                    %-  ~(rep in ducts)
                    |=  [=duct c=_core]
                    (ev-req-peek:(ev-abed:c duct) ~ path)
                  ::
                  sy-core
                ::
                --
              ::  on-publ-rift: XX
              ::
              ++  on-publ-rift
                |=  [=ship =rift]
                ^+  sy-core
                =?  rift.ames-state  =(our ship)
                  rift
                =/  peer  (sy-find-peer ship)
                ?~  ?=([?(%ship %chum) ~] peer)
                  ::  print error here? %rift was probably called before %keys
                  ::
                  ~>  %slog.1^leaf/"ames: missing peer-state on-publ-rift"
                  sy-core
                ?.  ?=([?(%ship %chum) ~ %known *] peer)
                  ::  ignore aliens
                  ::
                  sy-core
                =.  rift.+.u.peer  rift
                =?  chums.ames-state  ?=(%chum -.peer)
                  (~(put by chums.ames-state) ship u.peer)
                =?  peers.ames-state  ?=(%ship -.peer)
                  (~(put by peers.ames-state) ship u.peer)
                sy-core
              ::
              ++  insert-ship-state
                |=  [wer=?(%ship %chum) =ship =point:jael]
                ^-  $:  $%  [%ship ship-state]
                            [%chum chum-state]
                        ==
                        _sy-core
                    ==
                ::
                =/  =public-key     pass:(~(got by keys.point) life.point)
                :: =.  priv.ames-state    :: XX remove; needed when changing types in %lull after ++load is called
                ::   ;;  @
                ::   =<  q.q  %-  need  %-  need
                ::   =-  ~&  priv/-  -
                ::   (rof [~ ~] /ames %j `beam`[[our %vein %da now] /1])  :: XX remove
                =/  crypto-core     (nol:nu:crub:crypto priv.ames-state)
                =/  =private-key    sec:ex:crypto-core
                =/  =symmetric-key  (derive-symmetric-key public-key private-key)
                ::
                =/  peer
                  ::  XX if the peer doesn't previously exist we insert it
                  ::  based on the chosen core in state; see find-peer
                  ?:  ?=(%chum wer)
                    chum/`fren-state`sat:(ev-gut-per ship)
                  ship/(gut-peer-state:(ev:ames hen ames-state) ship)
                =.  life.peer           life.point
                =.  rift.peer           rift.point
                =.  public-key.peer     public-key
                =.  symmetric-key.peer  symmetric-key
                =.  qos.peer            [%unborn now]
                =.  sponsor.peer
                  ?^  sponsor.point
                    u.sponsor.point
                  (^^sein:title rof /ames our now ship)
                ::
                =?  ev-core  ?=(%czar (clan:title ship))
                  %-  ev-emit
                  :*  unix-duct.ames-state  %give  %nail  ship
                      ?.  ?=(%chum -.peer)
                        (get-forward-lanes our +.peer peers.ames-state)
                      ^-  (list lane)
                      ::  XX  refactor
                      %+  turn
                        (get-forward-lanes-mesa our +.peer chums.ames-state)
                      |=  =lane:pact
                      ^-  (each @pC address)
                      ?>  ?=(@ lane)  ::  XX FIXME
                      [%.y `@pC`lane]
                  ==
                ::
                ::  automatically set galaxy route, since unix handles lookup
                ::
                ?:  ?=(%chum -.peer)
                  =?  lane.peer  ?=(%czar (clan:title ship))
                    (some `@ux`ship)
                  =.  chums.ames-state
                    (~(put by chums.ames-state) ship known/+.peer)
                  [%chum known/+.peer]^sy-core
                ::
                =?  route.peer  ?=(%czar (clan:title ship))
                  `[direct=%.y lane=[%& ship]]
                =.  peers.ames-state
                  (~(put by peers.ames-state) ship known/+.peer)
                ::
                [%ship known/+.peer]^sy-core
              ::
              --
            ::  +sy-priv:  set our private key to jael's response
            ::
            ++  sy-priv
              |=  [=life vein=(map life private-key)]
              ^+  sy-core
              ::
              =.  priv.ames-state  (~(got by vein) life)
              =.  life.ames-state  life
              =/  crypto-core      (nol:nu:crub:crypto priv.ames-state)
              ::  recalculate each peer's symmetric key
              ::
              =.  chums.ames-state
                %-  ~(run by chums.ames-state)
                |=  =chum-state
                ^+  chum-state
                ::
                ?.  ?=(%known -.chum-state)
                  chum-state
                =/  =fren-state  +.chum-state
                =.  symmetric-key.fren-state
                  (derive-symmetric-key public-key.fren-state sec:ex:crypto-core)
                ::
                known/fren-state
              ::
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
                  (derive-symmetric-key public-key.+.ship-state sec:ex:crypto-core)
                ::
                known/peer-state
              sy-core
            ::
            ++  sy-prod
              =;  core=_ev-core
                sy-core(ev-core core)
              %-  ~(rep by chums.ames-state)
              |=  [[=ship =chum-state] core=_ev-core]
              ^+  core
              =+  per-sat=(ev-get-per ship)
              ?.  ?=([~ ~ %known *] per-sat)
                ::  XX  this shouldn't be needed
                ::  XX  only if %alien
                ~&  retrieving-keys-again/ship
                %+  ev-emit  [//keys]~
                [%pass /public-keys %j %public-keys [n=ship ~ ~]]
              ::
              =.  core  (ev-foco:core ship +.u.u.per-sat)
              ::
              %-  ~(rep by pit.sat.per.core)
              |=  [[=path req=request-state] core=_core]
              ~&  re-sending/path
              ::  XX  restore this when fixing +ev-update-qos
              =*  peer  sat.per.core
              =*  ship  ship.per.core
              ~&  ship/ship
              ::  update and print connection status
              ::
              =/  expiry=@da  (add ~s30 last-contact.qos.peer)
              =/  new=qos     ?.((gte now expiry) qos.peer [%dead now])
              =.  core  (ev-update-qos:core new)
              ::  if =(~ pay.req); %naxplanation, %cork or external (i.e. not
              ::  coming from %ames) $peek request
              ::
              %+  ev-push-pact:core
                (ev-make-pact:core ack=[ship path] pay.req rift.peer)
              lane.peer
            ::  +sy-snub: handle request to change ship blacklist
            ::
            ++  sy-snub
              |=  [form=?(%allow %deny) ships=(list ship)]
              ^+  sy-core
              =.  snub.ames-state  [form (^sy ships)]
              sy-core
            ::
            ++  sy-stun
              |=  =stun
              ^+  sy-core
              :: %-  %^  ev-trace  sun.veb  ship.stun
              ::     =/  lane=tape
              ::       ?:  &
              ::         ::  turn off until correct parsing ip/port in ames.c
              ::         ::  (see https://github.com/urbit/vere/pull/623)
              ::         ""
              ::       ?:  ?=(%& -.lane.stun)
              ::         "from {<p.lane.stun>}"
              ::       =,  lane.stun
              ::       =/  ip=@if  (end [0 32] p)
              ::       =/  pt=@ud  (cut 0 [32 16] p)
              ::       "lane {(scow %if ip)}:{((d-co:co 1) pt)} ({(scow %ux p)})"
              ::     |.("inject %stun {<-.stun>} {lane}")
              =.  ev-core
                %-  ev-emit
                %^  poke-ping-app  unix-duct.ames-state  our
                ?.  ?=(%fail -.stun)  -.stun
                [%kick fail=%.y]
              sy-core
            :: +sy-dear: handle lane from unix
            ::
            ++  sy-dear
              |=  [=ship =lane:pact]
              ^+  sy-core
              ?:  =(%czar (clan:title ship))
                sy-core
              =/  peer  (sy-find-peer ship)
              ?.  ?=([?(%ship %chum) ~ %known *] peer)
                sy-core
              =?  chums.ames-state  ?=(%chum -.peer)
                =.  lane.+.u.peer  `lane
                (~(put by chums.ames-state) ship u.peer)
              =?  peers.ames-state  ?=(%ship -.peer)
                =/  =^lane
                  ?@  lane  [%& `@p`lane]
                  [%| (can 3 (met 3 p.lane)^p.lane 2^q.lane ~)]
                =.  route.+.u.peer  `[direct=%.y lane]
                (~(put by peers.ames-state) ship u.peer)
              =.  ev-core
                =/  =^lane  :: XX refactor
                  ?@  lane  [%.y `@pC`lane]
                  :-  %.n
                  %+  can  3
                  :~  4^p.lane
                      2^q.lane
                  ==
                (ev-emit unix-duct.ames-state %give %nail ship ~[lane])
              sy-core
            ::
            ::  +sy-tame: handle request to delete a route
            ::
            ++  sy-tame
              |=  =ship
              ^+  sy-core
              ?:  =(%czar (clan:title ship))
                %-  %+  slog
                  leaf+"ames: bad idea to %tame galaxy {(scow %p ship)}, ignoring"
                ~
                sy-core
              =/  peer  (sy-find-peer ship)
              ?.  ?=([?(%ship %chum) ~ %known *] peer)
                %-  (slog leaf+"ames: no peer-state for {(scow %p ship)}, ignoring" ~)
                sy-core
              =?  chums.ames-state  ?=(%chum -.peer)
                =.  lane.+.u.peer  ~
                (~(put by chums.ames-state) ship u.peer)
              =?  peers.ames-state  ?=(%ship -.peer)
                =.  route.+.u.peer  ~
                (~(put by peers.ames-state) ship u.peer)
              =.  ev-core
                (ev-emit unix-duct.ames-state %give %nail ship ~)
              sy-core
            ::
            ::  +sy-sift: handle request to filter debug output by ship
            ::
            ++  sy-sift
              |=  ships=(list ship)
              ^+  sy-core
              =.  ships.bug.ames-state  (^sy ships)
              sy-core
            ::  +sy-spew: handle request to set verbosity toggles on debug output
            ::
            ++  sy-spew
              |=  verbs=(list verb)
              ^+  sy-core
              ::  start from all %.n's, then flip requested toggles
              ::
              =.  veb.bug.ames-state
                %+  roll  verbs
                |=  [=verb acc=veb-all-off]
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
                  %sun  acc(sun %.y)
                ==
              sy-core
            ::
            ++  sy-back
              |=  ship=(unit ship)
              |^  ^+  sy-core
              ?^  ship
                =/  =chum-state  (~(got by chums.ames-state) u.ship)
                ?.  ?=([%known *] chum-state)
                  sy-core
                (regress-chum u.ship +.chum-state)
              %-  ~(rep by chums.ames-state)
              |=  [[=^ship state=chum-state] core=_sy-core]
              ?:  ?=(%alien -.state)  core
              (regress-chum ship +.state)
              ::
              ++  regress-chum
                |=  [=^ship fren=fren-state]
                ^+  sy-core
                =|  peer=peer-state
                =:      -.peer  azimuth-state=-.fren
                    route.peer  ~  ::  get-route
                      qos.peer  qos.fren
                   corked.peer  (divide-bones corked.fren)
                  ossuary.peer  ossuary.fren
                    chain.peer  client-chain.fren
                ==
                ::
                =^  peek-moves  ames-state  (regress-peeks ship fren peer)
                =^  flow-moves  ames-state  (regress-flows ship fren ames-state)
                ::
                sy-core(ev-core (ev-emil (weld peek-moves flow-moves)))
              ::
              ++  divide-bones
                |=  bones=(set side)
                ^-  (set bone)
                %-  ~(rep in bones)
                |=  [side corked=(set bone)]
                (~(put in corked) ?:(?=(%for dire) bone (mix 0b1 bone)))
              ::
              ++  regress-flows
                |=  [her=^ship fren=fren-state state=axle]
                ^-  (quip move axle)
                =+  event-core=(ev:ames hen state)
                =/  peer=peer-state  (got-peer-state:event-core her)
                =+  peer-core=(abed-peer:pe:event-core her peer)
                =;  core=_peer-core
                  abet:abet:core
                =+  ev-core=%*(. ev-core sat.per fren)
                %-  ~(rep by flows.fren)
                |=  [[side state=flow-state] core=_peer-core]
                =+  fo-core=~(. fo:ev-core hen^bone^dire state)  :: XX make sat.per $fren-state
                ?:  ?=(%for dire)
                  =;  [cor=_core loads=_loads.state]
                    cor
                  ::  message-pump
                  ::
                  %^  (dip:fo-mop:fo-core ,cor=_core)  loads.state
                    core
                  |=  [cor=_core seq=@ud req=mesa-message]
                  ^-  [(unit mesa-message) stop=? cor=_core]
                  `[| (on-memo:peer-core bone req)]
                ::  message-sink
                ::
                =|  sink=message-sink-state
                ::  drop any pending ack state and past naxplanations
                ::  XX  if some is still actively reading a naxplanation,
                ::  do we need send it?
                ::  XX  better to drop any peeks for %naxplanations, %corks?
                ::
                =.  last-acked.sink  last-acked.state
                =.  last-heard.sink  last-acked.state
                =.  rcv.peer-state.peer-core
                  (~(put by rcv.peer-state.peer-core) bone sink)
                core(peer-state peer-state.peer-core)
              ::
              ++  regress-peeks
                |=  [her=^ship fren=fren-state peer=peer-state]
                ^-  (quip move axle)
                =+  event-core=(ev:ames hen ames-state)
                =;  core=_event-core
                  abet:core
                %-  ~(rep by pit.fren)
                |=  [[=path req=request-state] core=_event-core]
                ::  if =(~ pay.req) this could be a +peek for a %naxplanation,
                ::  %cork, or an external +peek (i.e. not part of flow)
                ::
                ?:  ?=(^ pay.req)  core  :: flows are migrated separatedly
                :: ?~  for.req        core  :: XX weird; log?  TMI
                =|  keen=keen-state
                =.  listeners.keen
                  %-  ~(rep in for.req)
                  |=  [hen=duct for=_for.req]
                  ::  XX  inspect the duct to find %mesa wires?
                  ::  XX  dropping any +peeks for %corks and %naxplanations
                  ::      can this makes us end up in a bad state?
                  ::
                  =?  for  ?=([[%mesa %flow *] *] hen)
                    (~(del in for) hen)
                  for
                ?~  listeners.keen  core
                ::  after filtering, all these should be external listeners
                ::
                =.  path
                  =/  [=space pax=^path]
                    [space inner]:(ev-decrypt-path path her)
                  ?-    -.space
                      %publ  pax
                  ::
                      %chum
                    =/  cyf
                      (scot %uv (en:crub:crypto key.space (spat pax)))
                    /a/x/1//chum/(scot %p our)/(scot %ud life.ames-state)/[cyf]
                  ::
                      %shut
                    =/  enc
                      (scot %uv (en:crub:crypto key.space (spat pax)))
                    /a/x/1//fine/shut/(scot %ud kid.space)/[enc]
                  ==
                ::
                (on-keen:core ~ her path)
              ::
              --
            ::
            +|  %internals
            ::
            ++  sy-get-sponsors
              ;;  (list ship)
              =<  q.q  %-  need  %-  need
              (rof [~ ~] /ames %j `beam`[[our %saxo %da now] /(scot %p our)])
            ::
            ++  sy-find-peer
              |=  =ship
              ^-  $%  [%ship (unit ship-state)]
                      [%chum (unit chum-state)]
                  ==
              ?^  chum-state=(~(get by chums.ames-state) ship)
                chum/chum-state
              ?^  ship-state=(~(get by peers.ames-state) ship)
                ship/ship-state
              ?:(?=(%mesa core.ames-state) [%chum ~] [%ship ~])
            ::
            --
          ::
          +|  %namespaces
          ::
          ++  ev-peek-publ
            |=  [bem=beam lyf=@ud =path]
            ^-  (unit (unit cage))
            ?~  lyf
              [~ ~]
            ?.  =(lyf life.ames-state)
              ~
            ?~  inn=(inner-path-to-beam our path)
              [~ ~]
            =/  view  ?@(vew.u.inn vew.u.inn (cat 3 [way car]:vew.u.inn))
            ?~  res=(rof ~ /ames/publ view bem.u.inn)
              ~
            ?~  u.res
              [~ ~]
            =/  priv=@uxI  (end 8 (rsh 3 priv.ames-state))  :: extract ed25519 key
            ::  XX  rift.ames-state
            =>  [bem=bem res=res priv=priv ..crypt]
            :: ~>  %memo./ames/publ
            =/  gag  [p q.q]:u.u.res  :: XX how does receiver distinguish these?
            =/  ful  (en-beam bem)
            =/  ser  (jam gag)  :: unencrypted
            :^  ~  ~  %message
            !>([%sign (sign:crypt priv ful (root:lss (met 3 ser)^ser)) ser])
          ::
          ++  ev-peek-chum
            |=  [bem=beam her=@p lyf=@ud hyf=@ud cyf=@uv]
            ^-  (unit (unit cage))
            ?.  =(lyf life.ames-state)
              ~
            ?~  key=(get-key-for her hyf chums.ames-state)
              ~
            =/  pat=path  (open-path:crypt u.key cyf)
            ?~  inn=(inner-path-to-beam our pat)
              ~
            ?~  res=(rof `[her ~ ~] /ames/chum vew.u.inn bem.u.inn)
              ~
            ?~  u.res
              [~ ~]
            =>  [key=u.key cyf=cyf bem=bem res=res ..crypt] :: XX rift.ames-state
            :: ~>  %memo./ames/chum
            :: XX rift.ames-state
            =/  gag  [p q.q]:u.u.res
            =/  ful  (en-beam bem)
            =/  ser  (jam gag)
            =/  cyr  (encrypt:crypt key cyf ser)
            ``[%message !>([%hmac (mac:crypt key ful (root:lss (met 3 cyr)^cyr)) cyr])]
          ::
          ++  ev-peek-shut
            |=  [bem=beam kid=@ cyf=@uv]
            ^-  (unit (unit cage))
            ?~  key=(get:key-chain server-chain.ames-state kid)
              ~
            =/  pat  (open-path:crypt -.u.key cyf)
            ::  XX check path prefix
            ?~  inn=(inner-path-to-beam our pat)
              ~
            ?~  res=(rof [~ ~] /ames/shut vew.u.inn bem.u.inn)
              ~
            ?~  u.res
              [~ ~]
            ::  XX  rift.ames-state
            =>  [key=key cyf=cyf bem=bem res=res ..crypt]
            :: ~>  %memo./ames/shut
            =/  cry=@uxI  (rsh 8 (rsh 3 -.u.key))
            =/  sgn=@uxI  (end 8 (rsh 3 -.u.key))
            =/  gag  [p q.q]:u.u.res
            =/  ful  (en-beam bem)
            =/  ser  (jam gag)
            =/  cyr  (encrypt:crypt cry iv=cyf ser)
            =/  sig  (sign:crypt sgn ful (root:lss (met 3 cyr)^cyr))
            ``[%message !>([%sign sig cyr])]
          ::
          ++  ev-peek-flow
            |=  [bone=@ud load=?(%plea %boon %ack-plea %ack-boon %nax) rcvr=ship mess=@ud]
            ^-  (unit (unit cage))
            =+  per-sat=(ev-get-per rcvr)
            ?.  ?=([~ ~ %known *] per-sat)
              ~  ::  %alien or missing
            =.  per  [rcvr +.u.u.per-sat]
            ::  ?>  ?=(%known -.sat.per)
            =/  dire=?(%for %bak)  (fo-infer-dire:fo load)
            ?:  ?&  (~(has in corked.sat.per) bone dire)
                    |(?=(%ack-plea load) ?=(%ack-boon load))
                ==
                ~&  >>>  corked-flow-dropping/load^corked.sat.per  :: XX remove
                ::  XX if %ack for a %corked flow (for both client and server),
                ::  produce %ack
                ::  if the flow is corked, block
                ::  XX when are corked bones evicted?
                ::
                ~  ::  XX  [~ ~]
            ::
            =/  res=(unit page)
              %.  [load mess]
              fo-peek:(fo-abed:fo ~[//scry] bone dire)
            ?~(res ~ ``[%message !>(u.res)])
          ::
          ::  ++  ev-peek-cork :: XX TODO?
          ::
          ++  ev-peek
            ^-  roon
            |=  [lyc=gang pov=path car=term bem=beam]
            ^-  (unit (unit cage))
            ?:  ?&  =(our p.bem)
                    =(%$ q.bem)
                    =([%ud 1] r.bem)
                    =(%x car)
                ==
              :: =+  core=(ev-abed:ev-core [now eny rof] ~[//scry])
              =*  core  ev-core
              =/  tyl=(pole knot)  s.bem
              ?+    tyl  ~
              ::  publisher-side, batch-level
              ::
                  [%hunk lop=@t len=@t pat=*]
                ::
                ?>  ?=([%mess ryf=@ %pact boq=@ %etch typ=?(%data %init) res=*] pat.tyl)
                =/  [lop=@ len=@]
                  [(slav %ud lop.tyl) (slav %ud len.tyl)]
                =*  scry  $
                ::
                =|  batch=(list @)
                ::
                :^  ~  ~  %batch
                !>  %-  flop
                |-  ^-  (list @)
                ?:  =(lop len)
                  batch
                =*  fag  lop
                =/  =path
                  ?:  =(%init typ.pat.tyl)
                    pat.tyl
                  ?>  ?=([fag=@ pat=*] res.pat.tyl)
                  pat.tyl(fag.res (scot %ud fag))
                =?  fag  ?=(%data typ.pat.tyl)
                  +(fag)
                =/  res=(unit (unit cage))
                  scry(lyc ~, pov /ames/batch, s.bem path)
                ?~  res
                  batch
                ?:  ?=(~ u.res)
                  batch
                ?.  ?=([%atom *] u.u.res)
                  batch
                =?  res.pat.tyl  ?=(%init typ.pat.tyl)
                  `^path`[fag='0' res.pat.tyl]
                =?  typ.pat.tyl  ?=(%init typ.pat.tyl)
                  %data
                $(batch [;;(@ q.q.u.u.res) batch])
              ::
              ::  publisher-side, protocol-level
              ::
                  [%mess ryf=@ res=*]
                =/  ryf  (slaw %ud ryf.tyl)
                ?~  ryf  [~ ~]
                ?.  =(rift.ames-state u.ryf)      ::  XX unauthenticated
                  ~
                =*  rif  u.ryf
                =/  nex
                  ^-  $@  ~
                      $:  pat=path
                          $=  pac       ::  XX control packet serialization
                          $@  ~
                          $:  boq=bloq
                              ser=?
                              wan=$@(~ [typ=?(%auth %data) fag=@ud])
                      ==  ==
                  ?+    res.tyl  ~
                      [%$ pat=*]  [pat.res.tyl ~]
                  ::
                      [%pact boq=@ ser=?(%etch %pure) %init pat=*]
                    ?~  boq=(slaw %ud boq.res.tyl)
                      ~
                    [pat.res.tyl u.boq ?=(%etch ser.res.tyl) ~]
                  ::
                      [%pact boq=@ ser=?(%etch %pure) typ=?(%auth %data) fag=@ pat=*]
                    =/  boq  (slaw %ud boq.res.tyl)
                    =/  fag  (slaw %ud fag.res.tyl)
                    ?:  |(?=(~ boq) ?=(~ fag))
                      ~
                    [pat.res.tyl u.boq ?=(%etch ser.res.tyl) typ.res.tyl u.fag]
                  ==
                ::
                ?~  nex
                  [~ ~]
                =*  pat  pat.nex
                =/  res
                  $(lyc ~, pov /ames/message, s.bem pat)
                ?.  ?&  ?=([~ ~ %message *] res)
                  :: ...validate that it's really a message
                  :: =>  [%message tag=?(sig hmac) ser=@]
                    ==
                  ~
                ?~  pac.nex  res
                ::
                ::  packets
                ::
                =*  boq  boq.pac.nex
                :: ?.  ?=(%13 boq)
                ::   ~ :: non-standard fragments for later
                =/  msg  ;;([typ=?(%sign %hmac) aut=@ ser=@] q.q.u.u.res)  :: XX types
                =/  mes=auth:mess  ?:(?=(%sign typ.msg) &+aut.msg |+aut.msg)
                =*  ser  ser.msg
                =/  wid  (met boq ser)
                =/  tob  (met 3 ser)
                ?<  ?=(%0 wid)  :: XX is this true?
                =/  nit=?  |    :: XX refactor
                |-  ^-  (unit (unit cage))
                ?~  wan.pac.nex
                  $(nit &, wan.pac.nex [?:((gth wid 1) %auth %data) 0])
                ::
                =*  fag  fag.wan.pac.nex
                ?.  (gth wid fag)
                  [~ ~]
                ?:  ?&  ?=(%auth typ.wan.pac.nex)
                        !=(0 fag)
                    ==
                  ~  :: non-standard proofs for later
                =;  [nam=name:pact dat=data:pact pairs=(list (unit [l=@ux r=@ux])) proof=(list @ux)]
                  =/  pac=pact:pact  [hop=0 %page nam dat ~]
                  ?:  (gth fag (div (add tob.dat 1.023) 1.024))
                    [~ ~]
                  ?.  ser.pac.nex
                    ``[%packet !>([pac pairs])]
                  =/  pof=@ux  (rep 8 proof)
                  =;  airs=(list @ux)
                    ``[%atom !>([p:(fax:plot (en:pact pac)) airs pof])]
                  %+  turn  pairs
                  |=  p=(unit [l=@ux r=@ux])
                  ?~  p  0x0
                  (rep 8 ~[l.u.p r.u.p])
                ::
                ?-    typ.wan.pac.nex
                    %auth
                  =/  nam  [[our rif] [boq ?:(nit ~ [%auth fag])] pat]
                  ::  NB: root excluded as it can be recalculated by the client
                  ::
                  =/  lss-proof
                    =>  [ser=ser ..lss]
                    :: ~>  %memo./ames/lss-auth
                    (build:lss (met 3 ser)^ser)
                  =/  dat  [tob [%& mes] (rep 8 proof.lss-proof)]  :: XX types
                  [nam dat ~ ~]
                ::
                    %data
                  =/  lss-proof
                    =>  [ser=ser ..lss]
                    :: ~>  %memo./ames/lss-data
                    (build:lss (met 3 ser)^ser)
                  =/  nam  [[our rif] [boq ?:(nit ~ [%data fag])] pat]
                  =/  aut
                    ?:  =(wid 1)
                      [%& mes]  :: single-fragment special case
                    [%| (snag fag pairs.lss-proof)]
                    ::
                  =/  dat  [tob aut (cut boq [fag 1] ser)]
                  =/  pairs
                    =/  per  (bex (sub boq 13))  ::  XX  unguarded
                    (swag [(mul per fag) (dec per)] pairs.lss-proof)
                  [nam dat pairs proof.lss-proof]
                ==
              ::
              ::  XX need a single namespace entrypoint to validate
              ::     generically any authentication tag for a message
              ::
              ::    /ax/[$ship]//1/validate-message/[auth-string]/[blake3-hash]/[path]
              ::
              ::  publisher-side, message-level (public namespace)
              ::
                  [%publ lyf=@ pat=*]
                =/  lyf  (slaw %ud lyf.tyl)
                ?~  lyf  [~ ~]
                (ev-peek-publ:core bem u.lyf pat.tyl)
              ::  publisher-side, message-level (two-party encrypted namespace)
              ::
                  [%chum lyf=@ her=@ hyf=@ cyf=@ ~]
                =/  lyf  (slaw %ud lyf.tyl)
                =/  her  (slaw %p her.tyl)
                =/  hyf  (slaw %ud hyf.tyl)
                =/  cyf  (slaw %uv cyf.tyl)
                ?:  |(?=(~ lyf) ?=(~ her) ?=(~ hyf) ?=(~ cyf))
                  [~ ~]
                (ev-peek-chum:core bem u.her u.lyf u.hyf u.cyf)
              ::  publisher-side, message-level (group encrypted namespace)
              ::
                  [%shut kid=@ cyf=@ ~]
                =/  kid  (slaw %ud kid.tyl)
                =/  cyf  (slaw %uv cyf.tyl)
                ?:  |(?=(~ kid) ?=(~ cyf))
                  [~ ~]
                (ev-peek-shut:core bem u.kid u.cyf)
              ::  publisher-side, flow-level
              ::
                  [%flow bone=@ load=?(%plea %boon %ack-plea %ack-boon %nax) rcvr=@ mess=@ ~]
                =/  bone  (slaw %ud bone.tyl)
                =/  rcvr  (slaw %p rcvr.tyl)
                =/  mess  (slaw %ud mess.tyl)
                ?:  |(?=(~ bone) ?=(~ rcvr) ?=(~ mess))
                  [~ ~]
                (ev-peek-flow:core u.bone load.tyl u.rcvr u.mess)
              ::  client %mesa %corks, flow-level
              ::
                  [%flow bone=@ %cork rcvr=@ ~]
                =>  .(tyl `(pole iota)`(ev-pave tyl))
                ?>  ?=(cork-pith tyl)
                =+  per-sat=(ev-get-per rcvr.tyl)
                ?.  ?=([~ ~ %known *] per-sat)
                  ~  ::  %alien or missing
                =.  per  [rcvr.tyl +.u.u.per-sat]
                ::  ?>  ?=(%known -.sat.per)
                =/  res=(unit page)
                  %.  [%cork *@ud]
                  fo-peek:(fo-abed:fo ~[//scry] bone.tyl dire=%for)  :: XX allow to read "server" corks
                ?~(res ~ ``[%message !>(u.res)])
              ::  comet attestations
              ::
                  [%comet %proof rcvr=@ life=@ ~]
                ::  only comets have this
                ::
                ?.  ?=(%pawn (clan:title our))
                  [~ ~]
                =/  rcvr  (slaw %p rcvr.tyl)
                =/  life  (slaw %ud life.tyl)
                ?:  |(?=(~ life) ?=(~ rcvr))
                  [~ ~]
                ::
                =+  core=(ev:ames ~[//attestation] ames-state)
                =/  comet-proof=open-packet
                  :*  pub:ex:crypto-core.ames-state
                      our
                      life.ames-state
                      u.rcvr
                      u.life
                  ==
                =*  priv  priv.ames-state
                :+  ~  ~
                [%message !>((sign:as:(nol:nu:crub:crypto priv) (jam comet-proof)))]
              ::  weight of a noun bounded at .pat, as measured by .boq
              ::
                  [%whit boq=@ pat=*]
                =/  boq  (slaw %ud boq.tyl)
                ?~  boq  [~ ~]
                ?~  inn=(inner-path-to-beam our pat.tyl)
                  ~
                ?~  res=(rof ~ /ames/whit vew.u.inn bem.u.inn)  :: XX only public data supported
                  ~
                :^  ~  ~  %whit
                !>([boq=u.boq (met u.boq (jam ?~(u.res ~ [p q.q]:u.u.res)))])
              ::  verify packet auth
              ::
                  [%veri typ=?(%sign %hmac) her=@ aut=@ rut=@ pat=*]
                =/  her  (slaw %p her.tyl)
                =/  aut  (slaw %uv aut.tyl)
                =/  rut  (slaw %uv rut.tyl)
                ?:  |(?=(~ her) ?=(~ aut) ?=(~ rut))
                  [~ ~]
                =/  ful  (en-beam [[u.her %$ ud+1] pat.tyl])
                :^  ~  ~  %flag  !>  :: XX is this right?
                ?-  typ.tyl
                  %sign  (verify-sig:crypt (ev-sig-key pat.tyl u.her) u.aut ful u.rut)
                  %hmac  (verify-mac:crypt (ev-mac-key pat.tyl u.her) u.aut ful u.rut)
                ==
            ::
              ==
            ::  only respond for the local identity, %$ desk, current timestamp
            ::
            ?.  ?&  =(our p.bem)
                    =([%da now] r.bem)
                    =(%$ q.bem)
                ==
              ~
            ::
            ::  /ax/chums/[ship]                 chum-state
            ::  /ax/chums/[ship]/forward-lane    lanes
            ::
            ?.  ?=(%x car)  ~
            =/  tyl=(pole knot)  s.bem
            ::  private endpoints
            ::
            ?.  =([~ ~] lyc)  ~
            ?+    tyl  ~
                [%chums her=@ req=*]
              =/  who  (slaw %p her.tyl)
              ?~  who  [~ ~]
              =/  chum  (~(get by chums.ames-state) u.who)
              ?+    req.tyl  [~ ~]
                  ~
                ?~  chum
                  ~&  (~(get by peers.ames-state) u.who)
                  [~ ~]
                ?>  ?=(%known -.u.chum)
                ``noun+!>(u.chum)
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
                !>  ^-  (list lane:pact)
                ?:  =(our u.who)
                  ~
                ?:  ?=([~ %known *] chum)
                  (get-forward-lanes-mesa our +.u.chum chums.ames-state)
                =/  sax  (rof [~ ~] /ames %j `beam`[[our %saxo %da now] /(scot %p u.who)])
                ?.  ?=([~ ~ *] sax)
                  ~
                =/  gal  (rear ;;((list ship) q.q.u.u.sax))
                ?:  =(our gal)
                  ~
                [`@ux`gal]~
              ==
            ==
          ::
          +|  %internals
          ::
          ::  XX  refactor; merge with +ev-update-qos in |pe:ames
          ::  +ev-update-qos: update and maybe print connection status
          ::
          ++  ev-update-qos
            |=  new=qos
            ^+  ev-core
            =*  old  qos.sat.per
            =.  qos.sat.per  new
            =/  text
              %^  qos-update-text  ship.per  %ames
              [old new [kay.veb ships]:bug.ames-state]
            ::  if no update worth reporting, we're done
            ::
            ?~  text   ev-core
            ::  print message
            ::
            (ev-emit hen %pass /qos %d %flog %text u.text)
          ::
          ++  ev-update-lane
            |=  [=lane:pact hop=@ud next=(list lane:pact)]
            ^+  per
            ?:  =(0 hop)
              per(lane.sat `lane)
            ?~  next  per
            per(lane.sat `i.next)
          ::
          ++  ev-push-pact  :: XX forwarding?
            |=  [=pact:pact lane=(unit lane:pact:ames)]
            ^+  ev-core
            =/  =ship
              ?-  +<.pact  ::  XX
                %peek  her.p.pact
                %poke  her.p.pact
                %page  her.p.pact
              ==
            =/  lanes=(list lane:pact:ames)
              %+  weld
                (drop (ev-get-sponsor ship))
              ?~(lane ~ (drop lane))
            ~&  pushin-pact-on/lanes
            %+  ev-emit  unix-duct.ames-state
            [%give %push lanes p:(fax:plot (en:^pact pact))]
          ::
          ++  ev-get-sponsor  :: XX move to %helper core?
            |=  =ship
            ^-  (unit @ux)
            =/  sax
              (rof [~ ~] /sax %j `beam`[[our %saxo %da now] /(scot %p ship)])
            ?.  ?=([~ ~ *] sax)
              ~  :: XX log
            =/  gal  (rear ;;((list ^ship) q.q.u.u.sax))  :: XX only galaxy
            ?:  =(our gal)
              ~  :: XX log
            [~ `@ux`gal]
          ::
          --
      ::
      |%
      ++  call
        |=  [hen=duct dud=(unit goof) wrapped-task=(hobo task)]  :: XX common tasks
        ^-  [(list move) _vane-gate]
        =/  =task  ((harden task) wrapped-task)
        =+  ev-core=(ev-abed:ev-core hen)
        ::
        =^  moves  ames-state
          ::  handle error notifications
          ::
          ::   we can crash somewhere in the packet layer, while handling a packet
          ::   the packet could be a one fragment poke, an ack or
          ::   a multiple-fragment poke payload, that the packet layer has send a +peek for.
          ::   crashes in the packet layer shouldn't be handled, since that can mean a malformed
          ::   message that the sender has given us.
          ::
          ::  if we crash in the message layer, the message and everyting has been decrypted and validated
          ::  so we could be crashing while handing a %plea or a %boon, or an %ack.
          ::  a crash while handling a %plea needs to be handled to then expose a %naxplanation (blank error for security)
          ::  in our namespace.
          ::
          ::  %boon are always acked, but we should print the error.
          ::
          ::  if we crashed processig an ack, the other end is missbehaving—sending something that's not a [%message %ack error=?]
          ::  so we print it but do nothing else, since we need to continue peeking until they fix their problem...
          ::
          ::  what to do with peers that we know are missbehaving? can we have a namespace that records network incidents,
          ::  so if something failed and we know a peer has broken flows, we can confirm that they have fixed the problem?
          ::  what does this gives us? we still need to track in state this broken peers, and we don't know if new flows
          ::  will still have the problem.
          ::
          ?^  dud
            ?+  -.task  ev-abet:(~(sy-crud sy:ev-core hen) -.task tang.u.dud)
              %heer   %-  %-  slog
                          :_  tang.u.dud
                          leaf+"mesa: %heer crashed {<mote.u.dud>}"
                      `ames-state
              %mess  ev-abet:(ev-call:ev-core %mess mess.task dud)
            ==
          ::
          =<  ev-abet
          ?+  -.task
              ::  ?(%plea %keen %cork) calls are handled directly in |peer
              ::
              ev-core ::  XX TODO: ?(%trim %stir %cong)
          ::
            %vega  ev-core  ::  handle kernel reload
            %init  sy-abet:~(sy-init sy:ev-core hen)
            %born  sy-abet:~(sy-born sy:ev-core hen)
            %plug  sy-abet:(~(sy-plug sy:ev-core hen) path.task)
            %prod  sy-abet:~(sy-prod sy:ev-core hen)  ::  XX handle ships=(list @p)
            %snub  sy-abet:(~(sy-snub sy:ev-core hen) [form ships]:task)
            %stun  sy-abet:(~(sy-stun sy:ev-core hen) stun.task)
            %dear  sy-abet:(~(sy-dear sy:ev-core hen) +.task)
            %tame  sy-abet:(~(sy-tame sy:ev-core hen) ship.task)
            %sift  sy-abet:(~(sy-sift sy:ev-core hen) ships.task)
            %spew  sy-abet:(~(sy-spew sy:ev-core hen) veb.task)
          ::  migration
          ::
            %back  sy-abet:(~(sy-back sy:ev-core hen) +.task)
          ::  from internal %ames request
          ::
            %meek  (ev-make-peek:ev-core +.task)
            %moke  (ev-make-poke:ev-core +.task)
          ::  from unix
          ::
            %heer      (ev-call:ev-core task)  ::  XX dud
          ::  from packet layer or XX
          ::
            %mess      (ev-call:ev-core %mess mess.task ~)
          ==
          ::
        [moves vane-gate]
      ::
      ++  take
        |=  [=wire hen=duct dud=(unit goof) =sign]
        ^-  [(list move) _vane-gate]
        ?^  dud
          ~|(%mesa-take-dud (mean tang.u.dud))
        ::
        =+  ev-core=(ev-abed:ev-core hen)
        =^  moves  ames-state
          ?:  ?=([%gall %unto *] sign)  :: XX from poking %ping app
            `ames-state
          ::
          =<  ev-abet
          ~|  sign=sign
          ?+  sign  ev-core  ::  ~&(mesa-take-sign/[&1^&2]:sign ev-core)
            [%behn %wake *]  (ev-take-wake:ev-core [wire error.sign])
          ::
              [%jael %private-keys *]
            sy-abet:(~(sy-priv sy:ev-core hen) [life vein]:sign)
          ::
              [%jael %public-keys *]
            sy-abet:(~(sy-publ sy:ev-core hen) wire +>.sign)
          ::
            [%jael %turf *]  (ev-emit:ev-core unix-duct %give %turf +>.sign)
          ::  vane gifts
          ::
            [%gall %flub ~]  (ev-take-flub:ev-core wire)
            [@ %done *]      (ev-poke-done:ev-core wire error.sign)
            [@ %boon *]      (ev-take-boon:ev-core wire %boon payload.sign)
            [@ %noon *]      (ev-take-boon:ev-core wire %noon [id payload]:sign)
          ::
          ::  network responses: acks/naxplanation payloads
          ::                     reentrant from %ames (either message or packet layer)
          ::
            [%ames %mess-response *]
          ::
            =/  response-pith  `(pole iota)`(ev-pave wire)
            %.  [wire +>.sign]
            ?+    response-pith   ~|  %mesa-evil-response-wire^wire  !!
                ::  %acks come directly into the message layer since they are always one
                ::  packet, and then given back to the flow layer that called them
                ::
                ?([%keen ~] ev-flow-wire)
              ev-take-response:ev-core  ::  %ack and %naxplanation payload
            ==
          ::
          ==
        [moves vane-gate]
      ::
      ++  scry  ev-peek:(ev-abed:ev-core [//scry]~)  :: XX
      ::
      --
    ::
    --
::
=>  ::  per-peer core-routing migration check
    ::
    |_  hen=duct
    ::
    +|  %helpers
    ::
    ++  pe-core  .
    ++  me-core  (ev-abed:ev-core:mesa hen)
    ++  pe-abed  |=(=duct pe-core(hen duct))
    ::
    ++  pe-find-peer
      |=  =ship
      ^-  $%  [%ames (unit ship-state)]
              [%mesa (unit chum-state)]
          ==
      ?^  chum-state=(~(get by chums.ames-state) ship)
        mesa/chum-state
      ?:  ?=(%mesa core.ames-state)  :: XX revisit this
        mesa/~
      ames/(~(get by peers.ames-state) ship)
    ::
    +|  %entry-points
    ::
    ++  call
      |=  [dud=(unit goof) =task]
      ?+  -.task  !!
        %load  `vane-gate(ames-state ames-state(core +.task))
        %plea  (pe-plea +.task)
        %cork  (pe-cork +.task)
        %keen  (pe-keen +.task)
        %whit  (pe-whit +.task)
        %yawn  (pe-cancel all=| +.task)
        %wham  (pe-cancel all=& +.task)
      ::  |mesa tasks
      ::
        %heer  (pe-heer dud +.task)
        %mess  (pe-mess dud +.task)
      ::
      ==
    ::
    +|  %common-tasks
    ::
    ++  pe-plea
      |=  [=ship =plea]
      ^-  [(list move) _vane-gate]
      =/  ship-state  (pe-find-peer ship)
      ::
      ?:  ?=(%ames -.ship-state)
        (call:ames hen ~ soft+plea/ship^plea)
      =^  moves  ames-state
        =<  ev-abet
        ?:  ?=([~ %known *] +.ship-state)
          (%*(ev-req-plea me-core per ship^+.u.ship-state) plea)
        ::
        %^  ev-enqueue-alien-todo:me-core  ship  +.ship-state
        |=  todos=ovni-state:me-core
        todos(pokes [[hen^plea/plea] pokes.todos])
      moves^vane-gate
    ::
    ++  pe-cork
      |=  her=ship
      =/  =plea  [%$ /cork %cork ~]
      =/  ship-state  (pe-find-peer her)
      ::
      ?:  ?=(%ames -.ship-state)
        (call:ames hen ~ soft+cork/ship)
      =^  moves  ames-state
        =<  ev-abet
        ?:  ?=([~ %known *] +.ship-state)
          (%*(ev-req-plea me-core per her^+.u.ship-state) plea)
        ::
        %^  ev-enqueue-alien-todo:me-core  her  +.ship-state
        |=  todos=ovni-state:me-core
        todos(pokes [[hen^plea/plea] pokes.todos])
      moves^vane-gate
    ::
    ++  pe-keen
      |=  [sec=(unit [idx=@ key=@]) spar:^ames]
      =/  ship-state  (pe-find-peer ship)
      ?:  ?=(%ames -.ship-state)
        (call:ames hen ~ soft+keen/sec^ship^path)
      =^  moves  ames-state
        =<  ev-abut  ::  XX  due to the way we deal with comet attestations, we
                     ::  cant' call the normal +ev-abet arm since we are not
                     ::  touching per.sat.ev-core in ++ev-make-peek, so calling
                     ::  +ev-abet will discard any changes made
                     ::
        ?:  ?=([~ %known *] +.ship-state)
          (%*(ev-req-peek me-core per ship^+.u.ship-state) sec path)
        ::
        %^  ev-enqueue-alien-todo:me-core  ship  +.ship-state
        |=  todos=ovni-state:me-core
        todos(peeks (~(put ju peeks.todos) path hen))
      moves^vane-gate
      ::
    ::
    ++  pe-cancel
      |=  [all=? =spar]
      =/  ship-state  (pe-find-peer ship.spar)
      ::
      ?:  ?=(%ames -.ship-state)
        (call:ames hen ~ %soft ?:(all %wham %yawn) spar)
      =^  moves  ames-state
        =<  ev-abet
        ?.  ?=([~ %known *] +.ship-state)
          ::  XX delete from alien agenda?
          ~&("peer still alien, skip peek cancel" me-core)
        (%*(ev-cancel-peek me-core per ship.spar^+.u.ship-state) all path.spar)
      moves^vane-gate
    ::
    ++  pe-hear
      |=  [dud=(unit goof) =lane =blob]
      ^-  [(list move) _vane-gate]
      =/  =shot       (sift-shot blob)
      =/  ship-state  (pe-find-peer sndr.shot)
      ?:  ?=([%ames *] ship-state)
        ::  both for %ames and %fine
        ::
        (call:ames hen dud soft+hear/lane^blob)
      ?.  ?=([~ %known *] +.ship-state)
        ::  XX weird; log
        ~&  [%hear-unknown sndr.shot]
        `vane-gate
      ::  XX  TODO: check if we are in fact tracking this path
      ::  XX  (necessary?)
      ::
      ~&  [%hear-migrated sndr.shot]
      :: =/  [=peep =meow]  (sift-purr `@ux`content.shot)
      :: =/  =path  (slag 3 path.peep)
      ::  old response, no-op. If we can find the peer in chums, it means that
      ::  they sent an %ahoy plea, but they haven't heard our %ack, and have not
      ::  migrated us.
      ::
      ::  any %fine requests should have been migrated and responses should
      ::  only come via %heer or %mess. if %ames, we no-op and the %sender will
      ::  resend the message as soon as they migrate us.
      ::
      `vane-gate
    ::
    ++  pe-whit  :: XX add sec
      |=  [boq=@ud =spar:^ames]
      =/  ship-state  (pe-find-peer ship.spar)
      ?:  ?=(%ames -.ship-state)
        ~&(%whit-is-not-supported-in-ames `vane-gate)
      (pe-keen ~ spar(path [%a %x '1' %$ %whit (scot %ud boq) path.spar]))
    ::
    +|  %mesa-tasks
    ::
    ++  pe-heer
      |=  [dud=(unit goof) =lane:pact blob=@]
      ::
      ::  XX  handle .dud
      ::
      ::  XX find peer first; if regressing back to |ames, we could hear old
      ::  |mesa tasks
      ::
      =/  =pact:pact  (parse-packet blob)
      =^  moves  ames-state
        ?-    +<.pact
            %page  ::(ev-pact-page lane hop.pact +>.pact)
          ?~  chum=(~(get by chums.ames-state) her.p.pact)
            ::  XX weird page; log
            `ames-state
          ?>  ?=([~ %known *] chum)  ::  XX alien agenda? log?
          =<  ev-abet
          %.  [lane hop.pact +>.pact]
          ev-pact-page:(ev-foco:me-core her.p.pact +.u.chum)
        ::
            %peek
          ev-abet:(ev-pact-peek:me-core +>.pact)
        ::
            %poke
        ::
          =*  her  her.q.pact  :: her from poke-path
          =/  chum-state  (~(get by chums.ames-state) her)
          ?:  ?&  ?=(%pawn (clan:title her))
                  |(?=(~ chum-state) ?=([~ %alien *] chum-state))
              ==
            =?  chum-state  ?=(~ chum-state)
              ::  first time: upgrade to %comet and +peek attestation proof
              ::
              `alien/*ovni-state
            ?>  ?=([~ %alien *] chum-state)
            ?:  ?=(^ pit.u.chum-state)
              ::  still waiting to hear attestation proof
              ::
              `ames-state
            =.  chums.ames-state
              (~(put by chums.ames-state) her u.chum-state)
            ::  start peeking the attestation proof
            ::
            ev-abet:(ev-read-proof:me-core her)
          ::
          ?.  ?=([~ %known *] chum-state)
            ::  request public keys from %jael; drop the packet, it'll be re-send
            ::
            =<  ev-abet
            %-  ev-enqueue-alien-todo:me-core
            [her chum-state |=(ovni-state +<)]
          =<  ev-abet
          %.  [lane hop.pact +>.pact]
          ev-pact-poke:(ev-foco:me-core her +.u.chum-state)
        ::
        ==
      moves^vane-gate
    ::
    ++  pe-mess  :: XX refactor
      |=  [dud=(unit goof) =mess]
      =^  moves  ames-state
        =<  ev-abet
        ?-    -.mess
            %poke
          ?~  chum=(~(get by chums.ames-state) ship.p.mess)
            me-core
          ::  XX  this assumes that %aliens are checked in the packet layer
          ?>  ?=([~ %known *] chum)  ::  XX alien agenda?
          (ev-mess-poke:(ev-foco:me-core ship.p.mess +.u.chum) dud +.mess)
        ::
            %peek
          ?~  chum=(~(get by chums.ames-state) ship.p.mess)
            me-core
          ::  XX  this assumes that %aliens are checked in the packet layer
          ?>  ?=([~ %known *] chum)  ::  XX alien agenda?
          (ev-mess-peek:(ev-foco:me-core ship.p.mess +.u.chum) p.mess)
        ::
            %page
          ?~  chum=(~(get by chums.ames-state) ship.p.mess)
            me-core
          ::  XX  this assumes that %aliens are checked in the packet layer
          ?>  ?=([~ %known *] chum)  ::  XX alien agenda?
          (ev-mess-page:(ev-foco:me-core ship.p.mess +.u.chum) [p q r]:mess)
        ::
        ==
      moves^vane-gate
    ::
    --
::
|%
++  call
  |=  [hen=duct dud=(unit goof) wrapped-task=(hobo task)]
  ^-  [(list move) _vane-gate]
  =/  =task  ((harden task) wrapped-task)
  ?-    -.task
    ::  %ames-only tasks
    ::
      ?(%kroc %deep %chum %cong %mate %stir)
    ::  XX can we call the wrong core? still check if ship has migrated?
    ::
    (call:ames hen dud soft/task)
    ::  %hear; check if this is coming from in-progress migrating flows
    ::
      %hear
    (~(pe-hear pe-core hen) dud +.task)
    ::  %mesa-only tasks
    ::
      ?(%heer %mess)
    (~(call pe-core hen) dud task)
    ::  XX can we call the wrong core? still check if ship has migrated?
    ::
      ?(%meek %moke %back)
    (call:mesa hen dud soft/task)
    ::  flow-independent tasks
    ::
      ?(%vega %init %born %snub %spew %stun %sift %plug %dear %init %tame)
    (call:mesa hen dud soft/task)
    ::  common tasks
    ::
      ?(%plea %cork %keen %yawn %wham %load %whit)  :: XX make %whit only for |mesa
    (~(call pe-core hen) dud task)
    ::  core-dependent tasks
    ::
      ?(%prod %trim)
    =^  ames-moves  vane-gate  (call:ames hen ~ soft+task)
    =^  mesa-moves  vane-gate  (call:mesa hen ~ soft+task)
    [(weld ames-moves mesa-moves) vane-gate]
  ::
  ==
::
++  take
  |=  [=wire =duct dud=(unit goof) =sign]
  ^-  [(list move) _vane-gate]
  =*  sample  +<
  ?^  dud
    ~|(%ames-take-dud (mean tang.u.dud))
    ::
  ?:  ?=([?(%turf %mesa %private-keys %public-keys) *] wire)
    (take:mesa sample)
  =/  parsed-wire  (parse-bone-wire wire)
  ?:  ?=(~ parsed-wire)
    (take:ames sample)
  ::  XX log
  ::  migrate wire if the peer is in chums
  ::
  =/  ship-state  (pe-find-peer her.u.parsed-wire)
  ?:  ?=(%ames -.ship-state)
    (take:ames sample)
  ?:  ?=(%old -.u.parsed-wire)  `vane-gate  :: drop old wires
  %-  take:mesa
  :_  +.sample
  ^-  ^wire
  :~  %mesa  %flow  %van  %bak
     (scot %p her.u.parsed-wire)
     (scot %ud rift.u.parsed-wire)
     (scot %ud (mix 0b1 bone.u.parsed-wire))
  ==
::  +stay: extract state before reload
::
++  stay  ames-state
::  +load: load in old state after reload
::
++  load
  =<  |=  $=  old
          $%  $:  %4
                  $%  [%larva *]
                      [%adult state=ames-state-4]
              ==  ==
              $:  %5
                  $%  [%larva *]
                      [%adult state=ames-state-5]
              ==  ==
              $:  %6
                  $%  [%larva *]
                      [%adult state=ames-state-6]
              ==  ==
              $:  %7
                  $%  [%larva *]
                      [%adult state=ames-state-7]
              ==  ==
              $:  %8
                  $%  [%larva *]
                      [%adult state=ames-state-8]
              ==  ==
              $:  %9
                  $%  [%larva *]
                      [%adult state=ames-state-9]
              ==  ==
              $:  %10
                  $%  [%larva *]
                      [%adult state=ames-state-10]
              ==  ==
              $:  %11
                  $%  [%larva *]
                      [%adult state=ames-state-11]
              ==  ==
              $:  %12
                  $%  [%larva *]
                      [%adult state=ames-state-12]
              ==  ==
              $:  %13
                  $%  [%larva *]
                      [%adult state=ames-state-13]
              ==  ==
              $:  %14
                  $%  [%larva *]
                      [%adult state=ames-state-14]
              ==  ==
              $:  %15
                  $%  [%larva *]
                      [%adult state=ames-state-15]
              ==  ==
              $:  %16
                  $%  [%larva *]
                      [%adult state=ames-state-16]
              ==  ==
              $:  %17
                  $%  [%larva *]
                      [%adult state=ames-state-17]
              ==  ==
              $:  %18
                  $%  [%larva *]
                      [%adult state=ames-state-18]
              ==  ==
              $:  %19
                  $%  [%larva *]
                      [%adult state=ames-state-19]
              ==  ==
              $:  %20
                  $%  [%larva *]
                      [%adult state=ames-state-20]
              ==  ==
              $:  %21
                  $%  [%larva *]
                      [%adult state=ames-state-21]
              ==  ==
              axle
          ==
      =|  moz=(list move)
      |-  ^+  vane-gate
      ::
      ?:  ?=([@ %larva *] old)
        ::  XX we need to handle the %larva type to account for %larval ++stay
        ::  but all live ships should have metamorphosed and if booting
        ::  from the first time, %ames will produce %0 axle in ++stay
        ::
        !!  ::  $(+<.old %adult, +>.old state.old)
      ?:  ?=(%0 -.old)  :: XX make it %22
         ~&  priv.old
        :: =.  peers.old  ~
        :: =.   chums.old  ~
        ::   %-  ~(run by chums.old)
        ::   |=  =chum-state
        ::   ?:  ?=(%alien -.chum-state)
        ::     ~&  %cleaning-alien
        ::     chum-state
        ::     :: chum-state(pit ~)

        ::   %_  chum-state
        ::     flows    ~&  %cleaning-flows  ~
        ::     pit      ~&  %cleaning-pit  ~
        ::     corked   ~
        ::     ossuary  =|  =ossuary  ossuary
        ::             ::  %_  ossuary
        ::               :: next-bone  40
        ::   ==        :: ==
        vane-gate(ames-state old)
      ::
      ?>  ?=([@ %adult *] old)
      ?:  ?=(%5 -.old)   $(old 6+adult/(state-5-to-6 state.old))
      ?:  ?=(%6 -.old)   $(old 7+adult/(state-6-to-7 state.old))
      ?:  ?=(%7 -.old)
        ~>  %slog.0^leaf/"ames: init daily recork timer"
        %_  $
          old  8+adult/(state-7-to-8 state.old)
          moz  [[/ames]~ %pass /recork %b %wait `@da`(add now ~d1)]~
        ==
      ::
      ?:  ?=(%8 -.old)   $(old 9+adult/(state-8-to-9 state.old))
      ?:  ?=(%9 -.old)   $(old 10+adult/(state-9-to-10 state.old))
      ?:  ?=(%10 -.old)  $(old 11+adult/(state-10-to-11 state.old))
      ?:  ?=(%11 -.old)  $(old 12+adult/(state-11-to-12 state.old))
      ?:  ?=(%12 -.old)  $(old 13+adult/(state-12-to-13 state.old))
      ?:  ?=(%13 -.old)  $(old 14+adult/(state-13-to-14 state.old))
      ?:  ?=(%14 -.old)  $(old 15+adult/(state-14-to-15 state.old))
      ?:  ?=(%15 -.old)  $(old 16+adult/(state-15-to-16 state.old))
      ?:  ?=(%16 -.old)
        %_    $
            old  17+adult/(state-16-to-17 state.old)
            moz
          ?:  ?=(~ moz)
            moz  ::  if we have just added the timer in state-7-to-8, skip
          =;  recork-timer=(list [@da duct])
            ?^  recork-timer  moz
            ~>  %slog.0^leaf/"ames: init daily recork timer"
            :_  moz
            [[/ames]~ %pass /recork %b %wait `@da`(add now ~d1)]
          %+  skim
            ;;  (list [@da duct])
            =<  q.q  %-  need  %-  need
            (rof [~ ~] /ames %bx [[our %$ da+now] /debug/timers])   ::  XX can't scry on ++load
          |=([@da =duct] ?=([[%ames %recork *] *] duct))
        ==
      ::
      ?:  ?=(%17 -.old)
        ~>  %slog.0^leaf/"ames: fetching our public keys"
        %_    $
            moz
          ^-  (list move)
          [[[/ames]~ %pass /public-keys %j %public-keys [n=our ~ ~]] moz]
        ==
      ::
      ?:  ?=(%18 -.old)  $(old 19+adult/(state-18-to-19 state.old))
      ?:  ?=(%19 -.old)
        ~>  %slog.0^leaf/"ames adult: retrieving sponsorship chain"
        :: =.  moz
        ::   =+  ev-core=(ev [now eny rof] [/saxo]~ ames-state)
        ::   :_  moz
        ::   :: [unix-duct.state.old %give %saxo get-sponsors:ev-core]  ::  XX can't scry on ++load
                                                                        ::  ask %jael to give us this on ++take
        ::   [[//ames/0v0]~ %give %saxo get-sponsors:ev-core]
        $(old 20+adult/(state-19-to-20 state.old))
      ::
      ?:  ?=(%20 -.old)  $(old 21+adult/(state-20-to-21 state.old))
      ::  XX  emit move to start %dead-flow timers
      ?>  ?=(%21 -.old)  $(old (state-21-to-0 state.old))
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
    %-  ~(urn by peers.ames-state)
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
      (rof [~ ~] /ames %j `beam`[[our %rift %da now] /(scot %p ship)])  ::  XX can't scry on ++load
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
    =+  !<(=rift q:(need (need (rof [~ ~] /ames %j our-beam))))  ::  XX can't scry on ++load
    =+  pk=sec:ex:crypto-core.old
    :*  peers=(~(run by peers.old) ship-state-12-to-13)
        unix-duct.old
        life.old
        rift
        ?:(=(*ring pk) *acru (nol:nu:crub:crypto pk))
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
    old(rift !<(=rift q:(need (need (rof [~ ~] /ames %j our-beam)))))  ::  XX can't scry on ++load
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
    ^-  ames-state-17
    %=    old
        cong
      :+  cong.old
        flow/~
      cork/`[~[/ames] /recork `@da`(add now ~d1)]
      ::
        peers
      %-  ~(run by peers.old)
      |=  ship-state=ship-state-16
      ^-  ship-state-17
      ?.  ?=(%known -.ship-state)
        ship-state
      |^
      %=  ship-state
        snd    (~(run by snd.ship-state) message-pump-16-to-17)
        keens  (~(run by keens.ship-state) keen-state-16-to-17)
        rcv    (~(urn by rcv.ship-state) remove-outbound-naxplanations)
      ==
      ::
      ++  message-pump-16-to-17
        |=  pump=message-pump-state-16
        ^-  message-pump-state-17
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
          ::  less than the current message, +pump-done:mu had been called,
          ::  so the message-num can be safely removed
          ::
          =?  nax  (gte message-num current.u.pump)
            (~(put in nax) message-num)
          nax
        ==
      --
    ==
  ::
  ++  state-18-to-19
    |=  old=ames-state-18
    ^-  ames-state-19
    %=    old
    ::
        dead  [dead.old ~]
    ::
        peers
      %-  ~(run by peers.old)
      |=  s=ship-state-17
      ^-  ship-state-20
      ?:  ?=(%alien -.s)
        %=  s
          keens  [keens.s ~]
        ==
      %=    s
      ::
          keens  [keens.s ~]
      ::
          snd.+
        %-  ~(urn by snd.+.s)
        |=  [=bone m=message-pump-state-17]
        =/  hed
          ?.  =(1 (end 0 bone))
            %plea
          ?:  =(0 (end 0 (rsh 0 bone)))
            %boon
          %naxplanation
        %=    m
            unsent-messages
          =*  um  unsent-messages.m
          =>  [..message hed=hed um=um ..cue]
          ~+  %-  ~(run to um)
          |=  b=message-blob
          ^-  message
          =>  [..message hed=hed ..cue arg=b]
          ~+  ;;(message [hed (cue arg)])
        ==
      ==
    ==
  ::
  ++  state-19-to-20
    |=  old=ames-state-19
    ^-  ames-state-20
    %=  old
      veb.bug  [&1 &2 &3 &4 &5 &6 &7 &8 |8 %.n]:veb.bug.old
    ==
  ::
  ++  state-20-to-21
    |=  old=ames-state-20
    ^-  ames-state-21
    %=     old
        peers
      %-  ~(run by peers.old)
      |=  s=ship-state-20
      ^-  ship-state
      ?:  ?=(%alien -.s)
        [%alien messages.s packets.s keens.s chums.s]
      :*  -.s  -.+.s  route.s  qos.s  ossuary.s  snd.s  rcv.s
          nax.s  closing.s  corked.s  keens.s  chain.s
      ==
    ==
  ::
  ++  state-21-to-0
    |=  old=ames-state-21
    ^-  axle
    :-  %0
    %=  old
      chain  [server-chain=chain.old priv=sec:ex:crypto-core.old chums=~ %ames]
    ==
  --
::  +scry: dereference namespace
::
++  scry
  ^-  roon
  |=  [lyc=gang pov=path car=term bem=beam]
  =*  sample  +<
  ?:  ?&  =(our p.bem)
          =(%$ q.bem)
          =([%ud 1] r.bem)
          =(%x car)
      ==
    =/  tyl=(pole knot)  s.bem
    ?+    tyl  ~
        ?([%fine %shut kef=@ enc=@ ~] [%chum her=@ lyf=@ cyf=@ ~])
      (scry:ames sample)
    ::
        $?  [%hunk lop=@t len=@t pat=*]
            [%mess ryf=@ res=*]
            [%publ lyf=@ pat=*]
            [%chum lyf=@ her=@ hyf=@ cyf=@ ~]
            [%shut kid=@ cyf=@ ~]
            [%flow bone=@ load=?(%plea %boon %ack-plea %ack-boon %nax) rcvr=@ mess=@ ~]
            [%flow bone=@ %cork rcvr=@ ~]
            [%comet %proof rcvr=@ life=@ ~]
            [%whit boq=@ pat=*]
        ==
      (scry:mesa sample)
    ==
  ::
  ?.  ?&  =(our p.bem)
          =([%da now] r.bem)
          =(%$ q.bem)
      ==
    ~
  ::
  ?.  ?=(%x car)  ~
  =/  tyl=(pole knot)  s.bem
  ::  public endpoints
  ::
  ?:  ?=([%fine %hunk lop=@t len=@t pax=^] tyl)
    (scry:ames sample)
  ::  private endpoints
  ::
  ?.  =([~ ~] lyc)  ~
  ?+  tyl   (scry:ames sample)
    [%chums her=@ ~]  (scry:mesa sample)
  ==
::
--