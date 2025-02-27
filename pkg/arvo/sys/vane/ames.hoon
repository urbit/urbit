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
      |=  $:  mode=?(%ames %fine %mesa)
              verb=?
              =ship
              ships=(set ship)
              print=(trap tape)
          ==
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
      |=  [s=ship mode=?(%ames %fine) old=qos new=qos k=? ships=(set ship)]
      ^-  (unit tape)
      ::
      =+  trace=(cury trace mode)
      ?+  [-.old -.new]  ~
        [%unborn %live]  `"; {(scow %p s)} is your neighbor"
        [%dead %live]    ((trace k s ships |.("is ok")) ~)
        [%live %dead]    ((trace k s ships |.("not responding still trying")) ~)
        [%unborn %dead]  ((trace k s ships |.("not responding still trying")) ~)
        [%live %unborn]  `"; {(scow %p s)} has sunk"
        [%dead %unborn]  `"; {(scow %p s)} has sunk"
      ==
    ::
    --
::
=>  ::  vane IO
    ::
    |%
    +$  sign
      $~  [%behn %wake ~]
      $%  [%ames $>(?(%tune %sage) gift)]
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
              $>(?(%deep %keen %meek %moke %mage %prod) task)
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
        ~?  >>>  !(~(has by fragments) index)
          whoops-assemble-fragments/index
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
    ::
    +$  ames-state-22
      $+  ames-state-22
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
              rots=[%rots (unit dead-timer)]
          ==
        ::
          =chain
        ==
    ::
    +$  ames-state-21
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
      $%  [?(%heed %jilt) =ship]      ::  introduced in state %4, removed in %21
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
      $%  [%kroc dry=?]             ::  introduced in state %10, modified in %17
          [%snub ships=(list ship)] ::  introduced in state %9,  modified in %11
          [?(%heed %jilt) =ship]    ::  introduced in state %4, removed in %21
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
      $%  [%kroc dry=?]             ::  introduced in state %10, modified in %17
          [%keen spar]              ::  introduced in state %13, modified in %19
          deep-task-14              ::  introduced in state %14, modified in %19
          [?(%heed %jilt) =ship]    ::  introduced in state %4, removed in %21
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
      $%  [%keen spar]              ::  introduced in state %13, modified in %19
          deep-task-14              ::  introduced in state %14, modified in %19
          [?(%heed %jilt) =ship]    ::  introduced in state %4, removed in %21
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
      $%  [?(%heed %jilt) =ship]      ::  introduced in state %4, removed in %21
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
      =;  zar=(trap (list lane:pact))
        ?~  lane.fren  $:zar
        [u.lane.fren $:zar]
      |.  ^-  (list lane:pact)
      ?:  ?=(%czar (clan:title sponsor.fren))
        ?:  =(our sponsor.fren)
          ~
        [`@ux`sponsor.fren]~
      =/  next  (~(get by chums) sponsor.fren)
      ?.  ?=([~ %known *] next)
        ~
      $(fren +.u.next)
    ::
    ++  key-chain     ((on ,@ ,[key=@ =path]) lte)
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
    ++  validate-path
      |=  =path
      ^-  (pole iota)
      ~|  path
      %-  mesa-pave
      ?~  inn=(inner-path-to-beam *@p path)  ~
      ~|  u.inn
      ?>  =([[%a %x] *@p %$ ud+1] [vew -.bem]:u.inn)
      s.bem.u.inn
    ::
    ++  mesa-pave
      |=  =path
      ^-  pith
      %+  turn  path
      |=  i=@ta
      (fall (rush i spot:stip) i)
    ::
    ++  make-space-path
      |=  [=space =path]
      ^+  path
      =>  [space=space path=path ..crypt]
      ?-    -.space
          %none  path
          %publ  `^path`[%publ (scot %ud life.space) path]  :: unencrypted
      ::
          %chum  :: encrypted with eddh key
        :-  %chum
        ^+  path
        :~  (scot %ud server-life.space)
            (scot %p client.space)
            (scot %ud client-life.space)
            (scot %uv (seal-path:crypt `@`key.space path))
        ==
      ::
          %shut  :: encrypted with group key, provided by the %keen task
        =/  cyf  (seal-path:crypt key.space path)
        /shut/[(scot %ud kid.space)]/[(scot %uv cyf)]
      ==
    ::
    +|  %encryption
    ::
    +$  binding  [=path root=@uxI]
    ++  get-group-key-for  |=(@ud `(unit @uxI)`(some `@uxI`0))  :: XX implement?
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
    +|  %flow-signs
    ::
    +$  flow-sign
      $%  $>(?(%flub %done) gift:gall)  :: from vanes
          [%sage seq=@ud sage:mess]     :: added seq number to %sage
      ==
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
    +$  chum-pith
      [%chum [%ud lyf=@ud] [%p her=@p] [%ud hyf=@ud] [%uv cyf=@uv] ~]
    ::
    +$  shut-pith  [%shut [%ud kid=@ud] [%uv cyf=@uv] ~]
    ::
    +|  %message-flow-paths
    ::
    +$  flow-pith
      $:  %flow
          [%ud bone=@ud]
          =load
          =dire
          [%p rcvr=@p]
          [%ud mess=@ud]
          ~
      ==
    ::
    +$  cork-pith
      $:  %flow
          [%ud bone=@ud]
          =load
          =dire
          [%p rcvr=@p]
          ~
      ==
    ::
    +|  %attestation-path
    ::
    +$  poof-pith
      $:  %pawn
          %proof
          [%p rcvr=@p]
          [%ud life=@ud]  :: XX reddundant?
          ~
      ==
    ::
    +|  %migration
    ::
    ++  print-check
      |=  [=term check=?]
      ^+  check
      ?:  check  check
      %-  (slog leaf/"{(trip term)}: failed" ~)
      check
    ::  +migration-test: .ames is the original state before %ahoy
    ::                   .back is the regressed state, from $chums to %ships
    ::
    ++  migration-test
      |=  [ames=ship-state back=ship-state]
      ^-  ?
      ?>  =(-.ames -.back)     :: both %known or %alien
      ?:  ?=(%alien -.ames)
        =(ames back)
      ::
      ?&  ?=(%known -.back)
          %+  print-check  %keys     =(+<.ames +<.back)
          %+  print-check  %route    ?|  ?&  ?=(~ route.ames)
                                             =(route.ames route.back)
                                         ==
                                         ?&  ?=(^ route.ames)  ?=(^ route.back)
                                             .=  lane.u.route.ames
                                                 lane.u.route.back
                                     ==  ==
          %+  print-check  %ossuary  =(ossuary.ames ossuary.back)
          %+  print-check  %closing  =(closing.ames closing.back)
          =-  ~?  !-  [ames=corked.ames back=corked.back]
              -
          %+  print-check  %corked   =(corked.ames corked.back)
          %+  print-check  %chain    =(chain.ames chain.back)
          =-  ~?  !-  [ames=keens.ames back=keens.back]
              -
          %+  print-check  %keens   =(~(key by keens.ames) ~(key by keens.back))
          %+  print-check  %nax     =(nax.ames nax.back)  :: XX ?
        ::  forward flows
        ::
          %+  print-check  %forward-flows
          %-  ~(rep by snd.ames)
          |=  [[=bone pump=message-pump-state] ok=?]
          ?:  =(%3 (mod bone 4))  ok  :: ignore naxplanation bones
          =+  back-pump=(~(got by snd.back) bone)
          ?&  ok
              =-  ~?  !-  [bone=bone ames=current.pump back=current.back-pump]
                  -
              %+  print-check  %forward-flows-current
              =(current.pump current.back-pump)
              =-  ~?  !-  [bone=bone ames=next.pump back=next.back-pump]
                  -
              %+  print-check  %forward-flows-next
              =(next.pump next.back-pump)
              ::  XX TODO: check live message sequence number
              ::
          ==
        ::  backwards flows
        ::
          %+  print-check  %backwards-flows
          %-  ~(rep by rcv.ames)
          |=  [[=bone sink=message-sink-state] ok=?]
          ?:  =(%2 (mod bone 4))  ok  :: ignore naxplanation %ack bones
          =+  back-sink=(~(got by rcv.back) bone)
          ?&  ok
              =-  ~?  !-
                    [bone=bone ames=last-acked.sink back=last-acked.back-sink]
                  -
              %+  print-check  %backwards-flows-acked
              =(last-acked.sink last-acked.back-sink)
          ==
      ==
    ::
    ++  regression-test
      |=  [mesa=chum-state back=chum-state]
      ^-  ?
      ?>  =(-.mesa -.back)     :: both %known or %alien
      ?:  ?=(%alien -.mesa)
        =(mesa back)
      ::
      ?&  ?=(%known -.back)
          %+  print-check  %keys     =(+<.mesa +<.back)
          %+  print-check  %lane    ?|  ?&  ?=(~ lane.mesa)
                                             =(lane.mesa lane.back)
                                         ==
                                         ?&  ?=(^ lane.mesa)  ?=(^ lane.back)
                                             =(u.lane.mesa u.lane.back)
                                     ==  ==
          %+  print-check  %ossuary  =(ossuary.mesa ossuary.back)
          :: %+  print-check  %closing  =(closing.mesa closing.back)
          =-  ~?  !-  [mesa=corked.mesa back=corked.back]
              -
          %+  print-check  %corked   =(corked.mesa corked.back)
          %+  print-check  %chain    =(client-chain.mesa client-chain.back)
          :: %+  print-check  %pit      =(pit.mesa pit.back)  :: XX
        ::  flows
        ::
          %+  print-check  %flows
          ::  some flows from the mesa state (e.g. in progress corks)
          ::  could have been removed, so we focus on the ones that exist
          ::  after regression
          ::
          %-  ~(rep by flows.back)
          |=  [[side back-flow=flow-state] ok=?]
          ~|  [%not-found side=bone^dire]
          =+  flow=(~(got by flows.mesa) bone^dire)
          ?&  ok
              :: XX lines don't match for the ahoy flow
              :: =(line.flow line.back-flow)
              =(closing.flow closing.back-flow)
              :: =-  ~?  !-  snd/[mesa=snd.flow back=snd.back-flow]
              ::     -
              =(snd.flow snd.back-flow)
              :: =-  ~?  !-  rcv/[mesa=rcv.flow back=rcv.back-flow]
              ::     -
              ::  nacked pokes are not migrated
              ::
              .=  [last-acked pending-ack]:rcv.flow
                  [last-acked pending-ack]:rcv.back-flow
          ==
      ==
    ::
    --
::  external vane interface
::
|=  our=ship
=|  ames-state=axle
=*  unix-duct  unix-duct.ames-state
=*  our-rift   rift.ames-state
::
=<  ::  %larval core
    ::
    =*  adult-gate  .
    ::  Queued events are a remainder of past version of the ames-core, and are
    ::  still supported only for backward compatibility (i.e. ships migrating
    ::  from an old version of %ames)
    ::
    ::  The purpose of the larval-core is to allow emitting moves and reading
    ::  from the arvo namespace in migrations to newer version of %ames. In the
    ::  past the purpose of the larval-core was to wait for a %born event that
    ::  sets up the unix-duct from the ames IO driver but as of the Directed
    ::  Messaging release, we could get the %born event at any time, so we
    ::  account for it by deferring any flow that could use the unix-duct
    ::
    =|  queued-events=(qeu queued-event)
    =|  $=  cached-state
        %-  unit
        $%  [%4 ames-state-4]
            [%5 ames-state-5]
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
            [%17 ames-state-17]
            [%18 ames-state-17]
            [%19 ames-state-19]
            [%20 ames-state-20]
            [%21 ames-state-21]
            [%22 ames-state-22]
            [%23 axle]
        ==
    ::
    ::
    |=  [now=@da eny=@ rof=roof]
    =*  larval-gate  .
    =*  adult-core   (adult-gate +<)
    ::  formal interface with no extra arms
    ::
    =<  |%
        ++  call  ^call
        ++  load  ^load
        ++  scry  ^scry
        ++  stay  ^stay
        ++  take  ^take
        --
    ::
    |%
    ++  larval-core  .
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
      ::  start draining queued events and then metamorphose
      ::
      =^  queu-moves  adult-gate
        =|  moves=(list move)
        |-  ^+  [moves adult-gate]
        ?:  =(~ queued-events)
          [(flop moves) adult-gate]
        =^  first-event  queued-events  ~(get to queued-events)
        =^  event-moves  adult-gate
          ?-  -.first-event
            %call  (call:adult-core [duct ~ wrapped-task]:+.first-event)
            %take  (take:adult-core [wire duct ~ sign]:+.first-event)
          ==
        $(moves (weld event-moves moves))
      ::
      =^  call-moves  adult-gate  (call:adult-core duct dud task)
      ~>  %slog.0^leaf/"ames: metamorphosis on %call"
      [:(weld molt-moves queu-moves call-moves) adult-gate]
    ::
    ++  take
      |=  [=wire =duct dud=(unit goof) =sign]
      ?^  dud
        ~|(%ames-larval-take-dud (mean tang.u.dud))
      ::
      =^  molt-moves  larval-core  molt
      ::  start draining queued events and then metamorphose
      ::
      =^  queu-moves  adult-gate
        =|  moves=(list move)
        |-  ^+  [moves adult-gate]
        ?:  =(~ queued-events)
          [(flop moves) adult-gate]
        ::
        =^  first-event  queued-events  ~(get to queued-events)
        =^  event-moves  adult-gate
          ?-  -.first-event
            %call  (call:adult-core [duct ~ wrapped-task]:+.first-event)
            %take  (take:adult-core [wire duct ~ sign]:+.first-event)
          ==
        $(moves (weld event-moves moves))
      ::
      =^  take-moves  adult-gate  (take:adult-core wire duct dud sign)
      ~>  %slog.0^leaf/"ames: metamorphosis on %take"
      [:(weld molt-moves queu-moves take-moves) adult-gate]
    ::
    ++  stay  [%23 larva/ames-state]
    ++  scry  scry:adult-core
    ++  load
      |=  $=  old
          $%  $:  %4
              $%  $:  %larva
                      events=(qeu queued-event-4-til-8)
                      state=ames-state-4
                  ==
                  [%adult state=ames-state-4]
              ==  ==
              $:  %5
              $%  $:  %larva
                      events=(qeu queued-event-4-til-8)
                      state=ames-state-5
                  ==
                  [%adult state=ames-state-5]
              ==  ==
              $:  %6
              $%  $:  %larva
                      events=(qeu queued-event-4-til-8)
                      state=ames-state-6
                  ==
                  [%adult state=ames-state-6]
              ==  ==
              $:  %7
              $%  $:  %larva
                      events=(qeu queued-event-4-til-8)
                      state=ames-state-7
                  ==
                  [%adult state=ames-state-7]
              ==  ==
              $:  %8
              $%  $:  %larva
                      events=(qeu queued-event-4-til-8)
                      state=ames-state-8
                  ==
                  [%adult state=ames-state-8]
              ==  ==
              $:  %9                             :: %snub introduced
              $%  $:  %larva
                      events=(qeu queued-event-9-til-11)
                      state=ames-state-9
                  ==
                  [%adult state=ames-state-9]
              ==  ==
              $:  %10                            :: %kroc introduced
              $%  $:  %larva
                      events=(qeu queued-event-9-til-11)
                      state=ames-state-10
                  ==
                  [%adult state=ames-state-10]
              ==  ==
              $:  %11
              $%  $:  %larva
                      events=(qeu queued-event-9-til-11)
                      state=ames-state-11
                  ==
                  [%adult state=ames-state-11]
              ==  ==
              $:  %12                            :: %snub modified
              $%  $:  %larva
                      events=(qeu queued-event-12-til-16)
                      state=ames-state-12
                  ==
                  [%adult state=ames-state-12]
              ==  ==
              $:  %13                            :: %keen introduced
              $%  $:  %larva
                      events=(qeu queued-event-12-til-16)
                      state=ames-state-13
                  ==
                  [%adult state=ames-state-13]
              ==  ==
              $:  %14                            :: %deep introduced
              $%  $:  %larva
                      events=(qeu queued-event-12-til-16)
                      state=ames-state-14
                  ==
                  [%adult state=ames-state-14]
              ==  ==
              $:  %15
              $%  $:  %larva
                      events=(qeu queued-event-12-til-16)
                      state=ames-state-15
                  ==
                  [%adult state=ames-state-15]
              ==  ==
              $:  %16
              $%  $:  %larva
                      events=(qeu queued-event-12-til-16)
                      state=ames-state-16
                  ==
                  [%adult state=ames-state-16]
              ==  ==
              $:  %17                            :: %kroc modified
              $%  $:  %larva
                      events=(qeu queued-event-17-and-18)
                      state=ames-state-17
                  ==
                  [%adult state=ames-state-17]
              ==  ==
              $:  %18
              $%  $:  %larva
                      events=(qeu queued-event-17-and-18)
                      state=ames-state-18
                  ==
                  [%adult state=ames-state-18]
              ==  ==
              $:  %19                            :: %keen & %deep modified
              $%  $:  %larva
                      events=(qeu queued-event-19-and-20)
                      state=ames-state-19
                  ==
                  [%adult state=ames-state-19]
              ==  ==
              $:  %20                            :: start informal %ping
              $%  $:  %larva
                      events=(qeu queued-event-19-and-20)
                      state=ames-state-20
                  ==
                  [%adult state=ames-state-20]
              ==  ==
              $:  %21                            :: remove %heed and %jilt
              $%  $:  %larva
                      events=(qeu queued-event)
                      state=ames-state-21
                  ==
                  [%adult state=ames-state-21]
              ==  ==
              $:  %22                            :: start dead /routes timer
              $%  $:  %larva
                      events=(qeu queued-event)
                      state=ames-state-22
                  ==
                  [%adult state=ames-state-22]
              ==  ==
              $:  %23                            :: Directed Messaging
                  ?(%adult %larva)               ::   (removes queued-events)
                  state=axle
          ==  ==
      |^  ?-  old
          [%4 %adult *]
        =.  cached-state  `[%4 state.old]
        ~>  %slog.1^leaf/"ames: larva %4 reload"
        larval-gate
      ::
          [%4 %larva *]
        =.  cached-state  `[%4 state.old]
        ~>  %slog.1^leaf/"ames: larva %5 load"
        larval-gate
      ::
          [%5 %adult *]
        =.  cached-state  `[%5 state.old]
        ~>  %slog.1^leaf/"ames: larva %5 reload"
        larval-gate
      ::
          [%5 %larva *]
        ~>  %slog.1^leaf/"ames: larva %5 load"
        =.  cached-state   `[%5 state.old]
        =.  queued-events   %-  event-20-to-last
                            %-  event-17-and-18-to-20
                            %-  event-12-til-16-to-17
                            %-  event-9-til-11-to-12
                            events.old
        larval-gate
      ::
          [%6 %adult *]
        =.  cached-state  `[%6 state.old]
        ~>  %slog.1^leaf/"ames: larva %6 reload"
        larval-gate
      ::
          [%6 %larva *]
        ~>  %slog.1^leaf/"ames: larva %6 load"
        =.  cached-state   `[%6 state.old]
        =.  queued-events   %-  event-20-to-last
                            %-  event-17-and-18-to-20
                            %-  event-12-til-16-to-17
                            %-  event-9-til-11-to-12
                            events.old
        larval-gate
      ::
          [%7 %adult *]
        =.  cached-state  `[%7 state.old]
        ~>  %slog.1^leaf/"ames: larva %7 reload"
        larval-gate
      ::
          [%7 %larva *]
        ~>  %slog.1^leaf/"ames: larva %7 load"
        =.  cached-state   `[%7 state.old]
        =.  queued-events   %-  event-20-to-last
                            %-  event-17-and-18-to-20
                            %-  event-12-til-16-to-17
                            %-  event-9-til-11-to-12
                            events.old
        larval-gate
      ::
          [%8 %adult *]
        =.  cached-state  `[%8 state.old]
        ~>  %slog.1^leaf/"ames: larva %8 reload"
        larval-gate
      ::
          [%8 %larva *]
        ~>  %slog.1^leaf/"ames: larva %8 load"
        =.  cached-state   `[%8 state.old]
        =.  queued-events   %-  event-20-to-last
                            %-  event-17-and-18-to-20
                            %-  event-12-til-16-to-17
                            %-  event-9-til-11-to-12
                            events.old
        larval-gate
      ::
          [%9 %adult *]
        =.  cached-state  `[%9 state.old]
        ~>  %slog.1^leaf/"ames: larva %9 reload"
        larval-gate
      ::
          [%9 %larva *]
        ~>  %slog.1^leaf/"ames: larva %9 load"
        =.  cached-state   `[%9 state.old]
        =.  queued-events   %-  event-20-to-last
                            %-  event-17-and-18-to-20
                            %-  event-12-til-16-to-17
                            %-  event-9-til-11-to-12
                            events.old
        larval-gate
      ::
          [%10 %adult *]
        =.  cached-state  `[%10 state.old]
        ~>  %slog.1^leaf/"ames: larva %10 reload"
        larval-gate
      ::
          [%10 %larva *]
        ~>  %slog.1^leaf/"ames: larva %10 load"
        =.  cached-state   `[%10 state.old]
        =.  queued-events   %-  event-20-to-last
                            %-  event-17-and-18-to-20
                            %-  event-12-til-16-to-17
                            %-  event-9-til-11-to-12
                            events.old
        larval-gate
      ::
          [%11 %adult *]
        =.  cached-state  `[%11 state.old]
        ~>  %slog.1^leaf/"ames: larva %11 reload"
        larval-gate
      ::
          [%11 %larva *]
        ~>  %slog.1^leaf/"ames: larva %11 load"
        =.  cached-state   `[%11 state.old]
        =.  queued-events   %-  event-20-to-last
                            %-  event-17-and-18-to-20
                            %-  event-12-til-16-to-17
                            %-  event-9-til-11-to-12
                            events.old
        larval-gate
      ::
          [%12 %adult *]
        =.  cached-state  `[%12 state.old]
        ~>  %slog.1^leaf/"ames: larva %12 reload"
        larval-gate
      ::
          [%12 %larva *]
        ~>  %slog.1^leaf/"ames: larva %12 load"
        =.  cached-state   `[%12 state.old]
        =.  queued-events   %-  event-20-to-last
                            %-  event-17-and-18-to-20
                            %-  event-12-til-16-to-17
                            events.old
        larval-gate
      ::
          [%13 %adult *]
        =.  cached-state  `[%13 state.old]
        ~>  %slog.1^leaf/"ames: larva %13 reload"
        larval-gate
      ::
          [%13 %larva *]
        ~>  %slog.1^leaf/"ames: larva %13 load"
        =.  cached-state   `[%13 state.old]
        =.  queued-events   %-  event-20-to-last
                            %-  event-17-and-18-to-20
                            %-  event-12-til-16-to-17
                            events.old
        larval-gate
      ::
          [%14 %adult *]
        =.  cached-state  `[%14 state.old]
        ~>  %slog.1^leaf/"ames: larva %14 reload"
        larval-gate
      ::
          [%14 %larva *]
        ~>  %slog.1^leaf/"ames: larva %14 load"
        =.  cached-state   `[%14 state.old]
        =.  queued-events   %-  event-20-to-last
                            %-  event-17-and-18-to-20
                            %-  event-12-til-16-to-17
                            events.old
        larval-gate
      ::
          [%15 %adult *]
        =.  cached-state  `[%15 state.old]
        ~>  %slog.0^leaf/"ames: larva %15 reload"
        larval-gate
      ::
          [%15 %larva *]
        ~>  %slog.1^leaf/"ames: larva %15 load"
        =.  cached-state   `[%15 state.old]
        =.  queued-events   %-  event-20-to-last
                            %-  event-17-and-18-to-20
                            %-  event-12-til-16-to-17
                            events.old
        larval-gate
      ::
          [%16 %adult *]
        =.  cached-state  `[%16 state.old]
        ~>  %slog.1^leaf/"ames: larva %16 reload"
        larval-gate
      ::
          [%16 %larva *]
        ~>  %slog.1^leaf/"ames: larva %16 load"
        =.  cached-state   `[%16 state.old]
        =.  queued-events   %-  event-20-to-last
                            %-  event-17-and-18-to-20
                            %-  event-12-til-16-to-17
                            events.old
        larval-gate
      ::
          [%17 %adult *]
        =.  cached-state  `[%17 state.old]
        ~>  %slog.1^leaf/"ames: larva %17 reload"
        larval-gate
      ::
          [%17 %larva *]
        ~>  %slog.1^leaf/"ames: larva %17 load"
        =.  cached-state   `[%17 state.old]
        =.  queued-events   %-  event-20-to-last
                            %-  event-17-and-18-to-20
                            events.old
        larval-gate
      ::
          [%18 %adult *]
        =.  cached-state  `[%18 state.old]
        ~>  %slog.1^leaf/"ames: larva %18 reload"
        larval-gate
      ::
          [%18 %larva *]
        ~>  %slog.1^leaf/"ames: larva %18 load"
        =.  cached-state   `[%18 state.old]
        =.  queued-events   %-  event-20-to-last
                            %-  event-17-and-18-to-20
                            events.old
        larval-gate
      ::
          [%19 %adult *]
        =.  cached-state  `[%19 state.old]
        ~>  %slog.1^leaf/"ames: larva %19 reload"
        larval-gate
      ::
          [%19 %larva *]
        ~>  %slog.1^leaf/"ames: larva %19 load"
        =.  cached-state   `[%19 state.old]
        =.  queued-events  (event-20-to-last events.old)
        larval-gate
      ::
          [%20 %adult *]
        =.  cached-state  `[%20 state.old]
        ~>  %slog.1^leaf/"ames: larva %20 reload"
        larval-gate
      ::
          [%20 %larva *]
        ~>  %slog.1^leaf/"ames: larva %20 load"
        =.  cached-state  `[%20 state.old]
        =.  queued-events  (event-20-to-last events.old)
        larval-gate
      ::
          [%21 %adult *]
        =.  cached-state  `[%21 state.old]
        ~>  %slog.1^leaf/"ames: larva %21 reload"
        larval-gate
      ::
          [%21 %larva *]
        =.  cached-state  `[%21 state.old]
        ~>  %slog.1^leaf/"ames: larva %21 load"
        =.  queued-events  events.old
        larval-gate
      ::
          [%22 %adult *]
        =.  cached-state  `[%22 state.old]
        ~>  %slog.1^leaf/"ames: larva %22 reload"
        larval-gate
      ::
          [%22 %larva *]
        =.  cached-state  `[%22 state.old]
        ~>  %slog.1^leaf/"ames: larva %22 load"
        =.  queued-events  events.old
        larval-gate
      ::
          [%23 *]
        ?-  +<.old
          %larva  larval-gate
          %adult  (load:adult-core state.old)
        ==
      ==
      ::
      ++  event-9-til-11-to-12
        |=  events=(qeu queued-event-9-til-11)
        ^-  (qeu queued-event-12-til-16)
        %-  ~(rep in events)
        |=  [e=queued-event-9-til-11 q=(qeu queued-event-12-til-16)]
        %-  ~(put to q)  ^-  queued-event-12-til-16
        ?.  ?=(%call -.e)  e
        =/  task=task-9-til-11  ((harden task-9-til-11) wrapped-task.e)
        %=    e
            wrapped-task
          ^-  task-12-til-16
          ?+  -.task  task
            %snub  [%snub %deny ships.task]
          ==
        ==
      ::
      ++  event-12-til-16-to-17
        |=  events=(qeu queued-event-12-til-16)
        ^-  (qeu queued-event-17-and-18)
        %-  ~(rep in events)
        |=  [e=queued-event-12-til-16 q=(qeu queued-event-17-and-18)]
        %-  ~(put to q)  ^-  queued-event-17-and-18
        ?.  ?=(%call -.e)  e
        =/  task=task-12-til-16  ((harden task-12-til-16) wrapped-task.e)
        %=    e
            wrapped-task
          ^-  task-16-and-18
          ?.  ?=(%kroc -.task)  task
          [%kroc ~]
        ==
      ::
      ++  event-17-and-18-to-20
        |=  events=(qeu queued-event-17-and-18)
        ^-  (qeu queued-event-19-and-20)
        %-  ~(rep in events)
        |=  [e=queued-event-17-and-18 q=(qeu queued-event-19-and-20)]
        %-  ~(put to q)  ^-  queued-event-19-and-20
        ?.  ?=(%call -.e)  e
        =/  task=task-16-and-18  ((harden task-16-and-18) wrapped-task.e)
        %=    e
            wrapped-task
          ^-  task-19-and-20
          ?:  ?=(%keen -.task)
            [%keen ~ +.task]
          ?.  ?=([%deep %nack *] task)  task
          =/  msg  =>([cue=cue arg=message-blob.task] ~+((cue arg)))
          =/  hed
            ?.  =(1 (end 0 nack-bone.task))
              %plea
            ?:  =(0 (end 0 (rsh 0 nack-bone.task)))
              %boon
            %naxplanation
          [%deep %nack ship.task nack-bone.task ;;(message [hed msg])]
        ==
      ::
      ++  event-20-to-last
        |=  events=(qeu queued-event-19-and-20)
        ^-  (qeu queued-event)
        %-  ~(rep in events)
        |=  [e=queued-event-19-and-20 q=(qeu queued-event)]
        ?.  ?=(%call -.e)  (~(put to q) e)
        =/  task=task-19-and-20  ((harden task-19-and-20) wrapped-task.e)
        ?:  ?=(?(%heed %jilt) -.task)  q
        (~(put to q) e(wrapped-task task))
      ::
      --
    ::
    ++  molt
      =|  moz=(list move)
      |^  ^+  [moz larval-core]
      ?~  cached-state  [~ larval-core]
      =*  old  u.cached-state
      ?:  ?=(%23 -.old)
        ::  no state migrations left; update state, clear cache, and exit
        ::
        [(flop moz) larval-core(ames-state.adult-gate +.old, cached-state ~)]
      ::
      ?:  ?=(%4 -.old)   $(cached-state `5+(state-4-to-5 +.old))
      ?:  ?=(%5 -.old)   $(cached-state `6+(state-5-to-6 +.old))
      ?:  ?=(%6 -.old)   $(cached-state `7+(state-6-to-7 +.old))
      ?:  ?=(%7 -.old)
        ~>  %slog.0^leaf/"ames: init daily recork timer"
        %_  $
          cached-state  `8+(state-7-to-8 +.old)
                   moz  [[/ames]~ %pass /recork %b %wait `@da`(add now ~d1)]^moz
        ==
      ::
      ?:  ?=(%8 -.old)   $(cached-state `9+(state-8-to-9 +.old))
      ?:  ?=(%9 -.old)   $(cached-state `10+(state-9-to-10 +.old))
      ?:  ?=(%10 -.old)  $(cached-state `11+(state-10-to-11 +.old))
      ?:  ?=(%11 -.old)  $(cached-state `12+(state-11-to-12 +.old))
      ?:  ?=(%12 -.old)  $(cached-state `13+(state-12-to-13 +.old))
      ?:  ?=(%13 -.old)  $(cached-state `14+(state-13-to-14 +.old))
      ?:  ?=(%14 -.old)  $(cached-state `15+(state-14-to-15 +.old))
      ?:  ?=(%15 -.old)  $(cached-state `16+(state-15-to-16 +.old))
      ?:  ?=(%16 -.old)
        %_    $
            cached-state  `17+(state-16-to-17 +.old)
            moz
          ?:  ?=(~ moz)
            moz  ::  if we have just added the timer in state-7-to-8, skip
          =;  recork-timer=(list [@da duct])
            ?^  recork-timer  moz
            ~>  %slog.0^leaf/"ames: init daily recork timer"
            [[/ames]~ %pass /recork %b %wait `@da`(add now ~d1)]^moz
          %+  skim
            ;;  (list [@da duct])
            =<  q.q  %-  need  %-  need
            (rof [~ ~] /ames %bx [[our %$ da+now] /debug/timers])
          |=([@da =duct] ?=([[%ames %recork *] *] duct))
        ==
      ::
      ?:  ?=(%17 -.old)
        ~>  %slog.0^leaf/"ames: fetching our public keys"
        %_    $
            moz
          ^-  (list move)
          [[/ames]~ %pass /public-keys %j %public-keys [n=our ~ ~]]^moz
        ==
      ::
      ?:  ?=(%18 -.old)  $(cached-state `19+(state-18-to-19 +.old))
      ?:  ?=(%19 -.old)
        ~>  %slog.0^leaf/"ames: retrieving sponsorship chain"
        =.  moz
          =+  ev-core=(ev:ames:adult-core [now eny rof] [/saxo]~ ames-state)
          ^-  (list move)
          [unix-duct.+.old %give %saxo get-sponsors:ev-core]^moz
          :: [[//ames/0v0]~ %give %saxo get-sponsors:ev-core]
        $(cached-state `20+(state-19-to-20 +.old))
      ::
      ?:  ?=(%20 -.old)  $(cached-state `21+(state-20-to-21 +.old))
      ?:  ?=(%21 -.old)
        %_    $
            cached-state  `22+(state-21-to-22 +.old)
        ::
            moz
          ^-  (list move)
          [[/ames]~ %pass /routes %b %wait `@da`(add now ~m2)]^moz
        ==
      ?>  ?=(%22 -.old)
      ~>  %slog.0^leaf/"ames: init |mesa retry timer"
      %_  $
        cached-state  `23+(state-22-to-23 +.old)
      ::
          moz
        [[/ames]~ %pass /mesa/retry %b %wait `@da`(add now ~m2)]^moz
      ==
      ::
      ++  our-beam  `beam`[[our %rift %da now] /(scot %p our)]
      ++  state-4-to-5
        |=  ames-state=ames-state-4
        ^-  ames-state-5
        ~>  %slog.0^leaf/"ames: migrating from state %4 to %5"
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
        ~>  %slog.0^leaf/"ames: migrating from state %5 to %6"
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
          (rof [~ ~] /ames %j `beam`[[our %rift %da now] /(scot %p ship)])
        :-   -.ship-state
        :_  +.peer-state
        =,  -.peer-state
        [symmetric-key life rift public-key sponsor]
      ::
      ++  state-6-to-7
        |=  ames-state=ames-state-6
        ^-  ames-state-7
        ~>  %slog.0^leaf/"ames: migrating from state %6 to %7"
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
        ~>  %slog.0^leaf/"ames: migrating from state %7 to %8"
        =,  ames-state
        :*  peers  unix-duct  life  crypto-core  bug
            *(set wire)
        ==
      ::
      ++  state-8-to-9
        |=  ames-state=ames-state-8
        ^-  ames-state-9
        ~>  %slog.0^leaf/"ames: migrating from state %8 to %9"
        =,  ames-state
        :*  peers  unix-duct  life  crypto-core  bug  corks
            *(set ship)
        ==
      ::
      ++  state-9-to-10
        |=  ames-state=ames-state-9
        ^-  ames-state-10
        ~>  %slog.0^leaf/"ames: migrating from state %9 to %10"
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
        ~>  %slog.0^leaf/"ames: migrating from state %10 to %11"
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
        ~>  %slog.0^leaf/"ames: migrating from state %11 to %12"
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
        ~>  %slog.0^leaf/"ames: migrating from state %12 to %13"
        =+  !<(=rift q:(need (need (rof [~ ~] /ames %j our-beam))))
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
        ~>  %slog.0^leaf/"ames: migrating from state %13 to %14"
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
        ~>  %slog.0^leaf/"ames: migrating from state %14 to %15"
        old(rift !<(=rift q:(need (need (rof [~ ~] /ames %j our-beam)))))
      ::
      ++  state-15-to-16
        |=  old=ames-state-15
        ^-  ames-state-16
        ~>  %slog.0^leaf/"ames: migrating from state %15 to %16"
        ::  re-initialize default congestion control values, if bunted
        ::
        old(cong ?.(=(cong.old [0 0]) cong.old [5 100.000]))
      ::
      ++  state-16-to-17
        |=  old=ames-state-16
        ^-  ames-state-17
        ~>  %slog.0^leaf/"ames: migrating from state %16 to %17"
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
            |=  p=message-pump-state-16
            ^-  message-pump-state-17
            %=    p
                metrics.packet-pump-state
              [rto rtt rttvar ssthresh cwnd counter]:metrics.packet-pump-state.p
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
        ~>  %slog.0^leaf/"ames: migrating from state %18 to %19"
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
        ~>  %slog.0^leaf/"ames: migrating from state %19 to %20"
        %=  old
          veb.bug  [&1 &2 &3 &4 &5 &6 &7 &8 |8 %.n]:veb.bug.old
        ==
      ::
      ++  state-20-to-21
        |=  old=ames-state-20
        ^-  ames-state-21
        ~>  %slog.0^leaf/"ames: migrating from state %20 to %21"
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
      ++  state-21-to-22
        |=  old=ames-state-21
        ^-  ames-state-22
        ~>  %slog.0^leaf/"ames: migrating from state %21 to %22"
        %=    old
            cork.dead
          :-  cork.dead.old
          rots/`[~[/ames] /routes `@da`(add now ~m2)]
        ==
      ::
      ++  state-22-to-23
        |=  old=ames-state-22
        ~>  %slog.0^leaf/"ames: migrating from state %22 to %23"
        ^-  axle
        ::  removes crypto cores and adds the private key to the state
        ::
        =,  old
        :*  peers  unix-duct  life  rift  bug  snub  cong
          ::
            :^    flow.dead  ::  preserve |ames dead-flow consolidation
                chum/`[~[/ames] /mesa/retry `@da`(add now ~m2)]
              cork.dead
            rots.dead
          ::
            server-chain=chain
            priv=sec:ex:crypto-core
            chums=~
            %ames
        ==
      ::
      --
    ::
    --
::  %adult core
::
|=  [now=@da eny=@uvJ rof=roof]
=*  vane-gate  .
=>  ::  network protocol core handlers
    ::
    |%
    ++  ames
      ::
      =<  ::  adult |ames formal interface, after metamorphosis from larva
          ::
          |=  [now=@da eny=@uvJ rof=roof]
          =*  veb  veb.bug.ames-state
          |%
          ::  +call: handle request $task
          ::
          ++  call
            |=  [=duct dud=(unit goof) wrapped-task=(hobo task)]
            ^-  [(list move) _vane-gate]
            ::
            ~|  wrapped-task
            =/  =task       ((harden task) wrapped-task)
            =/  event-core  (ev now^eny^rof duct ames-state)
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
            =/  event-core  (ev now^eny^rof duct ames-state)
            ::
            =^  moves  ames-state
              ?:  ?=([%gall %unto *] sign)
                `ames-state
              ::
              =<  abet
              ?+  sign  ~&(ames-weird-take-sign/[&1^&2]:sign^wire event-core)
                [@ %done *]  (on-take-done:event-core wire error.sign)
                [@ %boon *]  (on-take-boon:event-core wire +>.sign)
                [@ %noon *]  (on-take-noon:event-core wire +>.sign)
              ::
                [%ames %tune *]  (on-tune:event-core wire [+>- roar]:sign)
              ::
                [%behn %wake *]  (on-take-wake:event-core wire error.sign)
              ::
                [%gall %flub ~]  (on-take-flub:event-core wire)
              ==
            ::
            [moves vane-gate]
          ::  +scry: dereference namespace
          ::
          ++  scry  ^scry
          --
      ~%  %per-event  ..trace  ~
      |%
      ::  |ev: inner event-handling core
      ::
      ++  ev
        =|  moves=(list move)
        ~%  %event-gate  ..ev  ~
        |=  [[now=@da eny=@uvJ rof=roof] =duct ames-state=axle]
        =*  veb  veb.bug.ames-state
        =|  cork-bone=(unit bone)  ::  modified by +on-kroc
        =|  vane=?(%fine %ames)
        ~%  %event-core  ..$  ~
        |%
        +|  %helpers
        ::
        ++  event-core  .
        ++  abet  [(flop moves) ames-state]
        ++  emit  |=(=move event-core(moves [move moves]))
        ++  emil  |=(mos=_moves event-core(moves (weld (flop mos) moves)))
        ::
        ++  crypto-core
          =>  [priv=priv.ames-state ..crypto]
          ~>(%memo./ames/crypto-core (nol:nu:crub:crypto priv))
        ::
        ++  channel-state  [life.ames-state crypto-core bug.ames-state]
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
        +|  %routes
        ::
        ++  is-route-dead
          |=  [peer=ship =peer-state]
          ^-  ?
          ?&  ?=(^ route.peer-state)
              direct.u.route.peer-state  ::  XX what about indirect routes?
              !=(%czar (clan:title peer))
              ::  if we haven't tried to contact the peer, there hasn't been any
              ::  /pump or /fine timers that could have turned the peer to %dead
              ::  and we haven't received any packets from the peer, check if
              ::  the peer is actually dead
              ::
              ?|  ?=(%dead -.qos.peer-state)
                  (gte now (add ~s30 last-contact.qos.peer-state))
          ==  ==
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
          ?.  (is-route-dead peer peer-state)
            peer-state
          ?.  ?=(^ route.peer-state)
            peer-state
          peer-state(direct.u.route %.n, -.qos %dead)
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
            ?~  par  event-core
            =/  peer-core  (abed-peer:pe her u.par)
            =/  bones  ~(tap in ~(key by snd.u.par))
            |-  ^+  event-core
            ?~  bones      abet:peer-core
            =.  peer-core  abet:(call:(abed:mu:peer-core i.bones) %prod ~)
            $(bones t.bones)
          ::
          --
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
                (emit:event-core duct.u.ded %pass wire.u.ded %b rest/date.u.ded)
              =.  flow.dead.ames-state.event-core  [%flow ~]
              (wake-dead-flows:event-core ~)
            ::
            %-  (slog leaf+"ames: switching to dead flow consolidation" ~)
            =;  cor=event-core
              set-dead-flow-timer:cor
            %-  ~(rep by peers.ames-state)
            |=  [[=ship =ship-state] core=_event-core]
            ^+  core
            ?.  ?=(%known -.ship-state)
              core
            =*  peer-state  +.ship-state
            %-  ~(rep by snd.peer-state)
            |=  [[=bone pump=message-pump-state] cor=_core]
            ^+  cor
            =/  next-wake  next-wake.packet-pump-state.pump
            ?.  ?&  =(~m2 rto.metrics.packet-pump-state.pump)
                    ?=(^ next-wake)
                ==
              cor
            =+  pe-core=(abed-peer:pe:cor ship peer-state)
            =+  mu-pump=(abed:mu:pe-core bone)
            abet:(pu-emit:packet-pump:mu-pump %b %rest u.next-wake)
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
              %+  skim  tim
              |=([@da hen=^duct] ?=([[%ames ?(%pump %recork) *] *] hen))
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
          ~/  %on-hear-packet
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
          ~/  %on-hear-forward
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
          %-  send-blob
          [for=& rcvr.shot blob (~(get by peers.ames-state) rcvr.shot)]
        ::  +on-hear-keys: handle receipt of attestion request
        ::
        ++  on-hear-keys
          ~/  %on-hear-keys
          |=  [=lane =shot dud=(unit goof)]
          =+  %^  ev-trace  msg.veb  sndr.shot
              |.("requested attestation")
          ?.  =(%pawn (clan:title our))
            event-core
          =/  =blob  (attestation-packet sndr.shot 1)
          %-  send-blob
          [for=| sndr.shot blob (~(get by peers.ames-state) sndr.shot)]
        ::  +on-hear-open: handle receipt of plaintext comet self-attestation
        ::
        ++  on-hear-open
          ~/  %on-hear-open
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
            =+  sy-core=~(. sy:(mesa now^eny^rof) duct)
            =^  moves  ames-state
              sy-abet:(sy-publ:sy-core / [%full (my [sndr.shot point]~)])
            (emil moves)
          ::  manually add the lane to the peer state
          ::
          =/  =peer-state  (gut-peer-state sndr.shot)
          =.  route.peer-state  `[direct=%.n lane]
          =.  peers.ames-state
            (~(put by peers.ames-state) sndr.shot %known peer-state)
          ::
          ::  XX remove; sy-publ already emits the %nail
          =.  event-core
            %-  emit
            :*  unix-duct  %give  %nail  sndr.shot
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
            :*  unix-duct  %give  %nail  sndr.shot
                (get-forward-lanes our peer-state peers.ames-state)
            ==
          ::  perform peer-specific handling of packet
          ::
          =<  abet
          %.  [lane u.shut-packet dud]
          ~(on-hear-shut-packet pe peer-state channel)
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
          |=  [=wire id=* payload=*]
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
          abet:(on-memo:peer-core u.bone %boon payload)
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
          ?:  (~(has in closing.peer-state.peer-core) bone)
            %-  %^  ev-trace  odd.veb  ship
                |.("flow in closing bone={<bone>}; skip %cork")
            event-core
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
          ?:  &
            %-  (slog 'ames: %kroc task not allowed; TBD in |mesa' ~)
            event-core
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
            %ahoy  (migrate-peer bone.deep)  :: XX remove bone; it's next-bone
          ==
          ::
          ++  migrate-peer
            |=  =bone
            ::  XX  defer migrating the peer until we can read from their
            ::  namespace that they have migrated us?
            ::  XX  requires a namespace for migrated peers
            ::
            :: %-  %^  ev-trace  sun.veb  ship.deep
            ::     |.("migrating to |mesa")
            ~&  >>  "migrating to |mesa"
            =~  ::  ack ahoy plea, if we don't crash
                ::
                abet:(call:(abed:mi:peer-core bone) %done ok=%.y)
                ::  migrate all flows
                ::
                on-migrate
                :: after migration succeeds, delete ship from .peers
                ::
                pe-abel
            ==
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
          ::  sink %ack, pass %deep %kill <-  after +on-take-done, ack %cork and
          ::  task to self, and delete the    delete the flow in +handle-cork
          ::  flow (+kill-bone)
          ::
          ::
          ++  cork-bone
            |=  =bone
            =~  abet:(on-cork-flow:peer-core bone)
                (emit duct %give %done ~)
            ==
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
          %^  poke-ping-app  unix-duct  our
          ?.  ?=(%fail -.stun)  -.stun
          [%kick fail=%.y]
        :: +set-dead-flow-timer: set dead flow timer and corresponding state
        ::
        ++  set-dead-flow-timer
          ^+  event-core
          =.  flow.dead.ames-state.event-core
            flow/`[~[/ames] /dead-flow `@da`(add now ~m2)]
          (emit ~[/ames] %pass /dead-flow %b %wait `@da`(add now ~m2))
        ::
        ++  set-dead-routes-timer
          ^+  event-core
          =.  rots.dead.ames-state.event-core
            rots/`[~[/ames] /routes `@da`(add now ~m2)]
          (emit ~[/ames] %pass /routes %b %wait `@da`(add now ~m2))
        :: +wake-dead-flows: call on-wake on all dead flows, discarding any
        ::                   ames-state changes
        ::
        ++  wake-dead-flows
          |=  error=(unit tang)
          ^+  event-core
          %-  ~(rep by peers.ames-state)
          |=  [[=ship =ship-state] core=_event-core]
          ^+  core
          ?.  ?=(%known -.ship-state)
            core
          =*  peer-state  +.ship-state
          =+  pe-core=(abed-peer:pe:core ship peer-state)
          =<  abort
          ^+  pe-core
          %-  ~(rep by snd.peer-state)
          |=  [[=bone =message-pump-state] cor=_pe-core]
          ?.  ?&  =(~m2 rto.metrics.packet-pump-state.message-pump-state)
                  ?=(^ next-wake.packet-pump-state.message-pump-state)
              ==
            cor
          (on-wake:cor bone error)
        ::
        ++  expire-dead-routes
          |=  error=(unit tang)
          ^+  event-core
          =^  total-dead  event-core
            %-  ~(rep by peers.ames-state:event-core)
            |=  [[=ship =ship-state] n=@ core=_event-core]
            ?.  ?=(%known -.ship-state)
              [n core]
            =*  peer     +.ship-state
            =/  old-qos  -.qos.peer
            =/  old-rot  route.peer
            =.  peer     (update-peer-route ship peer)
            =/  expired=?
              ?&  !=(old-qos -.qos.peer)    ::  .route and .qos have changed...
                  !=(old-rot route.peer)    ::
                  ?=([~ %.n *] route.peer)  ::  ...to indirect and %dead
                  ?=(%dead -.qos.peer)      ::
              ==
            ::
            ?.  expired
              [n core]
            :-  +(n)
            ::
            %-  (ev-trace:core &(expired kay.veb) ship |.("route has expired"))
            =.  core
              %-  emit:core
              :*  unix-duct.ames-state  %give  %nail  ship
                  (get-forward-lanes our peer peers.ames-state)
              ==
            abet:(abed-peer:pe:core ship peer)
          ::
          =+  ?.  &(!=(0 total-dead) kay.veb)
                ~
              %.  ~
              (slog leaf/"ames: {<`@`total-dead>} routes have expired" ~)
          event-core
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
            =?  event-core  ?=(^ unix-duct)
              (wake-dead-flows error)
            =+  ?.  =(~ unix-duct)  ~
                %.  ~
                (slog leaf+"ames: unix-duct pending; resetting dead-flow" ~)
            set-dead-flow-timer:event-core
          ::
          ?:  ?=([%routes ~] wire)
            set-dead-routes-timer:(expire-dead-routes error)
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
              %+  slog
                leaf/"ames: got timer for strange ship: {<her.u.res>}, ignoring"
              ~
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
          ?:  =(~ unix-duct)
            %-  (slog leaf+"ames: unix-duct pending; resetting recork-timer" ~)
            event-core
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
        ::  +on-trim: handle request to free memory
        ::
        ::    (%ruin comets not seen for six months)
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
            =.  chain.u.ship-state
              (put:on:chain chain.u.ship-state [idx key /]:u.sec)
            =.  peers.ames-state
              (~(put by peers.ames-state) ship u.ship-state)
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
          |=  [ship=(unit ship) dry=?]
          |^  ^+  event-core
          =;  updated-core=_event-core
              ?:(dry event-core updated-core)
          ::
          ?^  ship
            ?~  peer=(~(get by peers.ames-state) u.ship)
              event-core
            (migrate-peer u.ship u.peer event-core)
          %-  ~(rep by peers.ames-state)
          |=  [[=^ship state=ship-state] core=_event-core]
          ?:  ?=(%alien -.state)  core
          (migrate-peer ship state core)
          ::
          ++  migrate-peer
            |=  [=^ship =ship-state core=_event-core]
            ^+  core
            ?>  ?=([%known *] ship-state)
            =+  peer-core=(abed-peer:pe:core ship +.ship-state)
            ?:  dry
              ~?  >>>  !on-migration-test:peer-core
                %local-test-migration-failed
              core
            ~&  >  %local-migration-worked
            pe-abel:on-migrate:peer-core
          ::
          --
        ::
        ++  on-ack-ahoy
          |=  =shot
          ^+  event-core
          ?.  sam.shot
            %-  (ev-trace odd.veb sndr.shot |.("weird no ames"))
            event-core
          =/  =chum-state  (~(got by chums.ames-state) sndr.shot)
          ?>  ?=([%known *] chum-state)
          =/  =channel    [[our sndr.shot] now channel-state +<.chum-state]
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
          =/  shut-packet=(unit shut-packet)
            (sift-shut-packet shot [symmetric-key her-life our-life]:channel)
          ?~  shut-packet
            %-  (ev-trace odd.veb sndr.shot |.("weird shut-packet"))
            event-core
          =/  =bone         bone.u.shut-packet
          =/  =message-num  message-num.u.shut-packet
          ?.  ?=(%& -.meat.u.shut-packet)
            %-  (ev-trace odd.veb sndr.shot |.("ignoring ack"))
            ::  ignore acks
            ::
            event-core
          =/  [num-fragments=@ud =fragment-num =fragment]  +.meat.u.shut-packet
          ?.  &(=(num-fragments 1) =(fragment-num 0))
            %-  (ev-trace odd.veb sndr.shot |.("ignore multi-fragment pleas"))
            ::  ignore multi-fragment pleas
            ::
            event-core
          =/  blob=*  (cue (rep packet-size [fragment]~))
          ?.  ?=(^ ;;((soft [%$ [%mesa ~] %ahoy ~]) blob))
            %-  (ev-trace odd.veb sndr.shot |.("ignore non ahoy pleas"))
            ::  ignore single-fragment non %ahoy pleas
            ::
            event-core
          ::  single-fragment %ahoy plea for migrated peer; always ack
          ::
          ::  check that chums has in fact the flow in chums for the
          ::  corresponding bone in the shut-packet
          ::
          =+  ev-core=(ev-foco:ev:(mesa now eny rof) sndr.shot +.chum-state)
          =+  fo-core=(fo-abed:fo:ev-core ~[//scry] side=[(mix 1 bone) %bak])
          ?~  res=(fo-peek:fo-core %ack message-num)
            %-  (ev-trace odd.veb sndr.shot |.("ack missing"))
            event-core
          ?.  ?=([%ack error=@] u.res)
            %-  (ev-trace odd.veb sndr.shot |.("weird ack"))
            event-core
          %-  (ev-trace snd.veb sndr.shot |.("send migrated ack"))
          ::
          =/  ok=?  ;;(? +.u.res)
          =/  ack-packet=^shut-packet
            :-  (mix 1 bone.u.shut-packet)
            [message-num.u.shut-packet %| %| ok lag=*@dr]
          %:  send-blob  for=|  sndr.shot
            %-  etch-shot
            %:  etch-shut-packet:ames
              ack-packet
              symmetric-key.channel
              our               sndr.shot
              our-life.channel  her-life.channel
            ==
          ::
            ship-state=~  :: send-blob finds the migrated peer in chums
          ==
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
              =/  chum-state=(unit chum-state)
                (~(get by chums.ames-state) ship)
              ?.  ?|  ?=([~ %known *] chum-state)
                      ?=([~ %known *] ship-state)
                  ==
                ?:  ?=(%pawn (clan:title ship))
                  (try-next-sponsor (^sein:title ship))
                ::  by default, %aliens are saved in peer.ames-state
                ::  XX use chums.ames-state as default
                ::
                %^  enqueue-alien-todo  ship  ship-state
                |=  todos=alien-agenda
                todos(packets (~(put in packets.todos) blob))
              ::
              =/  [sponsor=@p route=(unit [direct=? =lane])]
                ?:  ?=([~ %known *] ship-state)
                  [sponsor route]:u.ship-state
                ?>  ?=([~ %known *] chum-state)
                :-  sponsor.u.chum-state
                ::  XX refactor to arm (see sy-rege:sy:mesa)
                ::
                ?~  lane.u.chum-state  ~
                :-  ~
                ?@  u.lane.u.chum-state
                  [direct=%.y %.y `@p`u.lane.u.chum-state]
                :+  direct=%.n   %.n
                %+  can  3
                :~  4^p.u.lane.u.chum-state
                    2^q.u.lane.u.chum-state
                ==
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
                (try-next-sponsor sponsor)
              ::
              ?:  =(our ship)
                ::  if forwarding, don't send to sponsor to avoid loops
                ::
                ?:  for
                  event-core
                (try-next-sponsor sponsor)
              ::
              ?~  route
                %-  (ev-trace rot.veb final-ship |.("no route to:  {<ship>}"))
                (try-next-sponsor sponsor)
              ::
              %-  (ev-trace rot.veb final-ship |.("trying route: {<ship>}"))
              =.  event-core
                (emit unix-duct %give %send lane.u.route blob)
              ::
              ?:  direct.u.route
                event-core
              (try-next-sponsor sponsor)
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
          :_  crypto-core
          :*  ^=  public-key  pub:ex:crypto-core
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
                channel   [[our ship] now channel-state -.peer]
            ==
          ::
          ++  abort  event-core  :: keeps moves, discards state changes
          ++  abet
            ^+  event-core
            =.  peers.ames-state
              (~(put by peers.ames-state) her %known peer-state)
            event-core
          ::
          ++  pe-abel ::  XX ++abel?
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
            ?:  (~(has in corked.peer-state) bone)
              peer-core
            =/  pump=message-pump-state  (~(got by snd.peer-state) bone)
            ?:  (gth ~(wyt in unsent-messages.pump) msg.cong.ames-state)
              (pe-emit [/ames]~ %pass /clog %g %clog id)
            peer-core
          ::  +on-memo: handle request to send message
          ::
          ++  on-memo
            |=  [=bone =message]
            ^+  peer-core
            =+  log="ames: {<her>} ignoring {<-.message>} on "
            ?:  ?&  (~(has in closing.peer-state) bone)
                    !=(message [%plea %$ /flow %cork ~])
                ==
              ~>  %slog.0^leaf/(weld log "closing bone {<bone>}")
              peer-core
            ?:  (~(has in corked.peer-state) bone)
              ~>  %slog.0^leaf/(weld log "corked bone {<bone>}")
              peer-core
            ::
            abet:(call:(abed:mu bone) %memo message)
          ::  +on-wake: handle timer expiration
          ::
          ++  on-wake
            |=  [=bone error=(unit tang)]
            ^+  peer-core
            =?  peer-core  ?=(^ error)
              (pe-emit duct %pass /wake-fail %d %flog %crud %ames-wake u.error)
            ::  if we are still waiting for the %born task, reset timer
            ::
            ::  if we previously errored out, print and reset timer
            ::
            ::    This really shouldn't happen, but if it does, make sure we
            ::    don't brick either this messaging flow or Behn.
            ::
            ?:  |(?=(^ error) =(~ unix-duct))
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
            ::  expire direct route if the peer is not responding;
            ::  update and print connection state
            ::  (routes/qos for galaxies will remain direct/%live)
            ::
            =/  old-route   route.peer-state
            =.  peer-state  (update-peer-route her peer-state)
            =.  peer-core   (update-qos %ames qos.peer-state)
            =?  peer-core   !=(old-route route.peer-state)
              %-  pe-emit
              :*  unix-duct  %give  %nail  her
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
              %-  (fi-trace:fi fin.veb |.("dead-response {(spud path)}"))
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
            |=  b=bone
            ^+  peer-core
            ?:  (~(has in corked.peer-state) b)
              ~>  %slog.0^leaf/"ames: ignoring kill on corked bone {<b>}"
              peer-core
            =.  peer-state
              =,  peer-state
              %_  peer-state
                ::  if the publisher was behind, preemptively remove any nacks
                ::
                rcv              (~(del by (~(del by rcv) b)) (mix 0b10 b))
                snd              (~(del by snd) b)
                corked           (~(put in corked) b)
                closing          (~(del in closing) b)
                by-duct.ossuary  (~(del by by-duct.ossuary) (got-duct b))
                by-bone.ossuary  (~(del by by-bone.ossuary) b)
              ==
            ::  since we got one cork ack, try the next one
            ::
            recork-one
          ::
          +|  %migration
          ::
          ++  on-migrate
            ^+  peer-core
            =|  fren=fren-state
            |^  =:         -.fren  azimuth-state=-.peer-state
                        lane.fren  get-lane
                         qos.fren  qos.peer-state
                      corked.fren  divide-bones
                     ossuary.fren  align-bones
                client-chain.fren  chain.peer-state
              ==
            =^  poke-moves  flows.fren  (make-flows fren)
            =^  peek-moves  ames-state  (make-peeks fren)
            ::  XX  needed?  peek/poke-moves will have %send moves already
            ::
            ~&  >  %migration-done^her
            ::  enqueue a %prod to start sending unsent messages, after
            ::  all the %mokes (which trigger +peeks for %acks) have been
            ::  processed
            ::
            =/  prod-move=(list move)  [[/ames]~ %pass /mate %a %prod ~[her]]~
            ::  .her is fully migrated, +pe-abel will delete it from peers.
            ::
            peer-core(event-core (emil (weld poke-moves peek-moves)))
            ::
            ++  align-bones
              ^+  ossuary.peer-state
              ::  XX update ossuary in terms of [bone=@ud dire=?(%for %bak)]
              ::  XX assumes every bone  in the ossuary is forward
              ::
              ossuary.peer-state
            ::
            ++  divide-bones
              ^-  (set side)
              %-  ~(rep in corked.peer-state)
              |=  [=bone corked=_corked.fren]
              ?:  =(%3 (mod bone 4))
                ::  XX shouldn't happen
                ~&  >>>  "wrong state of the corked set"
                !!
              %+  ~(put in corked)
                ?.(=(%1 (mod bone 4)) bone (mix 0b1 bone))
              ?:(=(%0 (mod bone 4)) %for %bak)
            ::
            ++  make-flows
              |=  fren=fren-state
              ^-  (quip move (map side flow-state))
              ::  forward flows
              ::
              =^  moves  flows.fren
                %-  ~(rep by snd.peer-state)
                |=  $:  [=bone pump=message-pump-state]
                        moves=(list move)
                        flows=_flows.fren
                    ==
                =|  flow=flow-state
                =/  =dire
                  ?:  =(%0 (mod bone 4))  %for  :: send %plea; sink %boon
                  %bak  :: (1) sink %plea; (0) send %boon; (3)send/(2)sink %nax
                ::
                :: ?:  =(%3 (mod bone 4))
                ::   naxplanation bones are not migrated, but we account for
                ::   them in nax.rcv.flow-state, porting the last acked message
                ::   XX entries in nax.peer-state have not been used
                ::
                ::   =.  bone  (mix 0b1 (mix 0b10 bone))  ::  reference flow
                ::   =/  live=?  !=(current.pump next.pump)
                ::   =.  nax.rcv.flow
                ::     (~(put by nax.rcv.flow) current.pump *error)
                ::   ::
                ::   :-  moves
                ::   (~(put by flows) bone^dire flow)
                ?:  =(%2 (mod bone 4))
                  ::  XX this shouldn't exist
                  ~&  >>>  weird-naxp-ack-bone/bone=bone
                  moves^flows
                =/  naxp-bone=?  =(%3 (mod bone 4))
                =/  original-bone  bone
                =?  bone  =(%1 (mod bone 4))
                  (mix 0b1 bone)              ::  from %1 to [%0 dire=%bak]
                =?  bone  =(%3 (mod bone 4))
                  (mix 0b1 (mix 0b10 bone))   ::  from %3 to [%0 dire=%bak]
                ::  initialize fo-core
                ::
                =/  fo-core
                  =/  =^duct
                    (~(gut by by-bone.ossuary.peer-state) bone [/ames]~)
                  =+  mesa-core=(mesa now eny rof)
                  :: XX check that we don't add a naxplanation .bone here?
                  ::
                  =.  flows.fren  (~(put by flows.fren) bone^dire flow)
                  %.  [duct bone dire]
                  fo-abed:fo:~(ev-core ev:mesa-core [duct her^fren])
                ::
                =?  moves  !=(current.pump next.pump)
                  ::  we are waiting for an %ack, or have heard a %nack and
                  ::  so we defer processing it until we receive the
                  ::  naxplanation
                  ::
                  =*  live  live.packet-pump-state.pump
                  =/  current-live=?
                    %-  ~(rep by live)
                    |=  [[live-packet-key *] has=_`?`%.n]
                    |(has =(message-num current.pump))
                  ?:  current-live
                    ::  the packet pump has live fragments for current so
                    ::  we haven't receive either the %ack or %nack, and are
                    ::  still sending the message.
                    ::
                    moves
                  ::
                  ?:  naxp-bone
                    ::  sanity check that this is not a naxplanation bone
                    ::
                    ~&  >>>  weird-naxp-flow-got-nacked/bone=bone
                    moves
                  ::  if the packet-pump has no state about current.pump,
                  ::  it means that we have heard the %nack, and clear
                  ::  everything, but defered incrementing current until the
                  ::  naxplanation arrives.
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
                =/  live=(list [message-num message])
                  =+  queue=((on ,@ud message-blob) lte)
                  ::  every fragment contains the actual message as a blob
                  ::  we loop over every unsent and live fragments and save
                  ::  the blobs for each message
                  ::
                  =|  blobs=((mop ,@ud message-blob) lte)
                  =.  blobs
                    %+  roll  unsent-fragments.pump
                    |=  [static-fragment acc=_blobs]
                    (put:queue acc [message-num `@`fragment])
                  ::
                  =.  blobs
                    %+  roll
                      (tap:packet-queue:$:pu:mu live.packet-pump-state.pump)
                    |=  [[live-packet-key live-packet-val] acc=_blobs]
                    (put:queue acc [message-num `@`fragment])
                  %-  flop
                  %+  roll  (tap:queue blobs)
                  |=  $:  [=message-num =message-blob]
                          blobs=(list [message-num message])
                      ==
                  :_  blobs
                  :-  message-num
                  ;;  message  :_  (cue message-blob)
                  ?:  =(%0 (mod original-bone 4))  %plea
                  ?:  =(%1 (mod original-bone 4))  %boon
                  ?>  =(%3 (mod original-bone 4))  %naxplanation
                ::
                =^  forward-moves  flow
                  =;  core=_fo-core
                    [moves state]:core
                  =/  unsent=(list [message-num message])
                    %-  flop
                    =<  msgs
                    %-  ~(rep by unsent-messages.pump)
                    |=  [=message num=_next.pump msgs=(list [@ud message])]
                    :-  +(num)
                    [num^message msgs]
                  %+  roll  (weld live unsent)
                  ::
                  |=  [[=message-num =message] core=_fo-core]
                  ?.  ?=(%naxplanation -.message)
                    =?  core  ?=([%plea %$ [%flow ~] %cork ~] message)
                      ::  if we are sending a %cork, we don't know if the other
                      ::  side has corked the flow after receiving it, and the
                      ::  %ack got lost, so we could still be trying to send the
                      ::  %plea and it'll be dropped since the flow is corked.
                      ::
                      ::  At the same time that we are sending the %cork, we
                      ::  +peek for the %cork on the %bak side, and as soon as
                      ::  we can read it, we %cork the flow.
                      ::
                      fo-peek-cork:core
                    %.  [%pump message]
                    fo-call:core(next.snd.state message-num)
                  ::  if we are still sending a %naxplanation, we need to
                  ::  put it in our namespace so the other ship reads it
                  ::
                  %_    core
                      nax.rcv.state
                    %-  ~(put by nax.rcv.state.core)
                    [message-num error]:message
                  ==
                ::  all live messages processed; set next seq payload
                ::
                =.  next.snd.flow  next.pump
                ::  any pending %cork should be already in the load queue
                ::
                =.  closing.flow  (~(has in closing.peer-state) bone)
                ::
                ::  XX  do we care about this?
                ::
                ::  if this was a naxplanation flow (bone=%3) we migrate the
                ::  last acked naxplanation message into nax.rcv.state
                ::
                :: =?  flow  naxp-bone
                ::   %_  flow
                ::     nax.rcv  (~(put by nax.rcv.state.core) current.pump *error)
                ::   ==
                ::
                :_  (~(put by flows) [bone dire] flow)
                =.  moves  (weld forward-moves moves)
                =?  moves  ?=(^ next-wake.packet-pump-state.pump)
                  =*  wake  u.next-wake.packet-pump-state.pump
                  :_  moves  ^-  move
                  :-  [/ames]~
                  [%pass (make-pump-timer-wire her original-bone) %b %rest wake]
                moves
              ::  backward flows
              ::
              =.  flows.fren
                %-  ~(rep by rcv.peer-state)
                |=  [[=bone sink=message-sink-state] flows=_flows.fren]
                ::  if this was a naxplanation bone but we haven't finished
                ::  sink it, also drop it. the message pump has enough
                ::  information to know that we need to start +peeking it.
                ::
                ?:  =(%2 (mod bone 4))
                  ::  %naxplanation %ack on receiver; skip bone
                  flows
                =/  =dire
                  ?:  =(%0 (mod bone 4))  %for  ::  receiving %boon(s)
                  ?>  =(%1 (mod bone 4))  %bak  ::  receiving %plea(s)
                =?  bone  =(%1 (mod bone 4))
                  ::  in the new protocol we use %for/%bak to distinguish
                  ::  between sending/receiving pleas, and collapse the
                  ::  two pieces of state (snd and rcv) into the flow
                  ::  state that has both %outgoing (e.g. send %watch %plea)
                  ::  and %incoming (e.g. receive a %boon %fact) sections
                  ::
                  (mix 0b1 bone)
                =/  flow=flow-state
                  ::  this flow could be part of a subscription flow (both
                  ::  outgoing and incoming payloads) so we need to retrieve
                  ::  or produce the bunt if we were only receiving
                  ::
                  (~(gut by flows) bone^dire *flow-state)
                =:         closing.flow  (~(has in closing.peer-state) bone)
                              line.flow  last-acked.sink
                    last-acked.rcv.flow  last-acked.sink
                    ::  XX if there's a pending-vane ack it should have
                    ::  been sent to the vane already?
                    ::  drop any pending ack
                    ::
                    pending-ack.rcv.flow  %.n  ::  ?=(^ pending-vane-ack.sink)
                  ::
                      nax.rcv.flow
                    ::  carry over live naxplanations we have just migrated
                    ::
                    %-  ~(gas by *_nax.rcv.flow)
                    ::  if there are entries in nax.sink, we have nacked a
                    ::  plea/boon, but we were waiting on the naxplanation
                    ::  to be acked.
                    ::
                    ::  naxplanations are not sent anymore, just exposed
                    ::  in the namespace.
                    ::
                    ::  XX
                    ::  when a message in nacked (e.g. 25), we add it to
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
                    %+  turn  ~(tap in nax.sink)
                    |=  =message-num
                    :-  message-num
                    ?~  migrated-nax=(~(get by nax.rcv.flow) message-num)
                      *error
                    ::  if this is a live naxplation, keep the stack trace
                    ::
                    u.migrated-nax
                  ==
                (~(put by flows) bone^dire flow)
              ::
              moves^flows.fren
            ::
            ++  make-peeks
              |=  fren=fren-state
              ^-  (quip move axle)
              =/  co-core
                =/  chums  (~(put by chums.ames-state) her known/fren)
                %*  co-core  co:(mesa now eny rof)
                  chums.ames-state  chums
                ==
              =*  per  peer-state
              =<  co-abet
              ^+  co-core
              %-  ~(rep by keens.per)
              |=  [[=path keen=keen-state] core=_co-core]
              =|  req=request-state
              =>  .(path `(pole knot)`path)
              ~|  make-peeks-crashed/path
              ?.  ?=([van=@ car=@ cas=@ desk=@ pat=*] path)
                :: XX validate van, car, cas, desk ?
                ::
                ~&  skip-weird-path/path  core
              =;  [pax=^path =space]
                %-  ~(rep in listeners.keen)
                |=  [=^duct core=_core]
                (co-make-peek:core(hen duct) space her pax)
              ::  XX unitize this and no-op if failure to convert
              ::
              ?+    pat.path  [path [%publ life.per]]
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
              =*  lane  lane.u.route.peer-state
              :-  ~
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
          ++  on-migration-test
            ^-  ?
            =/  ahoy-state=axle  ~|(%migrate-crashed ames-state:on-migrate)
            =/  rege-state=axle
              =<  ames-state
              ~|  %regress-crashed
              %.  [`her dry=%.n]
              %*  sy-rege  sy:(mesa now eny rof)
                ames-state  ahoy-state
              ==
            ::  compare pre/post migrated states
            ::
            %+  migration-test
              (~(got by peers.ames-state) her)
            (~(got by peers.rege-state) her)
          ::
          +|  %implementation
          ::  +send-shut-packet: fire encrypted packet at rcvr (maybe sponsors)
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
            (on-memo i.boz %plea %$ /flow [%cork ~])
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
              (emit [/ames]~ %pass (make-pump-timer-wire ship bone) b+rest/wake)
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
              |=  =^bone
              %_  pump
                bone   bone
                state  (~(gut by snd.peer-state) bone *message-pump-state)
              ==
            ::
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
              |=  m=message
              pump(unsent-messages.state (~(put to unsent-messages.state) m))
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
            ::    This prevents us from having to wait for a message nack packet
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
                "duplicate done {<current=current.state num=message-num>}"
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
              ::  .current is complete; pop, emit local ack, and try next one
              ::
              =.  queued-message-acks.state
                (~(del by queued-message-acks.state) current.state)
              ::  clear all packets from this message from the packet pump
              ::
              ::    Note we did this when the original packet came in.
              ::    It's not clear why, but it doesn't always clear the
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
                =^  unsent  pump
                  =<  abut
                  %-  feed:packet-pump
                  [unsent-fragments.state num-slots:gauge:packet-pump]
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
              =.  unsent-fragments.state
                (split-message next.state (jim +.message))
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
                %^  pe-emit  duct  %pass
                [/clear-nack %a %deep %drop her (mix 0b10 bone) message-num]
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
                |=  [fragments=(list static-fragment) num-slots=@ud]
                ^+  pack
                ::  bite off as many fragments as we can send
                ::
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
                =.  pack  (feed [(to-static-fragment hed)]~ sot)
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
                      %+  send-shut-packet  bone
                      [message-num %& +]:u.static-fragment
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
              ::    metrics, possibly re-sending skipped packets.
              ::    Otherwise, no-op.
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
                ::  ack was on later packet; mark skipped, tell gauge, continue
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
                ::  because /app/ping has a special cased maximum backoff of
                ::  ~s25 and we don't want to consolidate that
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
              |=  =^bone
              %_  sink
                bone   bone
                state  (~(gut by rcv.peer-state) bone *message-sink-state)
              ==
            ::
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
                =/  [num-fragments=@ud =fragment-num =fragment]
                  +.meat.shut-packet.task
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
              ::    Only true if we've heard some packets we haven't acked,
              ::    which doesn't happen for boons.
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
              ::  last-heard<seq<10+last-heard; packet in a live message
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
              ::  ack any packet other than the last one, and continue
              ::
              =?  peer-core  !is-last-fragment
                %-  %+  pe-trace  rcv.veb  |.
                    =/  data
                      [seq=seq fragment-num=fragment-num frags=num-fragments]
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
              ::  we have whole message; update state, assemble, send to vane
              ::
              =.  last-heard.state     +(last-heard.state)
              =.  live-messages.state  (~(del by live-messages.state) seq)
              ::
              %-  %+  pe-trace  msg.veb
                  |.("hear {<her>} {<seq=seq>} {<num-fragments.u.live>}kb")
              =/  message=*
                (assemble-fragments [num-fragments fragments]:u.live)
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
              ::  u.next has not been sent to the vane so we assume ok=%.y;
              ::  +done will be called again in the case of error
              ::
              (handle-sink message-num.u.next message.u.next ok=%.y)
            ::
            +|  %implementation
            ::  +handle-sink: dispatch message
            ::
            ++  handle-sink
              |=  [=message-num message=* ok=?]
              |^  ^+  sink
              ?-((received bone) %plea ha-plea, %boon ha-boon, %nack ha-nack)
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
                =+  ;;(=plea message)
                ?:  ?&  =(%$ vane.plea)
                        ?=(%ahoy -.payload.plea)
                        ?=([%test %mesa ~] path.plea)
                    ==
                    ::  check that we can migrate this peer, without
                    ::  modifying the state
                    ::
                    ?>  on-migration-test
                    ::
                    :: %-  %^  ev-trace  sun.veb  her
                    ::     |.("migrating {<her>} test succeded")
                    ~&  >  "migrating {<her>} test succeded"
                    ::
                    (done ok=%.y)
                =.  peer-core
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
                  ?+    -.payload.plea  ~|(weird-migration-plea/plea !!)
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
                ?:  |(closing corked)
                  %-  %+  pe-trace  odd.veb  |.
                      =/  dat  [her bone=bone message-num=message-num]
                      "skip sink boon {<dat>}, flow in closing"
                sink
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
                  %^  pe-emit  duct  %pass
                  [wire %a %deep %sink her target ;;(naxplanation message)]
                ::  ack nack-trace message (only if we don't later crash)
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
                  ++  sign  sigh:as:crypto-core
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
                :: XX tack.keens
                peer-core(keens.peer-state (~(put by keens) path keen))
              ::
              =?  fine  ?=(^ next-wake.keen)
                (fi-rest u.next-wake.keen)
              :: XX tack.keens
              peer-core(keens.peer-state (~(del by keens) path))
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
              (fi-emit unix-duct %pass wire note)
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
              ::
              =.  next-wake.keen  ~
              ::  has the direct route expired?
              ::
              =/  old-route     route.peer-state
              ::  routes/qos for galaxies will remain direct/%live
              ::
              =.  peer-state    (update-peer-route her peer-state)
              =.  peer-core     (update-qos %fine qos.peer-state)
              =.  metrics.keen  %*(on-timeout fi-gauge vane %fine)
              ::
              ?:  =(~ unix-duct)
                ::  no-op; fi-abet will reset the timer
                ::
                fine
              =?  peer-core   !=(old-route route.peer-state)
                %-  pe-emit
                :*  unix-duct  %give  %nail  her
                    (get-forward-lanes our peer-state peers.ames-state)
                ==
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
      ::  |scry: dereference namespace
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
          ::  TODO
          ::  separate endpoint for the full message (instead of packet list)
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
              %+  etch-hunk  our
              [life.ames-state (nol:nu:crub:crypto priv.ames-state)]
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
              =/  [known=(list ship-state) alien=(list ship-state)]
                (skid ~(val by peers.ames-state) |=(^ =(%known +<-)))
              :~  peers-known+&+known
                  peers-alien+&+alien
              ==
            ``mass+!>(maz)
          ::
              [%chain %latest ~]
            :+  ~  ~
            :-  %noun  !>
            `[idx=@ key=@ =path]`(need (ram:on:chain server-chain.ames-state))
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
              ::  so long as neither the peer nor the peer's sponsoring galaxy
              ::  is us, and the peer has been reached recently:
              ::
              ::    - no route to the peer/peer has not been contacted recently:
              ::      send to the peer's sponsoring galaxy
              ::    - direct route to the peer: use that
              ::    - indirect route to the peer: send to both that route and
              ::      the peer's sponsoring galaxy
              ::
              :^  ~  ~  %noun
              !>  ^-  (list lane)
              ?:  =(our u.who)
                ~
              =/  sax
                %+  rof  [~ ~]
                [/ames %j `beam`[[our %saxo %da now] /(scot %p u.who)]]
              =/  gal=(unit @p)
                ?.  ?=([~ ~ *] sax)
                  ~
                `(rear ;;((list ship) q.q.u.u.sax))
              ?.  ?=([~ %known *] peer)
                ?~  gal
                  ~
                ::  if the peer is %alien or missing, send to the sponsor galaxy
                ::
                ?:(=(our u.gal) ~ [%& u.gal]~)
              =/  ev-core  (ev [now eny rof] [//scry]~ ames-state)
              (get-forward-lanes our +.u.peer peers.ames-state)
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
              [%corked her=@ req=*]
            =/  who  (slaw %p her.tyl)
            ?~  who  [~ ~]
            =/  per  (~(get by peers.ames-state) u.who)
            ?.  ?=([~ %known *] per)  [~ ~]
            ?+  req.tyl  [~ ~]
                ~
              ``noun+!>(corked.u.per)
            ::
                [bone=@ ~]
              ?~  bone=(slaw %ud bone.req.tyl)
                [~ ~]
              ``atom+!>((~(has in corked.u.per) u.bone))
            ==
          ::
              [%closing her=@ req=*]
            =/  who  (slaw %p her.tyl)
            ?~  who  [~ ~]
            =/  per  (~(get by peers.ames-state) u.who)
            ?.  ?=([~ %known *] per)  [~ ~]
            ?+  req.tyl  [~ ~]
                ~
              ``noun+!>(closing.u.per)
            ::
                [bone=@ ~]
              ?~  bone=(slaw %ud bone.req.tyl)
                [~ ~]
              ``atom+!>((~(has in closing.u.per) u.bone))
            ==
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
    ::  directed M E S s A ging
    ::
    ++  mesa
      |=  [now=@da eny=@uvJ rof=roof]
      ::
      =<  ::  adult |mesa formal interface, after metamorphosis from larva
          ::
          |%
          ++  call
            |=  [hen=duct dud=(unit goof) wrapped-task=(hobo task)]
            ^-  [(list move) _vane-gate]
            =/  =task  ((harden task) wrapped-task)
            =+  ev-core=(ev-abed:ev hen)
            =+  sy-core=~(sy-core sy hen)
            =+  co-core=(co-abed:co hen)
            ::
            =^  moves  ames-state
              ::  handle error notifications
              ::
              ::   we can crash while handling a packet, in the packet layer.
              ::   the packet could be a one fragment poke, an ack or a
              ::   multiple-fragment poke payload.
              ::   crashes in the packet layer need not to change state or no-op
              ::
              ::   if we crash in the message layer, the message and path have
              ::   been decrypted and validated so we could be crashing while
              ::   handing a %plea or a %boon, or an %ack.
              ::
              ::   a crash while handling a %plea needs to be handled to then
              ::   expose a %naxplanation (blank error for security) in our
              ::   namespace.
              ::
              ::   %boon are always acked, but we should print the error.
              ::
              ::   if we crashed processig an ack, the other end is missbehaving
              ::   sending something that's not a [%message %ack error=?] so we
              ::   print it but do nothing else, since we need to continue
              ::   peeking/retrying sending the poke.
              ::
              ::  XX  what to do with peers that we know are missbehaving?
              ::
              ?^  dud
                ?+    -.task  sy-abet:(~(sy-crud sy hen) -.task tang.u.dud)
                    ?(%mess %heer)
                  ::  these tasks are always called directly in |pe-core
                  ::
                  ~&  >>>  "wrong API call ({<-.task>}); use the |pe-core"
                  `ames-state
                ==
              ?:  ?=(?(%heer %mess) -.task)
                  ::  these tasks are always called directly in |pe-core
                  ::
                ~&  >>>  "wrong API call ({<-.task>}); use the |pe-core"
                `ames-state
              ::
              ?+  -.task
                  ::  ?(%plea %keen %cork) calls are handled directly in |peer
                  ::
                  `ames-state ::  XX TODO: ?(%trim %stir)
              ::
                %vega  `ames-state  ::  handle kernel reload
                %init  sy-abet:sy-init:sy-core
                %born  sy-abet:sy-born:sy-core
                %cong  sy-abet:sy-cong:sy-core
                %plug  sy-abet:(sy-plug:sy-core path.task)
                %prod  (sy-prod:sy-core ships.task)
                %snub  sy-abet:(sy-snub:sy-core [form ships]:task)
                %stun  sy-abet:(sy-stun:sy-core stun.task)
                %dear  sy-abet:(sy-dear:sy-core +.task)
                %tame  sy-abet:(sy-tame:sy-core ship.task)
                %sift  sy-abet:(sy-sift:sy-core ships.task)
                %spew  sy-abet:(sy-spew:sy-core veb.task)
              ::  regression
              ::
                %rege  sy-abet:(sy-rege:sy-core +.task)
              ::  from internal %ames request
              ::
                ?(%meek %moke %mage)  co-abet:(co-call:co-core task)
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
            =+  ev-core=(ev-abed:ev hen)
            =^  moves  ames-state
              ?:  ?=([%gall %unto *] sign)  :: XX from poking %ping app
                `ames-state
              ::
              ?+  sign  `ames-state  ::  ~&(mesa-take-sign/[&1^&2]:sign ev-core)
                [%behn %wake *]  sy-abet:(~(sy-wake sy hen) wire error.sign)
              ::
                  [%jael %private-keys *]
                sy-abet:(~(sy-priv sy hen) [life vein]:sign)
              ::
                  [%jael %public-keys *]
                sy-abet:(~(sy-publ sy hen) wire +>.sign)
              ::
                  [%jael %turf *]
                sy-abet:(~(sy-emit sy hen) unix-duct %give %turf +>.sign)
              ::  vane gifts
              ::
                  ?([%gall %flub ~] [@ %done *] [@ %boon *] [@ %noon *])
                ev-abet:(ev-take:ev-core wire +.sign)
              ::
              ::  remote responses: acks/poke/cork/naxplanation payloads
              ::    reentrant from %ames (from either message or packet layer)
              ::
                [%ames %sage *]
                =<  ev-abet
                =/  response-pith  `(pole iota)`(mesa-pave:ev-core wire)
                %.  [wire +.sign]
                ?+    response-pith   ~|  %mesa-evil-response-wire^wire  !!
                    ?([%keen ~] ev-flow-wire:ev-core)
                  ::ev-take-sage:ev-core
                  ev-take:ev-core
                ==
              ::
              ==
            [moves vane-gate]
          ::
          ++  scry  peek
          ::
          --
      ::
      |%
      +|  %events
      ::
      ++  ev
        ::
        =|  moves=(list move)
        =|  delete-per=?(%.y %.n)
        ::
        |_  [hen=duct her=ship per=fren-state]
        ::
        +|  %helpers
        ::
        ++  ev-core  .
        ++  ev-abet
          :-  moves
          ?:  delete-per  ames-state
          ames-state(chums (~(put by chums.ames-state) her %known per))
        ::
        ++  ev-abed  |=(=duct ev-core(hen duct))
        ++  ev-foco  |=([her=ship per=_per] ev-core(her her, per per))
        ++  ev-emit  |=(=move ev-core(moves [move moves]))
        ++  ev-emil  |=(mos=(list move) ev-core(moves (weld mos moves)))
        ++  ev-tace
          |=  [verb=? print=(trap tape)]
          ^+  same
          (trace %mesa verb her ships.bug.ames-state print)
        ::
        +|  %flow-wires
        ::
        ::  $?  for-acks=%ack  ::  XX revisit names ?
        ::      for-nax-payloads=%nax
        ::      to/from-vane=%van
        ::      for-corks=%cor
        ::      for-poke-payloads=%pok
        ::  ==
        +$  were  ?(%van %nax %ack %cor %pok)
        +$  ev-flow-wire
          $:  %mesa
              %flow
              =were
              =dire
              [%p her=@p]
              [%ud rift=@ud]
              [%ud bone=@ud]
              ~
          ==
        ::
        +$  ev-bone-wire
          $:  %bone
              [%p her=@p]
              [%ud rift=@ud]
              [%ud bone=@ud]
              ~
          ==
        ::
        +|  %validation
        ::
        ++  ev-parse-flow-wire
          |=  =wire
          ^-  (unit ev-flow-wire)
          =>  .(wire `(pole iota)`(mesa-pave wire))
          ?.  ?=(ev-flow-wire wire)  ~
          `wire
        ::
        ++  ev-parse-bone-wire
          |=  =wire
          ^-  (unit ev-bone-wire)
          =>  .(wire `(pole iota)`(mesa-pave wire))
          ?.  ?=(ev-bone-wire wire)  ~
          `wire
        ::
        ++  ev-decrypt-spac
          |=  [=space ser=@ cyf=(unit @)]
          ^+  ser
          ?-  -.space
            ?(%none %publ)  ser
            %shut           (decrypt:crypt key.space (need cyf) ser)
            %chum           (decrypt:crypt key.space (need cyf) ser)
          ==
        ::
        ++  ev-decrypt-path
          |=  [=path =ship]
          ^-  [=space (unit cyf=@) inner=^path]
          =/  tyl=(pole knot)  path
          ?+    tyl  ~|(ev-decrypt-path/tyl !!)
              [%publ lyf=@ pat=*]  :: unencrypted
            [publ/(slav %ud lyf.tyl) ~ pat.tyl]
          ::
              [%chum lyf=@ her=@ hyf=@ pat=[cyf=@ ~]] :: encrypted with eddh key
            =/  lyf  (slaw %ud lyf.tyl)
            =/  her  (slaw %p her.tyl)
            =/  hyf  (slaw %ud hyf.tyl)
            =/  cyf  (slaw %uv cyf.pat.tyl)
            ?>  &(?=(^ lyf) ?=(^ her) ?=(^ hyf) ?=(^ cyf))
            ::  XX check =(ship u.her)
            =/  her=@p  ?:(=(u.her our) ship u.her) :: %poke payloads are for us
            =+  sat=(got-per her)        :: XX get-per
            :: ?>  ?=(%known -.per)  :: XX wat if %alien?
            :: ?.  =(u.hyf life.per)   !!  :: XX handle?
            =*  key  `@uxI`symmetric-key.sat
            :+  [%chum server=life.ames-state client=her life.sat key]
              cyf
            (open-path:crypt key u.cyf)
          ::
              [%shut kid=@ pat=[cyf=@ ~]]  :: encrypted with group key
            =/  kid  (slaw %ud kid.tyl)
            =/  cyf  (slaw %uv cyf.pat.tyl)
            ?>  &(?=(^ kid) ?=(^ cyf))
            =+  sat=(got-per ship)       :: XX get-per
            :: ?>  ?=(%known -.per)  :: XX wat if %alien?
            ?~  key=(get:key-chain client-chain.sat u.kid)
              !!  :: XX handle
            :^    [%shut u.kid -.u.key]
                cyf
              -.u.key
            (open-path:crypt -.u.key u.cyf)
          ==
        ::
        +|  %request-flow
        ::
        ++  ev-req-plea
          |=  [vane=@tas =path payload=*]
          ^+  ev-core
          =/  cork=?  =([%$ /flow %cork ~] vane^path^payload)
          =^  bone  ossuary.per  ::  XX  to arm?
            =,  ossuary.per
            ?:  cork
              ~|  "flow not in the ossuary; ignore cork"
              [(~(got by by-duct) hen) ossuary.per]
            ?^  bone=(~(get by by-duct) hen)
              [u.bone ossuary.per]
            :-  next-bone  ^+  ossuary.per
            :+  (add 4 next-bone)
              (~(put by by-duct) hen next-bone)
            (~(put by by-bone) next-bone hen)
          ::
          =+  fo-core=(fo-abed:fo hen bone dire=%for)
          ::
          %-  %+  ev-tace  msg.veb.bug.ames-state
              =+  msg=?:(cork %cork %plea)
              =*  next  next.snd.fo-core
              |.("send {<msg>} {<[bone=bone seq=next path=(spud path)]>}")
          ::
          ?:  closing.state.fo-core
            %-  %+  ev-tace  odd.veb.bug.ames-state
                |.("flow {<bone=bone>} in closing; skip")
            ev-core
          %-  %+  ev-tace  &(cork sun.veb.bug.ames-state)
              |.("set flow {<bone=bone>} in closing")
          ::
          =<  fo-abet
          %.  [%pump %plea vane path payload]
          fo-call:fo-core(closing.state cork)
        ::
        ++  ev-req-boon
          |=  [=bone id=(unit *) load=*]
          ^+  ev-core
          ::
          =+  fo-core=(fo-abed:fo hen bone dire=%bak)
          %-  %+  ev-tace  msg.veb.bug.ames-state
              =*  next  next.snd.fo-core
              |.("send %boon {<[bone=bone seq=next]>}")
          ::
          =.  ev-core  fo-abet:(fo-call:fo-core %pump %boon load)
          ?~  id
            ev-core
          ?.  %+  gth  (wyt:fo-mop loads.snd.state):fo-core
              msg.cong.ames-state
            ev-core
          %-  %+  ev-tace  sun.veb.bug.ames-state
              |.("clog flow {<bone=bone>}")
          (ev-emit:ev-core [/ames]~ %pass /clog %g clog/u.id)
        ::
        ++  ev-req-peek
          |=  [=space =path]
          ^+  ev-core
          ::  +sy-plug should have already stored [kid key path] in
          ::  .chain.ames-state on the server, and the client would have
          ::  retrieved the key via the %ames key exchange. here we store it
          ::  in their peer state
          ::
          =?  client-chain.per  ?=(%shut -.space)
            (put:key-chain client-chain.per kid.space key.space path)
          ::  XX  early abet; antipattern
          ::
          ::  instead we should just emit the %meek task, instead of punching
          ::  through the message-builder core, so when retrieving the key from
          ::  chums.ames-state it's already been updated
          ::
          =^  moves-a  ames-state  ev-abet
          =^  moves-b  ames-state
            co-abet:(co-make-peek:(co-abed:co hen) space her path)
          ::  update per in the door's sample with the updated value from
          ::  ames-state; removing this will discard the last change when doing
          ::  +ev-abet
          ::
          =.  per  (got-per her)
          (ev-emil (weld moves-a moves-b))
        ::
        +|  %packet-entry-points
        ::
        ++  ev-pact
          |%
          ++  hear-poke
            |=  [dud=(unit goof) =lane:pact =pact:pact]
            ^+  ev-core
            ?>  ?=(%poke +<.pact)
            =*  data     data.pact
            =*  our-ack  her.ack.pact
            =*  rif-ack  rif.ack.pact
            =*  her-pok  her.pok.pact
            =*  rif-pok  rif.pok.pact
            ::  XX dispatch/hairpin &c
            ::
            ::  - pre-check that we want to process this poke
            ::    (recognize ack path, ship not blacklisted, &c)
            ::  - initialize our own outbound request for the poke payload
            ::  - start processing the part of the poke payload we already have
            ::    - validation should crash event/no-op to ensure that no
            ::      state is changed
            ::
            =/  [=space cyf=(unit @) =inner-poke=path]
              ~|  inner-path/[pat.ack^pat.pok]:pact
              (ev-decrypt-path [pat her]:pok.pact)
            ::
            =/  [pok=(pole iota) ack=(pole iota)]
              ::  path validation/decryption
              ::
              :-  (validate-path inner-poke-path)
              (validate-path inner:(ev-decrypt-path [pat.ack her.pok]:pact))
            ::
            ?>  &(?=(flow-pith ack) ?=(flow-pith pok))
            ?.  ?&  =(our our-ack)       ::  do we need to respond to this ack?
                    =(our-rift rif-ack)  ::  at the current rift
                ==
              %-  %+  ev-tace  odd.veb.bug.ames-state
                  =+  rifs=[our=our-rift pac=rif-ack]
                  |.("not our ack rcvr={<our-ack>} rifs={<rifs>}; skip")
              ev-core
            ?.  ?&  =(our rcvr.pok)      ::  are we the receiver of the poke?
                    =(rift.per rif-pok)  ::  at their current rift
                ==
              =+  rifs=[her=rift.per pac=rif-pok]
              %-  %+  ev-tace  odd.veb.bug.ames-state
                  |.("poke for {<rcvr.pok>} at rifts={<rifs>}; skip")
              ev-core
            ?.  =(her-pok rcvr.ack)      ::  do ack and pokes match?
              %-  %+  ev-tace  odd.veb.bug.ames-state
                  |.("ack {<rcvr.ack>} and poke {<her-pok>} missmatch; skip")
              ev-core
            ::
            %-  (ev-tace rcv.veb.bug.ames-state |.("hear poke packet"))
            ::
            =.  per  (ev-update-lane lane hop.pact ~)
            ::  update and print connection status
            ::
            =?  ev-core  ?=(^ lane.per)  (ev-update-qos %live last-contact=now)
            =?  ev-core  ?=(~ lane.per)  (ev-update-qos %dead last-contact=now)
            ::
            ?.  =(1 (div (add tob.data.pact 1.023) 1.024))
              %-  %+  ev-tace  msg.veb.bug.ames-state
                  |.("hear incomplete message")
              :: XX assert load is plea/boon?
              =+  fo-core=(fo-abed:fo hen [bone dire]:ack)
              ?:  (fo-message-is-acked:fo-core mess.pok)
                ::  don't peek if the message havs been already acked
                ::
                %-  %+  ev-tace  odd.veb.bug.ames-state
                    |.("poke [bon, seq]={<[bone mess]:pok>} already acked")
                ::
                fo-abet:(fo-send-ack:fo-core mess.pok)
              ::
              %-  %+  ev-tace  fin.veb.bug.ames-state
                  |.("peek for poke payload {<[flow=bone seq=mess]:pok>}")
              ::
              %^  ev-emit  hen  %pass
              [(fo-wire:fo-core %pok) %a %meek [none/~ [her pat]:pok.pact]]
            ::  authenticate one-fragment message
            ::
            ?>  %-  authenticate
                [(root:lss (met 3 dat.data)^dat.data) aut.data pok.pact]
            ::
            %:  hear-poke:ev-mess
              dud
              [her.ack.pact (pout ack)]
              [her.pok.pact (pout pok)]
              ;;(gage:mess (cue (ev-decrypt-spac space dat.data cyf)))
            ==
          ::
          ++  hear-peek
            |=  =name:pact
            ?.  =(our her.name)
              ev-core
            ::
            %-  %+  ev-tace  [|(rcv fin)]:veb.bug.ames-state
                |.("hear peek packet")
            ::
            =/  res=(unit (unit cage))
              (peek ~ /ames %x (name-to-beam name))
            ?.  ?=([~ ~ ^] res)
              ev-core
            ?.  ?=([%atom *] u.u.res)
              ev-core  ::  XX support both %atom and %packet
            =+  !<([dat=@ pairs=(list @ux) pof=@ux] q.u.u.res)
            (ev-emit hen %give %push ~ dat)
          ::
          ++  hear-page
            |=  [dud=(unit goof) =lane:pact =pact:pact]
            ^+  ev-core
            ?>  ?=(%page +<.pact)
            =*  data     data.pact
            =*  name     name.pact
            ::  check for pending request (peek|poke)
            ::
            =*  sealed-path  pat.name
            =/  [=space cyf=(unit @) =inner=path]
              (ev-decrypt-path pat.name her)
            ?~  res=(~(get by pit.per) sealed-path)
              %.  ev-core
              %+  ev-tace  odd.veb.bug.ames-state
              |.("missing page from pit {(spud inner-path)}")
            ::
            %-  (ev-tace rcv.veb.bug.ames-state |.("hear page packet"))
            ::
            ?.  =(rift.per rif.name)
              %-  %+  ev-tace  odd.veb.bug.ames-state
                  |.("wrong rift {<[rift.per rif.name]>}; skip")
              ev-core
            ::
            =.  per  (ev-update-lane lane hop.pact next.pact)
            ::  update and print connection status
            ::
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
              ?>  %-  authenticate
                  [(recover-root:verifier:lss proof) aut.data name]
              =/  state    (init:verifier:lss tof proof)
              =.  pit.per  (~(put by pit.per) sealed-path u.res(ps `[state ~]))
              ::
              %-  (ev-tace snd.veb.bug.ames-state |.("request frag={<fag>}"))
              ::  request next fragment
              ::
              ?:  =(~ unix-duct)
                %-  (slog leaf+"ames: unix-duct pending; will retry %push" ~)
                ev-core
              %-  ev-emit
              %+  push-pact
                [hop=0 %peek name(wan [%data 0])]
              (make-lanes her lane.per qos.per)
            ::
                %data
              ::  do we have packet state already?
              ::
              ?~  ps.u.res
                ::  is this this a standalone (jumbo or 1-frag) message?
                ::
                =/  mod  (bex (dec boq.name))  :: XX unguarded
                ?:  =(1 (div (add tob.data (dec mod)) mod))
                  ~|  aut.data
                  ?>  ?=(%& -.aut.data)
                  ?>  %-  authenticate
                      [(root:lss tob.data^dat.data) aut.data name]
                  =/  =spar       [her.name inner-path]
                  =/  =auth:mess  p.aut.data
                  =/  res=@       (ev-decrypt-spac space dat.data cyf)
                  ::  if %chum/%shut, we need to pass the sealed-path to find it
                  ::  in the pit.fren-state and then remove it
                  ::
                  %*  $  hear-page:ev-mess
                    sealed-path  `sealed-path
                    +<           spar^auth^res
                  ==
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
              ~|  los.ps^leaf^pair
              =.  los.ps   (verify-msg:verifier:lss los.ps [leaf pair])
              =.  fags.ps  [dat.data fags.ps]
              =.  pit.per  (~(put by pit.per) sealed-path u.res(ps `ps))
              ::  is the message incomplete?
              ::
              ?.  =(+(fag) leaves.los.ps)
                ::  request next fragment
                ::
                ?:  =(~ unix-duct)
                  %-  (slog leaf+"ames: unix-duct pending; will retry %push" ~)
                  ev-core
                ::
                %-  %+  ev-tace  snd.veb.bug.ames-state
                    |.("request frag={<counter.los.ps>}")
                ::
                %-  ev-emit
                %+  push-pact
                  [hop=0 %peek name(wan [%data counter.los.ps])]
                (make-lanes her lane.per qos.per)
              ::  yield complete message
              ::
              %-  (ev-tace rcv.veb.bug.ames-state |.("yield full message"))
              ::
              =/  =spar  [her.name inner-path]
              =/  =auth:mess  [%| *@uxH] :: XX should be stored in ps?
              =/  res=@  (ev-decrypt-spac space (rep 13 (flop fags.ps)) cyf)
              ::  if %chum/%shut, we need to pass the sealed-path to find it
              ::  in the pit.fren-state and then remove it f
              ::
              %*  $  hear-page:ev-mess
                sealed-path  `sealed-path
                +<           [spar auth res]
              ==
            ==
          ::
          --
        ::
        +|  %messages-entry-point
        ::
        ::  XX call +ev-update-qos again in the message layer?
        ::
        ++  ev-mess
          |%
          ++  hear-page
            =|  sealed-path=(unit path)   ::  XX set in the packet layer
            |=  [=spar =auth:mess res=@]  ::  XX assumes res and path decrypted
            ^+  ev-core
            =*  ship  ship.spar
            ?>  =(her ship.spar)
            ::
            =+  path=?~(sealed-path path.spar u.sealed-path)
            ?~  ms=(~(get by pit.per) path)
              %-  %+  ev-tace  odd.veb.bug.ames-state
                  |.("path missing from the .pit")
              ev-core
            ::  XX validate response
            =.  pit.per  (~(del by pit.per) path)
            ~|  gage-res-failed/`@ux`res
            =+  ;;(=gage:mess (cue res))
            ?>  ?=(^ gage)
            (ev-give-response for.u.ms path.spar gage)
          ::
          ++  hear-poke
            |=  [dud=(unit goof) =ack=spar =pok=spar =gage:mess]
            ^+  ev-core
            ::  XX  we punch through the message layer directly from the
            ::  packet layer, so ack/poke path validation happens there
            ::
            =/  pok=(pole iota)  (mesa-pave path.pok-spar)
            =/  ack=(pole iota)  (mesa-pave path.ack-spar)
            ?>  &(?=(flow-pith pok) ?=(flow-pith ack))
            ::
            ::  XX printed in the packet layer
            :: =+  ?~  dud  ~
            ::     %.  ~
            ::     %+  slog  leaf+"mesa: message crashed {<mote.u.dud>}"
            ::     ::  XX what if the crash is due to path validation
            ::     ::  and we can't infer the sequence number?
            ::     ?.  msg.veb.bug.ames-state  ~
            ::     :-  >[bone=bone message-num=mess]:pok<
            ::     tang.u.dud
            ::  XX  the packet layer has validated that this is a %poke for us
            ::
            ::  XX assumes that %aliens are checked in the packet layer
            ::  XX assumes that .per in the sample is set by the packet layer
            ::
            :: XX assert load is plea/boon
            =/  fo-core
              %.  [%sink mess.pok gage ?=(~ dud)]
              fo-call:(fo-abed:fo hen [bone dire]:ack)
            =.  ev-core  fo-abet:fo-core
            ev-core(delete-per delete-per.fo-core)
          ::
          ++  hear-peek
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
          --
        ::
        +|  %take-responses
        ::
        ++  ev-take
          |=  $:  =wire
                  $=  sign
                  $~  flub/~
                  $%([%flub ~] $>(?(%noon %boon %done %sage) gift))
              ==
          ^+  ev-core
          ?^  flow-wire=(ev-parse-flow-wire wire)
            =.  her  her.u.flow-wire
            =.  per  (got-per her)
            ?:  (lth rift.u.flow-wire rift.per)
              %-  %+  ev-tace  odd.veb.bug.ames-state
                    |.("ignore {<(trip -.sign)>} for old rift")
              ev-core
            ?>  ?=(%sage -.sign)
             (ev-take-sage +.sign [were bone dire]:u.flow-wire)
          ?~  bone-wire=(ev-parse-bone-wire wire)
            %-  %+  ev-tace  odd.veb.bug.ames-state
                  |.("weird wire on {<(trip -.sign)>} {(spud wire)}")
            ev-core
          =.  her  her.u.bone-wire
          =.  per  (got-per her)
          ?:  (lth rift.u.bone-wire rift.per)
            %-  %+  ev-tace  odd.veb.bug.ames-state
                  |.("ignore {<(trip -.sign)>} for old rift")
            ev-core
          ::  after %sage, all signs happen on backward flows
          ::
          =?  bone.u.bone-wire  =(%1 (mod bone.u.bone-wire 2))
            (mix 0b1 bone.u.bone-wire)
          =+  fo-core=(fo-abed:fo hen bone.u.bone-wire dire=%bak)
          ?+  -.sign  !!  :: %sage shouldn't use bone wires
            ::  XX for %done, we ack one message at at time, seq is not needed?
            ::  XX use it as an assurance check?
            ::
            ?(%flub %done)  fo-abet:(fo-take:fo-core %van sign)
          ::
              ?(%boon %noon)
            %+  ev-req-boon  bone.u.bone-wire
            ?-(-.sign %boon [id=~ payload.sign], %noon [`id payload]:sign)
          ==
        ::
        ::  +ev-take-sage: receive remote responses
        ::
        ++  ev-take-sage
          |=  [=sage:mess =were =side]
          ^+  ev-core
          ::
          =/  message-path=(pole iota)  (validate-path path.p.sage)
          =+  fo-core=(fo-abed:fo hen side)
          ::
          ?:  =(%cor were)
            ::  validate %cork path and wire
            ::
            ?>  ?&  ?=(cork-pith message-path)
                    ?|  &(=(%for dire.message-path) =(%bak dire.side))
                        &(=(%bak dire.message-path) =(%for dire.side))
                ==  ==
            ::  the server is reading corks on the forward side, the one
            ::  that sent the %cork, on the original flow (coming on a %watch)
            ::

            ?:  (~(has in corked.per) side)
              %-  %+  ev-tace  odd.veb.bug.ames-state
                  |.("unexpected %gone $page; ignore")
              ::  this shouldn't happen since fo-abel should have deleted the
              ::  entry in the .pit when hearing the %ack for the %cork $plea
              ::
              ev-core
            %-  %+  ev-tace  msg.veb.bug.ames-state
                |.("cork received; delete {<side>}")
            ::  if we don't crash, the client has removed the flow,
            ::  and we have succesfully +peek'ed the %cork
            ::
            fo-abel:(fo-take-cor:fo-core sage)
          ::
          ::  XX  validate that wire and path match?
          ::
          ?>  ?=(flow-pith message-path)
          ::
          ?:  =(%pok were)
            %-  hear-poke:ev-mess
            :^    dud=~
                ack-path=our^(pout message-path(load %ack, dire dire.side))
              her^(pout message-path)
            q.sage
          ::  wires are tagged ?(%ack %nax) so we can diferentiate if we are
          ::  proessing an ack or a naxplanation payload
          ::
          =.  fo-core
            ::  XX parse $ack payload in here, and call task instead?
            (fo-take:fo-core were sage/[mess.message-path sage])
          ::
          ?.  can-be-corked.fo-core
            fo-abet:fo-core
          ::  we received the %ack for the %cork %plea;
          ::  remove the flow and it's associated bone in the ossuary;
          ::  expose %cork flow in the namespace "~(put in corked)"
          ::
          %-  %+  ev-tace  msg.veb.bug.ames-state
              |.("hear cork ack; delete {<bone=bone.side>}")
          ::
          fo-abel:fo-core
        ::
        +|  %peek-subscribers
        ::
        ++  ev-give-response
          |=  [listeners=(set duct) =path =gage:mess]
          ^+  ev-core
          %-  ~(rep in listeners)
          |=  [hen=duct c=_ev-core]
          %.  (ev-emit:c hen %give %sage her^path gage)
          %+  ev-tace  fin.veb.bug.ames-state
          |.("give page={(spud path)}")
        ::
        ++  ev-cancel-peek
          |=  [all=? =path]  :: XX namespace?
          ^+  ev-core
          %-  %+  ev-tace  fin.veb.bug.ames-state
              |.("cancel peek path={(spud path)}}")
          ?~  ms=(~(get by pit.per) path)
            ev-core
          ?:  all
            =.  pit.per  (~(del by pit.per) path)
            (ev-give-response for.u.ms path ~)
          =.  for.u.ms  (~(del in for.u.ms) hen)
          =.  pit.per
            ?~  for.u.ms
              (~(del by pit.per) path)
            (~(put by pit.per) path u.ms)
          ev-core
        ::
        +|  %internals
        ::
        ::  XX  refactor; merge with +ev-update-qos in |pe:ames
        ::  +ev-update-qos: update and maybe print connection status
        ::
        ++  ev-update-qos
          |=  new=qos
          ^+  ev-core
          =/  old      qos.per
          =.  qos.per  new
          =/  text
            %^  qos-update-text  her  %ames
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
            %-  %+  ev-tace  rcv.veb.bug.ames-state
                |.("hear direct packet")
            per(lane `lane)
          ?~  next
            %-  %+  ev-tace  rcv.veb.bug.ames-state
                |.("hear indirect packet hop={<hop>}; no next lane")
            per
          =/  lane=tape
            ?@  i.next
              "from {<`@p`i.next>}"
            "lane={(scow %if p.i.next)}:{((d-co:^co 1) q.i.next)}"
          %-  %+  ev-tace  rcv.veb.bug.ames-state
              |.("hear indirect packet hop={<hop>} {lane}")
          per(lane `i.next)
        ::
        ++  ev-got-duct
          |=  =bone
          ^-  duct
          ~|  %dangling-bone^her^bone
          (~(got by by-bone.ossuary.per) bone)
        ::
        +|  %routes
        ::
        ++  ev-is-lane-dead
          |=  [fren=ship =fren-state]
          ^-  ?
          ?&  ?=(^ lane.fren-state)
              !=(%czar (clan:title fren))
              ::  if we haven't tried to contact the fren, there hasn't been any
              ::  /pump or /fine timers that could have turned the fren to %dead
              ::  and we haven't received any packets from the fren, check if
              ::  the peer is actually dead
              ::
              ?|  ?=(%dead -.qos.fren-state)
                  (gte now (add ~s30 last-contact.qos.fren-state))
          ==  ==
        ::
        +|  %flows
        ::
        ++  fo
          ::  flows exist only for known peers
          ::
          =|  can-be-corked=?(%.y %.n)
          =|  delete-per=?(%.y %.n)
          ::
          |_  [[hen=duct =side] state=flow-state]
          +*  bone  bone.side
              dire  dire.side
              snd   snd.state
              rcv   rcv.state
          ::
          +|  %helpers
          ::
          ++  fo-core  .
          ++  fo-abed
            |=  [=duct =^side]
            ::  XX use +got in another arm to assert when the flow should exist
            =.  state  (~(gut by flows.per) side *flow-state)
            fo-core(hen duct, side side)
          ::
          ++  fo-abet
            ^+  ev-core
            ?:  delete-per
              ev-core(delete-per delete-per)
            =?  flows.per  !fo-corked
              (~(put by flows.per) bone^dire state)
            %_    ev-core
                ames-state
              ames-state(chums (~(put by chums.ames-state) her known/per))
            ==
          ::
          ++  fo-abel
            ^+  ev-core
            ::
            =:   flows.per  (~(del by flows.per) bone^dire)
                corked.per  (~(put in corked.per) bone^dire)
            ::
                by-bone.ossuary.per
              ?:  =(%bak dire)  by-bone.ossuary.per
              (~(del by by-bone.ossuary.per) bone)
            ::
                by-duct.ossuary.per
              ?:  =(%bak dire)  by-duct.ossuary.per
              (~(del by by-duct.ossuary.per) (ev-got-duct bone))
            ::
                pit.per
                  =;  [pit=_pit.per *]
                    ::  a forward flow can be deleted when we hear an %ack for a
                    ::  %cork $plea, or a %gone $page for a corked flow +peek.
                    ::
                    ::  for the %ack, there could be a path in the .pit to read
                    ::  if the flow has been corked, so we can just derive it
                    ::  based on the flow-state, and attempt to delete it.
                    ::
                    %-  ~(del by pit)
                    (make-space-path chum-to-our (fo-cor-path seq=0 our))
                  ::
                  %^  (dip:fo-mop _pit.per)  loads.snd
                    pit.per
                  |=  [=_pit.per seq=@ud req=mesa-message]
                  :+  ~  |
                  =/  =path
                    ::  remove any unsent boon or cork $plea ack path in the pit
                    ::
                    (make-space-path chum-to-our (fo-ack-path seq our))
                  =+  ?.  (~(has by pit) path)  ~
                      %.  ~
                      %+  ev-tace  fin.veb.bug.ames-state
                      =+  load=?:(?=(%for dire) "%cork" "%boon")
                      =+  flow-info=[side=side seq=seq]
                      |.("remove {load} {<flow-info>} path={(spud path)}")
                  ::
                  (~(del by pit) path)
              ==
            ::
            =.  ames-state
              ames-state(chums (~(put by chums.ames-state) her known/per))
            ev-core
          ::
          ++  fo-emit  |=(=move fo-core(moves [move moves]))
          ++  fo-emil  |=(mos=(list move) fo-core(moves (weld mos moves)))
          ++  fo-to-close
            |=  poke=mesa-message
            ?&(closing.state !=(poke [%plea %$ /flow %cork ~]))
          ::
          ++  fo-corked     (~(has in corked.per) side)
          ++  fo-flip-dire  ?:(=(dire %for) %bak %for)
          ::  path examples
          ::
          :: where=@p  [in the protocol namespace; redundant with to]
          ::
          :: /flow/[bone]/[payload]/[to]/[seq]  :: %plea
          :: /flow/[bone]/[payload]/[to]/[seq]  :: %boon
          :: /flow/[bone]/[payload]/[to]/seq    :: %ack
          :: /flow/[bone]/[payload]/[to]/[seq]  :: %nax
          :: ::  meta paths
          :: ::
          :: /flow/bone/payload/to      :: %corks (meta)
          ::
          ::  the path refers to where the payload is stored.
          ::
          ::  side=[bone=0 %for] (e.g. sends %plea: %watch, %poke, %leave, %cork)
          ::  Options to consider:
          ::
          ::  [from=~zod] (*) /flow/bone=0/payload=plea         /to=~nec/mess=1
          ::                  /flow/bone=0/payload=poke/dire=for/to=~nec/mess=1
          ::  plea $page: (*) [tag=%plea *page]
          ::                  [tag=%poke *page] ?
          ::  the path tells us that this is plea handling so we assert:
          ::  ?>  ?=(%plea -.sage)
          ::
          ::  subscriptions:
          ::
          ::              (*) /flow/bone=0/payload=ack-boon     /to=~nec/mess=1
          ::                  /flow/bone=0/payload=ack/dire=for /to=~nec/mess=1
          ::
          ::  side=[bone=0 %bak] (e.g. sends %boon: %fact, %kick)
          ::
          ::  Options to consider:
          ::
          ::  [from=~nec] (*) /flow/bone=0/payload=ack-plea     /to=~zod/mess=1
          ::                  /flow/bone=0/payload=klea         /to=~zod/mess=1
          ::                  /flow/bone=0/payload=ack-bak      /to=~zod/mess=1
          ::                  /flow/bone=0/payload=ack/dire=bak /to=~zod/mess=1
          ::  ack $page:  (*) [%ack *page]
          ::                  [?(%ack-plea %ack-boon) *page]
          ::  subscriptions:
          ::
          ::              (*) /flow/bone=0/payload=boon         /to=~zod/mess=1
          ::                  /flow/bone=0/payload=poke/dire=bak/to=~zod/mess=1
          ::
          ::  $page:      (*) [tag=%boon *page]
          ::                  [tag=%poke *page] ?
          ::  the path tells us that this is boon handling so we assert:
          ::  ?>  ?=(%boon -.sage)
          ::
          ::  (*): currently used in ames.hoon, in the |mesa core
          ::
          ::  +fo-infer-dire: infer the side that's producing this payload
          ::  (e.g. when hearing a +peek request for this path, if the load
          ::  is a %plea, is always produced on the %for side)
          ::
          ++  fo-message-is-acked  |=(seq=@ud (lte seq last-acked.rcv))
          ++  fo-message-not-in-range
            |=  seq=@ud
            ^-  ?
            ?&  (gth seq +(last-acked.rcv))           ::  future ack
                ?|  (lte seq last-acked.rcv)
                    (gth (sub last-acked.rcv seq) 10) ::  too far ack
            ==  ==
          ::
          +|  %builders
          ::
          ++  fo-mop       ((on ,@ud mesa-message) lte)
          ++  fo-nax       ((on ,@ud [?(%wait %done) error]) lte)
          ++  fo-cac       ((on ,@ud ?) lte)
          ::  all path builders refers to payloads on the other side from ours
          ::  so the direction is always flipped
          ::
          ++  fo-ack-path  |=([s=@ r=@p] (fo-path s %ack r))
          ++  fo-pok-path  |=([s=@ r=@p] (fo-path s %poke r))
          ++  fo-nax-path  |=([s=@ r=@p] (fo-path s %naxp r))
          ++  fo-cor-path  |=([s=@ r=@p] (fo-path s %cork r))
          ++  fo-path
            |=  [seq=@ud =load server=@p]
            ^-  path
            :*  vane=%a  care=%x  case='1'  desk=%$
              ::
                %flow  (scot %ud bone)  load  fo-flip-dire  (scot %p server)
              ::  %corks refers to the whole flow; skip the sequence number
              ::
                ?:(=(%cork load) ~ [(scot %ud seq) ~])
            ==
          ::
          ++  fo-wire
            |=  =were
            ^-  wire
            :: add rift to avoid dangling bones from previous eras
            ::
            =?  bone  &(?=(%bak dire) ?=(%van were))
              (mix 0b1 bone)
            =+  cont=[[(scot %p her)] [(scot %ud rift.per)] [(scot %ud bone)] ~]
            ?:  ?=(%van were)
              ::  to be backward-compatible, to/from vane wires use the same
              ::  format as ames; =(%van were) in the wire is not used
              ::
              [%bone cont]
            ::  %for: %plea(s) are always sent forward, %boon(s) %bak.
            ::
            [%mesa %flow were dire cont]
          ::
          +|  %entry-points
          ::
          ++  fo-call
            |=  $=  poke
                $%  [%pump mesa-message]             :: outgoing payloads
                    [%sink seq=@ud =gage:mess ok=?]  :: incoming payloads
                ==
            ^+  fo-core
            ::
            |^  ?-(-.poke %pump (pump +.poke), %sink (sink +.poke))
            ::
            ++  pump
              |=  load=mesa-message
              ?:  |((fo-to-close load) fo-corked)
                %-  %+  ev-tace  odd.veb.bug.ames-state
                    ?:  (fo-to-close load)
                      |.("skip %cork $plea; flow {<bone>} is closing")
                    |.("skip send; flow {<bone>} has been corked")
                fo-core
              ::
              =:   next.snd   +(next.snd)
                  loads.snd   (put:fo-mop loads.snd next.snd load)
                ==
              fo-send
            ::
            ++  sink
              |=  [seq=@ud =gage:mess ok=?]
              ?.  ?=([%message mark *] gage)
                %-  %+  ev-tace  odd.veb.bug.ames-state
                    |.("no op; weird %message gage {<-.gage>}")
                fo-core
              ::
              ?:  |(closing.state (~(has in corked.per) side))
                %-  %+  ev-tace  odd.veb.bug.ames-state
                    =+  ;;(mess=@tas +<.gage)
                    ?:  closing.state
                      |.("skip {<mess>}; flow in closing flow={<bone>}")
                    |.("skip {<mess>}; flow is corked flow={<bone>} ")
                fo-core
              ::  check that the message can be acked
              ::
              =+  flow-state=[bone=bone seq=seq last=last-acked.rcv]
              ?:  (gth seq +(last-acked.rcv))
                ::  no-op if future message
                ::
                %-  %+  ev-tace  odd.veb.bug.ames-state
                    |.("skip sink; future ack {<flow-state>}")
                fo-core
              ?.  (lte seq last-acked.rcv)
                ::  a %plea sinks on the backward receiver (from a forward flow)
                ::  a %boon sinks on the forward receiver (from a backward flow)
                ::
                %.([+.gage ok] ?-(dire %bak fo-sink-plea, %for fo-sink-boon))
              ?:  (gth (sub last-acked.rcv seq) 10)
                %-  %+  ev-tace  odd.veb.bug.ames-state
                    |.("skip sink; {<flow-state>}")
                fo-core
              %-  %+  ev-tace  snd.veb.bug.ames-state
                  |.
                  %+  weld  "send dupe ack {<flow-state>} for "
                  ?-(dire %bak "%plea", %for "%boon")
              ::
              (fo-send-ack seq)
            ::
            --
          ::
          ++  fo-take
            |=  [=were sign=flow-sign]
            ^+  fo-core
            ?+    were  !!  :: %pok is handle outside, in the message layer
                %nax  ?>(?=(%sage -.sign) (fo-take-nax +.sign))
                %ack  ?>(?=(%sage -.sign) (fo-take-ack +.sign))
                %cor  ?>(?=(%sage -.sign) (fo-take-cor +>.sign))
              ::
                %van
              ?+  -.sign  !!  :: %sage doesn't come from vanes
                %done  (fo-take-done +.sign)  :: ack from client vane
                %flub  =.(pending-ack.rcv %.n fo-core)
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
                %naxp  ?~(nax=(~(get by nax.rcv) seq) ~ `nax/u.nax)
                %cork  ?.(fo-corked ~ `gone/~)
                %poke  ?~(v=(get:fo-mop loads.snd seq) ~ `u.v)
            ::
                %ack
              ?:  (~(has by nax.rcv) seq)
                ::  if we have naxplanation state for this message—even
                ::  for pre-migration messages—we can guarantee that
                ::  the message was nacked
                ::
                `ack/error=%.y
              ?:  (lth seq line.state)
                ::  refuse to answer for pre-migration messages
                ::
                ::  XX can we guarantee that line.state was an ack?
                ::
                ::  In theory we can't guarantee it just by looking at the
                ::  sate of the flow, but, if it was a %nack we would have
                ::  state in nax.rcv for live naxplanations and if the
                ::  naxplanation had suceeded then they are not going to
                ::  resend the payload anymore.
                ::
                ::  if line.state was an %ack but it got lost we can not
                ::  know for sure, but, because we were not removing the
                ::  correct message from nax.sink it's very likely that
                ::  if line.state is not in nax.rcv that's because it
                ::  was indeed a %nack.
                ::
                ~
              ?:  ?&  (lth seq last-acked.rcv)
                      (gth (sub last-acked.rcv seq) 10)
                  ==
                :: if seq > gth 10, refuse to answer
                ::
                ~
              ?.  =(seq last-acked.rcv)
                ::  refuse to answer to future acks
                ::
                ~
              `ack/error=%.n
            ::
            ==
          ::
          +|  %request-sender
          ::
          ++  fo-send
            ^+  fo-core
            =+  loads=loads.snd ::  cache
            |-  ^+  fo-core
            =*  loop  $
            =+  num=(wyt:fo-mop loads)
            ?:  =(0 num)
              fo-core
            ?.  (gth send-window.snd 0)
              fo-core
            ::
            =^  [seq=@ud request=mesa-message]  loads  (pop:fo-mop loads)
            =.  send-window.snd  (dec send-window.snd)
            ::
            =/  [ack=spar poke=path]
              ::  fo-path builders refer to the other side, but %poke is on our
              ::  side; flip direction
              ::
              :-  [her (fo-ack-path seq our)]
              (%*(fo-pok-path fo-core dire.side fo-flip-dire) seq her)
            =/   =space  chum-to-our  ::  XX the namespace refers to the ack
            =/    =wire  (fo-wire %ack)
            =.  fo-core  (fo-emit hen %pass wire %a moke/[space ack poke])
            loop
          ::
          +|  %request-receiver
          ::
          ++  fo-sink-boon
            |=  [=page ok=?]
            ^+  fo-core
            ?.  ?=([%boon *] page)
              %-  %+  ev-tace  odd.veb.bug.ames-state
                  |.("no op; weird %boon page {<-.page>}")
              fo-core
            =.  fo-core  (fo-emit (ev-got-duct bone) %give %boon +.page)
            ::  handle a previous crash
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
            =.  last-acked.rcv  +(last-acked.rcv)
            %-  %+  ev-tace  msg.veb.bug.ames-state
                |.("hear complete %boon {<[bone=bone seq=last-acked.rcv]>}")
            (fo-send-ack last-acked.rcv)
          ::
          ++  fo-sink-plea
            |=  [=page ok=?]
            ^+  fo-core
            =.  pending-ack.rcv  %.y
            ::  receiver of a %plea request
            ::
            ?.  ok
              (fo-take-done:fo-core `*error)
            ::
            =+  ;;([%plea =plea] page)
            %-  %+  ev-tace  msg.veb.bug.ames-state
                |.("hear complete %plea {<[bone=bone seq=+(last-acked.rcv)]>}")
            ::
            ?:  &(=(vane %$) ?=([%ahoy ~] payload) ?=([%mesa ~] path)):plea
              ::  migrated %ahoy pleas are always acked
              ::
              %-  %+  ev-tace  odd.veb.bug.ames-state
                  |.("acking migrated %ahoy plea")
              ::
              (fo-take-done:fo-core ~)
            ?.  =(%$ vane.plea)
              =/  =wire  (fo-wire %van)
              %-  fo-emit
              ?+  vane.plea  ~|  %mesa-evil-vane^our^her^vane.plea  !!
                ?(%c %e %g %j)  [hen %pass wire vane.plea %plea her plea]
              ==
            ?:  ?=([%back ~] payload.plea)
              ::  ack %rege plea
              ::
              =.  fo-core  (fo-take-done:fo-core ~)
              ?+    path.plea  ~|  %mesa-evil-rege^our^her^path.plea  !!
                  [%test %ames ~]
                ?>  (regression-test(ames-state ames-state:fo-abet:fo-core) her)
                ~&  >  %regression-test-worked
                fo-core
              ::
                  [%ames ~]
                ::  regress peer back to ames
                ::
                =^  moves  ames-state
                  =<  sy-abet
                  ~|  %regress-crashed
                  %.  [`her dry=%.n]
                  %*  sy-rege  sy
                    ames-state  ames-state:fo-abet:fo-core
                  ==
                (fo-emil:fo-core(delete-per %.y) moves)
              ==
            ?>  &(?=([%cork ~] payload) ?=([%flow ~] path)):plea
            ::  publisher receives %cork
            ::  mark flow as closing
            ::  publish %cork %ack (in +hear-poke:ev-mess) in corked.per
            ::
            =.  fo-core
              ::  start %peek request to check if they have corked the flow
              ::  after reading the ack from our namespace
              ::
              %-  %+  ev-tace  fin.veb.bug.ames-state
                  |.("peek for %cork flow={<bone>}")
              ::
              fo-peek-cork
            ::  XX just fo-core(closing.state %.y)?
            ::
            (fo-take-done:fo-core(closing.state %.y) ~)
          ::
          +|  %from-vane
          ::
          ++  fo-take-done
            |=  error=(unit error)
            ^+  fo-core
            ::  if there's a pending-vane ack, is always +(last-acked)
            ::
            ?>  =(%.y pending-ack.rcv)
            =/  seq=@ud  +(last-acked.rcv)
            =:  last-acked.rcv   seq
                pending-ack.rcv  %.n
              ==
            =?  nax.rcv  ?=(^ error)
              =?  nax.rcv  (gth seq 10)
                ::  only keep the last 10 nacks
                ::
                (~(del by nax.rcv) (sub seq 10))
              (~(put by nax.rcv) seq u.error)
            (fo-send-ack seq)
          ::
          +|  %from-network
          ::
          ++  fo-take-ack
            |=  [seq=@ud =spar =gage:mess]
            ^+  fo-core
            ::  if all pokes have been processed no-op
            ::
            ?~  first=(pry:fo-mop loads.snd)
              %-  %+  ev-tace  odd.veb.bug.ames-state
                  |.("no message to %ack {<[bone=bone seq=seq]>}")
              fo-core
            ::  only handle acks for %pokes that have been sent
            ::
            ?.  (lth seq next.snd)
              %-  %+  ev-tace  odd.veb.bug.ames-state
                  |.("old %ack {<[bone=bone seq=seq]>}")
              fo-core
            ?>  ?=([%message *] gage)
            =+  ;;([%ack error=?] +.gage)  ::  XX
            ?.  =(key.u.first seq)
              ::  XX we shouldn't see this since send-window is always 1
              ::
              %-  %+  ev-tace  odd.veb.bug.ames-state
                  |.("hear out of order ack {<[seq=seq first=key.u.first]>}")
              :: if the ack we receive is not for the first, save it
              ::  XX if error, start +peeking right away?
              ::
              fo-core
              ::  XX TODO
              :: fo-core(cache.state (put:fo-cac cache.state seq error))
            |-  ^+  fo-core
            ?:  error
              ::  XX  make error=(unit error), and include the naxplanation there
              ::
              ::  if error start +peek for naxplanation
              ::
              (fo-peek-naxplanation seq)
            %-  %+  ev-tace  msg.veb.bug.ames-state
                |.("hear ack for {<[bone=bone seq=seq]>}")
            ::  ack is for the first, oldest pending-ack sent message;
            ::  remove it and XX start processing cached acks
            ::
            =^  *  loads.snd  (del:fo-mop loads.snd seq)
            ::  increase the send-window so we can send the next message
            ::
            =.  send-window.snd  +(send-window.snd)
            =.  can-be-corked
              ?&  closing.state    ::  we sent a %cork %plea
                  ?=(~ loads.snd)  ::  nothing else is pending
              ==
            =~  ::  send next messages
                ::
                fo-send
                ::  don't give %done for %boon and %cork; implicit %ack
                ::
                ?:  ?|  ?=(%bak dire)
                        can-be-corked
                    ==
                  fo-core
                (fo-emit (ev-got-duct bone) %give %done ~)
            ==
            :: XX TODO
            :: ::  are there any cached acks?
            :: ::
            :: :: ?~  cack=(pry:fo-cac cache.state)  fo-core
            :: ?.  =(key.u.cack +(seq))           fo-core
            :: ::  first ack in the cache is the next sent %poke; process
            :: ::
            :: :: =^  *  cache.state  (del:fo-cac cache.state key.u.cack)
            :: $(error val.u.cack, seq key.u.cack)
          ::
          ++  fo-take-nax
            |=  [seq=@ud =spar =gage:mess]
            ^+  fo-core
            =/  first       (pry:fo-mop loads.snd)
            =/  no-pokes=?  ?=(~ first)
            =/  miss-nax=?  &(?=(^ first) !=(key.u.first seq))
            =/  next-msg=@  next.snd  ::  cache
            ::
            =?  fo-core  no-pokes
              %-  %+  ev-tace  odd.veb.bug.ames-state
                  |.("no message to %naxplain {<[bone=bone seq=seq]>}")
              fo-core
            :: XX  if the ack we receive is not for the first, no-op
            :: XX  as currently implemented we only hear the naxplanation of
            ::     the oldest message
            ::
            =?  fo-core  miss-nax
              %-  %+  ev-tace  odd.veb.bug.ames-state
                  |.("missordered %naxplanation {<[bone=bone seq=seq]>}")
              fo-core
            ::  XX  use .nax.snd to confirm that we are waiting for this %naxp
            ::
            ::  naxplanation should be for the first, oldest pending message
            ::
            =?  loads.snd  &(!no-pokes !miss-nax)  +:(del:fo-mop loads.snd seq)
            ::  increase the send-window so we can send the next message
            ::  XX
            ::
            =?  send-window.snd  (lth send-window.snd send-window-max.snd)
              +(send-window.snd)
            =.  fo-core          fo-send  ::  send next messages
            ::  XX check path.spar
            ::  XX path.spar will be the full namespace path, peel off before?
            ::
            ?>  ?=([%message *] gage)
            =+  ;;([%nax =error] +.gage)  ::  XX
            ::  if the bone belongs to a closing flow and we got a
            ::  naxplanation, don't relay ack to the client vane
            ::
            ?:  closing.state  fo-core
            ?:  miss-nax       fo-core
            ::
            ::  if there are no unacked messages in the queue, this naxplanation
            ::  should be from migrating a nacked flow.
            ::  XX check what happens if the naxplanation comes in again
            ::
            ?.  ?|  !no-pokes  ::  there were unacked messages
                    &(no-pokes =(seq (dec next-msg)))  :: nacked, migrated msg
                ==
              %-  %+  ev-tace  odd.veb.bug.ames-state
                  |.("weird %naxp {<[bone=bone seq=seq next=next-msg]>}; skip")
              fo-core
            %-  %+  ev-tace  msg.veb.bug.ames-state
                |.("take %naxplanation {<[bone=bone seq=seq]>}")
            ::
            (fo-emit (ev-got-duct bone) %give %done `error)
          ::
          ++  fo-take-cor
            |=  [=spar =gage:mess]
            ^+  fo-core
            ::  sanity checks on the state of the flow
            ::
            ?>  ?&  ?=([%message %gone ~] gage)         ::  corked page received
                    closing.state                       ::  flow is in closing
                    !pending-ack.rcv                    ::  no pending acks
                ==
            ?:  ?=(%bak dire)
              ~|  [%fo-take-client-cork gage/gage state]
              ?>  !(~(has by nax.rcv) last-acked.rcv)   ::  %cork was not nacked
              fo-core
            ~|  [%fo-take-server-cork gage/gage state]
            ?~  first=(pry:fo-mop loads.snd)  !!
            ?>  ?&  =(1 (wyt:fo-mop loads.snd))         ::  %cork is unacked
                    ?=([%plea %$ [%flow ~] %cork ~] val.u.first)
                ==
            fo-core
          ::
          +|  %internals
          ::
          ++  fo-peek-naxplanation
            |=  seq=@ud
            ^+  fo-core
            %-  %+  ev-tace  fin.veb.bug.ames-state
                |.("peek for %naxplanation {<[bone=bone seq=seq]>}")
            ::
            =/  =wire    (fo-wire %nax)
            =/  =space   chum-to-our
            (fo-emit hen [%pass wire %a meek/[space her (fo-nax-path seq our)]])
          ::
          ++  fo-send-ack
            |=  seq=@ud
            ^+  fo-core
            %-  %+  ev-tace  msg.veb.bug.ames-state
                =+  ack=?~((~(get by nax.rcv) seq) "ack" "nack")
                |.("{ack} message {<[bone=bone seq=seq]>}")
            ::  emit (n)ack to unix; see +fo-peek where the (n)ack is produced
            ::
            =/  =path  (%*(fo-ack-path fo-core dire.side fo-flip-dire) seq her)
            (fo-emit [/ames]~ %pass /make-page %a mage/[chum-to-her her^path])
          ::
          ++  fo-peek-cork
            %^  fo-emit  hen  %pass
            ::  for-cor-path will produce a path for the %cork on the other side
            ::
            [(fo-wire %cor) %a meek/[chum-to-our her (fo-cor-path seq=0 our)]]
          ::
          --
        ::
        +|  %space-helpers
        ::  +chum-to-our: refers to payloads bounded in other peer's namespace
        ::
        ++  chum-to-our
          :-  %chum
          [server=life.per client=our life.ames-state symmetric-key.per]
        ::  +chum-to-her: refers to payloads bounded in our namespace
        ::
        ++  chum-to-her
          :-  %chum
          [server=life.ames-state client=her [life symmetric-key]:per]
        ::
        --
      ::
      +|  %system
      ::
      ++  sy
        =|  moves=(list move)
        ::
        |_  hen=duct
        ::
        +|  %helpers
        ::
        ++  sy-core  .
        ++  sy-abet  [moves ames-state]
        ++  sy-emit  |=(=move sy-core(moves [move moves]))
        ++  sy-emil  |=(mos=(list move) sy-core(moves (weld mos moves)))
        ::
        +|  %entry-points
        ::
        ++  sy-born
          %-  (slog leaf+"ames: unix-duct received on %born" ~)
          ::
          =/  turfs
            ;;  (list turf)
            =<  q.q  %-  need  %-  need
            (rof [~ ~] /ames %j `beam`[[our %turf %da now] /])
          ::
          =?  sy-core  ?=(~ +.chum.dead.ames-state)
            (sy-emit ~[/ames] %pass /mesa/retry %b %wait `@da`(add now ~m2))
          =?  chum.dead.ames-state  ?=(~ +.chum.dead.ames-state)
            chum/`[~[/ames] /mesa/retry `@da`(add now ~m2)]
          =^  cork-moves  cork.dead.ames-state
            ?.  ?=(~ +.cork.dead.ames-state)
              `cork.dead.ames-state
            :-  [~[/ames] %pass /recork %b %wait `@da`(add now ~d1)]~
            cork/`[~[/ames] /recork `@da`(add now ~d1)]
          ::
          =^  rots-moves  rots.dead.ames-state
            ?.  ?=(~ +.rots.dead.ames-state)
              `rots.dead.ames-state
            :-  [~[/ames] %pass /routes %b %wait `@da`(add now ~m2)]~
            rots/`[~[/ames] /routes `@da`(add now ~m2)]
          ::
          =.  sy-core
            %-  sy-emil
            ;:  weld
              cork-moves
              rots-moves
            ::
              ^-  (list move)
              :~  [hen %give %turf turfs]
                  [hen %give %saxo sy-get-sponsors]
                  (poke-ping-app hen our %kick fail=%.n)
              ==
            ==
            sy-core(ames-state ames-state(unix-duct hen))
        ::  +sy-init: first boot; subscribe to our info from jael
        ::
        ++  sy-init
          ^+  sy-core
          %-  sy-emil
          :~  [hen %pass /turf %j %turf ~]
              [hen %pass /private-keys %j %private-keys ~]
              [hen %pass /public-keys %j %public-keys [n=our ~ ~]]
          ==
        ::
        ::  +on-cong: adjust congestion control parameters
        ::
        ++  sy-cong  |=([msg=@ud mem=@ud] sy-core(cong.ames-state msg^mem))
        ::  +sy-crud: handle event failure; print to dill
        ::
        ++  sy-crud  |=(e=error (sy-emit hen %pass /crud %d %flog %crud e))
        ::  +sy-plug: handle key reservation
        ::
        ++  sy-plug
          |=  =path
          ^+  sy-core
          =/  key=@  (kdf:crypt 32 "mesa-shut-key" 32^eny)
          =/  kid=@ud
            ?~  latest=(ram:key-chain server-chain.ames-state)
              1
            .+(key.u.latest)
          =.  server-chain.ames-state
            (put:key-chain server-chain.ames-state kid [key path])
          ::  kid^key kill be used by remote %keen task when sending $peek
          ::
          (sy-emit hen %give %stub kid key)
        ::
        ++  sy-wake
          |=  [=wire error=(unit tang)]
          ^+  sy-core
          =?  sy-core  ?=(^ error)   :: XX use verbosity flag
            (sy-emit hen %pass /crud %d %flog %crud %wake-error u.error)
          ?:  ?=([%mesa %ask gift=@ *] wire)
            =/  iota=(pole iota)  (mesa-pave:ev t.t.wire)
            ?+  iota  ~&(unexpected-ask-wire/iota sy-core)
                [%turf *]
              (sy-emit hen %pass /turf %j %turf ~)
            ::
                [%public-keys [%p ship=@] *]
              %^  sy-emit  hen  %pass
              [/public-keys %j %public-keys [ship.iota ~ ~]]
            ==
          ?.  ?=([%mesa %retry ~] wire)
            ~&  >>>  %evil-behn-timer^wire
            sy-core
          ::  XX log if error
          ::  XX if we wake up too early, no-op, otherwise set new timer
          ::  XX if timed-out, update qos
          ::  XX expire direct route if the peer is not responding (%nail)
          ::  XX re-send comet attestation?
          ::  XX only timed-out (dead) outgoing %poke requests
          ::
          =.  chum.dead.ames-state
            chum/`[~[/ames] /mesa/retry `@da`(add now ~m2)]
          =^  prod-moves  ames-state
            ::  if there's been an error, reset the timer and skip %proding
            ::
            ?^(error `ames-state (sy-prod ~))
          %-  sy-emil
          :_  prod-moves
          [~[/ames] %pass /mesa/retry %b %wait `@da`(add now ~m2)]
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
          ::   Abandon all pretense of continuity and delete all messaging state
          ::   associated with .ship, including sent and unsent messages.
          ::   Also cancel all timers related to .ship.
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
            =.  sy-core
              =/  old-qos=qos  qos.+.u.peer
              =/  text=(unit tape)
                %^  qos-update-text  ship  %ames
                [old-qos *qos [kay.veb ships]:bug.ames-state]
              ?~  text  sy-core
              (sy-emit hen %pass /qos %d %flog %text u.text)
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
              =.  +>.u.peer  +:*peer-state
              ::  XX  reinitialize galaxy route if applicable
              ::
              =?  route.+.u.peer  =(%czar (clan:title ship))
                `[direct=%.y %& ship]
              (~(put by peers.ames-state) ship u.peer)
            ::  cancel all timers related to .ship
            ::
            =?  sy-core    ?=(%ship -.peer)
              %+  roll  ~(tap by snd.u.peer)
              |=  [[=snd=bone =message-pump-state] core=_sy-core]
              ^+  core
              ::
              ?~  next-wake=next-wake.packet-pump-state.message-pump-state
                core
              ::  note: copies +on-pump-rest:message-pump
              ::
              =/  wire  (make-pump-timer-wire ship snd-bone)
              =/  duct  ~[/ames]
              (sy-emit:core duct %pass wire %b %rest u.next-wake)
            =.  sy-core
              %-  sy-emit
              :*  unix-duct  %give  %nail  ship
                  ?.  ?=(%chum -.peer)
                    (get-forward-lanes our +.u.peer peers.ames-state)
                  ^-  (list lane)
                  ::  XX refactor
                  %+  turn
                    (get-forward-lanes-mesa our +.u.peer chums.ames-state)
                  |=  =lane:pact
                  ^-  (each @pC address)
                  ?@  lane
                    [%.y `@p`lane]
                  :-  %.n
                  %+  can  3
                  :~  4^p.lane
                      2^q.lane
                  ==
              ==
            ::  if one of our sponsors breached, give the updated list to vere
            ::
            =/  sponsors  (~(gas in *(set ^ship)) sy-get-sponsors)
            =?  sy-core  (~(has in sponsors) ship)
              (sy-emit unix-duct %give %saxo ~(tap in sponsors))
            ::
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
            =/  old-key       symmetric-key.+.u.peer
            =/  =private-key  sec:ex:crypto-core
            =/  new-key       (derive-symmetric-key public-key private-key)
            ::  recalculate paths in the .pit using the new key
            ::
            =?  peer  ?=([%chum ~ %known *] peer)
              =;  pit=_pit.u.peer
                peer(pit.u pit)
              %-  ~(rep by pit.u.peer)
              |=  [[=path req=request-state] pit=(map path request-state)]
              =/  [=space ack=^path]
                [space inner]:(ev-decrypt-path:ev path ship)
              =.  space
                ::  update life/keys in the space; for acks, update the client
                ::
                ?+  -.space  space
                  %publ  space(life life)
                  %chum  space(client-life life, key new-key)
                ==
              =.  path  (make-space-path space ack)
              ::  only recalculate poke paths if there's an associated payload
              ::
              =?  pay.req  ?=(^ pay.req)
                =/  [=^space poke=^path]
                  [space inner]:(ev-decrypt-path:ev u.pay.req ship)
                ::  for poke paths, update the server life
                ::
                =.  space
                  ?+  -.space  space  :: for %publ, our life hansn't changed
                    %chum  space(server-life life, key new-key)
                  ==
                `(make-space-path space poke)
              (~(put by pit) path req(ps ~))  :: XX drop any partial state
            ::  update values
            ::
            =.  symmetric-key.+.u.peer  new-key
            =.  life.+.u.peer           life
            =.  public-key.+.u.peer     public-key
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
              (sy-emit unix-duct %give %saxo sy-get-sponsors)
            ?~  sponsor
              %-  (slog leaf+"ames: {(scow %p ship)} lost sponsor, ignoring" ~)
              sy-core
            ::
            =/  peer  (sy-find-peer ship)
            ?.  ?=([?(%ship %chum) ~ %known *] peer)
              %.  sy-core
              (slog leaf+"ames: missing peer {<ship>} on new sponsor, skip" ~)
            =.  sponsor.+.u.peer   u.sponsor
            =?  chums.ames-state  ?=(%chum -.peer)
              (~(put by chums.ames-state) ship u.peer)
            =?  peers.ames-state  ?=(%ship -.peer)
              (~(put by peers.ames-state) ship u.peer)
            %-  sy-emit
            :*  unix-duct  %give  %nail  ship
                ?.  ?=(%chum -.peer)
                  (get-forward-lanes our +.u.peer peers.ames-state)
                ^-  (list lane)
                ::  XX refactor
                %+  turn
                  (get-forward-lanes-mesa our +.u.peer chums.ames-state)
                |=  =lane:pact
                ^-  ^lane
                ?@  lane
                  [%.y `@p`lane]
                :-  %.n
                %+  can  3
                :~  4^p.lane
                    2^q.lane
                ==
            ==
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
                ::   (ev-emit unix-duct %give %saxo get-sponsors)
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
              =/  ames-core  (ev:ames now^eny^rof hen ames-state)
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
                ?:  =(plea [%$ /flow %cork ~])
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
                =.  ames-core
                  =<  abet  ^+  peer-core
                  %-  ~(rep by keens.todos)
                  |=  [[=path ducts=(set duct)] cor=_peer-core]
                  (~(rep in ducts) |=([=duct c=_cor] (on-keen:c path duct)))
                ::
                %-  ~(rep by chums.todos)
                |=  [[=path ducts=(set duct)] cor=_ames-core]
                (~(rep in ducts) |=([=duct c=_cor] (on-chum:c ship^path)))
              ::
              (sy-emil moves)
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
              =+  per=ship^+.chum-state
              =+  ev-core=(ev-foco:ev-core:ev per)
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
              ::
              =.  ev-core
                ::  apply (public) remote scry requests
                ::
                %-  ~(rep by peeks.todos)
                |=  [[=path ducts=(set duct)] core=_ev-core]
                %-  ~(rep in ducts)
                |=  [=duct c=_core]
                (ev-req-peek:(ev-abed:c duct) publ/life.+.per path)
              ::
              =.  ev-core
                ::  apply (two-party) remote scry requests
                ::
                %-  ~(rep by chums.todos)
                |=  [[=path ducts=(set duct)] core=_ev-core]
                %-  ~(rep in ducts)
                |=  [=duct c=_core]
                (ev-req-peek:(ev-abed:c duct) space=chum-to-our:c path)
              ::
              =^  moves  ames-state  ev-abet:ev-core
              (sy-emil moves)
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
            :: XX remove; needed when changing types in %lull (for testing)
            :: =.  priv.ames-state
            ::   ;;  @
            ::   =<  q.q  %-  need  %-  need
            ::   =-  ~&  priv/-  -
            ::   (rof [~ ~] /ames %j `beam`[[our %vein %da now] /1])
            ::
            =/  pk=private-key  sec:ex:crypto-core
            =/  =symmetric-key  (derive-symmetric-key public-key pk)
            ::
            =/  peer
              ::  XX if the peer doesn't previously exist we insert it
              ::  based on the chosen core in state; see find-peer
              ?:  ?=(%ship wer)
                :-  %ship
                (gut-peer-state:(ev:ames now^eny^rof hen ames-state) ship)
              =/  chum-state  (~(get by chums.ames-state) ship)
              :-  %chum
              ?.(?=([~ %known *] chum-state) *fren-state +.u.chum-state)
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
            =?  sy-core  ?=(%czar (clan:title ship))
              %-  sy-emit
              :*  unix-duct  %give  %nail  ship
                  ?.  ?=(%chum -.peer)
                    (get-forward-lanes our +.peer peers.ames-state)
                  ^-  (list lane)
                  ::  XX  refactor
                  %+  turn
                    (get-forward-lanes-mesa our +.peer chums.ames-state)
                  |=  =lane:pact
                  ^-  ^lane
                  ?@  lane
                    [%.y `@p`lane]
                  :-  %.n
                  %+  can  3
                  :~  4^p.lane
                      2^q.lane
                  ==
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
          |=  ships=(list @p)
          |^  ^-  (quip move axle)
          ?:  =(~ ships)
            (~(rep by chums.ames-state) prod-peer)
          =|  moves=(list move)
          |-
          ?~  ships  [moves ames-state]
          =^  new-moves  ames-state
            ?~  peer=(~(get by chums.ames-state) i.ships)
              `ames-state
            (prod-peer [i.ships u.peer] moves ames-state)
          $(ships t.ships, moves (weld moves new-moves))
          ::
          ++  prod-peer
            |=  [[=ship =chum-state] moves=(list move) state=_ames-state]
            =+  per-sat=(get-per:ev ship)
            ?.  ?=([~ ~ %known *] per-sat)
              :_  state
              ::  XX  this shouldn't be needed
              ::  XX  only if %alien
              ?:  ?=(%pawn (clan:title ship))
                ::  XX resend attestation request?
                ::
                =/  spon=@p  (^sein:title ship)
                ?:  =(our spon)  moves  ::  XX  don't send to ourselves
                moves:(~(al-read-proof al ~[/ames]) ship `@ux`spon)
              ~&  retrieving-keys-again/ship
              :_  moves
              [[//keys]~ %pass /public-keys %j %public-keys ship ~ ~]
            ::
            =+  core=~(ev-core ev hen ship +.u.u.per-sat)
            ::
            =^  resend-moves  state
              =;  c=_core  ev-abet:c
              %-  ~(rep by pit.per.core)
              |=  [[=path req=request-state] core=_core]
              =*  peer  per.core
              =*  ship  her.core
              ::  update and print connection status
              ::
              =?  core  (ev-is-lane-dead:core ship peer)
                (ev-update-qos:core dead/now)
              ::  if =(~ pay.req); %naxplanation, %cork or external (i.e. not
              ::  coming from %ames) $peek request
              ::
              ?~  pact=(co-make-pact:co ack=[ship path] pay.req rift.peer)
                ::  XX don't crash since we are going to block the queue
                ev-core:core
              ?:  =(~ unix-duct)
                %.  ev-core:core
                (slog leaf+"ames: unix-duct pending; retry %push" ~)
              %-  ev-emit:core
              (push-pact u.pact (make-lanes ship [lane qos]:per.core))
            :_  state
            (weld moves resend-moves)
          ::
          --
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
          ?:  =(~ unix-duct)
            %-  (slog leaf+"ames: unix-duct pending; no-op" ~)
            sy-core
          ::
          %-  %+  %*(ev-tace ev her ship.stun)  sun.veb.bug.ames-state
              =/  lane=tape
                ?:  ?=(%& -.lane.stun)
                  "from {<p.lane.stun>}"
                =,  lane.stun
                =/  ip=@if  (end [0 32] p)
                =/  pt=@ud  (cut 0 [32 16] p)
                "lane {(scow %if ip)}:{((d-co:^co 1) pt)} ({(scow %ux p)})"
              |.("inject %stun {<-.stun>} {lane}")
          ::
          %-  sy-emit
          %^  poke-ping-app  unix-duct  our
          ?.  ?=(%fail -.stun)  -.stun
          [%kick fail=%.y]
        :: +sy-dear: handle lane from unix
        ::
        ++  sy-dear
          |=  [=ship =lane]
          ^+  sy-core
          ?:  =(~ unix-duct)
            %-  (slog leaf+"ames: unix-duct pending; no-op" ~)
            sy-core
          ?:  =(%czar (clan:title ship))
            sy-core
          =/  peer  (sy-find-peer ship)
          ?.  ?=([?(%ship %chum) ~ %known *] peer)
            sy-core
          =?  chums.ames-state  ?=(%chum -.peer)
            =.  lane.+.u.peer
              :-  ~
              ^-  lane:pact
              ?-  -.lane
                  %&  `@ux`p.lane
              ::
                  %|
                :-  %if
                [ip=`@if`(end [0 32] p.lane) pt=`@ud`(cut 0 [32 16] p.lane)]
              ==
            (~(put by chums.ames-state) ship u.peer)
          =?  peers.ames-state  ?=(%ship -.peer)
            =.  route.+.u.peer  `[direct=%.y lane]
            (~(put by peers.ames-state) ship u.peer)
          (sy-emit unix-duct %give %nail ship ~[lane])
        ::
        ::  +sy-tame: handle request to delete a route
        ::
        ++  sy-tame
          |=  =ship
          ^+  sy-core
          ?:  =(~ unix-duct)
            %-  (slog leaf+"ames: unix-duct pending; no-op" ~)
            sy-core
          ?:  =(%czar (clan:title ship))
            %-  %+  slog
                leaf+"ames: bad idea to %tame galaxy {(scow %p ship)}, ignoring"
            ~
            sy-core
          =/  peer  (sy-find-peer ship)
          ?.  ?=([?(%ship %chum) ~ %known *] peer)
            %.  sy-core
            (slog leaf+"ames: no peer-state for {(scow %p ship)}, ignoring" ~)
          =?  chums.ames-state  ?=(%chum -.peer)
            =.  lane.+.u.peer  ~
            (~(put by chums.ames-state) ship u.peer)
          =?  peers.ames-state  ?=(%ship -.peer)
            =.  route.+.u.peer  ~
            (~(put by peers.ames-state) ship u.peer)
          (sy-emit unix-duct %give %nail ship ~)
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
        ++  sy-rege
          |=  [ship=(unit ship) dry=?]
          |^  ^+  sy-core
          =;  updated-core=_sy-core
              ?:  dry
                ~&  >  test-local-regression-worked/ship
                sy-core
              ~&  >  local-regression-worked/ship
              updated-core
          ::
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
                route.peer  (get-route lane.fren)
                  qos.peer  qos.fren
               corked.peer  (divide-bones corked.fren)
              ossuary.peer  ossuary.fren
                chain.peer  client-chain.fren
            ==
            =.  peers.ames-state  (~(put by peers.ames-state) ship %known peer)
            ::
            =^  peek-moves  ames-state  (regress-peeks ship fren peer)
            =^  flow-moves  ames-state  (regress-flows ship fren ames-state)
            ::  delete ship from .chums
            ::
            =.  chums.ames-state  (~(del by chums.ames-state) ship)
            ::
            (sy-emil (weld peek-moves flow-moves))
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
            =+  event-core=(ev:ames now^eny^rof hen state)
            =/  peer=peer-state  (got-peer-state:event-core her)
            =+  peer-core=(abed-peer:pe:event-core her peer)
            =;  core=_peer-core
              abet:abet:core
            =+  ev-core=(ev-foco:ev her fren)
            %-  ~(rep by flows.fren)
            |=  [[side state=flow-state] core=_peer-core]
            =+  fo-core=~(. fo:ev-core hen^bone^dire state)
            ::
            =?  bone  ?=(%bak dire)  (mix 0b1 bone) :: [bone=%0 %bak] -> bone=%1
            ::
            =.  core
              =;  [cor=_core loads=_loads.snd.state]
                cor
              ::  init message-pump with highest sent and acked messages
              ::
              =?  snd.peer-state.core  (gth next.snd.state 1)
                ::  if we haven't sent anything, skip entry in .snd map
                ::
                =|  pump=message-pump-state
                %+  ~(put by snd.peer-state.core)  bone
                =*  next  next.snd.state
                ?~  loads.snd.state
                  pump(next next, current next)
                pump(current (dec next), next (dec next))
              ::  message-pump for %pleas and %boons
              ::
              %^  (dip:fo-mop:fo-core ,cor=_core)  loads.snd.state
                core
              |=  [cor=_core seq=@ud req=mesa-message]
              ^-  [(unit mesa-message) stop=? cor=_core]
              `[| (on-memo:cor bone req)]
            ::  message-sink
            ::
            ?:  =(0 last-acked.rcv.state)
              ::  if we haven't acked anything, skip entry in .rcv map
              ::
              core
            ::
            =|  sink=message-sink-state
            ::  if the flow is in closing we need to stop +peeking for the %cork
            ::  (in +regress-peek) and remove the flow on our side
            ::
            =?  closing.peer-state.core  closing.state
              (~(put in closing.peer-state.core) bone)
            ?:  &(?=(%bak dire) closing.state)
              ~&  >  %flow-in-closing^bone
              ::  this is going to reset timers for %boon/%naxplanations that
              ::  don't exist (probably fine?)
              ::
              (handle-cork:core bone)
            ::  drop any pending ack state and past naxplanations
            ::  XX  if some is still actively reading a naxplanation,
            ::      do we need to send it?
            ::  XX  better to drop any peeks for %naxplanations, %corks?
            ::
            =.  last-acked.sink      last-acked.rcv.state
            =.  last-heard.sink      last-acked.rcv.state
            =.  rcv.peer-state.core  (~(put by rcv.peer-state.core) bone sink)
            ::  naxplanations
            ::
            =/  naxp=^bone  (mix 0b10 bone)  ::  bone=%1 -> bone=%3
            =/  keys=(list [seq=@ud =error])
              %+  sort  ~(tap by nax.rcv.state)
              |=  [a=[@ud *] b=[@ud *]]
              (gth -.a -.b)
            ?~  keys
              core
            ?.  =(last-acked.rcv.state seq.i.keys)
              ::  last naxplained message is not the last acked; skip
              ::
              core
            ::  the last acked message was nacked; send naxplanation
            ::
            =|  pump=message-pump-state
            ::  we init the naxplanation pump using message 0 since the receiver
            ::  is going to create the flow anew as soon as it hears anything
            ::
            :: =.  pump
            ::   %_  pump
            ::     current  (dec -.i.keys)  :: XX unguarded
            ::     next     (dec -.i.keys)  :: XX unguarded
            ::   ==
            =.  snd.peer-state.core  (~(put by snd.peer-state.core) naxp pump)
            ::  send highest seq nack-trace message; this could be a resend
            ::  but we don't know if the other has acked it.
            ::
            abet:(call:(abed:mu:core naxp) %memo %naxplanation i.keys)
          ::
          ++  regress-peeks
            |=  [her=^ship fren=fren-state peer=peer-state]
            ^-  (quip move axle)
            =+  event-core=(ev:ames now^eny^rof hen ames-state)
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
            %-  ~(rep in for.req)
            |=  [hen=duct c=_core]
            ::  XX  inspect the duct to find %mesa wires?
            ::  XX  dropping any +peeks for %corks and %naxplanations
            ::      can this makes us end up in a bad state?
            ::
            ?:  ?=([[%ames %mesa %flow *] *] hen)
              c
            =.  path
              =/  [=space pax=^path]
                [space inner]:(ev-decrypt-path:ev path her)
              ?-    -.space
                  ?(%none %publ)  pax
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
            (on-keen:core(duct hen) ~ her path)
          ::
          ++  get-route
            |=  lane=(unit lane:pact)
            ^-  (unit [direct=? =^lane])
            ?~  lane  ~
            :-  ~
            ?@  u.lane
              [direct=%.y %.y `@p`u.lane]
            :+  direct=%.n   %.n
            %+  can  3
            :~  4^p.u.lane
                2^q.u.lane
            ==
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
      +|  %aliens-comets
      ::
      ++  al
        =|  moves=(list move)
        ::
        |_  hen=duct
        ::
        +|  %helpers
        ::
        ++  al-core  .
        ++  al-abet  [moves ames-state]
        ++  al-abed  |=(=duct al-core(hen duct))
        ++  al-emit  |=(=move al-core(moves [move moves]))
        ++  al-emil  |=(mos=(list move) al-core(moves (weld mos moves)))
        ++  al-tace
          |=  [verb=? her=ship print=(trap tape)]
          ^+  same
          (trace %mesa verb her ships.bug.ames-state print)
        ::
        ::
        +|  %entry-points
        ::  +ev-enqueue-alien-todo: helper to enqueue a pending request
        ::
        ::    Also requests key and life from Jael on first request.
        ::    If talking to a comet, requests attestation packet.
        ::
        ++  al-enqueue-alien-todo
          |=  $:  =ship
                  chum-state=(unit chum-state)
                  mutate=$-(ovni-state ovni-state)
              ==
          ^+  al-core
          ::  create a default $ovni-state on first contact
          ::
          =/  [already-pending=? todos=ovni-state]
            ?~  chum-state
              [%.n *ovni-state]
            [%.y ?>(?=(%alien -.u.chum-state) +.u.chum-state)]
          ::  mutate .todos and apply to permanent state
          ::
          =.  todos  (mutate todos)
          =.  chums.ames-state
            (~(put by chums.ames-state) ship %alien todos)
          ?:  already-pending
            al-core
          ::
          ?:  =(%pawn (clan:title ship))
            =/  spon=@p  (^sein:title ship)
            ?:  =(our spon)  al-core  ::  XX  don't send to ourselves
            (al-read-proof ship `@ux`spon)
          ::  NB: we specifically look for this wire in +public-keys-give in
          ::  Jael.  if you change it here, you must change it there.
          ::
          (al-emit hen %pass /public-keys %j %public-keys [n=ship ~ ~])
        ::
        ++  al-register-comet
          |=  [comet=@p open-packet signature=@ signed=@]  :: XX to %lull
          ^+  al-core
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
          =/  keys  (~(put by *(map life [suite=@ud pass])) 1 1 public-key)
          =/  ship-state  (~(get by chums.ames-state) comet)
          ?:  ?=([~ %known *] ship-state)
            al-core
          ::  insert comet
          ::
          =?  chums.ames-state  =(~ ship-state)
            (~(put by chums.ames-state) comet %alien *ovni-state)
          ?>  ?=([~ %alien *] ship-state)
          %-  %^  al-tace  sun.veb.bug.ames-state  comet
              =+  :+  pokes=(lent pokes.u.ship-state)
                    peeks=~(wyt by peeks.u.ship-state)
                  chums=~(wyt by chums.u.ship-state)
              |.("todos: {<pokes=pokes>} {<peeks=peeks>} {<chums=chums>}")
          =^  moves  ames-state
            =<  sy-abet
            %^  ~(sy-publ sy hen)  /comet  %full
            %+  ~(put by *(map ship point:jael))  comet
            =|  =point:jael
            point(rift 0, life 1, keys keys, sponsor `(^sein:title comet))
          ::
          (al-emil moves)
        ::
        ++  al-read-proof
          |=  [comet=ship =lane:pact]
          ^+  al-core
          =/  =space  [%publ life=1]
          =/  =path
            %+  make-space-path  space
            /a/x/1//pawn/proof/[(scot %p our)]/[(scot %ud life.ames-state)]
          ::  XX set up a timer to resend faster than the ~m2 retry?
          ::
          ::  we call the arm directly instead of sending a %meek task
          ::  so we can set up the comet lane which is not in state
          ::
          ?~  pact=(co-make-pact:co `spar`comet^path ~ rift=0)
            !!
          %-  %^  al-tace  fin.veb.bug.ames-state  comet
              |.("peek for attestation proof")
          (al-emit (push-pact u.pact (make-lanes comet `lane *qos)))
        ::
        ++  al-take-proof
          |=  [=lane:pact hop=@ud =name:pact =data:pact =next:pact]
          ^+  al-core
          ?.  ?=(%pawn (clan:title her.name))
            al-core
          ::
          ?>  ?=([%publ lyf=%'1' res=*] pat.name)
          ::
          =+  path=(validate-path +>.pat.name)
          ?>  ?=(poof-pith path)
          %-  %^  al-tace  rcv.veb.bug.ames-state  her.name
              |.("hear attestation proof")
          ::  this is an attestation for us, at our current life
          ::
          ?>  &(=(our rcvr.path) =(life.path life.ames-state))
          ::  1-fragment attestation
          ::
          ?>  =(1 (div (add tob.data 1.023) 1.024))
          ?>  ?=(%& -.aut.data)
          ::
          ~|  [name=name data=data]
          ::
          =+  ;;(proof=gage:mess (cue dat.data))
          ?>  ?=([%message %proof *] proof)
          ::  XX refactor with sift-open-packet?
          ::
          =+  ;;  [signature=@ signed=@]  (cue ;;(@ +>.proof))
          =+  ;;  =open-packet            (cue signed)
          ::
          ?>  %-  verify-sig:crypt
              :^    (end 8 (rsh 3 public-key.open-packet))
                  p.p.aut.data
                (en-beam [[her.name %$ ud+1] pat.name])
              (root:lss tob.data^dat.data)
          ::
          =.  al-core
            (al-register-comet her.name open-packet signature signed)
          =.  ames-state
            ::  discard moves; %nail gift is included in +sy-publ
            ::
            =/  =^lane
              ?@  lane  [%.y `@p`lane]
              :-  %.n
              %+  can  3
              :~  4^p.lane
                  2^q.lane
              ==
            ames-state:(sy-dear:sy her.name lane)
          al-core
        ::
        --
      ::
      +|  %message-constructors
      ::
      ++  co
        =|  moves=(list move)
        ::
        |_  hen=duct
        ::
        +|  %helpers
        ::
        ++  co-core  .
        ++  co-abet  [moves ames-state]
        ++  co-abed  |=(=duct co-core(hen duct))
        ++  co-emit  |=(=move co-core(moves [move moves]))
        ++  co-emil  |=(mos=(list move) co-core(moves (weld mos moves)))
        ++  co-tace
          |=  [verb=? her=ship print=(trap tape)]
          ^+  same
          (trace %mesa verb her ships.bug.ames-state print)
        ::
        +|  %entry-points
        ::
        ++  co-call
          |=  =task
          ^+  co-core
          ?+  -.task  ~|(-.task !!)
            %mage  (co-make-page +.task)
            %meek  (co-make-peek +.task)
            %moke  (co-make-poke +.task)
          ==
        ::
        +|  %message-constructor
        ::
        ::  XX remove all spaces from the task, and make the paths at callsite?
        ::
        ++  co-make-peek
          |=  [=space =spar]
          (co-make-mess spar(path (make-space-path space path.spar)) ~)
        ::
        ++  co-make-poke
          |=  [=space =ack=spar =poke=path]
          ::  XX  make all paths when the %moke task is sent?
          ::
          =.  path.ack-spar   (make-space-path space path.ack-spar)
          =.  poke-path
            =?  space  ?=(?(%publ %chum) -.space)
              ::  switch life(s) and ship, for payloads
              ::  XX  test that lifes are correctly checked in the +scry handler
              ::
              ::  lifes need to be switched since for %pokes, this is a payload
              ::  in our namespace
              ::
              ?:  ?=(%publ -.space)
                space(life life.ames-state)
              %_  space
                  server-life  client-life.space
                  client-life  server-life.space
                  client       ship.ack-spar
              ==
            (make-space-path space poke-path)
          ::
          ::  ack and poke paths are already encrypted at this point
          ::
          (co-make-mess ack-spar `poke-path)
        ::
        ++  co-make-page
          |=  [=space spar]
          ^+  co-core
          =+  per=(get-per:ev ship)
          ?.  ?=([~ ~ %known *] per)
            %-  %^  co-tace  odd.veb.bug.ames-state  ship
                |.("missing peer for page={(spud path)}")
            co-core  ::  %alien or missing
          =*  sat  +.u.u.per
          =/  space-path=^path  (make-space-path space path)
          =/  =name:pact
            [[our rift.ames-state] [13 ~] space-path]
          ?~  page=(co-get-page name)
            %-  %^  co-tace  odd.veb.bug.ames-state  ship
                |.("missing page={(spud space-path)}")
            co-core
          ::  XX the use case for sending pages are acks, that fit in one
          ::  (bloq=13) fragment. no-op if bigger than that?
          ::
          ::  XX only allow %ames to send %mage task? (inspecting the duct?)
          ::
          ?:  =(~ unix-duct)
            %.  co-core
            (slog leaf+"ames: unix-duct pending; will retry %push" ~)
          %-  co-emit
          %+  push-pact
            [hop=0 page/[name u.page ~]]
          ::  XX  check here if we have a lane, and if not, assume that it
          ::  came via a sponsor, to avoid breaking symmetric routing
          ::  XX  unnecessary? vere wil probably ignore this lane
          ::  and use the one it has stored in the pit
          ::
          (make-lanes ship [lane qos]:sat)
        ::
        ++  co-make-mess
          |=  [remote=spar payload=(unit path)]
          ^+  co-core
          =/  her  (~(get by chums.ames-state) ship.remote)
          ?>  ?=([~ %known *] her)
          =/  per=fren-state  +.u.her
          ?^  res=(~(get by pit.per) path.remote)
            ?>  =(payload pay.u.res)  ::  prevent overriding payload
            =.  pit.per
              %+  ~(put by pit.per)  path.remote
              u.res(for (~(put in for.u.res) hen))
            %_  co-core
                chums.ames-state
              (~(put by chums.ames-state) ship.remote known/per)
            ==
          ::
          ?~  pact=(co-make-pact remote payload rift.per)
            ~|  [remote=remote payload=payload rift=rift.per]
            !!
          =|  new=request-state
          =.  for.new   (~(put in for.new) hen)
          =.  pay.new   payload
          =.  chums.ames-state
            %+  ~(put by chums.ames-state)  ship.remote
            known/per(pit (~(put by pit.per) path.remote new))
          ::
          ?:  =(~ unix-duct)
            %.  co-core
            (slog leaf+"ames: unix-duct pending; will retry %push" ~)
          (co-emit (push-pact u.pact (make-lanes ship.remote [lane qos]:per)))
        ::
        ++  co-make-pact
          |=  [p=spar q=(unit path) =per=rift]
          ^-  (unit pact:pact)
          =/  nam  [[ship.p per-rift] [13 ~] path.p]
          ?~  q
            `[hop=0 %peek nam]
          ::  XX assert that the serializes path fits in the MTU
          ::  XX if path will be too long, put in [tmp] and use that path
          ::  %-  mess:plot:d
          ::  (en:name:d [[her=~nec rif=40] [boq=0 wan=~] pat=['c~_h' ~]]))
          ::  [bloq=q=3 step=r=12]
          ::  =/  has  (shax u.u.res)
          ::  =.  tmp-chums.ames-state
          ::    %+  ~(put by tmp-chums.ames-state)  has
          ::    [%some-envelope original-path u.u.res])
          ::  //ax/[$ship]//1/temp/[hash]
          ::
          =/  man=name:pact  [[our rift.ames-state] [13 ~] u.q]
          ::
          ?~  page=(co-get-page man)
            ::  XX
            ~&  [%no-page man=man]  ~
          `[hop=0 %poke nam man u.page]
        ::
        ++  co-get-page
          |=  =name:pact
          ^-  (unit data:pact)
          =/  res=(unit (unit cage))
            (peek ~ /ames-get-page %x (name-to-beam name))  :: XX
          ?.  ?=([~ ~ %atom *] res)
            ~
          =;  page=pact:pact
            ?>(?=(%page +<.page) `data.page)
          =>  [res=res de=de:pact]
          :: ~>  %memo./ames/get-page
          =+  ;;([pac=@ *] q.q.u.u.res)
          -:($:de pac)
        ::
        --
      ::
      +|  %namespaces
      ::  publisher-side, message-level (public namespace)
      ::
      ++  peek-publ
        |=  [bem=beam tyl=(pole knot)]
        ^-  (unit (unit cage))
        ?>  ?=([%publ lyf=@ pat=*] tyl)
        =/  lyf  (slaw %ud lyf.tyl)
        ?~  lyf  [~ ~]
        ?.  =(u.lyf life.ames-state)
          ~
        ?~  inn=(inner-path-to-beam our pat.tyl)
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
      ::  publisher-side, message-level (two-party encrypted namespace)
      ::
      ++  peek-chum
        |=  [bem=beam tyl=(pole knot)]
        ^-  (unit (unit cage))
        ?>  ?=([%chum lyf=@ her=@ hyf=@ cyf=@ ~] tyl)
        =/  lyf  (slaw %ud lyf.tyl)
        =/  her  (slaw %p her.tyl)
        =/  hyf  (slaw %ud hyf.tyl)
        =/  cyf  (slaw %uv cyf.tyl)
        ?:  |(?=(~ lyf) ?=(~ her) ?=(~ hyf) ?=(~ cyf))
          [~ ~]
        ?.  =(u.lyf life.ames-state)
          ~
        ?~  key=(get-key-for u.her u.hyf)
          ~
        =/  pat=path  (open-path:crypt u.key u.cyf)
        ?~  inn=(inner-path-to-beam our pat)
          ~
        ?~  res=(rof `[u.her ~ ~] /ames/chum vew.u.inn bem.u.inn)
          ~
        ?~  u.res
          [~ ~]
        =>  [key=u.key cyf=u.cyf bem=bem res=res ..crypt] :: XX rift.ames-state
        :: ~>  %memo./ames/chum
        :: XX rift.ames-state
        =/  gag  [p q.q]:u.u.res
        =/  ful  (en-beam bem)
        =/  ser  (jam gag)
        =/  cyr  (encrypt:crypt key cyf ser)
        :+  ~  ~
        message/!>([%hmac (mac:crypt key ful (root:lss (met 3 cyr)^cyr)) cyr])
      ::  publisher-side, message-level (group encrypted namespace)
      ::
      ++  peek-shut
        |=  [bem=beam tyl=(pole knot)]
        ^-  (unit (unit cage))
        ?>  ?=([%shut kid=@ cyf=@ ~] tyl)
        =/  kid  (slaw %ud kid.tyl)
        =/  cyf  (slaw %uv cyf.tyl)
        ?:  |(?=(~ kid) ?=(~ cyf))
          [~ ~]
        ?~  key=(get:key-chain server-chain.ames-state u.kid)
          ~
        =/  pat  (open-path:crypt -.u.key u.cyf)
        ::  XX check path prefix
        ?~  inn=(inner-path-to-beam our pat)
          ~
        ?~  res=(rof [~ ~] /ames/shut vew.u.inn bem.u.inn)
          ~
        ?~  u.res
          [~ ~]
        ::  XX  rift.ames-state
        =>  [key=key cyf=u.cyf bem=bem res=res ..crypt]
        :: ~>  %memo./ames/shut
        =/  cry=@uxI  (rsh 8 (rsh 3 -.u.key))
        =/  sgn=@uxI  (end 8 (rsh 3 -.u.key))
        =/  gag  [p q.q]:u.u.res
        =/  ful  (en-beam bem)
        =/  ser  (jam gag)
        =/  cyr  (encrypt:crypt cry iv=cyf ser)
        =/  sig  (sign:crypt sgn ful (root:lss (met 3 cyr)^cyr))
        ``[%message !>([%sign sig cyr])]
      ::  publisher-side, flow-level
      ::
      ++  peek-flow
        |=  tyl=(pole knot)
        ^-  (unit (unit cage))
        ?:  ?=([%flow bone=@ %cork pat=*] tyl)
          (peek-cork tyl)
        ?>  ?=([%flow bone=@ =load =dire rcvr=@ mess=@ ~] tyl)
        =/  bone  (slaw %ud bone.tyl)
        =/  rcvr  (slaw %p rcvr.tyl)
        =/  mess  (slaw %ud mess.tyl)
        ?:  |(?=(~ bone) ?=(~ rcvr) ?=(~ mess))
          [~ ~]
        =+  per-sat=(get-per u.rcvr)
        ?.  ?=([~ ~ %known *] per-sat)
          ~  ::  %alien or missing
        =+  ev-core=(ev-foco:ev u.rcvr +.u.u.per-sat)
        =+  fo-core=(fo-abed:fo:ev-core ~[//scry] side=[u.bone dire.tyl])
        ?:  &(?=(%ack load.tyl) fo-corked:fo-core)
            :: ~&  >>>  corked-flow-dropping/load^corked.per  :: XX remove
            ::  if the flow is corked, block
            ::  XX when are corked bones evicted?
            ::
            ~  ::  XX  [~ ~]
        ::
        ?~(res=(fo-peek:fo-core load.tyl u.mess) ~ ``[%message !>(u.res)])
      ::  client/server %mesa %corks, flow-level
      ::
      ++  peek-cork
        |=  tyl=(pole knot)
        ^-  (unit (unit cage))
        ?>  ?=([%flow bone=@ %cork =dire rcvr=@ ~] tyl)
        =/  bone  (slaw %ud bone.tyl)
        =/  rcvr  (slaw %p rcvr.tyl)
        ?:  |(?=(~ bone) ?=(~ rcvr))
          [~ ~]
        =+  per-sat=(get-per u.rcvr)
        ?.  ?=([~ ~ %known *] per-sat)
          ~  ::  %alien or missing
        =+  ev-core=(ev-foco:ev u.rcvr +.u.u.per-sat)
        =+  fo-core=(fo-abed:fo:ev-core ~[//scry] side=[u.bone dire.tyl])
        ?~(res=(fo-peek:fo-core %cork 0) ~ ``[%message !>(u.res)])
      ::  comet attestations
      ::
      ++  peek-pawn
        |=  tyl=(pole knot)
        ^-  (unit (unit cage))
        ?>  ?=([%pawn %proof rcvr=@ life=@ ~] tyl)
        ::  only comets have this
        ::
        ?.  ?=(%pawn (clan:title our))
          [~ ~]
        =/  rcvr  (slaw %p rcvr.tyl)
        =/  life  (slaw %ud life.tyl)
        ?:  |(?=(~ life) ?=(~ rcvr))
          [~ ~]
        ::
        =+  core=(ev:ames now^eny^rof ~[//attestation] ames-state)
        =/  =open-packet
          [pub:ex:crypto-core our life.ames-state u.rcvr u.life]
        :+  ~  ~
        [%message !>(proof/(sign:as:crypto-core (jam open-packet)))]
      ::  publisher-side, weight of a noun bounded at .pat, as measured by .boq
      ::
      ++  peek-whey
        |=  tyl=(pole knot)
        ^-  (unit (unit cage))
        ?>  ?=([%whey boq=@ pat=*] tyl)
        =/  boq  (slaw %ud boq.tyl)
        ?~  boq  [~ ~]
        ?~  inn=(inner-path-to-beam our pat.tyl)
          ~
        ::
        ?~  res=(rof [~ ~] /ames/whey vew.u.inn bem.u.inn)
          ~
        :^  ~  ~  %whey
        !>([boq=u.boq (met u.boq (jam ?~(u.res ~ [p q.q]:u.u.res)))])
      ::  receiver-side, verify packet auth
      ::
      ++  peek-veri
        |=  tyl=(pole knot)
        ^-  (unit (unit cage))
        ?>  ?=([%veri typ=?(%sign %hmac) her=@ aut=@ rut=@ pat=*] tyl)
        =/  her  (slaw %p her.tyl)
        =/  aut  (slaw %uv aut.tyl)
        =/  rut  (slaw %uv rut.tyl)
        ?:  |(?=(~ her) ?=(~ aut) ?=(~ rut))
          [~ ~]
        =/  ful  (en-beam [[u.her %$ ud+1] pat.tyl])
        :^  ~  ~  %flag  !>  :: XX is this right?
        ?-  typ.tyl
          %sign  (verify-sig:crypt (sig-key pat.tyl u.her) u.aut ful u.rut)
          %hmac  (verify-mac:crypt (mac-key pat.tyl u.her) u.aut ful u.rut)
        ==
      ::  metadata query; XX only flow information supported
      ::
      ++  peek-meta
        |=  tyl=(pole knot)
        ^-  (unit (unit cage))
        ?>  ?=([%meta pat=*] tyl)
        =/  [ship=(unit ship) bone=(unit bone) =dire]
          ?+    pat.tyl  [~ ~ *dire]
              [%ship ship=@ flow=[%flow bone=@ =dire *]]
            :+  (slaw %p ship.pat.tyl)
              (slaw %ud bone.flow.pat.tyl)
            dire.flow.pat.tyl
          ==
        ?:  |(?=(~ ship) ?=(~ bone))
          [~ ~]
        =+  per-sat=(get-per u.ship)
        ?.  ?=([~ ~ %known *] per-sat)
          ~  ::  %alien or missing
        ?>  ?=([@ @ %flow @ @ qery=*] pat.tyl)
        =+  ev-core=(ev-foco:ev u.ship +.u.u.per-sat)
        =/  =side  [u.bone dire]
        =+  fo-core=(fo-abed:fo:ev-core ~[//scry] side)
        ?.  (~(has by flows.per.fo-core) side)
          ~
        =,  state:fo-core
        ?+    qery.pat.tyl  ~
            ~          ~ :: XX implement full meta
            [%clos ~]  ``message/!>(clogs/closing)
            [%cork ~]  ?~(r=(fo-peek:fo-core %cork 0) ~ ``[%message !>(u.r)])
            [%line ~]  ``message/!>(line/line)
            [%lods ~]  ``message/!>(lods/(wyt:fo-mop:fo-core loads.snd))
            [%next ~]  ``message/!>(next/next.snd)
            [%last ~]  ``message/!>(last/last-acked.rcv)
          ::
            [%mess mess=@ m-qery=*]
          =/  mess=(unit @ud)  (slaw %ud mess.qery.pat.tyl)
          ?:  ?=(~ mess)
            [~ ~]
          ?+  m-qery.qery.pat.tyl  ~
            [%whey ~]  ~
            [%naxp ~]  ``message/!>(naxp/(~(has by nax.rcv) u.mess))
          ==
        ==
      ::
      ++  peek
        ^-  roon
        |=  [lyc=gang pov=path car=term bem=beam]
        ^-  (unit (unit cage))
        ?:  ?&  =(our p.bem)
                =(%$ q.bem)
                =([%ud 1] r.bem)
                =(%x car)
            ==
          =/  tyl=(pole knot)  s.bem
          ?+    tyl  ~
            ::  message-level public namespaces
            ::
              [%publ lyf=@ pat=*]                         (peek-publ bem tyl)
              [%chum lyf=@ her=@ hyf=@ cyf=@ ~]           (peek-chum bem tyl)
              [%shut kid=@ cyf=@ ~]                       (peek-shut bem tyl)
              [%pawn %proof rcvr=@ life=@ ~]              (peek-pawn tyl)
            ::  message-level private namespaces
            ::
              $%([%flow *] [%pawn *] [%whey *] [%meta *])
            ?:  =(~ lyc)  ~
            ?+  tyl  ~
              [%flow bone=@ load dire rcvr=@ *]           (peek-flow tyl)
              [%whey boq=@ pat=*]                         (peek-whey tyl)
              [%meta pat=*]                               (peek-meta tyl)
            ==
            ::  client-side, protocol-level (authentication namespace)
            ::
              [%veri ?(%sign %hmac) her=@ aut=@ rut=@ *]  (peek-veri tyl)
            ::  publisher-side, protocol-level
            ::
              [%mess ryf=@ res=*]
            =/  ryf  (slaw %ud ryf.tyl)
            ?~  ryf  [~ ~]
            ?.  =(rift.ames-state u.ryf)      ::  XX unauthenticated
              ~
            =*  rif  u.ryf
            =/  nex
              =>  |%  +$  typ  ?(%auth %data)
                      +$  ser  ?(%etch %pure)
                  --
              ::
              ^-  $@  ~
                  $:  pat=path
                      $=  pac       ::  XX control packet serialization
                      $@  ~
                      $:  boq=bloq
                          ser=?
                          wan=$@(~ [=typ fag=@ud])
                  ==  ==
              ?+    res.tyl  ~
                  [%$ pat=*]  [pat.res.tyl ~]
              ::
                  [%pact boq=@ =ser %init pat=*]
                ?~  boq=(slaw %ud boq.res.tyl)
                  ~
                [pat.res.tyl u.boq ?=(%etch ser.res.tyl) ~]
              ::
                  [%pact boq=@ =ser =typ fag=@ pat=*]
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
            =/  msg  ;;([typ=?(%sign %hmac) aut=@ ser=@] q.q.u.u.res)
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
            =;  [=pact:pact pairs=(list (unit [@ux @ux])) pof=@ux]
              ?>  ?=(%page +<.pact)
              ?:  (gth fag (div (add tob.data.pact 1.023) 1.024))
                [~ ~]
              ?.  ser.pac.nex
                ``[%packet !>([pact pairs])]
              =;  airs=(list @ux)
                ``[%atom !>([p:(fax:plot (en:^pact pact)) airs pof])]
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
              =/  pof=@ux  (rep 8 proof.lss-proof)
              =/  dat  [tob [%& mes] (rep 8 proof.lss-proof)]  :: XX types
              [[hop=0 %page nam dat ~] ~ pof]
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
              [[hop=0 %page nam dat ~] pairs (rep 8 proof.lss-proof)]
            ==
          ::  XX need a single namespace entrypoint to validate
          ::     generically any authentication tag for a message
          ::
          ::  /ax/[$ship]//1/validate-message/[auth-string]/[blake3-hash]/[path]
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
        ::  /ax/corked/[ship]/[?(%for %bak)]           (set side)
        ::  /ax/corked/[ship]/[?(%for %bak)]/[bone]    ?(%.y %.n)
        ::  /ax/closing/[ship]/[?(%for %bak)]          (set side)
        ::  /ax/closing/[ship]/[?(%for %bak)]/[bone]   ?(%.y %.n)
        ::  /ax/closing/[ship]/lanes           $@(gal=@ux $%([%if ... [%is))
        ::  /ax/chums/[ship]                   chum-state
        ::  /ax/chums/[ship]/lanes             $@(gal=@ux $%([%if ... [%is))
        ::  /ax/ahoyed/[ship]                  ?(%.y %.n)
        ::
        ?.  ?=(%x car)  ~
        =/  tyl=(pole knot)  s.bem
        ::  private endpoints
        ::
        ?.  =([~ ~] lyc)  ~
        ?+    tyl  ~
            [%corked her=@ =dire req=*]
          =/  who  (slaw %p her.tyl)
          ?~  who  [~ ~]
          =/  per  (~(get by chums.ames-state) u.who)
          ?.  ?=([~ %known *] per)
            ~
          ?+  req.tyl  ~
              ~
            ``noun+!>(corked.u.per)
          ::
              [bone=@ ~]
            ?~  bone=(slaw %ud bone.req.tyl)
              [~ ~]
            ``atom+!>((~(has in corked.u.per) u.bone dire.tyl))
          ==
        ::
            [%closing her=@ =dire req=*]
          =/  who  (slaw %p her.tyl)
          ?~  who  [~ ~]
          =/  per  (~(get by chums.ames-state) u.who)
          ?.  ?=([~ %known *] per)
            ~
          ?+    req.tyl  ~
              ~
            :^  ~  ~  %noun  !>
            %-  ~(rep by flows.u.per)
            |=  [[=side flow=flow-state] clo=(set side)]
            ?.(closing.flow clo (~(put in clo) side))
          ::
              [bone=@ ~]
            ?~  bone=(slaw %ud bone.req.tyl)
              [~ ~]
            =+  ev-core=(ev-foco:ev u.who +.u.per)
            =+  fo-core=(fo-abed:fo:ev-core ~[//scry] u.bone dire.tyl)
            ``atom+!>(closing.state.fo-core)
          ==
        ::
            [%chums req=*]
          ?-    req.tyl
              ~
            :^  ~  ~  %noun
            !>  ^-  (map ship ?(%alien %known))
            (~(run by chums.ames-state) ^head)
          ::
              [%all ~]
            :^  ~  ~  %noun
            =|  out=(map ship [?(%peer %chum) ?(%alien %known)])
            !>  ^+  out
            =/  chums=_out
              (~(run by chums.ames-state) |=(chum-state chum/+<-))
            =/  peers=_out
              (~(run by peers.ames-state) |=(ship-state peer/+<-))
            %-  ~(gas by *_out)
            (weld ~(tap by chums) ~(tap by peers))
          ::
              [her=@ req=*]
            =/  who  (slaw %p her.req.tyl)
            ?~  who
              [~ ~]
            =/  chum  (~(get by chums.ames-state) u.who)
            =/  peer  (~(get by peers.ames-state) u.who)
            ?+    req.req.tyl  ~
                ~
              ?~  chum
                ~&  (~(get by peers.ames-state) u.who)
                ::
                [~ ~]
              ``noun+!>(u.chum)
              ::
                [%lanes ~]
              ::  this duplicates the routing hack from +send-blob:event-core so
              ::  long as neither the peer nor the peer's sponsoring galaxy is
              ::  us, and the peer has been reached recently:
              ::
              ::  - no route to the peer, or peer has not been reached recently:
              ::    send to the peer's sponsoring galaxy
              ::  - direct route to the peer: use that
              ::  - indirect route to the peer: send to both that route and the
              ::    the peer's sponsoring galaxy
              ::
              ?:  =(our u.who)
                ~
              =/  sax
                (rof [~ ~] /ames j/`beam`[[our %saxo %da now] /(scot %p u.who)])
              =/  gal=(unit @p)
                ?.  ?=([~ ~ *] sax)
                  ~
                `(rear ;;((list ship) q.q.u.u.sax))
              ?~  gal
                ~
              :^  ~  ~  %noun
              !>  ^-  [sponsor=@p (list lane:pact)]
              :-  u.gal
              ?:  ?=([~ %known *] chum)
                (get-forward-lanes-mesa our +.u.chum chums.ames-state)
              ?.  ?=([~ %known *] peer)
                %-  %+  %*(ev-tace ev-core:ev her u.who)  odd.veb.bug.ames-state
                    |.("alien peek for lanes")
                ::
                ?:(=(our u.gal) ~ [`@ux`u.gal]~)
              %+  turn  (get-forward-lanes our +.u.peer peers.ames-state)
              |=  lane=(each @p address)
              ?-    -.lane
                  %&  `@ux`p.lane
              ::
                  %|
                :+    %if
                  ip=`@if`(end [0 32] p.lane)
                pt=`@ud`(cut 0 [32 16] p.lane)
              ==
            ::
            ==
          ==
        ::
            [%ahoyed her=@ ~]
          =/  who  (slaw %p her.tyl)
          ?~  who  [~ ~]
          ``atom+!>((~(has by chums.ames-state) u.who))
        ==
      ::
      +|  %helpers
      ::
      ++  push-pact  :: XX forwarding?
        |=  [=pact:pact lanes=(list lane:pact:ames)]
        ^-  move
        ?<  =(~ unix-duct)
        =/  =ship
          ?-  +<.pact  ::  XX
            %peek  her.name.pact
            %poke  her.ack.pact
            %page  her.name.pact
          ==
        ::
        %-  %+  %*(ev-tace ev her ship)  snd.veb.bug.ames-state
            |.("pushing {<+<.pact>} packet")
        ::
        =/  p=plot  (en:^pact pact)
        =/  =blob   p:(fax:plot p)
        =+  parse-h=|=(a=@ -:($:de:head a))
        [unix-duct %give %push lanes blob]
      ::
      ++  make-lanes
        |=  [her=ship dir=(unit lane:pact) =qos]
        ^-  (list lane:pact:ames)
        %+  weld
          ?:  ?=(%live -.qos)  ~
          ::  if the route has expired, send to the sponsor as well
          ::
          (drop (get-sponsor her))
        ?~(dir ~ (drop dir))
      ::
      ++  get-sponsor
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
      ::  +get-key-for : eddh with our key
      ::
      ++  get-key-for
        |=  [=ship =life]
        ^-  (unit pass)
        =+  chum=(~(get by chums.ames-state) ship)
        =+  peer=(~(get by peers.ames-state) ship)
        ?:  &(?=(^ peer) ?=(~ chum))
          ::  migrated peer; no-op
          ::
          ~
        ?.  ?=([~ %known *] chum)
          =<  `pass  :: XX check suite?
          ;;  [suite=@ud =pass]
          =<  q.q  %-  need  %-  need
          %^  rof  [~ ~]  /mesa
          [%j `beam`[[our %puby %da now] /(scot %p ship)/(scot %ud life)]]
        ?.  =(life life.+.u.chum)
          ~  :: XX  log?
        `symmetric-key.+.u.chum
      ::
      ++  authenticate
        |=  [rut=@uxI aut=auth:pact =name:pact]
        ^-  ?
        ?>  ?=([%& *] aut)
        =/  ful  (en-beam [[her.name %$ ud+1] pat.name])
        ?-  -.p.aut
          %&  (verify-sig:crypt (sig-key [pat her]:name) p.p.aut ful rut)
          %|  (verify-mac:crypt (mac-key [pat her]:name) p.p.aut ful rut)
        ==
      ::
      ++  sig-key
        |=  [=path =ship]
        ^-  @uxI
        =/  tyl=(pole knot)  path
        ?>  ?=([%publ lyf=@ pat=*] tyl)
        =/  lyf  (slaw %ud lyf.tyl)
        ?>  ?=(^ lyf)
        =+  sat=(got-per ship)
        ?>  =(life.sat u.lyf)
        (end 8 (rsh 3 public-key.sat))
      ::
      ++  mac-key
        |=  [=path =ship]
        ^-  @uxI
        =/  tyl=(pole knot)  path
        ?+    tyl  !!
            [%chum lyf=@ her=@ hyf=@ pat=[cyf=@ ~]]
          =/  her  (slaw %p her.tyl)
          ?>  ?=(^ her)
          =/  her=@p  ?:(=(u.her our) ship u.her)
          =+  sat=(got-per her)
          ?>  (lte (met 3 symmetric-key.sat) 32)
          `@uxI`symmetric-key.sat
        ::
            [%shut kid=@ pat=[cyf=@ ~]]
          =/  kid  (slaw %ud kid.tyl)
          ?>  ?=(^ kid)
          ?~  key=(get:key-chain client-chain:(got-per ship) u.kid)
            !!  :: XX handle
          ?>  (lte (met 3 -.u.key) 32)
          `@uxI`-.u.key
        ==
      ::
      ++  crypto-core
        =>  [priv=priv.ames-state ..crypto]
        ~>(%memo./mesa/crypto-core (nol:nu:crub:crypto priv))
      ::
      ++  regression-test
        |=  her=ship
        ^-  ?
        =/  rege-state=axle
          ~|  %regress-crashed
          ames-state:(sy-rege:sy `her dry=%.n)
        =/  ahoy-state=axle
          ~|  %migrate-crashed
          =+  event-core=(ev:ames now^eny^rof ~[//rege] rege-state)
          =/  peer=peer-state  (got-peer-state:event-core her)
          ames-state:on-migrate:(abed-peer:pe:event-core her peer)
        ::  XX  compare pre/post migrated states
        ::
        ~&  >>  %regression-test
        %+  ^regression-test
          (~(got by chums.ames-state) her)
        (~(got by chums.ahoy-state) her)
      ::
      ++  validate-poke  !! :: XX TODO
      ::
      +|  %fren-helpers
      ::
      ++  got-per
        |=  =ship
        ^-  fren-state
        ~|  %freaky-alien^ship
        =-  ?>(?=([%known *] -) ->)
        (~(got by chums.ames-state) ship)
      ::  +get-per: lookup .her state, ~ if missing, [~ ~] if %alien
      ::
      ++  get-per
        |=  her=ship
        ^-  (unit (unit chum-state))
        ?~(per=(~(get by chums.ames-state) her) ~ `per)
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
    ++  me-core  (mesa now eny rof)
    ++  am-core  (ames now eny rof)
    ++  ev-core  (ev-abed:ev:me-core hen)
    ++  al-core  (al-abed:al:me-core hen)
    ++  pe-abed  |=(=duct pe-core(hen duct))
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
      ::  common tasks
      ::
        %load  `vane-gate(ames-state ames-state(core +.task))
        %plea  (pe-plea +.task)
        %cork  (pe-cork +.task)
        %keen  (pe-keen +.task)
        %chum  (pe-chum +.task)
        %yawn  (pe-cancel all=| +.task)
        %wham  (pe-cancel all=& +.task)
      ::  |mesa only tasks
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
        (call:am-core hen ~ soft+plea/ship^plea)
      =^  moves  ames-state
        ?:  ?=([~ %known *] +.ship-state)
          =<  ev-abet
          %.  plea
          ev-req-plea:(ev-foco:ev-core ship +.u.ship-state)
        ::
        =<  al-abet
        %^  al-enqueue-alien-todo:al-core  ship  +.ship-state
        |=  todos=ovni-state:ev-core
        todos(pokes [[hen^plea/plea] pokes.todos])
      moves^vane-gate
    ::
    ++  pe-cork
      |=  =ship
      =/  =plea  [%$ /flow %cork ~]
      =/  ship-state  (pe-find-peer ship)
      ::
      ?:  ?=(%ames -.ship-state)
        (call:am-core hen ~ soft+cork/ship)
      =^  moves  ames-state
        ?:  ?=([~ %known *] +.ship-state)
          =<  ev-abet
          %.  plea
          ev-req-plea:(ev-foco:ev-core ship +.u.ship-state)
        ::
        =<  al-abet
        %^  al-enqueue-alien-todo:al-core  ship  +.ship-state
        |=  todos=ovni-state:ev-core
        todos(pokes [[hen^plea/plea] pokes.todos])
      moves^vane-gate
    ::
    ++  pe-keen
      |=  [sec=(unit [idx=@ key=@]) spar:^ames]
      =/  ship-state  (pe-find-peer ship)
      ?:  ?=(%ames -.ship-state)
        (call:am-core hen ~ soft+keen/sec^ship^path)
      =^  moves  ames-state
        ?:  ?=([~ %known *] +.ship-state)
          =<  ev-abet
          =*  sat     +.u.ship-state
          =/  =space  ?~(sec publ/life.sat shut/[idx key]:u.sec)
          %.  [space path]
          ev-req-peek:(ev-foco:ev-core ship sat)
        ::
        =<  al-abet
        :: XX: key exchange over ames forces all encrypted scries to be
        :: to a known peer
        ?>  ?=(~ sec)
        %^  al-enqueue-alien-todo:al-core  ship  +.ship-state
        |=  todos=ovni-state:ev-core
        todos(peeks (~(put ju peeks.todos) path hen))
      moves^vane-gate
    ::
    ++  pe-chum
      |=  spar:^ames
      =/  ship-state  (pe-find-peer ship)
      ?:  ?=(%ames -.ship-state)
        (call:am-core hen ~ soft+chum/ship^path)
      =^  moves  ames-state
        ?:  ?=([~ %known *] +.ship-state)
          =+  ev-core=(ev-foco:ev-core ship +.u.ship-state)
          =<  ev-abet
          (ev-req-peek:ev-core chum-to-our:ev-core path)
        ::
        =<  al-abet
        %^  al-enqueue-alien-todo:al-core  ship  +.ship-state
        |=  todos=ovni-state:ev-core
        todos(chums (~(put ju chums.todos) path hen))
      moves^vane-gate
    ::
    ++  pe-cancel
      |=  [all=? =spar]
      =/  ship-state  (pe-find-peer ship.spar)
      ::
      ?:  ?=(%ames -.ship-state)
        (call:am-core hen ~ %soft ?:(all %wham %yawn) spar)
      =^  moves  ames-state
        =<  ev-abet
        ?.  ?=([~ %known *] +.ship-state)
          ::  XX delete from alien agenda?
          ~&("peer still alien, skip peek cancel" ev-core)
        %.  [all path.spar]
       ev-cancel-peek:(ev-foco:ev-core ship.spar +.u.ship-state)
      moves^vane-gate
    ::
    ++  pe-hear
      |=  [dud=(unit goof) =lane =blob]
      ^-  [(list move) _vane-gate]
      =/  =shot  (sift-shot blob)
      ::
      ?:  .=  =(%deny form.snub.ames-state)
          (~(has in ships.snub.ames-state) sndr.shot)
        %-  %+  %*(ev-tace ev-core her sndr.shot)  rcv.veb.bug.ames-state
            |.("snubbed")
        `vane-gate
      ::
      =/  ship-state  (pe-find-peer sndr.shot)
      ?:  ?=([%ames *] ship-state)
        ::  both for %ames and %fine
        ::
        (call:am-core hen dud %soft %hear lane blob)
      ?.  ?=([~ %known *] +.ship-state)
        ::
        %-  %+  %*(ev-tace ev-core her sndr.shot)  odd.veb.bug.ames-state
            |.("hear ames packet for migrated (alien) peer; ignore")
        ::
        `vane-gate
      ::  old response, no-op. If we can find the peer in chums, it means that
      ::  they sent an %ahoy plea, we migrated them, but they haven't heard our
      ::  %ack, and have not migrated us.
      ::
      ::  XX  TODO: check if we are in fact tracking this path
      ::  XX  (necessary?)
      ::
      :: =/  [=peep =meow]  (sift-purr `@ux`content.shot)
      :: =/  =path  (slag 3 path.peep)
      ::
      ::  any %fine requests should have been migrated and responses should
      ::  only come via %heer or %mess. if %ames, we no-op and the %sender will
      ::  resend the message as soon as they migrate us.
      ::
      %-  %+  %*(ev-tace ev-core her sndr.shot)  odd.veb.bug.ames-state
          |.("hear ames packet for migrated peer")
      ::
      =^  moves  ames-state
        =<  abet
        %.(shot on-ack-ahoy:(ev:(ames now eny rof) now^eny^rof hen ames-state))
      [moves vane-gate]
    ::
    +|  %mesa-tasks
    ::
    ++  pe-heer
      |=  [dud=(unit goof) =lane:pact blob=@]
      ::
      =+  ?~  dud  ~
          %.  ~
          %+  slog  leaf+"mesa: packet crashed {<mote.u.dud>}"
          ::  XX what if the crash is due to path validation
          ::  and we can't infer the sequence number?
          ?.  =+  [msg rcv]:veb.bug.ames-state  |(-< ->)  ~
          tang.u.dud
      ::
      =/  =pact:pact  (parse-packet blob)  :: XX handle crash here?
      =^  moves  ames-state
        ?-    +<.pact
            %page
          =*  her  her.name.pact
          =/  chum-state  (pe-find-peer her)
          ?.  ?=([%mesa *] chum-state)
            %-  %+  %*(ev-tace ev-core her her)  odd.veb.bug.ames-state
                |.("hear page for regressed chum")
            `ames-state
          ?.  ?=([~ %known *] +.chum-state)
            ::  if alien this can only be a comet attestation proof
            ::
            al-abet:(al-take-proof:al-core lane hop.pact +>.pact)
          =/  =fren-state  +.u.+.chum-state
          =<  ev-abet
          %.  [dud lane hop.pact %page +>.pact]
          hear-page:ev-pact:(ev-foco:ev-core her.name.pact fren-state)
        ::
            %peek
          ?~  dud
            ev-abet:(hear-peek:ev-pact:ev-core +>.pact)
          sy-abet:(~(sy-crud sy:me-core hen) %peek tang.u.dud)
        ::
            %poke
          =*  data     data.pact
          =*  our-ack  her.ack.pact
          =*  her-pok  her.pok.pact
          ::
          ?:  .=  =(%deny form.snub.ames-state)
              (~(has in ships.snub.ames-state) her-pok)
            %-  %+  %*(ev-tace ev-core her her-pok)  rcv.veb.bug.ames-state
                |.("snubbed")
            `ames-state
          =/  chum-state  (pe-find-peer her-pok)
          ?:  ?=([%ames ~ %known *] chum-state)
            ::  peer has been regressed to %ames (or XX?)
            ::
            ?.  =(1 (div (add tob.data.pact 1.023) 1.024))
              ::  only deal with single-fragment %rege pleas
              ::
              `ames-state
            ::
            %-  %+  %*(ev-tace ev-core her her-pok)  odd.veb.bug.ames-state
                |.("hear poke for regressed")
            ::
            =/  =peer-state     +.u.chum-state
            =/  =azimuth-state  -.peer-state
            =+  ames-core=(ev:ames now^eny^rof ~[//rege] ames-state)
            =+  peer-core=(abed-peer:pe:ames-core her-pok peer-state)
            =|  per=fren-state
            =.  -.per  azimuth-state
            =/  ev-core
              %*  ev-core  (ev-abed:ev:(mesa now eny rof) hen)
                chums.ames-state  (~(put by chums.ames-state) her-pok known/per)
              ==
            ::  XX refactor; same as hear-poke:ev-pact:ev:mesa
            ::
            =/  [=space cyf=(unit @) =inner-poke=path]
              ~|  inner-path/[pat.ack^pat.pok]:pact
              (ev-decrypt-path:ev-core [pat her]:pok.pact)
            ::
            =/  [pok=(pole iota) ack=(pole iota)]
              ::  path validation/decryption
              ::
              :-  (validate-path inner-poke-path)
              %-  validate-path
              inner:(ev-decrypt-path:ev-core [pat.ack her.pok]:pact)
            ::
            ?>  &(?=(flow-pith ack) ?=(flow-pith pok))
            ?.  =(our-ack our)  ::  do we need to respond to this ack?
              %-  %+  %*(ev-tace ev-core her her-pok)  odd.veb.bug.ames-state
                  |.("not our ack rcvr={<our-ack>}; skip")
              `ames-state
            ?.  =(rcvr.pok our)  ::  are we the receiver of the poke?
              %-  %+  %*(ev-tace ev-core her her-pok)  odd.veb.bug.ames-state
                  |.("poke for {<rcvr.pok>} not us  ; skip")
              `ames-state
            ?.  =(her-pok rcvr.ack)  ::  do ack and pokes match?
              %-  %+  %*(ev-tace ev-core her her-pok)  odd.veb.bug.ames-state
                  |.("ack {<rcvr.ack>} and poke {<her-pok>} missmatch; skip")
              `ames-state
            ::  authenticate one-fragment message
            ::
            ?>  %-  authenticate:ev-core
                [(root:lss (met 3 dat.data)^dat.data) aut.data pok.pact]
            =+  ;;  =gage:mess
                    (cue (ev-decrypt-spac:ev-core space dat.data cyf))
            ?.  ?=([%message mark *] gage)
              %-  %+  %*(ev-tace ev-core her her-pok)  odd.veb.bug.ames-state
                  |.("no op; weird %message gage {<-.gage>}")
              `ames-state
            ?:  ?=(%boon +<.gage)
              %-  %+  %*(ev-tace ev-core her her-pok)  odd.veb.bug.ames-state
                  |.("no op; ignore %boon")
              `ames-state
            =+  ;;([%plea =plea] +.gage)
            ?.  ?=([%$ path=[%ames ~] payload=[%back ~]] plea)
              %-  %+  %*(ev-tace ev-core her her-pok)  odd.veb.bug.ames-state
                  |.("no op; ignore {(spud path.plea)} plea")
              `ames-state  :: XX ignore non %rege plea
            ::  produce mesa ack
            ::
            %-  %+  %*(ev-tace ev-core her her-pok)  snd.veb.bug.ames-state
                |.("ack %rege plea")
            ::  XX check that we have the ack in peers.ames-state?
            ::
            =/  moves=(list move)
              ::  create temporary flow for ack payload
              ::
              =.  chums.ames-state.me-core
                =.  flows.per
                  =|  state=flow-state
                  %-  ~(put by flows.per)
                  [[bone dire]:ack state(last-acked.rcv mess.pok)]
                (~(put by chums.ames-state) her-pok known/per)
              =/  flow-roof
                ^-  roof
                |=  [lyc=gang pov=path vis=view bem=beam]
                ^-  (unit (unit cage))
                ?:  =(s.bem (pout ack))
                   (peek-flow:me-core (pout ack))
                (rof lyc pov vis bem)
              ::
              =<  moves
              %.  [space=[%none ~] spar=[her-pok pat.ack.pact]]
              co-make-page:co:me-core(rof flow-roof)
            [moves ames-state]
          =?  chum-state  ?=([%ames *] chum-state)
            [%mesa *(unit ^chum-state)]
          ?>  ?=([%mesa *] chum-state)
          ?.  ?&  ?=(%pawn (clan:title her-pok))
                  |(?=(~ +.chum-state) ?=([~ %alien *] +.chum-state))
              ==
            ?.  ?=([~ %known *] +.chum-state)
              ::  request keys from %jael; drop the packet, it'll be re-send
              ::
              =<  al-abet
              %-  al-enqueue-alien-todo:al-core
              [her-pok +.chum-state |=(ovni-state +<)]
            =/  fren=fren-state  +.u.+.chum-state
            =<  ev-abet
            %.  [dud lane hop.pact %poke +>.pact]
            hear-poke:ev-pact:(ev-foco:ev-core her-pok fren)
          =?  chums.ames-state  ?=(~ +.chum-state)
            ::  first time: upgrade to %alien and +peek attestation proof
            ::
            (~(put by chums.ames-state) her-pok alien/*ovni-state)
          ::  XX only peek if chum-state was ~?
          ::  still waiting to hear attestation proof; no-op
          ::
          :: `ames-state
          ::
          %-  %+  %*(ev-tace ev-core her her-pok)  fin.veb.bug.ames-state
              |.("peek for comet attestation")
          ::
          al-abet:(al-read-proof:al-core her-pok lane)
        ::
        ==
      moves^vane-gate
    ::
    ++  pe-mess  :: XX refactor
      |=  [dud=(unit goof) =mess]
      ::  XX %mess is never called directly, only from the packet layer
      ::  so any crash there will be a crash while handling a %heer
      ::
      =^  moves  ames-state
        =<  ev-abet
        ?-    -.mess
            %poke
          ?.  =(our ship.p.mess)
            ev-core
          ?~  chum=(~(get by chums.ames-state) ship.p.q.mess)
            ev-core
          ::  XX  this assumes that %aliens are checked in the packet layer
          ?>  ?=([~ %known *] chum)  ::  XX alien agenda?
          %.  [dud +.mess]
          hear-poke:ev-mess:(ev-foco:ev-core ship.p.q.mess +.u.chum)
        ::
            %peek
          ?~  chum=(~(get by chums.ames-state) ship.mess)
            ev-core
          ::  XX  this assumes that %aliens are checked in the packet layer
          ?>  ?=([~ %known *] chum)  ::  XX alien agenda?
          %.  +.mess
          hear-peek:ev-mess:(ev-foco:ev-core ship.mess +.u.chum)
        ::
            %page
          ?~  chum=(~(get by chums.ames-state) ship.p.mess)
            ev-core
          ::  XX  this assumes that %aliens are checked in the packet layer
          ?>  ?=([~ %known *] chum)  ::  XX alien agenda?
          %.  +.mess
          hear-page:ev-mess:(ev-foco:ev-core ship.p.mess +.u.chum)
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
  =*  sample  +<
  =+  me-core=(mesa now eny rof)
  =+  am-core=(ames now eny rof)
  =/  =task  ((harden task) wrapped-task)
  ?:  &(?=(~ unix-duct) ?=(?(%hear %heer %mess) -.task))
    ::  drop incoming packets until we get a %born
    ::
    ::    this also prevents %nail gifts in the following scenarios:
    ::      - on-hear-open/on-hear-shut for new routes
    ::      -  ...?
    `vane-gate
  ?-    -.task
    ::  %ames-only tasks
    ::
      ?(%kroc %deep %mate %stir)
    ::  XX can we call the wrong core? still check if ship has migrated?
    ::
    (call:am-core sample)
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
      ?(%meek %moke %mage %rege)
    (call:me-core sample)
    ::  flow-independent tasks
    ::
      ?(%vega %init %born %snub %spew %stun %sift %plug %dear %init %tame %cong)
    (call:me-core sample)
    ::  common tasks
    ::
      ?(%plea %cork %keen %chum %yawn %wham %load)
    (~(call pe-core hen) dud task)
    ::  core-dependent tasks
    ::
      ?(%prod %trim)
    =^  ames-moves  vane-gate  (call:am-core sample)
    =^  mesa-moves  vane-gate
      (call:me-core(ames-state ames-state.vane-gate) sample)
    [(weld ames-moves mesa-moves) vane-gate]
  ::
  ==
::
++  take
  |=  [=wire =duct dud=(unit goof) =sign]
  ^-  [(list move) _vane-gate]
  =*  sample  +<
  =+  me-core=(mesa now eny rof)
  =+  am-core=(ames now eny rof)
  ?^  dud
    ~|(%ames-take-dud (mean tang.u.dud))
  ::
  ?:  ?=([?(%turf %mesa %private-keys %public-keys) *] wire)
    ?.  ?&  ?=(?(%turf %public-keys) -.wire)
            ?=(~ unix-duct)
        ==
      ?~  flow-wire=(ev-parse-flow-wire:ev:me-core wire)
        (take:me-core sample)
      %.  sample
      ?:  =(%mesa -:(pe-find-peer her.u.flow-wire))
        take:me-core
      ::  XX this shouldn't happen. /mesa wires are used for peeking poke
      ::  payloads, naxplanations and corks. if the peer has been regressed, all
      ::  those peeks are dropped and the regression logic should guarantee that
      ::  whatever state is pending gets handled now using the |ames core.
      ::
      ~>  %slog.0^leaf/"mesa: taking weird {<[[- +<]:sign]>} for {(spud wire)}"
      take:me-core
    ::  If the unix-duct is not set, we defer applying %public-keys and %turf
    ::  gifts (which can trigger other gifts to be sent to unix) by setting up
    ::  a timer that will request them again
    ::
    =;  wires=(list ^wire)
      :_  vane-gate
      %+  turn  wires
      |=  =^wire
      ^-  move
      [~[/ames] %pass wire %b %wait `@da`(add now ~s30)]
    ?:  ?=(%turf -.wire)
      ~>  %slog.0^leaf/"ames: unix-duct missing; delay %turf"
      [%mesa %ask wire]~
    ?>  ?=([%jael %public-keys *] sign)
    =/  gift=public-keys-result:jael  +>.sign
    ?.  ?=(%full -.gift)
      ~&(unexpected-ask-gift/-.gift ~)
    =/  ships=(set ship)  ~(key by points.gift)
    %-  ~(rep in ships)
    |=  [=ship wires=(list ^wire)]
    ~>  %slog.0^leaf/"ames: unix-duct missing; delay {<i.wire>} for {<ship>}"
    :_  wires
    [%mesa %ask /public-keys/[(scot %p ship)]]
  ?~  parsed-wire=(parse-bone-wire wire)
    ::  not a /bone wire—used when passing %pleas to a local vane; use |ames
    ::  XX this is not a |mesa wire so it shouldn't happen for migrated flows
    ::
    (take:am-core sample)
  =/  ship-state  (pe-find-peer her.u.parsed-wire)
  %.  sample
  ?:  ?=(%ames -.ship-state)
    take:am-core
  take:me-core
::  +stay: extract state before reload
::
++  stay  [%23 adult/ames-state]
::  +load: load in old state after reload
::
++  load
  |=  state=axle
  :: =.  peers.state   (~(del by peers.state) ~fyr)
  :: =.  chums.state    ~
    :: %-  ~(run by chums.state)
    :: |=  =chum-state
    :: ?:  ?=(%alien -.chum-state)
    ::   ~&  %cleaning-alien
    ::   chum-state
    ::   :: chum-state(pit ~)
    :: %_  chum-state
    ::   flows    ~&  %cleaning-flows  ~
    ::   pit      ~&  %cleaning-pit  ~
    ::   corked   ~
    ::   ossuary  =|  =ossuary  ossuary
    ::           ::  %_  ossuary
    ::             :: next-bone  40
    :: ==        :: ==
  vane-gate(ames-state state)
::  +scry: dereference namespace
::
++  scry
  ^-  roon
  |=  [lyc=gang pov=path car=term bem=beam]
  =*  sample  +<
  =+  me-core=(mesa now eny rof)
  =+  am-core=(ames now eny rof)
  ?:  ?&  =(our p.bem)
          =(%$ q.bem)
          =([%ud 1] r.bem)
          =(%x car)
      ==
    =/  tyl=(pole knot)  s.bem
    ?.  =(~ lyc)
      ::  private, message-level namespaces
      ::
      ?.(?=(?(%flow %cork %whey %meta) -.tyl) ~ (scry:me-core sample))
    ?+    -.tyl  ~
        %fine                                   (scry:am-core sample)
        ?(%mess %publ %chum %shut %veri %pawn)  (scry:me-core sample)
      ::
        %chum
      ?+  +.tyl  ~
        [our=@ lyf=@ cyf=@ ~]                   (scry:am-core sample)
        [lyf=@ her=@ hyf=@ cyf=@ ~]             (scry:me-core sample)
      ==
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
    (scry:am-core sample)
  ::  private endpoints
  ::
  ?.  =([~ ~] lyc)  ~
  ?+    tyl       (scry:am-core sample)            ::  |ames scry endpoints
      [?(%chums %ahoyed) *]  (scry:me-core sample) ::  |mesa scry endpoints
  ::
      [?(%closing %corked %bones %snd-bones) her=@ *]
    =/  who  (slaw %p her.tyl)
    ?~  who  [~ ~]
    =/  wer  -:(pe-find-peer u.who)
    %.(sample ?:(?=(%ames wer) scry:am-core scry:me-core))
  ==
::
--
