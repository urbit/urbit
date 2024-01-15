!:
=,  ames
=/  packet-size  13
::  helper core
::
=>  |%
    +|  %helpers
    ::
    ++  chain
      =<  mop
      |%
      ++  on   ((^on ,@ ,[key=@ =path]) lte)
      +$  mop  ^chain
      --
    ::
    ++  parse-inner-path
      |=  [our=ship p=path]
      ^-  (unit [[@tas @tas] beam])
      ?.  ?=([@ @ @ @ *] p)  ~
      ?~  des=?~(i.t.t.p (some %$) (slaw %tas i.t.t.p))
        ~
      ?~  ved=(de-case i.t.t.t.p)  ~
      `[[i.p i.t.p] [[our u.des u.ved] t.t.t.t.p]]
    ::
    ++  get-key-for
      |=  [who=ship life=@]
      0x0
    ::
    ++  decrypt
      |=  [p=@t key=@]
      ^-  (unit path)
      (rush `@t`(dy:crub:crypto key (slav %uv p)) stap)
    ::
    ++  check-fine-key
      |=  [c=chain:ames =balk key-idx=@]
      ^-  ?
      ?~  link=(get:on:chain c key-idx)
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
    ++  derive-symmetric-key
      ~/  %derive-symmetric-key
      |=  [public-key=@uw private-key=@uw]
      ^-  symmetric-key
      ::
      ?>  =('b' (end 3 public-key))
      =.  public-key  (rsh 8 (rsh 3 public-key))
      ::
      ?>  =('B' (end 3 private-key))
      =.  private-key  (rsh 8 (rsh 3 private-key))
      ::
      `@`(shar:ed:crypto public-key private-key)
        ::
    +|  %types
    ::
    +$  veb-all-off
      $:  snd=_`?`%.n  ::  sending packets
          rcv=_`?`%.n  ::  receiving packets
          odd=_`?`%.n  ::  unusual events
          msg=_`?`%.n  ::  message-level events
          ges=_`?`%.n  ::  congestion control
          for=_`?`%.n  ::  packet forwarding
          rot=_`?`%.n  ::  routing attempts
          kay=_`?`%.n  ::  is ok/not responding
          fin=_`?`%.n  ::  remote-scry
      ==
    +$  bug  [veb=veb-all-off ships=(set ship)]
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
    ::
    +$  lane  $@  @ux
              $%  [%if p=@ifF q=@udE]
                  [%is p=@isH q=@udE]
              ==
    ::  $move: output effect; either request (to other vane) or response
    ::
    +$  move  [=duct card=(wind note gift)]
    ::  $note: request to other vane
    ::
    ::    Ames passes a %plea note to another vane when it receives a
    ::    message on a "forward flow" from a peer, originally passed from
    ::    one of the peer's vanes to the peer's Ames.
    ::
    ::    Ames passes a %deep task to itself to handle deferred calls
    ::    Ames passes a %private-keys to Jael to request our private keys.
    ::    Ames passes a %public-keys to Jael to request a peer's public keys.
    ::
    +$  note
      $~  [%b %wait *@da]
      $%  $:  %a
              $>(?(%deep %keen) task:ames)
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
              $>(%deal task:gall)
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
    +$  gift
    ::+$  client-gift  :: gifts emitted when sending requests/responses
      $%  ::gift:ames
          [%send =lane =blob]                  :: old packet send
          [%push lanes=(list lane) =blob]      :: new packet send
          [%pres lanes=(list lane) message=*]  :: new message send
    ::  ==
    ::+$  publisher-gift  :: gifts emitted when hearing requests
      ::$%
          [%boon message=*]                    :: assembled %boon
          [%done error=(unit error)]           :: ack to client vane
      ==
    +$  mesa-message
      $%  [%plea plea]       ::  client vane remote request
          [%boon payload=*]  ::  %facts, subscription updates
          [%cork ~]          ::  client vane is done, close flow
      ==
    +$  message-task
      $%  [%poke =bone message=mesa-message]
          [%sink =bone =pact]  ::  message=mess
      ==
    ::
    +$  sig    @uxJ    :: (ed25519)
    +$  hmac   @uxI   :: (keyed blake3)
    +$  proof  (list @uxI)
    +$  root  @uxI
    +$  once  [tot=@udF tag=?(sig hmac) aut=?(root proof) dat=@]
    +$  more  [aut=$@(~ [@uxI @uxI]) dat=@]
    +$  spac  ?(%pact %publ %mess %chum %shut)
    +$  name  [p=ship q=path r=bloq s=num=@udF]
    +$  data  [tot=@udF aut=@ux dat=@]
    +$  next  (list lane)
    +$  pact
      $%  [%page p=name q=data r=next]  :: [%page p=name q=(each once more) r=next]
          [%peek p=name]
          [%poke p=name q=name r=data]  :: [%poke p=name q=name r=once]
      ==
    ::
    +$  mesa-state
      $:  peers=(map ship ship-state)
          =unix=duct
          =life
          =rift
          crypto-core=acru:ames
          =bug
          snub=[form=?(%allow %deny) ships=(set ship)]
          cong=[msg=_5 mem=_100.000]
          =chain
        ==
    --
::
::  vane gate
::
|=  our=ship
::  |ev: inner event-handling core
::
=>  :: ~%  %per-event  ..chain  ~
    |%
    ++  ev
      =|  moves=(list move)
      ~%  %event-gate  ..ev  ~
      |=  [[now=@da eny=@ rof=roof] =duct =mesa-state]
      ~%  %ev-core  ..$  ~
      |%
      +|  %helpers
      ::
      ++  ev-core  .
      ++  ev-abet  [(flop moves) mesa-state]
      ++  ev-emit  |=(=move ev-core(moves [move moves]))
      ++  ev-emil  |=(mos=(list move) ev-core(moves (weld (flop mos) moves)))
      ++  ev-get-parties
        |=  =pact  ::  =mess
        ^-  [sndr=(unit @p) rcvr=@p]
        ?-  -.pact
          %peek  [~ p.p.pact]
          %page  [~ p.p.pact]
          %poke  [`p.q.pact p.p.pact]
        ==
      ::
      ++  ev-is-flow-path
        |=  path=(pole knot)
        ^-  ?
        ?=([sndr=@ vane=%$ rcvr=@ %flow bone=@ message=@] path)
      ::
      ++  ev-get-bone
        |=  =pact
        |^  ^-  bone
        ?:  ?=(%poke -.pact)
          =/  =name  q.pact
          (parse-path `(pole knot)`q.name)
        :: ?>  ?=(?(%page %peek) -.pact)
        *bone  :: XX
        ::=/  =name  p.pact
        ::(parse-path `(pole knot)`q.name)
        ::
        ++  parse-path
          |=  path=(pole knot)  ::  /~zod//~nec/flow/bone=0/message=0
          ^-  bone
          ?>(?=([sndr=@ vane=%$ rcvr=@ %flow bone=@ message=@] path) (rash bone.path dem))
        --
      ::
      +|  %tasks
      ::
      ++  on-poke
        |=  [=ship =plea]
        ^+  ev-core
        =/  ship-state  (~(get by peers.mesa-state) ship)
        ::
        ?.  ?=([~ %known *] ship-state)
          ::  XX handle
          !!
        ::
        =+  peer-core=(pe-abed-peer:pe ship +.u.ship-state)
        ::
        =^  =bone  peer-core  (pe-bind-duct:peer-core duct)
        ::  XX handle corked/closing bones
        ::
        ~&  [%poke bone %plea plea]
        pe-abet:(pe-call:peer-core %mess %poke bone %plea plea)  ::  XX namespace
      ::
      ++  on-sink
        ::XX  lane:mesa         (each blob mess)
        |=  [=lane:ames =pact]  ::  XX lane:mesa XX goof?
        ^+  ev-core
        ::  XX fake blob decoding  =+  ;;  =pact  (cue blob)
        :: ?.  ?=(%| -.request)  ev-core
        :: =/  =pact  +.request
        ?-  -.pact
          %peek  !!
          %page  !!  ::  data (from %peek, or %poke/%boon payload) or ack
        ::
            %poke
          ::
          ::  XX  fake decoding
          :: /~nec/ack/~zod/flow/0/1/1
          :: /~zod/poke/~nec/flow/0/1/1
          =/  [sndr=(unit @p) rcvr=@p]  (ev-get-parties pact)  ::  (pe-get-parties mess)
          ?.  =(rcvr our)
            ev-core  ::  XX no-op?
          =/  ship-state  (~(get by peers.mesa-state) (need sndr))
          ?.  ?=([~ %known *] ship-state)
            ::  XX handle
            !!
          ::
          =+  peer-core=(pe-abed-peer:pe (need sndr) +.u.ship-state)
          =/  =bone  (ev-get-bone pact)
          pe-abet:(pe-call:peer-core %mess %sink bone %poke +.pact) ::  pact -> mess
        ==
      ::
      +|  %internals
      ::
      ::  +pe: per-peer processing
      ::
      ++  pe
        |_  [=peer-state =channel]
        +*  veb    veb.bug.channel
            her    her.channel
            keens  keens.peer-state
        ::
        +|  %helpers
        ++  pe-core      .
        ++  pe-emit      |=(move pe-core(ev-core (ev-emit +<)))
        ++  pe-abed      |=(=ship (pe-abed-peer ship (pe-gut-peer-state ship)))
        ++  pe-abed-got  |=(=ship (pe-abed-peer ship (pe-got-peer-state ship)))
        ++  pe-abed-peer
          |=  [=ship peer=^peer-state]
          %_  pe-core
            peer-state  peer
               channel  [[our ship] now pe-channel -.peer]
          ==
        ::
        ++  pe-channel  [life crypto-core bug]:mesa-state
        ++  pe-abort  ev-core  :: keeps moves, discards state changes
        ++  pe-abet
          ^+  ev-core
          =.  peers.mesa-state  (~(put by peers.mesa-state) her known/peer-state)
          ev-core
        ::  +get-peer-state: lookup .her state or ~
        ::
        ++  pe-get-peer-state
          |=  her=ship
          ^-  (unit ^peer-state)
          ::
          =-  ?.(?=([~ %known *] -) ~ `+.u)
          (~(get by peers.mesa-state) her)
        ::  +got-peer-state: lookup .her state or crash
        ::
        ++  pe-got-peer-state
          |=  her=ship
          ^+  peer-state
          ::
          ~|  %freaky-alien^her
          =-  ?>(?=(%known -<) ->)
          (~(got by peers.mesa-state) her)
        ::  +gut-peer-state: lookup .her state or default
        ::
        ++  pe-gut-peer-state
          |=  her=ship
          ^+  peer-state
          =/  ship-state  (~(get by peers.mesa-state) her)
          ?.  ?=([~ %known *] ship-state)
            *^peer-state
          +.u.ship-state
        ::
        :: ++  pe-trace
        ::   |=  [verb=? print=(trap tape)]
        ::   ^+  same
        ::   (ev-trace verb her print)
        ::  +pe-got-duct: look up $duct by .bone, asserting already bound
        ::
        ++  pe-got-duct
          |=  b=bone
          ^-  ^duct
          ~|(%dangling-bone^her^b (~(got by by-bone.ossuary.peer-state) b))
        ::
        ::  +pe-bind-duct: find or make new $bone for .duct in .ossuary
        ::
        ++  pe-bind-duct
          |=  =^duct
          =*  ossa  ossuary.peer-state
          ^+  [next-bone.ossa pe-core]
          ?^  existing=(~(get by by-duct.ossa) duct)
            [u.existing pe-core]
          :-  next-bone.ossa
          =.  ossa
            :+  (add 4 next-bone.ossa)  ::  XX  4 bones needed per flow?
              (~(put by by-duct.ossa) duct next-bone.ossa)
            (~(put by by-bone.ossa) next-bone.ossa duct)
          pe-core
        ::
        +|  %tasks
        ::
        ++  pe-call
          |=  [=spac task=message-task]  ::  XX any namespace task?
          ^+  pe-core
          ?-  -.task
            %poke  fo-abet:(fo-call:(fo-abed:fo bone.task) spac message.task)
            %sink  fo-abet:(fo-call:(fo-abed:fo bone.task) spac sink/pact.task)
          ==
        ::
        +|  %internals
        ::
        ++  fo  ::  ++flow (fo) ||  ++bone (bo)
          =>  |%
              :: XX to lull.hoon,
              ::  - part of peer-state => (map bone poke-state)
              ::  - reshape message-pump/sink states into poke-states
              ::     using the bone to know the directtion of the flow ?
              ::     (bone numbers as see from the point of view of "our")
              ::
              ::       bone 0: sub (our) -> pub (her)  :: %plea: %poke, %watch, ...
              ::       bone 1: sub (her) <- pub (our)  :: %plea: %boon
              ::       bone 2: sub (our) <- pub (her)  :: %nacks
              ::
              ::       bone 3: sub (her) <- pub (our)  :: orphaned; naxplanations are read via %peek
              ::
              ::  XX -- TODO simplify packet-state for each message (next=@ud)
              ::
              ::  - packet-pump-state in message-pump gone; moved ouf ot arvo into vere
              ::    - collapsed into the message level since we only "send" one packet fragment
              ::    - XX only next-wake for backing off resends of expired packets
              ::  - message-sink-state only deals with messages, removes .partial-rcv-message
              ::
              ::  - packet state is done per message by the |pu core
              ::
              +$  new-peer-state  ::  (map her=ship peer-state)
                $:
                    pokes=(map bone poke-state)  ::  sender+receiver
                    ::
                      pokes=(map bone poke-sender-state)
                      coups=(map bone poke-receiver-state)
                    ::
                ==
              ::
              +$  peeks  (map path bone)  :: XX not needed
                                          :: poke datums for our requests
                                          :: removed when the ack is received
              +$  acks   (map path bone)  :: XX not needed
                                          :: acks to responses we give
                                          :: kept for current-10 messages
                                          :: when ack for current is heard, current-10 is removed
                                          ::  XX how to distinguinsh between first time?
                                          :: path contains the message/fragment number
              ::
              +$  poke-sender-state  :: per-bone, XX distinguish between req/resp bone?
                $:  current=_`message-num`1
                    next=_`message-num`1
                    unsent-messages=(qeu pact)
                    $+  queued-message-acks
                    queued-message-acks=(map message-num ack)
                ==
              +$  poke-receiver-state
                $:  last-acked=message-num
                    last-heard=message-num
                    pending-vane-ack=(qeu [=message-num message=*])
                    nax=(set message-num)
                ==
              ::
              +$  poke-state  ::  XX
                $:  poke-sender-state
                    poke-receiver-state
                ==
              ::
              +$  message-sign
                $%  [%sign ~]  ::  hear (n)ack for %poke, can trigger %peek for naxplanation
                    [%xxxx ~]
                ==
              --
          ::
          |_  [=bone state=poke-state]
          +|  %helpers
          ++  fo-core  .
          ++  fo-abed  |=(=^bone fo-core(state *poke-state))
          ++  fo-abet  pe-core
          ++  fo-en-plea
            ::  XX real encoding
            |=(=plea ^-(blob (jam plea)))
          ::  +fo-en-gift: encode gift to send based on namespacce
          ::
          ++  fo-en-gift
            |=  [=spac payload=(each blob message=*)]
            ^-  gift  ::  client-gift
            =|  lanes=(list lane)  :: XX
            ?-    spac  :: XX old packet format [%send =lane =blob]
                ?(%chum %shut %publ %mess)
              [%pres lanes ?>(?=(%| -.payload) +.payload)]
            ::
                %pact
              [%push lanes ?>(?=(%& -.payload) +.payload)]
            ==
          ::  +fo-en-spac: encode namespace paths
          ::
          ++  fo-en-spac
            |=  [=spac =path]
            ^+  path
            =/  [her=@ta rift=@ta]
              [(scot %p her) (scot %ud rift.peer-state)]
            ~&  her^rift
            ?-  spac
              %chum  !!
              %shut  !!
              %publ  !!
              %pact  !!
              %mess  [%ax her %$ ~.1 %mess rift path]
            ==
          ::  XX FIXME +encs: path encodings
          ::
          ++  encs
            |%  ++  her   =-  ~&(- -)  (scot %p her)
                ++  our   =-  ~&(- -)  (scot %p our)
                ++  bone  =-  ~&(- -)  (scot %ud bone)
                ++  mess  =-  ~&(- -)  |=(mess=@ud (scot %ud mess))
            --
          ::
          +|  %entry-points
          ::
          ++  fo-call
            =>  |%  +$  poke-task
                      $%  [%sink mess=pact]  ::  XX  [%sink =mess]
                          ::  XX remove %naxplanation from lull
                          mesa-message
                      ==
                --
            ::
            |=  [=spac poke=poke-task]
            ^+  fo-core
            ::
            ?-  -.poke
              ::  requests
              ::
              %plea  (fo-plea spac +.poke)  ::  send %plea request
              %boon  (fo-boon spac +.poke)  ::  send %boon request
              %cork  fo-cork
              ::  XX responses: (n)acks, pact/mess
              ::
              %sink  (fo-sink spac +.poke)  ::  unix responses (XX %hear?)
            ==
          ::
          ++  fo-take
            |=  sign=message-sign
            ^+  fo-core
            ::
            ?+  -.sign  !!
              %sign  fo-hear-sign
            ==
          ::
          +|  %tasks
          ::
          ++  fo-sink
            |=  [=spac =pact]
            ?+  -.pact  !!
              %poke  (fo-sink-poke spac [p q r]:pact)
            ==
          ::
          ++  fo-sink-poke
            |=  [=spac ack=name flow=name payload=data]
            ^+  fo-core
            ::  receiver of a %poke request
            ::  XX  parse path to get: requester, rift, bone, message
            ::
            ?.  =(1 tot.payload)
              !!  ::  XX  need to retrieve other fragments
            ::
            ::  XX parse `path`q.q.poke and check the bone to know if plea or boon
            ::
            =+  ;;  =plea  %-  cue  dat.payload  ::  XX fake data decoding
            ::  XX  pass plea/boon to gall
            =+  [her=~nec spac=%mess bone=1 rift=1]
            :: add rift to avoid dangling bones
            ::
            =/  =wire  /[spac]/flow/[(scot %p her)]/[(scot %ud bone)]/[(scot %ud rift)]
            =.  ev-core
              ?:  =(vane.plea %$)
                ev-core  ::  XX handle pre-cork ships
                         ::  XX taken care before, when checking protocol version
              ?+  vane.plea  ~|  %ames-evil-vane^our^her^vane.plea  !!
                ?(%c %e %g %j)  (ev-emit duct %pass wire vane.plea plea/her^plea)
              ==
            ::  XX  wait for done from vane
            ::  XX  then, place ack in namespace,
            ::  XX  emit $page as an effect to vere to cache it
            ::  XX  wait for cork to clean the flow
            fo-core
          ::
          ++  fo-boon
            |=  [=spac payload=*]
            fo-core
          ::
          ++  fo-cork  !!
          ::  +poke-plea: XX
          ::
          ++  fo-plea
            |=  [=spac =plea]
            ^+  fo-core
            ::
            ::  /ax/[$ship]//1/pact/[rift]/[$bloq]/[$frag]/data/[..$path]
            ::  /ax/[$ship]//1/mess/[rift]/[..$path]
            ::  /ax/[$ship]//1/chum/[ship-life]/[who]/[who-life]/[encrypted-path]
            ::
            ::  $path = /[$her]/ack/[$our]/flow/[$flow/bone]/[$mess]/[$frag]
            ::          /[$our]/poke/[$her]/flow/[$flow/bone]/[$mess]/[$frag]
            ::  (e.g.)  /~nec/ack/~zod/flow/0/1/1
            ::          /~zod/poke/~nec/flow/0/1/1
            ::
            =,  -.state
            :: =+  her:encs
            =/  ack-path=path
              %+  fo-en-spac  spac
              :~  (scot %p her)  %ack  (scot %p our)  %flow
                  (scot %ud bone)  (scot %ud next)  '0'
              ==
            =/  payload-path=path
              %+  fo-en-spac  spac
              :~  (scot %p our)  %poke  (scot %p her)  %flow
                  (scot %ud bone)  (scot %ud next)  '0'
              ==
            =/  =pact
              :^  %poke
                name=[her ack-path packet-size s=num=0]
                name=[our payload-path packet-size s=num=0]
              data=[tot=1 aut=*@ux dat=(fo-en-plea plea)]
           ~&  >  pact/pact
           =?  ev-core  ?=(~ unsent-messages)
             %-  ev-emil
             :~  [unix-duct.mesa-state give/(fo-en-gift spac %| pact)]  :: XX
                 :^  duct
                    %pass
                  /[spac]/[(scot %p her)]/[(scot %ud bone)]/[(scot %ud rift.peer-state)]
                 [%b %wait `@da`(add now ~s30)]
             ==
           %_  fo-core
             unsent-messages.state  (~(put to unsent-messages) pact)
           ==
          ::
          ++  fo-hear-sign  !!
            ::  name=[p=ship q=path r=bloq s=num=@udF]
            :: |=  [%page =name q=(each once more) r=next]
            :: ::
            :: ::  check if path.name is one our pokes
            :: ::
            :: =^  next-pact  unsent-messages.state
            ::   (~(get to unsent-messages.state) pact)
            :: =.  fo-core
            ::   %-  emil
            ::   :~  [%magic-send next-pact]
            ::       make-next-packet-timer
            ::   ==
          ::
          :: +|  %internals
          ++  fi  !!  :: |fine core, sends %peeks and assembles responses
          ++  pu
            ::
            :: =>  |%
            ::     $+  packet-state  [~ ~]
            ::     --
            ::
            |_  [=message-blob state=*]
            ::  +|  helpers
            ++  pu-core  .
            ++  pu-abed  !!
            ++  pu-abet  !!
            ::  +|  entry-points
            ::  +|  tasks
            ::  +|  internals
            --
          --
        --
      --
    ::
    ++  ev-mess  !!
    ++  ev-pact  !!
    --
::
=|  =mesa-state
|=  [now=@da eny=@uvJ rof=roof]
=*  mesa-gate  .
|%
::  XX FIXME
::
+$  new-task
  $%  [%sink =lane:ames =pact]  :: XX only %mess namespace support
                                :: XX use =mess instead
      $<(%sink task)
  ==
::
++  call
  ::
  |=  [=duct dud=(unit goof) wrapped-task=(hobo new-task)]
  ^-  [(list move) _mesa-gate]
  =/  task=new-task    ((harden new-task) wrapped-task)
  =/  ev-core  (ev [now eny rof] duct mesa-state)
  ::
  =^  moves  mesa-state
    =<  ev-abet
    ::  XX  handle error notifications
    ::
    ?^  dud
      !!
    ::
    ?+  -.task  !!
      ::  XX rename
      ::  XX choose what namespace this belongs to
      %plea  (on-poke:ev-core [ship plea]:task)
      %sink  (on-sink:ev-core [lane pact]:task)  :: XX only %mess namespace support
    ==
    ::
  [moves mesa-gate]
::
++  take
  |=  [=wire =duct dud=(unit goof) sign=sign-arvo]
  ^-  [(list move) _mesa-gate]
  [~ mesa-gate]
::
++  stay  mesa-state
::
++  load
  |=  old=*
  mesa-gate
::
++  scry
  |=  [lyc=gang pov=path car=term bem=beam]
  ^-  (unit (unit cage))
  =,  mesa-state
  =*  ren  car
  =*  why=shop  &/p.bem
  =*  syd  q.bem
  =*  lot=coin  $/r.bem
  =*  tyl  s.bem
  ?:  ?&  =(&+our why)
          =([%ud 1] r.bem)
          =(%$ syd)
          =(%x ren)
      ==
    =>  .(tyl `(pole knot)`tyl)
    ?+    tyl  ~
    ::
        [%mess rift=@ path=^]
      ?.  =(rift (slav %ud rift.tyl))
        ~
      =/  bem  [[our %$ ud+1] path.tyl]
      =/  res  (rof ~ /ames/mess %ax bem)
      ?.  ?=([~ ~ %message *] res)
        :: TODO validate message
        ~
      res
    ::
        [%publ life=@ path=^]
      ?.  =(life (slav %ud life.tyl))
        ~
      =/  p=(unit [[@tas @tas] beam])  (parse-inner-path our path.tyl)
      ?~  p  ~
      =/  res  (rof ~ /ames/publ u.p)
      ?-  res
        ~        ~
        [~ ~]    ``[%message !>([0x0 0])] :: sign empty response
        [~ ~ *]  ``[%message !>([0x0 0])] :: sign response
      ==
    ::
        [%chum life=@ who=@ her-life=@ encrypted-path=@]
      ?.  =(life (slav %ud life.tyl))
        ~
      =/  key=@  (get-key-for (slav %p who.tyl) (slav %ud her-life.tyl))
      =/  pat=(unit path)  (decrypt encrypted-path.tyl key)
      ?~  pat  [~ ~]
      =/  p=(unit [[@tas @tas] beam])  (parse-inner-path our u.pat)
      ?~  p  ~
      =/  res  (rof `[(slav %p who.tyl) ~ ~] /ames/chum u.p)
      ?-  res
        ~        ~
        [~ ~]    ``[%message !>([0x0 0])] :: hmac empty response
        [~ ~ *]  ``[%message !>([0x0 0])] :: hmac response
      ==
    ::
        [%shut key-id=@ encrypted-path=@]
      =/  key-idx=@  (slav %ud key-id.tyl)
      =/  key  (got:on:^chain chain key-idx)
      =/  pat=(unit path)  (decrypt encrypted-path.tyl key.key)
      ?~  pat  [~ ~]
      ?~  blk=(de-part:balk our rift life u.pat)
        [~ ~]
      ?.  (check-fine-key chain u.blk key-idx)
        [~ ~]
      =/  res  (rof [~ ~] /ames/shut (as-omen:balk u.blk))
      ?-  res
        ~        ~
        [~ ~]    ``[%message !>([0x0 0])] :: hmac empty response
        [~ ~ *]  ``[%message !>([0x0 0])] :: hmac response
      ==
    ==
  ~
--
