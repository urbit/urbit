::  %fine, remote scries
!:
::
::TODO  jael does ames-specific behavior for %public-keys gifts(?).
::      should it do something similar for fine?
::
=,  fine
|=  our=ship
=>  |%
    +$  move  [p=duct q=(wite note gift)]
    +$  note                                            ::  out request $->
      $%  $:  %b
          $%  $>(%wait task:behn)
              $>(%rest task:behn)
          ==  ==
          $:  %j
          $%  $>(%private-keys task:jael)
              $>(%public-keys task:jael)
      ==  ==  ==
    +$  sign
      $%  $:  %behn
          $%  $>(%wake gift:behn)
          ==  ==
          $:  %fine
          $%  $>(%tune gift)
              $>(%hoot gift)
              $>(%howl gift)
          ==  ==
          $:  %jael
          $%  $>(%private-keys gift:jael)
              $>(%public-keys gift:jael)
      ==  ==  ==
    ::
    ::TODO  get pubkeys from jael in the same way ames does
    ::
    +$  fine-state
      $:  %0
          hear=(jug path duct)                          ::  awaiting existence
          want=(jug path duct)                          ::  awaiting response
          part=(map path [siz=@ud pac=(map @ud @)])     ::  partial responses
      ==
    ::
    ::TODO  types for unpacked packets?
    ::
    +$  bits  [w=@ud d=@]
    ++  protocol-version  %0
    --
::
~%  %fine  ..part  ~
::
=|  fine-state
=*  state  -
|=  [now=@da eny=@uvJ rof=roof]
=*  fine-gate  .
=>  |%
    ++  spit
      |=  =path
      ^-  [pat=@t wid=@ud]
      =+  pat=(spat path)
      =+  wid=(met 3 pat)
      ?>  (lte wid 384)  ::TODO  check when we handle %keen, instead of here?
      [pat wid]
    ::
    ++  meet  ::NOTE  from ames, but bits instead of bytes
      |=  =ship
      ^-  [size=@ rank=@ub]
      =/  size=@  (met 3 ship)
      ?:  (lte size 2)  [16 %0b0]
      ?:  (lte size 4)  [32 %0b1]
      ?:  (lte size 8)  [64 %0b10]
      [128 %0b11]
    ::
    ++  header
      |=  [for=ship req=? bod=@ rel=?]
      ^-  bits
      =+  him=(meet our)
      =+  her=(meet for)
      =+  sum=(end [0 20] (mug bod))
      :-  32
      %+  can  0
      :~  2^0                 ::  reserved
          1^req               ::  request or response
          1^|                 ::  not ames
          3^protocol-version  ::  protocol version
          2^size.him          ::  sender address size
          2^size.her          ::  receiver address size
          20^sum              ::  checksum
          1^rel               ::  relayed
      ==
    ::
    ++  prelude
      |=  [for=[=ship =life] =life origin=(unit lane:ames)]
      ^-  bits
      =+  him=(meet our)
      =+  her=(meet ship.for)
      =/  ore=bits
        ?.  ?=([~ %| *] origin)  0^0
        (mul 8 (met 3 p.u.origin))^p.u.origin
      :-  :(add 4 4 size.him size.her w.ore)
      %+  can  0
      :~  4^(mod life 16)      ::  sender life
          4^(mod life.for 16)  ::  receiver life
          size.him^our         ::  sender
          size.her^ship.for    ::  receiver
          ore
      ==
    ::
    ++  request-body
      |=  [=path num=@ud]
      ^-  bits
      ?>  (lth num (bex 32))
      =+  (spit path)
      :-  :(add 32 16 (mul 8 wid))
      %+  can  0
      :~  32^num              ::  fragment number
          16^wid              ::  path size
          (mul 8 wid)^`@`pat  ::  namespace path
      ==
    ::
    ++  packetize-request
      |=  [=path num=@ud]
      ^-  hoot
      =+  bod=(request-body path num)
      (can 0 512^(sign:keys d.bod) bod ~)
    ::
    ++  packetize-response
      |=  [=path data=(unit (cask *))]
      ^-  song
      ::  prepend request descriptions to each response packet
      ::
      =;  pacs=(list bits)
        %-  head
        %^  spin  pacs  1
        |=  [pac=bits num=@ud]
        ^-  [purr _num]
        :_  +(num)
        ::NOTE  we stub out the receiver & origin details,
        ::      runtime should replace them as appropriate.
        =/  pre=bits  (prelude [*ship *life] life:keys ~)
        =/  req=bits  (request-body path num)
        =/  bod=bits  [:(add w.pre w.req w.pac) (can 0 pre req pac ~)]
        =/  hed=bits  (header *ship | +.bod |)
        (can 0 hed bod ~)
      ::  prepend a signature and split the data into 1024-byte fragments
      ::
      =/  frag=(list @)
        =/  sig=@  (full:keys path (fall data ~))
        ?~  data  [sig]~
        %+  rip  3^1.024  ::TODO  prints "rip: stub"
        (cat 3 sig (jam u.data))  ::REVIEW
      =/  size=@ud
        ?~(data 0 (lent frag))  ::REVIEW
      ::  sign & packetize the fragments
      ::
      %+  turn  frag
      |=  dat=@
      =/  wid=@ud  (met 3 dat)
      :-  :(add 512 32 16 (mul 8 wid))
      %+  can  0
      :~  512^(sign:keys dat)  ::  signature
          32^size                   ::  number of fragments
          16^wid                    ::  response data size in bytes  ::REVIEW
          (mul 8 wid)^dat           ::  response data
      ==
    ::
    ++  keys
      |%
      ++  full
        |=  [=path mess=*]
        (sign (shax (jam [our life path mess])))
      ::
      ++  life  ~+  (jael ^life %life /(scot %p our))
      ++  ring  ~+  (jael ^ring %vein /(scot %ud life))
      ++  sign      sign:as:(nol:nu:crub:crypto ring)
      ::
      ++  jael
        |*  [=mold =desk =path]
        !<  mold
        %-  tail  %-  need  %-  need
        (rof `[our ~ ~] [%jael %$] [our desk da+now] path)
      ::
      ++  lyfe
        |=  who=ship
        (jael (unit ^life) %lyfe /(scot %p our))
      ::
      ::TODO  don't scry! subscribe & get from state instead
      ++  pass
        |=  [who=ship lyf=^life]
        ::TODO  but might fail need checks?
        :: (jael * %deed )
        !!
      --
    ::
    ::
    ::TODO  copied from ames
    +$  dyad  [sndr=ship rcvr=ship]
    +$  packet
      $:  dyad
          sndr-tick=@ubC
          rcvr-tick=@ubC
          origin=(unit @uxaddress)
          content=@uxcontent
      ==
    ::
    ::TODO  copied from ames, only req parsing added
    ++  decode-packet
      ~/  %decode-packet
      |=  blob=@ux
      ^-  [req=? packet]
      ~|  %decode-packet-fail
      ::  first 32 (2^5) bits are header; the rest is body
      ::
      =/  header  (end 5 blob)
      =/  body    (rsh 5 blob)
      ::  read header; first two bits are reserved
      ::
      :-  req==(0 (cut 0 [2 1] header))
      =/  is-ames  (cut 0 [3 1] header)
      ?:  =(& is-ames)
        ~|  %fine-but-ames  !!
      ::
      =/  version  (cut 0 [4 3] header)
      ?.  =(protocol-version version)
        ~|  fine-protocol-version+version  !!
      ::
      =/  sndr-size  (decode-ship-size (cut 0 [7 2] header))
      =/  rcvr-size  (decode-ship-size (cut 0 [9 2] header))
      =/  checksum   (cut 0 [11 20] header)
      =/  relayed    (cut 0 [31 1] header)
      ::  origin, if present, is 6 octets long, at the end of the body
      ::
      =^  origin=(unit @)  body
        ?:  =(| relayed)
          [~ body]
        =/  len  (sub (met 3 body) 6)
        [`(end [3 6] body) (rsh [3 6] body)]
      ::  .checksum does not apply to the origin
      ::
      ?.  =(checksum (end [0 20] (mug body)))
        ~|  %ames-checksum  !!
      ::  read fixed-length sndr and rcvr life data from body
      ::
      ::    These represent the last four bits of the sender and receiver
      ::    life fields, to be used for quick dropping of honest packets to
      ::    or from the wrong life.
      ::
      =/  sndr-tick  (cut 0 [0 4] body)
      =/  rcvr-tick  (cut 0 [4 4] body)
      ::  read variable-length .sndr and .rcvr addresses
      ::
      =/  off   1
      =^  sndr  off  [(cut 3 [off sndr-size] body) (add off sndr-size)]
      ?.  (is-valid-rank sndr sndr-size)
        ~|  ames-sender-impostor+[sndr sndr-size]  !!
      ::
      =^  rcvr  off  [(cut 3 [off rcvr-size] body) (add off rcvr-size)]
      ?.  (is-valid-rank rcvr rcvr-size)
        ~|  ames-receiver-impostor+[rcvr rcvr-size]  !!
      ::  read variable-length .content from the rest of .body
      ::
      =/  content  (cut 3 [off (sub (met 3 body) off)] body)
      [[sndr rcvr] sndr-tick rcvr-tick origin content]
    ::  +decode-ship-size: decode a 2-bit ship type specifier into a byte width
    ::
    ::    Type 0: galaxy or star -- 2 bytes
    ::    Type 1: planet         -- 4 bytes
    ::    Type 2: moon           -- 8 bytes
    ::    Type 3: comet          -- 16 bytes
    ::
    ++  decode-ship-size
      ~/  %decode-ship-size
      |=  rank=@ubC
      ^-  @
      ::
      ?+  rank  !!
        %0b0   2
        %0b1   4
        %0b10  8
        %0b11  16
      ==
    ::  +is-valid-rank: does .ship match its stated .size?
    ::
    ++  is-valid-rank
      ~/  %is-valid-rank
      |=  [=ship size=@ubC]
      ^-  ?
      .=  size
      ?-  (clan:title ship)
        %czar  2
        %king  2
        %duke  4
        %earl  8
        %pawn  16
      ==
    ::
    +$  twit  ::  signed request
      $:  signature=@
          peep
      ==
    ::
    +$  peep  ::  request data
      $:  =path
          num=@ud
      ==
    ::
    +$  rawr  ::  response data
      $:  sig=@
          siz=@ud
          wid=@ud
          dat=@
      ==
    ::
    ++  decode-request
      |=  =hoot
      ^-  twit
      :-  sig=(cut 0 [0 512] hoot)
      -:(decode-request-info (rsh [0 512] hoot))
    ::
    ++  decode-request-info
      |=  =hoot
      ^-  [=peep =purr]
      =+  num=(cut 0 [0 32] hoot)
      =+  len=(cut 0 [32 16] hoot)
      =+  pat=(cut 3 [6 len] hoot)
      :-  [(stab pat) num]
      ::  if there is data remaining, it's the response
      (rsh [3 (add 6 len)] hoot)
    ::
    ++  decode-response
      |=  =purr
      =;  =rawr
        ~?  !=(wid.rawr (met 3 dat.rawr))  [%fine %unexpected-dat-size]
        rawr
      :*  sig=(cut 0 [0 512] purr)
          siz=(cut 0 [512 32] purr)
          wid=(cut 0 [544 16] purr)
          dat=(rsh 0^560 purr)
      ==
    ::
    ++  verify-response
      |=  rawr
      !!
    --
^?
|%
::  +call: handle a +task:fine request
::
++  call
  ~%  %fine-call  ..part  ~
  |=  $:  hen=duct
          dud=(unit goof)
          wrapped-task=(hobo task)
      ==
  ^-  [(list move) _fine-gate]
  ::
  =/  =task  ((harden task) wrapped-task)
  ::
  ?^  dud
    ~|(%fine-call-dud (mean -.task tang.u.dud))
  ::
  =^  moves  state
    ?-  -.task
        %keen
      :-  ~  ::TODO  emit request packet
      state(want (~(put ju want) path.task hen))
    ::
        %yawn
      [~ state(want (~(del ju want) path.task hen))]
    ::
        %purr
      =/  [req=? =packet]  (decode-packet purr.task)
      ?:  req
        =/  =twit  (decode-request `@ux`content.packet)
        ::TODO  verify request signature
        ::TODO  handle twit
        !!
      =/  [=peep =purr]  (decode-request-info `@ux`content.packet)
      =/  =rawr          (decode-response purr)
      ::TODO  validate response signature
      ?:  =(0 siz.rawr)
        ::TODO  complete instantly
        ::TODO  (~(del by part) path.peep) for safety?
        !!
      !!
      :: =/  have=(list @)  (~(get ja part) path.peep)
      :: ::TODO  if we get fancier, we could receive in any order
      :: ?>  =((lent have) (dec num.peep))
      :: =.  have  [[wid dat]:rawr have]
      :: ::  if we have all the parts now, construct the response
      :: ::
      :: ?>  (lte num.peep siz.rawr)
      :: ?:  =(num.peep siz.rawr)
      ::   ?.  =((lent have) siz.rawr)
      ::     ::TODO  wtf! start over?
      ::     !!
      ::   ::TODO  need to flop first y/n?
      ::   !!
      :: ::  otherwise, store the part, and send out the next request
      :: ::
      :: =.  part  (~(put by part) path.peep have)
      :: ::TODO  emit properly
      :: :+  %hoot
      ::   !!  ::TODO  get lane, from response or ames?
      :: ::TODO  needs header, prelude
      :: (packetize-request path.peep +(num.peep))
    ::
        %bide
      [~ state(hear (~(put ju hear) path.task hen))]
    ::
        %born
      [~ state(hear ~)]  ::REVIEW  assuming this is for runtime use only?
    ::
        %trim
      [~ state]  ::TODO  maybe clear part?
    ::
        %vega
      [~ state]
    ==
  [~ fine-gate]
::  +load: migrate an old state to a new fine version
::
++  load
  |=  old=fine-state
  ^+  fine-gate
  fine-gate(state old)
::  +scry: get packets
::
::    /fx/message/[full-scry-path]    song    all packets for some scry path
::
++  scry
  ^-  roon
  |=  [lyc=gang car=term bem=beam]
  ^-  (unit (unit cage))
  ::TODO  don't special-case whey scry
  ::
  ?:  &(=(car %$) =(s.bem /whey))
    =/  maz=(list mass)
      :~  state+&+state
      ==
    ``mass+!>(maz)
  ::  only respond for the local identity, %$ desk, current timestamp
  ::
  ?.  ?&  =(our p.bem)
          =(%$ q.bem)
          =([%da now] r.bem)
      ==
    ~
  ::
  ?.  ?=(%x car)  ~
  ?.  ?=([%message @ *] s.bem)  ~
  ::  s.bem is expected to be a scry path of the shape /vc/desk/rev/etc,
  ::  so we need to give it the right shape
  ::
  =/  pax=path
    [i.t.s.bem (scot %p our) t.t.s.bem]
  ?~  nom=(de-omen pax)  [~ ~]
  ::  we only support scrying into clay, and only if the data is fully public
  ::
  ?.  =(%c (end 3 (snag 0 pax)))  ~
  =+  pem=(rof lyc (need (de-omen %cp (slag 1 pax))))
  ?~  pem  ~
  ?~  u.pem  ~
  =+  per=!<([r=dict:clay w=dict:clay] q.u.u.pem)
  ?.  =([%black ~ ~] rul.r.per)  ~
  ::  scry out the data from clay and packetize it as appropriate
  ::
  =+  res=(rof lyc u.nom)
  ?-  res
    ~        ~
    [~ ~]    ``noun+!>((packetize-response pax ~))
    [~ ~ *]  ``noun+!>((packetize-response pax `[p q.q]:u.u.res))
  ==
::
++  stay  state
++  take
  |=  [tea=wire hen=duct dud=(unit goof) hin=sign]
  ^-  [(list move) _fine-gate]
  ?^  dud
    ~|(%fine-take-dud (mean tang.u.dud))
  ::
  ::TODO
  [~ fine-gate]
--
