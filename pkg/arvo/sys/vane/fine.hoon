::  %fine, remote scries
!:
::
=,  fine
|=  our=ship
=>  |%
    +$  move  [p=duct q=(wite note gift)]
    +$  note                                            ::  out request $->
      $%  $:  %b                                        ::TODO   to self
              $>(%wait task:behn)                       ::  set timer
      ==  ==                                            ::
    +$  sign
      $%  [%fine $>(%tune gift)]
          [%fine $>(%hoot gift)]
          [%fine $>(%howl gift)]
      ==
    ::
    +$  fine-state
      $:  %0
          hear=(jug path duct)                          ::  awaiting existence
          want=(jug path duct)                          ::  awaiting response
          part=(map path [size=@ud =song])              ::  partial responses
          ::TODO  cache? full=(map path (cask *))
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
      (can 0 512^(sign:sign-fine d.bod) bod ~)
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
        =/  pre=bits  (prelude [*ship *life] life:sign-fine ~)
        =/  req=bits  (request-body path num)
        =/  bod=bits  [:(add w.pre w.req w.pac) (can 0 pre req pac ~)]
        =/  hed=bits  (header *ship | +.bod |)
        (can 0 hed bod ~)
      ::  prepend a signature and split the data into 1024-byte fragments
      ::
      =/  frag=(list @)
        =/  sig=@  (sign-fine path (fall data ~))
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
      :~  512^(sign:sign-fine dat)  ::  signature
          32^size                   ::  number of fragments
          16^wid                    ::  response data size in bytes  ::REVIEW
          (mul 8 wid)^dat           ::  response data
      ==
    ::
    ++  sign-fine  ::TODO  +sign, except compiler bug
      |^  |=  [=path mess=*]
          (sign (shax (jam [our life path mess])))
      ::
      ++  life  ~+  (jael ^life %life /[(scot %p our)])
      ++  ring  ~+  (jael ^ring %vein /[(scot %ud life)])
      ++  sign      sign:as:(nol:nu:crub:crypto ring)
      ::
      ++  jael
        |*  [=mold =desk =path]
        !<  mold
        %-  tail  %-  need  %-  need
        (rof `[our ~ ~] [%jael %$] [our desk da+now] path)
      --
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
      %keen  !!  ::TODO  emit request packet
      %yawn  !!
    ::
      %purr  !!
      %bide  [~ state(hear (~(put ju hear) path.task hen))]
    ::
      %born  [~ state(hear ~)]  ::REVIEW  assuming this is for runtime use only?
      %trim  [~ state]  ::TODO  maybe clear part?
      %vega  [~ state]
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
