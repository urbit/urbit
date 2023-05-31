/+  *test, test-pub, test-sub
/=  ames-raw  /sys/vane/ames
/=  gall-raw  /sys/vane/gall
::
=/  ames-bunt  (ames-raw ~zod)
=/  gall-bunt  (gall-raw ~zod)
::  basic helpers
::
|%
++  crypto-core
  |%  ++  nec  (pit:nu:crub:crypto 512 (shaz 'nec'))
      ++  bud  (pit:nu:crub:crypto 512 (shaz 'bud'))
      ++  sign
        |=  [=ship data=@ux]
        %.  data
        ?:(=(ship ~nec) sigh:as:nec sigh:as:bud)
  --
::
++  make-gall
  |=  =ship
  =/  gall-pupa  (gall-raw ship)
  =/  gall-core  (gall-pupa now=~1111.1.1 eny=`@`0xdead.beef scry=*roof)
  =+  [out adult]=(call:gall-core duct=~[/init] dud=~ task=[%init ~])
  adult
::
++  ames-nec-bud
  |=  [life=[nec=@ud bud=@ud] rift=[nec=@ud bud=@ud]]
  ::  create ~nec
  ::
  =/  nec  (ames-raw ~nec)
  =.  now.nec  ~1111.1.1
  =.  eny.nec  0xdead.beef
  =.  life.ames-state.nec  nec.life
  =.  rift.ames-state.nec  nec.rift
  =.  rof.nec  |=(* ``[%noun !>(*(list turf))])
  =.  crypto-core.ames-state.nec  nec:crypto-core
  =/  nec-pub  pub:ex:crypto-core.ames-state.nec
  =/  nec-sec  sec:ex:crypto-core.ames-state.nec
  ::  create ~bud
  ::
  =/  bud  (ames-raw ~bud)
  =.  now.bud  ~1111.1.1
  =.  eny.bud  0xbeef.dead
  =.  life.ames-state.bud  bud.life
  =.  rift.ames-state.bud  bud.rift
  =.  rof.bud  |=(* ``[%noun !>(*(list turf))])
  =.  crypto-core.ames-state.bud  bud:crypto-core
  =/  bud-pub  pub:ex:crypto-core.ames-state.bud
  =/  bud-sec  sec:ex:crypto-core.ames-state.bud
  ::
  =/  nec-sym  (derive-symmetric-key:ames-raw bud-pub nec-sec)
  =/  bud-sym  (derive-symmetric-key:ames-raw nec-pub bud-sec)
  ?>  =(nec-sym bud-sym)
  ::  tell ~nec about ~bud
  ::
  =.  peers.ames-state.nec
    %+  ~(put by peers.ames-state.nec)  ~bud
    =|  =peer-state:ames
    =.  -.peer-state
      :*  symmetric-key=bud-sym
          life=bud.life
          rift=bud.rift
          public-key=bud-pub
          sponsor=~bud
      ==
    =.  route.peer-state  `[direct=%.y `lane:ames`[%& ~bud]]
    [%known peer-state]
  ::  tell ~bud about ~nec
  ::
  =.  peers.ames-state.bud
    %+  ~(put by peers.ames-state.bud)  ~nec
    =|  =peer-state:ames
    =.  -.peer-state
      :*  symmetric-key=nec-sym
          life=nec.life
          rift=nec.rift
          public-key=nec-pub
          sponsor=~nec
      ==
    =.  route.peer-state  `[direct=%.y `lane:ames`[%& ~nec]]
    [%known peer-state]
  ::  metamorphose
  ::
  =>  .(nec +:(call:(nec) ~[//unix] ~ %born ~))
  =>  .(bud +:(call:(bud) ~[//unix] ~ %born ~))
  ::
  [nec=nec bud=bud]
--
::  forward-declare to avoid repeated metamorphoses
=/  gall-adult  (make-gall ~zod)
=/  ames-adult  nec:(ames-nec-bud [1 1] [0 0])
::  main core
::
|%
+$  gall-gate  _gall-adult
+$  ames-gate  _ames-adult
::
++  nec-bud
  |=  [life=[nec=@ud bud=@ud] rift=[nec=@ud bud=@ud]]
  =/  a  (ames-nec-bud [nec bud]:life [nec bud]:rift)
  =/  gall-nec  (make-gall ~nec)
  =.  gall-nec  (load-agent ~nec gall-nec %sub test-sub)
  =/  gall-bud  (make-gall ~bud)
  =.  gall-bud  (load-agent ~bud gall-bud %pub test-pub)
  :*  nec=[ames=nec.a gall=gall-nec]
      bud=[ames=bud.a gall=gall-bud]
  ==
::  +gall-check-call: run gall task, assert produces expected-moves
::
++  gall-check-call
  |=  $:  =gall-gate
          [now=@da eny=@ =roof]
          [=duct task=(hobo task:gall)]
          expected-moves=(list move:gall-bunt)
      ==
  ^-  [tang ^gall-gate]
  =/  gall-core  (gall-gate now eny roof)
  =^  moves  gall-gate  (call:gall-core duct dud=~ task)
  [(expect-eq !>(expected-moves) !>(moves)) gall-gate]
::
++  gall-call
  |=  [=gall-gate =duct task=(hobo task:gall) =roof]
  %.  [duct dud=~ task]
  call:(gall-gate now=~1111.1.1 eny=`@`0xdead.beef roof)
::  +gall-check-take: run gall sign, assert produces expected-moves
::
++  gall-check-take
  |=  $:  =gall-gate
          [now=@da eny=@ =roof]
          [=wire =duct =sign-arvo]
          expected-moves=(list move:gall-bunt)
      ==
  ^-  [tang ^gall-gate]
  =/  gall-core  (gall-gate now eny roof)
  =^  moves  gall-gate  (take:gall-core wire duct dud=~ sign-arvo)
  [(expect-eq !>(expected-moves) !>(moves)) gall-gate]
::
++  gall-take
  |=  [=gall-gate =wire =duct =sign-arvo =roof]
  %.  [wire duct dud=~ sign-arvo]
  take:(gall-gate now=~1111.1.1 eny=`@`0xdead.beef roof)
::  +ames-check-call: run gall task, assert produces expected-moves
::
++  ames-check-call
  |=  $:  =ames-gate
          [now=@da eny=@ =roof]
          [=duct task=(hobo task:ames)]
          expected-moves=(list move:ames-bunt)
      ==
  ^-  [tang ^ames-gate]
  =/  ames-core  (ames-gate now eny roof)
  =^  moves  ames-gate  (call:ames-core duct dud=~ task)
  [(expect-eq !>(expected-moves) !>(moves)) ames-gate]
::
++  ames-call
  |=  [=ames-gate =duct task=(hobo task:ames) =roof]
  %.  [duct dud=~ task]
  call:(ames-gate now=~1111.1.1 eny=`@`0xdead.beef roof)
::  +ames: run ames sign, assert produces expected-moves
::
++  ames-check-take
  |=  $:  =ames-gate
          [now=@da eny=@ =roof]
          [=wire =duct =sign:ames-bunt]
          expected-moves=(list move:ames-bunt)
      ==
  ^-  [tang ^ames-gate]
  =/  ames-core  (ames-gate now eny roof)
  =^  moves  ames-gate  (take:ames-core wire duct dud=~ sign)
  [(expect-eq !>(expected-moves) !>(moves)) ames-gate]
::
++  ames-scry-hunk
  |=  $:  =ames-gate
          [now=@da eny=@ =roof]
          our=ship
          [lop=@ud len=@ud pax=path]
      ==
  ^-  [sig=@ux meows=(list @ux)]
  =/  =beam
    :-  [our %$ da+now]
    (welp /fine/hunk/[(scot %ud lop)]/[(scot %ud len)] pax)
  =+  pat=(spat pax)
  =+  wid=(met 3 pat)
  ?>  (lte wid 384)
  =/  meows
    !<  (list @ux)
    =<  q
    %-  need  %-  need
    (scry:(ames-gate now eny roof) ~ / %x beam)
  ::
  =/  paz=(list have:ames)
    %+  spun  meows
    |=  [blob=@ux num=_1]
    ^-  [have:ames _num]
    :_  +(num)
    [num (sift-meow:ames blob)]
  ::
  :-  sig:(sift-roar:ames-raw (lent paz) (flop paz))
  %+  spun  meows
  |=  [meow=@ux num=_1]
  :_  +(num)
  (can 3 4^num 2^wid wid^`@`pat (met 3 meow)^meow ~)
:: ::
++  ames-scry-peer
  |=  $:  =ames-gate
          [now=@da eny=@ =roof]
          our=ship
          her=ship
      ==
  ^-  peer-state:ames
  =-  ?>(?=(%known -<) ->)
  !<  ship-state:ames
  =<  q
  %-  need  %-  need
  %-  scry:(ames-gate now eny roof)
  [~ / %x [[our %$ da+now] /peers/(scot %p her)]]
::
++  gall-scry-nonce
  |=  $:  =gall-gate
          [now=@da eny=@ =roof]
          our=ship
          =dude:gall
          sub=[=ship =term =wire]
      ==
  ^-  @ud
  !<  @ud
  =<  q
  %-  need  %-  need
  %-  scry:(gall-gate now eny roof)
  [~ / %n [[our dude da+now] [%$ (scot %p ship.sub) [term wire]:sub]]]
::
++  load-agent
  |=  [=ship =gall-gate =dude:gall =agent:gall]
  =^  *  gall-gate
    %+  gall-call  gall-gate
    [~[/load] load/[[dude [ship %base da+~1111.1.1] agent]~] *roof]
  =^  *  gall-gate
    =/  =sign-arvo
      :+  %clay  %writ
      `[[%a da+~1111.1.1 %base] /app/[dude]/hoon vase+!>(!>(agent))]
    %:  gall-take
      gall-gate
      /sys/cor/[dude]/(scot %p ship)/base/(scot %da ~1111.1.1)
      ~[/load]
      sign-arvo
      *roof
    ==
  gall-gate
--
