/+  *test, test-pub, test-sub
/=  ames-raw  /sys/vane/ames
/=  gall-raw  /sys/vane/gall
::
=/  ames-bunt  (ames-raw ~zod)
=/  gall-bunt  (gall-raw ~zod)
::  basic helpers
::
|%
++  comet-a-ring
  0wfm.lBEWM.08gfy.AxYjy.8-tBQ.uq-aa.LZt9c.CVQqd.XBJIs.
  CoG90.BNNGV.1ZmVi.ZbAhY.LuhwC.idNnU.lCVkt.Z4qug.7iY92
::
++  comet-b-ring
  0w3-.kl6Hg.mLISf.pDTQQ.LymxF.q4Isr.AGUAv.uJvkf.DOjeU.
  U8OPv.XO9T2.Hfhe~.zFwpO.Q1tn5.BeBSQ.o4MTS.lTWAh.TcOJ2
::
++  moon-a  ~mister-dister-norsyr-torryn
++  moon-b  ~mister-roller-norsyr-torryn
++  planet-a  ~norsyr-torryn
++  planet-b  ~torryn-norsyr
::
++  crypto-core
  |%  ++  nec        (pit:nu:cric:crypto 512 (shaz 'nec') %b ~)
      ++  bud        (pit:nu:cric:crypto 512 (shaz 'bud') %b ~)
      ++  zod        (pit:nu:cric:crypto 512 (shaz 'zod') %b ~)
      ++  comet-a    (nol:nu:cric:crypto comet-a-ring)
      ++  comet-b    (nol:nu:cric:crypto comet-b-ring)
      ++  moon-a     (pit:nu:cric:crypto 32 (shaz ^moon-a) %b ~)
      ++  moon-b     (pit:nu:cric:crypto 32 (shaz ^moon-b) %b ~)
      ++  planet-a   (pit:nu:cric:crypto 32 (shaz ^planet-a) %b ~)
      ++  planet-b   (pit:nu:cric:crypto 32 (shaz ^planet-b) %b ~)
      ++  sign
        |=  [=ship data=@ux]
        ?:  =(ship ~nec)
          (sign:ed:crypto data sgn:ven:ex:nec)
        ?:  =(ship ~zod)
          (sign:ed:crypto data sgn:ven:ex:zod)
        (sign:ed:crypto data sgn:ven:ex:bud)
  ::
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
  =.  eny.nec  0v3f.arfnf
  =.  life.ames-state.nec  nec.life
  =.  rift.ames-state.nec  nec.rift
  =.  rof.nec  |=(* ``[%noun !>(*(list turf))])
  =.  ring.ames-state.nec  sec:ex:nec:crypto-core
  =.  pass.ames-state.nec  pub:ex:nec:crypto-core
  =.  saf.ames-state.nec   saf:ex:nec:crypto-core
  ::  create ~bud
  ::
  =/  bud  (ames-raw ~bud)
  =.  now.bud  ~1111.1.1
  =.  eny.bud  0v3f.arfnf
  =.  life.ames-state.bud  bud.life
  =.  rift.ames-state.bud  bud.rift
  =.  rof.bud  |=(* ``[%noun !>(*(list turf))])
  =.  ring.ames-state.bud  sec:ex:bud:crypto-core
  =.  pass.ames-state.bud  pub:ex:bud:crypto-core
  =.  saf.ames-state.bud   saf:ex:bud:crypto-core
  ::
  =/  nec-sym
    (derive-symmetric-key:ames-raw pub.saf.ames-state.bud sek.saf.ames-state.nec)
  =/  bud-sym
    (derive-symmetric-key:ames-raw pub.saf.ames-state.nec sek.saf.ames-state.bud)
  ?>  =(nec-sym bud-sym)
  ::  tell ~nec about ~bud
  ::
  =.  chums.ames-state.nec
    %+  ~(put by chums.ames-state.nec)  ~bud
    =|  =fren-state:ames
    =.  -.fren-state
      :*  symmetric-key=bud-sym
          life=bud.life
          rift=bud.rift
          [public-keys=pub.saf pass=pass]:ames-state.bud
          sponsor=~bud
      ==
    =.  lane.fren-state  `[0 *lane:pact:ames]
    [%known fren-state]
  ::  tell ~bud about ~nec
  ::
  =.  chums.ames-state.bud
    %+  ~(put by chums.ames-state.bud)  ~nec
    =|  =fren-state:ames
    =.  -.fren-state
      :*  symmetric-key=nec-sym
          life=nec.life
          rift=nec.rift
          [public-keys=pub.saf pass=pass]:ames-state.nec
          sponsor=~nec
      ==
    =.  lane.fren-state  `[0 *lane:pact:ames]
    [%known fren-state]
  ::  metamorphose
  ::
  =>  .(nec +:(call:(nec) ~[//unix] ~ %born ~))
  =>  .(bud +:(call:(bud) ~[//unix] ~ %born ~))
  ::
  [nec=nec bud=bud]
::
++  ames-comets-moons-planets-galaxies
  |=  $:  life=[comet-a=@ud comet-b=@ud moon-a=@ud moon-b=@ud planet-a=@ud planet-b=@ud galaxy-a=@ud galaxy-b=@ud]
          rift=[comet-a=@ud comet-b=@ud moon-a=@ud moon-b=@ud planet-a=@ud planet-b=@ud galaxy-a=@ud galaxy-b=@ud]
      ==
  ::  create each ship: set now, eny, life, rift, rof, and crypto keys
  ::
  =/  comet-a  (ames-raw ~dacrum-tordyt-dassel-mogred--sabnyx-malbes-mogdef-litzod)
  =.  now.comet-a   ~1111.1.1
  =.  eny.comet-a   0v3f.arfnf
  =.  life.ames-state.comet-a  comet-a.life
  =.  rift.ames-state.comet-a  comet-a.rift
  =.  rof.comet-a   |=(* ``[%noun !>(*(list turf))])
  =.  ring.ames-state.comet-a  sec:ex:comet-a:crypto-core
  =.  pass.ames-state.comet-a  pub:ex:comet-a:crypto-core
  =.  saf.ames-state.comet-a   saf:ex:comet-a:crypto-core
  ::
  =/  comet-b  (ames-raw ~lopdur-lopsyl-tagted-lidbet--podlud-sicnux-tidlev-marzod)
  =.  now.comet-b   ~1111.1.1
  =.  eny.comet-b   0v3f.arfnf
  =.  life.ames-state.comet-b  comet-b.life
  =.  rift.ames-state.comet-b  comet-b.rift
  =.  rof.comet-b   |=(* ``[%noun !>(*(list turf))])
  =.  ring.ames-state.comet-b  sec:ex:comet-b:crypto-core
  =.  pass.ames-state.comet-b  pub:ex:comet-b:crypto-core
  =.  saf.ames-state.comet-b   saf:ex:comet-b:crypto-core
  ::
  =/  moon-a  (ames-raw moon-a)
  =.  now.moon-a   ~1111.1.1
  =.  eny.moon-a   0v3f.arfnf
  =.  life.ames-state.moon-a  moon-a.life
  =.  rift.ames-state.moon-a  moon-a.rift
  =.  rof.moon-a   |=(* ``[%noun !>(*(list turf))])
  =.  ring.ames-state.moon-a  sec:ex:moon-a:crypto-core
  =.  pass.ames-state.moon-a  pub:ex:moon-a:crypto-core
  =.  saf.ames-state.moon-a   saf:ex:moon-a:crypto-core
  ::
  =/  moon-b  (ames-raw moon-b)
  =.  now.moon-b   ~1111.1.1
  =.  eny.moon-b   0v3f.arfnf
  =.  life.ames-state.moon-b  moon-b.life
  =.  rift.ames-state.moon-b  moon-b.rift
  =.  rof.moon-b   |=(* ``[%noun !>(*(list turf))])
  =.  ring.ames-state.moon-b  sec:ex:moon-b:crypto-core
  =.  pass.ames-state.moon-b  pub:ex:moon-b:crypto-core
  =.  saf.ames-state.moon-b   saf:ex:moon-b:crypto-core
  ::
  =/  planet-a  (ames-raw planet-a)
  =.  now.planet-a   ~1111.1.1
  =.  eny.planet-a   0v3f.arfnf
  =.  life.ames-state.planet-a  planet-a.life
  =.  rift.ames-state.planet-a  planet-a.rift
  =.  rof.planet-a   |=(* ``[%noun !>(*(list turf))])
  =.  ring.ames-state.planet-a  sec:ex:planet-a:crypto-core
  =.  pass.ames-state.planet-a  pub:ex:planet-a:crypto-core
  =.  saf.ames-state.planet-a   saf:ex:planet-a:crypto-core
  ::
  =/  planet-b  (ames-raw planet-b)
  =.  now.planet-b   ~1111.1.1
  =.  eny.planet-b   0v3f.arfnf
  =.  life.ames-state.planet-b  planet-b.life
  =.  rift.ames-state.planet-b  planet-b.rift
  =.  rof.planet-b   |=(* ``[%noun !>(*(list turf))])
  =.  ring.ames-state.planet-b  sec:ex:planet-b:crypto-core
  =.  pass.ames-state.planet-b  pub:ex:planet-b:crypto-core
  =.  saf.ames-state.planet-b   saf:ex:planet-b:crypto-core
  ::
  =/  galaxy-a  (ames-raw ~nec)
  =.  now.galaxy-a   ~1111.1.1
  =.  eny.galaxy-a   0v3f.arfnf
  =.  life.ames-state.galaxy-a  galaxy-a.life
  =.  rift.ames-state.galaxy-a  galaxy-a.rift
  =.  rof.galaxy-a   |=(* ``[%noun !>(*(list turf))])
  =.  ring.ames-state.galaxy-a  sec:ex:nec:crypto-core
  =.  pass.ames-state.galaxy-a  pub:ex:nec:crypto-core
  =.  saf.ames-state.galaxy-a   saf:ex:nec:crypto-core
  ::
  =/  galaxy-b  (ames-raw ~bud)
  =.  now.galaxy-b   ~1111.1.1
  =.  eny.galaxy-b   0v3f.arfnf
  =.  life.ames-state.galaxy-b  galaxy-b.life
  =.  rift.ames-state.galaxy-b  galaxy-b.rift
  =.  rof.galaxy-b   |=(* ``[%noun !>(*(list turf))])
  =.  ring.ames-state.galaxy-b  sec:ex:bud:crypto-core
  =.  pass.ames-state.galaxy-b  pub:ex:bud:crypto-core
  =.  saf.ames-state.galaxy-b   saf:ex:bud:crypto-core
  ::
  ::  connect-all: for each ship A, register every other ship B in A's chums.
  ::  iterates all ordered pairs (A,B) with A≠B; derives shared key from
  ::  A's sek and B's pub (ECDH), then writes B into A's chums map.
  ::
  =/  connect-all
    |=  ss=(list [g=_ames-bunt lif=@ud rif=@ud])
    ^+  ss
    %+  turn  ss
    |=  a=[g=_ames-bunt lif=@ud rif=@ud]
    ^+  a
    =/  others  ss
    |-  ^+  a
    ?~  others
      a
    =/  b  i.others
    ?:  =(our.g.b our.g.a)
      $(others t.others)
    =/  sym
      (derive-symmetric-key:ames-raw pub.saf.ames-state.g.b sek.saf.ames-state.g.a)
    =.  g.a
      =.  chums.ames-state.g.a
        %+  ~(put by chums.ames-state.g.a)  our.g.b
        =|  =fren-state:ames
        =.  -.fren-state
          :*  symmetric-key=sym
              lif.b
              rif.b
              [public-keys=pub.saf pass=pass]:ames-state.g.b
              sponsor=~bud
          ==
        =.  lane.fren-state  `[0 *lane:pact:ames]
        [%known fren-state]
      g.a
    $(others t.others)
  ::
  =/  ships
    %:  connect-all
        :~  [comet-a comet-a.life comet-a.rift]
            [comet-b comet-b.life comet-b.rift]
            [moon-a moon-a.life moon-a.rift]
            [moon-b moon-b.life moon-b.rift]
            [planet-a planet-a.life planet-a.rift]
            [planet-b planet-b.life planet-b.rift]
            [galaxy-a galaxy-a.life galaxy-a.rift]
            [galaxy-b galaxy-b.life galaxy-b.rift]
    ==  ==
  ::
  =/  comet-a   g:(snag 0 ships)
  =/  comet-b   g:(snag 1 ships)
  =/  moon-a    g:(snag 2 ships)
  =/  moon-b    g:(snag 3 ships)
  =/  planet-a  g:(snag 4 ships)
  =/  planet-b  g:(snag 5 ships)
  =/  galaxy-a  g:(snag 6 ships)
  =/  galaxy-b  g:(snag 7 ships)
  ::  metamorphose
  ::
  =>  .(comet-a +:(call:(comet-a) ~[//unix] ~ %born ~))
  =>  .(comet-b +:(call:(comet-b) ~[//unix] ~ %born ~))
  =>  .(moon-a +:(call:(moon-a) ~[//unix] ~ %born ~))
  =>  .(moon-b +:(call:(moon-b) ~[//unix] ~ %born ~))
  =>  .(planet-a +:(call:(planet-a) ~[//unix] ~ %born ~))
  =>  .(planet-b +:(call:(planet-b) ~[//unix] ~ %born ~))
  =>  .(galaxy-a +:(call:(galaxy-a) ~[//unix] ~ %born ~))
  =>  .(galaxy-b +:(call:(galaxy-b) ~[//unix] ~ %born ~))
  ::
  :*  comet-a=comet-a    comet-b=comet-b
      moon-a=moon-a      moon-b=moon-b
      planet-a=planet-a  planet-b=planet-b
      galaxy-a=galaxy-a  galaxy-b=galaxy-b
  ==
::
--
::  forward-declare to avoid repeated metamorphoses
::
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
::
++  ames-reply
  |=  [=ames-gate =duct pac=(list move:ames-bunt) =roof]
  ^-  [(list move:ames-bunt) ^ames-gate]
  ~|  pac
  =.  pac
    %+  skim  pac
    |=  =move:ames-bunt
    ?=([* [%give [%push *]]] move)
  ?>  ?=([[* [%give [%push *]]] *] pac)
  =/  ames-core  (ames-gate now=~1111.1.1 eny=`@`0xdead.beef roof)
  %-  call:ames-core
  ~!  p.card.i.pac
  [duct dud=~ %soft `task:ames`[%heer *lane:pact:ames q.p.card.i.pac]]
::
++  ames-expect-msg
  |=  [pac=(list move:ames-bunt) exp=noun]
  ~|  pac
  =.  pac
    %+  skim  pac
    |=  =move:ames-bunt
    ?=([* [%give [%sage *]]] move)
  ?>  ?=([[* [%give [%sage *]]] *] pac)
  ~|  card.i.pac
  ?>  ?=([%sage ^ [@tas *]] p.card.i.pac)
  ~|  q.sage.p.card.i.pac
  (expect-eq !>(q.q.sage.p.card.i.pac) !>(exp))
::
++  ames-make-pact
  |=  [=ames-gate =spar:ames =path =per=rift poke-roof=roof]
  ^-  @
  =/  sample     [now=~1111.1.1 eny=`@`0xdead.beef poke-roof]
  =/  ames-core  (ames-gate sample)
  ?~  pact=(co-make-pact:co:mesa:ames-core spar `path per-rift)
    !!
  p:(fax:plot (en:pact:ames u.pact))
::
++  ames-scry-payload
  |=  [=ames-gate her=ship our=ship =path]
  ^-  cage
  =/  ames-core  (ames-gate now=~1111.1.1 eny=`@`0xdead.beef *roof)
  %-  need   %-  need
  %-  scry:(ames-gate ~1111.1.10 `@`0xdead.beef *roof)
  =;  [care=@tas =beam]
    [`[her ~ ~] / care beam]
  =<  [?>(?=(^ vew) car.vew) bem]
  (need (inner-path-to-beam:ames-core our path))
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
++  ames-check-call-with-dude
  |=  $:  =ames-gate
          [now=@da eny=@ =roof]
          [=goof =duct task=(hobo task:ames)]
          expected-moves=(list move:ames-bunt)
      ==
  ^-  [tang ^ames-gate]
  =/  ames-core  (ames-gate now eny roof)
  =^  moves  ames-gate  (call:ames-core duct dud=`goof task)
  [(expect-eq !>(expected-moves) !>(moves)) ames-gate]
::
++  ames-call
  |=  [=ames-gate =duct task=(hobo task:ames) =roof]
  %.  [duct dud=~ task]
  call:(ames-gate now=~1111.1.1 eny=`@`0xdead.beef roof)
::
++  ames-call-with-dude
  |=  [=ames-gate =goof =duct task=(hobo task:ames) =roof]
  %.  [duct `goof task]
  call:(ames-gate now=~1111.1.1 eny=`@`0xdead.beef roof)
::  +ames: run ames sign, assert produces expected-moves
::
++  ames-check-take
  |=  $:  =ames-gate
          [now=@da eny=@ =roof]
          [=wire =duct sign=sign:ames-bunt]
          expected-moves=(list move:ames-bunt)
      ==
  ^-  [tang ^ames-gate]
  =/  ames-core  (ames-gate now eny roof)
  =^  moves  ames-gate  (take:ames-core wire duct dud=~ sign)
  [(expect-eq !>(expected-moves) !>(moves)) ames-gate]
::
++  ames-take
  |=  [=ames-gate =wire =duct sign=sign:ames-bunt =roof]
  %.  [wire duct dud=~ sign]
  take:(ames-gate now=~1111.1.1 eny=`@`0xdead.beef roof)
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
::
++  ames-scry-peer
  |=  $:  =ames-gate
          [now=@da eny=@ =roof]
          our=ship
          her=ship
      ==
  ^-  fren-state:ames
  =-  ?>(?=(%known -<) ->)
  !<  chum-state:ames
  =<  q
  %-  need  %-  need
  %-  scry:(ames-gate now eny roof)
  [[~ ~] / %x [[our %$ da+now] /chums/(scot %p her)]]
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
  [[~ ~] / %n [[our dude da+now] [%$ (scot %p ship.sub) [term wire]:sub]]]
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
