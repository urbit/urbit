!:
::  clay (4c), version control
::
|=  pit=vase
^-  vane
=>
  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ::              section 4cA, filesystem logic         ::
  ::
  ::
  ::
  |%
  ++  de                                              ::  per desk
    |=  [now=@da hun=duct hez=(unit duct)]
    |=  [[who=@p for=@p] syd=@ta rede]
    =*  red  +<+>
    =|  yel=(list ,[p=duct q=card])
    =|  byn=(list ,[p=duct q=riot])
    =|  vag=(list ,[p=duct q=card])
    =|  say=(list ,[p=duct q=path r=ship s=[p=@ud q=riff]])
    |%
    ++  abet
      ^-  [(list move) rede]
      :_  red
      ;:  weld
        %+  turn  (flop yel)
        |=([a=duct b=card] [hun %give b])
      ::
        %+  turn  (flop byn)
        |=([a=duct b=riot] [a %give [%writ b]])
      ::
        %+  turn  (flop vag)
        |=([a=duct b=card] [a %give b])
      ::
        %+  turn  (flop say)
        |=  [a=duct b=path c=ship d=[p=@ud q=riff]]
        :-  a
        [%toss %a b %want [who c] [%q %re p.q.d (scot %ud p.d) ~] q.d]
      ==
    ::
    ++  aver                                          ::  read
      |=  mun=mood
      ^-  (unit (unit ,*))
      =+  ezy=?~(ref ~ (~(get by haw.u.ref) mun))
      ?^  ezy  ezy
      =+  nao=(~(aeon ze lim dom) q.mun)
      ::  ~&  [%aver-mun nao [%from syd lim q.mun]]
      ?~(nao ~ [~ (~(avid ze lim dom) u.nao mun)])
    ::
    ++  balk                                          ::  read and send
      |=  [hen=duct oan=@ud mun=mood]
      ^+  +>
      =+  vid=(~(avid ze lim dom) oan mun)
      ?~  vid  (blob hen)  (blab hen mun u.vid)
    ::
    ++  blab                                          ::  ship result
      |=  [hen=duct mun=mood dat=*]
      ^+  +>
      +>(byn [[hen ~ [p.mun q.mun syd] r.mun dat] byn])
    ::
    ++  bleb                                          ::  ship sequence
      |=  [hen=duct ins=@ud hip=(list frog)]
      ^+  +>
      ?~  hip  +>
      %=  $
        hip  t.hip
        ins  +(ins)
        +>   (blab hen [%w [%ud ins] ~] i.hip)
      ==
    ::
    ++  blob                                          ::  ship stop
      |=  hen=duct
      %_(+> byn [[hen ~] byn])
    ::
    ++  doze                                          ::  sleep until
      =+  xiq=(~(tap by qyx) ~)
      =|  nex=(unit ,@da)
      |-  ^+  nex
      ?~  xiq  nex
      =+  ^=  zis  ^+  nex
          ?-    -.q.i.xiq
              &  ?.(?=(%da -.q.p.q.i.xiq) ~ [~ p.q.p.q.i.xiq])
          ::
              |
            =+  mot=`moat`p.q.i.xiq
            %+  hunt
              ?.  ?=(%da -.p.mot)  ~
              ?.((lth now p.p.mot) ~ [~ p.p.mot])
            ?.  ?=(%da -.q.mot)  ~
            ?.((lth now p.q.mot) [~ now] [~ p.q.mot])
          ==
      $(xiq t.xiq, nex (hunt nex zis))
    ::
    ++  duce                                          ::  produce request
      |=  [hen=duct rav=rave]
      ^+  +>
      =.  qyx  (~(put by qyx) hen rav)
      ?~  ref  +>
      |-  ^+  +>+.$
      =+  ^=  vaw  ^-  rave
        ?.  ?=([%& %v *] rav)  rav
        [%| [%ud let.dom] `case`q.p.rav]
      =+  inx=nix.u.ref
      %=  +>+.$
        say        [[hen [(scot %ud inx) ~] for [inx syd ~ vaw]] say]
        nix.u.ref  +(nix.u.ref)
        bom.u.ref  (~(put by bom.u.ref) inx [hen vaw])
        fod.u.ref  (~(put by fod.u.ref) hen inx)
      ==
    ::
    ++  ease                                          ::  release request
      |=  hen=duct
      ^+  +>
      =.  qyx  (~(del by qyx) hen)
      ?~  ref  +>
      |-  ^+  +>+.$
      =+  nux=(~(get by fod.u.ref) hen)
      ?~  nux  +>+.$
      %=  +>+.$
        say        [[hen [(scot %ud u.nux) ~] for [u.nux syd ~]] say]
        fod.u.ref  (~(del by fod.u.ref) hen)
        bom.u.ref  (~(del by bom.u.ref) u.nux)
      ==
    ::
    ++  eave                                          ::  subscribe
      |=  [hen=duct rav=rave]
      ^+  +>
      ?-    -.rav
          &
        =+  ver=(aver p.rav)
        ?~  ver
          (duce hen rav)
        ?~  u.ver
          (blob hen)
        (blab hen p.rav u.u.ver)
      ::
          |
        =+  nab=(~(aeon ze lim dom) p.p.rav)
        ?~  nab
          ?>  =(~ (~(aeon ze lim dom) q.p.rav))
          (duce hen rav)
        =+  huy=(~(aeon ze lim dom) q.p.rav)
        ?:  &(?=(^ huy) |((lth u.huy u.nab) &(=(0 u.huy) =(0 u.nab))))
          (blob hen)
        =+  top=?~(huy let.dom u.huy)
        =+  seb=(slag (sub let.dom top) hit.dom)
        =+  wid=(sub top u.nab)
        =+  fud=(flop (scag wid seb))
        =.  +>.$  (bleb hen u.nab fud)
        ?^  huy
          (blob hen)
        =+  ^=  ptr  ^-  case
            ?:  =(0 u.nab)  [%da _@da]
            =+(old=(slag wid seb) ?>(?=(^ old) `case`[%da p.i.old]))
        (duce hen `rave`[%| ptr q.p.rav])
      ==
    ::
    ++  echo                                          ::  announce changes
      |=  [hen=duct wen=@da lem=nori]
      ^+  +>
      %=    +>
          vag  ?~(hez vag :_(vag [u.hez [%ergo who syd let.dom]]))
          yel
        =+  pre=`path`~[(scot %p for) syd (scot %ud let.dom)]
        ?-  -.lem
          |  :_  yel
             [hen %note '=' %leaf :(weld (trip p.lem) " " (spud pre))]
          &  |-  ^+  yel
             ?~  q.q.lem  yel
             :_  $(q.q.lem t.q.q.lem)
             :-  hen
             :+  %note
               ?-(-.q.i.q.q.lem %del '-', %ins '+', %mut ':')
             [%leaf (spud (weld pre p.i.q.q.lem))]
        ==
      ==
    ::
    ++  edit                                          ::  apply changes
      |=  [wen=@da lem=nori]
      ^+  +>
      +>(dom +<+:(~(axel ze lim dom) wen lem))
    ::
    ++  exec                                          ::  change and update
      |=  [hen=duct wen=@da lem=nori]
      ^+  +>
      (echo:wake:(edit wen lem) hen wen lem)
    ::
    ++  knit                                          ::  external change
      |=  [inx=@ud rot=riot]
      ^+  +>
      ?>  ?=(^ ref)
      |-  ^+  +>+.$
      =+  ruv=(~(get by bom.u.ref) inx)
      ?~  ruv  +>+.$
      =>  ?.  |(?=(~ rot) ?=(& -.q.u.ruv))  .
          %_  .
            bom.u.ref  (~(del by bom.u.ref) inx)
            fod.u.ref  (~(del by fod.u.ref) p.u.ruv)
          ==
      ?~  rot
        =+  rav=`rave`q.u.ruv
        %=    +>+.$
            lim
          ?.(&(?=(| -.rav) ?=(%da -.q.p.rav)) lim `@da`p.q.p.rav)
        ::
            haw.u.ref
          ?.  ?=(& -.rav)  haw.u.ref
          (~(put by haw.u.ref) p.rav ~)
        ==
      ?<  ?=(%v p.p.u.rot)
      =.  haw.u.ref
        (~(put by haw.u.ref) [p.p.u.rot q.p.u.rot q.u.rot] ~ r.u.rot)
      ?.  ?=(%w p.p.u.rot)  +>+.$
      |-  ^+  +>+.^$
      =+  nez=[%w [%ud let.dom] ~]
      =+  nex=(~(get by haw.u.ref) nez)
      ?~  nex  +>+.^$
      ?~  u.nex  +>+.^$  ::  should never happen
      %=  $
        haw.u.ref  (~(del by haw.u.ref) nez)
        +>+.^$     =+  roo=(edit ((hard frog) u.u.nex))
                   ?>(?=(^ ref.roo) roo)
      ==
    ::
    ++  wake                                          ::  update subscribers
      ^+  .
      =+  xiq=(~(tap by qyx) ~)
      =|  xaq=(list ,[p=duct q=rave])
      |-  ^+  ..wake
      ?~  xiq
        ..wake(qyx (~(gas by *cult) xaq))
      ?-    -.q.i.xiq
          &
        =+  cas=?~(ref ~ (~(get by haw.u.ref) `mood`p.q.i.xiq))
        ?^  cas
          %=    $
              xiq     t.xiq
              ..wake  ?~  u.cas  (blob p.i.xiq)
                      (blab p.i.xiq p.q.i.xiq u.u.cas)
          ==
        =+  nao=(~(aeon ze lim dom) q.p.q.i.xiq)
        ?~  nao  $(xiq t.xiq, xaq [i.xiq xaq])
        $(xiq t.xiq, ..wake (balk p.i.xiq u.nao p.q.i.xiq))
      ::
          |
        =+  mot=`moat`p.q.i.xiq
        =+  nab=(~(aeon ze lim dom) p.mot)
        ?~  nab
          $(xiq t.xiq, xaq [i.xiq xaq])
        =+  huy=(~(aeon ze lim dom) q.mot)
        ?~  huy
          ?:  =(let.dom u.nab)
            $(xiq t.xiq, xaq [i.xiq xaq])
          ?>  ?=(^ hit.dom)
          =+  ptr=[%da p.i.hit.dom]
          =+  fud=(flop (scag (sub let.dom u.nab) `(list frog)`hit.dom))
          %=  $
            xiq     t.xiq
            xaq     [[p.i.xiq [%| ptr q.mot]] xaq]
            ..wake  (bleb p.i.xiq let.dom fud)
          ==
        =+  yad=(slag (sub let.dom u.huy) `(list frog)`hit.dom)
        =+  fud=(flop (scag (sub u.huy u.nab) yad))
        %=  $
          xiq     t.xiq
          ..wake  (blob:(bleb p.i.xiq +(u.nab) fud) p.i.xiq)
        ==
      ==
    --
  ::
  ++  do
    |=  [now=@da [who=ship him=ship] syd=@tas ruf=raft]
    =+  ^=  rug  ^-  rung
        =+  rug=(~(get by hoy.ruf) him)
        ?^(rug u.rug *rung)
    =+  ^=  red  ^-  rede
        =+  yit=(~(get by rus.rug) syd)
        ?^(yit u.yit `rede`[~2000.1.1 ~ [~ *rind] *dome])
    ((de now ~ ~) [who him] syd red)
  ::
  ++  posh
    |=  [him=ship syd=desk red=rede ruf=raft]
    ^-  raft
    =+  ^=  rug  ^-  rung
        =+  rug=(~(get by hoy.ruf) him)
        ?^(rug u.rug *rung)
    ruf(hoy (~(put by hoy.ruf) him rug(rus (~(put by rus.rug) syd red))))
  ::
  ++  un                                                ::  domestic ship
    |=  [who=@p now=@da ruf=raft]
    =+  ^=  yar  ^-  room
        =+  yar=(~(get by fat.ruf) who)
        ?~(yar *room u.yar)
    |%
    ++  abet  ruf(fat (~(put by fat.ruf) who yar))
    ++  doze
      =+  saz=(turn (~(tap by dos.yar) ~) |=([a=@tas b=*] a))
      =|  nex=(unit ,@da)
      |-  ^+   nex
      ?~  saz  nex
      $(saz t.saz, nex (hunt nex doze:(di i.saz)))
    ::
    ++  pish
      |=  [syd=@ta red=rede]
      %_(+> dos.yar (~(put by dos.yar) syd [qyx.red dom.red]))
    ::
    ++  wake
      =+  saz=(turn (~(tap by dos.yar) ~) |=([a=@tas b=*] a))
      =|  moz=(list move)
      =<  [moz ..wake]
      |-  ^+  +
      ?~  saz  +
      =+  sog=abet:wake:(di i.saz)
      $(saz t.saz, moz (weld moz -.sog), ..wake (pish i.saz +.sog))
    ::
    ++  di
      |=  syd=@ta
      =+  ^=  saq  ^-  dojo
          =+  saq=(~(get by dos.yar) syd)
          ?~(saq *dojo u.saq)
      ((de now hun.yar hez.yar) [who who] syd now p.saq ~ q.saq)
    --
  --
  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ::              section 4cA, filesystem vane          ::
  ::                                                    ::
  =|                                                    ::  instrument state
      $:  ruf=raft                                      ::  revision tree
      ==                                                ::
  |=  [now=@da eny=@ ska=$+(* (unit (unit)))]           ::  activate
  ^?                                                    ::  opaque core
  |%                                                    ::
  ++  take                                              ::  update
    |=  [tea=wire hen=duct typ=type fav=card]
    ^-  [p=(list move) q=vane]
    ?+    -.fav  [[[hen %give fav] ~] ..^$]
        %crud
      [[[hen %slip %d %flog fav] ~] ..^$]
    ::
        %soft
      $(fav ((hard card) p.fav))
    ::
        %init
      [~ ..^$(fat.ruf (~(put by fat.ruf) p.fav [hen ~ ~]))]
    ::
        ?(%info %into)
      ?:  =(%$ q.fav)
        ?.  ?=(%into -.fav)  [~ ..^$]
        =+  yar=(need (~(get by fat.ruf) p.fav))
        [~ ..^$(fat.ruf (~(put by fat.ruf) p.fav yar(hez [~ hen])))]
      =^  mos  ruf
        =+  une=(un p.fav now ruf)
        =+  zot=abet:(exec:(di:wake:une q.fav) hen now r.fav)
        :-  -.zot
        =.  une  (pish:une q.fav +.zot)
        abet:une(hez.yar ?.(=(%into -.fav) hez.yar.une [~ hen]))
      [mos ..^$]
    ::
        %waft
      ?>  ?=([@ @ ~] tea)
      =+  syd=(need (slaw %tas i.tea))
      =+  inx=(need (slaw %ud i.t.tea))
      =^  mos  ruf
        =+  ^=  zot
          abet:wake:(knit:(do now p.fav syd ruf) [inx ((hard riot) q.fav)])
        [-.zot (posh q.p.fav syd +.zot ruf)]
      [mos ..^$]
    ::
        %warp
      =^  mos  ruf
        ?:  =(p.p.fav q.p.fav)
          =+  une=(un p.p.fav now ruf)
          =+  wex=(di:une p.q.fav)
          =+  ^=  woo
            ?~  q.q.fav
              abet:(ease:wex hen)
            abet:(eave:wex hen u.q.q.fav)
          [-.woo abet:(pish:une p.q.fav +.woo)]
        =+  wex=(do now p.fav p.q.fav ruf)
        =+  ^=  woo
          ?~  q.q.fav
            abet:(ease:wex hen)
          abet:(eave:wex hen u.q.q.fav)
        [-.woo (posh q.p.fav p.q.fav +.woo ruf)]
      [mos ..^$]
    ::
        %wart
      ?>  ?=(%re q.fav)
      =+  ryf=((hard riff) s.fav)
      :_  ..^$
      :~  :-  hen
          :^  %toss  %c
            [(scot %p p.p.fav) (scot %p q.p.fav) r.fav]
          [%warp [p.p.fav p.p.fav] ryf]
      ==
    ::
        %writ
      ?>  ?=([@ @ *] tea)
      =+  our=(need (slaw %p i.tea))
      =+  him=(need (slaw %p i.t.tea))
      :_  ..^$
      :~  :-  hen
          [%toss %a ~ [%want [our him] [%r %re %c t.t.tea] p.fav]]
      ==
    ::
        %went                             ::  XX should actually propagate
      ?:  =(%good q.fav)  [~ ..^$]
      ~&  [%clay-lost p.fav tea]
      [~ ..^$]
    ::
        %wake
      =+  dal=(turn (~(tap by fat.ruf) ~) |=([a=@p b=room] a))
      =|  mos=(list move)
      |-  ^-  [p=(list move) q=vane]
      ?~  dal  [mos ..^^$]
      =+  une=(un i.dal now ruf)
      =^  som  une  wake:une
      $(dal t.dal, ruf abet:une, mos (weld som mos))
    ==
  ::
  ++  call                                                ::  process move
    |=  [hen=duct typ=type fav=card]
    (take ~ hen typ fav)
  ::
  ++  come
    |=  [sam=? old=vase]
    ^-  vane
    (load old)
  ::
  ++  doze
    |=  [now=@da hen=duct]
    =|  nex=(unit ,@da)
    =+  dal=(turn (~(tap by fat.ruf) ~) |=([a=@p b=room] a))
    |-  ^+  nex
    ?~  dal  nex
    $(dal t.dal, nex (hunt nex doze:(un i.dal now ruf)))
  ::
  ++  load
    |=  old=vase
    ^-  vane
    ?.  (~(nest ut -:!>(ruf)) & p.old)
      ~&  %clay-reset
      ..^$
    ..^$(ruf (raft q.old))
  ::
  ++  raze
    ^-  vane
    ..$(ruf *raft)
  ::
  ++  scry                                              ::  inspect
    |=  [fur=(unit (set monk)) ren=@tas his=ship syd=desk lot=coin tyl=path]
    ^-  (unit (unit))
    =+  got=(~(has by fat.ruf) his)
    =+  luk=?.(?=(%$ -.lot) ~ ((soft case) p.lot))
    ?~  luk  [~ ~]
    =+  une=(un his now ruf)
    ?:  =(%$ ren)
      [~ ~]
    =+  run=((soft care) ren)
    ?~  run  [~ ~]
    %.  [u.run u.luk tyl]
    =+  dud=?.(got (do now [his his] syd ruf) (di:une syd))
    aver:dud
  ::
  ++  stay  `vase`!>(ruf)
  ++  vern  [164 0]
  --
