!:
::  clay (4c), revision control
::
!:
|=  pit=vase
=>  |%
++  cult  (map duct rave)                               ::  subscriptions
++  dojo  ,[p=cult q=dome]                              ::  domestic desk state
++  gift                                                ::  out result <-$
          $%  [%ergo p=@p q=@tas r=@ud]                 ::  version update
              [%note p=@tD q=tank]                      ::  debug message
              [%writ p=riot]                            ::  response
          ==                                            ::
++  kiss                                                ::  in request ->$
          $%  [%info p=@p q=@tas r=nori]                ::  internal edit
              [%ingo p=@p q=@tas r=nori]                ::  internal noun edit
              [%init p=@p]                              ::  report install
              [%into p=@p q=@tas r=nori]                ::  external edit
              [%invo p=@p q=@tas r=nori]                ::  external noun edit
              [%merg p=@p q=@tas r=mizu]                ::  internal change
              [%wake ~]                                 ::  timer activate
              [%wart p=sock q=@tas r=path s=*]          ::  network request
              [%warp p=sock q=riff]                     ::  file request
          ==                                            ::
++  move  ,[p=duct q=(mold note gift)]                  ::  local move
++  nako  $:  gar=(map ,@ud tako)                       ::  new ids
              let=@ud                                   ::  next id
              lar=(set yaki)                            ::  new commits
              bar=(set blob)                            ::  new content
          ==                                            ::
++  note                                                ::  out request $->
          $%  $:  %a                                    ::  to %ames
          $%  [%want p=sock q=path r=*]                 ::
          ==  ==                                        ::
              $:  %c                                    ::  to %clay
          $%  [%warp p=sock q=riff]                     ::
          ==  ==                                        ::
              $:  %d                                    ::
          $%  [%flog p=[%crud p=@tas q=(list tank)]]    ::  to %dill
          ==  ==  ==                                    ::
++  sign                                                ::  in result $<-
          $?  $:  %a                                    ::  by %ames
          $%  [%waft p=sock q=*]                        ::
              [%went p=ship q=cape]                     ::
          ==  ==                                        ::
              $:  %c                                    ::  by %clay
          $%  [%writ p=riot]                            ::
          ==  ==                                        ::
              $:  @tas                                  ::  by any
          $%  [%crud p=@tas q=(list tank)]              ::
          ==  ==  ==                                    ::
++  raft                                                ::  filesystem
          $:  fat=(map ship room)                       ::  domestic
              hoy=(map ship rung)                       ::  foreign
              ran=rang                                  ::  hashes
          ==                                            ::
++  rave                                                ::  general request
          $%  [& p=mood]                                ::  single request
              [| p=moat]                                ::  change range
          ==                                            ::
++  rede                                                ::  universal project
          $:  lim=@da                                   ::  complete to
              qyx=cult                                  ::  subscribers
              ref=(unit rind)                           ::  outgoing requests
              dom=dome                                  ::  revision state
          ==                                            ::
++  riff  ,[p=desk q=(unit rave)]                       ::  request/desist
++  rind                                                ::  request manager
          $:  nix=@ud                                   ::  request index
              bom=(map ,@ud ,[p=duct q=rave])           ::  outstanding
              fod=(map duct ,@ud)                       ::  current requests
              haw=(map mood (unit))                     ::  simple cache
          ==                                            ::
++  room                                                ::  fs per ship
          $:  hun=duct                                  ::  terminal duct
              hez=(unit duct)                           ::  sync duch
              dos=(map desk dojo)                       ::  native desk
          ==                                            ::
++  rung  $:  rus=(map desk rede)                       ::  neighbor desks
          ==                                            ::
--  =>
  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ::              section 4cA, filesystem logic         ::
  ::
  ::
  ::
  |%
  ++  de                                              ::  per desk
    |=  [now=@da hun=duct hez=(unit duct)]
    |=  [[who=@p for=@p] syd=@ta rede ran=rang]
    =*  red  +<+>-
    =|  yel=(list ,[p=duct q=gift])
    =|  byn=(list ,[p=duct q=riot])
    =|  vag=(list ,[p=duct q=gift])
    =|  say=(list ,[p=duct q=path r=ship s=[p=@ud q=riff]])
    |%
    ++  abet
      ^-  [(list move) rede]
      :_  red
      ;:  weld
        %+  turn  (flop yel)
        |=([a=duct b=gift] [hun %give b])
      ::
        %+  turn  (flop byn)
        |=([a=duct b=riot] [a %give [%writ b]])
      ::
        %+  turn  (flop vag)
        |=([a=duct b=gift] [a %give b])
      ::
        %+  turn  (flop say)
        |=  [a=duct b=path c=ship d=[p=@ud q=riff]]
        :-  a
        [%pass b %a %want [who c] [%q %re p.q.d (scot %ud p.d) ~] q.d]
      ==
    ::
    ++  aver                                          ::  read
      |=  mun=mood
      ^-  (unit (unit ,*))
      ?:  &(=(p.mun %u) !=(p.q.mun now))              ::  prevent bad things
        ~&  [%clay-fail p.q.mun %now now]
        !!
      =+  ezy=?~(ref ~ (~(get by haw.u.ref) mun))
      ?^  ezy  ezy
      =+  nao=(~(aeon ze lim dom ran) q.mun)
      ::  ~&  [%aver-mun nao [%from syd lim q.mun]]
      ?~(nao ~ [~ (~(avid ze lim dom ran) u.nao mun)])
    ::
    ++  balk                                          ::  read and send
      |=  [hen=duct oan=@ud mun=mood]
      ^+  +>
      =+  vid=(~(avid ze lim dom ran) oan mun)
      ?~  vid  (blub hen)  (blab hen mun u.vid)
    ::
    ++  blab                                          ::  ship result
      |=  [hen=duct mun=mood dat=*]
      ^+  +>
      +>(byn [[hen ~ [p.mun q.mun syd] r.mun dat] byn])
    ::
    ++  bleb                                          ::  ship sequence
      |=  [hen=duct ins=@ud hip=nako]
      ^+  +>
      (blab hen [%w [%ud ins] ~] hip)
    ::
    ++  blub                                          ::  ship stop
      |=  hen=duct
      %_(+> byn [[hen ~] byn])
    ::
    ++  doze                                          ::  sleep until
      =+  xiq=(~(tap by qyx) ~)
      ^-  (unit ,@da)
      %+  roll  xiq
      |=  [xaq=[p=duct q=rave] nex=(unit ,@da)]
      %+  hunt  nex
      ?-    -.q.xaq
          &  ?.(?=(%da -.q.p.q.xaq) ~ [~ p.q.p.q.xaq])
          ::
          |
        =+  mot=`moat`p.q.xaq
          %+  hunt
            ?.  ?=(%da -.p.mot)  ~
            ?.((lth now p.p.mot) ~ [~ p.p.mot])
          ?.  ?=(%da -.q.mot)  ~
          ?.((lth now p.q.mot) [~ now] [~ p.q.mot])
        ==
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
        ?:  &(=(p.p.rav %u) !=(p.q.p.rav now))
          ~&  [%clay-fail p.q.p.rav %now now]
          !!
        =+  ver=(aver p.rav)
        ?~  ver
          (duce hen rav)
        ?~  u.ver
          (blub hen)
        (blab hen p.rav u.u.ver)
      ::
          |
        =+  nab=(~(aeon ze lim dom ran) p.p.rav)
        ?~  nab
          ?>  =(~ (~(aeon ze lim dom ran) q.p.rav))
          (duce hen rav)
        =+  huy=(~(aeon ze lim dom ran) q.p.rav)
        ?:  &(?=(^ huy) |((lth u.huy u.nab) &(=(0 u.huy) =(0 u.nab))))
          (blub hen)
        =+  top=?~(huy let.dom u.huy)
        =+  fud=(~(gack ze lim dom ran) u.nab let.dom)
        =.  +>.$  (bleb hen u.nab fud)
        ?^  huy
          (blub hen)
        =+  ^=  ptr  ^-  case
            [%ud +(let.dom)]
        (duce hen `rave`[%| ptr q.p.rav])
      ==
    ::
    ++  echa                                          ::  announce raw
      |=  [hen=duct wen=@da mer=mizu]
      ^+  +>
      %=    +>
          vag  ?~(hez vag :_(vag [u.hez [%ergo who syd let.dom]]))
          ::yel  [[hen %note '=' %leaf ~] yel]     ::  XX do better
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
      =+  axe=(~(axel ze lim dom ran) wen lem)
      =+  `[l=@da d=dome r=rang]`+<.axe
      +>.$(dom d, ran r)
    ::
    ++  edis                                          ::  apply subscription
      |=  nak=nako
      ^+  +>
      %=  +>
        hit.dom  (~(uni by hit.dom) gar.nak)
        let.dom  let.nak
        lat.ran  %+  roll  (~(tap in bar.nak) ~)
                 =<  .(yeb lat.ran)
                 |=  [sar=blob yeb=(map lobe blob)]
                 =+  zax=(zaax sar)
                 %+  ~(put by yeb)  zax  sar
        hut.ran  %+  roll  (~(tap in lar.nak) ~)
                 =<  .(yeb hut.ran)
                 |=  [sar=yaki yeb=(map tako yaki)]
                 %+  ~(put by yeb)  r.sar  sar
      ==
    ::
    ++  exec                                          ::  change and update
      |=  [hen=duct wen=@da lem=nori]
      ^+  +>
      (echo:wake:(edit wen lem) hen wen lem)
    ::
    ++  exem                                          ::  execute merge
      |=  [hen=duct wen=@da mer=mizu]                 ::  aka direct change
      ?.  (gte p.mer let.dom)  !!                     ::  no
      =.  +>.$  %=  +>.$
                  hut.ran  (~(uni by hut.r.mer) hut.ran)
                  lat.ran  (~(uni by lat.r.mer) lat.ran)
                  let.dom  p.mer
                  hit.dom  (~(uni by q.mer) hit.dom)
                ==
      =+  ^=  hed                                     ::  head commit
          =<  q
          %-  ~(got by hut.ran)
          %-  ~(got by hit.dom)
          let.dom
      =.  ank.dom  (~(azel ze lim dom ran) hed)       ::  real checkout
      (echa:wake hen wen mer)                         ::  notify or w/e
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
      =.  +>+.^$     =+  roo=(edis ((hard nako) u.u.nex))
                     ?>(?=(^ ref.roo) roo)
      %=  $
        haw.u.ref  (~(del by haw.u.ref) nez)
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
              xiq  t.xiq
              ..wake  ?~  u.cas  (blub p.i.xiq)
                      (blab p.i.xiq p.q.i.xiq u.u.cas)
          ==
        =+  nao=(~(aeon ze lim dom ran) q.p.q.i.xiq)
        ?~  nao  $(xiq t.xiq, xaq [i.xiq xaq])
        $(xiq t.xiq, ..wake (balk p.i.xiq u.nao p.q.i.xiq))
      ::
          |
        =+  mot=`moat`p.q.i.xiq
        =+  nab=(~(aeon ze lim dom ran) p.mot)
        ?~  nab
          $(xiq t.xiq, xaq [i.xiq xaq])
        =+  huy=(~(aeon ze lim dom ran) q.mot)
        ?~  huy
          ?:  =(let.dom u.nab)
            $(xiq t.xiq, xaq [i.xiq xaq])
          ?>  ?=(^ hit.dom)
          =+  ptr=[%ud +(let.dom)]
          ::=+  fud=(flop (scag (sub let.dom u.nab) `(list frog)`hit.dom))
          =+  fud=`nako`(~(gack ze lim dom ran) u.nab let.dom)
          %=  $
            xiq     t.xiq
            xaq     [[p.i.xiq [%| ptr q.mot]] xaq]
            ..wake  (bleb p.i.xiq let.dom fud)
          ==
        ::=+  fud=(flop (scag (sub u.huy u.nab) yad))
        =+  fud=(~(gack ze lim dom ran) u.nab u.huy)
        %=  $
          xiq     t.xiq
          ..wake  (blub:(bleb p.i.xiq +(u.nab) fud) p.i.xiq)
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
    ((de now ~ ~) [who him] syd red ran.ruf)
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
      ^-  (unit ,@da)
      %+  roll  (~(tap by dos.yar) ~)
      |=  [[saz=@tas *] nex=(unit ,@da)]
      %+  hunt  nex
      doze:(di saz)
    ::
    ++  pish
      |=  [syd=@ta red=rede run=rang]
      %_(+> dos.yar (~(put by dos.yar) syd [qyx.red dom.red]), ran.ruf run)
    ::
    ++  wake
      =+  saz=(turn (~(tap by dos.yar) ~) |=([a=@tas b=*] a))
      =|  moz=(list move)
      =<  [moz ..wake]
      |-  ^+  +
      ?~  saz  +
      =+  sog=abet:wake:(di i.saz)
      $(saz t.saz, moz (weld moz -.sog), ..wake (pish i.saz +.sog ran.ruf))
    ::
    ++  di
      |=  syd=@ta
      =+  ^=  saq  ^-  dojo
          =+  saq=(~(get by dos.yar) syd)
          ?~(saq *dojo u.saq)
      ((de now hun.yar hez.yar) [who who] syd [now p.saq ~ q.saq] ran.ruf)
    --
  --
  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ::              section 4cA, filesystem vane          ::
  ::                                                    ::
  =|                                                    ::  instrument state
      $:  %0                                            ::  vane version
          ruf=raft                                      ::  revision tree
      ==                                                ::
  |=  [now=@da eny=@ ski=sled]                          ::  activate
  ^?                                                    ::  opaque core
  |%                                                    ::
  ++  call                                              ::  handle request
    |=  $:  hen=duct
            hic=(hypo (hobo kiss))
        ==
    =>  %=    .                                         ::  XX temporary
            q.hic
          ^-  kiss
          ?:  ?=(%soft -.q.hic)
            ((hard kiss) p.q.hic)
          ?:  (~(nest ut -:!>(*kiss)) | p.hic)  q.hic
          ~&  [%clay-call-flub (,@tas `*`-.q.hic)]
          ((hard kiss) q.hic)
        ==
    ^-  [p=(list move) q=_..^$]
    ?-    -.q.hic
        %init
      [~ ..^$(fat.ruf (~(put by fat.ruf) p.q.hic [hen ~ ~]))]
    ::
        %merg                                               ::  direct state up
      =^  mos  ruf
        =+  une=(un p.q.hic now ruf)
        =+  ^=  zat
            (exem:(di:wake:une q.q.hic) hen now r.q.hic)
        =+  zot=abet.zat
        :-  -.zot
        =.  une  (pish:une q.q.hic +.zot ran.zat)
        abet:une(hez.yar ?.(=(%into -.q.hic) hez.yar.une [~ hen]))
      [mos ..^$]
      ::
        ?(%info %into)
      ?:  =(%$ q.q.hic)
        ?.  ?=(%into -.q.hic)  [~ ..^$]
        =+  yar=(need (~(get by fat.ruf) p.q.hic))
        [~ ..^$(fat.ruf (~(put by fat.ruf) p.q.hic yar(hez [~ hen])))]
      =^  mos  ruf
        =+  une=(un p.q.hic now ruf)
        =+  ^=  zat
            (exec:(di:wake:une q.q.hic) hen now r.q.hic)
        =+  zot=abet.zat
        :-  -.zot
        =.  une  (pish:une q.q.hic +.zot ran.zat)
        abet:une(hez.yar ?.(=(%into -.q.hic) hez.yar.une [~ hen]))
      [mos ..^$]
    ::
        ?(%ingo %invo)                                   ::  not yet used
      ?:  =(%$ q.q.hic)
        ?.  ?=(%invo -.q.hic)  [~ ..^$]
        =+  yar=(need (~(get by fat.ruf) p.q.hic))
        [~ ..^$(fat.ruf (~(put by fat.ruf) p.q.hic yar(hez [~ hen])))]
      =^  mos  ruf
        =+  une=(un p.q.hic now ruf)
        =+  ^=  zat
            (exec:(di:wake:une q.q.hic) hen now r.q.hic)
        =+  zot=abet:zat
        :-  -.zot
        =.  une  (pish:une q.q.hic +.zot ran.zat)
        abet:une(hez.yar ?.(=(%invo -.q.hic) hez.yar.une [~ hen]))
      [mos ..^$]
    ::
        %warp
      =^  mos  ruf
        ?:  =(p.p.q.hic q.p.q.hic)
          =+  une=(un p.p.q.hic now ruf)
          =+  wex=(di:une p.q.q.hic)
          =+  ^=  wao
            ?~  q.q.q.hic
              (ease:wex hen)
            (eave:wex hen u.q.q.q.hic)
          =+  ^=  woo
            abet:wao
          [-.woo abet:(pish:une p.q.q.hic +.woo ran.wao)]
        =+  wex=(do now p.q.hic p.q.q.hic ruf)
        =+  ^=  woo
          ?~  q.q.q.hic
            abet:(ease:wex hen)
          abet:(eave:wex hen u.q.q.q.hic)
        [-.woo (posh q.p.q.hic p.q.q.hic +.woo ruf)]
      [mos ..^$]
    ::
        %wart
      ?>  ?=(%re q.q.hic)
      =+  ryf=((hard riff) s.q.hic)
      :_  ..^$
      :~  :-  hen
          :^  %pass  [(scot %p p.p.q.hic) (scot %p q.p.q.hic) r.q.hic]
            %c
          [%warp [p.p.q.hic p.p.q.hic] ryf]
      ==
    ::
        %wake
      =+  dal=(turn (~(tap by fat.ruf) ~) |=([a=@p b=room] a))
      =|  mos=(list move)
      |-  ^-  [p=(list move) q=_..^^$]
      ?~  dal  [mos ..^^$]
      =+  une=(un i.dal now ruf)
      =^  som  une  wake:une
      $(dal t.dal, ruf abet:une, mos (weld som mos))
    ==
  ::
  ++  doze
    |=  [now=@da hen=duct]
    ^-  (unit ,@da)
    %+  roll  (~(tap by fat.ruf) ~)
    |=  [[dal=@p *] nex=(unit ,@da)]
    %+  hunt  nex
    doze:(un dal now ruf)
  ::
  ++  load
    |=  old=[%0 ruf=raft]
    ^+  ..^$
    ..^$(ruf ruf.old)
  ::
  ++  scry                                              ::  inspect
    |=  [fur=(unit (set monk)) ren=@tas his=ship syd=desk lot=coin tyl=path]
    ^-  (unit (unit (pair mark ,*)))
    =+  got=(~(has by fat.ruf) his)
    =+  luk=?.(?=(%$ -.lot) ~ ((soft case) p.lot))
    ?~  luk  [~ ~]
    =+  une=(un his now ruf)
    ?:  =(%$ ren)
      [~ ~]
    =+  run=((soft care) ren)
    ?~  run  [~ ~]
    %+  bind
      %.  [u.run u.luk tyl]
      aver:?.(got (do now [his his] syd ruf) (di:une syd))
    |=(a=(unit) (bind a |=(b=* [%noun b])))
  ::
  ++  stay  [%0 ruf]
  ++  take                                              ::  accept response
    |=  [tea=wire hen=duct hin=(hypo sign)]
    ^-  [p=(list move) q=_..^$]
    ?-    -.+.q.hin
        %crud
      [[[hen %slip %d %flog +.q.hin] ~] ..^$]
    ::
        %waft
      ?>  ?=([@ @ ~] tea)
      =+  syd=(need (slaw %tas i.tea))
      =+  inx=(need (slaw %ud i.t.tea))
      =+  ^=  zat
        =<  wake
        (knit:(do now p.+.q.hin syd ruf) [inx ((hard riot) q.+.q.hin)])
      =^  mos  ruf
        =+  zot=abet.zat
        [-.zot (posh q.p.+.q.hin syd +.zot ruf)]
      [mos ..^$(ran.ruf ran.zat)]                         ::  merge in new obj
    ::
        %writ
      ?>  ?=([@ @ *] tea)
      =+  our=(need (slaw %p i.tea))
      =+  him=(need (slaw %p i.t.tea))
      :_  ..^$
      :~  :-  hen
          [%pass ~ %a [%want [our him] [%r %re %c t.t.tea] p.+.q.hin]]
      ==
    ::
        %went
      ?:  =(%good q.+.q.hin)  [~ ..^$]
      ~&  [%clay-lost p.+.q.hin tea]
      [~ ..^$]
    ==
  --
