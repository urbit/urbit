!:
::  clay (4c), revision control
::
|=  pit=vase
^-  vane
=>
  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ::              section 4cA, filesystem logic         ::
  ::
  |%
  ::
  ++  un                                                ::  per ship
    |=  [who=@p now=@da ruf=raft]
    =+  ^=  yar  ^-  room
        =+  yar=(~(get by fat.ruf) who)
        ?~(yar *room u.yar)
    =|  yel=(list ,[p=duct q=card])
    =|  wot=(list ,[p=duct q=tape])
    =|  byn=(list ,[p=duct q=riot]) 
    =|  vag=(list ,[p=duct q=card])
    =|  say=(list ,[p=duct q=ship r=[p=@ud q=riff]])
    |%
    ++  abet
      ^-  [(list move) raft]
      :_  ruf(fat (~(put by fat.ruf) who yar))
      ;:  weld
        %+  turn  (flop yel)
        |=([a=duct b=card] [[~ %gold who] hun.yar b])
      ::
        %+  turn  (flop wot) 
        |=([a=duct b=tape] [[~ %gold who] a [%wort b]])
      ::
        %+  turn  (flop byn) 
        |=([a=duct b=riot] [[~ %gold who] a [%writ b]])
      ::
        %+  turn  (flop vag) 
        |=([a=duct b=card] [[~ %gold who] a b])
      ::
        %+  turn  (flop say) 
        |=([a=duct b=ship c=*] [[~ %gold who] [/a a] [%want b %re c]])
      ==
    ::
    ++  doze
      =+  saz=(turn (~(tap by dos.yar) ~) |=([a=@tas b=*] a))
      =|  nex=(unit ,@da)
      |-  ^+   nex
      ?~  saz  nex
      $(saz t.saz, nex (hunt nex doze:(di i.saz)))
    ::
    ++  fray
      |=  [hen=duct pal=(list disc) sab=saba]
      ^+  +>
      ?~  pal  +>
      $(pal t.pal, +> zoot:wake:(exec:(di i.pal) hen now [%| sab]))
    ::
    ++  wake
      ^+  .
      =+  saz=(turn (~(tap by dos.yar) ~) |=([a=@tas b=*] a))
      |-  ^+  ..wake
      ?~  saz  ..wake
      $(saz t.saz, ..wake zoot:wake:(di i.saz))
    ::
    ++  de                                              ::  per desk
      |_  [for=@p syd=@ta lim=@da qyx=cult dom=dome]
      ++  aeon                                          ::  act count through
        |=  lok=case
        ^-  (unit ,@ud)
        ?-    -.lok
            %da
          ?:  (gth p.lok lim)  ~
          |-  ^-  (unit ,@ud)
          ?~  hit.dom  [~ let.dom]
          ?:  (gte p.lok p.i.hit.dom)  [~ let.dom]
          $(hit.dom t.hit.dom, let.dom (dec let.dom))
        :: 
            %tas  (~(get by lab.dom) p.lok)
            %ud   ?:((gth p.lok let.dom) ~ [~ p.lok])
        ==
      ::
      ++  ache                                          ::  apex report
        ^-  apex
        =+  lam=(~(tap by r.ank.dom) ~)
        :+  @uvI
          %-  ~(gas by *(map ,@ta ,@uvI))
          |-  ^-  (list ,[@ta @uvI])
          ?~  lam  ~
          =+  mal=$(lam t.lam)
          ?~  q.q.i.lam  mal
          [[p.i.lam (sham u.q.q.i.lam)] mal]
        %-  ~(gas by *(map ,@ta ,~))
        |-  ^-  (list ,[@ta ~])
        ?~  lam  ~
        =+  mal=$(lam t.lam)
        ?~  r.q.i.lam  mal
        [[p.i.lam ~] mal]
      ::
      ++  amor                                          ::  endpoint query
        |=  ren=?(%x %y %z)
        ^-  (unit ,*)
        ?-  ren
          %x  q.ank.dom
          %y  [~ ache]
          %z  [~ ank.dom]
        ==
      ::
      ++  ante                                          ::  rewind by change
        |=  lem=maki
        ^+  +>
        ?-    -.lem
            &
          %_  +>
            ank.dom  ?.  ?=(& -.p.lem)  ank.dom
                     ank:(dusk:(zu ank.dom) p.p.lem)
            lab.dom  ?.  ?=(| -.p.lem)  lab.dom
                     (~(del by lab.dom) p.p.lem)
          ==
        ::
            |
          |-  ^+  +>.^$
          ?~  s.p.lem  +>.^$
          $(s.p.lem t.s.p.lem, +>.^$ ^$(lem i.s.p.lem))
        ==
      ::
      ++  argo                                          ::  rewind to aeon
        |=  oan=@ud
        ^+  +>
        ?:  =(let.dom oan)  +>
        ?>  ?=(^ hit.dom)
        ?>  ?=(& -.q.i.hit.dom)
        =>  .(+> (ante q.i.hit.dom))
        $(let.dom (dec let.dom), hit.dom t.hit.dom)
      ::
      ++  auto                                          ::  read at point
        |=  mun=mood
        ^-  (unit)
        ?:  &(?=(%w p.mun) !?=(%ud -.q.mun))
          ?^(r.mun ~ [~ let.dom])
        ?:  ?=(%w p.mun)
          ?>  ?=(^ hit.dom)  ?^(r.mun ~ [~ i.hit.dom])
        (amor(ank.dom ank:(deny:(zu ank.dom) r.mun)) p.mun)
      ::
      ++  aver                                          ::  domestic read 
        |=  mun=mood
        ^-  (unit (unit ,*))
        =+  nao=(aeon q.mun)
        ?~(nao ~ [~ (avid u.nao mun)])
      ::
      ++  avid                                          ::  seek and read
        |=  [oan=@ud mun=mood]
        ^-  (unit)
        ?:  &(?=(%w p.mun) !?=(%ud -.q.mun))            ::  NB optimization
          ?^(r.mun ~ [~ oan])
        (auto:(argo oan) mun)
      ::
      ++  balk                                          ::  read and send
        |=  [hen=duct oan=@ud mun=mood]
        ^+  +>
        =+  vid=(avid oan mun)
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
                ?.(&(?=(%da -.p.mot) (lth now p.p.mot)) ~ [~ p.p.mot])
              ?.(&(?=(%da -.q.mot) (lth now p.q.mot)) ~ [~ p.q.mot])
            ==
        $(xiq t.xiq, nex (hunt nex zis))
      ::
      ++  ease                                          ::  unsubscribe
        |=  hen=duct
        ^+  +>
        +>(qyx (~(del by qyx) hen))
      ::
      ++  eave                                          ::  subscribe
        |=  [hen=duct rav=rave]
        ^+  +>
        ?-    -.rav
            &
          =+  ver=(aver p.rav)
          ?~  ver  
            +>.$(qyx (~(put by qyx) hen rav))
          ?~  u.ver
            (blob hen)
          (blab hen p.rav u.u.ver)
        ::
            |
          =+  nab=(aeon p.p.rav)
          ?~  nab
            ?>  =(~ (aeon q.p.rav))
            +>.$(qyx (~(put by qyx) hen rav))
          =+  huy=(aeon q.p.rav)
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
              ?:  =(0 u.nab)  [%da @da]
              =+(old=(slag wid seb) ?>(?=(^ old) `case`[%da p.i.old]))
          +>.$(qyx (~(put by qyx) hen `rave`[%| ptr q.p.rav]))
        ==
      ::
      ++  edit                                          ::  apply changes
        |=  [hen=duct wen=@da lem=maki]
        ^+  +>
        ?-    -.lem
            & 
          %=    +>.$
              ank.dom  ?.  ?=(& -.p.lem)  ank.dom 
                       ank:(durn:(zu ank.dom) p.p.lem)
              let.dom  +(let.dom)
              hit.dom  :_(hit.dom [wen lem])
              lab.dom  ?.  ?=(| -.p.lem)  lab.dom
                       ?<  (~(has by lab.dom) p.p.lem)
                       (~(put by lab.dom) p.p.lem let.dom)
              vag      
            ?~(hez.yar vag :_(vag [u.hez.yar [%ergo who syd +(let.dom)]]))
          ::
              yel      
            =+  pre=`path`~[(scot %p for) syd (scot %ud +(let.dom))]
            ?-  -.p.lem
              |  :_  yel
                 [hen %note '=' %leaf :(weld (trip p.p.lem) " " (spud pre))]
              &  |-  ^+  yel
                 ?~  p.p.lem  yel
                 :_  $(p.p.lem t.p.p.lem)
                 :-  hen
                 :+  %note
                   ?-(-.q.i.p.p.lem %del '-', %ins '+', %mut ':')
                 [%leaf (spud (weld pre p.i.p.p.lem))]
            ==
          ==
        ::
            |  
          |-  ^+  +>.^$
          ?~  s.p.lem  +>.^$
          $(s.p.lem t.s.p.lem, +>.^$ ^$(lem i.s.p.lem))
        ==
      ::
      ++  exec                                          ::  change and update
        |=  [hen=duct wen=@da lem=maki]
        ^+  +>
        wake:(edit hen wen lem)
      ::
      ++  wake                                          ::  update subscribers
        ^+  .
        =+  xiq=(~(tap by qyx) ~)
        =|  xaq=(list ,[p=duct q=rave])
        |-  ^+  ..wake
        ?~  xiq  ..wake(qyx (~(gas by *cult) xaq))
        ?-    -.q.i.xiq
            &
          =+  nao=(aeon q.p.q.i.xiq)
          ?~  nao  $(xiq t.xiq, xaq [i.xiq xaq])
          $(xiq t.xiq, ..wake (balk p.i.xiq u.nao p.q.i.xiq))
        ::
            |
          =+  mot=`moat`p.q.i.xiq
          =+  nab=(aeon p.mot)
          ?:  |(?=(~ nab) =(let.dom u.nab))
            $(xiq t.xiq, xaq [i.xiq xaq])
          ?>  (gte let.dom u.nab)
          ?>  ?=(^ hit.dom)
          =+  huy=(aeon q.mot)
          ?~  huy
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
            ..wake  (blob:(bleb p.i.xiq +(u.nab) fud))
          ==
        ==
      ::
      ++  zoot
        %_(..de dos.yar (~(put by dos.yar) syd qyx dom))
      --
    ::
    ++  di
      |=  syd=@ta
      =+  ^=  saq  ^-  desk
          =+  saq=(~(get by dos.yar) syd)
          ?~(saq [~ [[@uvI ~ ~] 0 ~ ~]] u.saq)
      ~(. de who syd now p.saq q.saq)
    ::
    ++  fa
      |=  him=ship
      =+  ^=  raz
          =+  raz=(~(get by rid.yar) him)
          ?~(raz [p=*rind q=*rink] u.raz)
      |%
      ++  mete                                          ::  foreign request
        |=  [hen=duct ryf=riff]
        ^+  +>
        =+  nux=(~(get by fod.p.raz) hen)
        ?^  nux
          ?>  ?=(~ q.ryf)
          %=  +>.$
            say      [[[[%c (scot %ud u.nux) ~] hen] him [u.nux ryf]] say]
            fod.p.raz  (~(del by fod.p.raz) hen)
            bim.p.raz  (~(del by bim.p.raz) u.nux)
          ==
        ?~  q.ryf  +>.$
        =+  inx=nix.p.raz
        %=  +>.$
          say      [[[[%c (scot %ud inx) ~] hen] him [inx ryf]] say]
          nix.p.raz  +(nix.p.raz)
          bim.p.raz  (~(put by bim.p.raz) inx [hen ryf])
          fod.p.raz  (~(put by fod.p.raz) hen inx)
        ==
      ::
      ++  mote                                          ::  send/cancel request
        |=  [hen=duct ryf=riff]
        ^+  +>
        (mete hen ryf)
        ::  =+  rym=(~(get by mir.q.raz) p.ryf)
        ::  ?~  rym  (mete hen ryf)
        ::  =+  wex=~(. de [who p.ryf lim.u.rym qyx.u.rym dom.u.rym])
        ::  =+  wak=?~(q.ryf (ease:wex hen) (eave:wex hen u.q.ryf))
        ::  =:  byn        byn.wak
        ::      qyx.u.rym  qyx.wak
        ::      dom.u.rym  dom.wak
        ::    ==
        ::  +>.$(mir.q.raz (~(put by mir.q.raz) p.ryf u.rym))
      ::
      ++  poll                                          ::  pull result
        |=  [hen=duct syd=disc rot=riot]
        ^+  +>
        =+  rum=(need (~(get by mir.q.raz) syd))
        =+  kas=(need ask.rum)
        =<  abet
        =<  able
        |%  
        ++  abet
          ^+  +>.$
          +>.$(mir.q.raz (~(put by mir.q.raz) syd rum))
        ::
        ++  able
          ^+  .
          ?~  rot
            %-  pith(ask.rum ~, lim.rum kas) 
            ~(. de [him syd kas qyx.rum dom.rum])
          ?>  ?=(%w p.p.u.rot)
          ?>  =(syd r.p.u.rot)
          ?>  =(~ q.u.rot)
          ?>  ?=(%ud -.q.p.u.rot)
          ?>  =(let.dom.rum p.q.p.u.rot)
          =+  rog=((hard frog) r.u.rot)
          =+  sab=`saba`[him syd [p.q.p.u.rot +(p.q.p.u.rot)] [q.rog ~]]
          =.  ..fa  (fray hen pal.rum sab)
          (pith (~(exec de [him syd lim.rum qyx.rum dom.rum]) hen rog))
        ::
        ++  pith
          |=  wex=_de
          ^+  +>
          =+  wak=wake:wex
          %_  +>.$
            byn      byn.wak
            yel      yel.wak
            qyx.rum  qyx.wak
            dom.rum  dom.wak
          ==
        --
      ::
      ++  puke                                          ::  pull failed
        |=  [hen=duct syd=disc msg=tape]
        ^+  +>
        %_  +>
          mir.q.raz  (~(del by mir.q.raz) syd)
          yel        [[hen [%note '?' %leaf msg]] yel]
        ==
      ::
      ++  pull                                          ::  pull changeset
        |=  [hen=duct syd=disc pal=(list disc)]
        ^+  +>
        =+  ^=  rum  ^-  rede
            =+  rum=(~(get by mir.q.raz) syd)
            ?^  rum  u.rum  [~2000.1.1 ~ pal ~ [[@uvI ~ ~] 0 ~ ~]]
        ?>  ?=(~ ask.rum)
        =>  .(ask.rum [~ now])
        =.  +>.$
          %+  mete
            [[%c %pull (scot %p him) syd ~] hen]
          [syd ~ %| [%da lim.rum] [%da now]]
        +>.$(mir.q.raz (~(put by mir.q.raz) syd rum))
      ::       
      ++  tome                                          ::  accept response
        |=  [inx=@ud rot=riot]
        ^+  +>
        ::  ~&  [%tome inx ?~(rot ~ [p.u.rot q.u.rot])]
        =+  mub=(~(get by bim.p.raz) inx)
        ?~  mub  +>.$
        =+  die=?~(rot & &(?=(^ q.q.u.mub) ?=(& -.u.q.q.u.mub)))
        %=  +>.$
          byn      [[p.u.mub rot] byn]
          bim.p.raz  ?.(die bim.p.raz (~(del by bim.p.raz) inx))
          fod.p.raz  ?.(die fod.p.raz (~(del by fod.p.raz) p.u.mub))
          hac.q.raz  ?~  rot 
                       hac.q.raz 
                     %+  ~(put by hac.q.raz) 
                       [p.p.u.rot q.p.u.rot r.p.u.rot q.u.rot]
                     r.u.rot
        ==
      ::
      ++  tref                                          ::  request rejected
        |=  [hen=duct tea=wire]
        ^+  +>
        ~&  [%tref tea]
        ?.  ?=([@ *] tea)  +>
        =+  xoc=(slay i.tea)
        ?:  ?=([~ %$ %p @] xoc)
          ?>  ?=([@ ~] t.tea) 
          (puke hen i.t.tea "system failure")
        ?.  ?=([~ %$ %ud @] xoc)  +>.$
        =+  mub=(~(get by bim.p.raz) q.p.u.xoc)
        ?~  mub  +>.$
        %=  +>.$
          byn      [[p.u.mub ~] byn]
          fod.p.raz  (~(del by fod.p.raz) q.p.u.xoc)
        ==
      ::
      ++  vera                                          ::  search
        |=  [syd=@tas ren=care lok=case way=path]
        ^-  (unit)
        =+  haz=(~(get by hac.q.raz) ren lok syd way)
        ?^  haz  haz
        =+  rym=(~(get by mir.q.raz) syd)
        ?~  rym  ~
        =+  vyr=(~(aver de [him syd lim.u.rym qyx.u.rym dom.u.rym]) ren lok way)
        ?~(vyr ~ u.vyr)
      ::
      ++  zoom
        %_(+>.$ rid.yar (~(put by rid.yar) him raz))
      --
    --
  --
  ::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  ::              section 4cA, filesystem vane          ::
  ::                                                    ::
  =|                                                    ::  instrument state
      $:  ruf=raft                                      ::  revision tree
      ==                                                ::
  |=  [now=@da eny=@ sky=||(* (unit))]                  ::  activate
  ^?                                                    ::  opaque core
  |%                                                    ::
  ++  beat                                              ::  update
    |=  [wru=(unit writ) tea=wire hen=duct fav=curd]
    =>  .(fav ((hard card) fav))
    ^-  [p=(list move) q=vane]
    ?+    -.fav  [[[wru hen fav] ~] ..^$]
        %crud
      [[[wru [/d hen] %flog fav] ~] ..^$]
    ::
        %deem
      =.  wru  
          ?^  wru  wru
          ?.  =(%gold (adit hen))  ~
          [~ %gold p.fav] 
      $(fav q.fav)
    ::
        %init
      ::  [[%tell %0 %leaf "clay: home for {~(rend co ~ %p q.u.wru)}"] ~]
      [~ ..^$(fat.ruf (~(put by fat.ruf) q.u.wru [hen ~ ~ ~ ~]))]
    ::
        ?(%into %info)
      =.  wru  
          ?^  wru  wru
          ?.  =(%gold (adit hen))  ~
          [~ %gold p.fav] 
      ?>  =(q.u.wru p.fav)
      ?:  =(%$ q.fav)
        ?.  ?=(%into -.fav)  [~ ..^$]
        =+  yar=(need (~(get by fat.ruf) p.fav))
        [~ ..^$(fat.ruf (~(put by fat.ruf) p.fav yar(hez [~ hen])))]
      =^  mos  ruf  
        =+  ^=  zot
          zoot:(exec:(di:wake:(un q.u.wru now ruf) q.fav) hen now [%& r.fav])
        abet:zot(hez.yar ?.(=(%into -.fav) hez.yar.zot [~ hen]))
      [mos ..^$]
    ::
        %pull
      ?>  ?=(^ wru)
      ?>  !=(q.u.wru q.fav) 
      =^  mos  ruf
        abet:zoom:(pull:(fa:(un q.u.wru now ruf) p.fav) hen q.fav r.fav)
      [mos ..^$]
    ::
        %warp
      ?>  ?=(^ wru)
      =^  mos  ruf
        =<  abet
        =+  une=(un q.u.wru now ruf)
        ::  ~&  [%warp q.u.wru p.fav]
        ?.  =(q.u.wru p.fav)
          zoom:(mote:(fa:une p.fav) hen q.fav)
        =+  wex=(di:une p.q.fav)
        ?~  q.q.fav
          zoot:(ease:wex hen)
        zoot:(eave:wex hen u.q.q.fav)
      [mos ..^$]
    ::
        %wart
      ?>  ?=(%ru q.fav)
      ?~  s.fav  [~ ..^$]
      =^  mos  ruf
        =<  abet
        =<  zoom
        (tome:(fa:(un q.u.wru now ruf) p.fav) ((hard ,[@ud riot]) u.s.fav))
      [mos ..^$]
    ::
        %went
      ?:  =(%good q.fav)  [~ ..^$]
      ?>  ?=([@ *] tea)
      =+  une=(un q.u.wru now ruf)
      =^  mos  ruf
        ?+    i.tea  ~&([%went-wrong tea] !!)
            %pull
          abet:zoom:(tref:(fa:une p.fav) hen t.tea)
        ==
      [mos ..^$]
    ::
        %wake
      =+  dal=(turn (~(tap by fat.ruf) ~) |=([a=@p b=room] a))
      =|  mos=(list move)
      |-  ^-  [p=(list move) q=vane]
      ?~  dal  [mos ..^^$(las.ruf now)] 
      =^  som  ruf  abet:wake:(un i.dal now ruf)
      $(dal t.dal, mos (weld som mos))
    ::
        %wort
      ?>  ?=([@ *] tea)
      =+  une=(un q.u.wru now ruf)
      =^  mos  ruf
        ?+    i.tea  !!
            %pull
          ?>  ?=([@ @ ~] t.tea)
          =+  xoc=(slay i.t.tea)
          ?>  ?=([~ %$ %p @] xoc)
          =<  abet
          =<  zoom
          (puke:(fa:(un q.u.wru now ruf) q.p.u.xoc) hen i.t.t.tea p.fav)
        ==
      [mos ..^$]
    ::
        %writ
      ?>  ?=([@ *] tea)
      =+  une=(un q.u.wru now ruf)
      =^  mos  ruf
        ?+    i.tea  ~&([%writ-bad tea] !!)
            %pull
          ?>  ?=([@ @ ~] t.tea)
          =+  xoc=(slay i.t.tea)
          ?>  ?=([~ %$ %p @] xoc)
          =<  abet
          =<  zoom
          (poll:(fa:(un q.u.wru now ruf) q.p.u.xoc) hen i.t.t.tea p.fav)
        ==
      [mos ..^$]
    ==
  ::
  ++  come  
    |=  old=vase
    ^-  vane
    ~|(%load-nest-clay !!)
  ::
  ++  doze
    |=  [now=@da hen=duct]
    =|  nex=(unit ,@da)
    =+  dal=(turn (~(tap by fat.ruf) ~) |=([a=@p b=room] a))
    |-  ^+  nex
    ?~  dal  nex
    $(dal t.dal, nex (hunt nex doze:(un i.dal now ruf)))
  ::
  ++  flee  stay
  ::
  ++  load
    |=  new=vase
    ^-  vane
    ?.  (~(nest ut -:!>(ruf)) & p.new)
      (come new) 
    ..^$(ruf (raft q.new))
  ::
  ++  raze
    ^-  vane
    ..$(ruf *raft)
  ::
  ++  scry                                              ::  inspect
    |=  [our=ship ron=@tas his=ship syd=disc lot=coin tyl=path]
    ^-  (unit)
    =+  luk=?.(?=(%$ -.lot) ~ ((soft case) p.lot))
    =+  run=((soft care) ron)
    ?~  luk  ~
    ?~  run  ~
    ?.  =(our his)
      (vera:(fa:(un our now ruf) his) syd u.run u.luk tyl)
    =+  vyr=(aver:(di:(un our now ruf) syd) u.run u.luk tyl)
    ?~(vyr ~ u.vyr)
  ::
  ++  stay  `vase`!>(ruf)
  --
