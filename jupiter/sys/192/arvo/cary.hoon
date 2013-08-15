!:
::          %cary, revision control.  This file is in the public domain.
::
=>  |%
    ++  cy
      =+  car=*arch
      |%  
      ++  duel                                          ::  changes to sync
        |=  bus=arch  
        =+  ram=*hapt
        =+  maz=*umaz
        |-  ^-  umaz
        ?:  =(car bus)
          maz
        =+  pax=(flop ram)
        ?:  ?=(& -.car)
          ?.  ?=(& -.bus)  
            (uzag $(maz (uzag maz), bus car, car bus))
          ?:  =(q.car q.bus)
            maz
          ?:  (gth p.car p.bus)
            [p.maz [[pax %set q.car q.bus r.car] q.maz]]
          [[[pax %set q.bus q.car r.bus] p.maz] q.maz]
        ?.  ?=(| -.bus)
          ?:  (gth p.car p.bus)
            =>  .(maz $(bus nope))
            [p.maz [[pax %del q.bus] q.maz]]
          :-  =+  wob=[[pax %set q.bus @ r.bus] p.maz]
              ?:(=(nope car) wob [[pax %del @] wob])
          q.maz
        =+  lay=(gth p.car p.bus)
        =+  ryc=(~(tap by q.car) ~)
        =+  seb=(~(tap by q.bus) ~)
        =+  noy=(skip ryc |=([p=@ta q=arch] (~(has by q.bus) p)))
        =+  syr=(skip seb |=([p=@ta q=arch] (~(has by q.car) p)))
        =+  yel=(skim ryc |=([p=@ta q=arch] (~(has by q.bus) p)))
        =>  %=    .
                maz
              |-  ^-  umaz
              ?~  yel
                maz
              %=  $
                yel  t.yel 
                maz  %=  ^$
                       ram  [p.i.yel ram]
                       car  q.i.yel
                       bus  (need (~(get by q.bus) p.i.yel))
                     ==
              ==
            ==
        =>  %=    .
                maz
              |-  ^-  umaz
              ?~  noy
                maz
              %=  $
                noy  t.noy
                maz  %=  ^$
                         ram  [p.i.noy ram]
                         car  q.i.noy
                         bus  [%| ?:(lay @ p.bus) ~]
                     ==
              ==
            ==
        |-  ^-  umaz
        ?~  syr
          maz
        %=  $
          syr  t.syr
          maz  %=  ^$
                   ram  [p.i.syr ram]
                   car  [%| ?.(lay @ p.car) ~]
                   bus  q.i.syr
               ==
        ==
      ::
      ++  sift                                          ::  select subtree
        |=  pax=path
        ^-  arch
        ?~  pax
          car
        ?-    car
            [& *]  nope
            [| *]  
          =+  gos=(~(get by q.car) i.pax) 
          ?~(gos nope $(pax t.pax, car u.gos))
        ==
      ::
      ++  wane                                          ::  apply ukaz
        |=  [now=@da kuz=ukaz]
        ^-  arch
        ::  ~&  [%wane-ukaz p.kuz `@tas`-.q.kuz]
        |-  ^-  arch
        ?~  p.kuz
          =+  hux=?:(?=(& -.car) q.car 0)
          ?-  -.q.kuz
            %del  ?>(=(hux p.q.kuz) nope)
            %set  ?.  =(hux q.q.kuz)
                    ~&  [%miss-hux hux]
                    ~&  [%miss-uke kuz]
                    !!
                  ?>(=(hux q.q.kuz) [%& now p.q.kuz r.q.kuz])
          ==
        ?>  ?=(| -.car)
        =+  yit=(~(get by q.car) i.p.kuz)
        ?~  yit
          =+  dan=$(p.kuz t.p.kuz, car nope)
          ?:  =(nope dan)
            car
          [%| now (~(put by q.car) i.p.kuz dan)]
        =+  dan=$(p.kuz t.p.kuz, car u.yit)
        ?:  =(nope dan)
          [%| now (~(del by q.car) i.p.kuz)]
        ?:  =(u.yit dan)
          car
        [%| now (~(put by q.car) i.p.kuz dan)]
      ::
      ++  work                                          ::  apply ukazy
        |=  [now=@da kuz=(list ukaz)]
        ^-  arch
        ?~(kuz car $(car (wane now i.kuz), kuz t.kuz))
      --
    ++  uwed                                            ::  weld dual change
      |=  [a=umaz b=umaz]  ^-  umaz
      [(weld p.a p.b) (weld q.a q.b)]     
    ++  uzag                                            ::  reverse dual change
      |=(a=umaz ^-(umaz [q.a p.a]))
    --
|%
++  cary                                                ::  filesystem
  ^-  vane                                              ::  kernel instrument
  =|                                                    ::  instrument state
      $:  rof=roof                                      ::  revision tree
      ==                                                ::
  |=  [now=@da eny=@ sky=||(* (unit))]                  ::  activate
  ^?                                                    ::  opaque core
  |%                                                    ::
  ++  beat                                              ::  update
    |=  [whu=(unit flag) tea=tire hen=hose fav=card]
    ^-  [p=(list move) q=vane]
    ?+    -.fav  !! 
        %edit
      =.  whu  
          ?^  whu  whu
          ?.  =(%gold (adit hen))  ~
          [~ p.fav] 
      ?>  =(u.whu p.fav)
      =+  yar=(need (~(get by rof) u.whu))
      ?>  own.yar
      =+  lix=(~(get by dos.yar) q.fav)
      =+  saq=`desk`?^(lix u.lix [~ 0 ~])
      :-  =+  one=~(rend co ~ %ud let.saq)
          =+  two=~(rend co ~ %p u.whu)
          =+  tre=(rip 3 q.fav)
          %+  turn
            r.fav
          |=  uke=ukaz  ^-  move
          =+  qua=~(ram re (dish:ut [~ %path] p.uke))
          =+  qui=?-(-.q.uke %del '-', %set ?:(=(0 q.q.uke) '+' ':'))
          [whu [/d/ hen] %flog %note qui %leaf "/cx/{one}/{two}/{tre}{qua}"]
      %_    ..^$
          rof
        %+  ~(put by rof)
          u.whu
        %_    yar
            dos
          %+  ~(put by dos.yar)
            q.fav
          |-  ^-  desk 
          ?~  r.fav  saq
          =+  rad=`arch`?^(hit.saq r.i.hit.saq [%| ~2000.1.1 ~])
          %=    $
              r.fav    t.r.fav
              let.saq  +(let.saq)
              hit.saq  :-  [now i.r.fav (~(wane cy rad) now i.r.fav)]
                           hit.saq
          ==
        ==
      ==
    ::
        %keep
      ::  [[%tell %0 %leaf "cary: home for {~(rend co ~ %p u.whu)}"] ~]
      [~ ..^$(rof (~(put by rof) u.whu `room`[& ~]))]
    ::
        %ship
      ~&  [%ship u.whu]
      =+  yar=(need (~(get by rof) u.whu))
      ?>  own.yar
      =+  saq=(need (~(get by dos.yar) p.fav))
      ?<  (~(has by lab.saq) q.fav)
      :-  ~
      %_    ..^$
          rof
        %+  ~(put by rof)
          u.whu
        %_    yar
            dos
          %+  ~(put by dos.yar) 
            p.fav 
          saq(lab (~(put by lab.saq) q.fav let.saq))
        ==
      ==
    ==
  ++  scry                                              ::  inspect
    |=  [our=flag ren=@tas his=flag lot=coin tyl=path]
    ^-  (unit)
    ?~  tyl  ~
    ?>  =(our his)                                      ::  XX temporary
    =+  poj=(slay i.tyl)
    ?.  ?=([~ %% %tas *] poj)  ~
    =+  yar=(~(get by rof) his)
    ?~  yar  ~
    =+  siq=`(unit desk)`(~(get by dos.u.yar) q.p.u.poj)
    ?~  siq  ~
    ?:  ?=(%w ren)
      ?.  ?=([%% %ud @] lot)  ~
      ?^  t.tyl  ~
      ?:  (gte q.p.lot let.u.siq)  ~
      |-  ^-  (unit ukaz)
      ?~  hit.u.siq  ~
      ?:  =(q.p.lot let.u.siq)
        [~ q.i.hit.u.siq]
      $(let.u.siq (dec let.u.siq), hit.u.siq t.hit.u.siq)
    =+  ^=  rud  ^-  (unit arch)
        |-  ^-  (unit arch) 
        ?+    lot  ~
            [%% %ud @]
          ?:  =(0 q.p.lot)
            [~ %| ~2000.1.1 ~]
          ?:  (gth q.p.lot let.u.siq)  ~
          [~ r:(snag (sub let.u.siq q.p.lot) hit.u.siq)]
        ::
            [%% %da @]
          ?:  ?&  !own.u.yar 
                  ?=(^ hit.u.siq) 
                  (gth q.p.lot p.i.hit.u.siq)
              ==
            ~
          |-  ^-  (unit arch)
          ?~  hit.u.siq  ~
          ?:  (gte q.p.lot p.i.hit.u.siq)
            [~ r.i.hit.u.siq]
          $(hit.u.siq t.hit.u.siq)
        ::
            [%% %tas @]
          =+  lov=(~(get by lab.u.siq) q.p.lot)
          ?~  lov  ~
          $(lot [%% %ud u.lov])
        ==
    ?~  rud  ~
    =+  rad=(~(sift cy u.rud) t.tyl)
    ?+  ren  ~
      %x  ?.(?=(& -.rad) ~ [~ r.rad])
      %y  :-  ~
          ?-  -.rad
            &  [%& p.rad q.rad]
            |  [%| p.rad (turn (~(tap by q.rad) ~) |=([p=@ta *] p))]
          ==
      %z  [~ rad]
    ==
  --
--
