!:
::          %ray, revision control.  This file is in the public domain.
::
=>  |%
    ++  cy
      =+  car=*arch
      |%  
      ++  also                                          ::  metadata
        |=  pax=path  ^-  meta
        =>  .(car (sift pax))
        ?-  -.car
          &  [%& p.car q.car]
          |  [%| p.car (turn (~(tap by q.car) ~) |=([p=@ta *] p))]
        ==
      ::
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
        ?~  p.kuz
          =+  hux=?:(?=(& -.car) q.car 0)
          ?-  -.q.kuz
            %del  ?>(=(hux p.q.kuz) nope)
            %set  ?>(=(hux q.q.kuz) [%& now p.q.kuz r.q.kuz])
          ==
        ?>  ?=(| -.car)
        =+  yit=(~(get by q.car) i.p.kuz)
        =+  dan=$(p.kuz t.p.kuz, car ?~(yit nope u.yit))
        ?:  =(nope dan)
          ?~  yit 
            car 
          =+(hon=(~(del by q.car) i.p.kuz) ?~(hon nope [%| now hon]))
        ?~  yit 
          [%| now [[i.p.kuz dan] ~ ~]] 
        ?:  =(dan u.yit)
          car
        =+(hon=(~(put by q.car) i.p.kuz dan) [%| now hon])
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
++  clay                                                ::  revision control
  =+  rof=*roof
  |%
  ++  blip
    |=  [who=flag por=@tas]
    ^-  (unit desk)
    =+  yar=(~(get by rof) who)
    ?~  yar  ~
    (~(get by dos.u.yar) por)
  ::
  ++  drip
    |=  [now=@da who=flag fav=card]
    ^-  [p=(list card) q=_+>]
    ?+    -.fav  !! 
        %edit
      ::  ~&  [%edit who fav]
      =+  yar=(need (~(get by rof) who))
      ?>  own.yar
      =+  lix=(~(get by dos.yar) q.fav)
      =+  saq=`desk`?^(lix u.lix [~ 0 ~])
      :-  ~
      %_    +>.$
          rof
        %+  ~(put by rof)
          who
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
        %init
      ::  [[%tell %0 %leaf "cary: home for {~(rend co ~ %p who)}"] ~]
      [~ +>.$(rof (~(put by rof) who `room`[& ~]))]
    ::
        %ship
      ~&  [%ship who]
      =+  yar=(need (~(get by rof) who))
      ?>  own.yar
      =+  saq=(need (~(get by dos.yar) p.fav))
      ?<  (~(has by lab.saq) q.fav)
      :-  ~
      %_    +>.$
          rof
        %+  ~(put by rof)
          who
        %_    yar
            dos
          %+  ~(put by dos.yar) 
            p.fav 
          saq(lab (~(put by lab.saq) q.fav let.saq))
        ==
      ==
    ==
  ++  mine
    |=  [own=? lot=coin saq=desk]
    ^-  (unit arch)
    ?+    lot  ~
        [%% %ud @]
      ?:  =(0 q.p.lot)
        [~ %| ~2000.1.1 ~]
      ?:  (gth q.p.lot let.saq)  ~
      [~ r:(snag (sub let.saq q.p.lot) hit.saq)]
    ::
        [%% %da @]
      ?:  &(!own ?=(^ hit.saq) (gth q.p.lot p.i.hit.saq))  ~
      |-  ^-  (unit arch)
      ?~  hit.saq  ~
      ?:  (gte q.p.lot p.i.hit.saq)
        [~ r.i.hit.saq]
      $(hit.saq t.hit.saq)
    ::
        [%% %tas @]
      =+  lov=(~(get by lab.saq) q.p.lot)
      ?~  lov  ~
      $(lot [%% %ud u.lov])
    ==
  ++  scry
    |=  [ren=lens who=flag lot=coin tyl=path]
    ::  ~&  [%scry ren who lot tyl]
    ^-  (unit)
    ?~  tyl  ~
    =+  poj=(slay i.tyl)
    ?.  ?=([~ %% %tas *] poj)  ~
    =+  yar=(~(get by rof) who)
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
    =+  rud=(mine own.u.yar lot u.siq)
    ?~  rud  ~
    =+  rad=(~(sift cy u.rud) t.tyl)
    ?-  ren
      %x  ?.(?=(& -.rad) ~ [~ r.rad])
      %y  [~ ~(also cy rad)]
      %z  [~ rad]
    ==
  --
--
