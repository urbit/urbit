!:  ::  %gall, agent execution
!?  163
::::
|=  pit=vase
=>  =~
|%  ::::::::::::::::::::::::::::::::::::::::::::::::::::::    rest of arvo
    ::::::::::::::::::::::::::::::::::::::::::::::::::::::
++  volt  ?(%low %high)                                 ::  voltage
++  torc  $|(?(%iron %gold) [%lead p=ship])             ::  security control
++  roon                                                ::  reverse ames msg
  $%  [%d p=mark q=*]                                   ::  diff (diff)
      [%x ~]                                            ::
  ==                                                    ::
++  rook                                                ::  forward ames msg
  $%  [%m p=mark q=*]                                   ::  message
      [%s p=path]                                       ::  subscribe
      [%u ~]                                            ::  cancel/unsubscribe
  ==                                                    ::
--                                                      ::
|%  ::::::::::::::::::::::::::::::::::::::::::::::::::::::    local arvo
    ::::::::::::::::::::::::::::::::::::::::::::::::::::::
++  cote                                                ::  ++ap note
  $%  [%meta p=@tas q=vase]                             ::  
      [%send p=ship q=cush]                             ::  
      [%hiss p=mark q=cage]
  ==                                                    ::
++  cove  (pair bone (mold cote cuft))                  ::  internal move
++  move  ,[p=duct q=(mold note-arvo gift-arvo)]        ::  typed move
--                                                      ::
|%  ::::::::::::::::::::::::::::::::::::::::::::::::::::::    %gall state
    ::::::::::::::::::::::::::::::::::::::::::::::::::::::
++  axle-n  ?(axle axle-0)                              ::  upgrade path
++  axle-0  ,[%0 pol=(map ship mast-0)]                 ::
++  mast-0                                              ::
  (cork mast |=(mast +<(bum (~(run by bum) seat-0))))   ::
++  seat-0                                              ::
  (cork seat |=(seat +<(|7 zam=(scar |7.+<))))          ::
++  axle                                                ::  all state
  $:  %1                                                ::  state version
      pol=(map ship mast)                               ::  apps by ship
  ==                                                    ::
++  gest                                                ::  subscriber data
  $:  sup=bitt                                          ::  incoming subscribers
      neb=boat                                          ::  outgoing subscribers
      qel=(map bone ,@ud)                               ::  queue meter
  ==                                                    ::
++  mast                                                ::  ship state
  $:  sys=duct                                          ::  system duct
      sap=(map ship scad)                               ::  foreign contacts
      bum=(map dude seat)                               ::  running agents
      wub=(map dude sofa)                               ::  waiting queue
  ==                                                    ::
++  ffuc                                                ::  new cuff
    $:  p=(unit (set ship))                             ::  disclosing to
        q=ship                                          ::  attributed to
    ==                                                  ::
++  prey  (pair volt ffuc)                              ::  privilege
++  scad                                                ::  foreign connection
  $:  p=@ud                                             ::  index
      q=(map duct ,@ud)                                 ::  by duct
      r=(map ,@ud duct)                                 ::  by index
  ==                                                    ::
++  scar                                                ::  opaque input
  $:  p=@ud                                             ::  bone sequence
      q=(map duct bone)                                 ::  by duct
      r=(map bone duct)                                 ::  by bone
  ==                                                    ::
++  seat                                                ::  agent state
  $:  mom=duct                                          ::  control duct 
      liv=?                                             ::  unstopped
      toc=torc                                          ::  privilege
      tyc=stic                                          ::  statistics
      ged=gest                                          ::  subscribers
      hav=vase                                          ::  running state
      byk=beak                                          ::  update control
      pyl=(map bone mark)                               ::  req'd translations
      zam=scar                                          ::  opaque ducts
  ==                                                    ::
++  sofa                                                ::  queue for blocked
  $:  kys=(qeu (trel duct prey club))                   ::  queued kisses
  ==                                                    ::
++  stic                                                ::  statistics
  $:  act=@ud                                           ::  change number
      eny=@uvI                                          ::  entropy
      lat=@da                                           ::  time
  ==                                                    ::
--                                                      ::
    ::::::::::::::::::::::::::::::::::::::::::::::::::::::  vane header
    ::::::::::::::::::::::::::::::::::::::::::::::::::::::
.  ==
=|  all=axle                                            ::  all vane state
|=  $:  now=@da                                         ::  urban time
        eny=@uvI                                        ::  entropy
        ska=sled                                        ::  activate
    ==                                                  ::  opaque core
|%  ::::::::::::::::::::::::::::::::::::::::::::::::::::::  state machine
    ::::::::::::::::::::::::::::::::::::::::::::::::::::::
++  mo
  |_  $:  $:  our=@p 
              hen=duct
              moz=(list move)
          ==
          mast
      ==
  ++  mo-abed                                           ::  initialize
    |=  [our=@p hen=duct]
    ^+  +>
    %_    +>
      our  our
      hen  hen
      +<+  (~(got by pol.all) our)
    ==
  ::
  ++  mo-abet                                           ::  resolve to 
    ^-  [(list move) _+>+]
    :_  +>+(pol.all (~(put by pol.all) our +<+))
    %-  flop
    %+  turn  moz
    |=  a=move
    ?.  ?=(%pass -.q.a)  a
    [p.a %pass [(scot %p our) p.q.a] q.q.a]
  ::
  ++  mo-conf                                           ::  configure
    |=  [dap=dude lum=culm]
    (mo-boot dap ?:((~(has by bum) dap) %old %new) p.p.lum q.p.lum da/now)
  ::
  ++  mo-pass                                           ::  standard pass
    |=  [pax=path noh=note-arvo]
    %_(+> moz :_(moz [hen %pass pax noh]))
  ::
  ++  mo-give
    |=  git=gift-gall
    %_(+> moz :_(moz [hen %give git]))
  ::
  ++  mo-okay                                           ::  valid agent core
    |=  vax=vase
    ^-  ?
    =+  bol=(slew 12 vax)
    ?~  bol  |
    (~(nest ut p.u.bol) %| -:!>(*bowl))
  ::
  ++  mo-boom                                           ::  complete new boot
    |=  [dap=dude byk=beak dep=@uvH gux=gage]
    ^+  +>
    ?-    -.gux
        %tabl  ~|(%made-tabl !!)
        %|  
      =.  +>  (mo-bold byk dap dep)
      =.  +>  (mo-give %onto %| p.gux)
      +>
        %&
      ?>  ?=(@ p.p.gux)
      ?.  (mo-okay q.p.gux)
        (mo-give %onto %| [%leaf "{<dap>}: bogus core"]~)
      =.  +>  (mo-bold byk dap dep)
      =.  +>  (mo-born dap byk q.p.gux)
      =+  old=+>.$
      =+  wag=(ap-prop:(ap-abed:ap dap [%high [~ our]]) ~)
      ?^  -.wag
        =.  +>.$  old
        (mo-give %onto %| u.-.wag)
      =.  +>.$  ap-abet:+.wag
      (mo-give:(mo-claw dap) %onto %& dap %boot now)
    ==
  ::
  ++  mo-born                                           ::  new seat
    |=  [dap=dude byk=beak hav=vase]
    =+  sat=*seat
    %_    +>.$
        bum
      %+  ~(put by bum)  dap
      %_  sat
        mom  hen
        byk  byk
        hav  hav
        p.zam  1
        q.zam  [[[~ ~] 0] ~ ~]
        r.zam  [[0 [~ ~]] ~ ~]
      ==
    ==
  ::
  ++  mo-boon                                           ::  complete old boot
    |=  [dap=dude byk=beak dep=@uvH gux=gage]
    ^+  +>
    =+  sut=(~(get by bum) dap)
    ?~  sut
      ~&  [%gall-old-boon dap]
      +>.$
    =.  bum  (~(put by bum) dap u.sut(byk byk))
    =.  +>.$  (mo-bold byk dap dep)
    ?-  -.gux
      %tabl  ~|(%made-tabl !!)
      %|  (mo-give %onto %| p.gux)
      %&  ?>  ?=(@ p.p.gux)
          ap-abet:(ap-peep:(ap-abed:ap dap [%high [~ our]]) q.p.gux)
    ==
  ::
  ++  mo-bold                                           ::  wait for dep
    |=  [byk=beak dap=dude dep=@uvH]
    ^+  +>
    %+  mo-pass  [%sys %dep (scot %p p.byk) q.byk dap ~] 
    [%f %wasp our dep]
  ::
  ++  mo-boot                                           ::  create ship
    |=  [dap=dude how=?(%new %old) byk=beak]
    ^+  +>
    ::  ~&  [%mo-boot dap how byk]
    %+  mo-pass  [%sys how dap (scot %p p.byk) q.byk (scot r.byk) ~]
    ^-  note-arvo
    [%f %exec our `[byk %core [byk [dap %ape ~]]]]
  ::
  ++  mo-away                                           ::  foreign request
    |=  [him=ship caz=cush]                             ::  
    ^+  +>
    ::  ~&  [%mo-away him caz]
    ?:  ?=(%pump -.q.caz)
      ::
      ::  you'd think this would send an ack for the diff
      ::  that caused this pump.  it would, but we already
      ::  sent it when we got the diff in ++mo-cyst.  then
      ::  we'd have to save the network duct and connect it
      ::  to this returning pump.
      ::
      +>
    =^  num  +>.$  (mo-bale him)
    =+  ^=  roc  ^-  rook
        ?-  -.q.caz
          %peel  !!
          %poke  [%m p.p.q.caz q.q.p.q.caz]
          %pull  [%u ~]
          %puff  !!
          %punk  !!
          %peer  [%s p.q.caz]
        ==
    %+  mo-pass  
      [%sys %way -.q.caz ~]
    `note-arvo`[%a %wont [our him] [%g %ge p.caz ~] [num roc]]
  ::
  ++  mo-baal                                           ::  error convert a
    |=  art=(unit ares)
    ^-  ares
    ?~(art ~ ?~(u.art `[%blank ~] u.art))
  ::
  ++  mo-baba                                           ::  error convert b
    |=  ars=ares
    ^-  (unit tang)
    ?~  ars  ~
    `[[%leaf (trip p.u.ars)] q.u.ars]
  ::
  ++  mo-awed                                           ::  foreign response
    |=  [him=ship why=?(%peer %poke %pull) art=(unit ares)]
    ^+  +>
    ::  ~&  [%mo-awed him why art]
    =+  tug=(mo-baba (mo-baal art))
    ?-  why
      %peer  (mo-give %unto %reap tug)
      %poke  (mo-give %unto %coup tug)
      %pull  +>.$
    ==
  ::
  ++  mo-bale                                           ::  assign outbone
    |=  him=ship 
    ^-  [@ud _+>]
    =+  sad=(fall (~(get by sap) him) `scad`[1 ~ ~])
    =+  nom=(~(get by q.sad) hen)
    ?^  nom  [u.nom +>.$]
    :-  p.sad
    %_    +>.$
        sap
      %+  ~(put by sap)  him
      %_  sad
        p  +(p.sad)
        q  (~(put by q.sad) hen p.sad)
        r  (~(put by r.sad) p.sad hen)
      ==
    ==
  ::
  ++  mo-ball                                           ::  outbone by index
    |=  [him=ship num=@ud]
    ^-  duct
    (~(got by r:(~(got by sap) him)) num)
  ::
  ++  mo-come                                           ::  handle locally
    |=  [her=ship caz=cush]
    ^+  +>
    =+  pry=`prey`[%high [~ her]]
    (mo-club p.caz pry q.caz)
  ::
  ++  mo-coup                                           ::  back from mo-away
    |=  [dap=dude him=ship cup=ares]
    %^  mo-give  %unto  %coup
    ?~  cup  ~
    [~ `tang`[[%leaf (trip p.u.cup)] q.u.cup]]
  ::
  ++  mo-chew                                           ::  reverse build path
    |=  pax=path
    ^-  beak
    ?>  ?=([@ @ @ ~] pax)
    [(slav %p i.pax) i.t.pax da/(slav %da i.t.t.pax)]
  ::
  ++  mo-cyst                                           ::  take in /sys
    |=  [pax=path sih=sign-arvo]
    ^+  +>
    ?+    -.pax  !!
        %dep                                            ::  update
      ?>  ?=([%f %news *] sih)
      ?>  ?=([@ @ @ ~] t.pax)
      %^  mo-boot  i.t.t.t.pax
        ?:((~(has by bum) i.t.t.t.pax) %old %new)
      [(slav %p i.t.pax) i.t.t.pax [%da now]]
    ::
        %new
      ?>  ?=([%f %made *] sih)
      ?>  ?=([@ @ @ @ ~] t.pax)
      (mo-boom i.t.pax (mo-chew t.t.pax) +>.sih)
    ::
        %old                                            ::  reload old
      ?>  ?=([%f %made *] sih)
      ?>  ?=([@ @ @ @ ~] t.pax)
      (mo-boon i.t.pax (mo-chew t.t.pax) +>.sih)
    ::
        %pel                                            ::  translated peer
      ?>  ?=([@ ~] t.pax)
      =+  mar=i.t.pax
      ?>  ?=([%f %made *] sih)
      ?-  -.q.+.sih
        %tabl  ~|(%made-tabl !!)
        %&  (mo-give %unto %diff p.q.+>.sih)
        %|  =.  p.q.+>.sih  (turn p.q.+>.sih |=(a=tank rose/[~ "! " ~]^[a]~))
            ~>  %slog.`%*(. >[%wh %y]< +> [>%mo-cyst-fail< (flop p.q.+>.sih)])
            (mo-give %unto %quit ~)   ::  XX better errors pls
      ==
    ::
        %red                                            ::  diff ack
      ?>  ?=([@ @ @ ~] t.pax)
      ?>  ?=([%a %woot *] sih)
      =+  :*  him=(slav %p i.t.pax)
              dap=i.t.t.pax
              num=(slav %ud i.t.t.t.pax)
          ==
      =>  .(pax `path`[%req t.pax])
      ?~  q.+>.sih 
        (mo-pass [%sys pax] %g %deal [him our] dap %pump ~)
      ~&  [%diff-bad-ack q.+>.sih]                      ::  should not happen
      =.  +>.$  (mo-pass [%sys pax] %g %deal [him our] dap %pull ~)
      (mo-pass [%sys pax] %a %wont [our him] [%g %gh dap ~] [num %x ~])
    ::
        %rep                                            ::  reverse request
      ?>  ?=([@ @ @ ~] t.pax)
      ?>  ?=([%f %made *] sih)
      =+  :*  him=(slav %p i.t.pax)
              dap=i.t.t.pax
              num=(slav %ud i.t.t.t.pax)
          ==
      ?-  -.q.+>.sih
        %tabl  ~|(%made-tabl !!)
        %|  (mo-give %mack `p.q.+>.sih)                  ::  XX should crash
        %&  =.  +>.$  (mo-give %mack ~)             ::  XX pump should ack
            (mo-give(hen (mo-ball him num)) %unto %diff `cage`p.q.+>.sih)
      ==
    ::
        %req                                            ::  inbound request
      ?>  ?=([@ @ @ ~] t.pax)
      =+  :*  him=(slav %p i.t.pax)
              dap=i.t.t.pax
              num=(slav %ud i.t.t.t.pax)
          ==
      ?:  ?=([%f %made *] sih)
        ?-  -.q.+>.sih
          %tabl  ~|(%made-tabl !!)
          %|  (mo-give %mack `p.q.+>.sih)               ::  XX should crash
          %&  (mo-pass [%sys pax] %g %deal [him our] i.t.t.pax %poke p.q.+>.sih)
        ==
      ?:  ?=([%a %woot *] sih)  +>.$                    ::  quit ack, boring
      ?>  ?=([%g %unto *] sih)
      =+  cuf=`cuft`+>.sih
      ?-    -.cuf
        %coup  (mo-give %mack p.cuf)
        %diff  %+  mo-pass  [%sys %red t.pax]
               [%a %wont [our him] [%g %gh dap ~] [num %d p.p.cuf q.q.p.cuf]]
        %doff  !!
        %quit  %+  mo-pass  [%sys pax]
               [%a %wont [our him] [%g %gh dap ~] [num %x ~]]
        %reap  (mo-give %mack p.cuf)
      ==
    ::
        %val                                            ::  inbound validate
      ?>  ?=([@ @ ~] t.pax)
      =+  [him=(slav %p i.t.pax) dap=i.t.t.pax]
      ?>  ?=([%f %made *] sih)
      ?-  -.q.+>.sih
        %tabl  !!
        %|     (mo-give %unto %coup `p.q.+>.sih)        ::  XX invalid, crash
        %&     (mo-clip dap `prey`[%high ~ him] %poke p.q.sih)
      ==
    ::
        %way                                            ::  outbound request
      ?:  ?=([%a %went *] sih)  ::  XX AWFUL
        ~&  %way-went-bug
        ?>  ?=([@ ~] t.pax)
        %-  mo-awed
        :*  p.+>.sih
            (?(%peer %poke %pull) i.t.pax)
            ~
        ==
      ?>  ?=([%a %woot *] sih)
      ?>  ?=([@ ~] t.pax)
      %-  mo-awed
      :*  p.+>.sih
          (?(%peer %poke %pull) i.t.pax)
          +>+.sih
      ==
    ==
  ::
  ++  mo-cook                                           ::  take in /use
    |=  [pax=path hin=(hypo sign-arvo)]
    ^+  +>
    ?.  ?=([@ @ ?(%inn %out %cay) *] pax)
      ~&  [%mo-cook-bad-pax pax]
      !!
    =+  dap=`@tas`i.pax
    =+  pry=`prey`[%high [~ (slav %p i.t.pax)]]
    =+  pap=(ap-abed:ap dap pry) 
    =+  vax=(slot 3 `vase`hin)
    ?-  i.t.t.pax
      %inn  ap-abet:(ap-pour:pap t.t.t.pax (slot 3 `vase`hin))      
      %cay  ?.  ?=([%e %sigh *] q.hin)
              ~&  [%mo-cook-weird q.hin]
              ~&  [%mo-cook-weird-path pax]
              +>.$
            ap-abet:(ap-purr:pap +<.q.hin t.t.t.pax +>.q.hin)
    ::
      %out  ?:  ?=([%f %made *] q.hin)
              ?-  -.q.+>.q.hin  
                %tabl  ~|(%made-tabl !!)
                %&  ap-abet:(ap-pout:pap t.t.t.pax %diff +.q.+>.q.hin)
                %|  =+  why=p.q.+>.q.hin
                    =.  why  (turn why |=(a=tank rose/[~ "! " ~]^[a]~))
                    ~>  %slog.`rose/["  " "[" "]"]^[>%mo-cook-fail< (flop why)]
                    ~&  [him=q.q.pry our=our pax=pax]
                    ::
                    ::  here we should crash because the right thing
                    ::  for the client to do is to upgrade so that it
                    ::  understands the server's mark, thus allowing
                    ::  the message to proceed.  but ames is not quite
                    ::  ready for promiscuous crashes, so instead we
                    ::  send a pull outward and a quit downward.
                    ::  or not... outgoing dap (XXX) is not in the path.
                    ::  =.  +>.$  ap-abet:(ap-pout:pap t.t.t.pax %quit ~)
                    ::  %+  mo-pass
                    ::    [%use pax]
                    ::  [%g %deal [q.q.pry our] XXX %pull ~]
                    !!
              ==
            ?.  ?=([%g %unto *] q.hin)
              ~&  [%mo-cook-weird q.hin]
              ~&  [%mo-cook-weird-path pax]
              +>.$
            ?:  ?=(%doff +>-.q.hin)
              %+  mo-pass
                [%use pax]
              [%f %exec our ~ byk.pap %vale +.p.q.hin]
            ap-abet:(ap-pout:pap t.t.t.pax +>.q.hin)
    ==
  ::
  ++  mo-claw                                           ::  clear queue
    |=  dap=dude
    ^+  +>
    ?.  (~(has by bum) dap)  +>
    =+  suf=(~(get by wub) dap)
    ?~  suf  +>.$
    |-  ^+  +>.^$
    ?:  =(~ kys.u.suf)
      +>.^$(wub (~(del by wub) dap))
    =^  lep  kys.u.suf  [p q]:~(get to kys.u.suf)
    $(moz :_(moz [p.lep %slip %g %deal [q.q.q.lep our] dap r.lep]))
    ::  $(+>.^$ (mo-clip(hen p.lep) dap q.lep r.lep))
  ::
  ++  mo-beak                                           ::  build beak
    |=  dap=dude
    =-  ?.(=(p our) - -(r [%da now])) ::  soft dependencies
    ^-  beak
    byk:(~(got by bum) dap)
  ::
  ++  mo-clip                                           ::  apply club
    |=  [dap=dude pry=prey cub=club]
    ?:  ?=(%puff -.cub)
      %+  mo-pass
        [%sys %val (scot %p q.q.pry) dap ~]
      [%f %exec our ~ (mo-beak dap) %vale +.cub]
    ?:  ?=(%punk -.cub)
      %+  mo-pass
        [%sys %val (scot %p q.q.pry) dap ~]
      [%f %exec our ~ (mo-beak dap) %cast p.cub %$ q.cub]
    ap-abet:(ap-club:(ap-abed:ap dap pry) cub)
  ::
  ++  mo-club                                           ::  local action
    |=  [dap=dude pry=prey cub=club]
    ^+  +>
    ?:  |(!(~(has by bum) dap) (~(has by wub) dap))
      ::  ~&  [%mo-club-qeu dap cub]
      =+  syf=(fall (~(get by wub) dap) *sofa)
      +>.$(wub (~(put by wub) dap syf(kys (~(put to kys.syf) [hen pry cub]))))
    (mo-clip dap pry cub)
  ::
  ++  mo-gawk                                           ::  ames forward
    |=  [him=@p dap=dude num=@ud rok=rook]
    =.  +>  ?.(?=(%u -.rok) +> (mo-give %mack ~))
    %+  mo-pass  
      [%sys %req (scot %p him) dap (scot %ud num) ~]
    ^-  note-arvo
    ?-  -.rok
      ::  %m  [%f %exec our ~ (mo-beak dap) %vale p.rok q.rok]
      %m  [%g %deal [him our] dap %puff p.rok q.rok]
      %s  [%g %deal [him our] dap %peer p.rok]
      %u  [%g %deal [him our] dap %pull ~]
    ==
  ::
  ++  mo-gawd                                           ::  ames backward
    |=  [him=@p dap=dude num=@ud ron=roon]
    =.  +>  (mo-give %mack ~)
    =.  hen  (mo-ball him num)
    ?-  -.ron
      %d  (mo-give %unto %doff p.ron q.ron)
      %x  (mo-give %unto %quit ~)
    ==
  ::
  ++  ap                                                ::  agent engine
    |_  $:  $:  dap=dude
                pry=prey
                ost=bone
                zip=(list cove)
                dub=(list (each suss tang))
            ==
            seat
        ==
    ::
    ++  ap-abed                                         ::  initialize
      |=  [dap=dude pry=prey]
      ^+  +>
      =:  ^dap   dap
          ^pry   pry
          +>+<+  `seat`(~(got by bum) dap)
        ==
      =+  unt=(~(get by q.zam) hen)
      =:  act.tyc  +(act.tyc)
          eny.tyc  (shax (mix (add dap act.tyc) eny))
          lat.tyc  now
        ==
      ?^  unt
        +>.$(ost u.unt)
      %=  +>.$
        ost      p.zam
        p.zam    +(p.zam)
        q.zam    (~(put by q.zam) hen p.zam)
        r.zam    (~(put by r.zam) p.zam hen)
      ==
    ::
    ++  ap-abet                                         ::  resolve
      ^+  +>
      =>  ap-abut
      %_  +>  
        bum  (~(put by bum) dap +<+)
        moz  :(weld (turn zip ap-aver) (turn dub ap-avid) moz)
      ==
    ::
    ++  ap-abut                                         ::  track queue
      ^+  .
      =+  [pyz=zip ful=*(set bone)]
      |-  ^+  +>
      ?~  pyz
        =+  ded=(~(tap in ful) ~)
        |-  ^+  +>.^$
        ?~  ded  +>.^$
        %=    $
            ded   t.ded
            +>.^$
          %=  ap-kill
            ost      i.ded
            q.q.pry  p:(~(got by sup.ged) i.ded)
          ==
        ==
      ?.  ?=([%give %diff *] q.i.pyz)
        $(pyz t.pyz)
      =^  vad  +>  ap-fill(ost p.i.pyz)
      $(pyz t.pyz, ful ?:(vad ful (~(put in ful) p.i.pyz)))
    ::
    ++  ap-aver                                         ::  cove to move
      |=  cov=cove
      ^-  move 
      :-  (~(got by r.zam) p.cov)
      ?-    -.q.cov
          ?(%slip %sick)  !!
          %give  
        ?<  =(0 p.cov)
        ?.  ?=(%diff -.p.q.cov)
          [%give %unto p.q.cov]
        =+  cay=`cage`p.p.q.cov
        =+  mar=(fall (~(get by pyl) p.cov) p.cay)
        ?:  =(mar p.cay)  [%give %unto p.q.cov]
        :+  %pass
          [%sys %pel dap ~]
        [%f %exec our ~ (mo-beak dap) %cast mar %$ cay]
      ::
          %pass
        :+  %pass  `path`[%use dap p.q.cov]
        ?-  -.q.q.cov
          %hiss  `note-arvo`[%e %hiss p.q.q.cov q.q.q.cov]
          %send  `note-arvo`[%g %deal [our p.q.q.cov] q.q.q.cov]
          %meta  `note-arvo`[`@tas`p.q.q.cov %meta `vase`q.q.q.cov]
        ==
      ==
    ::
    ++  ap-avid                                         ::  onto results
      |=([a=(each suss tang)] [hen %give %onto a])
    ::
    ++  ap-call                                         ::  call into server
      |=  [cog=term arg=vase]
      ^-  [(unit tang) _+>]
      =.  +>  ap-bowl 
      =+  arm=(ap-farm cog)
      ?:  ?=(%| -.arm)  [`p.arm +>.$]
      =+  zem=(ap-slam cog p.arm arg)
      ?:  ?=(%| -.zem)  [`p.zem +>.$]
      (ap-sake p.zem) 
    ::
    ++  ap-club                                         ::  apply effect
      |=  cub=club
      ^+  +>
      ?-  -.cub
        %peel   (ap-peel +.cub)
        %poke   (ap-poke +.cub)
        %peer   (ap-peer +.cub)
        %puff   !!
        %punk   !!
        %pull   ap-pull
        %pump   ap-fall
      ==
    ::
    ++  ap-diff                                         ::  pour a diff
      |=  [her=ship pax=path cag=cage]
      =.  q.cag  (spec q.cag)
      =+  cug=(ap-find [%diff p.cag +.pax])
      ?~  cug
        %.  [| her +.pax]
        ap-pump:(ap-lame %diff (ap-suck "diff: no {<`path`[p.cag +.pax]>}"))
      =+  ^=  arg  ^-  vase
          %-  slop
          ?:  =(0 p.u.cug)
            [!>(`path`+.pax) !>(cag)]
          [!>((slag (dec p.u.cug) `path`+.pax)) q.cag]
      =^  cam  +>.$  (ap-call q.u.cug arg)
      ?^  cam   
        (ap-pump:(ap-lame q.u.cug u.cam) | her pax)
      (ap-pump & her pax)
    ::
    ++  ap-pump                                         ::  update subscription
      |=  [oak=? her=ship pax=path]
      =+  way=[(scot %p her) %out pax]
      ?:  oak
        (ap-pass way %send her -.pax %pump ~)
      (ap-pass:(ap-give %quit ~) way %send her -.pax %pull ~)
    ::
    ++  ap-fall                                         ::  drop from queue
      ^+  .
      ?.  (~(has by sup.ged) ost)  .
      =+  soy=(~(get by qel.ged) ost)
      ?:  |(?=(~ soy) =(0 u.soy))  
        ~&  [%ap-fill-under [our dap] q.q.pry ost]
        +
      =.  u.soy  (dec u.soy)
      ::  ~&  [%ap-fill-sub [[our dap] q.q.pry ost] u.soy]
      ?:  =(0 u.soy)  
        +(qel.ged (~(del by qel.ged) ost))
      +(qel.ged (~(put by qel.ged) ost u.soy))
    ::
    ++  ap-farm                                         ::  produce arm
      |=  cog=term
      ^-  (each vase tang)
      =+  puz=(mule |.((~(mint ut p.hav) [%noun [%cnzy cog]])))
      ?:  ?=(%| -.puz)  [%| p.puz]
      =+  ton=(mock [q.hav q.p.puz] ap-sled)
      ?-  -.ton
        %0  [%& p.p.puz p.ton]
        %1  [%| (turn p.ton |=(a=* (smyt (path a))))]
        %2  [%| p.ton]
      ==
    ::
    ++  ap-fill                                         ::  add to queue
      ^-  [? _.]
      =+  suy=(fall (~(get by qel.ged) ost) 0)
      ?:  =(20 suy)
        ~&  [%ap-fill-full [our dap] q.q.pry ost]
        [%| +]
      ::  ~?  !=(20 suy)  [%ap-fill-add [[our dap] q.q.pry ost] +(suy)]
      [%& +(qel.ged (~(put by qel.ged) ost +(suy)))]
    ::
    ++  ap-find                                         ::  general arm
      |=  [cog=term pax=path]
      =+  dep=0
      |-  ^-  (unit (pair ,@ud term))
      =+  ^=  spu
          ?~  pax  ~ 
          $(pax t.pax, dep +(dep), cog (ap-hype cog i.pax))
      ?^  spu  spu
      ?.((ap-fond cog) ~ `[dep cog])
    ::
    ++  ap-fond                                         ::  check for arm
      |=  cog=term
      ^-  ?
      (slob cog p.hav)
    ::
    ++  ap-give                                         ::  return result
      |=  cit=cuft
      ^+  +>
      +>(zip :_(zip [ost %give cit]))
    ::
    ++  ap-bowl                                         ::  set up bowl
      %_    .
          +12.q.hav
        ^-   bowl
        :*  :*  our                               ::  host
                q.q.pry                           ::  guest
                dap                               ::  agent
            ==                                    ::
            :*  wex=~                             ::  outgoing
                sup=sup.ged                       ::  incoming
            ==                                    ::
            :*  ost=ost                           ::  cause
                act=act.tyc                       ::  tick
                eny=eny.tyc                       ::  nonce
                now=lat.tyc                       ::  time
                byk=byk                           ::  source
        ==  ==                                    ::
      ==
    ::
    ++  ap-hype                                         ::  hyphenate
      |=([a=term b=term] `term`(cat 3 a (cat 3 '-' b)))
    ::
    ++  ap-move                                         ::  process each move
      |=  vax=vase
      ^-  (each cove tang)
      ?@  q.vax    [%| (ap-suck "move: invalid move (atom)")]
      ?^  -.q.vax  [%| (ap-suck "move: invalid move (bone)")]
      ?@  +.q.vax  [%| (ap-suck "move: invalid move (card)")]
      =+  hun=(~(get by r.zam) -.q.vax)
      ?.  (~(has by r.zam) -.q.vax) 
        [%| (ap-suck "move: invalid card (bone {<-.q.vax>})")]
      =+  cav=(slot 3 (spec (slot 3 vax)))
      ?+  +<.q.vax  
               (ap-move-pass -.q.vax +<.q.vax cav)
        %diff  (ap-move-diff -.q.vax cav)
        %hiss  (ap-move-hiss -.q.vax cav)
        %peer  (ap-move-peer -.q.vax cav)
        %pull  (ap-move-pull -.q.vax cav)
        %poke  (ap-move-poke -.q.vax cav)
        %send  (ap-move-send -.q.vax cav)
        %quit  (ap-move-quit -.q.vax cav)
      ==
    ::
    ++  ap-move-quit                                    ::  give quit move
      |=  [sto=bone vax=vase]
      ^-  (each cove tang)
      ?^  q.vax  [%| (ap-suck "move: improper quit")]
      [%& `cove`[sto %give `cuft`[%quit ~]]]
    ::
    ++  ap-move-diff                                    ::  give diff move
      |=  [sto=bone vax=vase]
      =.  vax  (spec vax)
      ^-  (each cove tang)
      ?.  &(?=(^ q.vax) ?=(@ -.q.vax) ((sane %tas) -.q.vax))
        [%| (ap-suck "move: improper diff")]
      [%& sto %give %diff `cage`[-.q.vax (slot 3 (spec vax))]]
    ::
    ++  ap-move-hiss                                    ::  pass %hiss
      |=  [sto=bone vax=vase]
      ^-  (each cove tang)
      ?.  &(?=([p=* q=@ q=^] q.vax) ((sane %tas) q.q.vax))
        [%| (ap-suck "move: malformed hiss ask.[%hiss path mark cage]")]
      =+  gaw=(slot 7 vax)
      ?.  &(?=([p=@ q=^] q.gaw) ((sane %tas) p.q.gaw))
        [%| (ap-suck "move: malformed cage")]
      =+  pux=((soft path) p.q.vax)
      ?.  &(?=(^ pux) (levy u.pux (sane %ta)))
        [%| (ap-suck "move: malformed path")]
      :^  %&  sto  %pass
      :-  [(scot %p q.q.pry) %cay u.pux]
      [%hiss q.q.vax [p.q.gaw (slot 3 (spec gaw))]]
    ::
    ++  ap-move-mess                                    ::  extract path, target
      |=  vax=vase
      ^-  (each (trel path ship term) tang)
      ?.  ?&  ?=([p=* [q=@ r=@] s=*] q.vax)
              (gte 1 (met 7 q.q.vax))
          ==
        [%| (ap-suck "move: malformed target")]
      =+  pux=((soft path) p.q.vax)
      ?.  &(?=(^ pux) (levy u.pux (sane %ta)))
        [%| (ap-suck "move: malformed path")]
      [%& [(scot %p q.q.vax) %out r.q.vax u.pux] q.q.vax r.q.vax]
    ::
    ++  ap-move-pass                                    ::  pass general move 
      |=  [sto=bone wut=* vax=vase]
      ^-  (each cove tang)
      ?.  &(?=(@ wut) ((sane %tas) wut))
        [%| (ap-suck "move: malformed card")]
      =+  pux=((soft path) -.q.vax)
      ?.  &(?=(^ pux) (levy u.pux (sane %ta)))
        [%| (ap-suck "move: malformed path")]
      =+  huj=(ap-vain wut)
      ?~  huj  [%| (ap-suck "move: unknown note {(trip wut)}")]
      :^  %&  sto  %pass
      :-  [(scot %p q.q.pry) %inn u.pux]
      [%meta u.huj (slop (ap-term %tas wut) (slot 3 vax))]
    ::
    ++  ap-move-poke                                    ::  pass %poke
      |=  [sto=bone vax=vase]
      ^-  (each cove tang)
      =+  yep=(ap-move-mess vax)
      ?:  ?=(%| -.yep)  yep
      =+  gaw=(slot 7 vax)
      ?.  &(?=([p=@ q=*] q.gaw) ((sane %tas) p.q.gaw))
        [%| (ap-suck "poke: malformed cage")]
      :^  %&  sto  %pass
      :-  p.p.yep
      [%send q.p.yep r.p.yep %poke p.q.gaw (slot 3 (spec gaw))]
    ::
    ++  ap-move-peer                                    ::  pass %peer
      |=  [sto=bone vax=vase]
      ^-  (each cove tang)
      =+  yep=(ap-move-mess vax)
      ?:  ?=(%| -.yep)  yep
      =+  pux=((soft path) +>.q.vax)
      ?.  &(?=(^ pux) (levy u.pux (sane %ta)))
        [%| (ap-suck "peer: malformed path")]
      :^  %&  sto  %pass
      :-  p.p.yep
      [%send q.p.yep r.p.yep %peer u.pux]
    ::
    ++  ap-move-pull                                    ::  pass %pull
      |=  [sto=bone vax=vase]
      ^-  (each cove tang)
      =+  yep=(ap-move-mess vax)
      ?:  ?=(%| -.yep)  yep
      ?.  =(~ +>.q.vax)
        [%| (ap-suck "pull: malformed card")]
      :^  %&  sto  %pass
      :-  p.p.yep
      [%send q.p.yep r.p.yep %pull ~]
    ::
    ++  ap-move-send                                    ::  pass gall action
      |=  [sto=bone vax=vase]
      ^-  (each cove tang)
      ?.  ?&  ?=([p=* [q=@ r=@] [s=@ t=*]] q.vax)
              (gte 1 (met 7 q.q.vax))
              ((sane %tas) r.q.vax)
          ==
        [%| (ap-suck "move: malformed send")]
      =+  pux=((soft path) p.q.vax)
      ?.  &(?=(^ pux) (levy u.pux (sane %ta)))
        [%| (ap-suck "move: malformed path")]
      ?:  ?=(%poke s.q.vax)
        =+  gav=(spec (slot 7 vax))
        ?>  =(%poke -.q.gav)
        ?.  ?&  ?=([p=@ q=*] t.q.vax)
                ((sane %tas) p.t.q.vax)
            ==
          [%| (ap-suck "move: malformed poke")]
        :^  %&  sto  %pass
        :-  [(scot %p q.q.vax) %out r.q.vax u.pux]
        ^-  cote
        ::  ~&  [%ap-move-send `path`[(scot %p q.q.vax) %out r.q.vax u.pux]]
        [%send q.q.vax r.q.vax %poke p.t.q.vax (slot 3 (spec (slot 3 gav)))]
      =+  cob=((soft club) [s t]:q.vax)
      ?~  cob
        [%| (ap-suck "move: malformed club")]
      :^  %&  sto  %pass
      :-  [(scot %p q.q.vax) %out r.q.vax u.pux]
      ::  ~&  [%ap-move-send `path`[(scot %p q.q.vax) %out r.q.vax u.pux]]
      [%send q.q.vax r.q.vax u.cob]
    ::
    ++  ap-pass                                         ::  request action
      |=  [pax=path coh=cote]
      ^+  +>
      +>(zip :_(zip [ost %pass pax coh]))
    ::
    ++  ap-peep                                         ::  reinstall
      |=  vax=vase
      ^+  +>
      (ap-prep(hav vax) `hav)
    ::
    ++  ap-peel
      |=  [mar=mark pax=path]
      =.  pyl  (~(put by pyl) ost mar)
      (ap-peer pax)
    ::
    ++  ap-peer                                         ::  apply %peer
      |=  pax=path
      ^+  +>
      =.  +>  (ap-peon pax)
      =+  cug=(ap-find %peer pax)
      ?~  cug  +>.$
      =+  old=zip
      =.  zip  ~
      =^  cam  +>.$ 
          %+  ap-call  q.u.cug
          !>(`path`(slag p.u.cug pax))
      =.  zip  (weld zip `(list cove)`[[ost %give %reap cam] old])
      ?^(cam ap-pule +>.$)
    ::
    ++  ap-peon                                         ::  add subscriber
      |=  pax=path
      %_  +>.$
        sup.ged  (~(put by sup.ged) ost [q.q.pry pax])
      ==
    ::
    ++  ap-poke                                         ::  apply %poke
      |=  cag=cage
      ^+  +>
      =+  cug=(ap-find %poke p.cag ~)
      ?~  cug
        (ap-give %coup `(ap-suck "no poke arm for {(trip p.cag)}"))
      ::  ~&  [%ap-poke dap p.cag cug]
      =^  tur  +>.$
          %+  ap-call  q.u.cug
          ?.  =(0 p.u.cug)  q.cag
          (slop (ap-term %tas p.cag) q.cag)
      (ap-give %coup tur)
    ::
    ++  ap-lame                                         ::  pour error
      |=  [wut=@tas why=tang]
      ^+  +>
      =+  cug=(ap-find /lame)
      ?~  cug
        =.  why  [>%ap-lame dap wut< (turn why |=(a=tank rose/[~ "! " ~]^[a]~))]
        ~>  %slog.`rose/["  " "[" "]"]^(flop why)
        +>.$
      =^  cam  +>.$
        %+  ap-call  q.u.cug
        !>([wut why])
      ?^  cam
        =.  why  [>%ap-lame-lame< (turn u.cam |=(a=tank rose/[~ "! " ~]^[a]~))]
        ~>  %slog.`rose/["  " "[" "]"]^(welp (flop why) leaf/"." (flop u.cam))
        +>.$
      +>.$
    ::
    ++  ap-pour                                         ::  generic take
      |=  [pax=path vax=vase]
      ^+  +>
      ?.  &(?=([@ *] q.vax) ((sane %tas) -.q.vax))
        (ap-lame %pour (ap-suck "pour: malformed card"))
      =+  cug=(ap-find [-.q.vax pax])
      ?~  cug
        (ap-lame -.q.vax (ap-suck "pour: no {(trip -.q.vax)}: {<pax>}"))
      =^  cam  +>.$
          %+  ap-call  q.u.cug
          %+  slop
            !>(`path`(slag p.u.cug pax))
          (slot 3 vax)
      ?^  cam  (ap-lame -.q.vax u.cam)
      +>.$
    ::
    ++  ap-purr                                         ::  unwrap take
      |=  [wha=term pax=path cag=cage]
      ^+  +>
      =.  q.cag  (spec q.cag)
      =+  cug=(ap-find [wha p.cag pax])
      ?~  cug
        (ap-lame wha (ap-suck "{(trip wha)}: no {<`path`[p.cag pax]>}"))
      =+  ^=  arg  ^-  vase
          %-  slop
          ?:  =(0 p.u.cug)
            [!>(`path`pax) !>(cag)]
          [!>((slag (dec p.u.cug) `path`pax)) q.cag]
      =^  cam  +>.$  (ap-call q.u.cug arg)
      ?^  cam   (ap-lame q.u.cug u.cam)
      +>.$
    ::
    ++  ap-pout                                         ::  specific take
      |=  [pax=path cuf=cuft]
      ^+  +>
      ?-  -.cuf
        %coup  (ap-take q.q.pry %coup +.pax `!>(p.cuf))
        %diff  (ap-diff q.q.pry pax p.cuf)
        %doff  !!
        %quit  (ap-take q.q.pry %quit +.pax ~)
        %reap  (ap-take q.q.pry %reap +.pax `!>(p.cuf))
      ==
    ::
    ++  ap-prep                                         ::  install
      |=  vux=(unit vase)
      ^+  +>
      =^  gac  +>.$  (ap-prop vux)
      %=    +>.$
          dub
        :_(dub ?~(gac [%& dap ?~(vux %boot %bump) now] [%| u.gac]))
      ==
    ::
    ++  ap-prop                                         ::  install
      |=  vux=(unit vase)
      ^-  [(unit tang) _+>]
      ?.  (ap-fond %prep) 
        ?~  vux
          `+>.$
        =+  [new=p:(slot 13 hav) old=p:(slot 13 u.vux)]
        ?.  (~(nest ut p:(slot 13 hav)) %| p:(slot 13 u.vux))
          :_(+>.$ `(ap-suck "prep mismatch"))
        `+>.$(+13.q.hav +13.q.u.vux)
      =^  tur  +>.$
          %+  ap-call  %prep
          ?~(vux !>(~) (slop !>(~) (slot 13 u.vux)))
      ?~(tur `+>.$ :_(+>.$ `u.tur))
    ::
    ++  ap-pule                                         ::  silent delete
      =+  wim=(~(get by sup.ged) ost)
      ?~  wim  +
      %_  +
        sup.ged  (~(del by sup.ged) ost)
        qel.ged  (~(del by qel.ged) ost)
      ==
    ::
    ++  ap-pull                                         ::  load delete
      =+  wim=(~(get by sup.ged) ost)
      ?~  wim  ~&(%ap-pull-none +)
      =:  sup.ged  (~(del by sup.ged) ost)
          qel.ged  (~(del by qel.ged) ost)
        ==
      =+  cug=(ap-find %pull q.u.wim)
      ?~  cug  +>
      =^  cam  +> 
        %+  ap-call  q.u.cug
        !>((slag p.u.cug q.u.wim))
      ?^  cam  (ap-lame q.u.cug u.cam)
      +>+
    ::
    ++  ap-kill                                         ::  queue kill
      ~&  [%ap-kill dap ost]
      (ap-give:ap-pull %quit ~)
    ::
    ++  ap-take                                         ::  non-diff gall take
      |=  [her=ship cog=term pax=path vux=(unit vase)]
      ^+  +>
      =+  cug=(ap-find cog pax)
      ?~  cug
        ~&  [%ap-take-none cog pax]
        +>.$
      =^  cam  +>.$  
        %+  ap-call  q.u.cug
        =+  den=!>((slag p.u.cug pax))
        ?~(vux den (slop den u.vux))
      ?^  cam  (ap-lame q.u.cug u.cam)
      +>.$
    ::
    ++  ap-safe                                         ::  process move list
      |=  vax=vase
      ^-  (each (list cove) tang)
      ?~  q.vax  [%& ~]
      ?@  q.vax  [%| (ap-suck "move: malformed list")]
      =+  sud=(ap-move (slot 2 vax))
      ?:  ?=(%| -.sud)  sud
      =+  res=$(vax (slot 3 vax))
      ?:  ?=(%| -.res)  res
      [%& p.sud p.res]
    ::
    ++  ap-sake                                         ::  handle result
      |=  vax=vase
      ^-  [(unit tang) _+>]
      ?:  ?=(@ q.vax)
        [`(ap-suck "sake: invalid product (atom)") +>.$]
      =+  muz=(ap-safe (slot 2 vax))
      ?:  ?=(%| -.muz)  [`p.muz +>.$]
      =+  sav=(ap-save (slot 3 vax))
      ?:  ?=(%| -.sav)  [`p.sav +>.$]
      :-  ~
      %_  +>.$
        zip  (weld (flop p.muz) zip)
        hav  p.sav 
      ==
    ::
    ++  ap-save                                         ::  verify core
      |=  vax=vase
      ^-  (each vase tang)
      ?.  (~(nest ut p.hav) %| p.vax)
        [%| (ap-suck "invalid core")]
      [%& vax]
    ::
    ++  ap-slam                                         ::  virtual slam
      |=  [cog=term gat=vase arg=vase]
      ^-  (each vase tang)
      =+  wiz=(mule |.((slit p.gat p.arg)))
      ?:  ?=(%| -.wiz)
        %-  =+  sam=(~(peek ut p.gat) %free 6)
            (slog >%ap-slam-mismatch< ~(duck ut p.arg) ~(duck ut sam) ~)
        [%| (ap-suck "call: {<cog>}: type mismatch")]
      =+  ton=(mong [q.gat q.arg] ap-sled)
      ?-  -.ton
        %0  [%& p.wiz p.ton]
        %1  [%| (turn p.ton |=(a=* (smyt (path a))))]
        %2  [%| p.ton]
      ==
    ::
    ++  ap-sled  (mole (slod ska))                      ::  namespace view
    ++  ap-suck                                         ::  standard tang
      |=  msg=tape
      ^-  tang
      [%leaf (weld "gall: {<dap>}: " msg)]~
    ::
    ++  ap-term                                         ::  atomic vase
      |=  [a=@tas b=@]  
      ^-  vase
      [[%cube b %atom a] b]
    ::
    ++  ap-vain                                         ::  card to vane
      |=  sep=@tas
      ^-  (unit ,@tas)
      ?+  sep  ~&  [%ap-vain sep]
               ~
        %cash  `%a
        %conf  `%g
        %deal  `%g
        %exec  `%f
        %flog  `%d
        %info  `%c
        %merg  `%c
        %mont  `%c
        %ogre  `%c
        %them  `%e
        %wait  `%b
        %want  `%a
        %wont  `%a                                      ::  XX for begin; remove
        %warp  `%c
      ==
    --
  --
++  call                                                ::  request
  |=  [hen=duct hic=(hypo (hobo kiss-gall))]
  ^-  [p=(list move) q=_..^$]
  =>  .(q.hic ?.(?=(%soft -.q.hic) q.hic ((hard kiss-gall) p.q.hic)))
  ?-    -.q.hic
      %conf
    ?.  (~(has by pol.all) p.p.q.hic)
      ~&  [%gall-not-ours p.p.q.hic]
      [~ ..^$]
    mo-abet:(mo-conf:(mo-abed:mo p.p.q.hic hen) q.p.q.hic q.q.hic)
  ::
      %deal
    =<  mo-abet
    ?.  (~(has by pol.all) q.p.q.hic)                   ::  either to us
      ?>  (~(has by pol.all) p.p.q.hic)                 ::  or from us
      (mo-away:(mo-abed:mo p.p.q.hic hen) q.p.q.hic q.q.hic)
    (mo-come:(mo-abed:mo q.p.q.hic hen) p.p.q.hic q.q.hic)
  ::
      %init 
    ::  ~&  [%gall-init p.q.hic]
    [~ ..^$(pol.all (~(put by pol.all) p.q.hic [hen ~ ~ ~]))]
  ::
      %west
    ?.  (~(has by pol.all) p.p.q.hic)
      ~&  [%gall-not-ours p.q.hic]
      [~ ..^$]
    ?>  ?=([?(%ge %gh) @ ~] q.q.hic)
    =+  dap=i.t.q.q.hic
    =+  our=p.p.q.hic
    =+  him=q.p.q.hic
    ?:  ?=(%ge i.q.q.hic)
      =+  mes=((hard ,[@ud rook]) r.q.hic)
      =<  mo-abet
      (mo-gawk:(mo-abed:mo our hen) him dap mes)
    =+  mes=((hard ,[@ud roon]) r.q.hic)
    =<  mo-abet
    (mo-gawd:(mo-abed:mo our hen) him dap mes)
  ::
      %wegh
    :_  ..^$  :_  ~
    :^  hen  %give  %mass
    :-  %gall
    :-  %|
    :~  all/`all
    ==
  ==
::
++  doze                                                ::  sleep until
  |=  [now=@da hen=duct]
  ^-  (unit ,@da)
  ~
::
++  load                                                ::  recreate vane
  |=  old=axle-n
  ^+  ..^$
  ?:  ?=(%1 -.old)  ..^$(all old)
  %=  $
    old  =>  |=(seat-0 `seat`+<(zam [~ zam]))
         =>  |=(mast-0 +<(bum (~(run by bum) +>)))
         old(- %1, pol (~(run by pol.old) .))
  ==
::
++  scry
  |=  [fur=(unit (set monk)) ren=@tas who=ship syd=desk lot=coin tyl=path]
  ^-  (unit (unit (pair mark ,*)))
  [~ ~]
::
++  stay                                                ::  save w/o cache
  `axle`all
::
++  take                                                ::  response
  |=  [tea=wire hen=duct hin=(hypo sign-arvo)]
  ^-  [p=(list move) q=_..^$]
  ~|  [%gall-take tea]
  ?>  ?=([@ ?(%sys %use) *] tea) 
  =+  our=(need (slaw %p i.tea))
  =+  mow=(mo-abed:mo our hen)
  ?:  ?=(%sys i.t.tea)
    mo-abet:(mo-cyst:mow t.t.tea q.hin)
  ?>  ?=(%use i.t.tea)
  mo-abet:(mo-cook:mow t.t.tea hin)
--
