::  ::  %gall, agent execution  
!?  163
::::
|=  pit/vase
=,  gall
=>  =~
|%  ::::::::::::::::::::::::::::::::::::::::::::::::::::::    rest of arvo
    ::::::::::::::::::::::::::::::::::::::::::::::::::::::
++  volt  ?($low $high)                                 ::  voltage
++  torc  $@(?($iron $gold) {$lead p/ship})             ::  security control
++  roon                                                ::  reverse ames msg
  $%  {$d p/mark q/*}                                   ::  diff (diff)
      {$x $~}                                           ::
  ==                                                    ::
++  rook                                                ::  forward ames msg
  $%  {$m p/mark q/*}                                   ::  message
      {$l p/mark q/path}                                ::  "peel" subscribe
      {$s p/path}                                       ::  subscribe
      {$u $~}                                           ::  cancel+unsubscribe
  ==                                                    ::
--                                                      ::
|%  ::::::::::::::::::::::::::::::::::::::::::::::::::::::    local arvo
    ::::::::::::::::::::::::::::::::::::::::::::::::::::::
++  cote                                                ::  ++ap note
  $%  {$meta p/@tas q/vase}                             ::  
      {$send p/ship q/cush}                             ::  
      {$hiss p/(unit knot) q/mark r/cage}               ::
  ==                                                    ::
++  cove  (pair bone (wind cote cuft))                  ::  internal move
++  move  {p/duct q/(wind note-arvo gift-arvo)}         ::  typed move
--                                                      ::
|%  ::::::::::::::::::::::::::::::::::::::::::::::::::::::    %gall state
    ::::::::::::::::::::::::::::::::::::::::::::::::::::::
++  axle-n  ?(axle)                                     ::  upgrade path
    ::::::::::::::::::::::::::::::::::::::::::::::::::::::  state proper
    ::::::::::::::::::::::::::::::::::::::::::::::::::::::
++  axle                                                ::  all state
  $:  $0                                                ::  state version
      pol/(map ship mast)                               ::  apps by ship
  ==                                                    ::
++  gest                                                ::  subscriber data
  $:  sup/bitt                                          ::  incoming subscribers
      neb/boat                                          ::  outgoing subscribers
      qel/(map bone @ud)                                ::  queue meter
  ==                                                    ::
++  mast                                                ::  ship state
  $:  mak/*                                             ::  (deprecated)
      sys/duct                                          ::  system duct
      sap/(map ship scad)                               ::  foreign contacts
      bum/(map dude seat)                               ::  running agents
      wub/(map dude sofa)                               ::  waiting queue
  ==                                                    ::
++  ffuc                                                ::  new cuff
    $:  p/(unit (set ship))                             ::  disclosing to
        q/ship                                          ::  attributed to
    ==                                                  ::
++  prey  (pair volt ffuc)                              ::  privilege
++  scad                                                ::  foreign connection
  $:  p/@ud                                             ::  index
      q/(map duct @ud)                                  ::  by duct
      r/(map @ud duct)                                  ::  by index
  ==                                                    ::
++  scar                                                ::  opaque input
  $:  p/@ud                                             ::  bone sequence
      q/(map duct bone)                                 ::  by duct
      r/(map bone duct)                                 ::  by bone
  ==                                                    ::  
::                                                      ::  
::  XX a hack, required to break a subscription loop    ::
::  which arises when an invalid mark crashes a diff.   ::
::  See usage in ap-misvale.                            ::
++  misvale-data  (set wire)                            ::  subscrs w/ bad marks
++  seat                                                ::  agent state
  $:  misvale/misvale-data                              ::  bad reqs
      vel/worm                                          ::  cache
      mom/duct                                          ::  control duct 
      liv/?                                             ::  unstopped
      toc/torc                                          ::  privilege
      tyc/stic                                          ::  statistics
      ged/gest                                          ::  subscribers
      hav/vase                                          ::  running state
      byk/beak                                          ::  update control
      pyl/(map bone mark)                               ::  req'd translations
      zam/scar                                          ::  opaque ducts
  ==                                                    ::
++  sofa                                                ::  queue for blocked
  $:  kys/(qeu (trel duct prey club))                   ::  queued kisses
  ==                                                    ::
++  stic                                                ::  statistics
  $:  act/@ud                                           ::  change number
      eny/@uvJ                                          ::  entropy
      lat/@da                                           ::  time
  ==                                                    ::
--                                                      ::
    ::::::::::::::::::::::::::::::::::::::::::::::::::::::  vane header
    ::::::::::::::::::::::::::::::::::::::::::::::::::::::
.  ==
=|  all/axle                                            ::  all vane state
|=  $:  now/@da                                         ::  urban time
        eny/@uvJ                                        ::  entropy
        ska/sley                                        ::  activate
    ==                                                  ::  opaque core
~%  %gall-top  ..is  ~
|%  ::::::::::::::::::::::::::::::::::::::::::::::::::::::  state machine
    ::::::::::::::::::::::::::::::::::::::::::::::::::::::
++  mo
  ~%  %gall-mo  +>  ~ 
  |_  $:  $:  our/@p 
              hen/duct
              moz/(list move)
          ==
          mast
      ==
  ++  mo-abed                                           ::  initialize
    |=  {our/@p hen/duct}
    ^+  +>
    %_    +>
      our  our
      hen  hen
      +<+  (~(got by pol.all) our)
    ==
  ::
  ++  mo-abet                                           ::  resolve to 
    ^+  [*(list move) +>+]
    :_  +>+(pol.all (~(put by pol.all) our +<+))
    %-  flop
    %+  turn  moz
    |=  a/move
    ?.  ?=($pass -.q.a)  a
    [p.a %pass [(scot %p our) p.q.a] q.q.a]
  ::
  ++  mo-conf                                           ::  configure
    |=  {dap/dude lum/culm}
    (mo-boot dap ?:((~(has by bum) dap) %old %new) p.p.lum q.p.lum da+now)
  ::
  ++  mo-pass                                           ::  standard pass
    |=  {pax/path noh/note-arvo}
    %_(+> moz :_(moz [hen %pass pax noh]))
  ::
  ++  mo-give
    |=  git/gift:able
    %_(+> moz :_(moz [hen %give git]))
  ::
  ++  mo-okay                                           ::  valid agent core
    |=  vax/vase
    ^-  ?
    =+  bol=(slew 12 vax)
    ?~  bol  |
    (~(nest ut p.u.bol) %| -:!>(*bowl))
  ::
  ++  mo-boom                                           ::  complete new boot
    |=  {dap/dude byk/beak dep/@uvH gux/gage:ford}
    ^+  +>
    ?-    -.gux
        $tabl  ~|(%made-tabl !!)
        $|
      =.  +>  (mo-bold byk dap dep)
      =.  +>  (mo-give %onto %| p.gux)
      +>
        $&
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
    |=  {dap/dude byk/beak hav/vase}
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
    |=  {dap/dude byk/beak dep/@uvH gux/gage:ford}
    ^+  +>
    =+  sut=(~(get by bum) dap)
    ?~  sut
      ~&  [%gall-old-boon dap]
      +>.$
    =.  bum  (~(put by bum) dap u.sut(byk byk))
    =.  +>.$  (mo-bold byk dap dep)
    ?-  -.gux
      $tabl  ~|(%made-tabl !!)
      $|     (mo-give %onto %| p.gux)
      $&     ?>  ?=(@ p.p.gux)
             ap-abet:(ap-peep:(ap-abed:ap dap [%high [~ our]]) q.p.gux)
    ==
  ::
  ++  mo-bold                                           ::  wait for dep
    |=  {byk/beak dap/dude dep/@uvH}
    ^+  +>
    %+  mo-pass  [%sys %dep (scot %p p.byk) q.byk dap ~] 
    [%f %wasp our dep &] 
  ::
  ++  mo-boot                                           ::  create ship
    |=  {dap/dude how/?($new $old) byk/beak}
    ^+  +>
    ::  ~&  [%mo-boot dap how byk]
    %+  mo-pass  [%sys how dap (scot %p p.byk) q.byk (scot r.byk) ~]
    ^-  note-arvo
    [%f %exec our `[byk %core [byk [dap %app ~]]]]
  ::
  ++  mo-away                                           ::  foreign request
    |=  {him/ship caz/cush}                             ::  
    ^+  +>
    ::  ~&  [%mo-away him caz]
    ?:  ?=($pump -.q.caz)
      ::
      ::  you'd think this would send an ack for the diff
      ::  that caused this pump.  it would, but we already
      ::  sent it when we got the diff in ++mo-cyst.  then
      ::  we'd have to save the network duct and connect it
      ::  to this returning pump.
      ::
      +>
    ?:  ?=($peer-not -.q.caz)
      ::  short circuit error
      (mo-give %unto %reap (some p.q.caz))
    =^  num  +>.$  (mo-bale him)
    =+  ^=  roc  ^-  rook
        ?-  -.q.caz
          $poke  [%m p.p.q.caz q.q.p.q.caz]
          $pull  [%u ~]
          $puff  !!
          $punk  !!
          $peel  [%l p.q.caz q.q.caz]
          $peer  [%s p.q.caz]
        ==
    %+  mo-pass  
      [%sys %way -.q.caz ~]
    `note-arvo`[%a %want [our him] [%g %ge p.caz ~] [num roc]]
  ::
  ++  mo-baal                                           ::  error convert a
    |=  art/(unit ares)
    ^-  ares
    ?~(art ~ ?~(u.art `[%blank ~] u.art))
  ::
  ++  mo-baba                                           ::  error convert b
    |=  ars/ares
    ^-  (unit tang)
    ?~  ars  ~
    `[[%leaf (trip p.u.ars)] q.u.ars]
  ::
  ++  mo-awed                                           ::  foreign response
    |=  {him/ship why/?($peer $peel $poke $pull) art/(unit ares)}
    ^+  +>
    ::  ~&  [%mo-awed him why art]
    =+  tug=(mo-baba (mo-baal art))
    ?-  why
      $peel  (mo-give %unto %reap tug)
      $peer  (mo-give %unto %reap tug)
      $poke  (mo-give %unto %coup tug)
      $pull  +>.$
    ==
  ::
  ++  mo-bale                                           ::  assign outbone
    |=  him/ship 
    ^-  {@ud _+>}
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
    |=  {him/ship num/@ud}
    ^-  duct
    (~(got by r:(~(got by sap) him)) num)
  ::
  ++  mo-come                                           ::  handle locally
    |=  {her/ship caz/cush}
    ^+  +>
    =+  pry=`prey`[%high [~ her]]
    (mo-club p.caz pry q.caz)
  ::
  ++  mo-coup                                           ::  back from mo-away
    |=  {dap/dude him/ship cup/ares}
    %^  mo-give  %unto  %coup
    ?~  cup  ~
    [~ `tang`[[%leaf (trip p.u.cup)] q.u.cup]]
  ::
  ++  mo-chew                                           ::  reverse build path
    |=  pax/path
    ^-  beak
    ?>  ?=({@ @ @ $~} pax)
    [(slav %p i.pax) i.t.pax da+(slav %da i.t.t.pax)]
  ::
  ++  mo-cyst                                           ::  take in /sys
    |=  {pax/path sih/sign-arvo}
    ^+  +>
    ?+    -.pax  !!
        $dep                                            ::  update
      ?>  ?=({$f $news *} sih)
      ?>  ?=({@ @ @ $~} t.pax)
      %^  mo-boot  i.t.t.t.pax
        ?:((~(has by bum) i.t.t.t.pax) %old %new)
      [(slav %p i.t.pax) i.t.t.pax [%da now]]
    ::
        $new
      ?>  ?=({$f $made *} sih)
      ?>  ?=({@ @ @ @ $~} t.pax)
      (mo-boom i.t.pax (mo-chew t.t.pax) +>.sih)
    ::
        $old                                            ::  reload old
      ?>  ?=({$f $made *} sih)
      ?>  ?=({@ @ @ @ $~} t.pax)
      (mo-boon i.t.pax (mo-chew t.t.pax) +>.sih)
    ::
        $pel                                            ::  translated peer
      ?>  ?=({@ $~} t.pax)
      =+  mar=i.t.pax
      ?>  ?=({$f $made *} sih)
      ?-  -.q.+.sih
        $tabl  ~|(%made-tabl !!)
        $&    (mo-give %unto %diff p.q.+>.sih)
        $|     =.  p.q.+>.sih  (turn p.q.+>.sih |=(a/tank rose+[~ "! " ~]^[a]~))
              ~>  %slog.`%*(. >[%wh %y]< +> [>%mo-cyst-fail< (flop p.q.+>.sih)])
              (mo-give %unto %quit ~)   ::  XX better errors pls
      ==
    ::
        $red                                            ::  diff ack
      ?>  ?=({@ @ @ $~} t.pax)
      ?.  ?=({$a $woot *} sih)
        ~&  [%red-went pax]
        +>.$
      =+  :*  him=(slav %p i.t.pax)
              dap=i.t.t.pax
              num=(slav %ud i.t.t.t.pax)
          ==
      =>  .(pax `path`[%req t.pax])
      ?~  q.+>.sih 
        (mo-pass [%sys pax] %g %deal [him our] dap %pump ~)
      ~&  [%diff-bad-ack q.+>.sih]                      ::  should not happen
      =.  +>.$  (mo-pass [%sys pax] %g %deal [him our] dap %pull ~)
      (mo-pass [%sys pax] %a %want [our him] [%g %gh dap ~] [num %x ~])
    ::
        $rep                                            ::  reverse request
      ?>  ?=({@ @ @ $~} t.pax)
      ?>  ?=({$f $made *} sih)
      =+  :*  him=(slav %p i.t.pax)
              dap=i.t.t.pax
              num=(slav %ud i.t.t.t.pax)
          ==
      ?-  -.q.+>.sih
        $tabl  ~|(%made-tabl !!)
        $|     (mo-give %mack `p.q.+>.sih)                  ::  XX should crash
        $&     =.  +>.$  (mo-give %mack ~)             ::  XX pump should ack
               (mo-give(hen (mo-ball him num)) %unto %diff `cage`p.q.+>.sih)
      ==
    ::
        $req                                            ::  inbound request
      ?>  ?=({@ @ @ $~} t.pax)
      =+  :*  him=(slav %p i.t.pax)
              dap=i.t.t.pax
              num=(slav %ud i.t.t.t.pax)
          ==
      ?:  ?=({$f $made *} sih)
        ?-  -.q.+>.sih
          $tabl  ~|(%made-tabl !!)
          $|  (mo-give %mack `p.q.+>.sih)               ::  XX should crash
          $&  (mo-pass [%sys pax] %g %deal [him our] i.t.t.pax %poke p.q.+>.sih)
        ==
      ?:  ?=({$a $woot *} sih)  +>.$                    ::  quit ack, boring
      ?>  ?=({$g $unto *} sih)
      =+  cuf=`cuft`+>.sih
      ?-    -.cuf
        $coup  (mo-give %mack p.cuf)
        $diff  %+  mo-pass  [%sys %red t.pax]
               [%a %want [our him] [%g %gh dap ~] [num %d p.p.cuf q.q.p.cuf]]
        $quit  %+  mo-pass  [%sys pax]
               [%a %want [our him] [%g %gh dap ~] [num %x ~]]
        $reap  (mo-give %mack p.cuf)
      ==
    ::
        $val                                            ::  inbound validate
      ?>  ?=({@ @ $~} t.pax)
      =+  [him=(slav %p i.t.pax) dap=i.t.t.pax]
      ?>  ?=({$f $made *} sih)
      ?-  -.q.+>.sih
        $tabl  !!
        $|     (mo-give %unto %coup `p.q.+>.sih)        ::  XX invalid, crash
        $&     (mo-clip dap `prey`[%high ~ him] %poke p.q.sih)
      ==
    ::
        $way                                            ::  outbound request
      ?>  ?=({$a $woot *} sih)
      ?>  ?=({@ $~} t.pax)
      %-  mo-awed
      :*  `ship`p.+>.sih
          ;;(?($peer $peel $poke $pull) i.t.pax)
          +>+.sih
      ==
    ==
  ::
  ++  mo-cook                                           ::  take in /use
    |=  {pax/path hin/(hypo sign-arvo)}
    ^+  +>
    ?.  ?=({@ @ $?($inn $out $cay) *} pax)
      ~&  [%mo-cook-bad-pax pax]
      !!
    =+  dap=`@tas`i.pax
    =+  pry=`prey`[%high [~ (slav %p i.t.pax)]]
    =+  pap=(ap-abed:ap dap pry) 
    =+  vax=(slot 3 `vase`hin)
    ?-  i.t.t.pax
      $inn  ap-abet:(ap-pour:pap t.t.t.pax (slot 3 `vase`hin))      
      $cay  ?.  ?=({$e $sigh *} q.hin)
              ~&  [%mo-cook-weird q.hin]
              ~&  [%mo-cook-weird-path pax]
              +>.$
            ap-abet:(ap-purr:pap +<.q.hin t.t.t.pax +>.q.hin)
    ::
      $out  ?.  ?=({$g $unto *} q.hin)
              ~&  [%mo-cook-weird q.hin]
              ~&  [%mo-cook-weird-path pax]
              +>.$
            ap-abet:(ap-pout:pap t.t.t.pax +>.q.hin)
    ==
  ::
  ++  mo-claw                                           ::  clear queue
    |=  dap/dude
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
    |=  dap/dude
    =-  ?.(=(p our) - -(r [%da now])) ::  soft dependencies
    ^-  beak
    byk:(~(got by bum) dap)
  ::
  ++  mo-peek
    |=  {dap/dude pry/prey ren/@tas tyl/path}
    ^-  (unit (unit cage))
    (ap-peek:(ap-abed:ap dap pry) ren tyl)
  ::
  ++  mo-clip                                           ::  apply club
    |=  {dap/dude pry/prey cub/club}
    ?:  ?=($puff -.cub)
      %+  mo-pass
        [%sys %val (scot %p q.q.pry) dap ~]
      [%f %exec our ~ (mo-beak dap) %vale +.cub]
    ?:  ?=($punk -.cub)
      %+  mo-pass
        [%sys %val (scot %p q.q.pry) dap ~]
      [%f %exec our ~ (mo-beak dap) %cast p.cub %$ q.cub]
    ?:  ?=($peer-not -.cub)
      (mo-give %unto %reap (some p.cub))
    ap-abet:(ap-club:(ap-abed:ap dap pry) cub)
  ::
  ++  mo-club                                           ::  local action
    |=  {dap/dude pry/prey cub/club}
    ^+  +>
    ?:  |(!(~(has by bum) dap) (~(has by wub) dap))
      ~&  >>  [%mo-not-running dap -.cub]
      ::  ~&  [%mo-club-qeu dap cub]
      =+  syf=(fall (~(get by wub) dap) *sofa)
      +>.$(wub (~(put by wub) dap syf(kys (~(put to kys.syf) [hen pry cub]))))
    (mo-clip dap pry cub)
  ::
  ++  mo-gawk                                           ::  ames forward
    |=  {him/@p dap/dude num/@ud rok/rook}
    =.  +>  ?.(?=($u -.rok) +> (mo-give %mack ~))
    %+  mo-pass  
      [%sys %req (scot %p him) dap (scot %ud num) ~]
    ^-  note-arvo
    ?-  -.rok
      ::  %m  [%f %exec our ~ (mo-beak dap) %vale p.rok q.rok]
      $m  [%g %deal [him our] dap %puff p.rok q.rok]
      $l  [%g %deal [him our] dap %peel p.rok q.rok]
      $s  [%g %deal [him our] dap %peer p.rok]
      $u  [%g %deal [him our] dap %pull ~]
    ==
  ::
  ++  mo-gawd                                           ::  ames backward
    |=  {him/@p dap/dude num/@ud ron/roon}
    ?-    -.ron
        $d
      %+  mo-pass
        [%sys %rep (scot %p him) dap (scot %ud num) ~]
      [%f %exec our ~ (mo-beak dap) %vale p.ron q.ron]
    ::
        $x  =.  +>  (mo-give %mack ~)                  ::  XX should crash
            (mo-give(hen (mo-ball him num)) %unto %quit ~)
    ==
  ::
  ++  ap                                                ::  agent engine
    ~%  %gall-ap  +>  ~
    |_  $:  $:  dap/dude
                pry/prey
                ost/bone
                zip/(list cove)
                dub/(list (each suss tang))
            ==
            seat
        ==
    ::
    ++  ap-abed                                         ::  initialize
      |=  {dap/dude pry/prey}
      ^+  +>
      =:  ^dap   dap
          ^pry   pry
          +>+<+  `seat`(~(got by bum) dap)
        ==
      =+  unt=(~(get by q.zam) hen)
      =:  act.tyc  +(act.tyc)
          eny.tyc  (shaz (mix (add dap act.tyc) eny))
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
      ?^  pyz
        ?.  ?=({$give $diff *} q.i.pyz)
          $(pyz t.pyz)
        =^  vad  +>  ap-fill(ost p.i.pyz)
        $(pyz t.pyz, ful ?:(vad ful (~(put in ful) p.i.pyz)))
      =+  ded=~(tap in ful)
      |-  ^+  +>.^$
      ?~  ded  +>.^$
      =>  %*(. $(ded t.ded) ost i.ded)
      =+  tib=(~(get by sup.ged) ost)
      ?~  tib  ~&([%ap-abut-bad-bone dap ost] ..ap-kill)
      ap-kill(q.q.pry p.u.tib)
    ::
    ++  ap-aver                                         ::  cove to move
      |=  cov/cove
      ^-  move 
      :-  (~(got by r.zam) p.cov)
      ?-    -.q.cov
          ?($slip $sick)  !!
          $give  
        ?<  =(0 p.cov)
        ?.  ?=($diff -.p.q.cov)
          [%give %unto p.q.cov]
        =+  cay=`cage`p.p.q.cov
        =+  mar=(fall (~(get by pyl) p.cov) p.cay)
        ?:  =(mar p.cay)  [%give %unto p.q.cov]
        :+  %pass
          [%sys %pel dap ~]
        [%f %exec our ~ (mo-beak dap) %cast mar %$ cay]
      ::
          $pass
        :+  %pass  `path`[%use dap p.q.cov]
        ?-  -.q.q.cov
          $hiss  `note-arvo`[%e %hiss +.q.q.cov]
          $send  `note-arvo`[%g %deal [our p.q.q.cov] q.q.q.cov]
          $meta  `note-arvo`[`@tas`p.q.q.cov %meta `vase`q.q.q.cov]
        ==
      ==
    ::
    ++  ap-avid                                         ::  onto results
      |=(a/(each suss tang) [hen %give %onto a])
    ::
    ++  ap-call                                         ::  call into server
      ~/  %ap-call
      |=  {cog/term arg/vase}
      ^-  {(unit tang) _+>}
      =.  +>  ap-bowl 
      =^  arm  +>.$  (ap-farm cog)
      ?:  ?=($| -.arm)  [`p.arm +>.$]
      =^  zem  +>.$  (ap-slam cog p.arm arg)
      ?:  ?=($| -.zem)  [`p.zem +>.$]
      (ap-sake p.zem) 
    ::
    ++  ap-peek
      |=  {ren/@tas tyl/path}
      ^-  (unit (unit cage))
      =+  ?.  ?=($x ren)
            [mar=%$ tyl=tyl]
          =+  `path`(flop tyl)
          ?>  ?=(^ -)
          [mar=i tyl=(flop t)]
      =+  cug=(ap-find %peek ren tyl)
      ?~  cug
        ((slog leaf+"peek find fail" >tyl< >mar< ~) [~ ~])
      =^  arm  +>.$  (ap-farm q.u.cug)
      ?:  ?=($| -.arm)  ((slog leaf+"peek farm fail" p.arm) [~ ~])
      =^  zem  +>.$  (ap-slam q.u.cug p.arm !>((slag p.u.cug `path`[ren tyl]))) 
      ?:  ?=($| -.zem)  ((slog leaf+"peek slam fail" p.zem) [~ ~])
      ?+  q.p.zem  ((slog leaf+"peek bad result" ~) [~ ~])
        $~              ~
        {$~ $~}         [~ ~]
        {$~ $~ ^}
          =+  caz=(spec (slot 7 p.zem))
          ?.  &(?=({p/@ *} q.caz) ((sane %tas) p.q.caz))
            ((slog leaf+"scry: malformed cage" ~) [~ ~])
          ?.  =(mar p.q.caz)
            [~ ~]
          ``[p.q.caz (slot 3 caz)]
      ==
    ::
    ++  ap-club                                         ::  apply effect
      |=  cub/club
      ^+  +>
      ?-  -.cub
        $peel   (ap-peel +.cub)
        $poke   (ap-poke +.cub)
        $peer   (ap-peer +.cub)
        $puff   !!
        $punk   !!
        $peer-not   !!
        $pull   ap-pull
        $pump   ap-fall
      ==
    ::
    ++  ap-diff                                         ::  pour a diff
      |=  {her/ship pax/path cag/cage}
      ::  =.  q.cag  (spec q.cag)
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
      |=  {oak/? her/ship pax/path}
      =+  way=[(scot %p her) %out pax]
      ?:  oak
        (ap-pass way %send her -.pax %pump ~)
      (ap-pass:(ap-give %quit ~) way %send her -.pax %pull ~)
    ::
    ++  ap-fall                                         ::  drop from queue
      ^+  .
      ?.  (~(has by sup.ged) ost)  .
      =+  soy=(~(get by qel.ged) ost)
      ?:  |(?=($~ soy) =(0 u.soy))  
        ::  ~&  [%ap-fill-under [our dap] q.q.pry ost]
        +
      =.  u.soy  (dec u.soy)
      ::  ~&  [%ap-fill-sub [[our dap] q.q.pry ost] u.soy]
      ?:  =(0 u.soy)  
        +(qel.ged (~(del by qel.ged) ost))
      +(qel.ged (~(put by qel.ged) ost u.soy))
    ::
    ++  ap-farm                                         ::  produce arm
      ~/  %ap-farm
      |=  cog/term
      ^-  {(each vase tang) _+>}
      =+  pyz=(mule |.((~(mint wa vel) p.hav [%limb cog])))
      ?:  ?=($| -.pyz)  
        :_(+>.$ [%| +.pyz])
      :_  +>.$(vel `worm`+>.pyz)
      =+  ton=(mock [q.hav q.+<.pyz] ap-sled)
      ?-  -.ton
        $0  [%& p.+<.pyz p.ton]
        $1  [%| (turn p.ton |=(a/* (smyt (path a))))]
        $2  [%| p.ton]
      ==
    ::
    ++  ap-fill                                         ::  add to queue
      ^-  {? _.}
      =+  suy=(fall (~(get by qel.ged) ost) 0)
      ?:  =(20 suy)
        ::  ~&  [%ap-fill-full [our dap] q.q.pry ost]
        [%| +]
      ::  ~?  !=(20 suy)  [%ap-fill-add [[our dap] q.q.pry ost] +(suy)]
      [%& +(qel.ged (~(put by qel.ged) ost +(suy)))]
    ::
    ++  ap-find                                         ::  general arm
      |=  {cog/term pax/path}
      =+  dep=0
      |-  ^-  (unit (pair @ud term))
      =+  ^=  spu
          ?~  pax  ~ 
          $(pax t.pax, dep +(dep), cog (ap-hype cog i.pax))
      ?^  spu  spu
      ?.((ap-fond cog) ~ `[dep cog])
    ::
    ++  ap-fond                                         ::  check for arm
      |=  cog/term
      ^-  ?
      (slob cog p.hav)
    ::
    ++  ap-give                                         ::  return result
      |=  cit/cuft
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
      |=({a/term b/term} `term`(cat 3 a (cat 3 '-' b)))
    ::
    ++  ap-move                                         ::  process each move
      ~/  %ap-move
      |=  vax/vase
      ^-  {(each cove tang) _+>}
      ?@  q.vax    :_(+>.$ [%| (ap-suck "move: invalid move (atom)")])
      ?^  -.q.vax  :_(+>.$ [%| (ap-suck "move: invalid move (bone)")])
      ?@  +.q.vax  :_(+>.$ [%| (ap-suck "move: invalid move (card)")])
      =+  hun=(~(get by r.zam) -.q.vax)
      ?.  &((~(has by r.zam) -.q.vax) !=(0 -.q.vax))
        :_(+>.$ [%| (ap-suck "move: invalid card (bone {<-.q.vax>})")])
      =^  pec  vel  (~(spot wa vel) 3 vax)
      =^  cav  vel  (~(slot wa vel) 3 pec)
      ?+  +<.q.vax  
               (ap-move-pass -.q.vax +<.q.vax cav)
        $diff  (ap-move-diff -.q.vax cav)
        $hiss  (ap-move-hiss -.q.vax cav)
        $peel  (ap-move-peel -.q.vax cav)
        $peer  (ap-move-peer -.q.vax cav)
        $pull  (ap-move-pull -.q.vax cav)
        $poke  (ap-move-poke -.q.vax cav)
        $send  (ap-move-send -.q.vax cav)
        $quit  (ap-move-quit -.q.vax cav)
      ==
    ::
    ++  ap-move-quit                                    ::  give quit move
      |=  {sto/bone vax/vase}
      ^-  {(each cove tang) _+>}
      :_  +>(sup.ged (~(del by sup.ged) sto))
      ?^  q.vax  [%| (ap-suck "quit: improper give")]
      [%& `cove`[sto %give `cuft`[%quit ~]]]
    ::
    ++  ap-move-diff                                    ::  give diff move
      |=  {sto/bone vax/vase}
      ^-  {(each cove tang) _+>}
      =^  pec  vel  (~(spec wa vel) vax)
      ?.  &(?=(^ q.pec) ?=(@ -.q.pec) ((sane %tas) -.q.pec))
        :_(+>.$ [%| (ap-suck "diff: improper give")])
      =^  tel  vel  (~(slot wa vel) 3 pec)
      :_(+>.$ [%& sto %give %diff `cage`[-.q.pec tel]])
    ::
    ++  ap-move-hiss                                    ::  pass %hiss
      |=  {sto/bone vax/vase}
      ^-  {(each cove tang) _+>}
      ?.  &(?=({p/* q/* r/@ s/{p/@ *}} q.vax) ((sane %tas) r.q.vax))
        =+  args="[%hiss wire (unit knot) mark cage]"
        :_(+>.$ [%| (ap-suck "hiss: bad hiss ask.{args}")])
      =^  gaw  vel  (~(slot wa vel) 15 vax)
      ?.  &(?=({p/@ *} q.gaw) ((sane %tas) p.q.gaw))
        :_(+>.$ [%| (ap-suck "hiss: malformed cage")])
      =^  paw  vel  (~(stop wa vel) 3 gaw)
      =+  usr=((soft (unit knot)) q.q.vax)
      ?.  &(?=(^ usr) ?~(u.usr & ((sane %ta) u.u.usr)))
        :_(+>.$ [%| (ap-suck "hiss: malformed (unit knot)")])
      =+  pux=((soft path) p.q.vax)
      ?.  &(?=(^ pux) (levy u.pux (sane %ta)))
        :_(+>.$ [%| (ap-suck "hiss: malformed path")])
      :_  +>.$
      :^  %&  sto  %pass
      :-  [(scot %p q.q.pry) %cay u.pux]
      ~!  *cote
      =-  ~!  -  `cote`-
      [%hiss u.usr r.q.vax [p.q.gaw paw]]
    ::
    ++  ap-move-mess                                    ::  extract path, target
      |=  vax/vase
      ^-  {(each (trel path ship term) tang) _+>}
      :_  +>.$
      ?.  ?&  ?=({p/* {q/@ r/@} s/*} q.vax)
              (gte 1 (met 7 q.q.vax))
          ==
        [%| (ap-suck "mess: malformed target")]
      =+  pux=((soft path) p.q.vax)
      ?.  &(?=(^ pux) (levy u.pux (sane %ta)))
        [%| (ap-suck "mess: malformed path")]
      [%& [(scot %p q.q.vax) %out r.q.vax u.pux] q.q.vax r.q.vax]
    ::
    ++  ap-move-pass                                    ::  pass general move 
      |=  {sto/bone wut/* vax/vase}
      ^-  {(each cove tang) _+>}
      ?.  &(?=(@ wut) ((sane %tas) wut))
        :_(+>.$ [%| (ap-suck "pass: malformed card")])
      =+  pux=((soft path) -.q.vax)
      ?.  &(?=(^ pux) (levy u.pux (sane %ta)))
        :_(+>.$ [%| (ap-suck "pass: malformed path")])
      =+  huj=(ap-vain wut)
      ?~  huj  :_(+>.$ [%| (ap-suck "move: unknown note {(trip wut)}")])
      =^  tel  vel  (~(slot wa vel) 3 vax)
      :_  +>.$
      :^  %&  sto  %pass
      :-  [(scot %p q.q.pry) %inn u.pux]
      [%meta u.huj (slop (ap-term %tas wut) tel)]
    ::
    ++  ap-move-poke                                    ::  pass %poke
      |=  {sto/bone vax/vase}
      ^-  {(each cove tang) _+>}
      =^  yep  +>.$  (ap-move-mess vax)
      ?:  ?=($| -.yep)  :_(+>.$ yep)
      =^  gaw  vel  (~(slot wa vel) 7 vax)
      ?.  &(?=({p/@ q/*} q.gaw) ((sane %tas) p.q.gaw))
        :_(+>.$ [%| (ap-suck "poke: malformed cage")])
      =^  paw  vel  (~(stop wa vel) 3 gaw)
      :_  +>.$
      :^  %&  sto  %pass
      :-  p.p.yep
      [%send q.p.yep r.p.yep %poke p.q.gaw paw]
    ::
    ++  ap-move-peel                                    ::  pass %peel
      |=  {sto/bone vax/vase}
      ^-  {(each cove tang) _+>}
      =^  yep  +>.$  (ap-move-mess vax)
      :_  +>.$
      ?:  ?=($| -.yep)  yep
      =+  mar=((soft mark) +>-.q.vax)
      ?~  mar
        [%| (ap-suck "peel: malformed mark")]
      =+  pux=((soft path) +>+.q.vax)
      ?.  &(?=(^ pux) (levy u.pux (sane %ta)))
        [%| (ap-suck "peel: malformed path")]
      ?:  (~(has in misvale) p.p.yep)
        =/  err  [leaf+"peel: misvalidation encountered"]~
        :^  %&  sto  %pass
        :-  p.p.yep
        [%send q.p.yep r.p.yep %peer-not err]
      :^  %&  sto  %pass
      :-  p.p.yep
      [%send q.p.yep r.p.yep %peel u.mar u.pux]
    ::
    ++  ap-move-peer                                    ::  pass %peer
      |=  {sto/bone vax/vase}
      ^-  {(each cove tang) _+>}
      =^  yep  +>.$  (ap-move-mess vax)
      :_  +>.$
      ?:  ?=($| -.yep)  yep
      =+  pux=((soft path) +>.q.vax)
      ?.  &(?=(^ pux) (levy u.pux (sane %ta)))
        [%| (ap-suck "peer: malformed path")]
      ?:  (~(has in misvale) p.p.yep)
        =/  err  [leaf+"peer: misvalidation encountered"]~
        :^  %&  sto  %pass
        :-  p.p.yep
        [%send q.p.yep r.p.yep %peer-not err]
      :^  %&  sto  %pass
      :-  p.p.yep
      [%send q.p.yep r.p.yep %peer u.pux]
    ::
    ++  ap-move-pull                                    ::  pass %pull
      |=  {sto/bone vax/vase}
      ^-  {(each cove tang) _+>}
      =^  yep  +>.$  (ap-move-mess vax)
      :_  +>.$
      ?:  ?=($| -.yep)  yep
      ?.  =(~ +>.q.vax)
        [%| (ap-suck "pull: malformed card")]
      :^  %&  sto  %pass
      :-  p.p.yep
      [%send q.p.yep r.p.yep %pull ~]
    ::
    ++  ap-move-send                                    ::  pass gall action
      |=  {sto/bone vax/vase}
      ^-  {(each cove tang) _+>}
      ?.  ?&  ?=({p/* {q/@ r/@} {s/@ t/*}} q.vax)
              (gte 1 (met 7 q.q.vax))
              ((sane %tas) r.q.vax)
          ==
        :_(+>.$ [%| (ap-suck "send: improper ask.[%send wire gill club]")])
      =+  pux=((soft path) p.q.vax)
      ?.  &(?=(^ pux) (levy u.pux (sane %ta)))
        :_(+>.$ [%| (ap-suck "send: malformed path")])
      ?:  ?=($poke s.q.vax)
        =^  gav  vel  (~(spot wa vel) 7 vax)
        ?>  =(%poke -.q.gav)
        ?.  ?&  ?=({p/@ q/*} t.q.vax)
                ((sane %tas) p.t.q.vax)
            ==
          :_(+>.$ [%| (ap-suck "send: malformed poke")])
        =^  vig  vel  (~(spot wa vel) 3 gav)
        =^  geb  vel  (~(slot wa vel) 3 vig)
        :_  +>.$
        :^  %&  sto  %pass
        :-  [(scot %p q.q.vax) %out r.q.vax u.pux]
        ^-  cote
        ::  ~&  [%ap-move-send `path`[(scot %p q.q.vax) %out r.q.vax u.pux]]
        [%send q.q.vax r.q.vax %poke p.t.q.vax geb]
      :_  +>.$
      =+  cob=((soft club) [s t]:q.vax)
      ?~  cob
        [%| (ap-suck "send: malformed club")]
      :^  %&  sto  %pass
      :-  [(scot %p q.q.vax) %out r.q.vax u.pux]
      ::  ~&  [%ap-move-send `path`[(scot %p q.q.vax) %out r.q.vax u.pux]]
      [%send q.q.vax r.q.vax u.cob]
    ::
    ++  ap-pass                                         ::  request action
      |=  {pax/path coh/cote}
      ^+  +>
      +>(zip :_(zip [ost %pass pax coh]))
    ::
    ++  ap-peep                                         ::  reinstall
      |=  vax/vase
      ^+  +>
      =+  pep=(ap-prep(hav vax) `hav)
      ?~  -.pep
        +.pep
      (ap-lame %prep-failed u.-.pep)
    ::
    ++  ap-peel
      |=  {mar/mark pax/path}
      =.  pyl  (~(put by pyl) ost mar)
      (ap-peer pax)
    ::
    ++  ap-peer                                         ::  apply %peer
      |=  pax/path
      ^+  +>
      =.  sup.ged  (~(put by sup.ged) ost [q.q.pry pax])
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
    ++  ap-poke                                         ::  apply %poke
      |=  cag/cage
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
      |=  {wut/@tas why/tang}
      ^+  +>
      =+  cug=(ap-find /lame)
      ?~  cug
        =.  why  [>%ap-lame dap wut< (turn why |=(a/tank rose+[~ "! " ~]^[a]~))]
        ~>  %slog.`rose+["  " "[" "]"]^(flop why)
        +>.$
      =^  cam  +>.$
        %+  ap-call  q.u.cug
        !>([wut why])
      ?^  cam
        =.  why  [>%ap-lame-lame< (turn u.cam |=(a/tank rose+[~ "! " ~]^[a]~))]
        ~>  %slog.`rose+["  " "[" "]"]^(welp (flop why) leaf+"." (flop u.cam))
        +>.$
      +>.$
    ::
    ++  ap-misvale                                      ::  broken vale
      |=  wir/wire
      ~&  [%ap-blocking-misvale wir]
      +>(misvale (~(put in misvale) wir))
    ::
    ++  ap-pour                                         ::  generic take
      |=  {pax/path vax/vase}
      ^+  +>
      ?.  &(?=({@ *} q.vax) ((sane %tas) -.q.vax))
        (ap-lame %pour (ap-suck "pour: malformed card"))
      =+  cug=(ap-find [-.q.vax pax])
      ?~  cug
        ?:  =(-.q.vax %went)
          +>.$
        (ap-lame -.q.vax (ap-suck "pour: no {(trip -.q.vax)}: {<pax>}"))
      =^  tel  vel  (~(slot wa vel) 3 vax)
      =^  cam  +>.$
          %+  ap-call  q.u.cug
          %+  slop
            !>(`path`(slag p.u.cug pax))
          tel
      ?^  cam  (ap-lame -.q.vax u.cam)
      +>.$
    ::
    ++  ap-purr                                         ::  unwrap take
      |=  {wha/term pax/path cag/cage}
      ^+  +>
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
      |=  {pax/path cuf/cuft}
      ^+  +>
      ?-  -.cuf
        $coup  (ap-take q.q.pry %coup +.pax `!>(p.cuf))
        $diff  (ap-diff q.q.pry pax p.cuf)
        $quit  (ap-take q.q.pry %quit +.pax ~)
        $reap  (ap-take q.q.pry %reap +.pax `!>(p.cuf))
      ==
    ::
    ++  ap-prep                                         ::  install
      |=  vux/(unit vase)
      ^-  {(unit tang) _+>}
      =^  gac  +>.$  (ap-prop vux)
      :-  gac
      %=    +>.$
          misvale
        ~?  !=(misvale *misvale-data)  misvale-drop+misvale
        *misvale-data                 ::  new app might mean new marks
      ::
          dub
        :_(dub ?~(gac [%& dap ?~(vux %boot %bump) now] [%| u.gac]))
      ==
    ::
    ++  ap-prop                                         ::  install
      |=  vux/(unit vase)
      ^-  {(unit tang) _+>}
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
      ?~  tur 
        `+>.$ 
      :_(+>.$ `u.tur)
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
      ?~  wim  +  ::  ~&(%ap-pull-none +)
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
      ::  ~&  [%ap-kill dap ost]
      (ap-give:ap-pull %quit ~)
    ::
    ++  ap-take                                         ::  non-diff gall take
      |=  {her/ship cog/term pax/path vux/(unit vase)}
      ^+  +>
      =+  cug=(ap-find cog pax)
      ?~  cug
        ::  ~&  [%ap-take-none cog pax]
        +>.$
      =^  cam  +>.$  
        %+  ap-call  q.u.cug
        =+  den=!>((slag p.u.cug pax))
        ?~(vux den (slop den u.vux))
      ?^  cam  (ap-lame q.u.cug u.cam)
      +>.$
    ::
    ++  ap-safe                                         ::  process move list
      |=  vax/vase
      ^-  {(each (list cove) tang) _+>}
      ?~  q.vax  :_(+>.$ [%& ~])
      ?@  q.vax  :_(+>.$ [%| (ap-suck "move: malformed list")])
      =^  hed  vel  (~(slot wa vel) 2 vax)
      =^  sud  +>.$  (ap-move hed)
      ?:  ?=($| -.sud)  :_(+>.$ sud)
      =^  tel  vel  (~(slot wa vel) 3 vax)
      =^  res  +>.$  $(vax tel)
      :_  +>.$
      ?:  ?=($| -.res)  res
      [%& p.sud p.res]
    ::
    ++  ap-sake                                         ::  handle result
      |=  vax/vase
      ^-  {(unit tang) _+>}
      ?:  ?=(@ q.vax)
        [`(ap-suck "sake: invalid product (atom)") +>.$]
      =^  hed  vel  (~(slot wa vel) 2 vax)
      =^  muz  +>.$  (ap-safe hed)
      ?:  ?=($| -.muz)  [`p.muz +>.$]
      =^  tel  vel  (~(slot wa vel) 3 vax)
      =^  sav  +>.$  (ap-save tel)
      ?:  ?=($| -.sav)  [`p.sav +>.$]
      :-  ~
      %_  +>.$
        zip  (weld (flop p.muz) zip)
        hav  p.sav 
      ==
    ::
    ++  ap-save                                         ::  verify core
      |=  vax/vase
      ^-  {(each vase tang) _+>}
      =^  gud  vel  (~(nest wa vel) p.hav p.vax)
      :_  +>.$
      ?.  gud
        [%| (ap-suck "invalid core")]
      [%& vax]
    ::
    ++  ap-slam                                         ::  virtual slam
      ~/  %ap-slam
      |=  {cog/term gat/vase arg/vase}
      ^-  {(each vase tang) _+>}
      =+  ^=  wyz  %-  mule  |.
          (~(mint wa vel) [%cell p.gat p.arg] [%cnsg [%$ ~] [%$ 2] [%$ 3] ~])
      ?:  ?=($| -.wyz)
        %-  =+  sam=(~(peek ut p.gat) %free 6)
            (slog >%ap-slam-mismatch< ~(duck ut p.arg) ~(duck ut sam) ~)
        :_(+>.$ [%| (ap-suck "call: {<cog>}: type mismatch")])
      :_  +>.$(vel +>.wyz)
      =+  [typ nok]=+<.wyz
      =+  ton=(mock [[q.gat q.arg] nok] ap-sled)
      ?-  -.ton
        $0  [%& typ p.ton]
        $1  [%| (turn p.ton |=(a/* (smyt (path a))))]
        $2  [%| p.ton]
      ==
    ::
    ++  ap-sled  (sloy ska)                             ::  namespace view
    ++  ap-suck                                         ::  standard tang
      |=  msg/tape
      ^-  tang
      [%leaf (weld "gall: {<dap>}: " msg)]~
    ::
    ++  ap-term                                         ::  atomic vase
      |=  {a/@tas b/@}  
      ^-  vase
      [[%atom a `b] b]
    ::
    ++  ap-vain                                         ::  card to vane
      |=  sep/@tas
      ^-  (unit @tas)
      ?+  sep  ~&  [%ap-vain sep]
               ~
        $cash  `%a
        $conf  `%g
        $cred  `%c
        $crew  `%c
        $crow  `%c
        $deal  `%g
        $exec  `%f
        $flog  `%d
        $drop  `%c
        $info  `%c
        $merg  `%c
        $mont  `%c
        $nuke  `%a
        $ogre  `%c
        $perm  `%c
        $serv  `%e
        $them  `%e
        $wait  `%b
        $want  `%a
        $warp  `%c
        $wipe  `%f                                      ::  XX cache clear
      ==
    --
  --
++  call                                                ::  request
  ~%  %gall-call  +>   ~
  |=  {hen/duct hic/(hypo (hobo task:able))}
  ^+  [p=*(list move) q=..^$]
  =>  .(q.hic ?.(?=($soft -.q.hic) q.hic ((hard task:able) p.q.hic)))
  ?-    -.q.hic
      $conf
    ?.  (~(has by pol.all) p.p.q.hic)
      ~&  [%gall-not-ours p.p.q.hic]
      [~ ..^$]
    mo-abet:(mo-conf:(mo-abed:mo p.p.q.hic hen) q.p.q.hic q.q.hic)
  ::
      $deal
    =<  mo-abet
    ?.  (~(has by pol.all) q.p.q.hic)                   ::  either to us
      ?>  (~(has by pol.all) p.p.q.hic)                 ::  or from us
      (mo-away:(mo-abed:mo p.p.q.hic hen) q.p.q.hic q.q.hic)
    (mo-come:(mo-abed:mo q.p.q.hic hen) p.p.q.hic q.q.hic)
  ::
      $init 
    ::  ~&  [%gall-init p.q.hic]
    [~ ..^$(pol.all (~(put by pol.all) p.q.hic %*(. *mast sys hen)))]
  ::
      $west
    ?.  (~(has by pol.all) p.p.q.hic)
      ~&  [%gall-not-ours p.q.hic]
      [~ ..^$]
    ?>  ?=({?($ge $gh) @ $~} q.q.hic)
    =+  dap=i.t.q.q.hic
    =+  our=p.p.q.hic
    =+  him=q.p.q.hic
    ?:  ?=($ge i.q.q.hic)
      =+  mes=((hard {@ud rook}) r.q.hic)
      =<  mo-abet
      (mo-gawk:(mo-abed:mo our hen) him dap mes)
    =+  mes=((hard {@ud roon}) r.q.hic)
    =<  mo-abet
    (mo-gawd:(mo-abed:mo our hen) him dap mes)
  ::
      $wegh
    :_  ..^$  :_  ~
    :^  hen  %give  %mass
    :-  %gall
    :-  %|
    %+  turn  ~(tap by pol.all)     :: XX single-home
    |=  {our/@ mast}  ^-  mass
    :+  (scot %p our)  %|
    :~  [%foreign [%& sap]]
        [%blocked [%| (sort ~(tap by (~(run by wub) |=(sofa [%& +<]))) aor)]]
        [%active [%| (sort ~(tap by (~(run by bum) |=(seat [%& +<]))) aor)]]
    ==
  ::  
      $went  !!   ::  XX fixme
  ==
::
++  doze                                                ::  sleep until
  |=  {now/@da hen/duct}
  ^-  (unit @da)
  ~
::
++  load                                                ::  recreate vane
  |=  old/axle-n
  ^+  ..^$
  ?-  -.old
      $0  ..^$(all old)
  ==
::
++  scry
  |=  {fur/(unit (set monk)) ren/@tas why/shop syd/desk lot/coin tyl/path}
  ^-  (unit (unit cage))
  ?.  ?=($& -.why)  ~
  =*  his  p.why
  ?:  ?&  =(%u ren)
          =(~ tyl)
          =([%$ %da now] lot)
          (~(has by pol.all) his)
          (~(has by bum:(~(got by pol.all) his)) syd)
      ==
    ``[%null !>(~)]
  ?.  (~(has by pol.all) his)
    ~
  ?.  =([%$ %da now] lot)
    ~
  ?.  (~(has by bum:(~(got by pol.all) his)) syd)
    [~ ~]
  ?.  ?=(^ tyl)
    ~
  (mo-peek:(mo-abed:mo his *duct) syd high+`his ren tyl)
::
++  stay                                                ::  save w+o cache
  `axle`all
::
++  take                                                ::  response
  |=  {tea/wire hen/duct hin/(hypo sign-arvo)}
  ^+  [p=*(list move) q=..^$]
  ~|  [%gall-take tea]
  ?>  ?=({@ ?($sys $use) *} tea) 
  =+  our=(need (slaw %p i.tea))
  =+  mow=(mo-abed:mo our hen)
  ?-  i.t.tea
    $sys  mo-abet:(mo-cyst:mow t.t.tea q.hin)
    $use  mo-abet:(mo-cook:mow t.t.tea hin)
  ==
--
