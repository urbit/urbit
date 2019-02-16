!:  ::  %gall, agent execution
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
      {$x ~}                                           ::
  ==                                                    ::
++  rook                                                ::  forward ames msg
  $%  {$m p/mark q/*}                                   ::  message
      {$l p/mark q/path}                                ::  "peel" subscribe
      {$s p/path}                                       ::  subscribe
      {$u ~}                                           ::  cancel+unsubscribe
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
      =mast                                             ::  apps by ship
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
      arms=(map [term path] (unit (pair @ud term)))     ::  ap-find cache
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
|=  $:  our=ship                                        ::  identity
        now=@da                                         ::  urban time
        eny=@uvJ                                        ::  entropy
        ska=sley                                        ::  activate
    ==                                                  ::  opaque core
~%  %gall-top  ..is  ~
|%  ::::::::::::::::::::::::::::::::::::::::::::::::::::::  state machine
    ::::::::::::::::::::::::::::::::::::::::::::::::::::::
++  mo
  ~%  %gall-mo  +>  ~
  =*  mas  mast.all
  |_  $:  hen=duct
          moz=(list move)
      ==
  ++  mo-abed                                           ::  initialize
    |=  =duct
    ^+  +>
    +>(hen duct)
  ::
  ++  mo-abet                                           ::  resolve to
    ^+  [*(list move) +>+]
    :_  +>+
    %-  flop
    %+  turn  moz
    |=  a/move
    ?.  ?=($pass -.q.a)  a
    [p.a %pass p.q.a q.q.a]
  ::
  ++  mo-conf                                           ::  configure
    |=  {dap/dude lum/culm}
    (mo-boot dap p.p.lum q.p.lum da+now)
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
    ~/  %mo-okay
    |=  vax/vase
    ^-  ?
    =+  bol=(slew 12 vax)
    ?~  bol  |
    (~(nest ut p.u.bol) %| -:!>(*bowl))
  ::  +mo-receive-core: receives an app core built by ford-turbo
  ::
  ++  mo-receive-core
    ~/  %mo-receive-core
    |=  [dap=dude byk=beak made-result=made-result:ford]
    ^+  +>
    ::
    ?:  ?=([%incomplete *] made-result)
      (mo-give %onto %| tang.made-result)
    ::
    =/  build-result  build-result.made-result
    ::
    ?:  ?=([%error *] build-result)
      (mo-give %onto %| message.build-result)
    ::
    =/  result-cage=cage  (result-to-cage:ford build-result)
    ::
    =/  app-data=(unit seat)  (~(get by bum.mas) dap)
    ?^  app-data
      ::  update the path
      ::
      =.  bum.mas  (~(put by bum.mas) dap u.app-data(byk byk))
      ::  magic update string from +mo-boon, "complete old boot"
      ::
      ap-abet:(ap-peep:(ap-abed:ap dap [%high [~ our]]) q.result-cage)
    ::  first install of the app
    ::
    ?.  (mo-okay q.result-cage)
      (mo-give %onto %| [%leaf "{<dap>}: bogus core"]~)
    =.  +>.$  (mo-born dap byk q.result-cage)
    =+  old=+>.$
    =+  wag=(ap-prop:(ap-abed:ap dap [%high [~ our]]) ~)
    ?^  -.wag
      =.  +>.$  old
      (mo-give %onto %| u.-.wag)
    =.  +>.$  ap-abet:+.wag
    (mo-give:(mo-claw dap) %onto %& dap %boot now)
  ::
  ++  mo-born                                           ::  new seat
    |=  {dap/dude byk/beak hav/vase}
    =+  sat=*seat
    %_    +>.$
        bum.mas
      %+  ~(put by bum.mas)  dap
      %_  sat
        mom  hen
        byk  byk
        hav  hav
        p.zam  1
        q.zam  [[[~ ~] 0] ~ ~]
        r.zam  [[0 [~ ~]] ~ ~]
      ==
    ==
  ::  +mo-boot: sends an %exec to ford.
  ::
  ++  mo-boot                                           ::  create ship
    |=  {dap/dude byk/beak}
    ^+  +>
    %+  mo-pass  [%sys %core dap (scot %p p.byk) q.byk (scot r.byk) ~]
    ^-  note-arvo
    [%f %build live=%.y [%core [[p q]:byk [%hoon dap %app ~]]]]
  ::
  ++  mo-away                                           ::  foreign request
    ~/  %mo-away
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
    `note-arvo`[%a %want him [%g %ge p.caz ~] [num roc]]
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
    =+  sad=(fall (~(get by sap.mas) him) `scad`[1 ~ ~])
    =+  nom=(~(get by q.sad) hen)
    ?^  nom  [u.nom +>.$]
    :-  p.sad
    %_    +>.$
        sap.mas
      %+  ~(put by sap.mas)  him
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
    (~(got by r:(~(got by sap.mas) him)) num)
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
    ?>  ?=({@ @ @ ~} pax)
    [(slav %p i.pax) i.t.pax da+(slav %da i.t.t.pax)]
  ::
  ++  mo-cyst                                           ::  take in /sys
    ~/  %mo-cyst
    |=  {pax/path sih/sign-arvo}
    ^+  +>
    ?+    -.pax  !!
        $core
      ?>  ?=([%f %made *] sih)
      ?>  ?=({@ @ @ @ ~} t.pax)
      (mo-receive-core i.t.pax (mo-chew t.t.pax) result.sih)
    ::
        %pel                                            ::  translated peer
      ?>  ?=({@ ~} t.pax)
      =+  mar=i.t.pax
      ?>  ?=([%f %made *] sih)
      ::
      ?:  ?=([%incomplete *] result.sih)
        (mo-give %unto %coup `tang.result.sih)
      ::
      =/  build-result  build-result.result.sih
      ::
      ?:  ?=([%error *] build-result)
        (mo-give %unto %coup `message.build-result)
      ::
      (mo-give %unto %diff (result-to-cage:ford build-result))
    ::
        $red                                            ::  diff ack
      ?>  ?=({@ @ @ ~} t.pax)
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
      :: should not happen (XX wat mean?)
      ::
      %-  ?.  ?=([~ ~ %mack *] q.+>.sih)
            ~&  [%diff-bad-ack q.+>.sih]
            same
          ~&  [%diff-bad-ack %mack]
          (slog (flop q.,.+>.q.+>.sih))
      =.  +>.$  (mo-pass [%sys pax] %g %deal [him our] dap %pull ~)
      (mo-pass [%sys pax] %a %want him [%g %gh dap ~] [num %x ~])
    ::
        %rep                                            ::  reverse request
      ?>  ?=({@ @ @ ~} t.pax)
      ?>  ?=([%f %made *] sih)
      =+  :*  him=(slav %p i.t.pax)
              dap=i.t.t.pax
              num=(slav %ud i.t.t.t.pax)
          ==
      ::
      ?:  ?=([%incomplete *] result.sih)
        ::  "XX should crash"
        (mo-give %mack `tang.result.sih)
      ::
      =/  build-result  build-result.result.sih
      ::
      ?:  ?=([%error *] build-result)
        ::  "XX should crash"
        (mo-give %mack `message.build-result)
      ::
      ::  "XX pump should ack"
      =.  +>.$  (mo-give %mack ~)
      =*  result-cage  (result-to-cage:ford build-result)
      (mo-give(hen (mo-ball him num)) %unto %diff result-cage)
    ::
        $req                                            ::  inbound request
      ?>  ?=({@ @ @ ~} t.pax)
      =+  :*  him=(slav %p i.t.pax)
              dap=i.t.t.pax
              num=(slav %ud i.t.t.t.pax)
          ==
      ?:  ?=({$f $made *} sih)
        ?:  ?=([%incomplete *] result.sih)
          ::  "XX should crash"
          (mo-give %mack `tang.result.sih)
        ::
        =/  build-result  build-result.result.sih
        ::
        ?:  ?=([%error *] build-result)
          ::  "XX should crash"
          (mo-give %mack `message.build-result)
        =/  cay/cage  (result-to-cage:ford build-result)
        (mo-pass [%sys pax] %g %deal [him our] i.t.t.pax %poke cay)
      ?:  ?=({$a $woot *} sih)  +>.$                    ::  quit ack, boring
      ?>  ?=({$g $unto *} sih)
      =+  cuf=`cuft`+>.sih
      ?-    -.cuf
        $coup  (mo-give %mack p.cuf)
        $diff  %+  mo-pass  [%sys %red t.pax]
               [%a %want him [%g %gh dap ~] [num %d p.p.cuf q.q.p.cuf]]
        $quit  %+  mo-pass  [%sys pax]
               [%a %want him [%g %gh dap ~] [num %x ~]]
        $reap  (mo-give %mack p.cuf)
      ==
    ::
        %val                                            ::  inbound validate
      ?>  ?=({@ @ ~} t.pax)
      =+  [him=(slav %p i.t.pax) dap=i.t.t.pax]
      ?>  ?=([%f %made *] sih)
      ::
      ?:  ?=([%incomplete *] result.sih)
        (mo-give %unto %coup `tang.result.sih)
      ::
      =/  build-result  build-result.result.sih
      ::
      ?:  ?=([%error *] build-result)
        (mo-give %unto %coup `message.build-result)
      ::
      =*  result-cage  (result-to-cage:ford build-result)
      (mo-clip dap `prey`[%high ~ him] [%poke result-cage])
    ::
        $way                                            ::  outbound request
      ?>  ?=({$a $woot *} sih)
      ?>  ?=({@ ~} t.pax)
      %-  mo-awed
      :*  `ship`p.+>.sih
          ;;(?($peer $peel $poke $pull) i.t.pax)
          +>+.sih
      ==
    ==
  ::
  ++  mo-cook                                           ::  take in /use
    ~/  %mo-cook
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
    ?.  (~(has by bum.mas) dap)  +>
    =+  suf=(~(get by wub.mas) dap)
    ?~  suf  +>.$
    |-  ^+  +>.^$
    ?:  =(~ kys.u.suf)
      +>.^$(wub.mas (~(del by wub.mas) dap))
    =^  lep  kys.u.suf  [p q]:~(get to kys.u.suf)
    $(moz :_(moz [p.lep %slip %g %deal [q.q.q.lep our] dap r.lep]))
    ::  $(+>.^$ (mo-clip(hen p.lep) dap q.lep r.lep))
  ::
  ++  mo-beak                                           ::  build beak
    |=  dap/dude
    =-  ?.(=(p our) - -(r [%da now])) ::  soft dependencies
    ^-  beak
    byk:(~(got by bum.mas) dap)
  ::
  ++  mo-peek
    ~/  %mo-peek
    |=  {dap/dude pry/prey ren/@tas tyl/path}
    ^-  (unit (unit cage))
    (ap-peek:(ap-abed:ap dap pry) ren tyl)
  ::
  ++  mo-clip                                           ::  apply club
    |=  {dap/dude pry/prey cub/club}
    ?:  ?=($puff -.cub)
      %+  mo-pass
        [%sys %val (scot %p q.q.pry) dap ~]
      [%f %build live=%.n [%vale [p q]:(mo-beak dap) +.cub]]
    ?:  ?=($punk -.cub)
      %+  mo-pass
        [%sys %val (scot %p q.q.pry) dap ~]
      :*  %f  %build  live=%.n
          ^-  schematic:ford
          [%cast [p q]:(mo-beak dap) p.cub [%$ q.cub]]
      ==
    ?:  ?=($peer-not -.cub)
      (mo-give %unto %reap (some p.cub))
    ap-abet:(ap-club:(ap-abed:ap dap pry) cub)
  ::
  ++  mo-club                                           ::  local action
    |=  {dap/dude pry/prey cub/club}
    ^+  +>
    ?:  |(!(~(has by bum.mas) dap) (~(has by wub.mas) dap))
      ~&  >>  [%mo-not-running dap -.cub]
      ::  ~&  [%mo-club-qeu dap cub]
      =+  syf=(fall (~(get by wub.mas) dap) *sofa)
      +>.$(wub.mas (~(put by wub.mas) dap syf(kys (~(put to kys.syf) [hen pry cub]))))
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
      [%f %build live=%.n [%vale [p q]:(mo-beak dap) p.ron q.ron]]
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
      ~/  %ap-abed
      |=  {dap/dude pry/prey}
      ^+  +>
      =:  ^dap   dap
          ^pry   pry
          +>+<+  `seat`(~(got by bum.mas) dap)
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
        bum.mas  (~(put by bum.mas) dap +<+)
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
      ~/  %ap-aver
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
        [%f %build live=%.n [%cast [p q]:(mo-beak dap) mar [%$ cay]]]
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
      ?:  ?=(%| -.arm)  [`p.arm +>.$]
      =^  zem  +>.$  (ap-slam cog p.arm arg)
      ?:  ?=(%| -.zem)  [`p.zem +>.$]
      (ap-sake p.zem)
    ::
    ++  ap-peek
      ~/  %ap-peek
      |=  {ren/@tas tyl/path}
      ^-  (unit (unit cage))
      =+  ?.  ?=($x ren)
            [mar=%$ tyl=tyl]
          =+  `path`(flop tyl)
          ?>  ?=(^ -)
          [mar=i tyl=(flop t)]
      =^  cug  +>.$  (ap-find %peek ren tyl)
      ?~  cug
        ((slog leaf+"peek find fail" >tyl< >mar< ~) [~ ~])
      =^  arm  +>.$  (ap-farm q.u.cug)
      ?:  ?=(%| -.arm)  ((slog leaf+"peek farm fail" p.arm) [~ ~])
      =^  zem  +>.$  (ap-slam q.u.cug p.arm !>((slag p.u.cug `path`[ren tyl])))
      ?:  ?=(%| -.zem)  ((slog leaf+"peek slam fail" p.zem) [~ ~])
      ?+  q.p.zem  ((slog leaf+"peek bad result" ~) [~ ~])
        ~              ~
        {~ ~}         [~ ~]
        {~ ~ ^}
          =+  caz=(sped (slot 7 p.zem))
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
      ~/  %ap-diff
      |=  {her/ship pax/path cag/cage}
      ::  =.  q.cag  (sped q.cag)
      =^  cug  +>.$  (ap-find [%diff p.cag +.pax])
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
      ~/  %ap-pump
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
      ?:  |(?=(~ soy) =(0 u.soy))
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
      ?:  ?=(%| -.pyz)
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
        [%| +]
      ::  ~?  !=(20 suy)  [%ap-fill-add [[our dap] q.q.pry ost] +(suy)]
      [%& +(qel.ged (~(put by qel.ged) ost +(suy)))]
    ::
    ++  ap-find                                         ::  general arm
      ~/  %ap-find
      |=  {cog/term pax/path}
      ^-  [(unit (pair @ud term)) _+>]
      ::  check cache
      ?^  maybe-result=(~(get by arms) [cog pax])
        [u.maybe-result +>.$]
      ::
      =/  result=(unit (pair @ud term))
        =+  dep=0
        |-  ^-  (unit (pair @ud term))
        =+  ^=  spu
            ?~  pax  ~
            $(pax t.pax, dep +(dep), cog (ap-hype cog i.pax))
        ?^  spu  spu
        ?.((ap-fond cog) ~ `[dep cog])
      ::
      =.  arms  (~(put by arms) [cog pax] result)
      [result +>.$]
    ::
    ++  ap-fond                                         ::  check for arm
      ~/  %ap-fond
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
      ~/  %ap-hype
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
        ~&  [q-vax+q.vax has-by-r-zam+(~(has by r.zam) -.q.vax)]
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
      ~/  %quit
      |=  {sto/bone vax/vase}
      ^-  {(each cove tang) _+>}
      :_  +>(sup.ged (~(del by sup.ged) sto))
      ?^  q.vax  [%| (ap-suck "quit: improper give")]
      [%& `cove`[sto %give `cuft`[%quit ~]]]
    ::
    ++  ap-move-diff                                    ::  give diff move
      ~/  %diff
      |=  {sto/bone vax/vase}
      ^-  {(each cove tang) _+>}
      =^  pec  vel  (~(sped wa vel) vax)
      ?.  &(?=(^ q.pec) ?=(@ -.q.pec) ((sane %tas) -.q.pec))
        :_(+>.$ [%| (ap-suck "diff: improper give")])
      =^  tel  vel  (~(slot wa vel) 3 pec)
      :_(+>.$ [%& sto %give %diff `cage`[-.q.pec tel]])
    ::
    ++  ap-move-hiss                                    ::  pass %hiss
      ~/  %hiss
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
      ~/  %mess
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
      ~/  %pass
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
      ~/  %poke
      |=  {sto/bone vax/vase}
      ^-  {(each cove tang) _+>}
      =^  yep  +>.$  (ap-move-mess vax)
      ?:  ?=(%| -.yep)  :_(+>.$ yep)
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
      ~/  %peel
      |=  {sto/bone vax/vase}
      ^-  {(each cove tang) _+>}
      =^  yep  +>.$  (ap-move-mess vax)
      :_  +>.$
      ?:  ?=(%| -.yep)  yep
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
      ~/  %peer
      |=  {sto/bone vax/vase}
      ^-  {(each cove tang) _+>}
      =^  yep  +>.$  (ap-move-mess vax)
      :_  +>.$
      ?:  ?=(%| -.yep)  yep
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
      ~/  %pull
      |=  {sto/bone vax/vase}
      ^-  {(each cove tang) _+>}
      =^  yep  +>.$  (ap-move-mess vax)
      :_  +>.$
      ?:  ?=(%| -.yep)  yep
      ?.  =(~ +>.q.vax)
        [%| (ap-suck "pull: malformed card")]
      :^  %&  sto  %pass
      :-  p.p.yep
      [%send q.p.yep r.p.yep %pull ~]
    ::
    ++  ap-move-send                                    ::  pass gall action
      ~/  %send
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
      ~/  %ap-peep
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
      ~/  %ap-peer
      |=  pax/path
      ^+  +>
      =.  sup.ged  (~(put by sup.ged) ost [q.q.pry pax])
      =^  cug  +>.$  (ap-find %peer pax)
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
      ~/  %ap-poke
      |=  cag/cage
      ^+  +>
      =^  cug  +>.$  (ap-find %poke p.cag ~)
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
      =^  cug  +>.$  (ap-find /lame)
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
      ~/  %ap-pour
      |=  {pax/path vax/vase}
      ^+  +>
      ?.  &(?=({@ *} q.vax) ((sane %tas) -.q.vax))
        (ap-lame %pour (ap-suck "pour: malformed card"))
      =^  cug  +>.$  (ap-find [-.q.vax pax])
      ?~  cug
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
      ~/  %ap-purr
      |=  {wha/term pax/path cag/cage}
      ^+  +>
      =^  cug  +>.$  (ap-find [wha p.cag pax])
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
          arms
        ~
      ::
          dub
        :_(dub ?~(gac [%& dap ?~(vux %boot %bump) now] [%| u.gac]))
      ==
    ::
    ++  ap-prop                                         ::  install
      ~/  %ap-prop
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
      =^  cug  ..ap-pull  (ap-find %pull q.u.wim)
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
      ~/  %ap-take
      |=  {her/ship cog/term pax/path vux/(unit vase)}
      ^+  +>
      =^  cug  +>.$  (ap-find cog pax)
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
      ~/  %ap-safe
      |=  vax/vase
      ^-  {(each (list cove) tang) _+>}
      ?~  q.vax  :_(+>.$ [%& ~])
      ?@  q.vax  :_(+>.$ [%| (ap-suck "move: malformed list")])
      =^  hed  vel  (~(slot wa vel) 2 vax)
      =^  sud  +>.$  (ap-move hed)
      ?:  ?=(%| -.sud)  :_(+>.$ sud)
      =^  tel  vel  (~(slot wa vel) 3 vax)
      =^  res  +>.$  $(vax tel)
      :_  +>.$
      ?:  ?=(%| -.res)  res
      [%& p.sud p.res]
    ::
    ++  ap-sake                                         ::  handle result
      ~/  %ap-sake
      |=  vax/vase
      ^-  {(unit tang) _+>}
      ?:  ?=(@ q.vax)
        [`(ap-suck "sake: invalid product (atom)") +>.$]
      =^  hed  vel  (~(slot wa vel) 2 vax)
      =^  muz  +>.$  (ap-safe hed)
      ?:  ?=(%| -.muz)  [`p.muz +>.$]
      =^  tel  vel  (~(slot wa vel) 3 vax)
      =^  sav  +>.$  (ap-save tel)
      ?:  ?=(%| -.sav)  [`p.sav +>.$]
      :-  ~
      %_  +>.$
        zip  (weld (flop p.muz) zip)
        hav  p.sav
      ==
    ::
    ++  ap-save                                         ::  verify core
      ~/  %ap-save
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
      ?:  ?=(%| -.wyz)
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
        $bonk  `%a
        $build  `%f
        $cash  `%a
        $conf  `%g
        $cred  `%c
        $crew  `%c
        $crow  `%c
        $deal  `%g
        $dirk  `%c
        $drop  `%c
        $flog  `%d
        $info  `%c
        $keep  `%f
        $kill  `%f
        $look  `%j
        $merg  `%c
        $mint  `%j
        $mont  `%c
        $nuke  `%a
        $ogre  `%c
        $perm  `%c
        $rest  `%b
        $rule  `%e
        $serv  `%e
        $snap  `%j
        $them  `%e
        $wait  `%b
        $want  `%a
        $warp  `%c
        $well  `%e
        $well  `%e
        $wind  `%j
        $wipe  `%f
      ==
    --
  --
++  call                                                ::  request
  ~%  %gall-call  +>   ~
  |=  {hen/duct hic/(hypo (hobo task:able))}
  ^+  [*(list move) ..^$]
  =>  .(q.hic ?.(?=($soft -.q.hic) q.hic ((hard task:able) p.q.hic)))
  ?-    -.q.hic
      $conf
    ?.  =(our p.p.q.hic)
      ~&  [%gall-not-ours p.p.q.hic]
      [~ ..^$]
    mo-abet:(mo-conf:(mo-abed:mo hen) q.p.q.hic q.q.hic)
  ::
      $deal
    =<  mo-abet
    ?.  =(our q.p.q.hic)                                ::  either to us
      ?>  =(our p.p.q.hic)                              ::  or from us
      (mo-away:(mo-abed:mo hen) q.p.q.hic q.q.hic)
    (mo-come:(mo-abed:mo hen) p.p.q.hic q.q.hic)
  ::
      $init
    [~ ..^$(sys.mast.all hen)]
  ::
      $sunk  [~ ..^$]
  ::
      $vega  [~ ..^$]
  ::
      $west
    ?>  ?=({?($ge $gh) @ ~} q.q.hic)
    =*  dap  i.t.q.q.hic
    =*  him  p.q.hic
    ?:  ?=($ge i.q.q.hic)
      =+  mes=((hard {@ud rook}) r.q.hic)
      =<  mo-abet
      (mo-gawk:(mo-abed:mo hen) him dap mes)
    =+  mes=((hard {@ud roon}) r.q.hic)
    =<  mo-abet
    (mo-gawd:(mo-abed:mo hen) him dap mes)
  ::
      $wegh
    =/  =mass
      :+  %gall  %|
      :~  foreign+&+sap.mast.all
          :+  %blocked  %|
          (sort ~(tap by (~(run by wub.mast.all) |=(sofa [%& +<]))) aor)
          :+  %active   %|
          (sort ~(tap by (~(run by bum.mast.all) |=(seat [%& +<]))) aor)
          dot+&+all
      ==
    =/  =move  [hen %give %mass mass]
    [[move ~] ..^$]
  ==
::
++  load                                                ::  recreate vane
  |=  old/axle-n
  ^+  ..^$
  ?-  -.old
      $0  ..^$(all old)
  ==
::
++  scry
  ~/  %gall-scry
  |=  {fur/(unit (set monk)) ren/@tas why/shop syd/desk lot/coin tyl/path}
  ^-  (unit (unit cage))
  ?.  ?=(%& -.why)  ~
  =*  his  p.why
  ?:  ?&  =(%u ren)
          =(~ tyl)
          =([%$ %da now] lot)
          =(our his)
          (~(has by bum.mast.all) syd)
      ==
    ``[%null !>(~)]
  ?.  =(our his)
    ~
  ?.  =([%$ %da now] lot)
    ~
  ?.  (~(has by bum.mast.all) syd)
    [~ ~]
  ?.  ?=(^ tyl)
    ~
  (mo-peek:mo-abed:mo syd high+`his ren tyl)
::
++  stay                                                ::  save w+o cache
  `axle`all
::
++  take                                                ::  response
  ~/  %gall-take
  |=  {tea/wire hen/duct hin/(hypo sign-arvo)}
  ^+  [*(list move) ..^$]
  ~|  [%gall-take tea]
  ?>  ?=([?($sys $use) *] tea)
  =+  mow=(mo-abed:mo hen)
  ?-  i.tea
    $sys  mo-abet:(mo-cyst:mow t.tea q.hin)
    $use  mo-abet:(mo-cook:mow t.tea hin)
  ==
--
