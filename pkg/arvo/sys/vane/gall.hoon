!:  ::  %gall, agent execution
!?  163
!:
::::
|=  pit=vase
=,  gall
=>  =~
|%  ::::::::::::::::::::::::::::::::::::::::::::::::::::::    rest of arvo
    ::::::::::::::::::::::::::::::::::::::::::::::::::::::
++  coke                                                ::  cook
  $?  %inn                                              ::
      %out                                              ::
      %cay                                              ::
  ==                                                    ::
++  volt  ?(%low %high)                                 ::  voltage
++  torc  $@(?(%iron %gold) [%lead p=ship])             ::  security control
++  roon                                                ::  reverse ames msg
  $%  [%d p=mark q=*]                                   ::  diff (diff)
      [%x ~]                                            ::
  ==                                                    ::
++  rook                                                ::  forward ames msg
  $%  [%m p=mark q=*]                                   ::  message
      [%l p=mark q=path]                                ::  "peel" subscribe
      [%s p=path]                                       ::  subscribe
      [%u ~]                                            ::  cancel+unsubscribe
  ==                                                    ::
++  whey                                                ::  foreign response
  $?  %peer                                             ::
      %peel                                             ::
      %poke                                             ::
      %pull                                             ::
  ==                                                    ::
--                                                      ::
|%  ::::::::::::::::::::::::::::::::::::::::::::::::::::::    local arvo
    ::::::::::::::::::::::::::::::::::::::::::::::::::::::
++  cote                                                ::  ++ap note
  $%  [%meta p=@tas q=vase]                             ::
      [%send p=ship q=cush]                             ::
      [%hiss p=(unit knot) q=mark r=cage]               ::
  ==                                                    ::
++  cove  (pair bone (wind cote cuft))                  ::  internal move
++  move  (pair duct (wind note-arvo gift-arvo))        ::  typed move
--                                                      ::
|%  ::::::::::::::::::::::::::::::::::::::::::::::::::::::    %gall state
    ::::::::::::::::::::::::::::::::::::::::::::::::::::::
++  axle-n  ?(axle)                                     ::  upgrade path
    ::::::::::::::::::::::::::::::::::::::::::::::::::::::  state proper
    ::::::::::::::::::::::::::::::::::::::::::::::::::::::
++  axle                                                ::  all state
  $:  %0                                                ::  state version
      =mast                                             ::  apps by ship
  ==                                                    ::
++  gest                                                ::  subscriber data
  $:  sup=bitt                                          ::  incoming subscribers
      neb=boat                                          ::  outgoing subscribers
      qel=(map bone @ud)                                ::  queue meter
  ==                                                    ::
++  mast                                                ::  ship state
  $:  mak=*                                             ::  (deprecated)
      sys=duct                                          ::  system duct
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
      q=(map duct @ud)                                  ::  by duct
      r=(map @ud duct)                                  ::  by index
  ==                                                    ::
++  scar                                                ::  opaque input
  $:  p=@ud                                             ::  bone sequence
      q=(map duct bone)                                 ::  by duct
      r=(map bone duct)                                 ::  by bone
  ==                                                    ::
::                                                      ::
::  XX a hack, required to break a subscription loop    ::
::  which arises when an invalid mark crashes a diff.   ::
::  See usage in ap-misvale.                            ::
++  misvale-data  (set wire)                            ::  subscrs w/ bad marks
++  seat                                                ::  agent state
  $:  misvale=misvale-data                              ::  bad reqs
      vel=worm                                          ::  cache
      arms=(map [term path] (unit (pair @ud term)))     ::  ap-find cache
      mom=duct                                          ::  control duct
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
      eny=@uvJ                                          ::  entropy
      lat=@da                                           ::  time
  ==                                                    ::
--                                                      ::
    ::::::::::::::::::::::::::::::::::::::::::::::::::::::  vane header
    ::::::::::::::::::::::::::::::::::::::::::::::::::::::
.  ==
=|  all=axle                                            ::  all vane state
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
  ::
  =*  mas  mast.all
  =*  bowl-type  -:!>(*bowl)
  =*  mo-context  +
  ::
  |_  $:  hen=duct
          moves=(list move)
      ==
  ::
  ++  mo-state  .
  ::
  ::  +mo-abed: initialise engine with the provided duct.
  ::
  ++  mo-abed
    |=  =duct
    ^+  mo-state
    ::
    mo-state(hen duct)
  ::
  ::  +mo-abet: resolve moves.
  ::
  ++  mo-abet
    ^-  [(list move) _mo-context]
    ::
    =/  resolved  (flop moves)
    [resolved mo-context]
  ::
  ::  +mo-conf: send an %exec to ford using a beak assembled with the current
  ::            timestamp.
  ::
  ++  mo-conf
    |=  [=dude =ship =desk]
    ^+  mo-state
    ::
    =/  =beak
      =/  =case  [%da now]
      [p=ship q=desk r=case]
    ::
    (mo-boot dude beak)
  ::
  ::  +mo-pass: prepend a standard %pass move to the move state.
  ::
  ++  mo-pass
    |=  pass=(pair path note-arvo)
    ^+  mo-state
    ::
    =/  =move  [hen %pass pass]
    mo-state(moves [move moves])
  ::
  ::  +mo-give: prepend a standard %give move to the move state.
  ::
  ++  mo-give
    |=  =gift:able
    ^+  mo-state
    ::
    =/  =move  [hen %give gift]
    mo-state(moves [move moves])
  ::
  :: +mo-okay: check that a ford-built app core produces a valid bowl.
  ::
  ++  mo-okay
    ~/  %mo-okay
    |=  =vase
    ^-  ?
    ::
    =/  val  (slew 12 vase)
    ?~  val
      %.n
    ::
    =/  bowl  p.u.val
    (~(nest ut bowl) %.n bowl-type)
  ::
  ::  +mo-receive-core: receives an app core built by ford-turbo.
  ::
  ++  mo-receive-core
    ~/  %mo-receive-core
    |=  [=dude =beak =made-result:ford]
    ^+  mo-state
    ::
    ?:  ?=([%incomplete *] made-result)
      (mo-give %onto %.n tang.made-result)
    ::
    =/  build-result  build-result.made-result
    ::
    ?:  ?=([%error *] build-result)
      (mo-give %onto %.n message.build-result)
    ::
    =/  =cage  (result-to-cage:ford build-result)
    =/  result-vase  q.cage
    ::
    =/  app-data=(unit seat)  (~(get by bum.mas) dude)
    ?^  app-data
      ::  update the path
      ::
      =/  updated  u.app-data(byk beak)
      =.  bum.mas  (~(put by bum.mas) dude updated)
      ::  magic update string from +mo-boon, "complete old boot"
      ::
      ap-abet:(ap-peep:(ap-abed:ap dude [%high [~ our]]) result-vase)
    ::  first install of the app
    ::
    ?.  (mo-okay result-vase)
      (mo-give %onto %.n [%leaf "{<dude>}: bogus core"]~)
    ::
    =.  mo-state  (mo-born dude beak result-vase)
    ::
    =/  old  mo-state
    =/  wag  (ap-prop:(ap-abed:ap dude [%high [~ our]]) ~)
    ::
    ?^  -.wag
      =.  mo-state  old
      (mo-give %onto %.n u.-.wag)
    =.  mo-state  ap-abet:+.wag
    (mo-give:(mo-claw dude) %onto %.y dude %boot now)
  ::
  ::  +mo-born: create a new seat.
  ::
  ++  mo-born
    |=  [=dude =beak =vase]
    ^+  mo-state
    ::
    =|  =seat
    ::
    =/  =scar
      =/  bone  1
      =/  bone-by-duct  [[[~ ~] 0] ~ ~]
      =/  duct-by-bone  [[0 [~ ~]] ~ ~]
      [p=bone q=bone-by-duct r=duct-by-bone]
    ::
    =/  new-seat
      %_  seat
        mom  hen
        byk  beak
        hav  vase
        zam  scar
      ==
    ::
    %_  mo-state
      bum.mas  (~(put by bum.mas) dude new-seat)
    ==
  ::
  ::  +mo-boot: sends an %exec to ford.
  ::
  ++  mo-boot
    |=  [=dude =beak]
    ^+  mo-state
    ::
    =/  =path
      =/  ship  (scot %p p.beak)
      =/  desk  q.beak
      =/  case  (scot r.beak)
      /sys/core/[dude]/[ship]/[desk]/[case]
    ::
    =/  =note-arvo
      =/  disc  [p q]:beak
      =/  spur  /hoon/[dude]/app
      =/  schematic  [%core disc spur]
      [%f %build live=%.y schematic]
    ::
    =/  pass  [path note-arvo]
    (mo-pass pass)
  ::
  :: +mo-away: handle a foreign request.
  ::
  ++  mo-away
    ~/  %mo-away
    |=  [=ship =cush]
    ^+  mo-state
    ::
    ?:  ?=(%pump -.q.cush)
      ::
      ::  you'd think this would send an ack for the diff
      ::  that caused this pump.  it would, but we already
      ::  sent it when we got the diff in ++mo-cyst.  then
      ::  we'd have to save the network duct and connect it
      ::  to this returning pump.
      ::
      mo-state
    ::
    ?:  ?=(%peer-not -.q.cush)
      =/  err  (some p.q.cush)
      (mo-give %unto %reap err)
    ::
    =^  num  mo-state  (mo-bale ship)
    ::
    =/  =rook
      ?-  -.q.cush
        %poke  [%m p.p.q.cush q.q.p.q.cush]
        %pull  [%u ~]
        %puff  !!
        %punk  !!
        %peel  [%l p.q.cush q.q.cush]
        %peer  [%s p.q.cush]
      ==
    ::
    =/  action  -.q.cush
    =/  =path  /sys/way/[action]
    =/  note=note-arvo  [%a %want ship [%g %ge p.cush ~] [num rook]]
    ::
    (mo-pass path note)
  ::
  ::  +mo-awed: handle foreign response.
  ::
  ++  mo-awed
    |=  [=whey art=(unit ares)]
    ^+  mo-state
    ::
    =/  =ares
      =/  tanks  [%blank ~]
      =/  tang  (some tanks)
      (fall art tang)
    ::
    =/  to-tang
      |=  ars=(pair term tang)
      ^-  tang
      =/  tape  (trip p.ars)
      [[%leaf tape] q.ars]
    ::
    =/  result  (bind ares to-tang)
    ::
    ?-  whey
      %peel  (mo-give %unto %reap result)
      %peer  (mo-give %unto %reap result)
      %poke  (mo-give %unto %coup result)
      %pull  mo-state
    ==
  ::
  ::  +mo-bale: assign an out bone.
  ::
  ++  mo-bale
    |=  =ship
    ^-  [@ud _mo-state]
    ::
    =/  =scad
      =/  default  [1 ~ ~]
      =/  existing  (~(get by sap.mas) ship)
      (fall existing default)
    ::
    =/  nom  (~(get by q.scad) hen)
    ::
    ?^  nom
      [u.nom mo-state]
    ::
    =/  index  p.scad
    ::
    =/  contacts
      %_  scad
        p  +(index)
        q  (~(put by q.scad) hen index)
        r  (~(put by r.scad) index hen)
      ==
    ::
    =/  new-state
      %_  mo-state
        sap.mas  (~(put by sap.mas) ship contacts)
      ==
    ::
    [index new-state]
  ::
  ::  +mo-ball: retrieve an out bone by index.
  ::
  ++  mo-ball
    |=  [=ship index=@ud]
    ^-  duct
    ::
    =/  conns  (~(got by sap.mas) ship)
    =/  duct  r:conns
    (~(got by duct) index)
  ::
  ::  +mo-cyst-core: receive a core.
  ::
  ++  mo-cyst-core
    |=  [=path =sign-arvo]
    ^+  mo-state
    ::
    ?>  ?=([%f %made *] sign-arvo)
    ?>  ?=([@ @ @ @ @ ~] path)
    ::
    =/  beak-path  t.t.path
    ::
    =/  =beak
      =/  ship  (slav %p i.beak-path)
      =/  desk  i.t.beak-path
      =/  case  [%da (slav %da i.t.t.beak-path)]
      [p=ship q=desk r=case]
    ::
    (mo-receive-core i.t.path beak result.sign-arvo)
  ::
  ::  +mo-cyst-pel: translated peer.
  ::
  ++  mo-cyst-pel
    |=  [=path =sign-arvo]
    ^+  mo-state
    ::
    ?>  ?=([%f %made *] sign-arvo)
    ?>  ?=([@ @ ~] path)
    ::
    ?:  ?=([%incomplete *] result.sign-arvo)
      =/  err  (some tang.result.sign-arvo)
      (mo-give %unto %coup err)
    ::
    =/  build-result  build-result.result.sign-arvo
    ::
    ?:  ?=([%error *] build-result)
      =/  err  (some message.build-result)
      (mo-give %unto %coup err)
    ::
    =/  =cage  (result-to-cage:ford build-result)
    (mo-give %unto %diff cage)
  ::
  ::  +mo-cyst-red: diff ack.
  ::
  ++  mo-cyst-red
    |=  [=path =sign-arvo]
    ^+  mo-state
    ::
    ?>  ?=([@ @ @ @ ~] path)
    ::
    ?.  ?=([%a %woot *] sign-arvo)
      ~&  [%red-want path]
      mo-state
    ::
    =/  him  (slav %p i.t.path)
    =/  dap  i.t.t.path
    =/  num  (slav %ud i.t.t.t.path)
    ::
    =/  gift  +>.sign-arvo
    ::
    =/  sys-path
      =/  pax  [%req t.path]
      [%sys pax]
    ::
    ?~  q.gift
      =/  =note-arvo  [%g %deal [him our] dap %pump ~]
      (mo-pass sys-path note-arvo)
    ::
    =/  gall-move  [%g %deal [him our] dap %pull ~]
    =/  ames-move  [%a %want him [%g %gh dap ~] [num %x ~]]
    ::
    =.  mo-state  (mo-pass sys-path gall-move)
    =.  mo-state  (mo-pass sys-path ames-move)
    ::
    ?.  ?=([~ ~ %mack *] q.gift)
      ~&  [%diff-bad-ack q.gift]
      mo-state
    ~&  [%diff-bad-ack %mack]
    =/  slaw  (slog (flop q.,.+>.q.gift)) :: FIXME overly noisy lark
    (slaw mo-state)
  ::
  ::  +mo-cyst-rep: reverse request.
  ::
  ++  mo-cyst-rep
    |=  [=path =sign-arvo]
    ^+  mo-state
    ::
    ?>  ?=([@ @ @ @ ~] path)
    ?>  ?=([%f %made *] sign-arvo)
    ::
    =/  him  (slav %p i.t.path)
    =/  dap  i.t.t.path
    =/  num  (slav %ud i.t.t.t.path)
    ::
    ?:  ?=([%incomplete *] result.sign-arvo)
      =/  err  (some tang.result.sign-arvo)
      (mo-give %mack err)
    ::
    =/  build-result  build-result.result.sign-arvo
    ::
    ?:  ?=([%error *] build-result)
      ::  "XX should crash"
      =/  err  (some message.build-result)
      (mo-give %mack err)
    ::
    ::  "XX pump should ack"
    =.  mo-state  (mo-give %mack ~)
    ::
    =/  duct  (mo-ball him num)
    =/  initialised  (mo-abed duct)
    ::
    =/  =cage  (result-to-cage:ford build-result)
    =/  move  [%unto %diff cage]
    ::
    (mo-give:initialised move)
  ::
  ::  +mo-cyst-req: inbound request.
  ::
  ++  mo-cyst-req
    |=  [=path =sign-arvo]
    ^+  mo-state
    ::
    ?>  ?=([@ @ @ @ ~] path)
    ::
    =/  him  (slav %p i.t.path)
    =/  dap  i.t.t.path
    =/  num  (slav %ud i.t.t.t.path)
    ::
    ?:  ?=([%f %made *] sign-arvo)
      ?:  ?=([%incomplete *] result.sign-arvo)
        =/  err  (some tang.result.sign-arvo)
        (mo-give %mack err)
      ::
      =/  build-result  build-result.result.sign-arvo
      ::
      ?:  ?=([%error *] build-result)
        =/  err  (some message.build-result)
        (mo-give %mack err)
      ::
      =/  =cage  (result-to-cage:ford build-result)
      =/  sys-path  [%sys path]
      =/  =note-arvo  [%g %deal [him our] i.t.t.path %poke cage]
      ::
      (mo-pass sys-path note-arvo)
    ::
    ?:  ?=([%a %woot *] sign-arvo)
      mo-state
    ::
    ?>  ?=([%g %unto *] sign-arvo)
    ::
    =/  =cuft  +>.sign-arvo
    ::
    ?-    -.cuft
        ::
        %coup
        ::
      (mo-give %mack p.cuft)
        ::
        %diff
        ::
      =/  sys-path  [%sys %red t.path]
      =/  note  [%a %want him [%g %gh dap ~] [num %d p.p.cuft q.q.p.cuft]]
      (mo-pass sys-path note)
        ::
        %quit
        ::
      =/  sys-path  [%sys path]
      =/  note  [%a %want him [%g %gh dap ~] [num %x ~]]
      (mo-pass sys-path note)
        ::
        %reap
        ::
      (mo-give %mack p.cuft)
    ==
  ::
  ::  +mo-cyst-val: inbound validate.
  ::
  ++  mo-cyst-val
    |=  [=path =sign-arvo]
    ^+  mo-state
    ::
    ?>  ?=([%f %made *] sign-arvo)
    ?>  ?=([@ @ @ ~] path)
    ::
    =/  him  (slav %p i.t.path)
    =/  dap  i.t.t.path
    ::
    ?:  ?=([%incomplete *] result.sign-arvo)
      =/  err  (some tang.result.sign-arvo)
      (mo-give %unto %coup err)
    ::
    =/  build-result  build-result.result.sign-arvo
    ::
    ?:  ?=([%error *] build-result)
      =/  err  (some message.build-result)
      (mo-give %unto %coup err)
    ::
    =/  =prey  [%high ~ him]
    =/  =cage  (result-to-cage:ford build-result)
    =/  =club  [%poke cage]
    (mo-clip dap prey club)
  ::
  ::  +mo-cyst-way: outbound request.
  ::
  ++  mo-cyst-way
    |=  [=path =sign-arvo]
    ^+  mo-state
    ::
    ?>  ?=([%a %woot *] sign-arvo)
    ?>  ?=([@ @ ~] path)
    ::
    =/  why  ;;(whey i.t.path)
    =/  art  +>+.sign-arvo
    ::
    (mo-awed why art)
  ::
  ::  +mo-cyst: take in /sys.
  ::
  ++  mo-cyst
    ~/  %mo-cyst
    |=  [pax=path sih=sign-arvo]
    ^+  mo-state
    ::
    ?+  -.pax  !!
      %core  (mo-cyst-core pax sih)
      %pel  (mo-cyst-pel pax sih)
      %red  (mo-cyst-red pax sih)
      %rep  (mo-cyst-rep pax sih)
      %req  (mo-cyst-req pax sih)
      %val  (mo-cyst-val pax sih)
      %way  (mo-cyst-way pax sih)
    ==
  ::
  ::  +mo-cook: take in /use.
  ::
  ++  mo-cook
    ~/  %mo-cook
    |=  [=path hin=(hypo sign-arvo)]
    ^+  mo-state
    ::
    ?.  ?=([@ @ coke *] path)
      ~&  [%mo-cook-bad-pax path]
      !!
    ::
    =/  pap
      =/  =term  i.path
      =/  =ffuc  [~ (slav %p i.t.path)]
      =/  =prey  [%high ffuc]
      (ap-abed:ap term prey)
    ::
    =/  vax
      =/  =vase  hin
      (slot 3 vase)
    ::
    ?-  i.t.t.path
        ::
        %inn
        ::
      =/  poured  (ap-pour:pap t.t.t.path vax)
      ap-abet:poured
        ::
        %cay
        ::
      ?.  ?=([%e %sigh *] q.hin)
        ~&  [%mo-cook-weird q.hin]
        ~&  [%mo-cook-weird-path path]
        mo-state
      =/  purred  (ap-purr:pap +<.q.hin t.t.t.path +>.q.hin)
      ap-abet:purred
        ::
        %out
        ::
      ?.  ?=([%g %unto *] q.hin)
        ~&  [%mo-cook-weird q.hin]
        ~&  [%mo-cook-weird-path path]
        mo-state
      =/  pouted  (ap-pout:pap t.t.t.path +>.q.hin)
      ap-abet:pouted
    ==
  ::
  ::  +mo-claw: clear queue.
  ::
  ++  mo-claw
    |=  =dude
    ^+  mo-state
    ::
    ?.  (~(has by bum.mas) dude)
      mo-state
    ::
    =/  suf  (~(get by wub.mas) dude)
    ?~  suf
      mo-state
    ::
    |-
    ^+  mo-state
    ::
    ?:  =(~ kys.u.suf)
      %_  mo-state
        wub.mas  (~(del by wub.mas) dude)
      ==
    ::
    =^  lep  kys.u.suf  [p q]:~(get to kys.u.suf)
    ::
    =/  move  [p.lep %slip %g %deal [q.q.q.lep our] dude r.lep]
    $(moves [move moves])
  ::
  ::  +mo-beak: build beak.
  ::
  ++  mo-beak
    |=  =dude
    ^-  beak
    ?~  app-data=(~(get by bum.mas) dude)
      ::
      ::  XX this fallback is necessary, as .dude could be either the source
      ::  or the destination app. ie, it might not exist locally ...
      ::
      [our %home %da now]
    byk.u.app-data
  ::
  ++  mo-peek
    ~/  %mo-peek
    |=  [=dude =prey =term =path]
    ^-  (unit (unit cage))
    ::
    =/  pap  (ap-abed:ap dude prey)
    (ap-peek:pap term path)
  ::
  ::  +mo-clip: apply club.
  ::
  ++  mo-clip
    |=  [=dude =prey =club]
    ^+  mo-state
    ::
    =/  =path
      =/  ship  (scot %p q.q.prey)
      /sys/val/[ship]/[dude]
    ::
    =/  info
      =/  beak  (mo-beak dude)
      [p q]:beak
    ::
    ?:  ?=(%puff -.club)
      =/  =note-arvo  [%f %build live=%.n [%vale info +.club]]
      (mo-pass path note-arvo)
    ::
    ?:  ?=(%punk -.club)
      =/  =note-arvo  [%f %build live=%.n [%cast info p.club [%$ q.club]]]
      (mo-pass path note-arvo)
    ::
    ?:  ?=(%peer-not -.club)
      =/  err  (some p.club)
      (mo-give %unto %reap err)
    ::
    =/  pap  (ap-abed:ap dude prey)
    =/  clubbed  (ap-club:pap club)
    ap-abet:clubbed
  ::
  ::  +mo-come: handle locally
  ::
  ++  mo-come
    |=  [=ship =cush]
    ^+  mo-state
    ::
    =/  default-sofa  *sofa
    ::
    =/  =prey  [%high [~ ship]]
    =/  =dude  p.cush
    =/  =club  q.cush
    ::
    ?:  |(!(~(has by bum.mas) dude) (~(has by wub.mas) dude))
      ~&  >>  [%mo-not-running dude -.club]
      =/  =sofa
        =/  kisses  (fall (~(get by wub.mas) dude) default-sofa)
        =/  kiss  [hen prey club]
        kisses(kys (~(put to kys.kisses) kiss))
      ::
      %_  mo-state
        wub.mas  (~(put by wub.mas) dude sofa)
      ==
    ::
    (mo-clip dude prey club)
  ::
  ::  +mo-gawk: ames forward.
  ::
  ++  mo-gawk
    |=  [him=@p =dude num=@ud =rook]
    ^+  mo-state
    ::
    =.  mo-state
      ?.  ?=(%u -.rook)
        mo-state
      (mo-give %mack ~)
    ::
    =/  =path
      =/  ship  (scot %p him)
      =/  numb  (scot %ud num)
      /sys/req/[ship]/[dude]/[numb]
    ::
    =/  =note-arvo
      ?-  -.rook
        %m  [%g %deal [him our] dude %puff p.rook q.rook]
        %l  [%g %deal [him our] dude %peel p.rook q.rook]
        %s  [%g %deal [him our] dude %peer p.rook]
        %u  [%g %deal [him our] dude %pull ~]
      ==
    ::
    (mo-pass path note-arvo)
  ::
  ::  +mo-gawd: ames backward.
  ::
  ++  mo-gawd
    |=  [him=@p =dude num=@ud =roon]
    ^+  mo-state
    ::
    ?-    -.roon
        ::
        %d
        ::
      =/  =path
        =/  ship  (scot %p him)
        =/  numb  (scot %ud num)
        /sys/rep/[ship]/[dude]/[numb]
      ::
      =/  =note-arvo
        =/  beak  (mo-beak dude)
        =/  info  [p q]:beak
        [%f %build live=%.n [%vale info p.roon q.roon]]
      ::
      (mo-pass path note-arvo)
    ::
        ::
        %x
        ::
      =.  mo-state  (mo-give %mack ~)                  ::  XX should crash
      ::
      =/  out  (mo-ball him num)
      (mo-give:(mo-abed out) %unto %quit ~)
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
        moves  :(weld (turn zip ap-aver) (turn dub ap-avid) moves)
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
        =+  mar=(~(gut by pyl) p.cov p.cay)
        ?:  =(mar p.cay)  [%give %unto p.q.cov]
        :+  %pass
          [%sys %pel dap ~]
        [%f %build live=%.n [%cast [p q]:(mo-beak dap) mar [%$ cay]]]
      ::
          $pass
        :+  %pass  `path`[%use dap p.q.cov]
        ?-  -.q.q.cov
::          $hiss  `note-arvo`[%e %hiss +.q.q.cov]
          $send  `note-arvo`[%g %deal [our p.q.q.cov] q.q.q.cov]
          $meta  `note-arvo`[`@tas`p.q.q.cov %meta `vase`q.q.q.cov]
::          $response  `note-arvo`[%i %response raw-http-response.q.q.cov]
        ==
        ::
        :: I'm sort of stumped on how to get a %give out of the above; it's
        :: just turning %cove into a %pass instead.
        ::
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
            [!>(`path`+.pax) (ap-cage cag)]
          [!>((slag (dec p.u.cug) `path`+.pax)) q.cag]
      =^  cam  +>.$  (ap-call q.u.cug arg)
      ?^  cam
        (ap-pump:(ap-lame q.u.cug u.cam) | her pax)
      (ap-pump & her pax)
    ::
    ++  ap-cage                                         ::  cage to tagged vase
      |=  cag/cage
      ^-  vase
      (slop `vase`[[%atom %tas `p.cag] p.cag] q.cag)
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
      =+  suy=(~(gut by qel.ged) ost 0)
      =/  subscriber=(unit (pair ship path))
        (~(get by sup.ged) ost)
      ?:  ?&  =(20 suy)
              ?|  ?=(~ subscriber)
                  !=(our p.u.subscriber)
              ==
          ==
        ~&  [%gall-pulling-20 ost (~(get by sup.ged) ost) (~(get by r.zam) ost)]
        [%| ..ap-fill]
      ::  ~&  :*  %gall-pushing-20
      ::          ost
      ::          suy=suy
      ::          (~(get by r.zam) ost)
      ::      ==
      [%& ..ap-fill(qel.ged (~(put by qel.ged) ost +(suy)))]
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
        ::  $hiss  (ap-move-hiss -.q.vax cav)
        $peel  (ap-move-peel -.q.vax cav)
        $peer  (ap-move-peer -.q.vax cav)
        $pull  (ap-move-pull -.q.vax cav)
        $poke  (ap-move-poke -.q.vax cav)
        $send  (ap-move-send -.q.vax cav)
        $quit  (ap-move-quit -.q.vax cav)
      ::
        ::  $connect  (ap-move-connect -.q.vax cav)
        $http-response  (ap-move-http-response -.q.vax cav)
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
    ++  ap-move-http-response
      |=  [sto=bone vax=vase]
      ^-  [(each cove tang) _+>]
      ::
      ::  TODO: Magic vase validation. I have no idea how malformed checking works.
      ::
      ::  This should be moved into +cote instead of the rest of
      ::
      :_  +>.$
      [%& sto %give %http-response ;;(http-event:http q.vax)]
    ::
    ::  ++  ap-move-request
    ::    |=  [sto=bone vax=vase]
    ::    ^-  [(each cove tang) _+>]
    ::    ::
    ::    ::  TODO: Magic vase validation
    ::    ::
    ::    :_  +>.$
    ::    :^  %&  sto  %pass
    ::    :-  [(scot %p q.q.pry) %cay u.pux]
    ::    ~!  *cote
    ::    =-  ~!  -  `cote`-
    ::    [%hiss u.usr r.q.vax [p.q.gaw paw]]
    ::
    ::  ++  ap-move-hiss                                    ::  pass %hiss
    ::    ~/  %hiss
    ::    |=  {sto/bone vax/vase}
    ::    ^-  {(each cove tang) _+>}
    ::    ?.  &(?=({p/* q/* r/@ s/{p/@ *}} q.vax) ((sane %tas) r.q.vax))
    ::      =+  args="[%hiss wire (unit knot) mark cage]"
    ::      :_(+>.$ [%| (ap-suck "hiss: bad hiss ask.{args}")])
    ::    =^  gaw  vel  (~(slot wa vel) 15 vax)
    ::    ?.  &(?=({p/@ *} q.gaw) ((sane %tas) p.q.gaw))
    ::      :_(+>.$ [%| (ap-suck "hiss: malformed cage")])
    ::    =^  paw  vel  (~(stop wa vel) 3 gaw)
    ::    =+  usr=((soft (unit knot)) q.q.vax)
    ::    ?.  &(?=(^ usr) ?~(u.usr & ((sane %ta) u.u.usr)))
    ::      :_(+>.$ [%| (ap-suck "hiss: malformed (unit knot)")])
    ::    =+  pux=((soft path) p.q.vax)
    ::    ?.  &(?=(^ pux) (levy u.pux (sane %ta)))
    ::      :_(+>.$ [%| (ap-suck "hiss: malformed path")])
    ::    :_  +>.$
    ::    :^  %&  sto  %pass
    ::    :-  [(scot %p q.q.pry) %cay u.pux]
    ::    ~!  *cote
    ::    =-  ~!  -  `cote`-
    ::    [%hiss u.usr r.q.vax [p.q.gaw paw]]
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
        ~&  [%bad-path pux]
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
            [!>(`path`pax) (ap-cage cag)]
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
        ::  ???
        $http-response  !!
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
        %bonk  `%a
        %build  `%f
        %cash  `%a
        %conf  `%g
        %cred  `%c
        %crew  `%c
        %crow  `%c
        %deal  `%g
        %dirk  `%c
        %drop  `%c
        %flog  `%d
        %info  `%c
        %keep  `%f
        %kill  `%f
        %look  `%j
        %listen  `%j
        %merg  `%c
        %mont  `%c
        %moon  `%j
        %nuke  `%a
        %ogre  `%c
        %perm  `%c
        %rest  `%b
        %rekey  `%j
        %wait  `%b
        %want  `%a
        %warp  `%c
        %wash  `%g
        %wipe  `%f
      ::
        %request         `%i
        %cancel-request  `%i
        %serve       `%e
        %connect     `%e
        %disconnect  `%e
        %rule        `%e
      ==
    --
  --
++  call                                                ::  request
  ~%  %gall-call  +>   ~
  |=  [hen=duct hic=(hypo (hobo task:able))]
  ^+  [*(list move) ..^$]
  ::
  =>  .(q.hic ?.(?=($soft -.q.hic) q.hic ;;(task:able p.q.hic)))
  ::
  ?-    -.q.hic
      ::
      %conf
      ::
    =/  doc=dock  p.q.hic
    =/  syp=ship  p.doc
    ?.  =(our syp)
      ~&  [%gall-not-ours syp]
      [~ ..^$]
    ::
    =/  confed
      =/  initialised  (mo-abed:mo hen)
      =/  dud=dude  q.doc
      =/  des=(pair ship desk)  q.q.hic
      (mo-conf:initialised dud des)
    ::
    mo-abet:confed
  ::
      ::
      %deal
      ::
    =<  mo-abet
    ?.  =(our q.p.q.hic)                                ::  either to us
      ?>  =(our p.p.q.hic)                              ::  or from us
      (mo-away:(mo-abed:mo hen) q.p.q.hic q.q.hic)
    (mo-come:(mo-abed:mo hen) p.p.q.hic q.q.hic)
  ::
      ::
      %init
      ::
    [~ ..^$(sys.mast.all hen)]
  ::
      ::
      %sunk  [~ ..^$]
      ::
  ::
      ::
      %vega  [~ ..^$]
      ::
  ::
      ::
      %west
      ::
    ?>  ?=({?($ge $gh) @ ~} q.q.hic)
    =*  dap  i.t.q.q.hic
    =*  him  p.q.hic
    ?:  ?=($ge i.q.q.hic)
      =+  mes=;;({@ud rook} r.q.hic)
      =<  mo-abet
      (mo-gawk:(mo-abed:mo hen) him dap mes)
    =+  mes=;;({@ud roon} r.q.hic)
    =<  mo-abet
    (mo-gawd:(mo-abed:mo hen) him dap mes)
  ::
      %wash
    =.  bum.mast.all  (~(run by bum.mast.all) |=(=seat seat(vel *worm)))
    [~ ..^$]
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
      ==
    ``[%noun !>((~(has by bum.mast.all) syd))]
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
