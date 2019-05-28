!:  ::  %gall, agent execution
!?  163
!:
::::
|=  pit=vase
=,  gall
=>  =~
::
::  (rest of arvo)
::
|%
::
::  +internal-gift: synonym for +cuft.
::
++  internal-gift  cuft
::
::  +internal-task: synonym for +cush.
::
++  internal-task  cush
::
::  +agent-action: synonym for +club.
::
++  agent-action  club
::
::  +coke: cook.
::
++  coke
  $?  %inn
      %out
      %cay
  ==
::
::  +volt: voltage.
::
++  volt  ?(%low %high)
::
::  +security-control: security control.
::
++  security-control  $@(?(%iron %gold) [%lead p=ship])
::
::  +reverse-ames: reverse ames message.
::
++  reverse-ames
  $%
      :: diff
      ::
      [action=%d p=mark q=*]
      ::  etc.
      ::
      [action=%x ~]
  ==
::
::  +forward-ames: forward ames message.
::
++  forward-ames
  $%
      :: message
      ::
      [action=%m mark=mark noun=*]
      :: "peel" subscribe
      ::
      [action=%l mark=mark path=path]
      :: subscribe
      ::
      [action=%s path=path]
      :: cancel+unsubscribe
      ::
      [action=%u ~]
  ==
::
::  +foreign-response: foreign response.
::
++  foreign-response
  $?  %peer
      %peel
      %poke
      %pull
  ==
--
::
::  (local arvo)
::
|%
::
::  +internal-note: +ap note.
::
++  internal-note
  $%  [task=%meta =term =vase]
      [task=%send =ship =internal-task]
      [task=%hiss knot=(unit knot) =mark =cage]
  ==
::
::  +internal-move: internal move.
::
++  internal-move
  $:
    =bone
    move=(wind internal-note internal-gift)
  ==
::
::  +move: typed move.
::
++  move  (pair duct (wind note-arvo gift-arvo))
--
::
::  (%gall state)
::
|%
::
::  +gall-old: upgrade path.
::
++  gall-old  ?(gall)
::
::  +gall: all state.
::
++  gall
  $:
      :: state version
      ::
      %0
      :: apps by ship
      ::
      =ship-state
  ==
::
::  +subscriber-data: subscriber data.
::
++  subscriber-data
  $:
      :: incoming subscribers
      ::
      incoming=bitt
      :: outgoing subscribers
      ::
      outgoing=boat
      :: queue meter
      ::
      meter=(map bone @ud)
  ==
::
::  +ship-state: ship state.
::
++  ship-state
  $:
      :: (deprecated)
      ::
      mak=*
      ::  system duct
      ::
      system-duct=duct
      ::  foreign contacts
      ::
      contacts=(map ship foreign)
      ::  running agents
      ::
      running=(map term agent)
      ::  waiting queue
      ::
      waiting=(map term blocked)
  ==
::
::  +routes: new cuff.
::
++  routes
    $:
        :: disclosing to
        ::
        disclosing=(unit (set ship))
        :: attributed to
        ::
        attributing=ship
    ==
::
::  +privilege: privilege.
::
++  privilege
    $:
        :: voltage
        ::
        =volt
        :: routes
        ::
        =routes
    ==
::
::  +foreign: foreign connections.
::
++  foreign
  $:
      :: index
      ::
      index=@ud
      :: by duct
      ::
      index-map=(map duct @ud)
      :: by index
      ::
      duct-map=(map @ud duct)
  ==
::
::  +opaque-ducts: opaque input.
::
++  opaque-ducts
  $:
      :: bone sequence
      ::
      bone=@ud
      :: by duct
      ::
      bone-map=(map duct bone)
      :: by bone
      ::
      duct-map=(map bone duct)
  ==
::
::  +misvale-data: subscribers with bad marks.
::
::    XX a hack, required to break a subscription loop
::    which arises when an invalid mark crashes a diff.
::    See usage in ap-misvale.
::
++  misvale-data  (set wire)
::
::  +agent: agent state.
::
++  agent
  $:
      :: bad reqs
      ::
      misvale=misvale-data
      :: cache
      ::
      cache=worm
      :: ap-find-arm cache
      ::
      arm-cache=(map [term path] (unit (pair @ud term)))
      :: control duct
      ::
      control-duct=duct
      :: unstopped
      ::
      live=?
      :: privilege
      ::
      privilege=security-control
      :: statistics
      ::
      stats=stats
      :: subscribers
      ::
      subscribers=subscriber-data
      :: running state
      ::
      running-state=vase
      :: update control
      ::
      beak=beak
      :: req'd translations
      ::
      required-trans=(map bone mark)
      :: opaque ducts
      ::
      ducts=opaque-ducts
  ==
::
:: +blocked: blocked kisses.
::
++  blocked  (qeu (trel duct privilege agent-action))
::
:: +stats: statistics.
::
++  stats
  $:
      :: change number
      ::
      change=@ud
      :: entropy
      ::
      eny=@uvJ
      :: time
      ::
      time=@da
  ==
--
::
:: (vane header)
::
.  ==
::
:: (all vane state)
::
=|  =gall
|=  $:
        :: identity
        ::
        our=ship
        :: urban time
        ::
        now=@da
        :: entropy
        ::
        eny=@uvJ
        :: activate
        ::
        ska=sley
    ==
::
::  (opaque core)
::
~%  %gall-top  ..is  ~
::
::  (state machine)
::
|%
::
::  +gall-payload:  gall payload.
::
++  gall-payload  +
::
::  +mo: move handling.
::
++  mo
  ~%  %gall-mo  +>  ~
  ::
  |_
    $:
      hen=duct
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
    ^-  [(list move) _gall-payload]
    ::
    =/  resolved  (flop moves)
    [resolved gall-payload]
  ::
  ::  +mo-boot: pass a %build move to %ford.
  ::
  ::    The move is passed along a /sys/core path suffixed by app, ship, desk,
  ::    and date information.
  ::
  ++  mo-boot
    |=  [=term =ship =desk]
    ^+  mo-state
    ::
    =/  =case  [%da now]
    ::
    =/  =path
      =/  ship  (scot %p ship)
      =/  case  (scot case)
      /sys/core/[term]/[ship]/[desk]/[case]
    ::
    =/  =note-arvo
      =/  disc  [ship desk]
      =/  spur  /hoon/[term]/app
      =/  =schematic:ford  [%core disc spur]
      [%f %build live=%.y schematic]
    ::
    =/  pass  [path note-arvo]
    (mo-pass pass)
  ::
  ::  +mo-pass: prepend a standard %pass move to the move state.
  ::
  ++  mo-pass
    |=  pass=(pair path note-arvo)
    ^+  mo-state
    ::
    =/  =move  [hen [%pass pass]]
    mo-state(moves [move moves])
  ::
  ::  +mo-give: prepend a standard %give move to the move state.
  ::
  ++  mo-give
    |=  =gift:able
    ^+  mo-state
    ::
    =/  =move  [hen [%give gift]]
    mo-state(moves [move moves])
  ::
  :: +mo-contains-valid-bowl: check that a vase contains a valid bowl.
  ::
  ++  mo-contains-valid-bowl
    ~/  %mo-contains-valid-bowl
    |=  =vase
    ^-  ?
    ::
    =/  inferred  -:!>(*bowl)
    =/  maybe-vase  (slew 12 vase)
    ::
    ?~  maybe-vase
      %.n
    =/  =type  p.u.maybe-vase
    (~(nest ut type) %.n inferred)
  ::
  ::  +mo-receive-core: receives an app core built by %ford.
  ::
  ++  mo-receive-core
    ~/  %mo-receive-core
    |=  [=term =beak =made-result:ford]
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
    =/  app-data=(unit agent)
      (~(get by running.ship-state.gall) term)
    ::
    ?^  app-data
      ::  update the path
      ::
      =/  updated  u.app-data(beak beak)
      ::
      =.  running.ship-state.gall
        (~(put by running.ship-state.gall) term updated)
      ::
      ::  magic update string from the old +mo-boon, "complete old boot"
      ::
      =/  =privilege
        =/  =routes  [disclosing=~ attributing=our]
        [%high routes]
      ::
      =/  initialised  (ap-abed:ap term privilege)
      =/  peeped  (ap-reinstall:initialised result-vase)
      ap-abet:peeped
    ::  first install of the app
    ::
    ?.  (mo-contains-valid-bowl result-vase)
      =/  err  [[%leaf "{<term>}: bogus core"] ~]
      (mo-give %onto %.n err)
    ::
    =.  mo-state  (mo-new-agent term beak result-vase)
    ::
    =/  old  mo-state
    ::
    =/  wag
      =/  =routes  [disclosing=~ attributing=our]
      =/  =privilege  [%high routes]
      =/  initialised  (ap-abed:ap term privilege)
      (ap-prep:initialised ~)
    ::
    =/  maybe-tang  -.wag
    =/  new  +.wag
    ::
    ?^  maybe-tang
      =.  mo-state  old
      (mo-give %onto %.n u.maybe-tang)
    ::
    =.  mo-state  ap-abet:new
    ::
    =/  cleared  (mo-clear-queue term)
    (mo-give:cleared %onto %.y term %boot now)
  ::
  ::  +mo-new-agent: create a new agent and add it to state.
  ::
  ++  mo-new-agent
    |=  [=term =beak =vase]
    ^+  mo-state
    ::
    =/  =opaque-ducts
      :+  bone=1
        bone-map=[[[~ ~] 0] ~ ~]
       duct-map=[[0 [~ ~]] ~ ~]
    ::
    =/  agent
      =/  default-agent  *agent
      %_  default-agent
        control-duct    hen
        beak            beak
        running-state   vase
        ducts           opaque-ducts
      ==
    ::
    =/  running  (~(put by running.ship-state.gall) term agent)
    ::
    %_  mo-state
      running.ship-state.gall  running
    ==
  ::
  :: +mo-handle-foreign-request: handle a foreign request.
  ::
  ++  mo-handle-foreign-request
    ~/  %mo-handle-foreign-request
    |=  [=ship =internal-task]
    ^+  mo-state
    ::
    =/  =term  p.internal-task
    =/  =agent-action  q.internal-task
    ::
    ?:  ?=(%pump -.agent-action)
      ::
      ::  you'd think this would send an ack for the diff
      ::  that caused this pump.  it would, but we already
      ::  sent it when we got the diff in +mo-handle-sys.  then
      ::  we'd have to save the network duct and connect it
      ::  to this returning pump.
      ::
      mo-state
    ::
    ?:  ?=(%peer-not -.agent-action)
      =/  =tang  p.agent-action
      (mo-give %unto %reap (some tang))
    ::
    =^  bone  mo-state  (mo-assign-bone ship)
    ::
    =/  =forward-ames
      ?-  -.agent-action
        %poke  [%m p.p.agent-action q.q.p.agent-action]
        %pull  [%u ~]
        %puff  !!
        %punk  !!
        %peel  [%l agent-action]
        %peer  [%s p.agent-action]
      ==
    ::
    =/  sys-path
      =/  action  -.agent-action
      /sys/way/[action]
    ::
    =/  =note-arvo
      =/  =path  /g/ge/[term]
      =/  =noun  [bone forward-ames]
      [%a %want ship path noun]
    ::
    (mo-pass sys-path note-arvo)
  ::
  ::  +mo-handle-foreign-response: handle foreign response.
  ::
  ++  mo-handle-foreign-response
    |=  [=foreign-response art=(unit ares)]
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
    ?-  foreign-response
      %peel  (mo-give %unto %reap result)
      %peer  (mo-give %unto %reap result)
      %poke  (mo-give %unto %coup result)
      %pull  mo-state
    ==
  ::
  ::  +mo-assign-bone: assign an out bone.
  ::
  ++  mo-assign-bone
    |=  =ship
    ^-  [bone _mo-state]
    ::
    =/  =foreign
      =/  existing  (~(get by contacts.ship-state.gall) ship)
      (fall existing [1 ~ ~])
    ::
    =/  existing  (~(get by index-map.foreign) hen)
    ::
    ?^  existing
      [u.existing mo-state]
    ::
    =/  index  index.foreign
    ::
    =/  contacts
      =/  new-foreign
        %_  foreign
          index      +(index)
          index-map  (~(put by index-map.foreign) hen index)
          duct-map   (~(put by duct-map.foreign) index hen)
        ==
      (~(put by contacts.ship-state.gall) ship new-foreign)
    ::
    =/  next
      %_  mo-state
        contacts.ship-state.gall  contacts
      ==
    ::
    [index next]
  ::
  ::  +mo-retrieve-bone: retrieve an out bone by index.
  ::
  ++  mo-retrieve-bone
    |=  [=ship index=@ud]
    ^-  duct
    ::
    =/  =foreign  (~(got by contacts.ship-state.gall) ship)
    (~(got by duct-map.foreign) index)
  ::
  ::  +mo-handle-sys-core: receive a core.
  ::
  ++  mo-handle-sys-core
    |=  [=path =sign-arvo]
    ^+  mo-state
    ::
    ?>  ?=([%f %made *] sign-arvo)
    ?>  ?=([@ @ @ @ @ ~] path)
    ::
    =/  beak-path  t.t.path
    ::
    =/  =beak
      =/  =ship  (slav %p i.beak-path)
      =/  =desk  i.t.beak-path
      =/  =case  [%da (slav %da i.t.t.beak-path)]
      [ship desk case]
    ::
    (mo-receive-core i.t.path beak result.sign-arvo)
  ::
  ::  +mo-handle-sys-pel: translated peer.
  ::
  ++  mo-handle-sys-pel
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
  ::  +mo-handle-sys-red: diff ack.
  ::
  ++  mo-handle-sys-red
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
    =/  =coop  q.+>.sign-arvo
    ::
    =/  sys-path
      =/  pax  [%req t.path]
      [%sys pax]
    ::
    ?~  coop
      =/  =note-arvo
        =/  =sock  [him our]
        =/  =cush  [dap %pump ~]
        =/  =task:able  [%deal sock cush]
        [%g task]
      (mo-pass sys-path note-arvo)
    ::
    =/  gall-move=note-arvo
      =/  =sock  [him our]
      =/  =cush  [dap %pull ~]
      =/  =task:able  [%deal sock cush]
      [%g task]
    ::
    =/  ames-move=note-arvo
      =/  path  [%g %gh dap ~]
      =/  =noun  [num %x ~]
      =/  =task:able:ames  [%want him path noun]
      [%a task]
    ::
    =.  mo-state  (mo-pass sys-path gall-move)
    =.  mo-state  (mo-pass sys-path ames-move)
    ::
    ?.  ?=([~ ~ %mack *] coop)
      ~&  [%diff-bad-ack coop]
      mo-state
    ::
    ~&  [%diff-bad-ack %mack]
    =/  print  (slog (flop q.,.+>.coop))
    (print mo-state)
  ::
  ::  +mo-handle-sys-rep: reverse request.
  ::
  ++  mo-handle-sys-rep
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
      ::  XX should crash
      =/  err  (some message.build-result)
      (mo-give %mack err)
    ::
    ::  XX pump should ack
    =.  mo-state  (mo-give %mack ~)
    ::
    =/  duct  (mo-retrieve-bone him num)
    =/  initialised  (mo-abed duct)
    ::
    =/  =cage  (result-to-cage:ford build-result)
    =/  move  [%unto [%diff cage]]
    ::
    (mo-give:initialised move)
  ::
  ::  +mo-handle-sys-req: inbound request.
  ::
  ++  mo-handle-sys-req
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
    =/  =internal-gift  +>.sign-arvo
    ::
    ?-    -.internal-gift
        ::
        %coup
        ::
      (mo-give %mack p.internal-gift)
        ::
        %diff
        ::
      =/  sys-path  [%sys %red t.path]
      =/  =note-arvo
        =/  path  [%g %gh dap ~]
        =/  noun  [num %d p.p.internal-gift q.q.p.internal-gift]
        [%a %want him path noun]
      ::
      (mo-pass sys-path note-arvo)
        ::
        %quit
        ::
      =/  sys-path  [%sys path]
      =/  =note-arvo
        =/  path  [%g %gh dap ~]
        =/  noun  [num %x ~]
        [%a %want him path noun]
      ::
      (mo-pass sys-path note-arvo)
        ::
        %reap
        ::
      (mo-give %mack p.internal-gift)
    ==
  ::
  ::  +mo-handle-sys-val: inbound validate.
  ::
  ++  mo-handle-sys-val
    |=  [=path =sign-arvo]
    ^+  mo-state
    ::
    ?>  ?=([%f %made *] sign-arvo)
    ?>  ?=([@ @ @ ~] path)
    ::
    =/  =ship  (slav %p i.t.path)
    =/  =term  i.t.t.path
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
    =/  =privilege
      =/  =routes  [disclosing=~ attributing=ship]
      [%high routes]
    ::
    =/  =cage  (result-to-cage:ford build-result)
    =/  =agent-action  [%poke cage]
    (mo-apply term privilege agent-action)
  ::
  ::  +mo-handle-sys-way: outbound request.
  ::
  ++  mo-handle-sys-way
    |=  [=path =sign-arvo]
    ^+  mo-state
    ::
    ?>  ?=([%a %woot *] sign-arvo)
    ?>  ?=([@ @ ~] path)
    ::
    =/  =foreign-response  (foreign-response i.t.path)
    =/  art  +>+.sign-arvo
    ::
    (mo-handle-foreign-response foreign-response art)
  ::
  ::  +mo-handle-sys: handle incoming on /sys.
  ::
  ++  mo-handle-sys
    ~/  %mo-handle-sys
    |=  [=path =sign-arvo]
    ^+  mo-state
    ::
    ?+  -.path  !!
      %core  (mo-handle-sys-core path sign-arvo)
      %pel   (mo-handle-sys-pel path sign-arvo)
      %red   (mo-handle-sys-red path sign-arvo)
      %rep   (mo-handle-sys-rep path sign-arvo)
      %req   (mo-handle-sys-req path sign-arvo)
      %val   (mo-handle-sys-val path sign-arvo)
      %way   (mo-handle-sys-way path sign-arvo)
    ==
  ::
  ::  +mo-handle-use: handle incoming on /use.
  ::
  ++  mo-handle-use
    ~/  %mo-handle-use
    |=  [=path hin=(hypo sign-arvo)]
    ^+  mo-state
    ::
    ?.  ?=([@ @ coke *] path)
      ~&  [%mo-handle-use-bad-path path]
      !!
    ::
    =/  initialised
      =/  =term  i.path
      =/  =privilege
        =/  =ship  (slav %p i.t.path)
        =/  =routes  [disclosing=~ attributing=ship]
        [%high routes]
      ::
      (ap-abed:ap term privilege)
    ::
    =/  =vase  (slot 3 hin)
    =/  =sign-arvo  q.hin
    ::
    ?-  i.t.t.path
        ::
        %inn
        ::
      =/  poured  (ap-generic-take:initialised t.t.t.path vase)
      ap-abet:poured
        ::
        %cay
        ::
      ?.  ?=([%e %sigh *] sign-arvo)
        ~&  [%mo-handle-use-weird sign-arvo]
        ~&  [%mo-handle-use-weird-path path]
        mo-state
      ::
      =/  purred
        =/  =cage  +>.sign-arvo
        (ap-unwrap-take:initialised %sigh t.t.t.path cage)
      ::
      ap-abet:purred
        ::
        %out
        ::
      ?.  ?=([%g %unto *] sign-arvo)
        ~&  [%mo-handle-use-weird sign-arvo]
        ~&  [%mo-handle-use-weird-path path]
        mo-state
      ::
      =/  pouted
        =/  =internal-gift  +>.sign-arvo
        (ap-specific-take:initialised t.t.t.path internal-gift)
      ::
      ap-abet:pouted
    ==
  ::
  ::  +mo-clear-queue: clear blocked kisses.
  ::
  ++  mo-clear-queue
    |=  =term
    ^+  mo-state
    ::
    ?.  (~(has by running.ship-state.gall) term)
      mo-state
    ::
    =/  maybe-blocked  (~(get by waiting.ship-state.gall) term)
    ::
    ?~  maybe-blocked
      mo-state
    ::
    =/  =blocked  u.maybe-blocked
    ::
    |-  ^+  mo-state
    ::
    ?:  =(~ blocked)
      =/  waiting   (~(del by waiting.ship-state.gall) term)
      %_  mo-state
        waiting.ship-state.gall  waiting
      ==
    ::
    =^  kiss  blocked  [p q]:~(get to blocked)
    ::
    =/  =duct  p.kiss
    =/  =privilege  q.kiss
    =/  =agent-action  r.kiss
    ::
    =/  move
      =/  =sock  [attributing.routes.privilege our]
      =/  =internal-task  [term agent-action]
      =/  card  [%slip %g %deal sock internal-task]
      [duct card]
    ::
    $(moves [move moves])
  ::
  ::  +mo-beak: assemble a beak for the provided app.
  ::
  ++  mo-beak
    |=  =term
    ^-  beak
    ?~  app-data=(~(get by running.ship-state.gall) term)
      ::
      ::  XX this fallback is necessary, as .term could be either the source
      ::  or the destination app. ie, it might not exist locally ...
      ::
      [our %home %da now]
    beak.u.app-data
  ::
  ::    Simply calls to +ap-peek, which is not accessible from outside of +mo.
  ::
  ++  mo-peek
    ~/  %mo-peek
    |=  [dude=term =privilege =term =path]
    ^-  (unit (unit cage))
    ::
    =/  initialised  (ap-abed:ap dude privilege)
    (ap-peek:initialised term path)
  ::
  ::  +mo-apply: apply action.
  ::
  ++  mo-apply
    |=  [=term =privilege =agent-action]
    ^+  mo-state
    ::
    =/  =path
      =/  ship  (scot %p attributing.routes.privilege)
      /sys/val/[ship]/[term]
    ::
    =/  ship-desk
      =/  =beak  (mo-beak term)
      [p q]:beak
    ::
    ?:  ?=(%puff -.agent-action)
      =/  =schematic:ford  [%vale ship-desk +.agent-action]
      =/  =note-arvo  [%f %build live=%.n schematic]
      (mo-pass path note-arvo)
    ::
    ?:  ?=(%punk -.agent-action)
      =/  =schematic:ford  [%cast ship-desk p.agent-action [%$ q.agent-action]]
      =/  =note-arvo  [%f %build live=%.n schematic]
      (mo-pass path note-arvo)
    ::
    ?:  ?=(%peer-not -.agent-action)
      =/  err  (some p.agent-action)
      (mo-give %unto %reap err)
    ::
    =/  initialised  (ap-abed:ap term privilege)
    =/  applied  (ap-apply:initialised agent-action)
    ap-abet:applied
  ::
  ::  +mo-handle-local: handle locally.
  ::
  ++  mo-handle-local
    |=  [=ship =internal-task]
    ^+  mo-state
    ::
    =/  =privilege
      =/  =routes  [disclosing=~ attributing=ship]
      [%high routes]
    ::
    =/  =term  p.internal-task
    =/  =agent-action  q.internal-task
    ::
    =/  is-running  (~(has by running.ship-state.gall) term)
    =/  is-waiting  (~(has by waiting.ship-state.gall) term)
    ::
    ?:  |(!is-running is-waiting)
      ::
      =/  =blocked
        =/  waiting  (~(get by waiting.ship-state.gall) term)
        =/  kisses  (fall waiting *blocked)
        =/  kiss  [hen privilege agent-action]
        (~(put to kisses) kiss)
      ::
      =/  waiting  (~(put by waiting.ship-state.gall) term blocked)
      ::
      %_  mo-state
        waiting.ship-state.gall  waiting
      ==
    ::
    (mo-apply term privilege agent-action)
  ::
  ::  +mo-handle-forward: ames forward.
  ::
  ++  mo-handle-forward
    |=  [=ship =term =bone =forward-ames]
    ^+  mo-state
    ::
    =.  mo-state
      ?.  ?=(%u action.forward-ames)
        mo-state
      (mo-give %mack ~)
    ::
    =/  =path
      =/  him  (scot %p ship)
      =/  num  (scot %ud bone)
      /sys/req/[him]/[term]/[num]
    ::
    =/  =sock  [ship our]
    ::
    =/  =note-arvo
      ?-  action.forward-ames
          ::
          %m
          ::
        =/  =task:able
          =/  =internal-task  [term %puff [mark noun]:forward-ames]
          [%deal sock internal-task]
        [%g task]
          ::
          %l
          ::
        =/  =task:able
          =/  =internal-task  [term %peel [mark path]:forward-ames]
          [%deal sock internal-task]
        [%g task]
          ::
          %s
          ::
        =/  =task:able
          =/  =internal-task  [term %peer path.forward-ames]
          [%deal sock internal-task]
        [%g task]
          ::
          %u
          ::
        =/  =task:able
          =/  =internal-task  [term %pull ~]
          [%deal sock internal-task]
        [%g task]
      ==
    ::
    (mo-pass path note-arvo)
  ::
  ::  +mo-handle-backward: ames backward.
  ::
  ++  mo-handle-backward
    |=  [=ship =term =bone =reverse-ames]
    ^+  mo-state
    ::
    ?-    action.reverse-ames
        ::
        %d
        ::
      =/  =path
        =/  him  (scot %p ship)
        =/  num  (scot %ud bone)
        /sys/rep/[him]/[term]/[num]
      ::
      =/  =note-arvo
        =/  beak  (mo-beak term)
        =/  info  [p q]:beak
        =/  =schematic:ford  [%vale info p.reverse-ames q.reverse-ames]
        [%f %build live=%.n schematic]
      ::
      (mo-pass path note-arvo)
        ::
        %x
        ::
      ::  XX should crash
      =.  mo-state  (mo-give %mack ~)
      ::
      =/  initialised
        =/  out  (mo-retrieve-bone ship bone)
        (mo-abed out)
      ::
      (mo-give:initialised %unto %quit ~)
    ==
  ::
  ::  +ap: agent engine
  ::
  ++  ap
    ~%  %gall-ap  +>  ~
    ::
    |_  $:  dap=term
            pry=privilege
            ost=bone
            zip=(list internal-move)
            dub=(list (each suss tang))
            sat=agent
        ==
    ::
    ++  ap-state  .
    ::
    ::  +ap-abed: initialise the provided app with the supplied privilege.
    ::
    ++  ap-abed
      ~/  %ap-abed
      |=  [=term =privilege]
      ^+  ap-state
      ::
      =/  =agent
        =/  running  (~(got by running.ship-state.gall) term)
        =/  =stats
          =/  change  +(change.stats.running)
          =/  trop  (shaz (mix (add term change) eny))
          [change=change eny=trop time=now]
        running(stats stats)
      ::
      =/  maybe-bone  (~(get by bone-map.ducts.agent) hen)
      ::
      ?^  maybe-bone
        =/  bone  u.maybe-bone
        %_  ap-state
          dap  term
          pry  privilege
          sat  agent
          ost  bone
        ==
      ::
      =/  =opaque-ducts
        =/  bone  +(bone.ducts.agent)
        :+  bone=bone
          bone-map=(~(put by bone-map.ducts.agent) hen bone)
        duct-map=(~(put by duct-map.ducts.agent) bone hen)
      ::
      %=  ap-state
        ost        bone.ducts.agent
        ducts.sat  opaque-ducts
      ==
    ::
    ::  +ap-abet: resolve moves.
    ::
    ++  ap-abet
      ^+  mo-state
      ::
      =>  ap-track-queue
      ::
      =/  running  (~(put by running.ship-state.gall) dap sat)
      ::
      =/  moves
        =/  giver  |=(report=(each suss tang) [hen %give %onto report])
        =/  from-internal  (turn zip ap-from-internal)
        =/  from-suss  (turn dub giver)
        :(weld from-internal from-suss moves)
      ::
      %_  mo-state
        running.ship-state.gall  running
        moves                    moves
      ==
    ::
    ::  +ap-track-queue: track queue.
    ::
    ++  ap-track-queue
      ^+  ap-state
      ::
      =/  internal-moves  zip
      =/  bones  *(set bone)
      ::
      |-  ^+  ap-state
      ::
      ?^  internal-moves
        ?.  ?=([%give %diff *] move.i.internal-moves)
          $(internal-moves t.internal-moves)
        ::
        =/  =internal-move  i.internal-moves
        =^  filled  ap-state  ap-enqueue(ost bone.internal-move)
        ::
        =/  new-bones
          ?:  filled
            bones
          (~(put in bones) bone.internal-move)
        ::
        $(internal-moves t.internal-moves, bones new-bones)
      ::
      =/  boned  ~(tap in bones)
      ::
      |-  ^+  ap-state
      ::
      ?~  boned
        ap-state
      ::
      =>  $(boned t.boned, ost i.boned)
      ::
      =/  tib  (~(get by incoming.subscribers.sat) ost)
      ::
      ?~  tib
        ~&  [%ap-track-queue-bad-bone dap ost]
        ap-state
      ::
      ap-kill(attributing.routes.pry p.u.tib)
    ::
    ::  +ap-from-internal: internal move to move.
    ::
    ++  ap-from-internal
      ~/  %ap-from-internal
      |=  =internal-move
      ^-  move
      ::
      ~|  [%gall-from-internal-failed internal-move]
      ::
      =/  =duct  (~(got by duct-map.ducts.sat) bone.internal-move)
      ::
      =/  card
        ?-    -.move.internal-move
            ::
            %slip  !!
            ::
            %give
            ::
          ?<  =(0 bone.internal-move)
          ::
          =/  =internal-gift  p.move.internal-move
          ?.  ?=(%diff -.internal-gift)
            [%give %unto internal-gift]
          ::
          =/  =cage  p.internal-gift
          =/  =mark
            =/  trans  (~(get by required-trans.sat) bone.internal-move)
            (fall trans p.cage)
          ::
          ?:  =(mark p.cage)
            [%give %unto internal-gift]
          ::
          =/  =path  /sys/pel/[dap]
          ::
          [%pass path note-arvo]
            ::
            %pass
            ::
          =/  =path  p.move.internal-move
          =/  =internal-note  q.move.internal-move
          ::
          =/  use-path  [%use dap path]
          ::
          =/  =note-arvo
            ?-  task.internal-note
                ::
                ::
                %send
                ::
              =/  =task:able
                =/  =sock  [our ship.internal-note]
                =/  =internal-task  internal-task.internal-note
                [%deal sock internal-task]
              [%g task]
                ::
                %meta
                ::
              =/  =term  term.internal-note
              =/  =vase  vase.internal-note
              [term %meta vase]
            ==
          ::
          [%pass use-path note-arvo]
        ==
      ::
      [duct card]
    ::
    ::  +ap-call: call into server.
    ::
    ++  ap-call
      ~/  %ap-call
      |=  [=term =vase]
      ^-  [(unit tang) _ap-state]
      ::
      =.  ap-state  ap-construct-bowl
      =^  arm  ap-state  (ap-produce-arm term)
      ::
      ?:  ?=(%.n -.arm)
        [(some p.arm) ap-state]
      ::
      =^  arm  ap-state  (ap-slam term p.arm vase)
      ::
      ?:  ?=(%.n -.arm)
        [(some p.arm) ap-state]
      (ap-handle-result p.arm)
    ::
    ::  +ap-peek: peek.
    ::
    ++  ap-peek
      ~/  %ap-peek
      |=  [=term tyl=path]
      ^-  (unit (unit cage))
      ::
      =/  marked
        ?.  ?=(%x term)
          [mark=%$ tyl=tyl]
        ::
        =/  =path  (flop tyl)
        ::
        ?>  ?=(^ path)
        [mark=i.path tyl=(flop t.path)]
      ::
      =/  =mark  mark.marked
      =/  tyl  tyl.marked
      ::
      =^  maybe-arm  ap-state  (ap-find-arm %peek term tyl)
      ::
      ?~  maybe-arm
        =/  =tank  [%leaf "peek find fail"]
        =/  print  (slog tank >tyl< >mark< ~)
        (print [~ ~])
      ::
      =^  arm  ap-state  (ap-produce-arm q.u.maybe-arm)
      ::
      ?:  ?=(%.n -.arm)
        =/  =tank  [%leaf "peek farm fail"]
        =/  print  (slog tank p.arm)
        (print [~ ~])
      ::
      =/  slammed
        =/  index  p.u.maybe-arm
        =/  term  q.u.maybe-arm
        =/  =vase
          =/  =path  [term tyl]
          =/  raw  (slag index path)
          !>  raw
        (ap-slam term p.arm vase)
      ::
      =^  possibly-vase  ap-state  slammed
      ::
      ?:  ?=(%.n -.possibly-vase)
        =/  =tank  [%leaf "peek slam fail"]
        =/  print  (slog tank p.possibly-vase)
        (print [~ ~])
      ::
      =/  slammed-vase  p.possibly-vase
      =/  vase-value  q.slammed-vase
      ::
      =/  err
        =/  =tank  [%leaf "peek bad result"]
        =/  print  (slog tank ~)
        (print [~ ~])
      ::
      ?+  vase-value  err
          ::
          ~
          ::
        ~
          ::
          [~ ~]
          ::
        [~ ~]
          ::
          [~ ~ ^]
          ::
        =/  =vase  (sped (slot 7 slammed-vase))
        ::
        ?.  ?=([p=@ *] q.vase)
          =/  =tank  [%leaf "scry: malformed cage"]
          =/  print  (slog tank ~)
          (print [~ ~])
        ::
        ?.  ((sane %as) p.q.vase)
          =/  =tank  [%leaf "scry: malformed cage"]
          =/  print  (slog tank ~)
          (print [~ ~])
        ::
        ?.  =(mark p.q.vase)
          [~ ~]
        ::
        =/  =cage  [p.q.vase (slot 3 vase)]
        (some (some cage))
      ==
    ::
    ::  +ap-apply: apply effect.
    ::
    ++  ap-apply
      |=  =agent-action
      ^+  ap-state
      ::
      ?-  -.agent-action
        %peel       (ap-peel +.agent-action)
        %poke       (ap-poke +.agent-action)
        %peer       (ap-peer +.agent-action)
        %puff       !!
        %punk       !!
        %peer-not   !!
        %pull       ap-load-delete
        %pump       ap-dequeue
      ==
    ::
    ::  +ap-diff: pour a diff.
    ::
    ++  ap-diff
      ~/  %ap-diff
      |=  [=ship =path =cage]
      ^+  ap-state
      ::
      =/  rest  +.path
      =/  diff  [%diff p.cage rest]
      ::
      =^  maybe-arm  ap-state  (ap-find-arm diff)
      ::
      ?~  maybe-arm
        =/  target  [%.n ship rest]
        ::
        =/  =tang
          =/  why  "diff: no {<[p.cage rest]>}"
          (ap-tang why)
        ::
        =/  lame  (ap-lame %diff tang)
        (ap-update-subscription:lame target)
      ::
      =/  arm  u.maybe-arm
      ::
      =/  =vase
        =/  target
          ?:  =(0 p.arm)
            =/  =vase  (ap-cage cage)
            [!>(rest) vase]
          [!>((slag (dec p.arm) rest)) q.cage]
        (slop target)
      ::
      =^  called  ap-state  (ap-call q.arm vase)
      ::
      ?^  called
        =/  lame  (ap-lame q.arm u.called)
        (ap-update-subscription:lame %.n ship path)
      (ap-update-subscription %.y ship path)
    ::
    ::  +ap-cage: cage to tagged vase.
    ::
    ++  ap-cage
      |=  =cage
      ^-  vase
      ::
      =/  =type  [%atom %tas (some p.cage)]
      =/  =vase  [type p.cage]
      (slop vase q.cage)
    ::
    ::  +ap-update-subscription: update subscription.
    ::
    ++  ap-update-subscription
      ~/  %ap-update-subscription
      |=  [is-ok=? =ship =path]
      ^+  ap-state
      ::
      =/  way  [(scot %p ship) %out path]
      ::
      ?:  is-ok
        =/  =internal-note  [%send ship -.path %pump ~]
        (ap-pass way internal-note)
      ::
      =/  give  (ap-give %quit ~)
      =/  =internal-note  [%send ship -.path %pull ~]
      (ap-pass:give way internal-note)
    ::
    ::  +ap-dequeue: drop from queue.
    ::
    ++  ap-dequeue
      ^+  ap-state
      ::
      ?.  (~(has by incoming.subscribers.sat) ost)
        ap-state
      ::
      =/  level  (~(get by meter.subscribers.sat) ost)
      ::
      ?:  |(?=(~ level) =(0 u.level))
        ap-state
      ::
      =.  u.level  (dec u.level)
      ::
      ?:  =(0 u.level)
        =/  deleted  (~(del by meter.subscribers.sat) ost)
        %_  ap-state
          meter.subscribers.sat  deleted
        ==
      ::
      =/  dropped  (~(put by meter.subscribers.sat) ost u.level)
      %_  ap-state
        meter.subscribers.sat  dropped
      ==
    ::
    ::  +ap-produce-arm: produce arm.
    ::
    ++  ap-produce-arm
      ~/  %ap-produce-arm
      |=  =term
      ^-  [(each vase tang) _ap-state]
      ::
      =/  compiled
        =/  =type  p.running-state.sat
        =/  =hoon  [%limb term]
        (~(mint wa cache.sat) type hoon)
      ::
      =/  virtual
        =/  trap  |.(compiled)
        (mule trap)
      ::
      ?:  ?=(%.n -.virtual)
        =/  =tang  p.virtual
        [[%.n tang] ap-state]
      ::
      =/  possibly-vase=(each vase tang)
        =/  value  q.running-state.sat
        =/  ton  (mock [value q.+<.virtual] ap-namespace-view)
        ?-  -.ton
          %0  [%.y p.+<.virtual p.ton]
          %1  [%.n (turn p.ton |=(a=* (smyt (path a))))]
          %2  [%.n p.ton]
        ==
      ::
      =/  next
        =/  =worm  +>.virtual
        %_  ap-state
          cache.sat  worm
        ==
      ::
      [possibly-vase next]
    ::
    ::  +ap-enqueue: add to queue.
    ::
    ++  ap-enqueue
      ^-  [? _ap-state]
      ::
      =/  meter  (~(gut by meter.subscribers.sat) ost 0)
      =/  subscriber=(unit (pair ship path))
        (~(get by incoming.subscribers.sat) ost)
      ::
      =/  incoming  (~(get by incoming.subscribers.sat) ost)
      =/  duct  (~(get by duct-map.ducts.sat) ost)
      ::
      ?:  ?&  =(20 meter)
              ?|  ?=(~ subscriber)
                  !=(our p.u.subscriber)
              ==
          ==
        ~&  [%gall-pulling-20 ost incoming duct]
        [%.n ap-state]
      ::
      =/  next
        =/  meter  (~(put by meter.subscribers.sat) ost +(meter))
        %_  ap-state
          meter.subscribers.sat  meter
        ==
      ::
      [%.y next]
    ::
    ::  +ap-find-arm: general arm.
    ::
    ++  ap-find-arm
      ~/  %ap-find-arm
      |=  [=term =path]
      ^-  [(unit (pair @ud @tas)) _ap-state]
      ::
      ::
      =/  maybe-cached  (~(get by arm-cache.sat) [term path])
      ?^  maybe-cached
        [u.maybe-cached ap-state]
      ::
      =/  result
        =/  dep  0
        |-  ^-  (unit (pair @ud @tas))
        =/  spu
          ?~  path
            ~
          =/  hyped  (cat 3 term (cat 3 '-' i.path))
          $(path t.path, dep +(dep), term hyped)
        ::
        ?^  spu
          spu
        ::
        ?.  (ap-exists-arm term)
          ~
        (some [dep term])
      ::
      =.  arm-cache.sat  (~(put by arm-cache.sat) [term path] result)
      ::
      [result ap-state]
    ::
    ::  +ap-exists-arm: check for arm.
    ::
    ++  ap-exists-arm
      ~/  %ap-exists-arm
      |=  =term
      ^-  ?
      ::
      =/  =type  p.running-state.sat
      (slob term type)
    ::
    ::  +ap-give: return result.
    ::
    ++  ap-give
      |=  =internal-gift
      ^+  ap-state
      ::
      =/  internal-moves
        =/  move  [%give internal-gift]
        =/  =internal-move  [ost move]
        [internal-move zip]
      ::
      ap-state(zip internal-moves)
    ::
    ::  +ap-construct-bowl: set up bowl.
    ::
    ++  ap-construct-bowl
      ^+  ap-state
      ::
      %_    ap-state
          +12.q.running-state.sat
        ^-   bowl
        :*  :*  our                               ::  host
                attributing.routes.pry            ::  guest
                dap                               ::  agent
            ==                                    ::
            :*  wex=~                             ::  outgoing
                sup=incoming.subscribers.sat      ::  incoming
            ==                                    ::
            :*  ost=ost                           ::  cause
                act=change.stats.sat              ::  tick
                eny=eny.stats.sat                 ::  nonce
                now=time.stats.sat                ::  time
                byk=beak.sat                      ::  source
        ==  ==                                    ::
      ==
    ::
    ::  +ap-move: process each move.
    ::
    ++  ap-move
      ~/  %ap-move
      |=  =vase
      ^-  [(each internal-move tang) _ap-state]
      ::
      =/  noun  q.vase
      ::
      ?@  noun
        =/  =tang  (ap-tang "move: invalid move (atom)")
        [[%.n tang] ap-state]
      ::
      ?^  -.noun
        =/  =tang  (ap-tang "move: invalid move (bone)")
        [[%.n tang] ap-state]
      ::
      ?@  +.noun
        =/  =tang  (ap-tang "move: invalid move (card)")
        [[%.n tang] ap-state]
      ::
      =/  =bone  -.noun
      =/  has-duct  (~(has by duct-map.ducts.sat) bone)
      ::
      ?.  &(has-duct !=(0 bone))
        =/  =tang  (ap-tang "move: invalid card (bone {<bone>})")
        [[%.n tang] ap-state]
      ::
      =^  vase  cache.sat  (~(spot wa cache.sat) 3 vase)
      =^  vase  cache.sat  (~(slot wa cache.sat) 3 vase)
      ::
      ?+  +<.noun  (ap-move-pass bone +<.noun vase)
        %diff  (ap-move-diff bone vase)
        %hiss  (ap-move-hiss bone vase)
        %peel  (ap-move-peel bone vase)
        %peer  (ap-move-peer bone vase)
        %pull  (ap-move-pull bone vase)
        %poke  (ap-move-poke bone vase)
        %send  (ap-move-send bone vase)
        %quit  (ap-move-quit bone vase)
        %http-response  (ap-move-http-response -.q.vax cav)
      ==
    ::
    ::  +ap-move-quit: give quit move.
    ::
    ++  ap-move-quit
      ~/  %quit
      |=  [=bone =vase]
      ^-  [(each internal-move tang) _ap-state]
      ::
      =/  possibly-internal-move=(each internal-move tang)
        ?^  q.vase
          =/  =tang  (ap-tang "quit: improper give")
          [%.n tang]
        ::
        =/  =internal-move
          =/  =internal-gift  [%quit ~]
          =/  move  [%give internal-gift]
          [bone move]
        ::
        [%.y internal-move]
      ::
      =/  next
        =/  incoming  (~(del by incoming.subscribers.sat) bone)
        %_  ap-state
          incoming.subscribers.sat  incoming
        ==
      ::
      [possibly-internal-move next]
    ::
    ::  +ap-move-diff: give diff move.
    ::
    ++  ap-move-diff
      ~/  %diff
      |=  [=bone =vase]
      ^-  [(each internal-move tang) _ap-state]
      ::
      =^  vase  cache.sat  (~(sped wa cache.sat) vase)
      ::
      =/  value  q.vase
      ::
      ?.  ?&  ?=(^ value)
              ?=(@ -.value)
              ((sane %tas) -.value)
          ==
        =/  =tang  (ap-tang "diff: improper give")
        [[%.n tang] ap-state]
      ::
      =^  vase  cache.sat  (~(slot wa cache.sat) 3 vase)
      ::
      =/  =internal-move
        =/  =cage  [-.value vase]
        =/  move  [%give %diff cage]
        [bone move]
      ::
      [[%.y internal-move] ap-state]
    ::
    ::  +ap-move-http-response
    ::
    ++  ap-move-http-response
      |=  [sto=bone vax=vase]
      ^-  [(each cove tang) _ap-state]
      ::
      ::  TODO: Magic vase validation. I have no idea how malformed checking
      ::  works.
      ::
      :_  ap-state
      [%& sto %give %http-response ;;(http-event:http q.vax)]
    ::
    ::
    ::  +ap-move-mess: extract path, target.
    ::
    ++  ap-move-mess
      ~/  %mess
      |=  =vase
      ^-  [(each (trel path ship term) tang) _ap-state]
      ::
      =/  possibly-trel=(each (trel path ship term) tang)
        ?.  ?&  ?=([p=* [q=@ r=@] s=*] q.vase)
                (gte 1 (met 7 q.q.vase))
            ==
          =/  =tang  (ap-tang "mess: malformed target")
          [%.n tang]
        ::
        =/  pax  ((soft path) p.q.vase)
        ::
        ?.  ?&  ?=(^ pax)
                (levy u.pax (sane %ta))
            ==
          =/  =tang  (ap-tang "mess: malformed path")
          [%.n tang]
        ::
        =/  =path  [(scot %p q.q.vase) %out r.q.vase u.pax]
        =/  =ship  q.q.vase
        =/  =term  r.q.vase
        [%.y path ship term]
      ::
      [possibly-trel ap-state]
    ::
    ::  +ap-move-pass: pass general move.
    ::
    ++  ap-move-pass
      ~/  %pass
      |=  [=bone =noun =vase]
      ^-  [(each internal-move tang) _ap-state]
      ::
      ?.  ?&  ?=(@ noun)
              ((sane %tas) noun)
          ==
        =/  =tang  (ap-tang "pass: malformed card")
        [[%.n tang] ap-state]
      ::
      =/  pax  ((soft path) -.q.vase)
      ::
      ?.  ?&  ?=(^ pax)
              (levy u.pax (sane %ta))
          ==
        =/  =tang  (ap-suck "pass: malformed path")
        ~&  [%bad-path pux]
        [[%.n tang] ap-state]
      ::
      =/  maybe-vane  (ap-vain noun)
      ::
      ?~  maybe-vane
        =/  =tang  (ap-tang "move: unknown note {(trip noun)}")
        [[%.n tang] ap-state]
      ::
      =/  vane  u.maybe-vane
      ::
      =^  at-slot  cache.sat  (~(slot wa cache.sat) 3 vase)
      ::
      =/  =internal-move
        =/  =path  [(scot %p attributing.routes.pry) %inn u.pax]
        =/  vase  (ap-atomic-vase %tas noun)
        =/  combined  (slop vase at-slot)
        =/  =internal-note  [%meta vane combined]
        =/  card  [%pass path internal-note]
        [bone card]
      ::
      [[%.y internal-move] ap-state]
    ::
    ::  +ap-move-poke: pass %poke.
    ::
    ++  ap-move-poke
      ~/  %poke
      |=  [=bone =vase]
      ^-  [(each internal-move tang) _ap-state]
      ::
      =^  possibly-target  ap-state  (ap-move-mess vase)
      ::
      ?:  ?=(%.n -.possibly-target)
        [possibly-target ap-state]
      ::
      =^  at-slot  cache.sat  (~(slot wa cache.sat) 7 vase)
      ::
      ?.  ?&  ?=([p=@ q=*] q.at-slot)
              ((sane %tas) p.q.at-slot)
          ==
        =/  =tang  (ap-tang "poke: malformed cage")
        [[%.n tang] ap-state]
      ::
      =^  specialised  cache.sat  (~(stop wa cache.sat) 3 at-slot)
      ::
      =/  target  p.possibly-target
      =/  =path  p.target
      =/  =ship  q.target
      =/  =term  r.target
      ::
      =/  =internal-move
        =/  =internal-task  [term %poke p.q.at-slot specialised]
        =/  =internal-note  [%send ship internal-task]
        =/  card  [%pass path internal-note]
        [bone card]
      ::
      [[%.y internal-move] ap-state]
    ::
    ::  +ap-move-peel: pass %peel.
    ::
    ++  ap-move-peel
      ~/  %peel
      |=  [=bone =vase]
      ^-  [(each internal-move tang) _ap-state]
      ::
      =^  possibly-target  ap-state  (ap-move-mess vase)
      ::
      ?:  ?=(%.n -.possibly-target)
        [possibly-target ap-state]
      ::
      =/  target  p.possibly-target
      =/  =ship  q.target
      =/  =term  r.target
      ::
      =/  mark  ((soft mark) +>-.q.vase)
      ::
      ?~  mark
        =/  =tang  (ap-tang "peel: malformed mark")
        [[%.n tang] ap-state]
      ::
      =/  pax  ((soft path) +>+.q.vase)
      ::
      ?.  ?&  ?=(^ pax)
              (levy u.pax (sane %ta))
          ==
        =/  =tang  (ap-tang "peel: malformed path")
        [[%.n tang] ap-state]
      ::
      =/  move
        ?:  (~(has in misvale.sat) p.target)
          =/  =internal-task
            =/  =tang  [[%leaf "peel: misvalidation encountered"] ~]
            =/  =agent-action  [%peer-not tang]
            [term agent-action]
          ::
          =/  =internal-note  [%send ship internal-task]
          =/  card  [%pass p.target internal-note]
          [bone card]
        ::
        =/  =agent-action  [%peel u.mark u.pax]
        =/  =internal-task  [term agent-action]
        =/  =internal-note  [%send ship internal-task]
        =/  card  [%pass p.target internal-note]
        [bone card]
      ::
      [[%.y move] ap-state]
    ::
    ::  +ap-move-peer: pass %peer.
    ::
    ++  ap-move-peer
      ~/  %peer
      |=  [=bone =vase]
      ^-  [(each internal-move tang) _ap-state]
      ::
      =^  possibly-target  ap-state  (ap-move-mess vase)
      ::
      ?:  ?=(%.n -.possibly-target)
        [possibly-target ap-state]
      ::
      =/  target  p.possibly-target
      =/  =ship  q.target
      =/  =term  r.target
      ::
      =/  pax  ((soft path) +>.q.vase)
      ::
      ?.  ?&  ?=(^ pax)
              (levy u.pax (sane %ta))
          ==
        =/  =tang  (ap-tang "peer: malformed path")
        [[%.n tang] ap-state]
      ::
      =/  move
        ?:  (~(has in misvale.sat) p.target)
          =/  err  [[%leaf "peer: misvalidation encountered"] ~]
          =/  =agent-action  [%peer-not err]
          =/  =internal-note  [%send ship term agent-action]
          =/  card  [%pass p.target internal-note]
          [bone card]
        ::
        =/  =agent-action  [%peer u.pax]
        =/  =internal-note  [%send ship term agent-action]
        =/  card  [%pass p.target internal-note]
        [bone card]
      ::
      [[%.y move] ap-state]
    ::
    ::  +ap-move-pull: pass %pull.
    ::
    ++  ap-move-pull
      ~/  %pull
      |=  [=bone =vase]
      ^-  [(each internal-move tang) _ap-state]
      ::
      =^  possibly-target  ap-state  (ap-move-mess vase)
      ::
      ?:  ?=(%.n -.possibly-target)
        [possibly-target ap-state]
      ::
      =/  target  p.possibly-target
      =/  =ship  q.target
      =/  =term  r.target
      ::
      ?.  =(~ +>.q.vase)
        =/  =tang  (ap-tang "pull: malformed card")
        [[%.n tang] ap-state]
      ::
      =/  move
        =/  =agent-action  [%pull ~]
        =/  =internal-note  [%send ship term agent-action]
        =/  card  [%pass p.target internal-note]
        [bone card]
      ::
      [[%.y move] ap-state]
    ::
    ::  +ap-move-send: pass gall action.
    ::
    ++  ap-move-send
      ~/  %send
      |=  [=bone =vase]
      ^-  [(each internal-move tang) _ap-state]
      ::
      ?.  ?&  ?=([p=* [q=@ r=@] [s=@ t=*]] q.vase)
              (gte 1 (met 7 q.q.vase))
              ((sane %tas) r.q.vase)
          ==
        =/  =tang  (ap-tang "send: improper ask.[%send wire gill agent-action]")
        [[%.n tang] ap-state]
      ::
      =/  pax  ((soft path) p.q.vase)
      ::
      ?.  ?&  ?=(^ pax)
              (levy u.pax (sane %ta))
          ==
        =/  =tang  (ap-tang "send: malformed path")
        [[%.n tang] ap-state]
      ::
      ?:  ?=($poke s.q.vase)
        ::
        =^  specialised  cache.sat  (~(spot wa cache.sat) 7 vase)
        ::
        ?>  =(%poke -.q.specialised)
        ::
        ?.  ?&  ?=([p=@ q=*] t.q.vase)
                ((sane %tas) p.t.q.vase)
            ==
          =/  =tang  (ap-tang "send: malformed poke")
          [[%.n tang] ap-state]
        ::
        =^  specialised  cache.sat  (~(spot wa cache.sat) 3 specialised)
        =^  at-slot  cache.sat  (~(slot wa cache.sat) 3 specialised)
        ::
        =/  move
          =/  =agent-action  [%poke p.t.q.vase at-slot]
          =/  =internal-note  [%send q.q.vase r.q.vase agent-action]
          =/  =path  [(scot %p q.q.vase) %out r.q.vase u.pax]
          =/  card  [%pass path internal-note]
          [bone card]
        ::
        [[%.y move] ap-state]
      ::
      =/  maybe-action  ((soft agent-action) [s t]:q.vase)
      ?~  maybe-action
        =/  =tang  (ap-tang "send: malformed agent-action")
        [[%.n tang] ap-state]
      ::
      =/  move
        =/  =agent-action  u.maybe-action
        =/  =internal-note  [%send q.q.vase r.q.vase agent-action]
        =/  =path  [(scot %p q.q.vase) %out r.q.vase u.pax]
        =/  card  [%pass path internal-note]
        [bone card]
      ::
      [[%.y move] ap-state]
    ::
    ::  +ap-pass: request action.
    ::
    ++  ap-pass
      |=  [=path =internal-note]
      ^+  ap-state
      ::
      =/  =internal-move
        =/  move  [%pass path internal-note]
        [ost move]
      ::
      =/  internal-moves  [internal-move zip]
      ::
      ap-state(zip internal-moves)
    ::
    ::  +ap-reinstall: reinstall.
    ::
    ++  ap-reinstall
      ~/  %ap-reinstall
      |=  =vase
      ^+  ap-state
      ::
      =/  prep
        =/  installed  ap-install(running-state.sat vase)
        =/  running  (some running-state.sat)
        (installed running)
      ::
      =^  maybe-tang  ap-state  prep
      ::
      ?~  maybe-tang
        ap-state
      (ap-lame %prep-failed u.maybe-tang)
    ::
    ::  +ap-peel: apply %peel.
    ::
    ++  ap-peel
      |=  [=mark =path]
      ^+  ap-state
      ::
      =.  required-trans.sat  (~(put by required-trans.sat) ost mark)
      ::
      (ap-peer path)
    ::
    ::  +ap-peer: apply %peer.
    ::
    ++  ap-peer
      ~/  %ap-peer
      |=  pax=path
      ^+  ap-state
      ::
      =/  incoming  [attributing.routes.pry pax]
      ::
      =.  incoming.subscribers.sat
        (~(put by incoming.subscribers.sat) ost incoming)
      ::
      =^  maybe-arm  ap-state  (ap-find-arm %peer pax)
      ::
      ?~  maybe-arm
        ap-state
      ::
      =/  arm  u.maybe-arm
      =/  =vase  !>((slag p.arm pax))
      =/  old  zip
      ::
      =.  zip  ~
      =^  maybe-tang  ap-state  (ap-call q.arm vase)
      ::
      =/  internal-moves=(list internal-move)
        =/  move  [ost %give %reap maybe-tang]
        [move old]
      ::
      =.  zip  (weld zip internal-moves)
      ::
      ?^  maybe-tang
        ap-silent-delete
      ap-state
    ::
    ::  +ap-poke: apply %poke.
    ::
    ++  ap-poke
      ~/  %ap-poke
      |=  =cage
      ^+  ap-state
      ::
      =^  maybe-arm  ap-state  (ap-find-arm %poke p.cage ~)
      ::
      ?~  maybe-arm
        =/  =tang  (ap-tang "no poke arm for {(trip p.cage)}")
        (ap-give %coup (some tang))
      ::
      =/  arm  u.maybe-arm
      ::
      =/  =vase
        =/  vas  (ap-atomic-vase %tas p.cage)
        ?.  =(0 p.arm)
          q.cage
        (slop vas q.cage)
      ::
      =^  tur  ap-state  (ap-call q.arm vase)
      (ap-give %coup tur)
    ::
    ::  +ap-lame: pour error.
    ::
    ++  ap-lame
      |=  [=term =tang]
      ^+  ap-state
      ::
      =^  maybe-arm  ap-state  (ap-find-arm /lame)
      ::
      =/  form  |=(=tank [%rose [~ "! " ~] tank ~])
      ::
      ?~  maybe-arm
        =/  tang  [>%ap-lame dap term< (turn tang form)]
        ~>  %slog.`rose+["  " "[" "]"]^(flop tang)
        ap-state
      ::
      =/  arm  u.maybe-arm
      =/  =vase  !>([term tang])
      ::
      =^  maybe-tang  ap-state  (ap-call q.arm vase)
      ::
      ?^  maybe-tang
        =/  tang  u.maybe-tang
        =/  etc  (flop [>%ap-lame-lame< (turn tang form)])
        ~>  %slog.`rose+["  " "[" "]"]^(welp etc [%leaf "." (flop tang)])
        ap-state
      ::
      ap-state
    ::
    ::  +ap-misvale: broken vale.
    ::
    ++  ap-misvale
      |=  =wire
      ^+  ap-state
      ::
      ~&  [%ap-blocking-misvale wire]
      =/  misvaled  (~(put in misvale.sat) wire)
      ap-state(misvale.sat misvaled)
    ::
    ::  +ap-generic-take: generic take.
    ::
    ++  ap-generic-take
      ~/  %ap-generic-take
      |=  [=path =vase]
      ^+  ap-state
      ::
      ?.  &(?=([@ *] q.vase) ((sane %tas) -.q.vase))
        =/  =tang  (ap-tang "pour: malformed card")
        (ap-lame %pour tang)
      ::
      =/  =term  -.q.vase
      ::
      =^  maybe-arm  ap-state  (ap-find-arm [term path])
      ::
      ?~  maybe-arm
        =/  =tang  (ap-tang "pour: no {(trip -.q.vase)}: {<path>}")
        (ap-lame term tang)
      ::
      =/  arm  u.maybe-arm
      ::
      =^  at-slot  cache.sat  (~(slot wa cache.sat) 3 vase)
      ::
      =/  vase  (slop !>((slag p.arm path)) at-slot)
      ::
      =^  maybe-tang  ap-state  (ap-call q.arm vase)
      ::
      ?^  maybe-tang
        (ap-lame term u.maybe-tang)
      ap-state
    ::
    ::  +ap-unwrap-take: unwrap take.
    ::
    ++  ap-unwrap-take
      ~/  %ap-unwrap-take
      |=  [=term pax=path =cage]
      ^+  ap-state
      ::
      =^  maybe-arm  ap-state  (ap-find-arm [term p.cage pax])
      ::
      ?~  maybe-arm
        =/  =tang  (ap-tang "{(trip term)}: no {<`path`[p.cage pax]>}")
        (ap-lame term tang)
      ::
      =/  arm  u.maybe-arm
      ::
      =/  =vase
        %-  slop
        ?:  =(0 p.arm)
          =/  =vase  (ap-cage cage)
          [!>(`path`pax) vase]
        [!>((slag (dec p.arm) `path`pax)) q.cage]
      ::
      =^  maybe-tang  ap-state  (ap-call q.arm vase)
      ::
      ?^  maybe-tang
        (ap-lame q.arm u.maybe-tang)
      ap-state
    ::
    ::  +ap-specific-take: specific take.
    ::
    ++  ap-specific-take
      |=  [=path =internal-gift]
      ^+  ap-state
      ::
      ?-  -.internal-gift
        %coup  (ap-non-diff-take %coup +.path (some !>(p.internal-gift)))
        %diff  (ap-diff attributing.routes.pry path p.internal-gift)
        %quit  (ap-non-diff-take %quit +.path ~)
        %reap  (ap-non-diff-take %reap +.path (some !>(p.internal-gift)))
        %http-response  !!
      ==
    ::
    ::  +ap-install: install wrapper.
    ::
    ++  ap-install
      |=  maybe-vase=(unit vase)
      ^-  [(unit tang) _ap-state]
      ::
      =^  maybe-tang  ap-state  (ap-prep maybe-vase)
      ::
      =/  new-misvale-data
         ~?  !=(misvale.sat *misvale-data)
           [%misvale-drop misvale.sat]
         :: new app might mean new marks
         *misvale-data
      ::
      =/  new-dub
        =/  =term  ?~(maybe-vase %boot %bump)
        ::
        =/  possibly-suss
          ?~  maybe-tang
            =/  =suss  [dap term now]
            [%.y suss]
          [%.n u.maybe-tang]
        ::
        [possibly-suss dub]
      ::
      =/  next
        %=  ap-state
          misvale.sat     new-misvale-data
          dub             new-dub
          arm-cache.sat   ~
        ==
      ::
      [maybe-tang next]
    ::
    ::  +ap-prep: low-level install.
    ::
    ++  ap-prep
      ~/  %ap-prep
      |=  maybe-vase=(unit vase)
      ^-  [(unit tang) _ap-state]
      ::
      ?.  (ap-exists-arm %prep)
        ?~  maybe-vase
          [~ ap-state]
        ::
        =/  new-type
          =/  new  (slot 13 running-state.sat)
          p.new
        ::
        =/  old-type
          =/  old  (slot 13 u.maybe-vase)
          p.old
        ::
        ?.  (~(nest ut new-type) %.n old-type)
          =/  =tang  (ap-tang "prep mismatch")
          [(some tang) ap-state]
        ::
        =/  next  ap-state(+13.q.running-state.sat +13.q.u.maybe-vase)
        [~ next]
      ::
      =/  =vase
        ?~  maybe-vase
          !>(~)
        (slop !>(~) (slot 13 u.maybe-vase))
      ::
      (ap-call %prep vase)
    ::
    ::  +ap-silent-delete: silent delete.
    ::
    ++  ap-silent-delete
      ^+  ap-state
      ::
      =/  incoming  (~(get by incoming.subscribers.sat) ost)
      ?~  incoming
        ap-state
      ::
      %_  ap-state
        incoming.subscribers.sat  (~(del by incoming.subscribers.sat) ost)
        meter.subscribers.sat     (~(del by meter.subscribers.sat) ost)
      ==
    ::
    ::  +ap-load-delete: load delete.
    ::
    ++  ap-load-delete
      ^+  ap-state
      ::
      =/  maybe-incoming  (~(get by incoming.subscribers.sat) ost)
      ?~  maybe-incoming
        ap-state
      ::
      =/  incoming  u.maybe-incoming
      ::
      =:  incoming.subscribers.sat  (~(del by incoming.subscribers.sat) ost)
          meter.subscribers.sat     (~(del by meter.subscribers.sat) ost)
      ==
      ::
      =^  maybe-arm  ap-state  (ap-find-arm %pull q.incoming)
      ::
      ?~  maybe-arm
        ap-state
      ::
      =/  arm  u.maybe-arm
      =/  =vase  !>((slag p.arm q.incoming))
      ::
      =^  maybe-tang  ap-state  (ap-call q.arm vase)
      ::
      ?^  maybe-tang
        (ap-lame q.arm u.maybe-tang)
      ap-state
    ::
    ::  +ap-kill: queue kill.
    ::
    ++  ap-kill
      ^+  ap-state
      ::
      (ap-give:ap-load-delete %quit ~)
    ::
    ::  +ap-non-diff-take: non-diff gall take.
    ::
    ++  ap-non-diff-take
      ~/  %ap-non-diff-take
      |=  [=term =path maybe-vase=(unit vase)]
      ^+  ap-state
      ::
      =^  maybe-arm  ap-state  (ap-find-arm term path)
      ::
      ?~  maybe-arm
        ap-state
      ::
      =/  arm  u.maybe-arm
      =/  =vase
        =/  vax  !>((slag p.arm path))
        ?~  maybe-vase
          vax
        (slop vax u.maybe-vase)
      ::
      =^  maybe-tang  ap-state  (ap-call q.arm vase)
      ::
      ?^  maybe-tang
        (ap-lame q.arm u.maybe-tang)
      ap-state
    ::
    ::  +ap-safe: process move list.
    ::
    ++  ap-safe
      ~/  %ap-safe
      |=  =vase
      ^-  [(each (list internal-move) tang) _ap-state]
      ::
      ?~  q.vase
        [[%.y ~] ap-state]
      ::
      ?@  q.vase
        =/  =tang  (ap-tang "move: malformed list")
        [[%.n tang] ap-state]
      ::
      =^  hed  cache.sat  (~(slot wa cache.sat) 2 vase)
      =^  possibly-internal-move  ap-state  (ap-move hed)
      ::
      ?:  ?=(%.n -.possibly-internal-move)
        [possibly-internal-move ap-state]
      ::
      =/  =internal-move  p.possibly-internal-move
      ::
      =^  tel  cache.sat  (~(slot wa cache.sat) 3 vase)
      =^  res  ap-state  $(vase tel)
      ::
      =/  possibly-internal-moves
        ?:  ?=(%.n -.res)
          res
        [%.y [internal-move p.res]]
      ::
      [possibly-internal-moves ap-state]
    ::
    ::  +ap-handle-result: handle result.
    ::
    ++  ap-handle-result
      ~/  %ap-handle-result
      |=  =vase
      ^-  [(unit tang) _ap-state]
      ::
      ?:  ?=(@ q.vase)
        =/  =tang  (ap-tang "ap-handle-result: invalid product (atom)")
        [(some tang) ap-state]
      ::
      =^  hed  cache.sat  (~(slot wa cache.sat) 2 vase)
      =^  possibly-internal-moves  ap-state  (ap-safe hed)
      ::
      ?:  ?=(%.n -.possibly-internal-moves)
        =/  =tang  p.possibly-internal-moves
        [(some tang) ap-state]
      ::
      =/  internal-moves  p.possibly-internal-moves
      ::
      =^  tel  cache.sat  (~(slot wa cache.sat) 3 vase)
      =^  possibly-vase  ap-state  (ap-verify-core tel)
      ::
      ?:  ?=(%.n -.possibly-vase)
        =/  =tang  p.possibly-vase
        [(some tang) ap-state]
      ::
      =/  next
        %_  ap-state
          zip                (weld (flop internal-moves) zip)
          running-state.sat  p.possibly-vase
        ==
      ::
      [~ next]
    ::
    ::  +ap-verify-core: verify core.
    ::
    ++  ap-verify-core
      ~/  %ap-verify-core
      |=  vax=vase
      ^-  [(each vase tang) _ap-state]
      ::
      =/  received-type  p.vax
      =/  running-type  p.running-state.sat
      ::
      =^  nests  cache.sat  (~(nest wa cache.sat) running-type received-type)
      ::
      =/  possibly-vase
        ?.  nests
          =/  =tang  (ap-tang "invalid core")
          [%.n tang]
        [%.y vax]
      ::
      [possibly-vase ap-state]
    ::
    ::  +ap-slam: virtual slam.
    ::
    ++  ap-slam
      ~/  %ap-slam
      |=  [=term gat=vase arg=vase]
      ^-  [(each vase tang) _ap-state]
      ::
      =/  compiled
        =/  =type  [%cell p.gat p.arg]
        =/  =hoon  [%cnsg [%$ ~] [%$ 2] [%$ 3] ~]
        (~(mint wa cache.sat) type hoon)
      ::
      =/  virtual
        =/  trap  |.(compiled)
        (mule trap)
      ::
      ?:  ?=(%.n -.virtual)
        =/  =tang  (ap-tang "call: {<term>}: type mismatch")
        =/  sam  (~(peek ut p.gat) %free 6)
        =/  print
          (slog >%ap-slam-mismatch< ~(duck ut p.arg) ~(duck ut sam) ~)
        (print [[%.n tang] ap-state])
      ::
      =/  =worm  +>.virtual
      =/  =vase  +<.virtual
      =/  =type  p.vase
      =/  nock  q.vase
      =/  ton  (mock [[q.gat q.arg] nock] ap-namespace-view)
      ::
      =/  possibly-vase
        ?-  -.ton
          %0  [%.y type p.ton]
          %1  [%.n (turn p.ton |=(a=* (smyt (path a))))]
          %2  [%.n p.ton]
        ==
      ::
      =/  next  ap-state(cache.sat worm)
      [possibly-vase next]
    ::
    ::  +ap-namespace-view: namespace view.
    ::
    ++  ap-namespace-view  (sloy ska)
    ::
    ::  +ap-tang: standard tang.
    ::
    ++  ap-tang
      |=  =tape
      ^-  tang
      ::
      =/  =tank  [%leaf (weld "gall: {<dap>}: " tape)]
      [tank ~]
    ::
    ::  +ap-atomic-vase: atomic vase.
    ::
    ++  ap-atomic-vase
      |=  [=term =atom]
      ^-  vase
      ::
      =/  =type  [%atom term (some atom)]
      [type atom]
    ::
    ::  +ap-vain: card to vane.
    ::
    ++  ap-vain
      |=  =term
      ^-  (unit @tas)
      ::
      ?+  term  ~&  [%ap-vain term]
          ~
        %bonk   `%a
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
::
::  +call: request.
::
++  call
  ~%  %gall-call  +>   ~
  |=  [=duct hic=(hypo (hobo task:able))]
  ^-  [(list move) _gall-payload]
  ::
  ~|  [%gall-call-failed duct q.hic]
  ::
  ::  make sure our task is hard
  =/  =task:able
    ?.  ?=(%soft -.q.hic)
      q.hic
    ;;  task:able  p.q.hic
  ::
  =/  initialised  (mo-abed:mo duct)
  ::
  ?-    -.task
      ::
      %conf
      ::
    =/  =dock  p.task
    =/  =ship  p.dock
    ?.  =(our ship)
      ~&  [%gall-not-ours ship]
      [~ gall-payload]
    ::
    =>  (mo-boot:initialised q.dock q.task)
    mo-abet
      ::
      %deal
      ::
    =/  =sock  p.task
    =/  =internal-task  q.task
    ::
    ?.  =(q.sock our)
      ?>  =(p.sock our)
      =>  (mo-handle-foreign-request:initialised q.sock internal-task)
      mo-abet
    ::
    =>  (mo-handle-local:initialised p.sock internal-task)
    mo-abet
      ::
      %init
      ::
    =/  payload  gall-payload(system-duct.ship-state.gall duct)
    [~ payload]
      ::
      %sunk
      ::
    [~ gall-payload]
      ::
      %vega
      ::
    [~ gall-payload]
      ::
      %west
      ::
    =/  =ship  p.task
    =/  =path  q.task
    =/  =noun  r.task
    ::
    ?>  ?=([?(%ge %gh) @ ~] path)
    =/  dap  i.t.path
    ::
    ?:  ?=(%ge i.path)
      =/  mes  ;;((pair @ud forward-ames) noun)
      =>  (mo-handle-forward:initialised ship dap mes)
      mo-abet
    ::
    =/  mes  ;;((pair @ud reverse-ames) noun)
    =>  (mo-handle-backward:initialised ship dap mes)
    mo-abet
  ::
      %wash
    =.  running.ship-state.gall  (~(run by running.ship-state.gall) |=(=agent agent(cache *worm)))
    [~ gall-payload]
      ::
      %wegh
      ::
    =/  waiting
      =/  queued  (~(run by waiting.ship-state.gall) |=(blocked [%.y +<]))
      (sort ~(tap by queued) aor)
    ::
    =/  running
      =/  active  (~(run by running.ship-state.gall) |=(agent [%.y +<]))
      (sort ~(tap by active) aor)
    ::
    =/  =mass
      :+  %gall  %.n
      :~  [%foreign %.y contacts.ship-state.gall]
          [%blocked %.n waiting]
          [%active %.n running]
          [%dot %.y gall]
      ==
    ::
    =/  moves
      =/  =move  [duct %give %mass mass]
      [move ~]
    ::
    [moves gall-payload]
  ==
::
::  +load: recreate vane.
::
++  load
  |=  =gall-old
  ^+  gall-payload
  ::
  ?-  -.gall-old
    %0  gall-payload(gall gall-old)
  ==
::
::  +scry: standard scry.
::
++  scry
  ~/  %gall-scry
  |=  [fur=(unit (set monk)) =term =shop =desk =coin =path]
  ^-  (unit (unit cage))
  ?.  ?=(%.y -.shop)
    ~
  ::
  =/  =ship  p.shop
  ::
  ?:  ?&  =(%u term)
          =(~ path)
          =([%$ %da now] coin)
          =(our ship)
      ==
    =/  =vase  !>((~(has by running.ship-state.gall) desk))
    =/  =cage  [%noun vase]
    (some (some cage))
  ::
  ?.  =(our ship)
    ~
  ::
  ?.  =([%$ %da now] coin)
    ~
  ::
  ?.  (~(has by running.ship-state.gall) desk)
    (some ~)
  ::
  ?.  ?=(^ path)
    ~
  ::
  =/  initialised  mo-abed:mo
  =/  =privilege  [%high [~ ship]]
  (mo-peek:initialised desk privilege term path)
::
::  +stay: save without cache.
::
++  stay  gall
::
::  +take: response.
::
++  take
  ~/  %gall-take
  |=  [=wire =duct hin=(hypo sign-arvo)]
  ^-  [(list move) _gall-payload]
  ::
  ~|  [%gall-take-failed wire]
  ::
  ?>  ?=([?(%sys %use) *] wire)
  ::
  =/  initialised  (mo-abed:mo duct)
  ::
  =>
  ::
  ?-  i.wire
    %sys  (mo-handle-sys:initialised t.wire q.hin)
    %use  (mo-handle-use:initialised t.wire hin)
  ==
  ::
  mo-abet
--
