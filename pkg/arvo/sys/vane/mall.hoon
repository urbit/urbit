!:  ::  %mall, agent execution
!?  163
!:
::::
|=  pit=vase
=,  mall
=>  =~
|%
::  +coke: cook
::
++  coke
  $?  %inn
      %out
      %cay
  ==
::  +reverse-ames: reverse ames message
::
++  reverse-ames
  $%  ::  diff
      ::
      [%d p=mark q=*]
      ::  etc.
      ::
      [%x ~]
  ==
::  +forward-ames: forward ames message
::
++  forward-ames
  $%  :: message
      ::
      [%m =mark noun=*]
      :: "peel" subscribe
      ::
      [%l =mark =path]
      :: subscribe
      ::
      [%s =path]
      :: cancel+unsubscribe
      ::
      [%u ~]
  ==
::  +foreign-response: foreign response
::
++  foreign-response
  $?  %peer
      %peel
      %poke
      %pull
  ==
--
|%
::  +move: Arvo-level move
::
++  move
  $:  =duct
      move=(wind note-arvo gift-arvo)
  ==
--
|%
::  +state-old: upgrade path
::
++  state-old  ?(state)
::  +state: all state
::
++  state
  $:  :: state version
      ::
      %0
      :: agents by ship
      ::
      =agents
  ==
::  +subscribers: subscriber data
::
++  subscribers
  $:  :: incoming subscribers
      ::
      incoming=bitt
      :: outgoing subscribers
      ::
      outgoing=boat
      :: queue meter
      ::
      meter=(map bone @ud)
  ==
::  +agents: ship state
::
++  agents
  $:  ::  system duct
      ::
      system-duct=duct
      ::  foreign contacts
      ::
      contacts=(map ship foreign)
      ::  running agents
      ::
      running=(map term running-agent)
      ::  waiting queue
      ::
      blocked=(map term blocked)
  ==
::  +routes: new cuff
::
++  routes
  $:  :: disclosing to
      ::
      disclosing=(unit (set ship))
      :: attributed to
      ::
      attributing=ship
  ==
::  +foreign: foreign connections
::
++  foreign
  $:  :: index
      ::
      index=@ud
      :: by duct
      ::
      index-map=(map duct @ud)
      :: by index
      ::
      duct-map=(map @ud duct)
  ==
::  +ducts: opaque input
::
++  ducts
  $:  :: bone sequence
      ::
      bone=@ud
      :: by duct
      ::
      bone-map=(map duct bone)
      :: by bone
      ::
      duct-map=(map bone duct)
  ==
::  +running-agent: agent state
::
++  running-agent
  $:  :: cache
      ::
      cache=worm
      :: control duct
      ::
      control-duct=duct
      :: unstopped
      ::
      live=?
      :: statistics
      ::
      =stats
      :: subscribers
      ::
      =subscribers
      ::  agent core
      ::
      =agent
      :: running state
      ::
      state=vase
      :: update control
      ::
      =beak
      :: req'd translations
      ::
      marks=(map bone mark)
      :: opaque ducts
      ::
      =ducts
  ==
:: +blocked: blocked tasks
::
++  blocked  (qeu (trel duct routes agent-action))
:: +stats: statistics
::
++  stats
  $:  :: change number
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
.  ==
=|  =state
|=  $:  :: identity
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
~%  %mall-top  ..is  ~
|%
::  +mall-payload:  mall payload
::
++  mall-payload  +
::  +mo: Arvo-level move handling
::
::    An outer core responsible for routing moves to and from Arvo; it calls
::    an inner core, +ap, to route internal moves to and from agents.
::
++  mo
  ~%  %mall-mo  +>  ~
  |_
    $:  hen=duct
        moves=(list move)
    ==
  ++  mo-core  .
  ::  +mo-abed: initialise state with the provided duct.
  ::
  ++  mo-abed
    |=  =duct
    ^+  mo-core
    ::
    mo-core(hen duct)
  ::  +mo-abet: resolve moves.
  ::
  ++  mo-abet
    ^-  [(list move) _mall-payload]
    ::
    =/  resolved  (flop moves)
    [resolved mall-payload]
  ::
  ::  +mo-boot: ask %ford to build us a core for the specified agent.
  ::
  ++  mo-boot
    |=  [=term =ship =desk]
    ^+  mo-core
    ::
    =/  =case  [%da now]
    =/  =path
      =/  ship  (scot %p ship)
      =/  case  (scot case)
      /sys/core/[term]/[ship]/[desk]/[case]
    ::
    =/  =note-arvo
      =/  =schematic:ford  [%core [ship desk] /hoon/[term]/age]
      [%f %build live=%.y schematic]
    ::
    =/  pass  [path note-arvo]
    (mo-pass pass)
  ::
  ::  +mo-pass: prepend a standard %pass to the current list of moves.
  ::
  ++  mo-pass
    |=  pass=(pair path note-arvo)
    ^+  mo-core
    ::
    =/  =move  [hen [%pass pass]]
    mo-core(moves [move moves])
  ::  +mo-give: prepend a standard %give to the current list of moves.
  ::
  ++  mo-give
    |=  =gift:able
    ^+  mo-core
    ::
    =/  =move  [hen [%give gift]]
    mo-core(moves [move moves])
  ::  +mo-receive-core: receives an app core built by %ford.
  ::
  ::    Presuming we receive a good core, we first check to see if the agent
  ::    is already running.  If so, we update its beak in %mall's state,
  ::    initialise an +ap core for the agent, install the core we got from
  ::    %ford, and then resolve any moves associated with it.
  ::
  ::    If we're dealing with a new agent, we create one using the result we
  ::    got from %ford, add it to the collection of agents %mall is keeping
  ::    track of, and then do more or less the same procedure as we did for the
  ::    running agent case.
  ::
  ++  mo-receive-core
    ~/  %mo-receive-core
    |=  [=term =beak =made-result:ford]
    ^+  mo-core
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
    =/  maybe-agent=(unit running-agent)
      (~(get by running.agents.state) term)
    ::
    ?^  maybe-agent
      =/  agent  u.maybe-agent(beak beak)
      =.  running.agents.state
        (~(put by running.agents.state) term agent)
      =/  =routes  [disclosing=~ attributing=our]
      =/  app  (ap-abed:ap term routes)
      =.  app  (ap-reinstall:app result-vase)
      ap-abet:app
    ::
    =/  maybe-new-agent  !<(agent result-vase)
    ?~  maybe-new-agent
      =/  err  [[%leaf "{<term>}: not valid agent"] ~]
      (mo-give %onto %.n err)
    =.  mo-core  (mo-new-agent term beak u.maybe-new-agent)
    =/  old  mo-core
    =/  wag
      =/  =routes  [disclosing=~ attributing=our]
      =/  app  (ap-abed:ap term routes)
      (ap-prep:app ~)
    ::
    =/  maybe-tang  -.wag
    =/  app  +.wag
    ?^  maybe-tang
      =.  mo-core  old
      (mo-give %onto %.n u.maybe-tang)
    ::
    =.  mo-core  ap-abet:app
    =.  mo-core  (mo-clear-queue term)
    =/  =suss  [term %boot now]
    (mo-give %onto [%.y suss])
  ::  +mo-new-agent: create a new agent and add it to %mall's state.
  ::
  ::    %mall maintains a collection of running agents.  This arm creates a
  ::    new one with the provided name, beak, and state (held in a vase).
  ::
  ++  mo-new-agent
    |=  [=term =beak =agent]
    ^+  mo-core
    ::
    =/  =ducts
      :+  bone=1
        bone-map=[[[~ ~] 0] ~ ~]
      duct-map=[[0 [~ ~]] ~ ~]
    ::
    =/  running-agent
      =/  default-agent  *running-agent
      %_  default-agent
        control-duct    hen
        beak            beak
        agent           agent
        state           !>(~)
        ducts           ducts
      ==
    ::
    %_  mo-core
      running.agents.state  (~(put by running.agents.state) term running-agent)
    ==
  ::  +mo-handle-foreign-request: handle a foreign request.
  ::
  ::    Handles tasks received on a +call that have come from another ship.
  ::
  ++  mo-handle-foreign-request
    ~/  %mo-handle-foreign-request
    |=  [=ship =internal-task]
    ^+  mo-core
    ::
    =/  =term  p.internal-task
    =/  =agent-action  q.internal-task
    ?:  ?=(%pump -.agent-action)
      ::
      ::  you'd think this would send an ack for the diff
      ::  that caused this pump.  it would, but we already
      ::  sent it when we got the diff in +mo-handle-sys.  then
      ::  we'd have to save the network duct and connect it
      ::  to this returning pump.
      ::
      mo-core
    ::
    ?:  ?=(%peer-not -.agent-action)
      =/  =tang  p.agent-action
      (mo-give %unto %reap (some tang))
    ::
    =^  bone  mo-core  (mo-assign-bone ship)
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
  ::  +mo-handle-foreign-response: handle foreign response.
  ::
  ::    Handle a received %woot from %ames.
  ::
  ++  mo-handle-foreign-response
    |=  [=foreign-response art=(unit ares)]
    ^+  mo-core
    ::
    =/  to-tang
      |=  =ares
      ^-  tang
      ?~  ares
        ~
      =/  tape  (trip p.u.ares)
      [[%leaf tape] q.u.ares]
    ::
    =/  result  (bind art to-tang)
    ?-  foreign-response
      %peel  (mo-give %unto %reap result)
      %peer  (mo-give %unto %reap result)
      %poke  (mo-give %unto %coup result)
      %pull  mo-core
    ==
  ::  +mo-assign-bone: assign an outbone to a ship.
  ::
  ::    If we know about the ship, we simply use its existing bone.  Otherwise
  ::    we register a new entry for the ship, and use a default bone for it.
  ::
  ++  mo-assign-bone
    |=  =ship
    ^-  [bone _mo-core]
    ::
    =/  =foreign
      =/  existing  (~(get by contacts.agents.state) ship)
      (fall existing [1 ~ ~])
    ::
    =/  existing  (~(get by index-map.foreign) hen)
    ?^  existing
      [u.existing mo-core]
    ::
    =/  index  index.foreign
    =/  contacts
      =/  new-foreign
        %_  foreign
          index      +(index)
          index-map  (~(put by index-map.foreign) hen index)
          duct-map   (~(put by duct-map.foreign) index hen)
        ==
      (~(put by contacts.agents.state) ship new-foreign)
    ::
    =/  next  mo-core(contacts.agents.state contacts)
    [index next]
  ::  +mo-retrieve-duct: retrieve a duct by index.
  ::
  ++  mo-retrieve-duct
    |=  [=ship index=@ud]
    ^-  duct
    ::
    =/  =foreign  (~(got by contacts.agents.state) ship)
    (~(got by duct-map.foreign) index)
  ::  +mo-handle-sys: handle a +sign incoming over /sys.
  ::
  ::    (Note that /sys implies the +sign should be routed to a vane.)
  ::
  ++  mo-handle-sys
    ~/  %mo-handle-sys
    |=  [=path =sign-arvo]
    ^+  mo-core
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
  ::  +mo-handle-sys-core: receive a core from %ford.
  ::
  ++  mo-handle-sys-core
    |=  [=path =sign-arvo]
    ^+  mo-core
    ::
    ?>  ?=([%f %made *] sign-arvo)
    ?>  ?=([@ @ @ @ @ ~] path)
    =/  beak-path  t.t.path
    =/  =beak
      =/  =ship  (slav %p i.beak-path)
      =/  =desk  i.t.beak-path
      =/  =case  [%da (slav %da i.t.t.beak-path)]
      [ship desk case]
    (mo-receive-core i.t.path beak result.sign-arvo)
  ::  +mo-handle-sys-pel: translated peer.
  ::
  ::    Validates a received %ford result and %gives an internal %diff.
  ::
  ++  mo-handle-sys-pel
    |=  [=path =sign-arvo]
    ^+  mo-core
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
  ::  +mo-handle-sys-red: diff ack.
  ::
  ::    On receipt of a valid +sign from %ames, we simply pass a %pump
  ::    acknowledgement internally; otherwise we pass both an internal
  ::    unsubscribing %pull, plus a %want to %ames, before complaining about a
  ::    bad message acknowledgment.
  ::
  ++  mo-handle-sys-red
    |=  [=path =sign-arvo]
    ^+  mo-core
    ::
    ?>  ?=([@ @ @ @ ~] path)
    ?.  ?=([%a %woot *] sign-arvo)
      ~&  [%red-want path]
      mo-core
    ::
    =/  him  (slav %p i.t.path)
    =/  dap  i.t.t.path
    =/  num  (slav %ud i.t.t.t.path)
    =/  =coop  q.+>.sign-arvo
    =/  sys-path
      =/  pax  [%req t.path]
      [%sys pax]
    ::
    ?~  coop
      =/  =note-arvo
        =/  =sock  [him our]
        =/  =internal-task  [dap %pump ~]
        =/  =task:able  [%deal sock internal-task]
        [%m task]
      (mo-pass sys-path note-arvo)
    ::
    =/  mall-move=note-arvo
      =/  =sock  [him our]
      =/  =internal-task  [dap %pull ~]
      =/  =task:able  [%deal sock internal-task]
      [%m task]
    ::
    =/  ames-move=note-arvo
      =/  path  [%m %gh dap ~]
      =/  =noun  [num %x ~]
      =/  =task:able:ames  [%want him path noun]
      [%a task]
    ::
    =.  mo-core  (mo-pass sys-path mall-move)
    =.  mo-core  (mo-pass sys-path ames-move)
    ::
    ?.  ?=([~ ~ %mack *] coop)
      ~&  [%diff-bad-ack coop]
      mo-core
    ::
    ~&  [%diff-bad-ack %mack]
    =/  print  (slog (flop q.,.+>.coop))
    (print mo-core)
  ::  +mo-handle-sys-rep: reverse request.
  ::
  ::    On receipt of a valid +sign from %ford, sets state to the appropriate
  ::    duct and gives an internal %diff containing the +sign payload.
  ::
  ++  mo-handle-sys-rep
    |=  [=path =sign-arvo]
    ^+  mo-core
    ::
    ?>  ?=([@ @ @ @ ~] path)
    ?>  ?=([%f %made *] sign-arvo)
    =/  him  (slav %p i.t.path)
    =/  dap  i.t.t.path
    =/  num  (slav %ud i.t.t.t.path)
    ::
    ?:  ?=([%incomplete *] result.sign-arvo)
      =/  err  (some tang.result.sign-arvo)
      (mo-give %mack err)
    ::
    =/  build-result  build-result.result.sign-arvo
    ?:  ?=([%error *] build-result)
      ::  XX should crash
      =/  err  (some message.build-result)
      (mo-give %mack err)
    ::  XX pump should ack
    =.  mo-core  (mo-give %mack ~)
    =/  duct  (mo-retrieve-duct him num)
    =.  mo-core  (mo-abed duct)
    =/  =cage  (result-to-cage:ford build-result)
    =/  move  [%unto [%diff cage]]
    (mo-give move)
  ::  +mo-handle-sys-req: process an inbound request.
  ::
  ++  mo-handle-sys-req
    |=  [=path =sign-arvo]
    ^+  mo-core
    ::
    ?>  ?=([@ @ @ @ ~] path)
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
      ?:  ?=([%error *] build-result)
        =/  err  (some message.build-result)
        (mo-give %mack err)
      ::
      =/  sys-path  [%sys path]
      =/  =note-arvo
        =/  =cage  (result-to-cage:ford build-result)
        [%m %deal [him our] i.t.t.path %poke cage]
      (mo-pass sys-path note-arvo)
    ::
    ?:  ?=([%a %woot *] sign-arvo)
      mo-core
    ::
    ?>  ?=([%m %unto *] sign-arvo)
    =/  =internal-gift  +>.sign-arvo
    ::
    ?-    -.internal-gift
        %coup
      (mo-give %mack p.internal-gift)
    ::
        %diff
      =/  sys-path  [%sys %red t.path]
      =/  =note-arvo
        =/  path  [%m %gh dap ~]
        =/  noun  [num %d p.p.internal-gift q.q.p.internal-gift]
        [%a %want him path noun]
      (mo-pass sys-path note-arvo)
    ::
        %quit
      =/  sys-path  [%sys path]
      =/  =note-arvo
        =/  path  [%m %gh dap ~]
        =/  noun  [num %x ~]
        [%a %want him path noun]
      (mo-pass sys-path note-arvo)
    ::
        %reap
      (mo-give %mack p.internal-gift)
    ::
        %http-response
      !!
    ==
  ::  +mo-handle-sys-val: inbound validate.
  ::
  ::    Validates an incoming +sign from %ford and applies it to the specified
  ::    agent.
  ::
  ++  mo-handle-sys-val
    |=  [=path =sign-arvo]
    ^+  mo-core
    ::
    ?>  ?=([%f %made *] sign-arvo)
    ?>  ?=([@ @ @ ~] path)
    =/  =ship  (slav %p i.t.path)
    =/  =term  i.t.t.path
    ?:  ?=([%incomplete *] result.sign-arvo)
      =/  err  (some tang.result.sign-arvo)
      (mo-give %unto %coup err)
    ::
    =/  build-result  build-result.result.sign-arvo
    ?:  ?=([%error *] build-result)
      =/  err  (some message.build-result)
      (mo-give %unto %coup err)
    ::
    =/  =routes  [disclosing=~ attributing=ship]
    =/  =cage  (result-to-cage:ford build-result)
    =/  =agent-action  [%poke cage]
    (mo-apply term routes agent-action)
  ::  +mo-handle-sys-way: outbound request.
  ::
  ++  mo-handle-sys-way
    |=  [=path =sign-arvo]
    ^+  mo-core
    ::
    ?>  ?=([%a %woot *] sign-arvo)
    ?>  ?=([@ @ ~] path)
    =/  =foreign-response  (foreign-response i.t.path)
    =/  maybe-ares  +>+.sign-arvo
    (mo-handle-foreign-response foreign-response maybe-ares)
  ::  +mo-handle-use: handle a typed +sign incoming on /use.
  ::
  ::    (Note that /use implies the +sign should be routed to an agent.)
  ::
  ::    Initialises the specified agent and then performs an agent-level +take
  ::    on the supplied +sign.
  ::
  ++  mo-handle-use
    ~/  %mo-handle-use
    |=  [=path hin=(hypo sign-arvo)]
    ^+  mo-core
    ::
    ?.  ?=([@ @ coke *] path)
      ~&  [%mo-handle-use-bad-path path]
      !!
    ::
    =/  app
      =/  =term  i.path
      =/  =ship  (slav %p i.t.path)
      =/  =routes  [disclosing=~ attributing=ship]
      (ap-abed:ap term routes)
    ::
    =/  =sign-arvo  q.hin
    ?-  i.t.t.path
        %inn
      =/  =vase  (slot 3 hin)
      =.  app  (ap-generic-take:app t.t.t.path vase)
      ap-abet:app
    ::
        %cay
      ~&  [%mo-handle-use-weird sign-arvo]
      ~&  [%mo-handle-use-weird-path path]
      mo-core
    ::
        %out
      ?.  ?=([%m %unto *] sign-arvo)
        ~&  [%mo-handle-use-weird sign-arvo]
        ~&  [%mo-handle-use-weird-path path]
        mo-core
      =.  app
        =/  =internal-gift  +>.sign-arvo
        (ap-specific-take:app t.t.t.path internal-gift)
      ap-abet:app
    ==
  ::  +mo-clear-queue: clear blocked tasks from the specified running agent.
  ::
  ++  mo-clear-queue
    |=  =term
    ^+  mo-core
    ::
    ?.  (~(has by running.agents.state) term)
      mo-core
    =/  maybe-blocked  (~(get by blocked.agents.state) term)
    ?~  maybe-blocked
      mo-core
    ::
    =/  =blocked  u.maybe-blocked
    ::
    |-  ^+  mo-core
    ?:  =(~ blocked)
      =/  blocked   (~(del by blocked.agents.state) term)
      %_  mo-core
        blocked.agents.state  blocked
      ==
    =^  task  blocked  [p q]:~(get to blocked)
    =/  =duct  p.task
    =/  =routes  q.task
    =/  =agent-action  r.task
    ::
    =/  move
      =/  =sock  [attributing.routes our]
      =/  =internal-task  [term agent-action]
      =/  card  [%slip %m %deal sock internal-task]
      [duct card]
    $(moves [move moves])
  ::  +mo-beak: assemble a beak for the specified agent.
  ::
  ++  mo-beak
    |=  =term
    ^-  beak
    ::
    ?~  running=(~(get by running.agents.state) term)
      ::  XX this fallback is necessary, as .term could be either the source
      ::  or the destination app. ie, it might not exist locally ...
      ::
      [our %home %da now]
    beak.u.running
  ::  +mo-peek:  call to +ap-peek (which is not accessible outside of +mo).
  ::
  ++  mo-peek
    ~/  %mo-peek
    |=  [agent=term =routes =term =path]
    ^-  (unit (unit cage))
    ::
    =/  app  (ap-abed:ap agent routes)
    (ap-peek:app term path)
  ::  +mo-apply: apply the supplied action to the specified agent.
  ::
  ++  mo-apply
    |=  [=term =routes =agent-action]
    ^+  mo-core
    ::
    =/  =path
      =/  ship  (scot %p attributing.routes)
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
    =/  app  (ap-abed:ap term routes)
    =.  app  (ap-apply:app agent-action)
    ap-abet:app
  ::  +mo-handle-local: handle locally.
  ::
  ::    If the agent is running or blocked, assign it the supplied +task.
  ::    Otherwise simply apply the action to the agent.
  ::
  ++  mo-handle-local
    |=  [=ship =internal-task]
    ^+  mo-core
    ::
    =/  =routes  [disclosing=~ attributing=ship]
    =/  =term  p.internal-task
    =/  =agent-action  q.internal-task
    =/  is-running  (~(has by running.agents.state) term)
    =/  is-blocked  (~(has by blocked.agents.state) term)
    ::
    ?:  |(!is-running is-blocked)
      =/  =blocked
        =/  waiting  (~(get by blocked.agents.state) term)
        =/  tasks  (fall waiting *blocked)
        =/  task  [hen routes agent-action]
        (~(put to tasks) task)
      ::
      %_  mo-core
        blocked.agents.state  (~(put by blocked.agents.state) term blocked)
      ==
    (mo-apply term routes agent-action)
  ::  +mo-handle-forward: handle forward %ames message.
  ::
  ++  mo-handle-forward
    |=  [=ship =term =bone =forward-ames]
    ^+  mo-core
    ::
    =.  mo-core
      ?.  ?=(%u -.forward-ames)
        mo-core
      (mo-give %mack ~)
    ::
    =/  =path
      =/  him  (scot %p ship)
      =/  num  (scot %ud bone)
      /sys/req/[him]/[term]/[num]
    ::
    =/  =sock  [ship our]
    =/  =note-arvo
      ?-  -.forward-ames
          %m
        =/  =task:able
          =/  =internal-task  [term %puff [mark noun]:forward-ames]
          [%deal sock internal-task]
        [%m task]
      ::
          %l
        =/  =task:able
          =/  =internal-task  [term %peel [mark path]:forward-ames]
          [%deal sock internal-task]
        [%m task]
      ::
          %s
        =/  =task:able
          =/  =internal-task  [term %peer path.forward-ames]
          [%deal sock internal-task]
        [%m task]
      ::
          %u
        =/  =task:able
          =/  =internal-task  [term %pull ~]
          [%deal sock internal-task]
        [%m task]
      ==
    (mo-pass path note-arvo)
  ::  +mo-handle-backward: handle reverse %ames message.
  ::
  ++  mo-handle-backward
    |=  [=ship =term =bone =reverse-ames]
    ^+  mo-core
    ::
    ?-    -.reverse-ames
        %d
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
      ::  XX should crash
      =.  mo-core  (mo-give %mack ~)
      =/  initialised
        =/  out  (mo-retrieve-duct ship bone)
        (mo-abed out)
      (mo-give:initialised %unto %quit ~)
    ==
  ::  +ap: agent engine
  ::
  ::    An inner, agent-level core.  The sample refers to the agent we're
  ::    currently focused on.
  ::
  ++  ap
    ~%  %mall-ap  +>  ~
    |_  $:  agent-name=term
            agent-routes=routes
            agent-bone=bone
            agent-moves=(list internal-move)
            agent-config=(list (each suss tang))
            current-agent=running-agent
        ==
    ++  ap-core  .
    ::  +ap-abed: initialise state for an agent, with the supplied routes.
    ::
    ::    The agent must already be running in +mall -- here we simply update
    ::    +ap's state to focus on it.
    ::
    ++  ap-abed
      ~/  %ap-abed
      |=  [=term =routes]
      ^+  ap-core
      ::
      =/  =running-agent
        =/  running  (~(got by running.agents.state) term)
        =/  =stats
          :+  +(change.stats.running)
            (shaz (mix (add term change.stats.running) eny))
          now
        running(stats stats)
      ::
      =.  agent-name  term
      =.  agent-routes  routes
      =.  current-agent  running-agent
      =/  maybe-bone  (~(get by bone-map.ducts.running-agent) hen)
      ?^  maybe-bone
        ap-core(agent-bone u.maybe-bone)
      ::
      =/  =ducts
        :+  +(bone.ducts.running-agent)
          (~(put by bone-map.ducts.running-agent) hen bone.ducts.running-agent)
        (~(put by duct-map.ducts.running-agent) bone.ducts.running-agent hen)
      ::
      %_  ap-core
        agent-bone           bone.ducts.running-agent
        ducts.current-agent  ducts
      ==
    ::  +ap-abet: resolve moves.
    ::
    ++  ap-abet
      ^+  mo-core
      ::
      =>  ap-track-queue
      =/  running  (~(put by running.agents.state) agent-name current-agent)
      =/  moves
        =/  giver  |=(report=(each suss tang) [hen %give %onto report])
        =/  from-internal  (turn agent-moves ap-from-internal)
        =/  from-suss  (turn agent-config giver)
        :(weld from-internal from-suss moves)
      ::
      %_  mo-core
        running.agents.state  running
        moves                 moves
      ==
    ::  +ap-track-queue: track queue.
    ::
    ++  ap-track-queue
      ^+  ap-core
      ::
      =/  internal-moves  agent-moves
      =/  bones  *(set bone)
      |-  ^+  ap-core
      ?^  internal-moves
        =/  =internal-move  i.internal-moves
        ?.  ?=([%give %diff *] move.internal-move)
          $(internal-moves t.internal-moves)
        ::
        =^  filled  ap-core  ap-enqueue(agent-bone bone.internal-move)
        =/  new-bones
          ?:  filled
            bones
          (~(put in bones) bone.internal-move)
        $(internal-moves t.internal-moves, bones new-bones)
      ::
      =/  bones  ~(tap in bones)
      ::
      |-  ^+  ap-core
      ?~  bones
        ap-core
      ::
      =>  $(bones t.bones, agent-bone i.bones)
      =/  incoming
        (~(get by incoming.subscribers.current-agent) agent-bone)
      ?~  incoming
        ~&  [%ap-track-queue-bad-bone agent-name agent-bone]
        ap-core
      ::
      =/  =ship  p.u.incoming
      ap-kill(attributing.agent-routes ship)
    ::  +ap-from-internal: internal move to move.
    ::
    ::    We convert from bone-indexed moves to duct-indexed moves when
    ::    resolving them in Arvo.
    ::
    ++  ap-from-internal
      ~/  %ap-from-internal
      |=  =internal-move
      ^-  move
      ::
      ~|  [%mall-move-conversion-failed internal-move]
      =/  =duct
        (~(got by duct-map.ducts.current-agent) bone.internal-move)
      ::
      =/  card
        ?-    -.move.internal-move
            %slip  !!
        ::
            %give
          ?<  =(0 bone.internal-move)
          ::
          =/  =internal-gift  p.move.internal-move
          ?.  ?=(%diff -.internal-gift)
            [%give %unto internal-gift]
          ::
          =/  =cage  p.internal-gift
          =/  =mark
            =/  mark  (~(get by marks.current-agent) bone.internal-move)
            (fall mark p.cage)
          ::
          ?:  =(mark p.cage)
            [%give %unto internal-gift]
          =/  =path  /sys/pel/[agent-name]
          =/  =note-arvo
            =/  =schematic:ford
              =/  =beak  (mo-beak agent-name)
              [%cast [p q]:beak mark [%$ cage]]
            [%f %build live=%.n schematic]
          ::
          [%pass path note-arvo]
        ::
            %pass
          =/  =path  p.move.internal-move
          =/  =internal-note  q.move.internal-move
          =/  use-path  [%use agent-name path]
          =/  =note-arvo
            ?-  -.internal-note
                %send
              =/  =task:able
                =/  =sock  [our ship.internal-note]
                =/  =internal-task  internal-task.internal-note
                [%deal sock internal-task]
              [%m task]
            ::
                %meta
              =/  =term  term.internal-note
              =/  =vase  vase.internal-note
              [term %meta vase]
            ==
          [%pass use-path note-arvo]
        ==
      [duct card]
    ::  +ap-agent-core: agent core with current bowl and state
    ::
    ++  ap-agent-core
      ~(. agent.current-agent ap-construct-bowl state.current-agent)
    ::  +ap-apply: apply effect.
    ::
    ++  ap-apply
      |=  =agent-action
      ^+  ap-core
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
    ::  +ap-peek: peek.
    ::
    ++  ap-peek
      ~/  %ap-peek
      |=  [=term tyl=path]
      ^-  (unit (unit cage))
      ::
      =/  peek-result=(each (unit (unit cage)) tang)
        (mule |.((handle-peek:ap-agent-core [term tyl])))
      ::
      ?-  -.peek-result
        %&  p.peek-result
        %|  ((slog leaf+"peek bad result" p.peek-result) [~ ~])
      ==
    ::  +ap-update-subscription: update subscription.
    ::
    ++  ap-update-subscription
      ~/  %ap-update-subscription
      |=  [is-ok=? =ship =path]
      ^+  ap-core
      ::
      =/  way  [(scot %p ship) %out path]
      ::
      ?:  is-ok
        =/  =internal-note  [%send ship -.path %pump ~]
        (ap-pass way internal-note)
      =.  ap-core  (ap-give %quit ~)
      =/  =internal-note  [%send ship -.path %pull ~]
      (ap-pass way internal-note)
    ::  +ap-dequeue: drop from queue.
    ::
    ::    Dequeues along the current bone, deleting the queue entirely if it
    ::    drops to zero.
    ::
    ++  ap-dequeue
      ^+  ap-core
      ::
      ?.  (~(has by incoming.subscribers.current-agent) agent-bone)
        ap-core
      =/  level  (~(get by meter.subscribers.current-agent) agent-bone)
      ?:  |(?=(~ level) =(0 u.level))
        ap-core
      ::
      =.  u.level  (dec u.level)
      ?:  =(0 u.level)
        =/  deleted  (~(del by meter.subscribers.current-agent) agent-bone)
        ap-core(meter.subscribers.current-agent deleted)
      ::
      =/  dequeued
        (~(put by meter.subscribers.current-agent) agent-bone u.level)
      ap-core(meter.subscribers.current-agent dequeued)
    ::  +ap-enqueue: add to queue.
    ::
    ::    Every agent has a 'meter', that tracks the number of incoming
    ::    subscribers by bone.  We get both the meter and ship associated with
    ::    the current bone; if the meter has hit twenty for another ship, we
    ::    don't enqueue the subscriber.  Otherwise we increment the meter for
    ::    the current bone and update the agent's state with it.
    ::
    ::    Returns a yes if the meter has been incremented, and no otherwise.
    ::
    ++  ap-enqueue
      ^-  [? _ap-core]
      ::
      =/  meter  (~(gut by meter.subscribers.current-agent) agent-bone 0)
      =/  subscriber=(unit (pair ship path))
        (~(get by incoming.subscribers.current-agent) agent-bone)
      ::
      ?:  ?&  =(20 meter)
              ?|  ?=(~ subscriber)
                  !=(our p.u.subscriber)
              ==
          ==
        =/  incoming  (~(get by incoming.subscribers.current-agent) agent-bone)
        =/  duct  (~(get by duct-map.ducts.current-agent) agent-bone)
        ~&  [%mall-pulling-20 agent-bone incoming duct]
        [%.n ap-core]
      ::
      =/  next
        =/  meter
          (~(put by meter.subscribers.current-agent) agent-bone +(meter))
        ap-core(meter.subscribers.current-agent meter)
      ::
      [%.y next]
    ::  +ap-give: return result.
    ::
    ++  ap-give
      |=  =internal-gift
      ^+  ap-core
      ::
      =/  internal-moves
        =/  move  [%give internal-gift]
        =/  =internal-move  [agent-bone move]
        [internal-move agent-moves]
      ap-core(agent-moves internal-moves)
    ::  +ap-construct-bowl: set up bowl.
    ::
    ++  ap-construct-bowl
      ^-  bowl
      :*  :*  our                                     ::  host
              attributing.agent-routes                ::  guest
              agent-name                              ::  agent
          ==                                          ::
          :*  :: NB (jtobin): see urbit/urbit#1466
              wex=~                                   ::  outgoing
              sup=incoming.subscribers.current-agent  ::  incoming
          ==                                          ::
          :*  agent-bone=agent-bone                   ::  cause
              act=change.stats.current-agent          ::  tick
              eny=eny.stats.current-agent             ::  nonce
              now=time.stats.current-agent            ::  time
              byk=beak.current-agent                  ::  source
      ==  ==
    ::  +ap-pass: request action.
    ::
    ++  ap-pass
      |=  [=path =internal-note]
      ^+  ap-core
      ::
      =/  =internal-move
        =/  move  [%pass path internal-note]
        [agent-bone move]
      =/  internal-moves  [internal-move agent-moves]
      ap-core(agent-moves internal-moves)
    ::  +ap-reinstall: reinstall.
    ::
    ++  ap-reinstall
      ~/  %ap-reinstall
      |=  =vase
      ^+  ap-core
      ::
      =/  maybe-agent  !<(agent vase)
      ?~  maybe-agent
        (ap-lame %new-core-not-agent ~)
      ::
      =/  prep
        =/  =agent  u.maybe-agent
        =/  installed  ap-install(agent.current-agent agent)
        =/  running  (some state.current-agent)
        (installed running)
      ::
      =^  maybe-tang  ap-core  prep
      ?~  maybe-tang
        ap-core
      (ap-lame %prep-failed u.maybe-tang)
    ::  +ap-peel: apply %peel.
    ::
    ++  ap-peel
      |=  [=mark =path]
      ^+  ap-core
      ::
      =.  marks.current-agent  (~(put by marks.current-agent) agent-bone mark)
      (ap-peer path)
    ::  +ap-peer: apply %peer.
    ::
    ++  ap-peer
      ~/  %ap-peer
      |=  pax=path
      ^+  ap-core
      ::
      =/  incoming  [attributing.agent-routes pax]
      =.  incoming.subscribers.current-agent
        (~(put by incoming.subscribers.current-agent) agent-bone incoming)
      ::
      =^  maybe-tang  ap-core
        %+  ap-ingest  %reap  |.
        (handle-peer:ap-agent-core pax)
      ?^  maybe-tang
        ap-silent-delete
      ap-core
    ::  +ap-poke: apply %poke.
    ::
    ++  ap-poke
      ~/  %ap-poke
      |=  =cage
      ^+  ap-core
      ::
      =^  maybe-tang  ap-core
        %+  ap-ingest  %coup  |.
        (handle-poke:ap-agent-core cage)
      ap-core
    ::  +ap-lame: pour error.
    ::
    ++  ap-lame
      |=  [=term =tang]
      ^+  ap-core
      ::
      =/  form  |=(=tank [%rose [~ "! " ~] tank ~])
      =^  maybe-tang  ap-core
        %+  ap-ingest  ~  |.
        (handle-lame:ap-agent-core term (turn tang form))
      ap-core
    ::  +ap-generic-take: generic take.
    ::
    ++  ap-generic-take
      ~/  %ap-generic-take
      |=  [=wire =vase]
      ^+  ap-core
      ::
      =^  maybe-tang  ap-core
        %+  ap-ingest  ~  |.
        (handle-take:ap-agent-core wire vase)
      ?^  maybe-tang
        (ap-lame %take u.maybe-tang)
      ap-core
    ::  +ap-specific-take: specific take.
    ::
    ++  ap-specific-take
      |=  [=path =internal-gift]
      ^+  ap-core
      ::
      =^  maybe-tang  ap-core
        %+  ap-ingest  ~  |.
        (handle-mall:ap-agent-core +.path internal-gift)
      ?:  ?=(%diff -.internal-gift)
        (ap-update-subscription =(~ maybe-tang) attributing.agent-routes +.path)
      ?^  maybe-tang
        (ap-lame -.internal-gift u.maybe-tang)
      ap-core
    ::  +ap-install: install wrapper.
    ::
    ++  ap-install
      |=  maybe-vase=(unit vase)
      ^-  [(unit tang) _ap-core]
      ::
      =^  maybe-tang  ap-core  (ap-prep maybe-vase)
      ::
      =/  new-agent-config
        =/  =term  ?~(maybe-vase %boot %bump)
        =/  possibly-suss
          ?~  maybe-tang
            =/  =suss  [agent-name term now]
            [%.y suss]
          [%.n u.maybe-tang]
        [possibly-suss agent-config]
      ::
      =/  next
        ap-core(agent-config new-agent-config)
      ::
      [maybe-tang next]
    ::  +ap-prep: low-level install.
    ::
    ++  ap-prep
      ~/  %ap-prep
      |=  maybe-vase=(unit vase)
      ^-  [(unit tang) _ap-core]
      ::
      =^  maybe-tang  ap-core
        %+  ap-ingest  ~
        ?~  maybe-vase
          |.  handle-init:ap-agent-core
        |.  (handle-prep:ap-agent-core u.maybe-vase)
      [maybe-tang ap-core]
    ::  +ap-silent-delete: silent delete.
    ::
    ++  ap-silent-delete
      ^+  ap-core
      ::
      ?~  (~(get by incoming.subscribers.current-agent) agent-bone)
        ap-core
      ::
      =/  incoming  (~(del by incoming.subscribers.current-agent) agent-bone)
      =/  meter  (~(del by meter.subscribers.current-agent) agent-bone)
      %_  ap-core
        incoming.subscribers.current-agent  incoming
        meter.subscribers.current-agent     meter
      ==
    ::  +ap-load-delete: load delete.
    ::
    ++  ap-load-delete
      ^+  ap-core
      ::
      =/  maybe-incoming
        (~(get by incoming.subscribers.current-agent) agent-bone)
      ?~  maybe-incoming
        ap-core
      ::
      =/  incoming  u.maybe-incoming
      =.  incoming.subscribers.current-agent
        (~(del by incoming.subscribers.current-agent) agent-bone)
      =.  meter.subscribers.current-agent
        (~(del by meter.subscribers.current-agent) agent-bone)
      ::
      =^  maybe-tang  ap-core
        %+  ap-ingest  ~  |.
        (handle-pull:ap-agent-core q.incoming)
      ?^  maybe-tang
        (ap-lame %pull u.maybe-tang)
      ap-core
    ::  +ap-kill: queue kill.
    ::
    ++  ap-kill
      ^+  ap-core
      ::
      =>  ap-load-delete
      (ap-give %quit ~)
    ::  +ap-ingest: call agent arm
    ::
    ::    Handle acks here because they need to be emitted before the
    ::    rest of the moves.
    ::
    ++  ap-ingest
      |=  [ack=?(%coup %reap ~) run=_^?(|.(*step:agent))]
      ^-  [(unit tang) _ap-core]
      =/  result  (mule run)
      =^  new-moves  ap-core  (ap-handle-result result)
      =/  maybe-tang=(unit tang)
        ?:  ?=(%& -.result)
          ~
        `p.result
      =/  ack-moves
        ?-  ack
          ~      ~
          %coup  [agent-bone %give %coup maybe-tang]~
          %reap  [agent-bone %give %reap maybe-tang]~
        ==
      ::
      =.  agent-moves 
        :(weld new-moves ack-moves agent-moves)
      [maybe-tang ap-core]
    ::  +ap-handle-result: handle result.
    ::
    ++  ap-handle-result
      ~/  %ap-handle-result
      |=  result=(each step:agent tang)
      ^-  [(list move:agent) _ap-core]
      ?:  ?=(%| -.result)
        `ap-core
      ::
      =/  new-subs  (ap-handle-quits -.p.result)
      ::
      :-  (flop -.p.result)
      %_  ap-core
        state.current-agent                 +.p.result
        incoming.subscribers.current-agent  new-subs
      ==
    ::  +ap-handle-quits: handle cancels of incoming subscriptions
    ::
    ++  ap-handle-quits
      ~/  %ap-handle-quits
      |=  moves=(list move:agent)
      ^-  bitt
      =/  quits=(list bone)
        %+  murn  moves
        |=  =move:agent
        ^-  (unit bone)
        ?.  ?=([%give %quit *] move.internal-move)
          ~
        `bone.move
      ::
      =/  quit-map=bitt
        (malt (turn quits |=(=bone [bone *[ship path]])))
      (~(dif by incoming.subscribers.current-agent) quit-map)
    ::  +ap-tang: standard tang.
    ::
    ++  ap-tang
      |=  =tape
      ^-  tang
      ::
      =/  =tank  [%leaf (weld "mall: {<agent-name>}: " tape)]
      [tank ~]
    --
  --
::  +call: request
::
++  call
  ~%  %mall-call  +>   ~
  |=  [=duct hic=(hypo (hobo task:able))]
  ^-  [(list move) _mall-payload]
  ::
  ~|  [%mall-call-failed duct q.hic]
  ::  make sure our task is hard
  ::
  =/  =task:able
    ?.  ?=(%soft -.q.hic)
      q.hic
    ;;  task:able  p.q.hic
  ::
  =/  initialised  (mo-abed:mo duct)
  ?-    -.task
      %conf-mall
    =/  =dock  p.task
    =/  =ship  p.dock
    ?.  =(our ship)
      ~&  [%mall-not-ours ship]
      [~ mall-payload]
    ::
    ~&  [%mall-ours ship]
    =>  (mo-boot:initialised q.dock q.task)
    ~&  [%mall-initialized ship]
    mo-abet
  ::
      ?(%deal %deal-mall)
    =/  =sock  p.task
    =/  =internal-task  q.task
    ?.  =(q.sock our)
      ?>  =(p.sock our)
      =>  (mo-handle-foreign-request:initialised q.sock internal-task)
      mo-abet
    ::
    =>  (mo-handle-local:initialised p.sock internal-task)
    mo-abet
  ::
      %init
    =/  payload  mall-payload(system-duct.agents.state duct)
    [~ payload]
  ::
      %vega
    [~ mall-payload]
  ::
      %west
    =/  =ship  p.task
    =/  =path  q.task
    =/  =noun  r.task
    ::
    ?>  ?=([?(%ge %gh) @ ~] path)
    =/  agent-name  i.t.path
    ::
    ?:  ?=(%ge i.path)
      =/  mes  ;;((pair @ud forward-ames) noun)
      =>  (mo-handle-forward:initialised ship agent-name mes)
      mo-abet
    ::
    =/  mes  ;;((pair @ud reverse-ames) noun)
    =>  (mo-handle-backward:initialised ship agent-name mes)
    mo-abet
  ::
      %wash
    =.  running.agents.state
      %-  ~(run by running.agents.state)
      |=  =running-agent
      running-agent(cache *worm)
    [~ mall-payload]
  ::
      %wegh
    =/  blocked
      =/  queued  (~(run by blocked.agents.state) |=(blocked [%.y +<]))
      (sort ~(tap by queued) aor)
    ::
    =/  running
      =/  active  (~(run by running.agents.state) |=(running-agent [%.y +<]))
      (sort ~(tap by active) aor)
    ::
    =/  =mass
      :+  %mall  %.n
      :~  [%foreign %.y contacts.agents.state]
          [%blocked %.n blocked]
          [%active %.n running]
          [%dot %.y state]
      ==
    ::
    =/  moves
      =/  =move  [duct %give %mass mass]
      [move ~]
    ::
    [moves mall-payload]
  ==
::  +load: recreate vane
::
++  load
  |=  =state-old
  ^+  mall-payload
  ::
  ?-  -.state-old
    %0  mall-payload  ::  (state state-old)
  ==
::  +scry: standard scry
::
++  scry
  ~/  %mall-scry
  |=  [fur=(unit (set monk)) =term =shop =desk =coin =path]
  ^-  (unit (unit cage))
  ?.  ?=(%.y -.shop)
    ~
  ::
  =/  =ship  p.shop
  ?:  ?&  =(%u term)
          =(~ path)
          =([%$ %da now] coin)
          =(our ship)
      ==
    =/  =vase  !>((~(has by running.agents.state) desk))
    =/  =cage  [%noun vase]
    (some (some cage))
  ::
  ?.  =(our ship)
    ~
  ::
  ?.  =([%$ %da now] coin)
    ~
  ::
  ?.  (~(has by running.agents.state) desk)
    (some ~)
  ::
  ?.  ?=(^ path)
    ~
  ::
  =/  initialised  mo-abed:mo
  =/  =routes  [~ ship]
  (mo-peek:initialised desk routes term path)
::  +stay: save without cache
::
++  stay  state
::  +take: response
::
++  take
  ~/  %mall-take
  |=  [=wire =duct hin=(hypo sign-arvo)]
  ^-  [(list move) _mall-payload]
  ::
  ~|  [%mall-take-failed wire]
  ?>  ?=([?(%sys %use) *] wire)
  =/  initialised  (mo-abed:mo duct)
  =/  =sign-arvo  q.hin
  =>
  ?-  i.wire
    %sys  (mo-handle-sys:initialised t.wire sign-arvo)
    %use  (mo-handle-use:initialised t.wire hin)
  ==
  mo-abet
--
