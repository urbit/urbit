!:  ::  %gall, agent execution
!?  163
!:
::::
|=  pit=vase
=,  gall
=>  =~
|%
::  +coke: cook
::
++  coke
  $?  %inn
      %out
      %cay
  ==
::  +ames-response: network response message (%boon)
::
::    %d: diff
::    %x: quit
::
++  ames-response
  $%  [%d =mark noun=*]
      [%x ~]
  ==
::  +ames-request: network request (%plea)
::
::    %m: poke
::    %l: "peel" mark-translated subscribe
::    %s: subscribe
::    %u: unsubscribe
::
++  ames-request
  $%  [%m =mark noun=*]
      [%l =mark =path]
      [%s =path]
      [%u ~]
  ==
::  +remote-request: kinds of $agent-action that can cross the network
::
::    Used in wires to identify the kind of remote request we made.
::    Bijective with the tags of $ames-request.
::
++  remote-request
  $?  %peer
      %peel
      %poke
      %pull
  ==
--
|%
::  +internal-note: +ap note
::
++  internal-note
  $%  [%meta =term =vase]
      [%send =ship =internal-task]
  ==
::  +internal-move: agent-level move
::
::    Analogous to an Arvo move, except these are routed by bone, instead of
::    duct.
::
++  internal-move
  $:  =bone
      move=(wind internal-note internal-gift)
  ==
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
  ==
::  +agents: ship state
::
++  agents
  $:  ::  system duct
      ::
      system-duct=duct
      ::  foreign contacts
      ::
      contacts=(set ship)
      ::  running agents
      ::
      running=(map term agent)
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
::  +misvale-data: subscribers with bad marks
::
::    XX a hack, required to break a subscription loop
::    which arises when an invalid mark crashes a diff.
::    See usage in ap-misvale.
::
++  misvale-data  (set wire)
::  +agent: agent state
::
++  agent
  $:  :: bad reqs
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
      :: statistics
      ::
      =stats
      :: subscribers
      ::
      =subscribers
      :: running state
      ::
      running-state=vase
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
~%  %gall-top  ..is  ~
|%
::  +gall-payload:  gall payload
::
++  gall-payload  +
::  +mo: Arvo-level move handling
::
::    An outer core responsible for routing moves to and from Arvo; it calls
::    an inner core, +ap, to route internal moves to and from agents.
::
++  mo
  ~%  %gall-mo  +>  ~
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
    ^-  [(list move) _gall-payload]
    ::
    =/  resolved  (flop moves)
    [resolved gall-payload]
  ::
  ::  +mo-boot: ask %ford to build us a core for the specified agent.
  ::
  ++  mo-boot
    |=  [=term =ship =desk]
    ^+  mo-core
    ::
    =/  =case  [%da now]
    =/  =wire
      =/  ship  (scot %p ship)
      =/  case  (scot case)
      /sys/core/[term]/[ship]/[desk]/[case]
    ::
    =/  =note-arvo
      =/  =schematic:ford  [%core [ship desk] /hoon/[term]/app]
      [%f %build live=%.y schematic]
    ::
    (mo-pass wire note-arvo)
  ::  +mo-pass: prepend a standard %pass to the current list of moves.
  ::
  ++  mo-pass
    |=  pass=(pair wire note-arvo)
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
  ::  +mo-contains-valid-bowl: check that a vase contains a valid bowl.
  ::
  ++  mo-contains-valid-bowl
    ~/  %mo-contains-valid-bowl
    |=  =vase
    ^-  ?
    ::
    =/  maybe-vase  (slew 12 vase)
    ?~  maybe-vase
      %.n
    =/  =type  p.u.maybe-vase
    (~(nest ut type) %.n -:!>(*bowl))
  ::  +mo-receive-core: receives an app core built by %ford.
  ::
  ::    Presuming we receive a good core, we first check to see if the agent
  ::    is already running.  If so, we update its beak in %gall's state,
  ::    initialise an +ap core for the agent, install the core we got from
  ::    %ford, and then resolve any moves associated with it.
  ::
  ::    If we're dealing with a new agent, we create one using the result we
  ::    got from %ford, add it to the collection of agents %gall is keeping
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
    =/  maybe-agent=(unit agent)
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
    ?.  (mo-contains-valid-bowl result-vase)
      =/  err  [[%leaf "{<term>}: bogus core"] ~]
      (mo-give %onto %.n err)
    ::
    =.  mo-core  (mo-new-agent term beak result-vase)
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
  ::  +mo-new-agent: create a new agent and add it to %gall's state.
  ::
  ::    %gall maintains a collection of running agents.  This arm creates a
  ::    new one with the provided name, beak, and state (held in a vase).
  ::
  ++  mo-new-agent
    |=  [=term =beak =vase]
    ^+  mo-core
    ::
    =/  =ducts
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
        ducts           ducts
      ==
    ::
    %_  mo-core
      running.agents.state  (~(put by running.agents.state) term agent)
    ==
  ::  +mo-send-foreign-request: handle local app request to .ship
  ::
  ++  mo-send-foreign-request
    ~/  %mo-send-foreign-request
    |=  [=ship =internal-task]
    ^+  mo-core
    ::
    =/  foreign-agent  p.internal-task
    =/  =agent-action  q.internal-task
    ::
    ?:  ?=(%peer-not -.agent-action)
      (mo-give %unto %reap `p.agent-action)
    ::
    =.  mo-core  (mo-track-rift ship)
    =/  =ames-request
      ?-  -.agent-action
        %poke  [%m p.p.agent-action q.q.p.agent-action]
        %pull  [%u ~]
        %puff  !!
        %punk  !!
        %peel  [%l agent-action]
        %peer  [%s p.agent-action]
      ==
    ::
    =/  =wire
      =/  action  -.agent-action
      /sys/way/[action]
    ::
    =/  =note-arvo
      =/  =path  /ge/[foreign-agent]
      [%a %plea ship %g path ames-request]
    ::
    (mo-pass wire note-arvo)
  ::  +mo-track-rift: ensure we're subscribed to jael for .ship breaches
  ::
  ++  mo-track-rift
    |=  =ship
    ^+  mo-core
    ::  if already contacted, no-op
    ::
    ?:  (~(has in contacts.agents.state) ship)
      mo-core
    ::  first contact; update state and subscribe to jael
    ::
    =.  contacts.agents.state  (~(put in contacts.agents.state) ship)
    =/  =note-arvo  [%j %public-keys (silt ship ~)]
    =.  moves
      [[system-duct.agents.state %pass /sys/jael note-arvo] moves]
    mo-core
  ::  +mo-untrack-rift: cancel jael subscription to .ship's breaches
  ::
  ++  mo-untrack-rift
    |=  =ship
    ^+  mo-core
    ::  if already canceled, no-op
    ::
    ?.  (~(has in contacts.agents.state) ship)
      mo-core
    ::  delete .ship from state and kill subscription
    ::
    =.  contacts.agents.state  (~(del in contacts.agents.state) ship)
    =/  =note-arvo  [%j %nuke (silt ship ~)]
    =.  moves
      [[system-duct.agents.state %pass /sys/jael note-arvo] moves]
    mo-core
  ::  +mo-breach: ship breached, so forget about them
  ::
  ++  mo-breach
    |=  =ship
    ^+  mo-core
    =.  mo-core  (mo-untrack-rift ship)
    =/  agents=(list [name=term =agent])  ~(tap by running.agents.state)
    |-  ^+  mo-core
    ?~  agents
      mo-core
    =.  mo-core
      =/  =routes  [disclosing=~ attributing=ship]
      =/  app  (ap-abed:ap name.i.agents routes)
      ap-abet:(ap-breach:app ship)
    $(agents t.agents)
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
      %jael  (mo-handle-sys-jael path sign-arvo)
      %core  (mo-handle-sys-core path sign-arvo)
      %pel   (mo-handle-sys-pel path sign-arvo)
      %rep   (mo-handle-sys-rep path sign-arvo)
      %req   (mo-handle-sys-req path sign-arvo)
      %val   (mo-handle-sys-val path sign-arvo)
      %way   (mo-handle-sys-way path sign-arvo)
    ==
  ::  +mo-handle-sys-jael: receive update about contact
  ::
  ++  mo-handle-sys-jael
    |=  [=path =sign-arvo]
    ^+  mo-core
    ?>  ?=([%jael ~] path)
    ?>  ?=([%j %public-keys *] sign-arvo)
    ::  TODO reinstate
    ::?.  ?=(%breach -.public-keys-result.sign-arvo)
    ::  mo-core
    ::(mo-breach who.public-keys-result.sign-arvo)
    mo-core
  ::  +mo-handle-sys-core: receive a core from %ford.
  ::
  ++  mo-handle-sys-core
    |=  [=path =sign-arvo]
    ^+  mo-core
    ::
    ?>  ?=([@ @ @ @ @ ~] path)
    ?>  ?=([%f %made *] sign-arvo)
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
    ?>  ?=([%pel @ ~] path)
    ?>  ?=([%f %made *] sign-arvo)
    ::
    ?-    result.sign-arvo
        [%incomplete *]
      (mo-give %unto %coup `tang.result.sign-arvo)
    ::
        [%complete %error *]
      (mo-give %unto %coup `message.build-result.result.sign-arvo)
    ::
        [%complete %success *]
      (mo-give %unto %diff (result-to-cage:ford build-result.result.sign-arvo))
    ==
  ::  +mo-handle-sys-rep: reverse request.
  ::
  ::    On receipt of a valid +sign from %ford, sets state to the appropriate
  ::    duct and gives an internal %diff containing the +sign payload.
  ::
  ++  mo-handle-sys-rep
    |=  [=path =sign-arvo]
    ^+  mo-core
    ::
    ?>  ?=([%rep @ @ *] path)
    ?>  ?=([%f %made *] sign-arvo)
    ::
    ?-    result.sign-arvo
        [%incomplete *]
      (mo-give %done `[%gall-fail tang.result.sign-arvo])
    ::
        [%complete %error *]
      (mo-give %done `[%gall-fail message.build-result.result.sign-arvo])
    ::
        [%complete %success *]
      (mo-give %unto %diff (result-to-cage:ford build-result.result.sign-arvo))
    ==
  ::  +mo-handle-sys-req: TODO description
  ::
  ::    TODO: what should we do if the remote nacks our %pull?
  ::
  ++  mo-handle-sys-req
    |=  [=path =sign-arvo]
    ^+  mo-core
    ::
    ?>  ?=([%req @ @ ~] path)
    =/  him  (slav %p i.t.path)
    =/  dap  i.t.t.path
    ::
    ?>  ?=([%g %unto *] sign-arvo)
    =/  =internal-gift  +>.sign-arvo
    ::
    ?-    -.internal-gift
        %coup
      =/  err=(unit error:ames)
        ?~  p.internal-gift  ~
        `[%coup u.p.internal-gift]
      (mo-give %done err)
    ::
        %diff
      =+  [mark noun]=[p q.q]:p.internal-gift
      (mo-give %boon %d mark noun)
    ::
        %quit
      (mo-give %boon %x ~)
    ::
        %reap
      =/  err=(unit error:ames)
        ?~  p.internal-gift  ~
        `[%reap u.p.internal-gift]
      (mo-give %done err)
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
    ?>  ?=([%val @ @ ~] path)
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
  ::  +mo-handle-sys-way: handle response to outgoing remote request
  ::
  ++  mo-handle-sys-way
    |=  [=path =sign-arvo]
    ^+  mo-core
    ::
    ?>  ?=([%way @ *] path)
    ::
    ?+    sign-arvo  !!
        [%a %done *]
      ::
      =/  =remote-request  ;;(remote-request i.t.path)
      =/  err=(unit tang)
        ?~  error=error.sign-arvo
          ~
        `[[%leaf (trip tag.u.error)] tang.u.error]
      ::
      ?-  remote-request
        %peel  (mo-give %unto %reap err)
        %peer  (mo-give %unto %reap err)
        %poke  (mo-give %unto %coup err)
        %pull  mo-core
      ==
    ::
        [%a %boon *]
      ::
      ?>  ?=([@ @ ~] t.t.path)
      =/  him              (slav %p i.t.path)
      =/  agent-name       `@tas`i.t.t.path
      ::
      =/  =ames-response  ;;(ames-response payload.sign-arvo)
      (mo-handle-ames-response him agent-name ames-response)
    ::
        [%a %lost *]
      ::
      ?>  ?=([@ @ ~] t.t.path)
      =/  him              (slav %p i.t.path)
      =/  agent-name       `@tas`i.t.t.path
      ::
      =/  sys-wire  [%sys path]
      ::  TODO: %drip %quit so app crash can't kill the remote %pull
      ::
      =.  mo-core  (mo-give %unto %quit ~)
      =.  mo-core  (mo-pass sys-wire %a %plea him %g /ge/[agent-name] %u ~)
      mo-core
    ==
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
      ?.  ?=([%g %unto *] sign-arvo)
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
      =/  card  [%slip %g %deal sock internal-task]
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
    |=  [agent=term =routes =agent-action]
    ^+  mo-core
    ::
    =/  =path
      =/  ship  (scot %p attributing.routes)
      /sys/val/[ship]/[agent]
    ::
    =/  ship-desk
      =/  =beak  (mo-beak agent)
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
    =/  app  (ap-abed:ap agent routes)
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
    =/  agent  p.internal-task
    =/  =agent-action  q.internal-task
    =/  is-running  (~(has by running.agents.state) agent)
    =/  is-blocked  (~(has by blocked.agents.state) agent)
    ::
    ?:  |(!is-running is-blocked)
      =/  =blocked
        =/  waiting  (~(get by blocked.agents.state) agent)
        =/  tasks  (fall waiting *blocked)
        =/  task  [hen routes agent-action]
        (~(put to tasks) task)
      ::
      %_  mo-core
        blocked.agents.state  (~(put by blocked.agents.state) agent blocked)
      ==
    (mo-apply agent routes agent-action)
  ::  +mo-handle-ames-request: handle ames request message
  ::
  ++  mo-handle-ames-request
    |=  [=ship agent-name=term =ames-request]
    ^+  mo-core
    ::  %u/%pull gets automatically acked
    ::
    =?  mo-core  ?=(%u -.ames-request)  (mo-give %done ~)
    ::
    =/  =wire  /sys/req/(scot %p ship)/[agent-name]
    ::
    =/  =agent-action
      ?-  -.ames-request
        %m  [%puff [mark noun]:ames-request]
        %l  [%peel [mark path]:ames-request]
        %s  [%peer path.ames-request]
        %u  [%pull ~]
      ==
    (mo-pass wire %g %deal [ship our] agent-name agent-action)
  ::  +mo-handle-ames-response: handle ames response message
  ::
  ++  mo-handle-ames-response
    |=  [=ship agent-name=term =ames-response]
    ^+  mo-core
    ::
    ?-    -.ames-response
        ::  %d: diff; ask ford to validate .noun as .mark
        ::
        %d
      =/  =wire
        /sys/rep/(scot %p ship)/[agent-name]
      ::
      =/  =note-arvo
        =/  =disc:ford  [p q]:(mo-beak agent-name)
        [%f %build live=%.n %vale disc [mark noun]:ames-response]
      ::
      (mo-pass wire note-arvo)
    ::
        ::  %x: quit; tell agent the publisher canceled the subscription
        ::
        %x
      (mo-give %unto %quit ~)
    ==
  ::  +ap: agent engine
  ::
  ::    An inner, agent-level core.  The sample refers to the agent we're
  ::    currently focused on.
  ::
  ++  ap
    ~%  %gall-ap  +>  ~
    |_  $:  agent-name=term
            agent-routes=routes
            agent-bone=bone
            agent-moves=(list internal-move)
            agent-config=(list (each suss tang))
            current-agent=agent
        ==
    ++  ap-core  .
    ::  +ap-abed: initialise state for an agent, with the supplied routes.
    ::
    ::    The agent must already be running in +gall -- here we simply update
    ::    +ap's state to focus on it.
    ::
    ++  ap-abed
      ~/  %ap-abed
      |=  [=term =routes]
      ^+  ap-core
      ::
      =/  =agent
        =/  running  (~(got by running.agents.state) term)
        =/  =stats
          :+  +(change.stats.running)
            (shaz (mix (add term change.stats.running) eny))
          now
        running(stats stats)
      ::
      =.  agent-name  term
      =.  agent-routes  routes
      =.  current-agent  agent
      =/  maybe-bone  (~(get by bone-map.ducts.agent) hen)
      ?^  maybe-bone
        ap-core(agent-bone u.maybe-bone)
      ::
      =/  =ducts
        :+  +(bone.ducts.agent)
          (~(put by bone-map.ducts.agent) hen bone.ducts.agent)
        (~(put by duct-map.ducts.agent) bone.ducts.agent hen)
      ::
      %_  ap-core
        agent-bone           bone.ducts.agent
        ducts.current-agent  ducts
      ==
    ::  +ap-abet: resolve moves.
    ::
    ++  ap-abet
      ^+  mo-core
      ::
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
      ~|  [%gall-move-conversion-failed internal-move]
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
              [%g task]
            ::
                %meta
              =/  =term  term.internal-note
              =/  =vase  vase.internal-note
              [term %meta vase]
            ==
          [%pass use-path note-arvo]
        ==
      [duct card]
    ::  +ap-breach: ship breached, so forget about them
    ::
    ++  ap-breach
      |=  =ship
      ^+  ap-core
      =/  in=(list [=bone =^ship =path])
        ~(tap by incoming.subscribers.current-agent)
      |-  ^+  ap-core
      ?^  in
        =?  ap-core  =(ship ship.i.in)
          =/  core  ap-load-delete(agent-bone bone.i.in)
          core(agent-bone agent-bone)
        $(in t.in)
      ::
      =/  out=(list [[=bone =wire] ? =^ship =path])
        ~(tap by outgoing.subscribers.current-agent)
      |-  ^+  ap-core
      ?~  out
        ap-core
      =?  ap-core  =(ship ship.i.out)
        =/  core  (ap-specific-take(agent-bone bone.i.out) wire.i.out %quit ~)
        core(agent-bone agent-bone)
      $(out t.out)
    ::  +ap-call: call into server.
    ::
    ++  ap-call
      ~/  %ap-call
      |=  [=term =vase]
      ^-  [(unit tang) _ap-core]
      ::
      =.  ap-core  ap-construct-bowl
      =^  arm  ap-core  (ap-produce-arm term)
      ?:  ?=(%.n -.arm)
        [(some p.arm) ap-core]
      ::
      =^  arm  ap-core  (ap-slam term p.arm vase)
      ?:  ?=(%.n -.arm)
        [(some p.arm) ap-core]
      (ap-handle-result p.arm)
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
        ?>  ?=(^ path)
        [mark=i.path tyl=(flop t.path)]
      ::
      =/  =mark  mark.marked
      =/  tyl  tyl.marked
      =^  maybe-arm  ap-core  (ap-find-arm %peek term tyl)
      ::
      ?~  maybe-arm
        =/  =tank  [%leaf "peek find fail"]
        =/  print  (slog tank >tyl< >mark< ~)
        (print [~ ~])
      ::
      =^  arm  ap-core  (ap-produce-arm q.u.maybe-arm)
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
          !>  (slag index path)
        (ap-slam term p.arm vase)
      ::
      =^  possibly-vase  ap-core  slammed
      ?:  ?=(%.n -.possibly-vase)
        =/  =tank  [%leaf "peek slam fail"]
        =/  print  (slog tank p.possibly-vase)
        (print [~ ~])
      ::
      =/  slammed-vase  p.possibly-vase
      =/  vase-value  q.slammed-vase
      =/  err
        |.
        =/  =tank  [%leaf "peek bad result"]
        =/  print  (slog tank ~)
        (print [~ ~])
      ::
      ?+  vase-value  $:err
          ~
        ~
      ::
          [~ ~]
        [~ ~]
      ::
          [~ ~ ^]
        =/  =vase  (sped (slot 7 slammed-vase))
        ::
        ?.  ?=([p=@ *] q.vase)
          =/  =tank  [%leaf "scry: malformed cage"]
          =/  print  (slog tank ~)
          (print [~ ~])
        ::
        ?.  ((sane %tas) p.q.vase)
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
      ==
    ::  +ap-diff: pour a diff.
    ::
    ++  ap-diff
      ~/  %ap-diff
      |=  [=ship =path =cage]
      ^+  ap-core
      ::
      =/  rest  +.path
      =/  pax  [p.cage rest]
      =^  maybe-arm  ap-core  (ap-find-arm %diff pax)
      ::
      ?~  maybe-arm
        =/  target  [%.n ship rest]
        =/  =tang
          =/  why  "diff: no {<[p.cage rest]>}"
          (ap-tang why)
        ::
        =.  ap-core  (ap-lame %diff tang)
        (ap-update-subscription target)
      ::
      =/  arm  u.maybe-arm
      =/  =vase
        =/  target
          ?:  =(0 p.arm)
            =/  =vase  (ap-cage cage)
            [!>(rest) vase]
          [!>((slag (dec p.arm) rest)) q.cage]
        (slop target)
      ::
      =^  called  ap-core  (ap-call q.arm vase)
      ?^  called
        =.  ap-core  (ap-lame q.arm u.called)
        (ap-update-subscription %.n ship path)
      (ap-update-subscription %.y ship path)
    ::  +ap-cage: cage to tagged vase.
    ::
    ++  ap-cage
      |=  =cage
      ^-  vase
      ::
      =/  =type  [%atom %tas (some p.cage)]
      =/  =vase  [type p.cage]
      (slop vase q.cage)
    ::  +ap-update-subscription: update subscription.
    ::
    ++  ap-update-subscription
      ~/  %ap-update-subscription
      |=  [is-ok=? =ship =path]
      ^+  ap-core
      ::
      ?:  is-ok
        ap-core
      ::  not ok; kill in both directions with %quit and %pull
      ::
      =.  ap-core  (ap-give %quit ~)
      ::
      =/  way  [(scot %p ship) %out path]
      =/  =internal-note  [%send ship -.path %pull ~]
      (ap-pass way internal-note)
    ::  +ap-produce-arm: produce arm.
    ::
    ++  ap-produce-arm
      ~/  %ap-produce-arm
      |=  =term
      ^-  [(each vase tang) _ap-core]
      ::
      =/  compiled
        =/  =type  p.running-state.current-agent
        =/  =hoon  [%limb term]
        (~(mint wa cache.current-agent) type hoon)
      ::
      =/  virtual
        =/  trap  |.(compiled)
        (mule trap)
      ::
      ?:  ?=(%.n -.virtual)
        =/  =tang  p.virtual
        [[%.n tang] ap-core]
      ::
      =/  possibly-vase=(each vase tang)
        =/  value  q.running-state.current-agent
        =/  ton  (mock [value q.+<.virtual] ap-namespace-view)
        ?-  -.ton
          %0  [%.y p.+<.virtual p.ton]
          %1  [%.n (turn p.ton |=(a=* (smyt (path a))))]
          %2  [%.n p.ton]
        ==
      ::
      =/  next
        =/  =worm  +>.virtual
        ap-core(cache.current-agent worm)
      ::
      [possibly-vase next]
    ::  +ap-find-arm: general arm.
    ::
    ++  ap-find-arm
      ~/  %ap-find-arm
      |=  [=term =path]
      ^-  [(unit (pair @ud @tas)) _ap-core]
      ::
      =/  maybe-cached  (~(get by arm-cache.current-agent) [term path])
      ?^  maybe-cached
        [u.maybe-cached ap-core]
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
      =.  arm-cache.current-agent
        (~(put by arm-cache.current-agent) [term path] result)
      [result ap-core]
    ::  +ap-exists-arm: check for an arm in the running agent state.
    ::
    ++  ap-exists-arm
      ~/  %ap-exists-arm
      |=  =term
      ^-  ?
      ::
      =/  =type  p.running-state.current-agent
      (slob term type)
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
      ^+  ap-core
      ::
      %_    ap-core
          +12.q.running-state.current-agent
        ^-   bowl
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
      ==
    ::  +ap-move: process each move.
    ::
    ++  ap-move
      ~/  %ap-move
      |=  =vase
      ^-  [(each internal-move tang) _ap-core]
      ::
      =/  noun  q.vase
      ?@  noun
        =/  =tang  (ap-tang "move: invalid move (atom)")
        [[%.n tang] ap-core]
      ::
      ?^  -.noun
        =/  =tang  (ap-tang "move: invalid move (bone)")
        [[%.n tang] ap-core]
      ::
      ?@  +.noun
        =/  =tang  (ap-tang "move: invalid move (card)")
        [[%.n tang] ap-core]
      ::
      =/  =bone  -.noun
      =/  has-duct  (~(has by duct-map.ducts.current-agent) bone)
      ?.  &(has-duct !=(0 bone))
        =/  =tang  (ap-tang "move: invalid card (bone {<bone>})")
        [[%.n tang] ap-core]
      ::
      =^  vase  cache.current-agent  (~(spot wa cache.current-agent) 3 vase)
      =^  vase  cache.current-agent  (~(slot wa cache.current-agent) 3 vase)
      ?+  +<.noun  (ap-move-pass bone +<.noun vase)
        %diff  (ap-move-diff bone vase)
        %peel  (ap-move-peel bone vase)
        %peer  (ap-move-peer bone vase)
        %pull  (ap-move-pull bone vase)
        %poke  (ap-move-poke bone vase)
        %send  (ap-move-send bone vase)
        %quit  (ap-move-quit bone vase)
        %http-response  (ap-move-http-response bone vase)
      ==
    ::  +ap-move-quit: give quit move.
    ::
    ++  ap-move-quit
      ~/  %quit
      |=  [=bone =vase]
      ^-  [(each internal-move tang) _ap-core]
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
        =/  incoming  (~(del by incoming.subscribers.current-agent) bone)
        %_  ap-core
          incoming.subscribers.current-agent  incoming
        ==
      [possibly-internal-move next]
    ::  +ap-move-diff: give diff move.
    ::
    ++  ap-move-diff
      ~/  %diff
      |=  [=bone =vase]
      ^-  [(each internal-move tang) _ap-core]
      ::
      =^  vase  cache.current-agent  (~(sped wa cache.current-agent) vase)
      =/  value  q.vase
      ?.  ?&  ?=(^ value)
              ?=(@ -.value)
              ((sane %tas) -.value)
          ==
        =/  =tang  (ap-tang "diff: improper give")
        [[%.n tang] ap-core]
      ::
      =^  vase  cache.current-agent  (~(slot wa cache.current-agent) 3 vase)
      =/  =internal-move
        =/  =cage  [-.value vase]
        =/  move  [%give %diff cage]
        [bone move]
      [[%.y internal-move] ap-core]
    ::  +ap-move-http-response
    ::
    ++  ap-move-http-response
      |=  [sto=bone vax=vase]
      ^-  [(each internal-move tang) _ap-core]
      ::  TODO: Magic vase validation. I have no idea how malformed checking
      ::  works.
      ::
      :_  ap-core
      [%& sto %give %http-response ;;(http-event:http q.vax)]
    ::  +ap-move-mess: extract path, target.
    ::
    ++  ap-move-mess
      ~/  %mess
      |=  =vase
      ^-  [(each (trel path ship term) tang) _ap-core]
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
      [possibly-trel ap-core]
    ::  +ap-move-pass: pass general move.
    ::
    ++  ap-move-pass
      ~/  %pass
      |=  [=bone =noun =vase]
      ^-  [(each internal-move tang) _ap-core]
      ::
      ?.  ?&  ?=(@ noun)
              ((sane %tas) noun)
          ==
        =/  =tang  (ap-tang "pass: malformed card")
        [[%.n tang] ap-core]
      ::
      =/  pax  ((soft path) -.q.vase)
      ?.  ?&  ?=(^ pax)
              (levy u.pax (sane %ta))
          ==
        =/  =tang  (ap-tang "pass: malformed path")
        ~&  [%bad-path pax]
        [[%.n tang] ap-core]
      ::
      =/  maybe-vane  (ap-vain noun)
      ?~  maybe-vane
        =/  =tang  (ap-tang "move: unknown note {(trip noun)}")
        [[%.n tang] ap-core]
      ::
      =/  vane  u.maybe-vane
      =^  at-slot  cache.current-agent
        (~(slot wa cache.current-agent) 3 vase)
      =/  =internal-move
        =/  =path  [(scot %p attributing.agent-routes) %inn u.pax]
        =/  vase  (ap-atomic-vase %tas noun)
        =/  combined  (slop vase at-slot)
        =/  =internal-note  [%meta vane combined]
        =/  card  [%pass path internal-note]
        [bone card]
      [[%.y internal-move] ap-core]
    ::  +ap-move-poke: pass %poke.
    ::
    ++  ap-move-poke
      ~/  %poke
      |=  [=bone =vase]
      ^-  [(each internal-move tang) _ap-core]
      ::
      =^  possibly-target  ap-core  (ap-move-mess vase)
      ::
      ?:  ?=(%.n -.possibly-target)
        [possibly-target ap-core]
      ::
      =^  at-slot  cache.current-agent
        (~(slot wa cache.current-agent) 7 vase)
      ::
      ?.  ?&  ?=([p=@ q=*] q.at-slot)
              ((sane %tas) p.q.at-slot)
          ==
        =/  =tang  (ap-tang "poke: malformed cage")
        [[%.n tang] ap-core]
      ::
      =^  specialised  cache.current-agent
        (~(stop wa cache.current-agent) 3 at-slot)
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
      [[%.y internal-move] ap-core]
    ::  +ap-move-peel: pass %peel.
    ::
    ++  ap-move-peel
      ~/  %peel
      |=  [=bone =vase]
      ^-  [(each internal-move tang) _ap-core]
      ::
      =^  possibly-target  ap-core  (ap-move-mess vase)
      ?:  ?=(%.n -.possibly-target)
        [possibly-target ap-core]
      ::
      =/  target  p.possibly-target
      =/  =ship  q.target
      =/  =term  r.target
      =/  mark  ((soft mark) +>-.q.vase)
      ?~  mark
        =/  =tang  (ap-tang "peel: malformed mark")
        [[%.n tang] ap-core]
      ::
      =/  pax  ((soft path) +>+.q.vase)
      ::
      ?.  ?&  ?=(^ pax)
              (levy u.pax (sane %ta))
          ==
        =/  =tang  (ap-tang "peel: malformed path")
        [[%.n tang] ap-core]
      ::
      =/  move
        ?:  (~(has in misvale.current-agent) p.target)
          =/  =internal-task
            =/  =tang  [[%leaf "peel: misvalidation encountered"] ~]
            =/  =agent-action  [%peer-not tang]
            [term agent-action]
          =/  =internal-note  [%send ship internal-task]
          =/  card  [%pass p.target internal-note]
          [bone card]
        ::
        =/  =agent-action  [%peel u.mark u.pax]
        =/  =internal-task  [term agent-action]
        =/  =internal-note  [%send ship internal-task]
        =/  card  [%pass p.target internal-note]
        [bone card]
      [[%.y move] ap-core]
    ::  +ap-move-peer: pass %peer.
    ::
    ++  ap-move-peer
      ~/  %peer
      |=  [=bone =vase]
      ^-  [(each internal-move tang) _ap-core]
      ::
      =^  possibly-target  ap-core  (ap-move-mess vase)
      ?:  ?=(%.n -.possibly-target)
        [possibly-target ap-core]
      ::
      =/  target  p.possibly-target
      =/  =ship  q.target
      =/  =term  r.target
      =/  pax  ((soft path) +>.q.vase)
      ?.  ?&  ?=(^ pax)
              (levy u.pax (sane %ta))
          ==
        =/  =tang  (ap-tang "peer: malformed path")
        [[%.n tang] ap-core]
      ::
      =/  move
        ?:  (~(has in misvale.current-agent) p.target)
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
      [[%.y move] ap-core]
    ::  +ap-move-pull: pass %pull.
    ::
    ++  ap-move-pull
      ~/  %pull
      |=  [=bone =vase]
      ^-  [(each internal-move tang) _ap-core]
      ::
      =^  possibly-target  ap-core  (ap-move-mess vase)
      ?:  ?=(%.n -.possibly-target)
        [possibly-target ap-core]
      ::
      =/  target  p.possibly-target
      =/  =ship  q.target
      =/  =term  r.target
      ?.  =(~ +>.q.vase)
        =/  =tang  (ap-tang "pull: malformed card")
        [[%.n tang] ap-core]
      ::
      =/  move
        =/  =agent-action  [%pull ~]
        =/  =internal-note  [%send ship term agent-action]
        =/  card  [%pass p.target internal-note]
        [bone card]
      ::
      [[%.y move] ap-core]
    ::  +ap-move-send: pass gall action.
    ::
    ++  ap-move-send
      ~/  %send
      |=  [=bone =vase]
      ^-  [(each internal-move tang) _ap-core]
      ::
      ?.  ?&  ?=([p=* [q=@ r=@] [s=@ t=*]] q.vase)
              (gte 1 (met 7 q.q.vase))
              ((sane %tas) r.q.vase)
          ==
        =/  =tang
          (ap-tang "send: improper ask.[%send wire gill agent-action]")
        [[%.n tang] ap-core]
      ::
      =/  pax  ((soft path) p.q.vase)
      ?.  ?&  ?=(^ pax)
              (levy u.pax (sane %ta))
          ==
        =/  =tang  (ap-tang "send: malformed path")
        [[%.n tang] ap-core]
      ::
      ?:  ?=($poke s.q.vase)
        =^  specialised  cache.current-agent
          (~(spot wa cache.current-agent) 7 vase)
        ::
        ?>  =(%poke -.q.specialised)
        ::
        ?.  ?&  ?=([p=@ q=*] t.q.vase)
                ((sane %tas) p.t.q.vase)
            ==
          =/  =tang  (ap-tang "send: malformed poke")
          [[%.n tang] ap-core]
        ::
        =^  specialised  cache.current-agent
          (~(spot wa cache.current-agent) 3 specialised)
        =^  at-slot  cache.current-agent
          (~(slot wa cache.current-agent) 3 specialised)
        ::
        =/  move
          =/  =agent-action  [%poke p.t.q.vase at-slot]
          =/  =internal-note  [%send q.q.vase r.q.vase agent-action]
          =/  =path  [(scot %p q.q.vase) %out r.q.vase u.pax]
          =/  card  [%pass path internal-note]
          [bone card]
        ::
        [[%.y move] ap-core]
      ::
      =/  maybe-action  ((soft agent-action) [s t]:q.vase)
      ?~  maybe-action
        =/  =tang  (ap-tang "send: malformed agent-action")
        [[%.n tang] ap-core]
      ::
      =/  move
        =/  =agent-action  u.maybe-action
        =/  =internal-note  [%send q.q.vase r.q.vase agent-action]
        =/  =path  [(scot %p q.q.vase) %out r.q.vase u.pax]
        =/  card  [%pass path internal-note]
        [bone card]
      ::
      [[%.y move] ap-core]
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
      =/  prep
        =/  installed  ap-install(running-state.current-agent vase)
        =/  running  (some running-state.current-agent)
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
      =^  maybe-arm  ap-core  (ap-find-arm %peer pax)
      ?~  maybe-arm
        ap-core
      ::
      =/  arm  u.maybe-arm
      =/  =vase  !>((slag p.arm pax))
      =/  old  agent-moves
      =.  agent-moves  ~
      =^  maybe-tang  ap-core  (ap-call q.arm vase)
      =/  internal-moves=(list internal-move)
        =/  move  [agent-bone %give %reap maybe-tang]
        [move old]
      ::
      =.  agent-moves  (weld agent-moves internal-moves)
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
      =^  maybe-arm  ap-core  (ap-find-arm %poke p.cage ~)
      ?~  maybe-arm
        =/  =tang  (ap-tang "no poke arm for {(trip p.cage)}")
        (ap-give %coup (some tang))
      ::
      =/  arm  u.maybe-arm
      =/  =vase
        =/  vas  (ap-atomic-vase %tas p.cage)
        ?.  =(0 p.arm)
          q.cage
        (slop vas q.cage)
      ::
      =^  tur  ap-core  (ap-call q.arm vase)
      (ap-give %coup tur)
    ::  +ap-lame: pour error.
    ::
    ++  ap-lame
      |=  [=term =tang]
      ^+  ap-core
      ::
      =^  maybe-arm  ap-core  (ap-find-arm /lame)
      =/  form  |=(=tank [%rose [~ "! " ~] tank ~])
      ?~  maybe-arm
        =/  tang  [>%ap-lame agent-name term< (turn tang form)]
        ~>  %slog.`rose+["  " "[" "]"]^(flop tang)
        ap-core
      ::
      =/  arm  u.maybe-arm
      =/  =vase  !>([term tang])
      =^  maybe-tang  ap-core  (ap-call q.arm vase)
      ?^  maybe-tang
        =/  tang  u.maybe-tang
        =/  etc  (flop [>%ap-lame-lame< (turn tang form)])
        ~>  %slog.`rose+["  " "[" "]"]^(welp etc [%leaf "." (flop tang)])
        ap-core
      ::
      ap-core
    ::  +ap-misvale: broken vale.
    ::
    ++  ap-misvale
      |=  =wire
      ^+  ap-core
      ::
      ~&  [%ap-blocking-misvale wire]
      =/  misvaled  (~(put in misvale.current-agent) wire)
      ap-core(misvale.current-agent misvaled)
    ::  +ap-generic-take: generic take.
    ::
    ++  ap-generic-take
      ~/  %ap-generic-take
      |=  [=path =vase]
      ^+  ap-core
      ::
      ?.  &(?=([@ *] q.vase) ((sane %tas) -.q.vase))
        =/  =tang  (ap-tang "pour: malformed card")
        (ap-lame %pour tang)
      ::
      =/  =term  -.q.vase
      =^  maybe-arm  ap-core  (ap-find-arm [term path])
      ?~  maybe-arm
        =/  =tang  (ap-tang "pour: no {(trip -.q.vase)}: {<path>}")
        (ap-lame term tang)
      ::
      =/  arm  u.maybe-arm
      =^  at-slot  cache.current-agent
        (~(slot wa cache.current-agent) 3 vase)
      =/  vase  (slop !>((slag p.arm path)) at-slot)
      ::
      =^  maybe-tang  ap-core  (ap-call q.arm vase)
      ?^  maybe-tang
        (ap-lame term u.maybe-tang)
      ap-core
    ::  +ap-unwrap-take: unwrap take.
    ::
    ++  ap-unwrap-take
      ~/  %ap-unwrap-take
      |=  [=term pax=path =cage]
      ^+  ap-core
      ::
      =^  maybe-arm  ap-core  (ap-find-arm [term p.cage pax])
      ::
      ?~  maybe-arm
        =/  =tang  (ap-tang "{(trip term)}: no {<`path`[p.cage pax]>}")
        (ap-lame term tang)
      ::
      =/  arm  u.maybe-arm
      =/  =vase
        %-  slop
        ?:  =(0 p.arm)
          =/  =vase  (ap-cage cage)
          [!>(`path`pax) vase]
        [!>((slag (dec p.arm) `path`pax)) q.cage]
      ::
      =^  maybe-tang  ap-core  (ap-call q.arm vase)
      ?^  maybe-tang
        (ap-lame q.arm u.maybe-tang)
      ap-core
    ::  +ap-specific-take: specific take.
    ::
    ++  ap-specific-take
      |=  [=path =internal-gift]
      ^+  ap-core
      ::
      =/  pax  +.path
      ?-  -.internal-gift
          %coup
        =/  maybe-vase  (some !>(p.internal-gift))
        (ap-non-diff-take %coup pax maybe-vase)
      ::
          %diff
        =/  =ship  attributing.agent-routes
        =/  =cage  p.internal-gift
        (ap-diff ship path cage)
      ::
          %quit
        (ap-non-diff-take %quit pax ~)
      ::
          %reap
        =/  maybe-vase  (some !>(p.internal-gift))
        (ap-non-diff-take %reap pax maybe-vase)
      ::
          %http-response
        !!
      ==
    ::  +ap-install: install wrapper.
    ::
    ++  ap-install
      |=  maybe-vase=(unit vase)
      ^-  [(unit tang) _ap-core]
      ::
      =^  maybe-tang  ap-core  (ap-prep maybe-vase)
      =/  new-misvale-data
         ~?  !=(misvale.current-agent *misvale-data)
           [%misvale-drop misvale.current-agent]
         :: new app might mean new marks
         *misvale-data
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
        %=  ap-core
          misvale.current-agent    new-misvale-data
          agent-config             new-agent-config
          arm-cache.current-agent  ~
        ==
      ::
      [maybe-tang next]
    ::  +ap-prep: low-level install.
    ::
    ++  ap-prep
      ~/  %ap-prep
      |=  maybe-vase=(unit vase)
      ^-  [(unit tang) _ap-core]
      ::
      ?.  (ap-exists-arm %prep)
        ?~  maybe-vase
          [~ ap-core]
        ::
        =/  new-type
          =/  new  (slot 13 running-state.current-agent)
          p.new
        ::
        =/  old-type
          =/  old  (slot 13 u.maybe-vase)
          p.old
        ::
        ?.  (~(nest ut new-type) %.n old-type)
          =/  =tang  (ap-tang "prep mismatch")
          [(some tang) ap-core]
        ::
        =/  next
          ap-core(+13.q.running-state.current-agent +13.q.u.maybe-vase)
        [~ next]
      ::
      =/  =vase
        ?~  maybe-vase
          !>(~)
        (slop !>(~) (slot 13 u.maybe-vase))
      ::
      (ap-call %prep vase)
    ::  +ap-silent-delete: silent delete.
    ::
    ++  ap-silent-delete
      ^+  ap-core
      ::
      %=    ap-core
          incoming.subscribers.current-agent
        (~(del by incoming.subscribers.current-agent) agent-bone)
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
      ::
      =^  maybe-arm  ap-core  (ap-find-arm %pull q.incoming)
      ?~  maybe-arm
        ap-core
      ::
      =/  arm  u.maybe-arm
      =/  =vase  !>((slag p.arm q.incoming))
      =^  maybe-tang  ap-core  (ap-call q.arm vase)
      ?^  maybe-tang
        (ap-lame q.arm u.maybe-tang)
      ap-core
    ::  +ap-kill: queue kill.
    ::
    ++  ap-kill
      ^+  ap-core
      ::
      =>  ap-load-delete
      (ap-give %quit ~)
    ::  +ap-non-diff-take: non-diff gall take.
    ::
    ++  ap-non-diff-take
      ~/  %ap-non-diff-take
      |=  [=term =path maybe-vase=(unit vase)]
      ^+  ap-core
      ::
      =^  maybe-arm  ap-core  (ap-find-arm term path)
      ?~  maybe-arm
        ap-core
      ::
      =/  arm  u.maybe-arm
      =/  =vase
        =/  vax  !>((slag p.arm path))
        ?~  maybe-vase
          vax
        (slop vax u.maybe-vase)
      ::
      =^  maybe-tang  ap-core  (ap-call q.arm vase)
      ?^  maybe-tang
        (ap-lame q.arm u.maybe-tang)
      ap-core
    ::  +ap-safe: process move list.
    ::
    ++  ap-safe
      ~/  %ap-safe
      |=  =vase
      ^-  [(each (list internal-move) tang) _ap-core]
      ::
      ?~  q.vase
        [[%.y ~] ap-core]
      ::
      ?@  q.vase
        =/  =tang  (ap-tang "move: malformed list")
        [[%.n tang] ap-core]
      ::
      =^  hed  cache.current-agent  (~(slot wa cache.current-agent) 2 vase)
      =^  possibly-internal-move  ap-core  (ap-move hed)
      ?:  ?=(%.n -.possibly-internal-move)
        [possibly-internal-move ap-core]
      ::
      =/  =internal-move  p.possibly-internal-move
      =^  tel  cache.current-agent  (~(slot wa cache.current-agent) 3 vase)
      =^  res  ap-core  $(vase tel)
      =/  possibly-internal-moves
        ?:  ?=(%.n -.res)
          res
        [%.y [internal-move p.res]]
      ::
      [possibly-internal-moves ap-core]
    ::  +ap-handle-result: handle result.
    ::
    ++  ap-handle-result
      ~/  %ap-handle-result
      |=  =vase
      ^-  [(unit tang) _ap-core]
      ::
      ?:  ?=(@ q.vase)
        =/  =tang  (ap-tang "ap-handle-result: invalid product (atom)")
        [(some tang) ap-core]
      ::
      =^  hed  cache.current-agent  (~(slot wa cache.current-agent) 2 vase)
      =^  possibly-internal-moves  ap-core  (ap-safe hed)
      ?:  ?=(%.n -.possibly-internal-moves)
        =/  =tang  p.possibly-internal-moves
        [(some tang) ap-core]
      ::
      =/  internal-moves  p.possibly-internal-moves
      =^  tel  cache.current-agent  (~(slot wa cache.current-agent) 3 vase)
      =^  possibly-vase  ap-core  (ap-verify-core tel)
      ::
      ?:  ?=(%.n -.possibly-vase)
        =/  =tang  p.possibly-vase
        [(some tang) ap-core]
      ::
      =/  next
        %_  ap-core
          agent-moves  (weld (flop internal-moves) agent-moves)
          running-state.current-agent  p.possibly-vase
        ==
      ::
      [~ next]
    ::  +ap-verify-core: verify core.
    ::
    ++  ap-verify-core
      ~/  %ap-verify-core
      |=  vax=vase
      ^-  [(each vase tang) _ap-core]
      ::
      =/  received-type  p.vax
      =/  running-type  p.running-state.current-agent
      =^  nests  cache.current-agent
        (~(nest wa cache.current-agent) running-type received-type)
      ::
      =/  possibly-vase
        ?.  nests
          =/  =tang  (ap-tang "invalid core")
          [%.n tang]
        [%.y vax]
      ::
      [possibly-vase ap-core]
    ::  +ap-slam: virtual slam.
    ::
    ++  ap-slam
      ~/  %ap-slam
      |=  [=term gat=vase arg=vase]
      ^-  [(each vase tang) _ap-core]
      ::
      =/  compiled
        =/  =type  [%cell p.gat p.arg]
        =/  =hoon  [%cnsg [%$ ~] [%$ 2] [%$ 3] ~]
        (~(mint wa cache.current-agent) type hoon)
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
        (print [[%.n tang] ap-core])
      ::
      =/  =worm  +>.virtual
      =/  =vase  +<.virtual
      =/  =type  p.vase
      =/  nock  q.vase
      =/  ton  (mock [[q.gat q.arg] nock] ap-namespace-view)
      =/  possibly-vase
        ?-  -.ton
          %0  [%.y type p.ton]
          %1  [%.n (turn p.ton |=(a=* (smyt (path a))))]
          %2  [%.n p.ton]
        ==
      ::
      =/  next  ap-core(cache.current-agent worm)
      [possibly-vase next]
    ::  +ap-namespace-view: namespace view.
    ::
    ++  ap-namespace-view  (sloy ska)
    ::  +ap-tang: standard tang.
    ::
    ++  ap-tang
      |=  =tape
      ^-  tang
      ::
      =/  =tank  [%leaf (weld "gall: {<agent-name>}: " tape)]
      [tank ~]
    ::  +ap-atomic-vase: atomic vase.
    ::
    ++  ap-atomic-vase
      |=  [=term =atom]
      ^-  vase
      ::
      =/  =type  [%atom term (some atom)]
      [type atom]
    ::  +ap-vain: card to vane.
    ::
    ++  ap-vain
      |=  =term
      ^-  (unit @tas)
      ::
      ?+  term  ~&  [%ap-vain term]
          ~
        %bonk            `%a
        %build           `%f
        %cash            `%a
        %conf            `%g
        %cred            `%c
        %crew            `%c
        %crow            `%c
        %deal            `%g
        %dirk            `%c
        %drop            `%c
        %flog            `%d
        %info            `%c
        %keep            `%f
        %kill            `%f
        %knob            `%d
        %look            `%j
        %listen          `%j
        %merg            `%c
        %mont            `%c
        %moon            `%j
        %nuke            `%a
        %ogre            `%c
        %perm            `%c
        %rest            `%b
        %rekey           `%j
        %wait            `%b
        %plea            `%a
        %warp            `%c
        %wash            `%g
        %wipe            `%f
        %request         `%i
        %cancel-request  `%i
        %serve           `%e
        %connect         `%e
        %disconnect      `%e
        %rule            `%e
      ==
    --
  --
::  +call: request
::
++  call
  ~%  %gall-call  +>   ~
  |=  [=duct hic=(hypo (hobo task:able))]
  ^-  [(list move) _gall-payload]
  ::
  ~|  [%gall-call-failed duct q.hic]
  ::  make sure our task is hard
  ::
  =/  =task:able
    ?.  ?=(%soft -.q.hic)
      q.hic
    ;;  task:able  p.q.hic
  ::
  =/  initialised  (mo-abed:mo duct)
  ?-    -.task
      %conf
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
    =/  =sock  p.task
    =/  =internal-task  q.task
    ?.  =(q.sock our)
      ?>  =(p.sock our)
      =>  (mo-send-foreign-request:initialised q.sock internal-task)
      mo-abet
    ::
    =>  (mo-handle-local:initialised p.sock internal-task)
    mo-abet
  ::
      %init
    =/  payload  gall-payload(system-duct.agents.state duct)
    [~ payload]
  ::
      %trim
    ::  reuse %wash task to clear caches on memory-pressure
    ::
    ::    XX cancel subscriptions if =(0 trim-priority) ?
    ::
    ~>  %slog.[0 leaf+"gall: trim: clearing caches"]
    =/  =move  [duct %pass / %g [%wash ~]]
    [[move ~] gall-payload]
  ::
      %vega
    [~ gall-payload]
  ::
      %plea
    =/  =ship  ship.task
    =/  =path  path.plea.task
    =/  =noun  payload.plea.task
    ::
    ~|  [ship=ship plea-path=path]
    ?>  ?=([%ge @ ~] path)
    =/  agent-name  i.t.path
    ::
    =/  =ames-request  ;;(ames-request noun)
    =>  (mo-handle-ames-request:initialised ship agent-name ames-request)
    mo-abet
  ::
      %wash
    =.  running.agents.state
      (~(run by running.agents.state) |=(=agent agent(cache *worm)))
    [~ gall-payload]
  ::
      %wegh
    =/  blocked
      =/  queued  (~(run by blocked.agents.state) |=(blocked [%.y +<]))
      (sort ~(tap by queued) aor)
    ::
    =/  running
      =/  active  (~(run by running.agents.state) |=(agent [%.y +<]))
      (sort ~(tap by active) aor)
    ::
    =/  =mass
      :+  %gall  %.n
      :~  [%foreign %.y contacts.agents.state]
          [%blocked %.n blocked]
          [%active %.n running]
          [%dot %.y state]
      ==
    ::
    [[duct %give %mass mass]~ gall-payload]
  ==
::  +load: recreate vane
::
++  load
  |=  =state-old
  ^+  gall-payload
  ::
  ?-  -.state-old
    %0  gall-payload(state state-old)
  ==
::  +scry: standard scry
::
++  scry
  ~/  %gall-scry
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
  ~/  %gall-take
  |=  [=wire =duct hin=(hypo sign-arvo)]
  ^-  [(list move) _gall-payload]
  ::
  ~|  [%gall-take-failed wire]
  ::
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
