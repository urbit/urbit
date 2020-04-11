!:  ::  %gall, agent execution
!?  163
!:
::::
|=  pit=vase
=,  gall
=>  =~
|%
::  +ames-response: network response message (%boon)
::
::    %d: fact
::    %x: quit
::
++  ames-response
  $%  [%d =mark noun=*]
      [%x ~]
  ==
::  +ames-request: network request (%plea)
::
::    %m: poke
::    %l: watch-as
::    %s: watch
::    %u: leave
::
++  ames-request
  $%  [%m =mark noun=*]
      [%l =mark =path]
      [%s =path]
      [%u ~]
  ==
::  +remote-request: kinds of agent actions that can cross the network
::
::    Used in wires to identify the kind of remote request we made.
::    Bijective with the tags of $ames-request.
::
++  remote-request
  $?  %watch
      %watch-as
      %poke
      %leave
      %missing
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
::  +state: all state
::
++  state
  $:  :: state version
      ::
      %4
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
      ::  outstanding request queue
      ::
      outstanding=(map [wire duct] (qeu remote-request))
      ::  foreign contacts
      ::
      contacts=(set ship)
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
      :: update control
      ::
      =beak
      :: req'd translations
      ::
      marks=(map duct mark)
  ==
:: +blocked: blocked tasks
::
++  blocked  (qeu (trel duct routes deal))
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
        ski=sley
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
      /sys/cor/[term]/[ship]/[desk]/[case]
    ::
    =/  =note-arvo
      =/  =schematic:ford  [%core [ship desk] /hoon/[term]/app]
      [%f %build live=%.y schematic]
    ::
    (mo-pass wire note-arvo)
  ::
  ::  +mo-reboot: ask %ford to rebuild the specified agent
  ::
  ++  mo-reboot
    |=  [force=? =term =ship]
    ^+  mo-core
    =/  gent  (~(got by running.agents.state) term)
    =.  hen  control-duct.gent
    =*  desk  q.beak.gent
    ::  if we're forcing a reboot, we don't try to %kill the old build
    ::
    ?:  force
      (mo-boot term ship desk)
    ::
    =/  =wire
      =/  ship  (scot %p ship)
      =/  case  (scot r.beak.gent)
      /sys/cor/[term]/[ship]/[desk]/[case]
    %.  [term ship desk]
    =<  mo-boot
    =/  =note-arvo  [%f %kill ~]
    (mo-pass wire note-arvo)
  ::
  ::  +mo-goad: rebuild agent(s)
  ::
  ++  mo-goad
    |=  [force=? agent=(unit dude)]
    ^+  mo-core
    ?^  agent
      ~|  goad-gone+u.agent
      (mo-reboot force u.agent our)
    ::
    =/  agents=(list term)
      ~(tap in ~(key by running.agents.state))
    |-  ^+  mo-core
    ?~  agents
      mo-core
    %=  $
      agents     t.agents
      ..mo-core  (mo-reboot force i.agents our)
    ==
  ::
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
    =/  maybe-new-agent  (mule |.(!<(agent result-vase)))
    ?:  ?=(%| -.maybe-new-agent)
      =/  err  [[%leaf "{<term>}: not valid agent"] p.maybe-new-agent]
      (mo-give %onto %.n err)
    =.  mo-core  (mo-new-agent term beak p.maybe-new-agent)
    =/  old  mo-core
    =/  wag
      =/  =routes  [disclosing=~ attributing=our]
      =/  app  (ap-abed:ap term routes)
      (ap-upgrade-state:app ~)
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
    |=  [=term =beak =agent]
    ^+  mo-core
    ::
    =/  running-agent
      =/  default-agent  *running-agent
      %_  default-agent
        control-duct    hen
        beak            beak
        agent           agent
      ==
    ::
    %_  mo-core
      running.agents.state  (~(put by running.agents.state) term running-agent)
    ==
  ::  +mo-send-foreign-request: handle local request to .ship
  ::
  ++  mo-send-foreign-request
    ~/  %mo-send-foreign-request
    |=  [=ship foreign-agent=term =deal]
    ^+  mo-core
    ::
    =.  mo-core  (mo-track-ship ship)
    ?<  ?=(?(%raw-poke %poke-as) -.deal)
    =/  =ames-request
      ?-  -.deal
        %poke      [%m p.cage.deal q.q.cage.deal]
        %leave     [%u ~]
        %watch-as  [%l deal]
        %watch     [%s path.deal]
      ==
    ::
    =/  wire
      /sys/way/(scot %p ship)/[foreign-agent]
    ::
    =/  =note-arvo
      =/  =path  /ge/[foreign-agent]
      [%a %plea ship %g path ames-request]
    ::
    =.  outstanding.agents.state
      =/  stand
        (~(gut by outstanding.agents.state) [wire hen] *(qeu remote-request))
      (~(put by outstanding.agents.state) [wire hen] (~(put to stand) -.deal))
    (mo-pass wire note-arvo)
  ::  +mo-track-ship: subscribe to ames and jael for notices about .ship
  ::
  ++  mo-track-ship
    |=  =ship
    ^+  mo-core
    ::  if already contacted, no-op
    ::
    ?:  (~(has in contacts.agents.state) ship)
      mo-core
    ::  first contact; update state and subscribe to notifications
    ::
    =.  contacts.agents.state  (~(put in contacts.agents.state) ship)
    ::  ask ames to track .ship's connectivity
    ::
    =.  moves  [[system-duct.agents.state %pass /sys/lag %a %heed ship] moves]
    ::  ask jael to track .ship's breaches
    ::
    =/  =note-arvo  [%j %public-keys (silt ship ~)]
    =.  moves
      [[system-duct.agents.state %pass /sys/era note-arvo] moves]
    mo-core
  ::  +mo-untrack-ship: cancel subscriptions to ames and jael for .ship
  ::
  ++  mo-untrack-ship
    |=  =ship
    ^+  mo-core
    ::  if already canceled, no-op
    ::
    ?.  (~(has in contacts.agents.state) ship)
      mo-core
    ::  delete .ship from state and kill subscriptions
    ::
    =.  contacts.agents.state  (~(del in contacts.agents.state) ship)
    ::
    =.  moves  [[system-duct.agents.state %pass /sys/lag %a %jilt ship] moves]
    ::
    =/  =note-arvo  [%j %nuke (silt ship ~)]
    =.  moves
      [[system-duct.agents.state %pass /sys/era note-arvo] moves]
    mo-core
  ::  +mo-breach: ship breached, so forget about them
  ::
  ++  mo-breach
    |=  =ship
    ^+  mo-core
    =.  mo-core  (mo-untrack-ship ship)
    =.  mo-core  (mo-filter-queue ship)
    =/  agents=(list [name=term =running-agent])  ~(tap by running.agents.state)
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
      %era  (mo-handle-sys-era path sign-arvo)
      %cor  (mo-handle-sys-cor path sign-arvo)
      %lag  (mo-handle-sys-lag path sign-arvo)
      %pel  (mo-handle-sys-pel path sign-arvo)
      %rep  (mo-handle-sys-rep path sign-arvo)
      %req  (mo-handle-sys-req path sign-arvo)
      %val  (mo-handle-sys-val path sign-arvo)
      %way  (mo-handle-sys-way path sign-arvo)
    ==
  ::  +mo-handle-sys-era: receive update about contact
  ::
  ++  mo-handle-sys-era
    |=  [=path =sign-arvo]
    ^+  mo-core
    ?>  ?=([%j %public-keys *] sign-arvo)
    ?>  ?=([%era ~] path)
    ?.  ?=(%breach -.public-keys-result.sign-arvo)
      mo-core
    (mo-breach who.public-keys-result.sign-arvo)
  ::  +mo-handle-sys-cor: receive a cor from %ford.
  ::
  ++  mo-handle-sys-cor
    |=  [=path =sign-arvo]
    ^+  mo-core
    ::
    ?>  ?=([%cor @ @ @ @ ~] path)
    ?>  ?=([%f %made *] sign-arvo)
    =/  beak-path  t.t.path
    =/  =beak
      =/  =ship  (slav %p i.beak-path)
      =/  =desk  i.t.beak-path
      =/  =case  [%da (slav %da i.t.t.beak-path)]
      [ship desk case]
    (mo-receive-core i.t.path beak result.sign-arvo)
  ::  +mo-handle-sys-lag: handle an ames %clog notification
  ::
  ++  mo-handle-sys-lag
    |=  [=path =sign-arvo]
    ^+  mo-core
    ::
    ?>  ?=([%lag ~] path)
    ?>  ?=([%a %clog *] sign-arvo)
    ::
    =/  agents=(list term)  ~(tap in ~(key by running.agents.state))
    |-  ^+  mo-core
    ?~  agents  mo-core
    ::
    =.  mo-core
      =/  =routes  [disclosing=~ attributing=our]
      =/  app  (ap-abed:ap i.agents routes)
      ap-abet:(ap-clog:app ship.sign-arvo)
    ::
    $(agents t.agents)
  ::  +mo-handle-sys-pel: translated peer.
  ::
  ::    Validates a received %ford result and %gives an internal
  ::    %fact.
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
      (mo-give %unto %poke-ack `tang.result.sign-arvo)
    ::
        [%complete %error *]
      (mo-give %unto %poke-ack `message.build-result.result.sign-arvo)
    ::
        [%complete %success *]
      (mo-give %unto %fact (result-to-cage:ford build-result.result.sign-arvo))
    ==
  ::  +mo-handle-sys-rep: reverse request.
  ::
  ::    On receipt of a valid +sign from %ford, sets state to the
  ::    appropriate duct and gives an internal %fact
  ::    containing the +sign payload.
  ::
  ++  mo-handle-sys-rep
    |=  [=path =sign-arvo]
    ^+  mo-core
    ::
    ?>  ?=([%rep ~] path)
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
      (mo-give %unto %fact (result-to-cage:ford build-result.result.sign-arvo))
    ==
  ::  +mo-handle-sys-req: TODO description
  ::
  ::    TODO: what should we do if the remote nacks our %pull?
  ++  mo-handle-sys-req
    |=  [=path =sign-arvo]
    ^+  mo-core
    ::
    ?>  ?=([%req @ @ ~] path)
    =/  him  (slav %p i.t.path)
    =/  dap  i.t.t.path
    ::
    ?>  ?=([?(%g %b) %unto *] sign-arvo)
    =/  =sign:agent  +>.sign-arvo
    ::
    ?-    -.sign
        %poke-ack
      =/  err=(unit error:ames)
        ?~  p.sign  ~
        `[%poke-ack u.p.sign]
      (mo-give %done err)
    ::
        %fact
      =+  [mark noun]=[p q.q]:cage.sign
      (mo-give %boon %d mark noun)
    ::
        %kick
      (mo-give %boon %x ~)
    ::
        %watch-ack
      =/  err=(unit error:ames)
        ?~  p.sign  ~
        `[%watch-ack u.p.sign]
      (mo-give %done err)
    ==
  ::  +mo-handle-sys-val: inbound validate.
  ::
  ::    Validates an incoming +sign from %ford and applies it to the
  ::    specified agent.
  ::
  ++  mo-handle-sys-val
    |=  [=path =sign-arvo]
    ^+  mo-core
    ::
    ?>  ?=([%val @ @ ~] path)
    ?>  ?=([%f %made *] sign-arvo)
    =/  =ship  (slav %p i.t.path)
    =/  =term  i.t.t.path
    ?:  ?=([%incomplete *] result.sign-arvo)
      =/  err  (some tang.result.sign-arvo)
      (mo-give %unto %poke-ack err)
    ::
    =/  build-result  build-result.result.sign-arvo
    ?:  ?=([%error *] build-result)
      =/  err  (some message.build-result)
      (mo-give %unto %poke-ack err)
    ::
    =/  =routes  [disclosing=~ attributing=ship]
    =/  =cage  (result-to-cage:ford build-result)
    =/  =deal  [%poke cage]
    (mo-apply term routes deal)
  ::  +mo-handle-sys-way: handle response to outgoing remote request
  ::
  ++  mo-handle-sys-way
    |=  [=wire =sign-arvo]
    ^+  mo-core
    ?>  ?=([%way @ @ $@(~ [@ ~])] wire)
    =/  =ship           (slav %p i.t.wire)
    =/  foreign-agent   i.t.t.wire
    ::
    ?+    sign-arvo  !!
        [%a %done *]
      =^  remote-request  outstanding.agents.state
        ?~  t.t.t.wire
          =/  full-wire  sys+wire
          =/  stand
            %+  ~(gut by outstanding.agents.state)  [full-wire hen]
            ::  default is do nothing; should only hit if cleared queue
            ::  in +load 3-to-4
            ::
            (~(put to *(qeu remote-request)) %missing)
          ~|  [full-wire=full-wire hen=hen stand=stand outs=outstanding.agents.state]
          =^  rr  stand  ~(get to stand)
          [rr (~(put by outstanding.agents.state) [full-wire hen] stand)]
        ::  non-null case of wire is old, remove on next breach after
        ::  2019/12
        ::
        [;;(remote-request i.t.t.t.wire) outstanding.agents.state]
      ::
      =/  err=(unit tang)
        ?~  error=error.sign-arvo
          ~
        `[[%leaf (trip tag.u.error)] tang.u.error]
      ::
      ?-  remote-request
        %watch-as  (mo-give %unto %watch-ack err)
        %watch     (mo-give %unto %watch-ack err)
        %poke      (mo-give %unto %poke-ack err)
        %leave     mo-core
        %missing   (mo-give:(mo-give %unto %watch-ack err) %unto %poke-ack err)
      ==
    ::
        [%a %boon *]
      ?^  t.t.t.wire
        ::  kill subscriptions which use the old wire format
        ::
        !!
      =/  =ames-response  ;;(ames-response payload.sign-arvo)
      (mo-handle-ames-response ames-response)
    ::
        [%a %lost *]
      ::  note this should only happen on reverse bones, so only facts
      ::  and kicks
      ::
      =/  sys-wire  [%sys wire]
      ::  TODO: %drip %kick so app crash can't kill the remote %pull
      ::
      =.  mo-core  (mo-pass sys-wire %a %plea ship %g /ge/[foreign-agent] %u ~)
      =.  mo-core  (mo-give %unto %kick ~)
      mo-core
    ==
  ::  +mo-handle-use: handle a typed +sign incoming on /use.
  ::
  ::    (Note that /use implies the +sign should be routed to an agent.)
  ::
  ::    Initialises the specified agent and then performs an agent-level
  ::    +take on the supplied +sign.
  ::
  ++  mo-handle-use
    ~/  %mo-handle-use
    |=  [=path hin=(hypo sign-arvo)]
    ^+  mo-core
    ::
    ?.  ?=([@ @ *] path)
      ~&  [%mo-handle-use-bad-path path]
      !!
    ::
    =/  =sign-arvo  q.hin
    ?.  ?=([?(%g %b) %unto *] sign-arvo)
      =/  app
        =/  =term  i.path
        =/  =ship  (slav %p i.t.path)
        =/  =routes  [disclosing=~ attributing=ship]
        (ap-abed:ap term routes)
      ::
      =.  app  (ap-generic-take:app t.t.path sign-arvo)
      ap-abet:app
    =/  =sign:agent  +>.sign-arvo
    =/  app
      ?>  ?=([%out @ @ *] t.t.path)
      =/  =term  i.path
      =/  =ship  (slav %p i.t.t.t.path)
      =/  =routes  [disclosing=~ attributing=ship]
      (ap-abed:ap term routes)
    =.  app
      (ap-specific-take:app t.t.path sign)
    ap-abet:app
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
    =/  =deal  r.task
    ::
    =/  move
      =/  =sock  [attributing.routes our]
      =/  card  [%slip %g %deal sock term deal]
      [duct card]
    $(moves [move moves])
  ::  +mo-filter-queue: remove all blocked tasks from ship.
  ::
  ++  mo-filter-queue
    |=  =ship
    =/  agents=(list [name=term =blocked])  ~(tap by blocked.agents.state)
    =|  new-agents=(map term blocked)
    |-  ^+  mo-core
    ?~  agents
      mo-core(blocked.agents.state new-agents)
    =|  new-blocked=blocked
    |-  ^+  mo-core
    ?:  =(~ blocked.i.agents)
      ?~  new-blocked
        ^$(agents t.agents)
      %=  ^$
        agents      t.agents
        new-agents  (~(put by new-agents) name.i.agents new-blocked)
      ==
    =^  mov  blocked.i.agents  ~(get to blocked.i.agents)
    =?  new-blocked  !=(ship attributing.q.p.mov)
      (~(put to new-blocked) mov)
    $
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
    |=  [agent=term =routes =deal]
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
    ?:  ?=(%raw-poke -.deal)
      =/  =schematic:ford  [%vale ship-desk +.deal]
      =/  =note-arvo  [%f %build live=%.n schematic]
      (mo-pass path note-arvo)
    ::
    ?:  ?=(%poke-as -.deal)
      =/  =schematic:ford  [%cast ship-desk mark.deal [%$ cage.deal]]
      =/  =note-arvo  [%f %build live=%.n schematic]
      (mo-pass path note-arvo)
    ::
    =/  app  (ap-abed:ap agent routes)
    =.  app  (ap-apply:app deal)
    ap-abet:app
  ::  +mo-handle-local: handle locally.
  ::
  ::    If the agent is running or blocked, assign it the supplied +deal.
  ::    Otherwise simply apply the action to the agent.
  ::
  ++  mo-handle-local
    |=  [=ship agent=term =deal]
    ^+  mo-core
    ::
    =/  =routes  [disclosing=~ attributing=ship]
    =/  is-running  (~(has by running.agents.state) agent)
    =/  is-blocked  (~(has by blocked.agents.state) agent)
    ::
    ?:  |(!is-running is-blocked)
      =/  =blocked
        =/  waiting  (~(get by blocked.agents.state) agent)
        =/  deals  (fall waiting *blocked)
        =/  deal  [hen routes deal]
        (~(put to deals) deal)
      ::
      %-  (slog leaf+"gall: not running {<agent>} yet, got {<-.deal>}" ~)
      %_  mo-core
        blocked.agents.state  (~(put by blocked.agents.state) agent blocked)
      ==
    (mo-apply agent routes deal)
  ::  +mo-handle-ames-request: handle %ames request message.
  ::
  ++  mo-handle-ames-request
    |=  [=ship agent-name=term =ames-request]
    ^+  mo-core
    ::  %u/%leave gets automatically acked
    ::
    =.  mo-core  (mo-track-ship ship)
    =?  mo-core  ?=(%u -.ames-request)  (mo-give %done ~)
    ::
    =/  =wire  /sys/req/(scot %p ship)/[agent-name]
    ::
    =/  =deal
      ?-  -.ames-request
        %m  [%raw-poke [mark noun]:ames-request]
        %l  [%watch-as [mark path]:ames-request]
        %s  [%watch path.ames-request]
        %u  [%leave ~]
      ==
    (mo-pass wire %g %deal [ship our] agent-name deal)
  ::  +mo-handle-ames-response: handle ames response message.
  ::
  ++  mo-handle-ames-response
    |=  =ames-response
    ^+  mo-core
    ::
    ?-    -.ames-response
        ::  %d: diff; ask ford to validate .noun as .mark
        ::
        %d
      =/  =wire  /sys/rep
      ::  agents load their code from the %home desk, including marks
      ::
      =/  =note-arvo
        =/  =disc:ford  [our %home]
        [%f %build live=%.n %vale disc [mark noun]:ames-response]
      ::
      (mo-pass wire note-arvo)
    ::
        ::  %x: kick; tell agent the publisher canceled the subscription
        ::
        %x
      (mo-give %unto %kick ~)
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
            agent-duct=duct
            agent-moves=(list move)
            agent-config=(list (each suss tang))
            current-agent=running-agent
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
      =.  agent-duct  hen
      ap-core
    ::  +ap-abet: resolve moves.
    ::
    ++  ap-abet
      ^+  mo-core
      ::
      =/  running  (~(put by running.agents.state) agent-name current-agent)
      =/  moves
        =/  giver  |=(report=(each suss tang) [hen %give %onto report])
        =/  from-suss  (turn agent-config giver)
        :(weld agent-moves from-suss moves)
      ::
      %_  mo-core
        running.agents.state  running
        moves                 moves
      ==
    ::  +ap-from-internal: internal move to move.
    ::
    ::    We convert from cards to duct-indexed moves when resolving
    ::    them in Arvo.
    ::
    ++  ap-from-internal
      ~/  %ap-from-internal
      |=  card=(wind neat gift:agent)
      ^-  (list move)
      ::
      ?-    -.card
          %slip  !!
      ::
          %give
        =/  =gift:agent  p.card
        ?:  ?=(%kick -.gift)
          =/  ducts=(list duct)  (ap-ducts-from-paths paths.gift ship.gift)
          %+  turn  ducts
          |=  =duct
          ~?  &(=(duct system-duct.agents.state) !=(agent-name %hood))
            [%agent-giving-on-system-duct agent-name -.gift]
          [duct %give %unto %kick ~]
        ::
        ?.  ?=(%fact -.gift)
          [agent-duct %give %unto gift]~
        ::
        =/  ducts=(list duct)  (ap-ducts-from-paths paths.gift ~)
        =/  =cage  cage.gift
        %+  turn  ducts
        |=  =duct
        ~?  &(=(duct system-duct.agents.state) !=(agent-name %hood))
          [%agent-giving-on-system-duct agent-name -.gift]
        ^-  move
        =/  =mark
          (~(gut by marks.current-agent) duct p.cage)
        ::
        ?:  =(mark p.cage)
          [duct %give %unto %fact cage.gift]
        =/  =path  /sys/pel/[agent-name]
        =/  =note-arvo
          =/  =schematic:ford
            =/  =beak  (mo-beak agent-name)
            [%cast [p q]:beak mark [%$ cage]]
          [%f %build live=%.n schematic]
        ::
        [duct %pass path note-arvo]
      ::
          %pass
        =/  =duct  system-duct.agents.state
        =/  =wire  p.card
        =/  =neat:agent  q.card
        =.  wire
          ?:  ?=(%agent -.neat)
            ::  remove `our` in next breach after 2019/12 and reflect in
            ::  +mo-handle-use (non-unto case)
            ::
            :-  (scot %p our)
            [%out (scot %p ship.neat) name.neat wire]
          [(scot %p attributing.agent-routes) wire]
        =.  wire
          [%use agent-name wire]
        =/  =note-arvo
          ?-    -.neat
              %arvo  note-arvo.neat
              %agent
            =/  =task:able
              =/  =sock  [our ship.neat]
              [%deal sock [name deal]:neat]
            [%g task]
          ==
        [duct %pass wire note-arvo]~
      ==
    ::  +ap-breach: ship breached, so forget about them
    ::
    ++  ap-breach
      |=  =ship
      ^+  ap-core
      =/  in=(list [=duct =^ship =path])
        ~(tap by incoming.subscribers.current-agent)
      |-  ^+  ap-core
      ?^  in
        =?  ap-core  =(ship ship.i.in)
          =/  core  ap-load-delete(agent-duct duct.i.in)
          core(agent-duct agent-duct)
        $(in t.in)
      ::
      =/  out=(list [[=wire =^ship =term] ? =path])
        ~(tap by outgoing.subscribers.current-agent)
      |-  ^+  ap-core
      ?~  out
        ap-core
      =?  ap-core  =(ship ship.i.out)
        =/  core
          =.  agent-duct  system-duct.agents.state
          =/  way  [%out (scot %p ship) term.i.out wire.i.out]
          (ap-specific-take way %kick ~)
        core(agent-duct agent-duct)
      $(out t.out)
    ::  +ap-clog: handle %clog notification from ames
    ::
    ::    Kills subscriptions from .ship in both directions:
    ::      - notifies local app that subscription is dead
    ::      - gives remote %quit to notify subscriber ship
    ::    TODO: %drip local app notification for error isolation
    ::
    ++  ap-clog
      |=  =ship
      ^+  ap-core
      ::
      =/  in=(list [=duct =^ship =path])
        ~(tap by incoming.subscribers.current-agent)
      |-  ^+  ap-core
      ?~  in  ap-core
      ::
      =?  ap-core  =(ship ship.i.in)
        =/  core  ap-kill-up(agent-duct duct.i.in)
        core(agent-duct agent-duct)
      $(in t.in)
    ::  +ap-agent-core: agent core with current bowl and state
    ::
    ++  ap-agent-core
      ~(. agent.current-agent ap-construct-bowl)
    ::  +ap-ducts-from-paths: get ducts subscribed to paths
    ::
    ++  ap-ducts-from-paths
      |=  [target-paths=(list path) target-ship=(unit ship)]
      ^-  (list duct)
      ?:  &(?=(~ target-paths) ?=(~ target-ship))
        ~[agent-duct]
      %-  zing
      %+  turn  target-paths
      |=  =path
      (ap-ducts-from-path `path target-ship)
    ::  +ap-ducts-from-path: get ducts subscribed to path
    ::
    ++  ap-ducts-from-path
      |=  [target-path=(unit path) target-ship=(unit ship)]
      ^-  (list duct)
      ?:  &(?=(~ target-path) ?=(~ target-ship))
        ~[agent-duct]
      %+  murn  ~(tap by incoming.subscribers.current-agent)
      |=  [=duct =ship =path]
      ^-  (unit ^duct)
      ?~  target-ship
        ?:  =(target-path `path)
          `duct
        ~
      ?~  target-path
        ?:  =(target-ship `ship)
          `duct
        ~
      ?:  &(=(target-path `path) =(target-ship `ship))
        `duct
      ~
    ::  +ap-apply: apply effect.
    ::
    ++  ap-apply
      |=  =deal
      ^+  ap-core
      ::
      ?-  -.deal
        %watch-as  (ap-subscribe-as +.deal)
        %poke      (ap-poke +.deal)
        %watch     (ap-subscribe +.deal)
        %raw-poke  !!
        %poke-as   !!
        %leave     ap-load-delete
      ==
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
      ::
      =/  peek-result=(each (unit (unit cage)) tang)
        (ap-mule-peek |.((on-peek:ap-agent-core [term tyl])))
      ::
      ?-  -.peek-result
        %&  p.peek-result
        %|  ((slog leaf+"peek bad result" p.peek-result) [~ ~])
      ==
    ::  +ap-update-subscription: update subscription.
    ::
    ++  ap-update-subscription
      ~/  %ap-update-subscription
      |=  [is-ok=? =other=ship other-agent=term =wire]
      ^+  ap-core
      ::
      ?:  is-ok
        ap-core
      (ap-kill-down wire [other-ship other-agent])
    ::  +ap-give: return result.
    ::
    ++  ap-give
      |=  =gift:agent
      ^+  ap-core
      ::
      =/  internal-moves
        (weld (ap-from-internal %give gift) agent-moves)
      ap-core(agent-moves internal-moves)
    ::  +ap-construct-bowl: set up bowl.
    ::
    ++  ap-construct-bowl
      ^-  bowl
      :*  :*  our                                     ::  host
              attributing.agent-routes                ::  guest
              agent-name                              ::  agent
          ==                                          ::
          :*  wex=outgoing.subscribers.current-agent  ::  outgoing
              sup=incoming.subscribers.current-agent  ::  incoming
          ==                                          ::
          :*  act=change.stats.current-agent          ::  tick
              eny=eny.stats.current-agent             ::  nonce
              now=time.stats.current-agent            ::  time
              byk=beak.current-agent                  ::  source
      ==  ==
    ::  +ap-pass: request action.
    ::
    ++  ap-pass
      |=  [=path =neat]
      ^+  ap-core
      ::
      =/  internal-moves
        (ap-from-internal %pass path neat)
      ap-core(agent-moves (weld internal-moves agent-moves))
    ::  +ap-reinstall: reinstall.
    ::
    ++  ap-reinstall
      ~/  %ap-reinstall
      |=  =vase
      ^+  ap-core
      ::
      =/  maybe-agent  (mule |.(!<(agent vase)))
      ?:  ?=(%| -.maybe-agent)
        (ap-error %new-core-not-agent p.maybe-agent)
      ::
      =/  prep
        =/  =agent  p.maybe-agent
        =/  running
          %-  some
          ~(on-save agent.current-agent ap-construct-bowl)
        =/  installed  ap-install(agent.current-agent agent)
        (installed running)
      ::
      =^  maybe-tang  ap-core  prep
      ?~  maybe-tang
        ap-core
      (ap-error %load-failed u.maybe-tang)
    ::  +ap-subscribe-as: apply %watch-as.
    ::
    ++  ap-subscribe-as
      |=  [=mark =path]
      ^+  ap-core
      ::
      =.  marks.current-agent  (~(put by marks.current-agent) agent-duct mark)
      (ap-subscribe path)
    ::  +ap-subscribe: apply %watch.
    ::
    ++  ap-subscribe
      ~/  %ap-subscribe
      |=  pax=path
      ^+  ap-core
      ::
      =/  incoming  [attributing.agent-routes pax]
      =.  incoming.subscribers.current-agent
        (~(put by incoming.subscribers.current-agent) agent-duct incoming)
      ::
      =^  maybe-tang  ap-core
        %+  ap-ingest  %watch-ack  |.
        (on-watch:ap-agent-core pax)
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
        %+  ap-ingest  %poke-ack  |.
        (on-poke:ap-agent-core cage)
      ap-core
    ::  +ap-error: pour error.
    ::
    ++  ap-error
      |=  [=term =tang]
      ^+  ap-core
      ::
      =/  form  |=(=tank [%rose [~ "! " ~] tank ~])
      =^  maybe-tang  ap-core
        %+  ap-ingest  ~  |.
        (on-fail:ap-agent-core term (turn tang form))
      ap-core
    ::  +ap-generic-take: generic take.
    ::
    ++  ap-generic-take
      ~/  %ap-generic-take
      |=  [=wire =sign-arvo]
      ^+  ap-core
      ::
      =^  maybe-tang  ap-core
        %+  ap-ingest  ~  |.
        (on-arvo:ap-agent-core wire sign-arvo)
      ?^  maybe-tang
        (ap-error %arvo-response u.maybe-tang)
      ap-core
    ::  +ap-specific-take: specific take.
    ::
    ++  ap-specific-take
      |=  [=wire =sign:agent]
      ^+  ap-core
      ::
      ~|  wire=wire
      ?>  ?=([%out @ @ *] wire)
      =/  other-ship  (slav %p i.t.wire)
      =/  other-agent  i.t.t.wire
      =/  =dock  [other-ship other-agent]
      =/  agent-wire  t.t.t.wire
      ::  if subscription ack or close, handle before calling user code
      ::
      =?  outgoing.subscribers.current-agent  ?=(%kick -.sign)
        %-  ~(del by outgoing.subscribers.current-agent)
        [agent-wire dock]
      ?:  ?&  ?=(%watch-ack -.sign)
              !(~(has by outgoing.subscribers.current-agent) [agent-wire dock])
          ==
        %-  %:  slog
              leaf+"{<agent-name>}: got ack for nonexistent subscription"
              leaf+"{<dock>}: {<agent-wire>}"
              >wire=wire<
              >out=outgoing.subscribers.current-agent<
              ~
            ==
        ap-core
      ::
      =?  outgoing.subscribers.current-agent  ?=(%watch-ack -.sign)
        ?^  p.sign
          %-  ~(del by outgoing.subscribers.current-agent)
          [agent-wire dock]
        %+  ~(jab by outgoing.subscribers.current-agent)  [agent-wire dock]
        |=  [acked=? =path]
        =.  .
          ?.  acked
            .
          %-  =/  =tape
                "{<agent-name>}: received 2nd watch-ack on {<wire dock path>}"
              (slog leaf+tape ~)
          .
        [& path]
      ::
      =^  maybe-tang  ap-core
        %+  ap-ingest  ~  |.
        (on-agent:ap-agent-core agent-wire sign)
      ::  if failed %fact handling, kill subscription
      ::
      =?  ap-core  ?=(%fact -.sign)
        (ap-update-subscription =(~ maybe-tang) p.dock q.dock agent-wire)
      ?^  maybe-tang
        (ap-error -.sign leaf/"closing subscription" u.maybe-tang)
      ap-core
    ::  +ap-install: install wrapper.
    ::
    ++  ap-install
      |=  maybe-vase=(unit vase)
      ^-  [(unit tang) _ap-core]
      ::
      =^  maybe-tang  ap-core  (ap-upgrade-state maybe-vase)
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
    ::  +ap-upgrade-state: low-level install.
    ::
    ++  ap-upgrade-state
      ~/  %ap-upgrade-state
      |=  maybe-vase=(unit vase)
      ^-  [(unit tang) _ap-core]
      ::
      =^  maybe-tang  ap-core
        %+  ap-ingest  ~
        ?~  maybe-vase
          |.  on-init:ap-agent-core
        |.  (on-load:ap-agent-core u.maybe-vase)
      [maybe-tang ap-core]
    ::  +ap-silent-delete: silent delete.
    ::
    ++  ap-silent-delete
      ^+  ap-core
      ::
      %=    ap-core
          incoming.subscribers.current-agent
        (~(del by incoming.subscribers.current-agent) agent-duct)
      ==
    ::  +ap-load-delete: load delete.
    ::
    ++  ap-load-delete
      ^+  ap-core
      ::
      =/  maybe-incoming
        (~(get by incoming.subscribers.current-agent) agent-duct)
      ?~  maybe-incoming
        ap-core
      ::
      =/  incoming  u.maybe-incoming
      =.  incoming.subscribers.current-agent
        (~(del by incoming.subscribers.current-agent) agent-duct)
      ::
      =^  maybe-tang  ap-core
        %+  ap-ingest  ~  |.
        (on-leave:ap-agent-core q.incoming)
      ?^  maybe-tang
        (ap-error %leave u.maybe-tang)
      ap-core
    ::  +ap-kill-up: 2-sided kill from publisher side
    ::
    ++  ap-kill-up
      ^+  ap-core
      ::
      =>  ap-load-delete
      (ap-give %kick ~ ~)
    ::  +ap-kill-down: 2-sided kill from subscriber side
    ::
    ::    Must process leave first in case kick handler rewatches.
    ::
    ++  ap-kill-down
      |=  [=wire =dock]
      ^+  ap-core
      ::
      =.  ap-core
        (ap-pass wire %agent dock %leave ~)
      =/  way  [%out (scot %p p.dock) q.dock wire]
      (ap-pass way %arvo %b %huck !>([%unto %kick ~]))
    ::  +ap-mule: run virtualized with intercepted scry, preserving type
    ::
    ::    Compare +mute and +mule.  Those pass through scry, which
    ::    doesn't allow us to catch crashes due to blocking scry.  If
    ::    you intercept scry, you can't preserve the type
    ::    polymorphically.  By monomorphizing, we are able to do so
    ::    safely.
    ::
    ++  ap-mule
      |=  run=_^?(|.(*step:agent))
      ^-  (each step:agent tang)
      =/  res  (mock [run %9 2 %0 1] (sloy ski))
      ?-  -.res
        %0  [%& !<(step:agent [-:!>(*step:agent) p.res])]
        %1  [%| (turn p.res |=(a=* (smyt (path a))))]
        %2  [%| p.res]
      ==
    ::  +ap-mule-peek: same as +ap-mule but for (unit (unit cage))
    ::
    ++  ap-mule-peek
      |=  run=_^?(|.(*(unit (unit cage))))
      ^-  (each (unit (unit cage)) tang)
      =/  res  (mock [run %9 2 %0 1] (sloy ski))
      ?-  -.res
        %0  [%& !<((unit (unit cage)) [-:!>(*(unit (unit cage))) p.res])]
        %1  [%| (turn p.res |=(a=* (smyt (path a))))]
        %2  [%| p.res]
      ==
    ::  +ap-ingest: call agent arm
    ::
    ::    Handle acks here because they need to be emitted before the
    ::    rest of the moves.
    ::
    ++  ap-ingest
      |=  [ack=?(%poke-ack %watch-ack ~) run=_^?(|.(*step:agent))]
      ^-  [(unit tang) _ap-core]
      =/  result  (ap-mule run)
      =^  new-moves  ap-core  (ap-handle-result result)
      =/  maybe-tang=(unit tang)
        ?:  ?=(%& -.result)
          ~
        `p.result
      =/  ack-moves=(list move)
        %-  zing
        %-  turn  :_  ap-from-internal
        ^-  (list card:agent)
        ?-  ack
          ~      ~
          %poke-ack   [%give %poke-ack maybe-tang]~
          %watch-ack  [%give %watch-ack maybe-tang]~
        ==
      ::
      =.  agent-moves
        :(weld (flop new-moves) ack-moves agent-moves)
      [maybe-tang ap-core]
    ::  +ap-handle-result: handle result.
    ::
    ++  ap-handle-result
      ~/  %ap-handle-result
      |=  result=(each step:agent tang)
      ^-  [(list move) _ap-core]
      ?:  ?=(%| -.result)
        `ap-core
      ::
      =.  agent.current-agent  +.p.result
      =/  moves  (zing (turn -.p.result ap-from-internal))
      =.  incoming.subscribers.current-agent
        (ap-handle-kicks moves)
      (ap-handle-peers moves)
    ::  +ap-handle-kicks: handle cancels of incoming subscriptions
    ::
    ++  ap-handle-kicks
      ~/  %ap-handle-kicks
      |=  moves=(list move)
      ^-  bitt
      =/  quits=(list duct)
        %+  murn  moves
        |=  =move
        ^-  (unit duct)
        ?.  ?=([* %give %unto %kick *] move)
          ~
        `duct.move
      ::
      =/  quit-map=bitt
        (malt (turn quits |=(=duct [duct *[ship path]])))
      (~(dif by incoming.subscribers.current-agent) quit-map)
    ::  +ap-handle-peers: handle new outgoing subscriptions
    ::
    ++  ap-handle-peers
      ~/  %ap-handle-peers
      |=  moves=(list move)
      ^-  [(list move) _ap-core]
      =|  new-moves=(list move)
      |-  ^-  [(list move) _ap-core]
      ?~  moves
        [(flop new-moves) ap-core]
      =/  =move  i.moves
      ?:  ?=([* %pass * %g %deal * * %leave *] move)
        =/  =wire  p.move.move
        ?>  ?=([%use @ @ %out @ @ *] wire)
        =/  short-wire  t.t.t.t.t.t.wire
        =/  =dock  [q.p q]:q.move.move
        =.  outgoing.subscribers.current-agent
          (~(del by outgoing.subscribers.current-agent) [short-wire dock])
        $(moves t.moves, new-moves [move new-moves])
      ?.  ?=([* %pass * %g %deal * * %watch *] move)
        $(moves t.moves, new-moves [move new-moves])
      =/  =wire  p.move.move
      ?>  ?=([%use @ @ %out @ @ *] wire)
      =/  short-wire  t.t.t.t.t.t.wire
      =/  =dock  [q.p q]:q.move.move
      =/  =path  path.r.q.move.move
      ?:  (~(has by outgoing.subscribers.current-agent) short-wire dock)
        =.  ap-core
          =/  =tang
            ~[leaf+"subscribe wire not unique" >agent-name< >short-wire< >dock<]
          %-  (slog >out=outgoing.subscribers.current-agent< tang)
          (ap-error %watch-not-unique tang)
        $(moves t.moves)
      =.  outgoing.subscribers.current-agent
        (~(put by outgoing.subscribers.current-agent) [short-wire dock] [| path])
      $(moves t.moves, new-moves [move new-moves])
    --
  --
::  +call: request
::
++  call
  ~%  %gall-call  +>   ~
  |=  [=duct dud=(unit goof) hic=(hypo (hobo task:able))]
  ^-  [(list move) _gall-payload]
  ?^  dud
    ~|(%gall-call-dud (mean tang.u.dud))
  ::
  ~|  [%gall-call-failed duct q.hic]
  =/  =task:able  ((harden task:able) q.hic)
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
    =/  =term  q.task
    =/  =deal  r.task
    ?.  =(q.sock our)
      ?>  =(p.sock our)
      =>  (mo-send-foreign-request:initialised q.sock term deal)
      mo-abet
    ::
    =>  (mo-handle-local:initialised p.sock term deal)
    mo-abet
  ::
      %goad
    mo-abet:(mo-goad:initialised force.task agent.task)
  ::
      %sear
    mo-abet:(mo-filter-queue:initialised ship.task)
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
      %-  ~(run by running.agents.state)
      |=  =running-agent
      running-agent(cache *worm)
    [~ gall-payload]
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
  |^
  |=  =all-state
  ^+  gall-payload
  ::
  =?  all-state  ?=(%0 -.all-state)
    (state-0-to-1 all-state)
  ::
  =?  all-state  ?=(%1 -.all-state)
    (state-1-to-2 all-state)
  ::
  =?  all-state  ?=(%2 -.all-state)
    (state-2-to-3 all-state)
  ::
  =?  all-state  ?=(%3 -.all-state)
    (state-3-to-4 all-state)
  ::
  ?>  ?=(%4 -.all-state)
  gall-payload(state all-state)
  ::
  ::  +all-state: upgrade path
  ::
  ++  all-state  $%(state-0 state-1 state-2 state-3 ^state)
  ::
  ++  state-3-to-4
    |=  =state-3
    ^-  ^state
    %=    state-3
        -  %4
        outstanding.agents  ~
    ==
  ::
  ++  state-3
    $:  %3
        =agents
    ==
  ::
  ++  state-2-to-3
    |=  =state-2
    ^-  state-3
    %=    state-2
        -  %3
        running.agents-2
      %-  ~(run by running.agents-2.state-2)
      |=  =running-agent-2
      ^-  running-agent
      %=  running-agent-2
        agent-2  (agent-2-to-3 agent-2.running-agent-2)
      ==
    ==
  ::
  ++  agent-2-to-3
    |=  =agent-2
    ^-  agent
    =>  |%
        ++  cards-2-to-3
          |=  cards=(list card:^agent-2)
          ^-  (list card:agent)
          %+  turn  cards
          |=  =card:^agent-2
          ^-  card:agent
          ?.  ?=([%give ?(%fact %kick) *] card)  card
          %=(card path.p (drop path.p.card))
        --
    |_  =bowl:gall
    +*  this  .
        pass  ~(. agent-2 bowl)
    ++  on-init
      =^  cards  agent-2  on-init:pass
      [(cards-2-to-3 cards) this]
    ::
    ++  on-save
      on-save:pass
    ::
    ++  on-load
      |=  old-state=vase
      =^  cards  agent-2  (on-load:pass old-state)
      [(cards-2-to-3 cards) this]
    ::
    ++  on-poke
      |=  [=mark =vase]
      =^  cards  agent-2  (on-poke:pass mark vase)
      [(cards-2-to-3 cards) this]
    ::
    ++  on-watch
      |=  =path
      =^  cards  agent-2  (on-watch:pass path)
      [(cards-2-to-3 cards) this]
    ::
    ++  on-leave
      |=  =path
      =^  cards  agent-2  (on-leave:pass path)
      [(cards-2-to-3 cards) this]
    ::
    ++  on-peek
      |=  =path
      (on-peek:pass path)
    ::
    ++  on-agent
      |=  [=wire =sign:agent:gall]
      =^  cards  agent-2  (on-agent:pass wire sign)
      [(cards-2-to-3 cards) this]
    ::
    ++  on-arvo
      |=  [=wire =sign-arvo]
      =^  cards  agent-2  (on-arvo:pass wire sign-arvo)
      [(cards-2-to-3 cards) this]
    ::
    ++  on-fail
      |=  [=term =tang]
      =^  cards  agent-2  (on-fail:pass term tang)
      [(cards-2-to-3 cards) this]
    --
  ::
  ++  state-2
    $:  %2
        =agents-2
    ==
  ::
  ++  agents-2
    $:  system-duct=duct
        outstanding=(map [wire duct] (qeu remote-request))
        contacts=(set ship)
        running=(map term running-agent-2)
        blocked=(map term blocked)
    ==
  ::
  ++  running-agent-2
    $:  cache=worm
        control-duct=duct
        live=?
        =stats
        =subscribers
        =agent-2
        =beak
        marks=(map duct mark)
    ==
  ::
  ++  agent-2
    =<  form
    |%
    +$  step  (quip card form)
    +$  card  (wind note gift)
    +$  note  note:agent
    +$  task  task:agent
    +$  sign  sign:agent
    +$  gift
      $%  [%fact path=(unit path) =cage]
          [%kick path=(unit path) ship=(unit ship)]
          [%watch-ack p=(unit tang)]
          [%poke-ack p=(unit tang)]
      ==
    ++  form
      $_  ^|
      |_  bowl
      ++  on-init
        *(quip card _^|(..on-init))
      ::
      ++  on-save
        *vase
      ::
      ++  on-load
        |~  old-state=vase
        *(quip card _^|(..on-init))
      ::
      ++  on-poke
        |~  [mark vase]
        *(quip card _^|(..on-init))
      ::
      ++  on-watch
        |~  path
        *(quip card _^|(..on-init))
      ::
      ++  on-leave
        |~  path
        *(quip card _^|(..on-init))
      ::
      ++  on-peek
        |~  path
        *(unit (unit cage))
      ::
      ++  on-agent
        |~  [wire sign]
        *(quip card _^|(..on-init))
      ::
      ++  on-arvo
        |~  [wire sign-arvo]
        *(quip card _^|(..on-init))
      ::
      ++  on-fail
        |~  [term tang]
        *(quip card _^|(..on-init))
      --
    --
  ::
  ++  state-1-to-2
    |=  =state-1
    ^-  state-2
    %=    state-1
        -           %2
        +.agents-1  [~ +.agents-1.state-1]
    ==
  ::
  ++  state-1
    $:  %1
        =agents-1
    ==
  ::
  ++  agents-1
    $:  system-duct=duct
        contacts=(set ship)
        running=(map term running-agent-2)
        blocked=(map term blocked)
    ==
  ::
  ++  state-0-to-1
    |=  =state-0
    ^-  state-1
    %=    state-0
        -  %1
        running.agents-0
      %-  ~(run by running.agents-0.state-0)
      |=  =running-agent-0
      ^-  running-agent-2
      %=  running-agent-0
        agent-0  (agent-0-to-1 agent-0.running-agent-0)
      ==
    ==
  ::
  ++  agent-0-to-1
    |=  =agent-0
    ^-  agent-2
    |_  =bowl:gall
    +*  this  .
        pass  ~(. agent-0 bowl)
    ++  on-init
      =^  cards  agent-0  on-init:pass
      [cards this]
    ::
    ++  on-save
      on-save:pass
    ::
    ++  on-load
      |=  old-state=vase
      =^  cards  agent-0  (on-load:pass old-state)
      [cards this]
    ::
    ++  on-poke
      |=  [=mark =vase]
      =^  cards  agent-0  (on-poke:pass mark vase)
      [cards this]
    ::
    ++  on-watch
      |=  =path
      =^  cards  agent-0  (on-watch:pass path)
      [cards this]
    ::
    ++  on-leave
      |=  =path
      =^  cards  agent-0  (on-leave:pass path)
      [cards this]
    ::
    ++  on-peek
      |=  =path
      (on-peek:pass path)
    ::
    ++  on-agent
      |=  [=wire =sign:agent:gall]
      =^  cards  agent-0  (on-agent:pass wire sign)
      [cards this]
    ::
    ++  on-arvo
      |=  [=wire =sign-arvo]
      ?<  ?=([%d %pack *] sign-arvo)
      =^  cards  agent-0  (on-arvo:pass wire `sign-arvo-0`sign-arvo)
      [cards this]
    ::
    ++  on-fail
      |=  [=term =tang]
      =^  cards  agent-0  (on-fail:pass term tang)
      [cards this]
    --
  ::
  ++  state-0
    $:  %0
        =agents-0
    ==
  ::
  ++  agents-0
    $:  system-duct=duct
        contacts=(set ship)
        running=(map term running-agent-0)
        blocked=(map term blocked)
    ==
  ::
  ++  running-agent-0
    $:  cache=worm
        control-duct=duct
        live=?
        =stats
        =subscribers
        =agent-0
        =beak
        marks=(map duct mark)
    ==
  ::
  ++  agent-0
    =<  form
    |%
    +$  step  (quip card form)
    +$  card  (wind note gift)
    +$  note  note:agent
    +$  task  task:agent
    +$  gift  gift:agent-2
    +$  sign  sign:agent
    ++  form
      $_  ^|
      |_  bowl
      ++  on-init
        *(quip card _^|(..on-init))
      ::
      ++  on-save
        *vase
      ::
      ++  on-load
        |~  old-state=vase
        *(quip card _^|(..on-init))
      ::
      ++  on-poke
        |~  [mark vase]
        *(quip card _^|(..on-init))
      ::
      ++  on-watch
        |~  path
        *(quip card _^|(..on-init))
      ::
      ++  on-leave
        |~  path
        *(quip card _^|(..on-init))
      ::
      ++  on-peek
        |~  path
        *(unit (unit cage))
      ::
      ++  on-agent
        |~  [wire sign]
        *(quip card _^|(..on-init))
      ::
      ++  on-arvo
        |~  [wire sign-arvo-0]
        *(quip card _^|(..on-init))
      ::
      ++  on-fail
        |~  [term tang]
        *(quip card _^|(..on-init))
      --
    --
  ::
  ++  sign-arvo-0
    $%  {$a gift:able:ames}
        $:  $b
            $%  gift:able:behn
                [%writ riot:clay]
                $>(%mere gift:able:clay)
                $>(%unto gift:able:gall)
            ==
        ==
        {$c gift:able:clay}
        {$d $<(%pack gift:able:dill)}
        {$f gift:able:ford}
        [%e gift:able:eyre]
        {$g gift:able:gall}
        [%i gift:able:iris]
        {$j gift:able:jael}
    ==
  --
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
  |=  [=wire =duct dud=(unit goof) hin=(hypo sign-arvo)]
  ^-  [(list move) _gall-payload]
  ?^  dud
    ~|(%gall-take-dud (mean tang.u.dud))
  ::
  ~|  [%gall-take-failed wire]
  ::
  ?>  ?=([?(%sys %use) *] wire)
  =/  initialised  (mo-abed:mo duct)
  =/  =sign-arvo  q.hin
  =>  ?-  i.wire
        %sys  (mo-handle-sys:initialised t.wire sign-arvo)
        %use  (mo-handle-use:initialised t.wire hin)
      ==
  mo-abet
--
