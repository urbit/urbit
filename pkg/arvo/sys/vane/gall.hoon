::  ::  %gall, agent execution
!?  163
::
::::
|=  pit=vase
=,  gall
=>
|%
+|  %main
::
::  $move: Arvo-level move
::
+$  move  [=duct move=(wind note-arvo gift-arvo)]
::  $state-6: overall gall state, versioned
::
+$  state-6  [%6 state]
::  $state: overall gall state
::
::    system-duct: TODO document
::    outstanding: outstanding request queue
::    contacts: other ships we're in communication with
::    yokes: running agents
::    blocked: moves to agents that haven't been started yet
::
+$  state
  $:  system-duct=duct
      outstanding=(map [wire duct] (qeu remote-request))
      contacts=(set ship)
      yokes=(map term yoke)
      blocked=(map term (qeu blocked-move))
  ==
::  $watches: subscribers and publications
::
::    TODO: rename this, to $ties?
::    TODO: rename $boat and $bitt and document
::
+$  watches  [inbound=bitt outbound=boat]
::  $routes: new cuff; TODO: document
::
+$  routes
  $:  disclosing=(unit (set ship))
      attributing=ship
  ==
::  $yoke: agent runner state
::
::    control-duct: TODO document
::    live: is this agent running? TODO document better
::    stats: TODO document
::    watches: incoming and outgoing subscription state
::    agent: agent core
::    beak: compilation source
::    marks: mark conversion requests
::
+$  yoke
  $:  control-duct=duct
      live=?
      =stats
      =watches
      =agent
      =beak
      marks=(map duct mark)
  ==
::  $blocked-move: enqueued move to an agent
::
+$  blocked-move  [=duct =routes =deal]
::  $stats: statistics
::
::    change: how many moves this agent has processed
::    eny: entropy
::    time: date of current event processing
::
+$  stats  [change=@ud eny=@uvJ time=@da]
::  $ames-response: network response message (%boon)
::
::    %d: fact
::    %x: quit
::
+$  ames-response
  $%  [%d =mark noun=*]
      [%x ~]
  ==
::  $ames-request: network request (%plea)
::
::    %m: poke
::    %l: watch-as
::    %s: watch
::    %u: leave
::
+$  ames-request
  $%  [%m =mark noun=*]
      [%l =mark =path]
      [%s =path]
      [%u ~]
  ==
::  $remote-request: kinds of agent actions that can cross the network
::
::    Used in wires to identify the kind of remote request we made.
::    Bijective with the tags of $ames-request.
::
+$  remote-request
  $?  %watch
      %watch-as
      %poke
      %leave
      %missing
  ==
::  |migrate: data structures for upgrades
::
+|  %migrate
::
::  $spore: structures for update, produced by +stay
::
+$  spore
  $:  %6
      system-duct=duct
      outstanding=(map [wire duct] (qeu remote-request))
      contacts=(set ship)
      eggs=(map term egg)
      blocked=(map term (qeu blocked-move))
  ==
::  $egg: migratory agent state; $yoke with .old-state instead of .agent
::
+$  egg
  $:  control-duct=duct
      live=?
      =stats
      =watches
      old-state=vase
      =beak
      marks=(map duct mark)
  ==
--
::  pupal gall core, on upgrade
::
=<  =*  adult-gate  .
    =|  =spore
    |=  [our=ship now=@da eny=@uvJ ski=sley]
    =*  pupal-gate  .
    =*  adult-core  (adult-gate +<)
    =<  |%
        ++  call  ^call
        ++  load  ^load
        ++  scry  ^scry
        ++  stay  ^stay
        ++  take  ^take
        --
    |%
    ++  molt
      |=  [=duct fec=(unit move)]
      ^-  [(list move) _adult-gate]
      ~>  %slog.[0 leaf+"gall: molting"]
      ~<  %slog.[0 leaf+"gall: molted"]
      ::  +molt should never notify its client about agent changes
      ::
      =-  [(skip -< |=(move ?=([* %give %onto *] +<))) ->]
      =/  adult  adult-core
      =.  state.adult
        [%6 system-duct outstanding contacts yokes=~ blocked]:spore
      =/  mo-core  (mo-abed:mo:adult duct)
      =.  mo-core
        =/  apps=(list [dap=term =egg])  ~(tap by eggs.spore)
        |-  ^+  mo-core
        ?~  apps  mo-core
        ~>  %slog.[0 leaf+"gall: upgrading {<dap.i.apps>}"]
        =/  ap-core  (ap-abut:ap:mo-core i.apps)
        =^  tan  ap-core  (ap-install:ap-core `old-state.egg.i.apps)
        ?^  tan
          (mean u.tan)
        $(apps t.apps, mo-core ap-abet:ap-core)
      =.  mo-core  (mo-subscribe-to-agent-builds:mo-core now)
      =^  moves  adult-gate  mo-abet:mo-core
      =?  moves  ?=(^ fec)  (weld moves [u.fec]~)
      [moves adult-gate]
    ::
    ++  call
      |=  [=duct dud=(unit goof) typ=type wrapped-task=(hobo task:able)]
      =*  call-args  +<
      ?:  =(~ eggs.spore)
        ~>  %slog.[0 leaf+"gall: direct morphogenesis"]
        =.  state.adult-gate  spore(eggs *(map term yoke))
        (call:adult-core call-args)
      ?^  dud
        ~>  %slog.[0 leaf+"gall: pupa call dud"]
        (mean >mote.u.dud< tang.u.dud)
      =/  task  ((harden task:able:gall) wrapped-task)
      (molt duct `[duct %slip %g task])
    ::
    ++  scry  scry:adult-core
    ++  stay  ~|(%gall-subinvolution !!)
    ++  take
      |=  [=wire =duct dud=(unit goof) typ=type sign=sign-arvo]
      =*  take-args  +<
      ?:  =(~ eggs.spore)
        ~>  %slog.[0 leaf+"gall: direct morphogenesis"]
        =.  state.adult-gate  spore(eggs *(map term yoke))
        (take:adult-core take-args)
      ?^  dud
        ~>  %slog.[0 leaf+"gall: pupa take dud"]
        (mean >mote.u.dud< tang.u.dud)
      ?:  =(/sys/lyv wire)
        (molt duct ~)
      ::  TODO: test this or remove and assert /sys/lyv
      ::
      (molt duct `[duct %pass wire %b %huck !>(sign)])
    ::
    ++  load
      |^
      |=  old=all-state
      =.  spore  (upgrade old)
      ?.  =(~ eggs.spore)
        pupal-gate
      ~>  %slog.[0 leaf+"gall: direct morphogenesis"]
      adult-gate(state spore(eggs *(map term yoke)))
      ::
      ++  upgrade
        |=  =all-state
        ^-  ^spore
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
        =?  all-state  ?=(%4 -.all-state)
          (state-4-to-5 all-state)
        ::
        =?  all-state  ?=(%5 -.all-state)
          (state-5-to-spore-6 all-state)
        ::
        ?>  ?=(%6 -.all-state)
        all-state
      ::  +all-state: upgrade path
      ::
      +$  all-state  $%(state-0 state-1 state-2 state-3 state-4 state-5 ^spore)
      ::
      ++  state-5-to-spore-6
        |=  s=state-5
        ^-  ^spore
        %=    s
            -  %6
            outstanding  ~  ::  TODO: do we need to process these somehow?
            running
          (~(run by running.s) |=(y=yoke-0 +:y(agent on-save:agent.y)))
        ==
      ::
      ++  state-4-to-5  |=(s=state-4 `state-5`s(- %5, outstanding ~))
      ++  state-3-to-4  |=(s=state-3 `state-4`s(- %4, outstanding ~))
      ++  state-2-to-3  |=(s=state-2 `state-3`s(- %3))
      ++  state-1-to-2  |=(s=state-1 `state-2`s(- %2, +< +<.s, +> `+>.s))
      ++  state-0-to-1  |=(s=state-0 `state-1`s(- %1))
      ::
      +$  state-5  [%5 agents-2]
      +$  state-4  [%4 agents-2]
      +$  state-3  [%3 agents-2]
      +$  state-2  [%2 agents-2]
      +$  state-1  [%1 agents-0]
      +$  state-0  [%0 agents-0]
      ::
      +$  agents-2
        $:  system-duct=duct
            outstanding=(map [wire duct] (qeu remote-request))
            contacts=(set ship)
            running=(map term yoke-0)
            blocked=(map term (qeu blocked-move))
        ==
      ::
      +$  agents-0
        $:  system-duct=duct
            contacts=(set ship)
            running=(map term yoke-0)
            blocked=(map term (qeu blocked-move))
        ==
      ::
      +$  yoke-0
        $:  cache=worm
            control-duct=duct
            live=?
            =stats
            =watches
            agent=any-agent
            =beak
            marks=(map duct mark)
        ==
      ::
      ++  any-agent
        $_
        ^|
        |_  bowl
        ++  on-init   **
        ++  on-save   *vase
        ++  on-load   **
        ++  on-poke   **
        ++  on-watch  **
        ++  on-leave  **
        ++  on-peek   **
        ++  on-agent  **
        ++  on-arvo   **
        ++  on-fail   **
        --
      --
    --
::  adult gall vane interface, for type compatibility with pupa
::
=|  state=state-6
|=  [our=ship now=@da eny=@uvJ ski=sley]
=*  gall-payload  .
=<  ~%  %gall-wrap  ..mo  ~
    |%
    ++  call  ^call
    ++  load  ^load
    ++  scry  ^scry
    ++  stay  ^stay
    ++  take  ^take
    --
~%  %gall-top  ..is  ~
|%
::  +mo: Arvo-level move handling
::
::    An outer core responsible for routing moves to and from Arvo; it calls
::    an inner core, +ap, to route internal moves to and from agents.
::
++  mo
  ~%  %gall-mo  +>  ~
  |_  [hen=duct moves=(list move)]
  ::  +mo-abed: initialise state with the provided duct
  ::  +mo-abet: finalize, reversing moves
  ::  +mo-pass: prepend a standard %pass to the current list of moves
  ::  +mo-give: prepend a standard %give to the current list of moves
  ::
  ++  mo-core  .
  ++  mo-abed  |=(hun=duct mo-core(hen hun))
  ++  mo-abet  [(flop moves) gall-payload]
  ++  mo-pass  |=(p=[wire note-arvo] mo-core(moves [[hen pass+p] moves]))
  ++  mo-give  |=(g=gift:able mo-core(moves [[hen give+g] moves]))
  ::  +mo-boot: ask %ford to build us a core for the specified agent.
  ::
  ++  mo-boot
    |=  [dap=term =ship =desk]
    ^+  mo-core
    =/  =case  [%da now]
    =/  =wire  /sys/cor/[dap]/(scot %p ship)/[desk]/(scot case)
    (mo-pass wire %c %warp ship desk ~ %sing %a case /app/[dap]/hoon)
  ::  +mo-reboot: ask %ford to rebuild the specified agent
  ::
  ++  mo-reboot
    |=  [dap=term =ship]
    ^+  mo-core
    =/  gent  (~(got by yokes.state) dap)
    =*  desk  q.beak.gent
    (mo-boot:(mo-abed control-duct.gent) dap ship desk)
  ::  +mo-goad: rebuild agent(s)
  ::
  ++  mo-goad
    |=  agent=(unit dude)
    ^+  mo-core
    ?^  agent
      ~|  goad-gone+u.agent
      (mo-reboot u.agent our)
    =/  agents=(list term)  ~(tap in ~(key by yokes.state))
    |-  ^+  mo-core
    ?~  agents  mo-core
    $(agents t.agents, mo-core (mo-reboot i.agents our))
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
    |=  [dap=term bek=beak =agent]
    ^+  mo-core
    ::
    =/  existing  (~(get by yokes.state) dap)
    =/  re  ?~(existing "" "re")
    ~>  %slog.[0 leaf+"gall: {re}loading {<dap>}"]
    ::
    ?^  existing
      =.  yokes.state
        (~(put by yokes.state) dap u.existing(beak bek))
      =/  =routes  [disclosing=~ attributing=our]
      =/  ap-core  (ap-abed:ap dap routes)
      =.  ap-core  (ap-reinstall:ap-core agent)
      ap-abet:ap-core
    ::
    =.  yokes.state
      %+  ~(put by yokes.state)  dap
      =/  default-yoke  *yoke
      default-yoke(control-duct hen, beak bek, agent agent)
    ::
    =/  old  mo-core
    =/  wag
      =/  =routes  [disclosing=~ attributing=our]
      =/  ap-core  (ap-abed:ap dap routes)
      (ap-upgrade-state:ap-core ~)
    ::
    =/  maybe-tang  -.wag
    =/  ap-core  +.wag
    ?^  maybe-tang
      =.  mo-core  old
      (mo-give %onto %.n u.maybe-tang)
    ::
    =.  mo-core  ap-abet:ap-core
    =.  mo-core  (mo-clear-queue dap)
    =/  =suss  [dap %boot now]
    (mo-give %onto [%.y suss])
  ::  +mo-subscribe-to-agent-builds: request agent update notices
  ::
  ::    Also subscribe to our own source path, in case we get reloaded
  ::    but none of the agents do.  This way, Clay will still notify us,
  ::    and we'll be able to exit the chrysalis.
  ::
  ++  mo-subscribe-to-agent-builds
    |=  date=@da
    ^+  mo-core
    =.  mo-core  (mo-abed system-duct.state)
    =/  =wire  /sys/lyv
    =.  mo-core  (mo-pass /sys/lyv %c %warp our %home ~)
    =/  =mool:clay
      :-  da+date
      %-  ~(gas in *(set [care:clay path]))
      :*  [%z /sys/hoon/hoon]
          [%z /sys/arvo/hoon]
          [%z /sys/zuse/hoon]
          [%z /sys/vane/gall/hoon]
          %+  turn  ~(tap in ~(key by yokes.state))
          |=  dap=term
          ^-  [care:clay path]
          [%a /app/[dap]/hoon]
      ==
    (mo-pass wire %c %warp our %home ~ %mult mool)
  ::  +mo-scry-agent-cage: read $agent core from clay
  ::
  ++  mo-scry-agent-cage
    |=  [dap=term =case:clay]
    ^-  (each agent tang)
    =/  bek=beak  [our %home case]
    =/  sky  (ski [%141 %noun] ~ %ca bek /hoon/[dap]/app)
    ?~  sky  |+[leaf+"gall: {<dap>} scry blocked"]~
    ?~  u.sky  |+[leaf+"gall: {<dap>} scry failed"]~
    =/  =cage  u.u.sky
    ?.  =(%vase p.cage)
      |+[leaf+"gall: bad mark {<p.cage>} for agent {<dap>}"]~
    =/  res  (mule |.(!<(agent !<(vase q.cage))))
    ?:  ?=(%& -.res)
      &+p.res
    |+[[leaf+"gall: {<dap>} not valid agent"] p.res]
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
        %watch-as  [%l [mark path]:deal]
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
    =.  outstanding.state
      =/  stand
        (~(gut by outstanding.state) [wire hen] *(qeu remote-request))
      (~(put by outstanding.state) [wire hen] (~(put to stand) -.deal))
    (mo-pass wire note-arvo)
  ::  +mo-track-ship: subscribe to ames and jael for notices about .ship
  ::
  ++  mo-track-ship
    |=  =ship
    ^+  mo-core
    ::  if already contacted, no-op
    ::
    ?:  (~(has in contacts.state) ship)
      mo-core
    ::  first contact; update state and subscribe to notifications
    ::
    =.  contacts.state  (~(put in contacts.state) ship)
    ::  ask ames to track .ship's connectivity
    ::
    =.  moves  [[system-duct.state %pass /sys/lag %a %heed ship] moves]
    ::  ask jael to track .ship's breaches
    ::
    =/  =note-arvo  [%j %public-keys (silt ship ~)]
    =.  moves
      [[system-duct.state %pass /sys/era note-arvo] moves]
    mo-core
  ::  +mo-untrack-ship: cancel subscriptions to ames and jael for .ship
  ::
  ++  mo-untrack-ship
    |=  =ship
    ^+  mo-core
    ::  if already canceled, no-op
    ::
    ?.  (~(has in contacts.state) ship)
      mo-core
    ::  delete .ship from state and kill subscriptions
    ::
    =.  contacts.state  (~(del in contacts.state) ship)
    ::
    =.  moves  [[system-duct.state %pass /sys/lag %a %jilt ship] moves]
    ::
    =/  =note-arvo  [%j %nuke (silt ship ~)]
    =.  moves
      [[system-duct.state %pass /sys/era note-arvo] moves]
    mo-core
  ::  +mo-breach: ship breached, so forget about them
  ::
  ++  mo-breach
    |=  =ship
    ^+  mo-core
    =.  mo-core  (mo-untrack-ship ship)
    =.  mo-core  (mo-filter-queue ship)
    =/  agents=(list [name=term =yoke])  ~(tap by yokes.state)
    =.  outstanding.state
      %-  malt
      %+  skip  ~(tap by outstanding.state)
      |=  [[=wire duct] (qeu remote-request)]
      =(/sys/way/(scot %p ship) (scag 3 wire))
    ::
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
      %lyv  (mo-handle-sys-lyv path sign-arvo)
      %era  (mo-handle-sys-era path sign-arvo)
      %cor  (mo-handle-sys-cor path sign-arvo)
      %lag  (mo-handle-sys-lag path sign-arvo)
      %req  (mo-handle-sys-req path sign-arvo)
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
  ::  +mo-handle-sys-cor: receive a built agent from %clay
  ::
  ++  mo-handle-sys-cor
    |=  [=path =sign-arvo]
    ^+  mo-core
    ::
    ?>  ?=([%cor @ @ @ @ ~] path)
    =/  [dap=term her=@ta desk=@ta dat=@ta ~]  t.path
    =/  tim  (slav da+dat)
    =/  =beak  [(slav %p her) desk da+tim]
    ?>  ?=([?(%b %c) %writ *] sign-arvo)
    |^  ^+  mo-core
    ?~  p.sign-arvo
      (fail leaf+"gall: failed to build agent {<dap>}" ~)
    =/  cag=cage  r.u.p.sign-arvo
    ?.  =(%vase p.cag)
      (fail leaf+"gall: bad %writ {<p.cag>} for {<dap>}" ~)
    =/  res  (mule |.(!<(agent !<(vase q.cag))))
    ?:  ?=(%| -.res)
      (fail leaf+["gall: bad agent {<dap>}"] p.res)
    =.  mo-core  (mo-receive-core dap beak p.res)
    (mo-subscribe-to-agent-builds tim)
    ::
    ++  fail
      |=  =tang
      ^+  mo-core
      =.  mo-core  (mo-give %onto |+tang)
      =/  =case  [%da tim]
      =/  =wire  /sys/cor/[dap]/[her]/[desk]/(scot case)
      (mo-pass wire %c %warp p.beak desk ~ %next %a case /app/[dap]/hoon)
    --
  ::  +mo-handle-sys-lyv: handle notice that agents have been rebuilt
  ::
  ++  mo-handle-sys-lyv
    |=  [=path =sign-arvo]
    ^+  mo-core
    ?>  ?=([%lyv ~] path)
    ?>  ?=([?(%b %c) %wris *] sign-arvo)
    =/  bek=beak  [our %home p.sign-arvo]
    =/  nex=(list [=care:clay =^path])  ~(tap in q.sign-arvo)
    ~>  %slog.[0 leaf+"gall: reloading agents"]
    ~<  %slog.[0 leaf+"gall: reloaded agents"]
    =;  cor  (mo-subscribe-to-agent-builds:cor p.p.sign-arvo)
    %+  roll  nex
    |=  [[=care:clay =^path] cor=_mo-core]
    ^+  cor
    ?>  =(%a care)
    =/  dap  dap:;;([%app dap=@tas %hoon ~] path)
    =/  rag  (mo-scry-agent-cage dap p.sign-arvo)
    ?:  ?=(%| -.rag)
      (mean p.rag)
    (mo-receive-core:cor dap bek p.rag)
  ::  +mo-handle-sys-lag: handle an ames %clog notification
  ::
  ++  mo-handle-sys-lag
    |=  [=path =sign-arvo]
    ^+  mo-core
    ::
    ?>  ?=([%lag ~] path)
    ?>  ?=([%a %clog *] sign-arvo)
    ::
    =/  agents=(list term)  ~(tap in ~(key by yokes.state))
    |-  ^+  mo-core
    ?~  agents  mo-core
    ::
    =.  mo-core
      =/  =routes  [disclosing=~ attributing=our]
      =/  app  (ap-abed:ap i.agents routes)
      ap-abet:(ap-clog:app ship.sign-arvo)
    ::
    $(agents t.agents)
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
      =^  remote-request  outstanding.state
        ?~  t.t.t.wire
          =/  full-wire  sys+wire
          =/  stand
            (~(gut by outstanding.state) [full-wire hen] ~)
          ::
          ::  default is to send both ack types; should only hit if
          ::  cleared queue in +load 3-to-4 or +load-4-to-5
          ::
          =?  stand  ?=(~ stand)
            (~(put to *(qeu remote-request)) %missing)
          ~|  [full-wire=full-wire hen=hen stand=stand]
          =^  rr  stand  ~(get to stand)
          [rr (~(put by outstanding.state) [full-wire hen] stand)]
        ::  non-null case of wire is old, remove on next breach after
        ::  2019/12
        ::
        [;;(remote-request i.t.t.t.wire) outstanding.state]
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
    |=  dap=term
    ^+  mo-core
    ?.  (~(has by yokes.state) dap)
      mo-core
    ?~  maybe-blocked=(~(get by blocked.state) dap)
      mo-core
    =/  blocked=(qeu blocked-move)  u.maybe-blocked
    |-  ^+  mo-core
    ?:  =(~ blocked)
      =.  blocked.state  (~(del by blocked.state) dap)
      mo-core
    =^  [=duct =routes =deal]  blocked  ~(get to blocked)
    =/  move
      =/  =sock  [attributing.routes our]
      =/  card  [%slip %g %deal sock dap deal]
      [duct card]
    $(moves [move moves])
  ::  +mo-filter-queue: remove all blocked tasks from ship.
  ::
  ++  mo-filter-queue
    |=  =ship
    =/  agents=(list [name=term blocked=(qeu blocked-move)])
      ~(tap by blocked.state)
    =|  new-agents=(map term (qeu blocked-move))
    |-  ^+  mo-core
    ?~  agents
      mo-core(blocked.state new-agents)
    =|  new-blocked=(qeu blocked-move)
    |-  ^+  mo-core
    ?:  =(~ blocked.i.agents)
      ?~  new-blocked
        ^$(agents t.agents)
      %=  ^$
        agents      t.agents
        new-agents  (~(put by new-agents) name.i.agents new-blocked)
      ==
    =^  mov=blocked-move  blocked.i.agents  ~(get to blocked.i.agents)
    =?  new-blocked  !=(ship attributing.routes.mov)
      (~(put to new-blocked) mov)
    $
  ::  +mo-beak: assemble a beak for the specified agent.
  ::
  ++  mo-beak
    |=  dap=term
    ^-  beak
    ?^  yoke=(~(get by yokes.state) dap)
      beak.u.yoke
    ::  XX this fallback is necessary, as .term could be either the source
    ::  or the destination app. ie, it might not exist locally ...
    ::
    [our %home %da now]
  ::  +mo-peek:  call to +ap-peek (which is not accessible outside of +mo).
  ::
  ++  mo-peek
    ~/  %mo-peek
    |=  [dap=term =routes care=term =path]
    ^-  (unit (unit cage))
    ::
    =/  app  (ap-abed:ap dap routes)
    (ap-peek:app care path)
  ::
  ++  mo-apply
    |=  [dap=term =routes =deal]
    ^+  mo-core
    ?-    -.deal
        ?(%watch %watch-as %leave %poke)
      (mo-apply-sure dap routes deal)
    ::
        %raw-poke
      =/  =case:clay  da+now
      =/  sky  (ski [%141 %noun] ~ %cb [our %home case] /[mark.deal])
      ?-    sky
          ?(~ [~ ~])
        =/  ror  "gall: raw-poke fail :{(trip dap)} {<mark.deal>}"
        (mo-give %unto %poke-ack `[leaf+ror]~)
      ::
          [~ ~ *]
        =+  !<(=dais:clay q.u.u.sky)
        =/  res  (mule |.((vale:dais noun.deal)))
        ?:  ?=(%| -.res)
          =/  ror  "gall: raw-poke vale fail :{(trip dap)} {<mark.deal>}"
          (mo-give %unto %poke-ack `[leaf+ror p.res])
        =.  mo-core
          %+  mo-pass  /nowhere
          [%c %warp our %home ~ %sing %b case /[mark.deal]]
        (mo-apply-sure dap routes [%poke mark.deal p.res])
      ==
    ::
        %poke-as
      =/  =case:clay  da+now
      =/  =mars:clay  [p.cage mark]:deal
      =/  mars-path   /[a.mars]/[b.mars]
      =/  sky  (ski [%141 %noun] ~ %cc [our %home case] (flop mars-path))
      ?-    sky
          ?(~ [~ ~])
        =/  ror  "gall: poke cast fail :{(trip dap)} {<mars>}"
        (mo-give %unto %poke-ack `[leaf+ror]~)
      ::
          [~ ~ *]
        =+  !<(=tube:clay q.u.u.sky)
        =/  res  (mule |.((tube q.cage.deal)))
        ?:  ?=(%| -.res)
          =/  ror  "gall: poke-as cast fail :{(trip dap)} {<mars>}"
          (mo-give %unto %poke-ack `[leaf+ror p.res])
        =.  mo-core
          %+  mo-pass  /nowhere
          [%c %warp our %home ~ %sing %c case /[a.mars]/[b.mars]]
        (mo-apply-sure dap routes [%poke mark.deal p.res])
      ==
    ==
  ::
  ++  mo-apply-sure
    |=  [dap=term =routes =deal]
    ^+  mo-core
    =/  app  (ap-abed:ap dap routes)
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
    =/  is-running  (~(has by yokes.state) agent)
    =/  is-blocked  (~(has by blocked.state) agent)
    ::
    ?:  |(!is-running is-blocked)
      =/  blocked=(qeu blocked-move)
        =/  waiting  (~(get by blocked.state) agent)
        =/  deals  (fall waiting *(qeu blocked-move))
        =/  deal  [hen routes deal]
        (~(put to deals) deal)
      ::
      %-  (slog leaf+"gall: not running {<agent>} yet, got {<-.deal>}" ~)
      %_  mo-core
        blocked.state  (~(put by blocked.state) agent blocked)
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
    ?-    -.ames-response
        ::  %d: diff; ask clay to validate .noun as .mark
        ::
        %d
      =/  =case:clay  da+now
      =/  sky  (ski [%141 %noun] ~ %cb [our %home case] /[mark.ames-response])
      ?-    sky
          ?(~ [~ ~])
        =/  ror  "gall: ames mark fail {<mark.ames-response>}"
        (mo-give %done `vale+[leaf+ror]~)
      ::
          [~ ~ *]
        =+  !<(=dais:clay q.u.u.sky)
        =/  res  (mule |.((vale:dais noun.ames-response)))
        ?:  ?=(%| -.res)
          =/  ror  "gall: ames vale fail {<mark.deal>}"
          (mo-give %done `vale+[leaf+ror p.res])
        =.  mo-core
          %+  mo-pass  /nowhere
          [%c %warp our %home ~ %sing %b case /[mark.ames-response]]
        (mo-give %unto %fact mark.ames-response p.res)
      ==
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
            current-agent=yoke
        ==
    ++  ap-core  .
    ::  +ap-abed: initialise state for an agent, with the supplied routes.
    ::
    ::    The agent must already be running in +gall -- here we simply update
    ::    +ap's state to focus on it.
    ::
    ++  ap-abed
      ~/  %ap-abed
      |=  [dap=term =routes]
      ^+  ap-core
      (ap-yoke dap routes (~(got by yokes.state) dap))
    ::  +ap-hatch: initialize agent state from $egg, after upgrade
    ::
    ++  ap-abut
      |=  [dap=term =egg]
      ^+  ap-core
      =/  res  (mo-scry-agent-cage dap da+now)
      ?:  ?=(%| -.res)
        (mean p.res)
      =/  =yoke  egg(old-state `agent`p.res)
      =/  =routes  [disclosing=~ attributing=our]
      (ap-yoke dap routes yoke)
    ::  +ap-yoke: initialize agent state, starting from a $yoke
    ::
    ++  ap-yoke
      |=  [dap=term =routes =yoke]
      ^+  ap-core
      =.  stats.yoke
        :+  +(change.stats.yoke)
          (shaz (mix (add dap change.stats.yoke) eny))
        now
      =.  agent-name  dap
      =.  agent-routes  routes
      =.  current-agent  yoke
      =.  agent-duct  hen
      ap-core
    ::  +ap-abet: resolve moves.
    ::
    ++  ap-abet
      ^+  mo-core
      ::
      =/  running  (~(put by yokes.state) agent-name current-agent)
      =/  moves
        =/  giver  |=(report=(each suss tang) [hen %give %onto report])
        =/  from-suss  (turn agent-config giver)
        :(weld agent-moves from-suss moves)
      ::
      %_  mo-core
        yokes.state  running
        moves        moves
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
          ~?  &(=(duct system-duct.state) !=(agent-name %hood))
            [%agent-giving-on-system-duct agent-name -.gift]
          [duct %give %unto %kick ~]
        ::
        ?.  ?=(%fact -.gift)
          [agent-duct %give %unto gift]~
        ::
        =/  ducts=(list duct)  (ap-ducts-from-paths paths.gift ~)
        =/  =cage  cage.gift
        %-  zing
        %+  turn  ducts
        |=  =duct
        ~?  &(=(duct system-duct.state) !=(agent-name %hood))
          [%agent-giving-on-system-duct agent-name -.gift]
        ^-  (list move)
        =/  =mark
          (~(gut by marks.current-agent) duct p.cage)
        ::
        ?:  =(mark p.cage)
          [duct %give %unto %fact cage.gift]~
        =/  =mars:clay  [p.cage mark]
        =/  =case:clay  da+now
        =/  bek=beak    [our %home case]
        =/  mars-path  /[a.mars]/[b.mars]
        =/  sky  (ski [%141 %noun] ~ %cc bek (flop mars-path))
        ?-    sky
            ?(~ [~ ~])
          %-  (slog leaf+"watch-as fact conversion find-fail" >sky< ~)
          (ap-kill-up-slip duct)
        ::
            [~ ~ *]
          =+  !<(=tube:clay q.u.u.sky)
          =/  res  (mule |.((tube q.cage)))
          ?:  ?=(%| -.res)
            %-  (slog leaf+"watch-as fact conversion failure" p.res)
            (ap-kill-up-slip duct)
          :~  [duct %pass /nowhere %c %warp our %home ~ %sing %c case mars-path]
              [duct %give %unto %fact b.mars p.res]
          ==
        ==
      ::
          %pass
        =/  =duct  system-duct.state
        =/  =wire  p.card
        =/  =neat  q.card
        =.  wire
          ?:  ?=(%agent -.neat)
            ::  remove `our` in next breach after 2019/12 and reflect in
            ::  +mo-handle-use (non-unto case)
            ::
            :-  (scot %p our)
            [%out (scot %p ship.neat) name.neat wire]
          [(scot %p attributing.agent-routes) wire]
        =.  wire  [%use agent-name wire]
        =/  =note-arvo
          ?-  -.neat
            %arvo   note-arvo.neat
            %agent  [%g %deal [our ship.neat] [name deal]:neat]
          ==
        [duct %pass wire note-arvo]~
      ==
    ::  +ap-breach: ship breached, so forget about them
    ::
    ++  ap-breach
      |=  =ship
      ^+  ap-core
      =/  in=(list [=duct =^ship =path])
        ~(tap by inbound.watches.current-agent)
      |-  ^+  ap-core
      ?^  in
        =?  ap-core  =(ship ship.i.in)
          =/  core  ap-load-delete(agent-duct duct.i.in)
          core(agent-duct agent-duct)
        $(in t.in)
      ::
      =/  out=(list [[=wire =^ship =term] ? =path])
        ~(tap by outbound.watches.current-agent)
      |-  ^+  ap-core
      ?~  out
        ap-core
      =?  ap-core  =(ship ship.i.out)
        =/  core
          =.  agent-duct  system-duct.state
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
        ~(tap by inbound.watches.current-agent)
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
      ?~  target-paths
        ?~  target-ship
          ~[agent-duct]
        %+  murn  ~(tap by inbound.watches.current-agent)
        |=  [=duct =ship =path]
        ^-  (unit ^duct)
        ?:  =(target-ship `ship)
          `duct
        ~
      %-  zing
      %+  turn  target-paths
      |=  =path
      (ap-ducts-from-path path target-ship)
    ::  +ap-ducts-from-path: get ducts subscribed to path
    ::
    ++  ap-ducts-from-path
      |=  [target-path=path target-ship=(unit ship)]
      ^-  (list duct)
      %+  murn  ~(tap by inbound.watches.current-agent)
      |=  [=duct =ship =path]
      ^-  (unit ^duct)
      ?:  ?&  =(target-path path)
              |(=(target-ship ~) =(target-ship `ship))
          ==
        `duct
      ~
    ::  +ap-apply: apply effect.
    ::
    ++  ap-apply
      |=  =deal
      ^+  ap-core
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
      |=  [care=term tyl=path]
      ^-  (unit (unit cage))
      ::  take trailing mark off path for %x scrys
      ::
      =^  want=mark  tyl
        ?.  ?=(%x care)  [%$ tyl]
        =.  tyl  (flop tyl)
        [(head tyl) (flop (tail tyl))]
      ::  call the app's +on-peek, producing [~ ~] if it crashes
      ::
      =/  peek-result=(each (unit (unit cage)) tang)
        (ap-mule-peek |.((on-peek:ap-agent-core [care tyl])))
      ?:  ?=(%| -.peek-result)
        ((slog leaf+"peek bad result" p.peek-result) [~ ~])
      ::  for non-%x scries, or failed %x scries, or %x results that already
      ::  have the requested mark, produce the result as-is
      ::
      ?.  ?&  ?=(%x care)
              ?=([~ ~ *] p.peek-result)
              !=(mark p.u.u.p.peek-result)
          ==
        p.peek-result
      ::  for %x scries, attempt to convert to the requested mark if needed
      ::
      =*  have  p.u.u.p.peek-result
      =*  vase  q.u.u.p.peek-result
      =/  tub=(unit tube:clay)
        ?:  =(have want)  `(bake same ^vase)
        =/  tuc=(unit (unit cage))
          (ski [%141 %noun] ~ %cc [our %home da+now] (flop /[have]/[want]))
        ?.  ?=([~ ~ *] tuc)  ~
        `!<(tube:clay q.u.u.tuc)
      ?~  tub
        ((slog leaf+"peek no tube from {(trip have)} to {(trip want)}" ~) ~)
      =/  res  (mule |.((u.tub vase)))
      ?:  ?=(%& -.res)
        ``want^p.res
      ((slog leaf+"peek failed tube from {(trip have)} to {(trip want)}" ~) ~)
    ::  +ap-update-subscription: update subscription.
    ::
    ++  ap-update-subscription
      ~/  %ap-update-subscription
      |=  [is-ok=? =other=ship other-agent=term =wire]
      ^+  ap-core
      ?:  is-ok
        ap-core
      (ap-kill-down wire [other-ship other-agent])
    ::  +ap-give: return result.
    ::
    ++  ap-give
      |=  =gift:agent
      ^+  ap-core
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
          :*  wex=outbound.watches.current-agent  ::  outgoing
              sup=inbound.watches.current-agent  ::  incoming
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
      =/  internal-moves
        (ap-from-internal %pass path neat)
      ap-core(agent-moves (weld internal-moves agent-moves))
    ::  +ap-reinstall: reinstall.
    ::
    ++  ap-reinstall
      ~/  %ap-reinstall
      |=  =agent
      ^+  ap-core
      =/  old-state=vase  ~(on-save agent.current-agent ap-construct-bowl)
      =^  error  ap-core
        (ap-install(agent.current-agent agent) `old-state)
      ?~  error
        ap-core
      (mean >%load-failed< u.error)
    ::  +ap-subscribe-as: apply %watch-as.
    ::
    ++  ap-subscribe-as
      |=  [=mark =path]
      ^+  ap-core
      =.  marks.current-agent  (~(put by marks.current-agent) agent-duct mark)
      (ap-subscribe path)
    ::  +ap-subscribe: apply %watch.
    ::
    ++  ap-subscribe
      ~/  %ap-subscribe
      |=  pax=path
      ^+  ap-core
      =/  incoming  [attributing.agent-routes pax]
      =.  inbound.watches.current-agent
        (~(put by inbound.watches.current-agent) agent-duct incoming)
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
      =^  maybe-tang  ap-core
        %+  ap-ingest  %poke-ack  |.
        (on-poke:ap-agent-core cage)
      ap-core
    ::  +ap-error: pour error.
    ::
    ++  ap-error
      |=  [=term =tang]
      ^+  ap-core
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
      ~|  wire=wire
      ?>  ?=([%out @ @ *] wire)
      =/  other-ship  (slav %p i.t.wire)
      =/  other-agent  i.t.t.wire
      =/  =dock  [other-ship other-agent]
      =/  agent-wire  t.t.t.wire
      ::  if subscription ack or close, handle before calling user code
      ::
      =?  outbound.watches.current-agent  ?=(%kick -.sign)
        %-  ~(del by outbound.watches.current-agent)
        [agent-wire dock]
      ?:  ?&  ?=(%watch-ack -.sign)
              !(~(has by outbound.watches.current-agent) [agent-wire dock])
          ==
        %-  %:  slog
              leaf+"{<agent-name>}: got ack for nonexistent subscription"
              leaf+"{<dock>}: {<agent-wire>}"
              >wire=wire<
              ~
            ==
        ap-core
      ::
      =?  outbound.watches.current-agent  ?=(%watch-ack -.sign)
        ?^  p.sign
          %-  ~(del by outbound.watches.current-agent)
          [agent-wire dock]
        %+  ~(jab by outbound.watches.current-agent)  [agent-wire dock]
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
      |=  old-agent-state=(unit vase)
      ^-  [(unit tang) _ap-core]
      ::
      =^  maybe-tang  ap-core  (ap-upgrade-state old-agent-state)
      ::
      =.  agent-config
        =/  =term  ?~(old-agent-state %boot %bump)
        =/  possibly-suss
          ?~  maybe-tang
            =/  =suss  [agent-name term now]
            [%.y suss]
          [%.n u.maybe-tang]
        [possibly-suss agent-config]
      ::
      [maybe-tang ap-core]
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
          inbound.watches.current-agent
        (~(del by inbound.watches.current-agent) agent-duct)
      ==
    ::  +ap-load-delete: load delete.
    ::
    ++  ap-load-delete
      ^+  ap-core
      ::
      =/  maybe-incoming
        (~(get by inbound.watches.current-agent) agent-duct)
      ?~  maybe-incoming
        ap-core
      ::
      =/  incoming  u.maybe-incoming
      =.  inbound.watches.current-agent
        (~(del by inbound.watches.current-agent) agent-duct)
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
    ::  +ap-kill-up-slip: 2-sided kill from publisher side by slip
    ::
    ::  +ap-kill-up is reentrant if you call it in the
    ::  middle of processing another deal
    ::
    ::  Should probably call +ap-error with error message
    ::
    ++  ap-kill-up-slip
      |=  =duct
      ^-  (list move)
      ::
      :~  [duct %slip %g %deal [our our] agent-name %leave ~]
          [duct %give %unto %kick ~]
      ==
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
      =.  inbound.watches.current-agent
        (ap-handle-kicks moves)
      (ap-handle-peers moves)
    ::  +ap-handle-kicks: handle cancels of inbound.watches
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
      (~(dif by inbound.watches.current-agent) quit-map)
    ::  +ap-handle-peers: handle new outbound.watches
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
        =.  outbound.watches.current-agent
          (~(del by outbound.watches.current-agent) [short-wire dock])
        $(moves t.moves, new-moves [move new-moves])
      ?.  ?=([* %pass * %g %deal * * ?(%watch %watch-as) *] move)
        $(moves t.moves, new-moves [move new-moves])
      =/  =wire  p.move.move
      ?>  ?=([%use @ @ %out @ @ *] wire)
      =/  short-wire  t.t.t.t.t.t.wire
      =/  =dock  [q.p q]:q.move.move
      =/  =path
        ?-  -.r.q.move.move
          %watch     path.r.q.move.move
          %watch-as  path.r.q.move.move
        ==
      ?:  (~(has by outbound.watches.current-agent) short-wire dock)
        =.  ap-core
          =/  =tang
            ~[leaf+"subscribe wire not unique" >agent-name< >short-wire< >dock<]
          =/  have
            (~(got by outbound.watches.current-agent) short-wire dock)
          %-  (slog >out=have< tang)
          (ap-error %watch-not-unique tang)  ::  reentrant, maybe bad?
        $(moves t.moves)
      =.  outbound.watches.current-agent
        (~(put by outbound.watches.current-agent) [short-wire dock] [| path])
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
  =/  mo-core  (mo-abed:mo duct)
  ?-    -.task
      %conf  mo-abet:(mo-boot:mo-core dap.task our %home)
      %deal
    =/  [=sock =term =deal]  [p q r]:task
    ?.  =(q.sock our)
      ?>  =(p.sock our)
      mo-abet:(mo-send-foreign-request:mo-core q.sock term deal)
    mo-abet:(mo-handle-local:mo-core p.sock term deal)
  ::
      %goad  mo-abet:(mo-goad:mo-core agent.task)
      %init  [~ gall-payload(system-duct.state duct)]
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
    =>  (mo-handle-ames-request:mo-core ship agent-name ames-request)
    mo-abet
  ::
      %sear  mo-abet:(mo-filter-queue:mo-core ship.task)
      %trim  [~ gall-payload]
      %vega  [~ gall-payload]
  ==
::  +load: recreate vane; note, only valid if called from pupa
::
++  load  !!
::  +scry: standard scry
::
++  scry
  ~/  %gall-scry
  |=  [fur=(unit (set monk)) care=term =shop dap=desk =coin =path]
  ^-  (unit (unit cage))
  ?.  ?=(%.y -.shop)
    ~
  =/  =ship  p.shop
  ?:  &(=(care %$) =(path /whey))
    =/  blocked
      =/  queued  (~(run by blocked.state) |=((qeu blocked-move) [%.y +<]))
      (sort ~(tap by queued) aor)
    ::
    =/  running
      =/  active  (~(run by yokes.state) |=(yoke [%.y +<]))
      (sort ~(tap by active) aor)
    ::
    =/  maz=(list mass)
      :~  [%foreign %.y contacts.state]
          [%blocked %.n blocked]
          [%active %.n running]
      ==
    ``mass+!>(maz)
  ::
  ?:  ?&  =(%u care)
          =(~ path)
          =([%$ %da now] coin)
          =(our ship)
      ==
    [~ ~ noun+!>((~(has by yokes.state) dap))]
  ::
  ?.  =(our ship)
    ~
  ?.  =([%$ %da now] coin)
    ~
  ?.  (~(has by yokes.state) dap)
    [~ ~]
  ?.  ?=(^ path)
    ~
  =/  =routes  [~ ship]
  (mo-peek:mo dap routes care path)
::  +stay: save without cache
::
++  stay
  ^-  spore
  =;  eggs=(map term egg)  state(yokes eggs)
  %-  ~(run by yokes.state)
  |=(=yoke `egg`yoke(agent on-save:agent.yoke))
::  +take: response
::
++  take
  ~/  %gall-take
  |=  [=wire =duct dud=(unit goof) hin=(hypo sign-arvo)]
  ^-  [(list move) _gall-payload]
  ?^  dud
    ~&(%gall-take-dud ((slog tang.u.dud) [~ gall-payload]))
  ?:  =(/nowhere wire)
    [~ gall-payload]
  ::
  ~|  [%gall-take-failed wire]
  ::
  ?>  ?=([?(%sys %use) *] wire)
  =/  mo-core  (mo-abed:mo duct)
  =/  =sign-arvo  q.hin
  =>  ?-  i.wire
        %sys  (mo-handle-sys:mo-core t.wire sign-arvo)
        %use  (mo-handle-use:mo-core t.wire hin)
      ==
  mo-abet
--
