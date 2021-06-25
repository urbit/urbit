!:
::  ::  %gall, agent execution
!?  163
::
::::
|=  our=ship
=,  gall
=>
|%
+|  %main
::
::  $move: Arvo-level move
::
+$  move  [=duct move=(wind note-arvo gift-arvo)]
::  $state-8: overall gall state, versioned
::
+$  state-9  [%9 state]
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
::    run-nonce: unique for each rebuild
::    sub-nonce: app-wide global %watch nonce
::    live: is this agent running? TODO document better
::    stats: TODO document
::    watches: incoming and outgoing subscription state
::    agent: agent core
::    beak: compilation source
::    marks: mark conversion requests
::
+$  yoke
  $:  control-duct=duct
      run-nonce=@t
      sub-nonce=_1
      live=?
      =stats
      =watches
      agent=(each agent vase)
      =beak
      marks=(map duct mark)
  ==
::  $blocked-move: enqueued move to an agent
::
+$  blocked-move  [=duct =routes move=(each deal unto)]
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
+$  ames-request-all
  $%  [%0 ames-request]
  ==
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
  $:  %9
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
      run-nonce=@t
      sub-nonce=@
      live=?
      =stats
      =watches
      old-state=(each vase vase)
      =beak
      marks=(map duct mark)
  ==
--
::  pupal gall core, on upgrade
::
=<  =*  adult-gate  .
    =|  =spore
    |=  [now=@da eny=@uvJ rof=roof]
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
      =-  :_  ->
          %+  welp
            (skip -< |=(move ?=([* %give %onto *] +<)))
          [^duct %pass /whiz/gall %$ %whiz ~]~
      =/  adult  adult-core
      =.  state.adult
        [%9 system-duct outstanding contacts yokes=~ blocked]:spore
      =/  mo-core  (mo-abed:mo:adult duct)
      =.  mo-core
        =/  apps=(list [dap=term =egg])  ~(tap by eggs.spore)
        |-  ^+  mo-core
        ?~  apps  mo-core
        ?.  =(%base q.beak.egg.i.apps)
          ~>  %slog.[0 leaf+"gall: suspending {<dap.i.apps>}"]
          =.  old-state.egg.i.apps
            =/  old  old-state.egg.i.apps
            |/?-(-.old %| p.old, %& p.old)
          =/  ap-core  (ap-abut:ap:mo-core i.apps)
          $(apps t.apps, mo-core ap-abet:ap-core)
        ~>  %slog.[0 leaf+"gall: upgrading {<dap.i.apps>}"]
        =/  ap-core  (ap-abut:ap:mo-core i.apps)
        =?  ap-core  ?=(%& -.old-state.egg.i.apps)
          =^  tan  ap-core  (ap-install:ap-core `p.old-state.egg.i.apps)
          ?^  tan
            (mean u.tan)
          ap-core
        $(apps t.apps, mo-core ap-abet:ap-core)
      =.  mo-core  (mo-subscribe-to-agent-builds:mo-core now)
      =^  moves  adult-gate  mo-abet:mo-core
      =?  moves  ?=(^ fec)  (weld moves [u.fec]~)
      [moves adult-gate]
    ::
    ++  call
      |=  [=duct dud=(unit goof) wrapped-task=(hobo task)]
      =*  call-args  +<
      ?:  =(~ eggs.spore)
        ~>  %slog.[0 leaf+"gall: direct morphogenesis"]
        =.  state.adult-gate  spore(eggs *(map term yoke))
        (call:adult-core call-args)
      ?^  dud
        ~>  %slog.[0 leaf+"gall: pupa call dud"]
        (mean >mote.u.dud< tang.u.dud)
      =/  task  ((harden task:gall) wrapped-task)
      ?:  ?=(%vega -.task)
        [~ pupal-gate]
      (molt duct `[duct %slip %g task])
    ::
    ++  scry  scry:adult-core
    ++  stay  spore
    ++  take
      |=  [=wire =duct dud=(unit goof) sign=sign-arvo]
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
      (molt duct `[duct %pass wire %b %huck sign])
    ::
    ++  load
      |^  |=  old=spore-any
          =?  old  ?=(%7 -.old)  (spore-7-to-8 old)
          =?  old  ?=(%8 -.old)  (spore-8-to-9 old)
          ?>  ?=(%9 -.old)
          =.  spore  old
          ?.  =(~ eggs.spore)
            pupal-gate
          ~>  %slog.[0 leaf+"gall: direct morphogenesis"]
          %_  adult-gate
            state  spore(eggs *(map term yoke))
          ==
      ::
      +$  spore-any  $%(^spore spore-8 spore-7)
      +$  spore-7
        $:  %7
            wipe-eyre-subs=_|  ::NOTE  band-aid for #3196
            system-duct=duct
            outstanding=(map [wire duct] (qeu remote-request))
            contacts=(set ship)
            eggs=(map term egg-7)
            blocked=(map term (qeu blocked-move))
        ==
      ::
      +$  spore-8
        $:  %8
            system-duct=duct
            outstanding=(map [wire duct] (qeu remote-request))
            contacts=(set ship)
            eggs=(map term egg-8)
            blocked=(map term (qeu blocked-move))
        ==
      ::
      +$  egg-7  egg-8
      +$  egg-8
        $:  control-duct=duct
            run-nonce=@t
            live=?
            =stats
            watches=watches-8
            old-state=(each vase vase)
            =beak
            marks=(map duct mark)
        ==
      ::
      +$  watches-8  [inbound=bitt outbound=boat-8]
      +$  boat-8  (map [wire ship term] [acked=? =path])
      ::
      ++  spore-7-to-8
        |=  old=spore-7
        ^-  spore-8
        :-  %8
        =.  eggs.old
          %-  ~(urn by eggs.old)
          |=  [a=term e=egg-7]
          ::NOTE  kiln will kick off appropriate app revival
          e(old-state [%| p.old-state.e])
        +>.old
      ::
      ++  spore-8-to-9
        |=  old=spore-8
        ^-  ^spore
        =-  old(- %9, eggs -)
        %-  ~(run by eggs.old)
        |=  =egg-8
        ^-  egg
        :*  control-duct.egg-8
            run-nonce.egg-8
            sub-nonce=0
            live.egg-8
            stats.egg-8
            [inbound.watches.egg-8 (boat-8-to-9 outbound.watches.egg-8)]
            [old-state beak marks]:egg-8
        ==
      ::
      ++  boat-8-to-9
        |=  =boat-8
        ^-  boat
        %-  ~(run by boat-8)
        |=  [acked=? =path]
        [acked path nonce=0]
      --
    --
::  adult gall vane interface, for type compatibility with pupa
::
=|  state=state-9
|=  [now=@da eny=@uvJ rof=roof]
=*  gall-payload  .
=<  ~%  %gall-wrap  ..mo  ~
    |%
    ++  call  ^call
    ++  load  ^load
    ++  scry  ^scry
    ++  stay  ^stay
    ++  take  ^take
    --
~%  %gall-top  ..part  ~
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
  ++  mo-give  |=(g=gift mo-core(moves [[hen give+g] moves]))
  ++  mo-past
    |=  =(list [wire note-arvo])
    ?~  list
      mo-core
    =.  mo-core  (mo-pass i.list)
    $(list t.list)
  ::  +mo-jolt: (re)start agent if not already started on this desk
  ::
  ++  mo-jolt
    |=  [dap=term =ship =desk]
    ^+  mo-core
    =/  yak  (~(get by yokes.state) dap)
    ?~  yak
      (mo-boot dap ship desk)
    ?.  -.agent.u.yak
      (mo-boot dap ship desk)
    ?.  =(desk q.beak.u.yak)
      (mo-boot dap ship desk)
    mo-core
  ::  +mo-boot: ask %ford to build us a core for the specified agent.
  ::
  ++  mo-boot
    |=  [dap=term =ship =desk]
    ^+  mo-core
    =/  =case  [%da now]
    =/  =wire  /sys/cor/[dap]/(scot %p ship)/[desk]/(scot case)
    (mo-pass wire %c %warp ship desk ~ %sing %a case /app/[dap]/hoon)
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
    =/  yak  (~(get by yokes.state) dap)
    =/  tex
      ?~  yak  "installing"
      ?-  -.agent.u.yak
        %&  "reloading"
        %|  "reviving"
      ==
    ~>  %slog.[0 leaf+"gall: {tex} {<dap>}"]
    ::
    ?^  yak
      =.  yokes.state
        (~(put by yokes.state) dap u.yak(beak bek))
      =/  ap-core  (ap-abed:ap dap `our)
      =.  ap-core  (ap-reinstall:ap-core agent)
      =.  mo-core  ap-abet:ap-core
      (mo-clear-queue dap)
    ::
    =.  yokes.state
      %+  ~(put by yokes.state)  dap
      %*  .  *yoke
        control-duct  hen
        beak          bek
        agent         &+agent
        run-nonce         (scot %uw (end 5 (shas %yoke-nonce eny)))
      ==
    ::
    =/  old  mo-core
    =/  wag
      =/  ap-core  (ap-abed:ap dap `our)
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
    ::
    =/  sources=(jug desk [care:clay path])
      %+  ~(put by *(jug desk [care:clay path]))  %base
      %-  sy
      :~  [%z /sys/hoon/hoon]
          [%z /sys/arvo/hoon]
          [%z /sys/lull/hoon]
          [%z /sys/zuse/hoon]
          [%z /sys/vane/gall/hoon]
          [%z /sys/kelvin]
      ==
    ::
    =.  sources
      =/  apps=(list [dap=term =yoke])  ~(tap by yokes.state)
      |-  ^+  sources
      ?~  apps
        sources
      =?  sources  ?=(%& -.agent.yoke.i.apps)
        (~(put ju sources) q.beak.yoke.i.apps %a /app/[dap.i.apps]/hoon)
      $(apps t.apps)
    ::
    %-  mo-past
    %-  zing
    %+  turn  ~(tap by sources)
    |=  [=desk paths=(set [care:clay path])]
    :~  [/sys/lyv %c %warp our desk ~]
        [/sys/lyv %c %warp our desk ~ %mult da+date paths]
    ==
  ::  +mo-scry-agent-cage: read $agent core from clay
  ::
  ++  mo-scry-agent-cage
    |=  [dap=term =desk =case:clay]
    ^-  (each agent tang)
    =/  bek=beak  [our desk case]
    =/  sky  (rof ~ %ca bek /app/[dap]/hoon)
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
    =/  =ames-request-all
      :-  %0
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
      [%a %plea ship %g path ames-request-all]
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
    |=  [=wire =sign-arvo]
    ^+  mo-core
    ::
    ?+  -.wire  !!
      %lyv  (mo-handle-sys-lyv wire sign-arvo)
      %era  (mo-handle-sys-era wire sign-arvo)
      %cor  (mo-handle-sys-cor wire sign-arvo)
      %lag  (mo-handle-sys-lag wire sign-arvo)
      %req  (mo-handle-sys-req wire sign-arvo)
      %way  (mo-handle-sys-way wire sign-arvo)
    ==
  ::  +mo-handle-sys-era: receive update about contact
  ::
  ++  mo-handle-sys-era
    |=  [=wire =sign-arvo]
    ^+  mo-core
    ?>  ?=([%jael %public-keys *] sign-arvo)
    ?>  ?=([%era ~] wire)
    ?.  ?=(%breach -.public-keys-result.sign-arvo)
      mo-core
    (mo-breach who.public-keys-result.sign-arvo)
  ::  +mo-handle-sys-cor: receive a built agent from %clay
  ::
  ++  mo-handle-sys-cor
    |=  [=wire =sign-arvo]
    ^+  mo-core
    ::
    ?>  ?=([%cor @ @ @ @ ~] wire)
    =/  [dap=term her=@ta desk=@ta dat=@ta ~]  t.wire
    =/  =beak  [(slav %p her) desk da+now]
    ?>  ?=([?(%behn %clay) %writ *] sign-arvo)
    ?~  p.sign-arvo
      (mean leaf+"gall: failed to build agent {<dap>}" ~)
    =/  cag=cage  r.u.p.sign-arvo
    ?.  =(%vase p.cag)
      (mean leaf+"gall: bad %writ {<p.cag>} for {<dap>}" ~)
    =/  res  (mule |.(!<(agent !<(vase q.cag))))
    ?:  ?=(%| -.res)
      (mean leaf+["gall: bad agent {<dap>}"] p.res)
    =.  mo-core  (mo-receive-core dap beak p.res)
    (mo-subscribe-to-agent-builds now)
  ::  +mo-handle-sys-lyv: handle notice that agents have been rebuilt
  ::
  ++  mo-handle-sys-lyv
    |=  [=wire =sign-arvo]
    ^+  mo-core
    ?>  ?=([%lyv ~] wire)
    ?>  ?=([?(%behn %clay) %wris *] sign-arvo)
    =/  nex=(list [=care:clay =path])  ~(tap in q.sign-arvo)
    ~>  %slog.[0 leaf+"gall: reloading agents"]
    ~<  %slog.[0 leaf+"gall: reloaded agents"]
    =;  cor  (mo-subscribe-to-agent-builds:cor now)
    %+  roll  nex
    |=  [[=care:clay =path] cor=_mo-core]
    ^+  cor
    ::  We throw away %z results because we only have them to guarantee
    ::  molting.  Clay will tell us if e.g. changing hoon.hoon affects
    ::  the result of a particular app (usually it will).
    ::
    ?.  =(%a care)
      cor
    ~|  path=path
    =/  dap  dap:;;([%app dap=@tas %hoon ~] path)
    =/  yok=(unit yoke)  (~(get by yokes.state) dap)
    ?~  yok
      ~>  %slog.[0 leaf+"gall: no agent to reload: {<dap>}"]
      cor
    ?:  ?=(%| -.agent.u.yok)
      ~>  %slog.[0 leaf+"gall: dead agent reload: {<dap>}"]
      cor
    =/  bek=beak  [our q.beak.u.yok da+now]
    =/  rag  (mo-scry-agent-cage dap q.bek da+now)
    ?:  ?=(%| -.rag)
      (mean p.rag)
    (mo-receive-core:cor dap bek p.rag)
  ::  +mo-handle-sys-lag: handle an ames %clog notification
  ::
  ++  mo-handle-sys-lag
    |=  [=wire =sign-arvo]
    ^+  mo-core
    ::
    ?>  ?=([%lag ~] wire)
    ?>  ?=([%ames %clog *] sign-arvo)
    ::
    =/  agents=(list term)  ~(tap in ~(key by yokes.state))
    |-  ^+  mo-core
    ?~  agents  mo-core
    ::
    =.  mo-core
      =/  app  (ap-abed:ap i.agents `our)
      ap-abet:(ap-clog:app ship.sign-arvo)
    ::
    $(agents t.agents)
  ::  +mo-handle-sys-req: TODO description
  ::
  ::    TODO: what should we do if the remote nacks our %pull?
  ++  mo-handle-sys-req
    |=  [=wire =sign-arvo]
    ^+  mo-core
    ::
    ?>  ?=([%req @ @ ~] wire)
    =/  him  (slav %p i.t.wire)
    =/  dap  i.t.t.wire
    ::
    ?>  ?=([?(%gall %behn) %unto *] sign-arvo)
    =/  =unto  +>.sign-arvo
    ::
    ?-    -.unto
        %raw-fact  ~|([%gall-raw-req wire] !!)
        %poke-ack
      =/  err=(unit error:ames)
        ?~  p.unto  ~
        `[%poke-ack u.p.unto]
      (mo-give %done err)
    ::
        %fact
      =+  [mark noun]=[p q.q]:cage.unto
      (mo-give %boon %d mark noun)
    ::
        %kick
      (mo-give %boon %x ~)
    ::
        %watch-ack
      =/  err=(unit error:ames)
        ?~  p.unto  ~
        `[%watch-ack u.p.unto]
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
        [%ames %done *]
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
            ~&  [%gall-missing wire hen]
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
        [%ames %boon *]
      ?^  t.t.t.wire
        ::  kill subscriptions which use the old wire format
        ::
        !!
      =/  =ames-response  ;;(ames-response payload.sign-arvo)
      (mo-handle-ames-response ames-response)
    ::
        [%ames %lost *]
      ::  note this should only happen on reverse bones, so only facts
      ::  and kicks
      ::
      ::  TODO: %drip %kick so app crash can't kill the remote %pull
      ::
      =.  mo-core  (mo-send-foreign-request ship foreign-agent %leave ~)
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
    |=  [=wire =sign-arvo]
    ^+  mo-core
    ::
    ?.  ?=([@ @ @ *] wire)
      ~&  [%mo-handle-use-bad-wire wire]
      !!
    ::
    =/  dap=term  i.wire
    =/  yoke  (~(get by yokes.state) dap)
    ?~  yoke
      %-  (slog leaf+"gall: {<dap>} dead, got {<+<.sign-arvo>}" ~)
      mo-core
    ?.  =(run-nonce.u.yoke i.t.wire)
      %-  (slog leaf+"gall: got old {<+<.sign-arvo>} for {<dap>}" ~)
      mo-core
    ?.  ?=([?(%gall %behn) %unto *] sign-arvo)
      ?:  ?=(%| -.agent.u.yoke)
        %-  (slog leaf+"gall: {<dap>} dozing, dropping {<+<.sign-arvo>}" ~)
        mo-core
      =/  app
        =/  =ship  (slav %p i.t.t.wire)
        =/  =routes  [disclosing=~ attributing=ship]
        (ap-abed:ap dap routes)
      ::
      =.  app  (ap-generic-take:app t.t.t.wire sign-arvo)
      ap-abet:app
    ?>  ?=([%out @ @ *] t.t.wire)
    =/  =ship  (slav %p i.t.t.t.wire)
    =/  =routes  [disclosing=~ attributing=ship]
    =/  =unto  +>.sign-arvo
    ?:  ?=(%| -.agent.u.yoke)
      =/  blocked=(qeu blocked-move)
        =/  waiting  (~(get by blocked.state) dap)
        =/  deals  (fall waiting *(qeu blocked-move))
        =/  deal  [hen routes |+unto]
        (~(put to deals) deal)
      ::
      %-  (slog leaf+"gall: {<dap>} dozing, got {<-.unto>}" ~)
      %_  mo-core
        blocked.state  (~(put by blocked.state) dap blocked)
      ==
    =/  app  (ap-abed:ap dap routes)
    =.  app
      (ap-specific-take:app t.t.wire unto)
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
    =^  [=duct =routes blocker=(each deal unto)]  blocked
      ~(get to blocked)
    ?:  ?=(%| -.blocker)  $
    =/  =move
      =/  =sock  [attributing.routes our]
      =/  card   [%slip %g %deal sock dap p.blocker]
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
  ::  +mo-idle: put agent to sleep
  ::
  ++  mo-idle
    |=  dap=dude
    ^+  mo-core
    ?.  (~(has by yokes.state) dap)
      ~>  %slog.0^leaf/"gall: ignoring %idle for {<dap>}, not running"
      mo-core
    ap-abet:ap-idle:(ap-abed:ap dap `our)
  ::  +mo-nuke: delete agent completely
  ::
  ++  mo-nuke
    |=  dap=dude
    ^+  mo-core
    ?.  (~(has by yokes.state) dap)
      ~>  %slog.0^leaf/"gall: ignoring %nuke for {<dap>}, not running"
      mo-core
    ~>  %slog.0^leaf/"gall: nuking {<dap>}"
    =.  mo-core  ap-abet:ap-nuke:(ap-abed:ap dap `our)
    mo-core(yokes.state (~(del by yokes.state) dap))
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
      =/  =desk  q.beak:(~(got by yokes.state) dap)
      =/  sky  (rof ~ %cb [our desk case] /[mark.deal])
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
          [%c %warp our desk ~ %sing %b case /[mark.deal]]
        (mo-apply-sure dap routes [%poke mark.deal p.res])
      ==
    ::
        %poke-as
      =/  =case:clay  da+now
      =/  =mars:clay  [p.cage mark]:deal
      =/  mars-path   /[a.mars]/[b.mars]
      =/  =desk  q.beak:(~(got by yokes.state) dap)
      =/  sky  (rof ~ %cc [our desk case] mars-path)
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
          [%c %warp our desk ~ %sing %c case /[a.mars]/[b.mars]]
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
  ::    If the agent is not running or blocked, assign it the supplied
  ::    +deal.  Otherwise simply apply the action to the agent.
  ::
  ++  mo-handle-local
    |=  [=ship agent=term =deal]
    ^+  mo-core
    ::
    =/  =routes  [disclosing=~ attributing=ship]
    =/  running  (~(get by yokes.state) agent)
    =/  is-running  ?~(running %| ?=(%& -.agent.u.running))
    =/  is-blocked  (~(has by blocked.state) agent)
    ::
    ?:  |(!is-running is-blocked)
      =/  blocked=(qeu blocked-move)
        =/  waiting  (~(get by blocked.state) agent)
        =/  deals  (fall waiting *(qeu blocked-move))
        =/  deal  [hen routes &+deal]
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
      ::  %d: diff; ask clay to validate .noun as .mark
      ::  %x: kick; tell agent the publisher canceled the subscription
      ::
    ?-  -.ames-response
      %d  (mo-give %unto %raw-fact mark.ames-response noun.ames-response)
      %x  (mo-give %unto %kick ~)
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
            =yoke
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
      =/  yak=^yoke
        ?:  ?=(%| -.old-state.egg)
          egg
        =/  res  (mo-scry-agent-cage dap q.beak.egg da+now)
        ?:  ?=(%| -.res)
          (mean p.res)
        egg(p.old-state `agent`p.res)
      (ap-yoke dap `our yak)
    ::  +ap-yoke: initialize agent state, starting from a $yoke
    ::
    ++  ap-yoke
      |=  [dap=term =routes yak=^yoke]
      ^+  ap-core
      =.  stats.yak
        :+  +(change.stats.yak)
          (shaz (mix (add dap change.stats.yak) eny))
        now
      =.  agent-name  dap
      =.  agent-routes  routes
      =.  yoke  yak
      =.  agent-duct  hen
      ap-core
    ::  +ap-abet: resolve moves.
    ::
    ++  ap-abet
      ^+  mo-core
      ::
      =/  running  (~(put by yokes.state) agent-name yoke)
      =/  moves
        =/  giver  |=(report=(each suss tang) [hen %give %onto report])
        =/  from-suss  (turn agent-config giver)
        :(weld agent-moves from-suss moves)
      ::
      %_  mo-core
        yokes.state  running
        moves        moves
      ==
    ::
    ++  ap-idle
      ?:  ?=(%| -.agent.yoke)  ap-core
      ap-core(agent.yoke |+on-save:ap-agent-core)
    ::
    ++  ap-nuke
      ^+  ap-core
      =/  out=(list [[=wire =ship =term] ? =path nonce=@])
        ~(tap by outbound.watches.yoke)
      =/  inbound-paths=(set path)
        %-  silt
        %+  turn  ~(tap by inbound.watches.yoke)
        |=  [=duct =ship =path]
        path
      =/  will=(list card:agent:gall)
        %+  welp
          ?:  =(~ inbound-paths)
            ~
          [%give %kick ~(tap in inbound-paths) ~]~
        %+  turn  ~(tap by outbound.watches.yoke)
        |=  [[=wire =ship =term] ? =path nonce=@]
        [%pass wire %agent [ship term] %leave ~]
      =^  maybe-tang  ap-core  (ap-ingest ~ |.([will *agent]))
      ap-core
    ::  +ap-from-internal: internal move to move.
    ::
    ::    We convert from cards to duct-indexed moves when resolving
    ::    them in Arvo.
    ::
    ::    We accept %huck to "fake" being a message to a ship but
    ::    actually send it to a vane.
    ::
    +$  neet
      $%  neat
          [%huck [=ship name=term] =note-arvo]
      ==
    ::
    ++  ap-from-internal
      ~/  %ap-from-internal
      |=  card=(wind neet gift:agent)
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
        ^-  (list move)
        ~?  &(=(duct system-duct.state) !=(agent-name %hood))
          [%agent-giving-on-system-duct agent-name -.gift]
        =/  =mark  (~(gut by marks.yoke) duct p.cage)
        ::
        ?:  =(mark p.cage)
          [duct %give %unto %fact cage.gift]~
        =/  =mars:clay  [p.cage mark]
        =/  =case:clay  da+now
        =/  bek=beak    [our q.beak.yoke case]
        =/  mars-path  /[a.mars]/[b.mars]
        =/  sky  (rof ~ %cc bek mars-path)
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
          :~  :*  duct  %pass  /nowhere  %c  %warp  our  q.beak.yoke  ~
                  %sing  %c  case  mars-path
              ==
              [duct %give %unto %fact b.mars p.res]
          ==
        ==
      ::
          %pass
        =/  =duct  system-duct.state
        =/  =wire  p.card
        =/  =neet  q.card
        ?:  ?=(%pyre -.neet)
          %:  mean
            leaf/"gall: %pyre from {<agent-name>}, killing event"
            leaf/"wire: {<wire>}"
            tang.neet
          ==
        =.  wire
          :^  %use  agent-name  run-nonce.current-agent
          ?-  -.neet
            %agent  [%out (scot %p ship.neet) name.neet wire]
            %huck   [%out (scot %p ship.neet) name.neet wire]
            %arvo   [(scot %p attributing.agent-routes) wire]
          ==
        ::
        =/  =note-arvo
          ?-  -.neet
            %arvo   note-arvo.neet
            %huck   note-arvo.neet
            %agent  [%g %deal [our ship.neet] [name deal]:neet]
          ==
        [system-duct.state %pass wire note-arvo]~
      ==
    ::  +ap-breach: ship breached, so forget about them
    ::
    ++  ap-breach
      |=  =ship
      ^+  ap-core
      =/  in=(list [=duct =^ship =path])
        ~(tap by inbound.watches.yoke)
      |-  ^+  ap-core
      ?^  in
        =?  ap-core  =(ship ship.i.in)
          =/  core  ap-load-delete(agent-duct duct.i.in)
          core(agent-duct agent-duct)
        $(in t.in)
      ::
      =/  out=(list [[=wire =^ship =term] ? =path nonce=@])
        ~(tap by outbound.watches.yoke)
      |-  ^+  ap-core
      ?~  out
        ap-core
      =?  ap-core  =(ship ship.i.out)
        =/  core
          =.  agent-duct  system-duct.state
          =/  way
            [%out (scot %p ship) term.i.out (scot %ud nonce.i.out) wire.i.out]
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
        ~(tap by inbound.watches.yoke)
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
      ?>  ?=(%& -.agent.yoke)
      ~(. p.agent.yoke ap-construct-bowl)
    ::  +ap-ducts-from-paths: get ducts subscribed to paths
    ::
    ++  ap-ducts-from-paths
      |=  [target-paths=(list path) target-ship=(unit ship)]
      ^-  (list duct)
      ?~  target-paths
        ?~  target-ship
          ~[agent-duct]
        %+  murn  ~(tap by inbound.watches.yoke)
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
      %+  murn  ~(tap by inbound.watches.yoke)
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
          (rof ~ %cc [our q.beak.yoke da+now] /[have]/[want])
        ?.  ?=([~ ~ *] tuc)  ~
        `!<(tube:clay q.u.u.tuc)
      ?~  tub
        ((slog leaf+"peek no tube from {(trip have)} to {(trip want)}" ~) ~)
      =/  res  (mule |.((u.tub vase)))
      ?:  ?=(%& -.res)
        ``want^p.res
      ((slog leaf+"peek failed tube from {(trip have)} to {(trip want)}" ~) ~)
    ::  +ap-move: send move
    ::
    ++  ap-move
      |=  =(list move)
      ap-core(agent-moves (weld (flop list) agent-moves))
    ::  +ap-give: return result.
    ::
    ++  ap-give
      |=  =gift:agent
      (ap-move (ap-from-internal %give gift))
    ::  +ap-pass: request action.
    ::
    ++  ap-pass
      |=  [=path =neet]
      (ap-move (ap-from-internal %pass path neet))
    ::  +ap-construct-bowl: set up bowl.
    ::
    ++  ap-construct-bowl
      ^-  bowl
      :*  :*  our                                     ::  host
              attributing.agent-routes                ::  guest
              agent-name                              ::  agent
          ==                                          ::
          :*  wex=outbound.watches.yoke               ::  outgoing
              sup=inbound.watches.yoke                ::  incoming
          ==                                          ::
          :*  act=change.stats.yoke                   ::  tick
              eny=eny.stats.yoke                      ::  nonce
              now=time.stats.yoke                     ::  time
              byk=beak.yoke                           ::  source
      ==  ==
    ::  +ap-reinstall: reinstall.
    ::
    ++  ap-reinstall
      ~/  %ap-reinstall
      |=  =agent
      ^+  ap-core
      =/  old-state=vase
        ?:  ?=(%& -.agent.yoke)
          on-save:ap-agent-core
        p.agent.yoke
      =^  error  ap-core
        (ap-install(agent.yoke &+agent) `old-state)
      ?~  error
        ap-core
      (mean >%load-failed< u.error)
    ::  +ap-subscribe-as: apply %watch-as.
    ::
    ++  ap-subscribe-as
      |=  [=mark =path]
      ^+  ap-core
      =.  marks.yoke  (~(put by marks.yoke) agent-duct mark)
      (ap-subscribe path)
    ::  +ap-subscribe: apply %watch.
    ::
    ++  ap-subscribe
      ~/  %ap-subscribe
      |=  pax=path
      ^+  ap-core
      =/  incoming  [attributing.agent-routes pax]
      =.  inbound.watches.yoke
        (~(put by inbound.watches.yoke) agent-duct incoming)
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
      |=  [=wire =unto]
      ^+  ap-core
      ~|  wire=wire
      ?>  ?=([%out @ @ *] wire)
      =/  other-ship  (slav %p i.t.wire)
      =/  other-agent  i.t.t.wire
      =/  =dock  [other-ship other-agent]
      =/  agent-wire  t.t.t.wire
      ::
      =^  =sign:agent  ap-core
        ?.  ?=(%raw-fact -.unto)
          [unto ap-core]
        =/  =case:clay  da+now
        ?:  ?=(%spider agent-name)
          :-  [%fact mark.unto !>(noun.unto)]
          ap-core
        =/  sky  (rof ~ %cb [our q.beak.yoke case] /[mark.unto])
        ?.  ?=([~ ~ *] sky)
          (mean leaf+"gall: ames mark fail {<mark.unto>}" ~)
        ::
        =+  !<(=dais:clay q.u.u.sky)
        =/  res  (mule |.((vale:dais noun.unto)))
        ?:  ?=(%| -.res)
          (mean leaf+"gall: ames vale fail {<mark.unto>}" p.res)
        :-  [%fact mark.unto p.res]
        %-  ap-move  :_  ~
        :^  hen  %pass  /nowhere
        [%c %warp our q.beak.yoke ~ %sing %b case /[mark.unto]]
      |^  ^+  ap-core
      ::  %poke-ack has no nonce
      ::
      ?:  ?=(%poke-ack -.sign)
        ingest-and-check-error
      ::  pop nonce off .agent-wire and match against stored subscription
      =^  nonce=@  agent-wire  [(slav %ud (head agent-wire)) (tail agent-wire)]
      =/  sub-key  [agent-wire dock]
      =/  wat  (~(get by outbound.watches.yoke) sub-key)
      ?~  wat
        ::  we should be subscribed, but if not, no-op for integrity
        ::
        %.  ap-core
        %-  slog  :~
          leaf+"{<agent-name>}: got {<-.sign>} for nonexistent subscription"
          leaf+"{<dock>}: {<agent-wire>}"
          >wire=wire<
        ==
      ::  make sure wire nonce matches stored nonce
      ::
      ?.  =(nonce.u.wat nonce)
        %.  ap-core
        %-  slog  :~
          =/  nonces  [expected=nonce.u.wat got=nonce]
          =/  ok  |(?=(?(%fact %kick) -.sign) =(~ p.sign))
          leaf+"{<agent-name>}: stale %watch-ack {<nonces>} ok={<ok>}"
        ::
          leaf+"{<dock>}: {<agent-wire>}"
          >wire=wire<
        ==
      ?-    -.sign
          %fact
        =^  tan  ap-core  ingest
        ?~  tan  ap-core
        =.  ap-core  (ap-kill-down sub-key)
        (ap-error -.sign leaf/"take %fact failed, closing subscription" u.tan)
      ::
          %kick
        ::  if subscription ack or close, handle before calling user code
        ::
        =.  outbound.watches.yoke
          %-  ~(del by outbound.watches.yoke)
          [agent-wire dock]
        ::
        ingest-and-check-error
      ::
          %watch-ack
        ?.  (~(has by outbound.watches.yoke) sub-key)
          %-  %:  slog
                  leaf+"{<agent-name>}: got ack for nonexistent subscription"
                  leaf+"{<dock>}: {<agent-wire>}"
                  >wire=wire<
                  ~
                ==
          ap-core
        =.  outbound.watches.yoke
          ?^  p.sign
            (~(del by outbound.watches.yoke) sub-key)
          ::
          %+  ~(jab by outbound.watches.yoke)  sub-key
          |=  val=[acked=? =path nonce=@]
          =?  .  acked.val
            %.(. (slog leaf+"{<agent-name>} 2nd watch-ack on {<val>}" ~))
          val(acked &)
        ::
        ingest-and-check-error
      ==
      ++  ingest  (ap-ingest ~ |.((on-agent:ap-agent-core agent-wire sign)))
      ++  ingest-and-check-error
        ^+  ap-core
        =^  tan  ap-core  ingest
        ?~(tan ap-core (ap-error -.sign leaf/"take {<-.sign>} failed" u.tan))
      --
    ::  +ap-install: install wrapper.
    ::
    ++  ap-install
      |=  old-agent-state=(unit vase)
      ^-  [(unit tang) _ap-core]
      ::
      =^  maybe-tang  ap-core  (ap-upgrade-state old-agent-state)
      ::
      =.  agent-config
        :_  agent-config
        ^-  (each suss tang)
        ?^  maybe-tang
          |/u.maybe-tang
        &/[agent-name ?~(old-agent-state %boot %bump) now]
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
          inbound.watches.yoke
        (~(del by inbound.watches.yoke) agent-duct)
      ==
    ::  +ap-load-delete: load delete.
    ::
    ++  ap-load-delete
      ^+  ap-core
      ::
      =/  maybe-incoming
        (~(get by inbound.watches.yoke) agent-duct)
      ?~  maybe-incoming
        ap-core
      ::
      =/  incoming  u.maybe-incoming
      =.  inbound.watches.yoke
        (~(del by inbound.watches.yoke) agent-duct)
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
      (ap-pass wire %huck dock %b %huck `sign-arvo`[%gall %unto %kick ~])
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
      =/  res  (mock [run %9 2 %0 1] (look rof ~))
      ?-  -.res
        %0  [%& !<(step:agent [-:!>(*step:agent) p.res])]
        %1  [%| (smyt ;;(path p.res)) ~]
        %2  [%| p.res]
      ==
    ::  +ap-mule-peek: same as +ap-mule but for (unit (unit cage))
    ::
    ++  ap-mule-peek
      |=  run=_^?(|.(*(unit (unit cage))))
      ^-  (each (unit (unit cage)) tang)
      =/  res  (mock [run %9 2 %0 1] (look rof ~))
      ?-  -.res
        %0  [%& !<((unit (unit cage)) [-:!>(*(unit (unit cage))) p.res])]
        %1  [%| (smyt ;;(path p.res)) ~]
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
      =.  agent.yoke  &++.p.result
      =/  moves  (zing (turn -.p.result ap-from-internal))
      =.  inbound.watches.yoke
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
      (~(dif by inbound.watches.yoke) quit-map)
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
        =.  outbound.watches.yoke
          (~(del by outbound.watches.yoke) [short-wire dock])
        $(moves t.moves, new-moves [move new-moves])
      ?.  ?=([* %pass * %g %deal * * ?(%watch %watch-as) *] move)
        $(moves t.moves, new-moves [move new-moves])
      =/  =wire  p.move.move
      ?>  ?=([%use @ @ %out @ @ *] wire)
      =/  sys-wire=^wire  (scag 6 `^wire`wire)
      =/  sub-wire=^wire  (slag 6 `^wire`wire)
      =/  [=dock =deal]  [[q.p q] r]:q.move.move
      ::
      ?:  (~(has by outbound.watches.yoke) sub-wire dock)
        =.  ap-core
          =/  =tang
            ~[leaf+"subscribe wire not unique" >agent-name< >sub-wire< >dock<]
          =/  have
            (~(got by outbound.watches.yoke) sub-wire dock)
          %-  (slog >out=have< tang)
          (ap-error %watch-not-unique tang)  ::  reentrant, maybe bad?
        $(moves t.moves)
      ::
      =.  p.move.move
        (weld sys-wire [(scot %ud sub-nonce.yoke) sub-wire])
      %=    $
          moves  t.moves
          new-moves  [move new-moves]
          sub-nonce.current-agent  +(sub-nonce.current-agent)
          outbound.watches.current-agent
        %+  ~(put by outbound.watches.yoke)  [sub-wire dock]
        :+  acked=|
          path=?+(-.deal !! %watch path.deal, %watch-as path.deal)
        sub-nonce.yoke
      ==
    --
  --
::  +call: request
::
++  call
  ~%  %gall-call  +>   ~
  |=  [=duct dud=(unit goof) hic=(hobo task)]
  ^-  [(list move) _gall-payload]
  ?^  dud
    ~|(%gall-call-dud (mean tang.u.dud))
  ::
  ~|  [%gall-call-failed duct hic]
  =/  =task  ((harden task) hic)
  ::
  =/  mo-core  (mo-abed:mo duct)
  ?-    -.task
      %deal
    =/  [=sock =term =deal]  [p q r]:task
    ?.  =(q.sock our)
      ?>  =(p.sock our)
      mo-abet:(mo-send-foreign-request:mo-core q.sock term deal)
    mo-abet:(mo-handle-local:mo-core p.sock term deal)
  ::
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
    =+  ;;(=ames-request-all noun)
    ?>  ?=(%0 -.ames-request-all)
    =>  (mo-handle-ames-request:mo-core ship agent-name +.ames-request-all)
    mo-abet
  ::
      %sear  mo-abet:(mo-filter-queue:mo-core ship.task)
      %jolt  mo-abet:(mo-jolt:mo-core dude.task our desk.task)
      %idle  mo-abet:(mo-idle:mo-core dude.task)
      %nuke  mo-abet:(mo-nuke:mo-core dude.task)
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
  ^-  roon
  |=  [lyc=gang care=term bem=beam]
  ^-  (unit (unit cage))
  =/  =shop  &/p.bem
  =*  dap  q.bem
  =/  =coin  $/r.bem
  =*  path  s.bem
  ::
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
    =;  hav=?
      [~ ~ noun+!>(hav)]
    =/  yok=(unit yoke)  (~(get by yokes.state) dap)
    ?~(yok | -.agent.u.yok)
  ::
  ?:  ?&  =(%d care)
          =(~ path)
          =([%$ %da now] coin)
          =(our ship)
      ==
    =/  yok=(unit yoke)  (~(get by yokes.state) dap)
    ?~  yok
      [~ ~]
    [~ ~ desk+!>(q.beak.u.yok)]
  ::
  ?:  ?&  =(%e care)
          =(~ path)
          =([%$ %da now] coin)
          =(our ship)
      ==
    :+  ~  ~
    :-  %apps  !>  ^-  (set [=dude live=?])
    =*  syd=desk  dap
    %+  roll  ~(tap by yokes.state)
    |=  [[=dude =yoke] acc=(set [=dude live=?])]
    ?.  =(syd q.beak.yoke)
      acc
    (~(put in acc) [dude -.agent.yoke])
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
::  +stay: save without cache; suspend non-%base agents
::
::    TODO: superfluous? see +molt
::
++  stay
  ^-  spore
  =;  eggs=(map term egg)  state(yokes eggs)
  %-  ~(run by yokes.state)
  |=  =yoke
  ^-  egg
  %=    yoke
      agent
    ?:  ?=(%| -.agent.yoke)
      [%| p.agent.yoke]
    ?:  =(%base q.beak.yoke)
      [%& on-save:p.agent.yoke]
    [%| on-save:p.agent.yoke]
  ==
::  +take: response
::
++  take
  ~/  %gall-take
  |=  [=wire =duct dud=(unit goof) syn=sign-arvo]
  ^-  [(list move) _gall-payload]
  ?^  dud
    ~&(%gall-take-dud ((slog tang.u.dud) [~ gall-payload]))
  ?:  =(/nowhere wire)
    [~ gall-payload]
  ?:  =(/clear-huck wire)
    =/  =gift  ?>(?=([%behn %heck %gall *] syn) +>+.syn)
    [[duct %give gift]~ gall-payload]
  ::
  ~|  [%gall-take-failed wire]
  ?>  ?=([?(%sys %use) *] wire)
  =<  mo-abet
  %.  [t.wire ?:(?=([%behn %heck *] syn) syn.syn syn)]
  ?-  i.wire
    %sys  mo-handle-sys:(mo-abed:mo duct)
    %use  mo-handle-use:(mo-abed:mo duct)
  ==
--
