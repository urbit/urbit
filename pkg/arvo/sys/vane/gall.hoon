!:
::  ::  %gall, agent execution
!?  163
::
::::
|=  our=ship
::  veb: verbosity flags
::
=/  veb-all-off
  ::  TODO: add more flags?
  ::
  :*  odd=`?`%.n  ::  unusual events
  ==
=,  gall
=>
|%
+|  %helpers
::  +trace: print if .verb is set and we're tracking .dude
::
++  trace
  |=  [verb=? =dude dudes=(set dude) print=tang]
  ^+  same
  ?.  verb
    same
  ?.  =>  [dude=dude dudes=dudes in=in]
      ~+  |(=(~ dudes) (~(has in dudes) dude))
    same
  %+  slog
    :-  %leaf  %+  weld  "gall: "
    ?:  =(%$ dude)  ""
    "<dude>: "
  print
::
::  $bug: debug printing configuration
::
::    veb: verbosity toggles
::    dudes: app filter; if ~, print for all
::
+$  bug
  $:  veb=_veb-all-off
      dudes=(set dude)
  ==
::
+|  %main
::
::  $move: Arvo-level move
::
+$  move  [=duct move=(wind note-arvo gift-arvo)]
::  $state-18: overall gall state, versioned
::
+$  state-18  [%18 state]
::  $state: overall gall state
::
::    system-duct: TODO document
::    outstanding: outstanding request queue
::    contacts: other ships we're in communication with
::    yokes: running agents
::    blocked: moves to agents that haven't been started yet
::    bug: debug printing configuration
::    leaves: retry nacked %leaves timer, if set
::    flubs: list of flubed apps, per ship
::    halts: list of missing/suspended apps, per ship
::
+$  state
  $+  state
  $:  system-duct=duct
      outstanding=(map [wire duct] (qeu remote-request))
      contacts=(set ship)
      yokes=(map term yoke)
      blocked=(map term (qeu blocked-move))
      =bug
      leaves=(unit [=duct =wire date=@da])
      flub-ducts=(map ship duct)
      flubs=(jug ship app=term)
      halts=(jug app=term [ship =duct])
  ==
::  $routes: new cuff; TODO: document
::
+$  routes
  $:  disclosing=(unit (set ship))
      attributing=[=ship =path]
  ==
+$  brood  [=coop =hutch]
::  $yoke: agent runner state
::
::    control-duct: TODO document
::    run-nonce: unique for each rebuild
::    sub-nonce: app-wide global %watch nonce
::    stats: TODO document
::    bitt: incoming subscriptions
::    boat: outgoing subscriptions
::    boar: and their nonces
::    code: most recently loaded code
::    agent: agent core
::    beak: compilation source
::    marks: mark conversion requests
::    sky: scry bindings
::    ken: open keen requests
::
+$  yoke
  $%  [%nuke sky=(map spur @ud) cop=(map coop hutch)]
      $:  %live
          control-duct=duct
          run-nonce=@t
          sub-nonce=_1
          =stats
          =bitt
          =boat
          =boar
          code=*
          agent=(each agent vase)
          =beak
          marks=(map duct mark)
          sky=farm
          ken=(jug spar:ames wire)
          pen=(jug spar:ames wire)
          gem=(jug coop [path page])
  ==  ==
::
++  of-farm
  |_  =farm
  ++  key-coops
    |=  pos=path
    ^-  (list coop)
    =/  frm  (get-farm pos)
    ?~  frm  ~
    =.  farm  u.frm
    |-
    ?:  ?=(%coop -.farm)
      ~[pos]
    %-  zing
    %+  turn  ~(tap by q.farm)
    |=  [seg=@ta f=^farm]
    ^-  (list coop)
    ^$(pos (snoc pos seg), farm f)
  ::
  ++  match-coop
    =|  wer=path
    |=  =path
    ^-  (unit coop)
    ?:  ?=(%coop -.farm)
      `(flop wer)
    ?~  path
      ~
    ?~  nex=(~(get by q.farm) i.path)
      ~
    $(wer [i.path wer], path t.path, farm u.nex)
  ::
  ++  put
    |=  [=path =plot]
    ^-  _farm
    ?:  ?=(%coop -.farm)
      farm(q (~(put by q.farm) path plot))
    ?~  path
      farm(p `plot)
    =/  nex  (~(get by q.farm) i.path)
    =/  res  $(path t.path, farm ?~(nex *^farm u.nex))
    farm(q (~(put by q.farm) i.path res))
  ::
  ++  put-grow
    |=  [=path =plot]
    ^-  (unit _farm)
    ?:  ?=(%coop -.farm)
      ~
    ?~  path
      `farm(p `plot)
    =/  nex  (~(get by q.farm) i.path)
    =/  res
      $(path t.path, farm ?~(nex *^farm u.nex))
    ?~  res  ~
    `farm(q (~(put by q.farm) i.path u.res))
  ::
  ++  put-tend
    |=  [=path =plot]
    ^-  (unit _farm)
    ?:  ?=(%coop -.farm)
      `farm(q (~(put by q.farm) path plot))
    ?~  path
      `farm(p `plot)
    ?~  nex=(~(get by q.farm) i.path)
      ~
    =/  res
      $(path t.path, farm u.nex)
    ?~  res  ~
    `farm(q (~(put by q.farm) i.path u.res))
  ::
  ++  grow
    |=  [=spur now=@da =page]
    =/  ski  (gut spur)
    %+  put  spur
    =-  ski(fan (put:on-path fan.ski -< -> &/page))
    ?~  las=(ram:on-path fan.ski)
      [?~(bob.ski 1 +(u.bob.ski)) now]
    :_  (max now +(p.val.u.las))
    ?~(bob.ski +(key.u.las) +((max key.u.las u.bob.ski)))
  ::
  ++  germ
    |=  [=coop =hutch]
    ^-  (unit _farm)
    ?~  coop
      ?.  |(=(%coop -.farm) =([%page ~ ~] farm))
        ~
      `[%coop hutch ~]
    ?:  ?=(%coop -.farm)
      ~
    ?~  nex=(~(get by q.farm) i.coop)
      ~
    $(coop t.coop, farm u.nex)
  ::
  ++  tend
    |=  [=coop =path =plot]
    ^-  (unit _farm)
    ?~  coop
      ?.  ?=(%coop -.farm)
        ~
      `farm(q (~(put by q.farm) path plot))
    ?.  ?=(%plot -.farm)
      ~
    ?~  nex=(~(get by q.farm) i.coop)
      ~
    $(coop t.coop, farm u.nex)
  ::
  ++  del
    |=  =path
    ^+  farm
    ?:  ?=(%coop -.farm)
      farm(q (~(del by q.farm) path))
    ?~  path
      farm(p ~)
    ?~  nex=(~(get by q.farm) i.path)
      farm
    $(path t.path, farm u.nex)
  ::
  ++  gut
    |=  =path
    ^-  plot
    (fall (get path) *plot)
  ::
  ++  put-hutch
    |=  [=path =hutch]
    ^-  (unit _farm)
    ?~  path
      ?:  ?=(%coop -.farm)
        `farm(p hutch)
      ?.  =([%plot ~ ~] farm)
        ~
      `[%coop hutch ~]
    ?:  ?=(%coop -.farm)
      ~
    =/  nex  (~(gut by q.farm) i.path *^farm)
    =/  res  $(path t.path, farm nex)
    ?~  res  ~
    `farm(q (~(put by q.farm) i.path u.res))
  ::
  ++  get-hutch
    |=  =path
    ^-  (unit hutch)
    ?~  path
      ?.  ?=(%coop -.farm)
        ~
      `p.farm
    ?:  ?=(%coop -.farm)
      ~
    ?~  nex=(~(get by q.farm) i.path)
      ~
    $(path t.path, farm u.nex)
  ::
  ++  get-farm
    |=  =path
    ^-  (unit ^farm)
    ?:  ?=(%coop -.farm)
      ?~  (~(get by q.farm) path)
        ~
      `farm
    ?~  path  ~
    ?~  nex=(~(get by q.farm) i.path)
      ~
    $(path t.path, farm u.nex)
  ::
  ++  get
    |=  =path
    ^-  (unit plot)
    ?:  ?=(%coop -.farm)
      (~(get by q.farm) path)
    ?~  path
      p.farm
    ?~  nex=(~(get by q.farm) i.path)
      ~
    $(path t.path, farm u.nex)
  ::
  ++  tap-plot
    =|  wer=path
    |-  ^-  (list [path plot])
    =*  tap-plot  $
    ?:  ?=(%coop -.farm)
      %+  turn  ~(tap by q.farm)
      |=  [=path =plot]
      [(welp wer path) plot]
    %+  welp  ?~(p.farm ~ [wer u.p.farm]~)
    %-  zing
    %+  turn  ~(tap by q.farm)
    |=  [seg=@ta f=^farm]
    ^-  (list [path plot])
    tap-plot(wer (snoc wer seg), farm f)
  ::
  ++  run-plot
    |*  fun=gate
    %-  ~(gas by *(map path _(fun)))
    %+  turn  tap-plot
    |=  [=path =plot]
    [path (fun plot)]
  ::
  ++  gas-hutch
    |=  =(list [=coop =hutch])
    ^-  (unit _farm)
    ?~  list
      `farm
    =/  nex
      (put-hutch i.list)
    ?~  nex  ~
    $(farm u.nex, list t.list)
  ::
  ++  tap-hutch
    =|  wer=path
    %-  ~(gas in *(set [=coop =hutch]))
    |-  ^-  (list [=coop =hutch])
    =*  loop  $
    ?:  ?=(%coop -.farm)
      [wer p.farm]~
    %-  zing
    %+  turn  ~(tap by q.farm)
    |=  [seg=@ta f=^farm]
    ^-  (list [=coop =hutch])
    loop(wer (snoc wer seg), farm f)
  --
::
++  on-path  ((on @ud (pair @da (each page @uvI))) lte)
::  $blocked-move: enqueued move to an agent
::
+$  blocked-move  [=duct =routes move=(each deal unto)]
::
::  $fine-request: key exchange request for $coop
::
+$  fine-request
  [%0 =path]
::
::  $fine-response: key exchange response for $coop
::
+$  fine-response
  [%0 bod=(unit brood)]
::
+$  ames-response
  $%  [%d =mark noun=*]
      [%x ~]
  ==
::
+$  flub-requesst  [%0 ~]
::
+$  flub-response
  $:  %0
      ::  we could remove bone, and halt every outstanding flow but this could
      ::  mean that a flow that was acked right before the agent got suspended
      ::  but for which the ack packet arrived after the %boon %flub, won't be
      ::  handled and dropped in %ames
      ::
      $%  [%flub foreign-agent=term =bone:ames]
          [%spur foreign-agent=term]
  ==  ==
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
::  remember to duplicate version tag changes here to $egg-any:gall in lull
::
+$  spore
  $:  system-duct=duct
      outstanding=(map [wire duct] (qeu remote-request))
      contacts=(set ship)
      eggs=(map term egg)
      blocked=(map term (qeu blocked-move))
      =bug
      leaves=(unit [=duct =wire date=@da])
      flub-ducts=(map ship duct)
      flubs=(jug ship app=term)
      halts=(jug app=term [ship duct])
  ==
+$  spore-18  [%18 spore]
--
::  adult gall vane interface, for type compatibility with pupa
::
=|  state=state-18
|=  [now=@da eny=@uvJ rof=roof]
=*  gall-payload  .
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
  ::
  ++  trace
    |=  [verb=? =dude print=tang]
    ^+  same
    (^trace verb dude dudes.bug.state print)
  ::
  ::  +mo-abed: initialise state with the provided duct
  ::  +mo-abet: finalize, reversing moves
  ::  +mo-pass: prepend a standard %pass to the current list of moves
  ::  +mo-give: prepend a standard %give to the current list of moves
  ::  +mo-talk: build task to print config report or failure trace
  ::
  ++  mo-core  .
  ++  mo-abed  |=(hun=duct mo-core(hen hun))
  ++  mo-abet  [(flop moves) gall-payload]
  ++  mo-emit  |=(=move mo-core(moves [move moves]))
  ++  mo-give  |=(=gift (mo-emit hen give+gift))
  ++  mo-talk
    |=  rup=(each suss tang)
    ^-  [wire note-arvo]
    :+  /sys/say  %d
    ^-  task:dill
    ?-  -.rup
      %&  [%text "gall: {(t q)}ed %{(t p)}":[t=trip p.rup]]
      %|  [%talk leaf+"gall: failed" (flop p.rup)]
    ==
  ++  mo-pass  |=(p=[wire note-arvo] (mo-emit hen pass+p))
  ++  mo-slip  |=(p=note-arvo (mo-emit hen slip+p))
  ++  mo-past
    |=  =(list [wire note-arvo])
    ?~  list
      mo-core
    =.  mo-core  (mo-pass i.list)
    $(list t.list)
  ::  +mo-jolt: (re)start agent
  ::
  ++  mo-jolt
    |=  [dap=term =ship =desk]
    ^+  mo-core
    =/  =wire  /sys/cor/[dap]/(scot %p ship)/[desk]
    ..mo-core
    ::  XX  (mo-pass wire %c %jolt dap ship desk)
  ::  +mo-doff: kill all outgoing subscriptions
  ::
  ++  mo-doff
    |=  [prov=path dude=(unit dude) ship=(unit ship)]
    ^+  mo-core
    =/  apps=(list (pair term yoke))
      ?~  dude  ~(tap by yokes.state)
      (drop (bind (~(get by yokes.state) u.dude) (lead u.dude)))
    |-  ^+  mo-core
    ?~  apps  mo-core
    ?:  ?=(%nuke -.q.i.apps)  $(apps t.apps)
    =/  ap-core  (ap-yoke:ap p.i.apps [~ our prov] q.i.apps)
    $(apps t.apps, mo-core ap-abet:(ap-doff:ap-core ship))
  ::  +mo-rake: send %cork's for old subscriptions if needed
  ::
  ++  mo-rake
    |=  [prov=path dude=(unit dude) all=?]
    ^+  mo-core
    =/  apps=(list (pair term yoke))
      ?~  dude  ~(tap by yokes.state)
      (drop (bind (~(get by yokes.state) u.dude) (lead u.dude)))
    |-  ^+  mo-core
    ?~  apps  mo-core
    ?:  ?=(%nuke -.q.i.apps)  $(apps t.apps)
    =/  ap-core  (ap-yoke:ap p.i.apps [~ our prov] q.i.apps)
    $(apps t.apps, mo-core ap-abet:(ap-rake:ap-core all))
  ::  +mo-lave: delete all incoming stale subscriptions
  ::
  ++  mo-lave
    |=  [prov=path subs=(list [v=?(%g %a) =ship =dude =duct])]
    ^+  mo-core
    |-  ^+  mo-core
    ?~  subs  mo-core
    ?~  yoke=(~(get by yokes.state) dude.i.subs)
      $(subs t.subs)
    =?  mo-core  ?=(%live -.u.yoke)
      =/  ap-core  (ap-yoke:ap dude.i.subs [~ ship.i.subs prov] u.yoke)
      ap-abet:(ap-lave:ap-core [v duct]:i.subs)
    $(subs t.subs)
  ::  +mo-halt: give remote %flub to $plea sender
  ::
  ++  mo-halt
    |=  [prov=path =ship agent=term =bone:ames]
    ^+  mo-core
    ?~  duct=(~(get by flub-ducts.state) ship)
      mo-core  :: XX log
    ::  XX don't give bone? receiver can ignore it and halt all flows to agent
    ::
    (mo-emit u.duct %give %boon %0 %flub agent bone)
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
    |=  [prov=path dap=term bek=beak =agent]
    ^+  mo-core
    ::
    =/  yak  (~(get by yokes.state) dap)
    =/  tex=(unit tape)
      ?~  yak  `"installing"
      ?:  ?=(%nuke -.u.yak)  `"unnuking"  ::TODO good message here?
      ?-    -.agent.u.yak
          %|  `"reviving"
          %&
        ?:  =(code.u.yak agent)
          ~
        `"reloading"
      ==
    =+  ?~  tex  ~
        ~>  %slog.[0 leaf+"gall: {u.tex} {<dap>}"]  ~
    ::
    ?:  ?=([~ %live *] yak)
      ?:  &(=(q.beak.u.yak q.bek) =(code.u.yak agent) =(-.agent.u.yak &))
        mo-core
      ::
      =.  yokes.state
        (~(put by yokes.state) dap u.yak(beak bek, code agent))
      =/  ap-core  (ap-abed:ap dap [~ our prov])
      =.  ap-core  (ap-reinstall:ap-core agent)
      =.  mo-core  ap-abet:ap-core
      =.  mo-core  (mo-give-halts dap)
      (mo-clear-queue dap)
    ::

    =.  yokes.state
      %+  ~(put by yokes.state)  dap
      %*    .  *$>(%live yoke)
          control-duct  hen
          beak          bek
          code          agent
          agent         &+agent
          run-nonce     (scot %uw (end 5 (shas %yoke-nonce eny)))
      ::
          sky
        ?~  yak  *farm
        =|  =farm
        =.  farm  (need (~(gas-hutch of-farm farm) ~(tap by cop.u.yak)))
        =/  sky=(list [=spur bob=@ud])  ~(tap by sky.u.yak)
        |-
        ?~  sky  farm
        =.  farm  (need (~(put-grow of-farm farm) spur.i.sky [`bob.i.sky ~]))
        $(sky t.sky)
      ==
    ::
    =/  old  mo-core
    =/  wag
      =/  ap-core  (ap-abed:ap dap [~ our prov])
      (ap-upgrade-state:ap-core ~)
    ::
    =/  maybe-tang  -.wag
    =/  ap-core  +.wag
    ?^  maybe-tang
      =.  mo-core  old
      (mo-pass (mo-talk %.n u.maybe-tang))
    ::
    =.  mo-core  ap-abet:ap-core
    =.  mo-core  (mo-clear-queue dap)
    =.  mo-core  (mo-give-halts dap)
    =/  =suss  [dap %boot now]
    (mo-pass (mo-talk %.y suss))
  ::  +mo-send-foreign-request: handle local request to .ship
  ::
  ++  mo-send-foreign-request
    ~/  %mo-send-foreign-request
    |=  [=ship foreign-agent=term =deal]
    ^+  mo-core
    ::
    =.  mo-core  (mo-track-ship ship)
    =.  mo-core  (mo-track-flubs ship)
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
    ::  ask jael to track .ship's breaches
    ::
    =/  =note-arvo  [%j %public-keys (silt ship ~)]
    mo-core(moves [[system-duct.state %pass /sys/era note-arvo] moves])
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
    =/  =note-arvo  [%j %nuke (silt ship ~)]
    mo-core(moves [[system-duct.state %pass /sys/era note-arvo] moves])
  ::  +mo-breach: ship breached, so forget about them
  ::
  ++  mo-breach
    |=  [prov=path =ship]
    ^+  mo-core
    =.  mo-core  (mo-untrack-ship ship)
    =.  mo-core  (mo-untrack-flub ship)
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
    =?  mo-core  ?=(%live -.yoke.i.agents)
      =/  =routes  [disclosing=~ attributing=[ship prov]]
      =/  app  (ap-abed:ap name.i.agents routes)
      ap-abet:(ap-breach:app ship)
    $(agents t.agents)
  ::  +mo-track-flubs: open system flow to handle %boon %flubs
  ::
  ++  mo-track-flubs
    |=  =ship
    ^+  mo-core
    ::  if already sent, no-op
    ::
    ?:  (~(has by flubs.state) ship)
      mo-core
    ::  first contact; update state and subscribe to /flub notifications
    ::
    =.  flubs.state  (~(put by flubs.state) ship ~)
    ::  ask foreign %gall to notify us about %flubs
    ::
    =/  =note-arvo  [%a %plea ship %g /gf %0 ~]  :: XX add info to payload?
    =.  moves
      [[system-duct.state %pass /sys/flu/(scot %p ship) note-arvo] moves]
    mo-core
  ::
  ++  mo-untrack-flub
    |=  =ship
    ^+  mo-core
    ::  if already canceled, no-op
    ::
    ?.  (~(has by flubs.state) ship)
      mo-core
    ::  delete .ship from state
    ::
    mo-core(flubs.state (~(del by flubs.state) ship))
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
      %lyv  ..mo-core  ::  vestigial
      %cor  ..mo-core  ::  vestigial
      %era  (mo-handle-sys-era wire sign-arvo)
      %req  (mo-handle-sys-req wire sign-arvo)
      %way  (mo-handle-sys-way wire sign-arvo)
      %flu  (mo-handle-sys-flu wire sign-arvo)
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
    (mo-breach /jael who.public-keys-result.sign-arvo)
  ::  +mo-handle-sys-clog: handle an ames %clog notification
  ::
  ++  mo-handle-sys-clog
    |=  [=duct agent-name=term]
    ^+  mo-core
    =/  yoke  (~(get by yokes.state) agent-name)
    ?~  yoke
      mo-core
    =?  mo-core  ?=(%live -.u.yoke)
      =/  app  (ap-abed:ap agent-name [~ our /ames])
      ap-abet:(ap-clog:app duct)
    ::
    mo-core
  ::
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
      (mo-give %noon [dap [/gall/sys/req/[i.t.wire]/[dap] hen]] %d mark noun)
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
      ::
        [%ames %done *]
      =/  err=(unit tang)
        ?~  error=error.sign-arvo
          ~
        `[[%leaf (trip tag.u.error)] tang.u.error]
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
          :-  rr
          ?.  =(~ stand)
            (~(put by outstanding.state) [full-wire hen] stand)
          ::  outstanding leaves are only deleted when acked;
          ::  a nacked %leave is flagged with a %missing request
          ::  we add to the outstanding queue that is only checked
          ::  in the nacked-leaves timer to skip dead-flow %leave(s)
          ::
          ?.  &(?=(^ err) ?=(%leave rr))
            (~(del by outstanding.state) [full-wire hen])
          %-  ~(put by outstanding.state)
          [[full-wire hen] (~(gas to stand) ~[%leave %missing])]
        ::  non-null case of wire is old, remove on next breach after
        ::  2019/12
        ::
        [;;(remote-request i.t.t.t.wire) outstanding.state]
      ::  send a %cork if we get a %nack upon initial subscription
      ::
      =?  mo-core
          &(?=(^ err) |(?=(%watch-as remote-request) ?=(%watch remote-request)))
          ::  XX TODO maybe also send a %cork if we get a %nack for a %poke?
          ::  &(?=(^ err) ?+(remote-request %.y %leave %.n, %missing %.n))
          ::
        (mo-pass sys+wire %a %cork ship)
      ::
      ?-    remote-request
          %poke                (mo-give %unto %poke-ack err)
          %missing             ~>(%slog.[3 'gall: missing'] mo-core)
          ?(%watch %watch-as)  (mo-give %unto %watch-ack err)
      ::
          %leave
        ::  if we get an %ack for a %leave, send %cork. otherwise,
        ::  the /nacked-leaves timer will re-send the %leave eventually.
        ::
        ?~  err
          (mo-pass sys+wire %a %cork ship)
        %-  %:  ^trace  odd.veb.bug.state  *dude  ~
              leaf/"{<ship>} got %nacked %leave {<(spud wire)>}"  ~
            ==
        ::  if first time hearing a %nack for a %leave, after upgrade
        ::  or if all outstanding %leaves have been handled, set up timer
        ::
        =?  mo-core  ?=(~ leaves.state)
          (mo-emit [/gall]~ %pass /nacked-leaves %b %wait `@da`(add now ~m2))
        =?  leaves.state  ?=(~ leaves.state)
          `[[/gall]~ /nacked-leaves `@da`(add now ~m2)]
        mo-core
      ==
    ::
        [%ames %boon *]
      ?^  t.t.t.wire
        ::  kill subscriptions which use the old wire format
        ::
        !!
      =/  key  [[%sys wire] hen]
      =+  outs=(~(gut by outstanding.state) key ~)
      =/  =ames-response  ;;(ames-response payload.sign-arvo)
      ::  if there are outstanding $pleas and we get a %fact, assume that the %ack
      ::  will be resend later on, and give the %watch-ack now.
      ::
      ::  when the actual ack for the $plea arrives, we won't deliver this second
      ::  %ack to the agent and just no-op.
      ::
      ::  if the outstanding $plea is a %leave, the subcription should have been
      ::  deleted from boat.yoke, and we would also no-op
      ::
      ::    (see +run-sign:ap-specific-take)
      ::
      =?  mo-core  ?=(^ outs)  (mo-give %unto %watch-ack ~)
      ::  %d: diff; ask clay to validate .noun as .mark
      ::  %x: kick; tell agent the publisher canceled the subscription and
      ::      cork; tell ames to close the associated flow.
      ::
      ?-  -.ames-response
          %d  (mo-give %unto %raw-fact mark.ames-response noun.ames-response)
          %x
        =.  mo-core  (mo-give %unto %kick ~)
        =?  outstanding.state  =(~ outs)
          ::  if there is an outstanding $plea don't delete it from
          ::  .outstanding to avoid a %gall-missing print
          ::
          (~(del by outstanding.state) key)
        (mo-pass sys+wire a/cork+ship)
      ==
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
  ::  +mo-handle-sys-flu
  ::
  ++  mo-handle-sys-flu
    |=  [=wire =sign-arvo]
    ^+  mo-core
    ?>  ?=([%flu @ ~] wire)
    ?~  ship=(slaw %p +<.wire)
      mo-core
    ?+    sign-arvo  !!
        [%ames %done *]
      ?~  error.sign-arvo
        mo-core
      ::  if error, delete the ship from .flubs; on next contact we will retry
      ::
      =.  flubs.state  (~(del by flubs.state) u.ship)
      mo-core
      ::
        [%ames %boon *]
      ~|  payload.sign-arvo
      =+  ;;  response=flub-response  payload.sign-arvo
      ?>  ?=(%0 -.response)
      ?-    +<.response
          %flub
        ::  XX ignore bone? find all outstanding request to .foreign-agent?
        ::
        ::  add agent to list of suspended/not running agents
        ::
        =.  flubs.state  (~(put ju flubs.state) u.ship foreign-agent.response)
        (mo-pass /remote-flub %a %halt u.ship [foreign-agent bone]:response)
      ::
          %spur
        =.  flubs.state
          =-  ::  if we have deleted all flubbed apps, re-add the ship
              ::  with an empty list of apps to not resend the /flub $plea
              ::
              (~(put by -) u.ship (~(gut by -) u.ship ~))
          (~(del ju flubs.state) u.ship foreign-agent.response)
        %-  ~(rep by outstanding.state)
        |=  [[[=^wire =duct] queue=*] m=_mo-core]
        ?.  =(/sys/way/(scot %p u.ship)/[foreign-agent.response] wire)
          m
        (mo-pass:m(hen duct) wire %a %goad u.ship)
      ==
    ==
  ::
  ++  mo-handle-key
    ~/  %mo-handle-stub
    |=  [=(pole knot) syn=sign-arvo]
    ?.  ?=([agent=@ nonce=@ rest=*] pole)
      ~&  [%mo-handle-key-bad-wire wire]
      !!
    =*  dap   agent.pole
    =/  yoke  (~(get by yokes.state) agent.pole)
    ?.  ?=([~ %live *] yoke)
      %-  (slog leaf+"gall: {<`@t`dap>} dead, got %stub" ~)
      mo-core
    ?.  =(run-nonce.u.yoke nonce.pole)
      %-  (slog leaf+"gall: got old stub for {<dap>}" ~)
      mo-core
    =/  =routes  [disclosing=~ attributing=[our /]]
    =/  ap-core  (ap-abed:ap agent.pole routes)
    ?+    rest.pole  ~|(mo-handle-key-bad-wire/wire !!)
        [%pug rest=*]
      ?>  ?=([%ames %stub *] syn)
      ap-abet:(ap-stub:ap-core rest.rest.pole [num key]:syn)
    ::
        [%bod rest=*]
      ap-abet:(ap-take-brood:ap-core rest.rest.pole syn)
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
    ?.  ?=([~ %live *] yoke)
      %-  (slog leaf+"gall: {<dap>} dead, got {<+<.sign-arvo>}" ~)
      mo-core
    ?.  =(run-nonce.u.yoke i.t.wire)
      %-  (slog leaf+"gall: got old {<+<.sign-arvo>} for {<dap>}" ~)
      mo-core
    ::
    ?.  ?=([?(%gall %behn) %unto *] sign-arvo)
      ?:  ?=(%| -.agent.u.yoke)
        %-  (slog leaf+"gall: {<dap>} dozing, dropping {<+<.sign-arvo>}" ~)
        mo-core
      =/  app
        =/  =ship  (slav %p i.t.t.wire)
        =/  =routes  [disclosing=~ attributing=[ship /[-.sign-arvo]]]
        (ap-abed:ap dap routes)
      ::
      =.  app  (ap-generic-take:app t.t.t.wire sign-arvo)
      ap-abet:app
    ?>  ?=([%out @ @ *] t.t.wire)
    =/  =ship  (slav %p i.t.t.t.wire)
    =/  other-agent  i.t.t.t.t.wire
    =/  prov=path  ?.(=(ship our) *path /gall/[other-agent])
    =/  =routes  [disclosing=~ attributing=[ship prov]]
    =/  =unto  +>.sign-arvo
    ?:  ?=(%| -.agent.u.yoke)
      =/  blocked=(qeu blocked-move)
        =/  waiting  (~(get by blocked.state) dap)
        =/  deals  (fall waiting *(qeu blocked-move))
        =/  deal  [[[%gall %use wire] hen] routes |+unto]
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
    ::
    =/  =move
      =/  =sack  [ship.attributing.routes our path.attributing.routes]
      ?:  ?=(%& -.blocker)
        ?>  ?=(^ duct)
        [+.duct %slip %g %deal sack dap p.blocker]
      [duct %give %unto p.blocker]
    $(moves [move moves])
  ::
  ++  mo-give-halts
    |=  dap=term
    ^+  mo-core
    ?~  yok=(~(get by yokes.state) dap)
      mo-core
    ?>  ?=([~ %live *] yok)
    ?~  halts=(~(get by halts.state) dap)
      mo-core
    %-  ~(rep in u.halts)
    |=  [[=ship =duct] m=_mo-core]
    =.  halts.state.m  (~(del ju halts.state.m) dap ship duct)
    =.  m  (mo-give:m(hen duct) %spur ~)  ::  un-halt flow
    ~|  mo-give-halts/ship^dap
    (mo-emit:m (~(got by flub-ducts.state) ship) %give %boon %0 %spur dap)
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
    =?  new-blocked  !=(ship ship.attributing.routes.mov)
      (~(put to new-blocked) mov)
    $
  ::  +mo-idle: put agent to sleep
  ::
  ++  mo-idle
    |=  [prov=path dap=dude]
    ^+  mo-core
    =/  yoke=(unit yoke)  (~(get by yokes.state) dap)
    ?:  |(?=(~ yoke) ?=(%nuke -.u.yoke))
      ~>  %slog.0^leaf/"gall: ignoring %idle for {<dap>}, not running"
      mo-core
    ap-abet:ap-idle:(ap-abed:ap dap [~ our prov])
  ::  +mo-nuke: delete agent completely
  ::
  ++  mo-nuke
    |=  [prov=path dap=dude]
    ^+  mo-core
    =?  mo-core  (~(has by blocked.state) dap)
      =/  num-movs=@  ~(wyt by (~(got by blocked.state) dap))
      ~>  %slog.0^leaf/"gall: deleted {<num-movs>} blocked moves for {<dap>}"
      mo-core(blocked.state (~(del by blocked.state) dap))
    =/  yoke=(unit yoke)  (~(get by yokes.state) dap)
    ?:  |(?=(~ yoke) ?=(%nuke -.u.yoke))
      ~>  %slog.0^leaf/"gall: ignoring %nuke for {<dap>}, not running"
      mo-core
    ~>  %slog.0^leaf/"gall: nuking {<dap>}"
    =.  mo-core  ap-abet:ap-nuke:(ap-abed:ap dap [~ our prov])
    =-  mo-core(yokes.state -)
    %+  ~(jab by yokes.state)  dap
    |=  =^yoke
    ?:  ?=(%nuke -.yoke)  yoke
    :+  %nuke
      %-  ~(run-plot of-farm sky.yoke)
      |=  plot
      (fall (clap bob (bind (ram:on-path fan) head) max) 0)
    ~(tap-hutch of-farm sky.yoke)
  ::  +mo-load: install agents
  ::
  ++  mo-load
    |=  [prov=path agents=(list [=dude =beak =agent])]
    =.  mo-core
      |-  ^+  mo-core
      ?~  agents  mo-core
      =/  [=dude =desk]  [dude q.beak]:i.agents
      ::  ~>  %slog.0^leaf/"gall: starting {<dude>} on {<desk>}"
      $(agents t.agents, mo-core (mo-receive-core prov i.agents))
    ::
    =/  kil
      =/  lol
        (skim ~(tap by yokes.state) |=([* y=yoke] &(?=(%live -.y) -.agent.y)))
      =/  mol  (~(gas by *(map term yoke)) lol)
      =/  sol  ~(key by mol)
      =/  new  (silt (turn agents head))
      ~(tap in (~(dif in sol) new))
    |-  ^+  mo-core
    ?~  kil  mo-core
    ~>  %slog.0^leaf/"gall: stopping {<i.kil>}"
    $(kil t.kil, mo-core (mo-idle prov i.kil))
  ::
  ++  mo-authorized-coop
    |=  [lyc=(set ship) =farm dap=term =path =coop]
    %-  ~(all in lyc)
    |=  =ship
    =/  cag  (mo-peek | dap [~ ship path] %c (snoc coop (scot %p ship)))
    ?.  ?=([~ ~ ^] cag)
      %.n
    ?~  res=((soft ,?) q.q.u.u.cag)
      %.n
    u.res
  ::
  ++  mo-authorized
    |=  [lyc=gang =farm dap=term =path]
    ^-  ?
    ?:  =([~ ~] lyc)
      %.y
    ?~  (~(get-hutch of-farm farm) path)
      %.y
    ?:  ?=(~ lyc)
      %.n
    ?~  coop=(~(match-coop of-farm farm) path)
      %.n
    (mo-authorized-coop u.lyc farm dap path u.coop)
  ::  +mo-peek:  call to +ap-peek (which is not accessible outside of +mo).
  ::
  ++  mo-peek
    ~/  %mo-peek
    |=  [veb=? dap=term =routes care=term =path]
    ^-  (unit (unit cage))
    ::
    ?.  ?=([~ %live *] (~(get by yokes.state) dap))  [~ ~]
    =/  app  (ap-abed:ap dap routes)
    (ap-peek:app veb care path)
  ::
  ++  mo-apply
    |=  [dap=term =routes =deal]
    ^+  mo-core
    ?-    -.deal
        ?(%watch %watch-as %leave %poke)
      (mo-apply-sure dap routes deal)
    ::
        %raw-poke
      ::  don't validate %noun pokes, for performance
      ::
      ?:  =(%noun mark.deal)
        (mo-apply-sure dap routes [%poke %noun %noun noun.deal])
      =/  =case  da+now
      =/  yok  (~(got by yokes.state) dap)
      =/  =desk  q.beak:?>(?=(%live -.yok) yok)  ::TODO acceptable assertion?
      =/  sky  (rof [~ ~] /gall %cb [our desk case] /[mark.deal])
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
      =/  =case       da+now
      =/  =mars:clay  [p.cage mark]:deal
      =/  mars-path   /[a.mars]/[b.mars]
      =/  yok  (~(got by yokes.state) dap)
      =/  =desk  q.beak:?>(?=(%live -.yok) yok)  ::TODO acceptable assertion?
      =/  sky  (rof [~ ~] /gall %cc [our desk case] mars-path)
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
    |=  [prov=path =ship agent=term =deal]
    ^+  mo-core
    ::
    =/  =routes  [disclosing=~ attributing=[ship prov]]
    =/  running  (~(get by yokes.state) agent)
    =/  is-running  &(?=([~ %live *] running) ?=(%& -.agent.u.running))
    =/  is-blocked  (~(has by blocked.state) agent)
    ::  agent is running; deliver move normally
    ::
    ?.  |(!is-running is-blocked)
      (mo-apply agent routes deal)
    ::
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
  ::  +mo-handle-key-request: handle request for keys
  ::
  ++  mo-handle-key-request
    |=  [=ship agent-name=term =path]
    ^+  mo-core
    =/  yok=(unit yoke)  (~(get by yokes.state) agent-name)
    ?.  ?=([~ %live *] yok)
      (mo-give %done ~)
    =/  ap-core  (ap-abed:ap agent-name [~ our /gall])
    =^  bod=(each (unit brood) tang)  mo-core
      (ap-serve-brood:ap-core ship path)
    ?:  ?=(%| -.bod)
      (mo-give %done `keys/p.bod)
    =/  =fine-response  [%0 p.bod]
    =.  mo-core  (mo-give %boon fine-response)
    (mo-give %done ~)
  ::  +mo-handle-ames-request: handle %ames request message.
  ::
  ++  mo-handle-ames-request
    |=  [=ship agent-name=term =ames-request]
    ^+  mo-core
    ::
    =.  mo-core  (mo-track-ship ship)
    ::
    =/  yok=(unit yoke)  (~(get by yokes.state) agent-name)
    ?:  ?|   ?=(~ yok)
             ?=(%nuke -.u.yok)
             ?=(%.n -.agent.u.yok)
        ==
      ::  %ames wil pass a $deep task to itself to halt the flow, at the same
      ::  time, on the /flub flow, we send a %boon with the bone that the sender
      ::  needs to halt as well to stop sending any outstanding $pleas
      ::
      ::  XX if %leave, cork the flow; otherwise, halt it?
      ::  currently we always halt it
      ::
      =?  halts.state  (~(has by flub-ducts.state) ship)
        ::  only add the app if we have received the /gf $plea
        ::
        (~(put ju halts.state) agent-name ship hen)
      %+  mo-give  %flub
      ::  if we are waiting to hear the /gf $plea, only %flub the flow in %ames
      ::  and skip sending the %flub $boon
      ::
      ?.  (~(has by flub-ducts.state) ship)
        ~
      [~ agent-name]
    ::  %u/%leave gets automatically acked
    ::
    =?  mo-core  ?=(%u -.ames-request)
      (mo-give %done ~)
    =/  =wire  /sys/req/(scot %p ship)/[agent-name]
    ::
    =/  =deal
      ?-  -.ames-request
        %m  [%raw-poke [mark noun]:ames-request]
        %l  [%watch-as [mark path]:ames-request]
        %s  [%watch path.ames-request]
        %u  [%leave ~]
      ==
    (mo-pass wire %g %deal [ship our /] agent-name deal)
  ::
  ++  mo-handle-flub-plea
    |=  =ship
    =.  flub-ducts.state  (~(put by flub-ducts.state) ship hen)
    (mo-give %done error=~)
  ::  +mo-spew: handle request to set verbosity toggles on debug output
  ::
  ++  mo-spew
    |=  verbs=(list verb)
    ^+  mo-core
    ::  start from all %.n's, then flip requested toggles
    ::
    =.  veb.bug.state
      %+  roll  verbs
      |=  [=verb acc=_veb-all-off]
      ^+  veb.bug.state
      ?-  verb
        %odd  acc(odd %.y)
      ==
    mo-core
  ::  +mo-sift: handle request to filter debug output by agent
  ::
  ++  mo-sift
    |=  dudes=(list dude)
    ^+  mo-core
    =.  dudes.bug.state  (sy dudes)
    mo-core
  ::
  ++  mo-handle-nacked-leaves
    |=  =wire
    ^+  mo-core
    ?>  ?=([%sys %way @ @ ~] wire)
    (mo-pass wire %a %plea (slav %p &3.wire) %g /ge/[&4.wire] %0 %u ~)
  ::
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
            =$>(%live yoke)
        ==
    ::
    ++  trace
      |=  [verb=? print=tang]
      ^+  same
      (^trace verb agent-name print)
    ::
    ++  ap-nonce-wire
      |=  [=wire =dock]
      ^+  wire
      =/  nonce=@  (~(got by boar.yoke) wire dock)
      ?:  =(0 nonce)  wire
      [(scot %ud nonce) wire]
    ::
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
      %^  ap-yoke  dap  routes
      =<  ?>(?=(%live -) .)
      (~(got by yokes.state) dap)
    ::  +ap-yoke: initialize agent state, starting from a $yoke
    ::
    ++  ap-yoke
      |=  [dap=term =routes yak=$>(%live ^yoke)]
      ^+  ap-core
      =.  stats.yak
        :+  +(change.stats.yak)
          (shaz (mix (add dap change.stats.yak) eny))  ::  TODO: so bad, use +og
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
        =/  talker  |=(report=(each suss tang) [hen %pass (mo-talk report)])
        =/  from-suss  (turn agent-config talker)
        :(weld agent-moves from-suss moves)
      ::
      %_  mo-core
        yokes.state  running
        moves        moves
      ==
    ++  ap-request-brood
      |=  [=wire =ship =(pole knot)]
      ^+  ap-core
      ?.  ?=([%g %x cas=@ app=@ rest=*] pole)
        %.  ap-core
        %+  trace  odd.veb.bug.state
        [leaf+"brood request {<pole>} invalid, dropping"]~
      =.  pen.yoke  (~(put ju pen.yoke) [ship pole] wire)
      =/  =fine-request  [%0 rest.pole]
      =/  =plea:ames     [%g /gk/[app.pole] fine-request]
      =/  out=^wire
        (welp /key/[agent-name]/[run-nonce.yoke]/bod/(scot %p ship) pole)
      (ap-move [hen %pass out %a %plea ship plea]~)
    ::
    ++  ap-take-brood
      |=  [=wire syn=sign-arvo]
      ^+  ap-core
      ~|  ap-take-brood/wire
      ?>  ?=([@ *] wire)  :: TODO: strip crash semantics
      =/  =ship  (slav %p i.wire)
      =/  wis=(list ^wire)  ~(tap in (~(get ju pen.yoke) [ship t.wire]))
      ?+    syn  ~|(weird-sign-ap-take-brood/-.syn !!)
          [%ames %boon *]
        =/  bud  (fall ((soft fine-response) payload.syn) *fine-response)
        |-
        ?~  wis
          =.  pen.yoke  (~(del by pen.yoke) [ship t.wire])
          ap-core
        ?~  bod.bud
          =.  ap-core  (ap-generic-take i.wis %ames %sage [ship t.wire] ~)
          $(wis t.wis)
        =.  ap-core
          (ap-pass i.wis %arvo %a %keen `[idx key]:hutch.u.bod.bud ship t.wire)
        $(wis t.wis)
      ::
          [%ames %done *]
        ?~  error.syn
          ap-core
        |-
        ?~  wis
          =.  pen.yoke  (~(del by pen.yoke) [ship t.wire])
          ap-core
        =.  ap-core
          %.  (ap-generic-take i.wis %ames %sage [ship t.wire] ~)
          %+  trace  odd.veb.bug.state
          [leaf/"bad brood res {<ship>} {<t.wire>}"]~
        $(wis t.wis)
      ==
    ::
    ++  ap-serve-brood
      |=  [=ship =(pole knot)]
      ^-  [(each (unit brood) tang) _mo-core]
      ?.  ?=([%$ ver=@ rest=*] pole)
        :_  ap-abet
        |+[leaf/"gall: {<agent-name>} bad brood req {<ship>} {<pole>}"]~
      =/  ver  (slav %ud ver.pole)
      ?.  =(1 ver)
        :_  ap-abet
        |+[leaf/"gall: {<agent-name>} bad brood ver {<ver>} {<ship>} {<rest.pole>}"]~
      ?~  cop=(ap-match-coop rest.pole)
        %.  [&+~ ap-abet]
        %+  trace  odd.veb.bug.state
        [leaf/"gall: {<agent-name>} no coop match {<ship>} {<rest.pole>}"]~
      =/  cag=(unit (unit cage))
        (ap-peek %| %c (snoc u.cop (scot %p ship)))
      =/  has-perms=?
        ?.  ?=([~ ~ ^] cag)
          |
        ?~  res=((soft ,?) q.q.u.u.cag)
          |
        u.res
      =/  =hutch  (need (~(get-hutch of-farm sky.yoke) u.cop))
      ?.  has-perms
        %.  [[%.y ~] ap-abet]
        %+  trace  odd.veb.bug.state
        [leaf/"gall: {<agent-name>} no perms for {<coop>} {<ship>} {<rest.pole>}"]~
      =/  =brood  [u.cop hutch]
      [[%.y `brood] ap-abet]
    ::
    ++  ap-yawn-all
      ^-  (list card:agent)
      %-  zing
      %+  turn  ~(tap by ken.yoke)
      |=  [=spar:ames wyz=(set wire)]
      %+  turn  ~(tap in wyz)
      |=  =wire
      [%pass wire %arvo %a %yawn spar]
    ::
    ++  ap-idle
      ^+  ap-core
      ?:  ?=(%| -.agent.yoke)  ap-core
      ~>  %spin.[(crip "on-save/{<agent-name>}")]
      =>  [ken=ken.yoke (ap-ingest ~ |.([ap-yawn-all p.agent.yoke]))]
      ap-core(ken.yoke ken, agent.yoke |+on-save:ap-agent-core)
    ::
    ++  ap-nuke
      ^+  ap-core
      =/  inbound-paths=(set path)
        %-  silt
        %+  turn  ~(tap by bitt.yoke)
        |=  [=duct =ship =path]
        path
      =/  will=(list card:agent)
        ;:  welp
          ?:  =(~ inbound-paths)
            ~
          [%give %kick ~(tap in inbound-paths) ~]~
        ::
          %+  turn  ~(tap by boat.yoke)
          |=  [[=wire =dock] ? =path]
          [%pass wire %agent dock %leave ~]
        ::
          ap-yawn-all
        ==
      =^  maybe-tang  ap-core  (ap-ingest ~ |.([will *agent]))
      ap-core
    ::
    ++  ap-match-coop
      |=  =path
      ^-  (unit coop)
      (~(match-coop of-farm sky.yoke) path)
    ::
    ++  ap-keen
      |=  [=wire secret=? =spar:ames]
      ^+  ap-core
      ?:  secret
        (ap-request-brood wire spar)
      =.  ken.yoke  (~(put ju ken.yoke) spar wire)
      (ap-pass wire %arvo %a %keen ~ spar)
    ::  +ap-tend: bind path in namespace, encrypted
    ::
    ++  ap-tend
      |=  [=coop =path =page]
      ?~  cop=(~(get-hutch of-farm sky.yoke) coop)
        ?.  (~(has by gem.yoke) coop)
          %.  ap-core
          %+  trace  &
          [leaf+"no such coop {<coop>}, dropping %tend at {<path>}"]~
        =.  gem.yoke  (~(put ju gem.yoke) coop path page)
        ap-core
      =.  sky.yoke  (~(grow of-farm sky.yoke) (welp coop path) now page)
      ap-core
    ::
    ++  ap-germ
      |=  =coop
      =/  pen  (~(get by gem.yoke) coop)
      =/  exists  !=(~ (~(get of-farm sky.yoke) coop))
      =?  gem.yoke  &(!exists ?=(~ pen))
        (~(put by gem.yoke) coop ~)
      =/  =wire  (welp /key/[agent-name]/[run-nonce.yoke]/pug coop)
      ::  XX %plug reserves keys in %ames using (shaz eny) of length 64
      :: use %gulp that reserves keys of length 32?
      ::
      (ap-move [hen %pass wire %a %plug [%g %x agent-name %$ '1' coop]]~)
    ::
    ++  ap-stub
      |=  [=coop num=@ud key=@]
      ^+  ap-core
      =/  =hutch
        ?^  h=(~(get-hutch of-farm sky.yoke) coop)
          u.h
        *hutch
      =.  hutch  [.+(rev.hutch) num key]
      =.  sky.yoke
        ?^  new-sky=(~(put-hutch of-farm sky.yoke) coop hutch)
          u.new-sky
        sky.yoke
      =/  gem  ~(tap in (~(get ju gem.yoke) coop))
      |-  ^+  ap-core
      ?~  gem  ap-core
      $(gem t.gem, ap-core (ap-tend coop i.gem))
    ::
    ++  ap-snip
      |=  =coop
      ap-core
      :: ap-core(cop.yoke (~(del by cop.yoke) coop)) :: TODO: fix
    ::  +ap-grow: bind a path in the agent's scry namespace
    ::
    ++  ap-grow
      |=  [=spur =page]
      ^+  ap-core
      :: check here, and no-op, so that +need below does not crash
      ?:  ?=(^ (ap-match-coop spur))
        %.  ap-core
        %+  trace  &
        [leaf+"grow {<spur>} has coop, dropping"]~
      =-  ap-core(sky.yoke -)
      (~(grow of-farm sky.yoke) spur now page)
    ::  +ap-tomb: tombstone -- replace bound value with hash
    ::
    ++  ap-tomb
      |=  [=case =spur]
      ^+  ap-core
      =-  ap-core(sky.yoke -)
      =/  yon  ?>(?=(%ud -.case) p.case)
      =/  old  (~(get of-farm sky.yoke) spur)
      ?~  old  ::  no-op if nonexistent
        %.  sky.yoke
        %+  trace  odd.veb.bug.state
        [leaf+"tomb {<[case spur]>} no sky"]~
      =/  val  (get:on-path fan.u.old yon)
      ?~  val  ::  no-op if nonexistent
        %.  sky.yoke
        %+  trace  odd.veb.bug.state
        [leaf+"tomb {<[case spur]>} no val"]~
      ?-    -.q.u.val
          %|  ::  already tombstoned, no-op
        %.  sky.yoke
        %+  trace  odd.veb.bug.state
        [leaf+"tomb {<[case spur]>} no-op"]~
      ::
          %&  ::  replace with hash
        %+  ~(put of-farm sky.yoke)  spur
        u.old(fan (put:on-path fan.u.old yon u.val(q |/(shax (jam p.q.u.val)))))
      ==
    ::  +ap-cull: delete all bindings up to and including .case
    ::
    ::    Also store .case as the high water mark for .spur
    ::    to prevent any deleted cases from being re-bound later.
    ::
    ++  ap-cull
      |=  [=case =spur]
      ^+  ap-core
      =-  ap-core(sky.yoke -)
      =/  yon  ?>(?=(%ud -.case) p.case)
      =/  old  (~(get of-farm sky.yoke) spur)
      ?~  old  ::  no-op if nonexistent
        %.  sky.yoke
        %+  trace  odd.veb.bug.state
        [leaf+"cull {<[case spur]>} no-op"]~
      ?~  las=(ram:on-path fan.u.old)
        %.  sky.yoke
        %+  trace  &
        [leaf+"cull {<[case spur]>} no paths"]~
      =/  fis  (need (pry:on-path fan.u.old))
      ?.  &((gte yon key.fis) (lte yon key.u.las))
        %.  sky.yoke
        %+  trace  &
        :_  ~
        :-  %leaf
        %+  weld
          "cull {<[case spur]>} out of range, "
        "min: {<key.fis>}, max: {<key.u.las>}"
      %+  ~(put of-farm sky.yoke)  spur  ::  delete all older paths
      [`yon (lot:on-path fan.u.old `yon ~)]
    ::  +ap-from-internal: internal move to move.
    ::
    ::    We convert from cards to duct-indexed moves when resolving
    ::    them in Arvo.
    ::
    ::    We accept %huck to "fake" being a message to a ship but
    ::    actually send it to a vane.
    ::
    +$  carp  $+  carp  (wind neet gift:agent)
    +$  neet  $+  neet
      $<  ?(%grow %tomb %cull %tend %germ %snip %keen)
      $%  note:agent
          [%agent [=ship name=term] task=[%raw-poke =mark =noun]]
          [%huck [=ship name=term] =note-arvo]
      ==
    ::
    ++  ap-from-internal
      ~/  %ap-from-internal
      |=  card=carp
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
        =/  =case       da+now
        =/  bek=beak    [our q.beak.yoke case]
        =/  mars-path  /[a.mars]/[b.mars]
        =/  sky  (rof [~ ~] /gall %cc bek mars-path)
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
          :^  %use  agent-name  run-nonce.yoke
          ?-  -.neet
            %agent  [%out (scot %p ship.neet) name.neet wire]
            %huck   [%out (scot %p ship.neet) name.neet wire]
            %arvo   [(scot %p ship.attributing.agent-routes) wire]
          ==
        ::
        =/  =note-arvo
          =/  prov=path  /gall/[agent-name]
          ?-  -.neet
            %arvo   ?.  ?=([%l *] +.neet)
                      +.neet
                    ?+  +.neet
                      ~|(%nope !!)
                      [%l ?(%spin %shut) *]  +.neet(name [agent-name name.+.neet])
                      [%l %spit *]           +.neet(name [agent-name name.+.neet])
                    ==
            %huck   note-arvo.neet
            %agent  [%g %deal [our ship.neet prov] [name task]:neet]
          ==
        [duct %pass wire note-arvo]~
      ==
    ::  +ap-breach: ship breached, so forget about them
    ::
    ++  ap-breach
      |=  =ship
      ^+  ap-core
      =/  in=(list [=duct =^ship =path])  ~(tap by bitt.yoke)
      |-  ^+  ap-core
      ?^  in
        =?  ap-core  =(ship ship.i.in)
          =/  core  ap-load-delete(agent-duct duct.i.in)
          core(agent-duct agent-duct)
        $(in t.in)
      ::
      =/  out=(list [=wire =^ship =term])
        ~(tap ^in ~(key by boat.yoke))
      |-  ^+  ap-core
      ?~  out
        ap-core
      =?  ap-core  =(ship ship.i.out)
        =/  core
          =.  agent-duct  system-duct.state
          =.  wire.i.out  (ap-nonce-wire i.out)
          =/  way         [%out (scot %p ship) term.i.out wire.i.out]
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
      |=  =duct
      ^+  ap-core
      ::
      =/  core  ap-kill-up(agent-duct duct)
      core(agent-duct agent-duct)
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
        %+  murn  ~(tap by bitt.yoke)
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
      %+  murn  ~(tap by bitt.yoke)
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
      |=  [veb=? care=term tyl=path]
      ^-  (unit (unit cage))
      ~>  %spin.[(crip "on-peek/{<agent-name>}")]
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
        ?.  veb  [~ ~]
        ((slog leaf+"peek bad result" p.peek-result) [~ ~])
      ::  for non-%x scries, or failed %x scries, or %x results that already
      ::  have the requested mark, produce the result as-is
      ::
      ?.  ?&  ?=(%x care)
              ?=([~ ~ *] p.peek-result)
              !=(want p.u.u.p.peek-result)
          ==
        p.peek-result
      ::  for %x scries, attempt to convert to the requested mark if needed
      ::
      =*  have  p.u.u.p.peek-result
      =*  vase  q.u.u.p.peek-result
      =/  tub=(unit tube:clay)
        ?:  =(have want)  `(bake same ^vase)
        =/  tuc=(unit (unit cage))
          (rof [~ ~] /gall %cc [our q.beak.yoke da+now] /[have]/[want])
        ?.  ?=([~ ~ *] tuc)  ~
        `!<(tube:clay q.u.u.tuc)
      ?~  tub
        =/  msg  "%{(trip agent-name)}: ".
                 "peek no tube from {(trip have)} to {(trip want)}"
        ((slog leaf+msg ~) ~)
      =/  res  (mule |.((u.tub vase)))
      ?:  ?=(%& -.res)
        ``want^p.res
      =/  msg  "%{(trip agent-name)}: ".
               "peek failed tube from {(trip have)} to {(trip want)}"
      ((slog leaf+msg ~) ~)
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
              ship.attributing.agent-routes           ::  guest
              agent-name                              ::  agent
              path.attributing.agent-routes           ::  provenance
          ==                                          ::
          :*  wex=boat.yoke                           ::  outgoing
              sup=bitt.yoke                           ::  incoming
              ^=  sky                                 ::  bindings
              %-  ~(run-plot of-farm sky.yoke)
              (bake tail ,plot)
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
      ~>  %spin.[(crip "on-save/{<agent-name>}")]
      =/  old-state=vase
        ?:  ?=(%& -.agent.yoke)
          on-save:ap-agent-core
        p.agent.yoke
      =?  ap-core  &(?=(%| -.agent.yoke) ?=(^ ken.yoke))
        =-  +:(ap-ingest ~ |.([+< agent]))
        %-  zing
        %+  turn  ~(tap by `(jug spar:ames wire)`ken.yoke)
        |=  [=spar:ames wyz=(set wire)]
        (turn ~(tap in wyz) |=(=wire [%pass wire %arvo %a %keen ~ spar]))
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
      ~>  %spin.[(crip "on-watch/{<agent-name>}")]
      =/  incoming   [ship.attributing.agent-routes pax]
      =.  bitt.yoke  (~(put by bitt.yoke) agent-duct incoming)
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
      ~>  %spin.[(crip "on-poke/{<agent-name>}")]
      =^  maybe-tang  ap-core
        %+  ap-ingest  %poke-ack  |.
        (on-poke:ap-agent-core cage)
      ap-core
    ::  +ap-error: pour error.
    ::
    ++  ap-error
      |=  [=term =tang]
      ^+  ap-core
      ~>  %spin.[(crip "on-fail/{<agent-name>}")]
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
      ~>  %spin.[(crip "on-arvo/{<agent-name>}")]
      =?  sign-arvo  ?=([%lick *] sign-arvo)
        ?+  sign-arvo
          ~|(%nope !!)
        ::
            [%lick %soak *]
          =-  sign-arvo(name -)
          ?>  &(?=(^ name.sign-arvo) =(agent-name i.name.sign-arvo))
          t.name.sign-arvo
        ==
      =^  maybe-tang  ap-core
        %+  ap-ingest  ~  |.
        (on-arvo:ap-agent-core wire sign-arvo)
      =?  ken.yoke   ?=([%ames %sage *] sign-arvo)
        (~(del ju ken.yoke) p.sage.sign-arvo wire)
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
      =/  nonce=@  0
      ::
      =^  =sign:agent  ap-core
        ?.  ?=(%raw-fact -.unto)
          [unto ap-core]
        =/  =case  da+now
        ?:  ?=(%spider agent-name)
          :-  [%fact mark.unto !>(noun.unto)]
          ap-core
        =/  sky  (rof [~ ~] /gall %cb [our q.beak.yoke case] /[mark.unto])
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
          ::  %poke-ack has no nonce; ingest directly
          ::
          ?:  ?=(%poke-ack -.sign)
            ingest-and-check-error
          ::  if .agent-wire matches, it's an old pre-nonce subscription
          ::
          ?:  (~(has by boat.yoke) sub-key)
            run-sign
          ::  if an app happened to use a null wire, no-op
          ::
          ?:  =(~ agent-wire)
            on-missing
          =/  has-nonce=(unit @ud)  (slaw %ud (head agent-wire))
          ?:  &(?=(~ has-nonce) ?=(%kick -.sign))
            on-weird-kick
          ::  pop nonce off .agent-wire and match against stored subscription
          ::
          ?>  ?=(^ has-nonce)
          =:  nonce       u.has-nonce
              agent-wire  (tail agent-wire)
            ==
          ?~  got=(~(get by boar.yoke) sub-key)
            on-missing
          ?:  =(nonce.u.got nonce)
            run-sign
          (on-bad-nonce nonce.u.got)
      ::
      ++  sub-key  [agent-wire dock]
      ++  ingest
        ~>  %spin.[(crip "on-agent/{<agent-name>}")]
        (ap-ingest ~ |.((on-agent:ap-agent-core agent-wire sign)))
      ++  run-sign
        ?-    -.sign
            %poke-ack  !!
            %fact
          =^  tan  ap-core  ingest
          ?~  tan  ap-core
          =.  ap-core  (ap-kill-down sub-key)
          (ap-error -.sign leaf/"take %fact failed, closing subscription" u.tan)
        ::
            %kick
          =:  boar.yoke  (~(del by boar.yoke) sub-key)
              boat.yoke  (~(del by boat.yoke) sub-key)
            ==
          ingest-and-check-error
        ::
            %watch-ack
          ?.  (~(has by boat.yoke) sub-key)
            %.  ap-core
            %+  trace  odd.veb.bug.state  :~
              leaf+"got ack for nonexistent subscription"
              leaf+"{<dock>}: {<agent-wire>}"
              >wire=wire<
            ==
          =?  boar.yoke  ?=(^ p.sign)  (~(del by boar.yoke) sub-key)
          ::
          =/  [acked=? =path]  (~(got by boat.yoke) sub-key)
          ?:  &(?=(~ p.sign) acked)
            ::  if there's no error and the subscription has been acked, no-op
            ::
            %.  ap-core
            %^  trace  odd.veb.bug.state
            leaf/"2nd watch-ack on {<path>}"  ~
          =.  boat.yoke
            ?^  p.sign  (~(del by boat.yoke) sub-key)
            ::
            %+  ~(jab by boat.yoke)  sub-key
            |=  val=[acked=? =^path]
            val(acked &)
          ::
          ingest-and-check-error
        ==
      ::
      ++  on-missing
        %.  ap-core
        %+  trace  odd.veb.bug.state  :~
          leaf+"got {<-.sign>} for nonexistent subscription"
          leaf+"{<dock>}: {<[nonce=nonce agent-wire]>}"
          >wire=wire<
        ==
      ::
      ++  on-weird-kick
        %.  run-sign
        %+  trace  odd.veb.bug.state  :~
          leaf+"got %kick for nonexistent subscription"
          leaf+"{<dock>}: {<agent-wire>}"
          >wire=wire<
        ==
      ::
      ++  on-bad-nonce
        |=  stored-nonce=@
        %.  ap-core
        %+  trace  odd.veb.bug.state  :~
          =/  nonces  [expected=stored-nonce got=nonce]
          =/  ok  |(?=(?(%fact %kick) -.sign) =(~ p.sign))
          leaf+"stale {<-.sign>} {<nonces>} ok={<ok>}"
        ::
          leaf+"{<dock>}: {<agent-wire>}"
          >wire=wire<
        ==
      ::
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
      ~>  %spin.[(crip "on-init/{<agent-name>}")]
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
      ap-core(bitt.yoke (~(del by bitt.yoke) agent-duct))
    ::  +ap-load-delete: load delete.
    ::
    ++  ap-load-delete
      ^+  ap-core
      ~>  %spin.[(crip "on-leave/{<agent-name>}")]
      ::
      =/  maybe-incoming  (~(get by bitt.yoke) agent-duct)
      ?~  maybe-incoming
        ap-core
      ::
      =/  incoming   u.maybe-incoming
      =.  bitt.yoke  (~(del by bitt.yoke) agent-duct)
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
      =/  =sack  [our our /gall/[agent-name]]
      :~  [duct %slip %g %deal sack agent-name %leave ~]
          [duct %give %unto %kick ~]
      ==
    ::  +ap-kill-down: 2-sided kill from subscriber side
    ::
    ::    Must process leave first in case kick handler rewatches.
    ::
    ++  ap-kill-down
      |=  [sub-wire=wire =dock]
      ^+  ap-core
      =.  ap-core
        ::  we take care to include the nonce in the "kernel-facing" wire
        ::
        (ap-pass (ap-nonce-wire sub-wire dock) %agent dock %leave ~)
      (ap-pass sub-wire %huck dock %b %huck `sign-arvo`[%gall %unto %kick ~])
    ::  +ap-doff: kill old-style outgoing subscriptions
    ::
    ++  ap-doff
      |=  ship=(unit ship)
      ^+  ap-core
      =/  subs  ~(tap in ~(key by boat.yoke))
      |-  ^+  ap-core
      ?~  subs  ap-core
      =+  [wyr dok]=i.subs
      ?:  &(?=(^ ship) !=(u.ship ship.dok))
        $(subs t.subs)
      ::  if we haven't created new-style (nonced) subscriptions yet,
      ::  kick the old-style (nonceless) one that's in use right now.
      ::
      ::NOTE  yes, still safe for pre-release ships with nonce=1,
      ::      this makes a new flow but cleans it up right away.
      ::
      =?  ap-core  (gte 1 (~(got by boar.yoke) wyr dok))
        (ap-pass wyr %agent dok %leave ~)
      $(subs t.subs)
    ::  +ap-rake: clean up the dead %leave's
    ::
    ++  ap-rake
      |=  all=?
      =/  subs  ~(tap in ~(key by boat.yoke))
      |^  ^+  ap-core
      ?~  subs  ap-core
      =/  [=wire =dock]  i.subs
      =/  non  (~(got by boar.yoke) wire dock)
      ?:  &(!all =(0 non))
        $(subs t.subs)
      ?~  per=(scry-peer-state p.dock)
        $(subs t.subs)
      ::
      =/  dud=(set duct)
        =/  mod=^wire
          :*  %gall  %use  agent-name  run-nonce.yoke
              %out  (scot %p p.dock)  q.dock
              '0'  wire
          ==
        %-  ~(rep by by-duct.ossuary.u.per)
        |=  [[=duct =bone] out=(set duct)]
        ^+  out
        ?.  ?&  ?=([* [%gall %use @ @ %out @ @ @ *] *] duct)
                =(mod i.t.duct(i.t.t.t.t.t.t.t '0'))
            ==
          out
        ?:  (~(has in closing.u.per) bone)  out
        ~>  %slog.0^leaf+"gall: rake {<i.t.duct>}"
        (~(put in out) duct)
      ::
      %-  ap-move
      (turn ~(tap in dud) |=(d=duct [+.d %pass -.d %a %cork p.dock]))
      ::
      ++  scry-peer-state
        |=  her=ship
        ~+  ^-  (unit peer-state:ames)
        =/  sky  (rof [~ ~] /gall %ax [our %$ da+now] /peers/(scot %p her))
        ?:  |(?=(~ sky) ?=(~ u.sky))
          ~
        =/  sat  !<(ship-state:ames q.u.u.sky)
        ?>(?=(%known -.sat) (some +.sat))
      --
    ::  +ap-lave: delete stale incoming subscriptions in %gall
    ::
    ++  ap-lave
      |=  [v=?(%a %g) =duct]
      ^+  ap-core
      =;  core=_ap-core
        core(agent-duct agent-duct)
      ?:  ?=(%a v)
        ap-kill-up(agent-duct duct)
      ap-load-delete(agent-duct duct)
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
      =/  res  (mock [run %9 2 %0 1] (look rof [~ ~] /gall/[agent-name]))
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
      =/  res  (mock [run %9 2 %0 1] (look rof [~ ~] /gall/[agent-name]))
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
        ^-  (list carp)
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
      =^  fex  ap-core  (ap-handle-sky -.p.result)
      =.  ken.yoke    (ap-handle-ken fex)
      =/  moves       (zing (turn fex ap-from-internal))
      =.  bitt.yoke   (ap-handle-kicks moves)
      (ap-handle-peers moves)
    ::  +ap-handle-sky: apply effects to the agent's scry namespace
    ::
    ++  ap-handle-sky
      =|  fex=(list carp)
      |=  caz=(list card:agent)
      ^+  [fex ap-core]
      ?~  caz  [(flop fex) ap-core]
      ?-  i.caz
        [%pass * %grow *]  $(caz t.caz, ap-core (ap-grow +.q.i.caz))
        [%pass * %tomb *]  $(caz t.caz, ap-core (ap-tomb +.q.i.caz))
        [%pass * %cull *]  $(caz t.caz, ap-core (ap-cull +.q.i.caz))
        [%pass * %tend *]  $(caz t.caz, ap-core (ap-tend +.q.i.caz))
        [%pass * %germ *]  $(caz t.caz, ap-core (ap-germ +.q.i.caz))
        [%pass * %snip *]  $(caz t.caz, ap-core (ap-snip +.q.i.caz))
        [%pass * %keen *]  $(caz t.caz, ap-core (ap-keen p.i.caz +.q.i.caz))
        [%pass * ?(%agent %arvo %pyre) *]  $(caz t.caz, fex [i.caz fex])
        [%give *]  $(caz t.caz, fex [i.caz fex])
        [%slip *]  !!
      ==
    ::  +ap-handle-ken
    ::
    ++  ap-handle-ken
      |=  fex=(list carp)
      ^+  ken.yoke
      %+  roll  fex
      |=  [=carp ken=_ken.yoke]
      ?+  carp  ken
        [%pass * %arvo %a %keen @ spar=*]  (~(put ju ken) [spar.q p]:carp)
        [%pass * %arvo %a %yawn spar=*]  (~(del ju ken) [spar.q p]:carp)
      ==
    ::  +ap-handle-kicks: handle cancels of bitt.watches
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
      (~(dif by bitt.yoke) quit-map)
    ::  +ap-handle-peers: handle new boat.watches
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
        =/  =dock           [q.p q]:q.move.move
        =/  sys-wire=^wire  (scag 6 `^wire`wire)
        =/  sub-wire=^wire  (slag 6 `^wire`wire)
        ::
        ?.  (~(has by boat.yoke) sub-wire dock)
          %.  $(moves t.moves)
          %^  trace  odd.veb.bug.state
          leaf/"missing subscription, got %leave"  ~
        =/  nonce=@  (~(got by boar.yoke) sub-wire dock)
        =.  p.move.move
          %+  weld  sys-wire
          (ap-nonce-wire sub-wire dock)
        =:  boat.yoke  (~(del by boat.yoke) [sub-wire dock])
            boar.yoke  (~(del by boar.yoke) [sub-wire dock])
          ==
        ::  if nonce = 0, this was a pre-nonce subscription so later
        ::  subscriptions need to start subscribing on the next nonce
        ::
        =?  sub-nonce.yoke  =(nonce 0)  +(sub-nonce.yoke)
        $(moves t.moves, new-moves [move new-moves])
      ?.  ?=([* %pass * %g %deal * * ?(%watch %watch-as) *] move)
        $(moves t.moves, new-moves [move new-moves])
      =/  =wire  p.move.move
      ?>  ?=([%use @ @ %out @ @ *] wire)
      =/  sys-wire=^wire  (scag 6 `^wire`wire)
      =/  sub-wire=^wire  (slag 6 `^wire`wire)
      =/  [=dock =deal]  [[q.p q] r]:q.move.move
      ::
      ?:  (~(has by boat.yoke) sub-wire dock)
        =.  ap-core
          =/  =tang
            ~[leaf+"subscribe wire not unique" >agent-name< >sub-wire< >dock<]
          =/  have  (~(got by boat.yoke) sub-wire dock)
          %-  (slog >out=have< tang)
          (ap-error %watch-not-unique tang)  ::  reentrant, maybe bad?
        $(moves t.moves)
      ::
      ::NOTE  0-check guards against pre-release bug
      =?  p.move.move  !=(0 sub-nonce.yoke)
        (weld sys-wire [(scot %ud sub-nonce.yoke) sub-wire])
      %_    $
          moves            t.moves
          new-moves       [move new-moves]
          sub-nonce.yoke  +(sub-nonce.yoke)
      ::
          boat.yoke
        %+  ~(put by boat.yoke)  [sub-wire dock]
        :-  acked=|
        path=?+(-.deal !! %watch path.deal, %watch-as path.deal)
      ::
          boar.yoke
        (~(put by boar.yoke) [sub-wire dock] sub-nonce.yoke)
      ==
    --
  --
::  +call: request
::
++  call
  ~%  %gall-call  +>   ~
  |=  [=duct dud=(unit goof) hic=(hobo task)]
  ^-  [(list move) _gall-payload]
  ~>  %spin.['call/gall']
  ?^  dud
    ~|(%gall-call-dud (mean tang.u.dud))
  ::
  ~|  [%gall-call-failed duct hic]
  =/  =task  ((harden task) hic)
  =/  prov=path
    ?:  ?=(%deal -.task)
      ?.(=(p.p.task our) *path r.p.task)
    ?.  ?&  ?=([^ *] duct)
            ?=  $?  %ames  %behn  %clay
                    %dill  %eyre  %gall
                    %iris  %jael  %khan
                ==
            i.i.duct
        ==
      *path
    /[i.i.duct]
  ::
  =/  mo-core  (mo-abed:mo duct)
  ?-    -.task
      %clog
    =+  ;;([dap=@tas =^duct] id.task)
    mo-abet:(mo-handle-sys-clog:mo-core duct dap)
      ::
      %deal
    =/  [=sack =term =deal]  [p q r]:task
    ?.  =(q.sack our)
      ?>  =(p.sack our)
      mo-abet:(mo-send-foreign-request:mo-core q.sack term deal)
    mo-abet:(mo-handle-local:mo-core prov p.sack term deal)
  ::
      %init  [~ gall-payload(system-duct.state duct)]
      %plea
    =/  =ship  ship.task
    =/  =path  path.plea.task
    =/  =noun  payload.plea.task
    ::
    ?:  ?=([%gf *] path)
      ?>  ?=(flub-requesst noun)
      mo-abet:(mo-handle-flub-plea:mo-core ship)
    ?:  ?=([%gk @ ~] path)
      =/  agent-name  i.t.path
      =+  ;;(=fine-request noun)
      =<  mo-abet
      (mo-handle-key-request:mo-core ship agent-name path.fine-request)
    ?>  ?=([%ge @ ~] path)
    =/  agent-name  i.t.path
    ::
    =+  ;;(=ames-request-all noun)
    ?>  ?=(%0 -.ames-request-all)
    =<  mo-abet
    (mo-handle-ames-request:mo-core ship agent-name +.ames-request-all)
  ::
      %sear  mo-abet:(mo-filter-queue:mo-core ship.task)
      %jolt  mo-abet:(mo-jolt:mo-core dude.task our desk.task)
      %idle  mo-abet:(mo-idle:mo-core prov dude.task)
      %load  mo-abet:(mo-load:mo-core prov +.task)
      %nuke  mo-abet:(mo-nuke:mo-core prov dude.task)
      %doff  mo-abet:(mo-doff:mo-core prov +.task)
      %rake  mo-abet:(mo-rake:mo-core prov +.task)
      %lave  mo-abet:(mo-lave:mo-core prov +.task)
      %halt  mo-abet:(mo-halt:mo-core prov +.task)
      %spew  mo-abet:(mo-spew:mo-core veb.task)
      %sift  mo-abet:(mo-sift:mo-core dudes.task)
      %trim  [~ gall-payload]
      %vega  [~ gall-payload]
  ==
::  +load: recreate vane; note, only valid if called from pupa
::
++  load
  |^  |=  old=spore-any
      ~>  %spin.['load/gall']
      =?  old  ?=(%7 -.old)   (spore-7-to-8 +.old)
      =?  old  ?=(%8 -.old)   (spore-8-to-9 +.old)
      =?  old  ?=(%9 -.old)   (spore-9-to-10 +.old)
      =?  old  ?=(%10 -.old)  (spore-10-to-11 +.old)
      =?  old  ?=(%11 -.old)  (spore-11-to-12 +.old)
      =?  old  ?=(%12 -.old)  (spore-12-to-13 +.old)
      =?  old  ?=(%13 -.old)  (spore-13-to-14 +.old)
      =?  old  ?=(%14 -.old)  (spore-14-to-15 +.old)
      =?  old  ?=(%15 -.old)  (spore-15-to-16 +.old)
      =?  old  ?=(%16 -.old)  (spore-16-to-17 +.old)
      =?  old  ?=(%17 -.old)  (spore-17-to-18 +.old)
      ?>  ?=(%18 -.old)
      gall-payload(state old)
  ::
  +$  spore-any
    $%  [%18 spore]
        [%7 spore-7]
        [%8 spore-8]
        [%9 spore-9]
        [%10 spore-10]
        [%11 spore-11]
        [%12 spore-12]
        [%13 spore-13]
        [%14 spore-14]
        [%15 spore-15]
        [%16 spore-16]
        [%17 spore-17]
    ==
  +$  spore-17  spore-16
  +$  spore-16
    $:  system-duct=duct
        outstanding=(map [wire duct] (qeu remote-request))
        contacts=(set ship)
        eggs=(map term egg)
        blocked=(map term (qeu blocked-move))
        =bug
        leaves=(unit [=duct =wire date=@da])
    ==
  +$  spore-15
    $+  spore-15
    $:  system-duct=duct
        outstanding=(map [wire duct] (qeu remote-request))
        contacts=(set ship)
        eggs=(map term egg-15)
        blocked=(map term (qeu blocked-move))
        =bug
        leaves=(unit [=duct =wire date=@da])
    ==
  +$  spore-14
    $:  system-duct=duct
        outstanding=(map [wire duct] (qeu remote-request))
        contacts=(set ship)
        eggs=(map term egg-15)
        blocked=(map term (qeu blocked-move))
        =bug
    ==
  ::
  +$  spore-13
    $:  system-duct=duct
        outstanding=(map [wire duct] (qeu remote-request))
        contacts=(set ship)
        eggs=(map term egg-15)
        blocked=(map term (qeu blocked-move-13))
        =bug
    ==
  ::
  +$  blocked-move-13  [=duct routes=routes-13 move=(each deal unto)]
  +$  routes-13
    $:  disclosing=(unit (set ship))
        attributing=ship
    ==
  +$  spore-12
    $:  system-duct=duct
        outstanding=(map [wire duct] (qeu remote-request))
        contacts=(set ship)
        eggs=(map term egg-12)
        blocked=(map term (qeu blocked-move-13))
        =bug
    ==
  +$  egg-12
    $%  [%nuke sky=(map spur @ud)]
        $:  %live
            control-duct=duct
            run-nonce=@t
            sub-nonce=@
            =stats
            =bitt
            =boat
            =boar
            code=~
            old-state=[%| vase]
            =beak
            marks=(map duct mark)
            sky=(map spur plot)
    ==  ==
  +$  spore-11
    $:  system-duct=duct
        outstanding=(map [wire duct] (qeu remote-request))
        contacts=(set ship)
        eggs=(map term egg-11)
        blocked=(map term (qeu blocked-move-13))
        =bug
    ==
  +$  egg-11
    $:  control-duct=duct
        run-nonce=@t
        sub-nonce=@
        =stats
        =bitt
        =boat
        =boar
        code=~
        old-state=[%| vase]
        =beak
        marks=(map duct mark)
    ==
  +$  spore-10
    $:  system-duct=duct
        outstanding=(map [wire duct] (qeu remote-request))
        contacts=(set ship)
        eggs=(map term egg-10)
        blocked=(map term (qeu blocked-move-13))
        =bug
    ==
  +$  egg-10
    $:  control-duct=duct
        run-nonce=@t
        sub-nonce=@
        live=?
        =stats
        =bitt
        =boat
        =boar
        old-state=(each vase vase)
        =beak
        marks=(map duct mark)
    ==
  +$  spore-9
    $:  system-duct=duct
        outstanding=(map [wire duct] (qeu remote-request-9))
        contacts=(set ship)
        eggs=(map term egg-10)
        blocked=(map term (qeu blocked-move-13))
        =bug
    ==
  ::
  +$  remote-request-9  ?(remote-request %cork)
  ::
  +$  spore-8
    $:  system-duct=duct
        outstanding=(map [wire duct] (qeu remote-request-9))
        contacts=(set ship)
        eggs=(map term egg-8)
        blocked=(map term (qeu blocked-move-13))
    ==
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
  +$  watches-8  [inbound=bitt outbound=boat-8]
  +$  boat-8  (map [wire ship term] [acked=? =path])
  +$  spore-7
    $:  wipe-eyre-subs=_|  ::NOTE  band-aid for #3196
        system-duct=duct
        outstanding=(map [wire duct] (qeu remote-request-9))
        contacts=(set ship)
        eggs=(map term egg-8)
        blocked=(map term (qeu blocked-move-13))
    ==
  ::
  ++  spore-7-to-8
    |=  old=spore-7
    ^-  spore-any
    :-  %8
    ^-  spore-8
    =.  eggs.old
      %-  ~(urn by eggs.old)
      |=  [a=term e=egg-8]
      ::  kiln will kick off appropriate app revival
      ::
      e(old-state [%| p.old-state.e])
    +.old
  ::
  ++  spore-8-to-9
    |=  old=spore-8
    :-  %9
    ^-  spore-9
    =-  old(eggs -, blocked [blocked.old *bug])
    %-  ~(run by eggs.old)
    |=  =egg-8
    ^-  egg-10
    =/  [=bitt =boat =boar]  (watches-8-to-9 watches.egg-8)
    :*  control-duct.egg-8
        run-nonce.egg-8
        sub-nonce=1
        live.egg-8
        stats.egg-8
        bitt  boat  boar
        [old-state beak marks]:egg-8
    ==
  ::
  ++  watches-8-to-9
    |=  watches-8
    ^-  [bitt boat boar]
    [inbound outbound (~(run by outbound) |=([acked=? =path] nonce=0))]
  ::
  ::  remove %cork
  ::
  ++  spore-9-to-10
    |=  old=spore-9
    :-  %10
    ^-  spore-10
    =-  old(outstanding -)
    %-  ~(run by outstanding.old)
    |=  q=(qeu remote-request-9)
    %-  ~(gas to *(qeu remote-request))
    %+  murn  ~(tap to q)
    |=(r=remote-request-9 ?:(?=(%cork r) ~ `r))
  ::
  ::  removed live
  ::  changed old-state from (each vase vase) to [%| vase]
  ::  added code
  ::
  ++  spore-10-to-11
    |=  old=spore-10
    :-  %11
    ^-  spore-11
    %=    old
        eggs
      %-  ~(urn by eggs.old)
      |=  [a=term e=egg-10]
      ^-  egg-11
      e(|3 |4.e(|4 `|8.e(old-state [%| p.old-state.e])))
    ==
  ::
  ::  added sky
  ::
  ++  spore-11-to-12
    |=  old=spore-11
    :-  %12
    ^-  spore-12
    %=    old
        eggs
      %-  ~(urn by eggs.old)
      |=  [a=term e=egg-11]
      ^-  egg-12
      live/e(marks [marks.e sky:*$>(%live egg-12)])
    ==
  ::
  ::  added ken
  ::
  ++  spore-12-to-13
    |=  old=spore-12
    :-  %13
    ^-  spore-13
    %=    old
        eggs
      %-  ~(urn by eggs.old)
      |=  [a=term e=egg-12]
      ^-  egg-15
      ?:  ?=(%nuke -.e)  e
      ::!!
      e(sky [sky.e ken:*$>(%live egg-15)])
    ==
  ::
  ++  spore-13-to-14
    |=  old=spore-13
    :-  %14
    ^-  spore-14
    %=    old
        blocked
      ^-  (map term (qeu blocked-move))
      %-  ~(run by blocked.old)
      |=  q=(qeu blocked-move-13)
      %-  ~(gas to *(qeu blocked-move))
      %+  turn  ~(tap to q)
      |=  blocked=blocked-move-13
      ^-  blocked-move
      %=  blocked
        attributing.routes  [ship=attributing.routes.blocked path=/]
      ==
    ==
  ::  added nacked-leaves timer
  ::
  ++  spore-14-to-15
    |=  old=spore-14
    :-  %15
    ^-  spore-15
    old(bug [bug.old ~])
  ::  convert to versioned sky
  ::
  ++  spore-15-to-16
    |=  old=spore-15
    :-  %16
    ^-  spore-16
    %=    old
        eggs
      %-  ~(urn by eggs.old)
      |=  [=term e=egg-15]
      ^-  egg
      ?:  ?=(%nuke -.e)  [%nuke ~ ~]
      %=    e
          ken  [ken.e ~ ~]
      ::
          sky
        =|  =farm
        =/  ski  ~(tap by sky.e)
        |-  ^+  farm
        ?~  ski
          farm
        =/  [=spur p=plot]  i.ski
        =;  new
          ?~  nex=(~(put-grow of-farm farm) spur new)
            ~&  %weird
            !!  :: shouldn't continue else loss of ref integrity
            :: $(ski t.ski)
          $(farm u.nex, ski t.ski)
        :-  ~
        =/  m  ~(val by fan.p)
        %+  gas:on-path  *_fan.p
        %+  turn
          ^-  (list @)
          =/  wit  ~(wyt by fan.p)
          ?:  =(0 wit)  ~
          (gulf 1 wit)
        |=  a=@ud
        [a (snag (dec a) m)]
      ==
    ==
  ::  drop unto blocked moves;s
  ::
  ++  spore-16-to-17
    |=  old=spore-16
    :-  %17
    ^-  spore-17
    %=    old
        blocked
      %-  ~(urn by blocked.old)
      |=  [=term q=(qeu blocked-move)]
      ^+  q
      %-  ~(rep by q)
      |=  [=blocked-move r=(qeu blocked-move)]
      ?:  ?=(%| -.move.blocked-move)
        r
      ::  /gall-use-wire will be dropped in mo-clear-queu
      ::
      (~(put to r) blocked-move(duct [/gall-use-wire duct.blocked-move]))
    ==
  ::
  ::  add flubbed/halted agents
  ::
  ++  spore-17-to-18
    |=  old=spore-17
    ^-  spore-18
    :-  %18
    %=    old
        leaves
      [leaves.old flub-ducts=~ flubs=~ halts=~]
    ==
  ::
  --
::  +scry: standard scry
::
++  scry
  ~/  %gall-scry
  ^-  roon
  |=  [lyc=gang pov=path care=term bem=beam]
  ^-  (unit (unit cage))
  ~>  %spin.['scry/gall']
  =*  ship  p.bem
  =*  dap  q.bem
  =/  =coin  $/r.bem
  =*  path  s.bem
  ::
  ?:  ?&  ?=(%da -.r.bem)
          (gth p.r.bem now)
      ==
    ~
  ::
  ?.  ?=([%$ *] path)  ::  [%$ *] is for the vane, all else is for the agent
    ?.  ?&  =(our ship)
            =([%$ %da now] coin)
            =([~ ~] lyc)
        ==                           ~
    ?.  (~(has by yokes.state) dap)  [~ ~]
    ?.  ?=(^ path)                   ~
    =/  =routes  [~ ship pov]
    (mo-peek:mo & dap routes care path)
  ::
  =>  .(path t.path)
  ::
  ?:  ?&  =(%u care)
          =(~ path)
          =([%$ %da now] coin)
          =(our ship)
          =([~ ~] lyc)
      ==
    =;  hav=?
      [~ ~ noun+!>(hav)]
    =/  yok=(unit yoke)  (~(get by yokes.state) dap)
    &(?=([~ %live *] yok) -.agent.u.yok)
  ::
  ?:  ?&  =(%d care)
          =(~ path)
          =([%$ %da now] coin)
          =(our ship)
          =([~ ~] lyc)
      ==
    =/  yok=(unit yoke)  (~(get by yokes.state) dap)
    ?.  ?=([~ %live *] yok)
      [~ ~]
    [~ ~ desk+!>(q.beak.u.yok)]
  ::
  ?:  ?&  =(%e care)
          =(~ path)
          =([%$ %da now] coin)
          =(our ship)
          =([~ ~] lyc)
      ==
    :+  ~  ~
    :-  %apps  !>  ^-  (set [=dude live=?])
    =*  syd=desk  dap
    %+  roll  ~(tap by yokes.state)
    |=  [[=dude =yoke] acc=(set [=dude live=?])]
    ?.  ?&  ?=(%live -.yoke)
            =(syd q.beak.yoke)
        ==
      acc
    (~(put in acc) [dude -.agent.yoke])
  ::
  ?:  ?&  =(%f care)
          =(~ path)
          =([%$ %da now] coin)
          =(our ship)
          =([~ ~] lyc)
      ==
    :+  ~  ~
    :-  %nonces  !>  ^-  (map dude @)
    %-  malt  %+  murn  ~(tap by yokes.state)
    |=  [=dude =yoke]
    ?:  ?=(%nuke -.yoke)  ~  `[dude sub-nonce.yoke]
  ::
  ?:  ?&  =(%g care)
          =(~ path)
          =([%$ %da now] coin)
          =(our ship)
          =([~ ~] lyc)
      ==
    ::  XX support per ship
    ``flubs+!>(flubs.state)
  ::
  ?:  ?&  =(%h care)
          =(~ path)
          =([%$ %da now] coin)
          =(our ship)
          =([~ ~] lyc)
      ==
    ``halts+!>(halts.state)
  ::
  ?:  ?&  =(%n care)
          ?=([@ @ ^] path)
          =([%$ %da now] coin)
          =(our ship)
          =([~ ~] lyc)
      ==
    =/  yok  (~(get by yokes.state) dap)
    ?.  ?=([~ %live *] yok)
      [~ ~]
    =/  [=^ship =term =wire]
      [(slav %p i.path) i.t.path t.t.path]
    ?~  nonce=(~(get by boar.u.yok) [wire ship term])
      [~ ~]
    [~ ~ atom+!>(u.nonce)]
  ::
  ?:  ?&  =(%v care)
          =([%$ %da now] coin)
          =(our ship)
          =([~ ~] lyc)
      ==
    =/  yok  (~(get by yokes.state) dap)
    ?.  ?=([~ %live *] yok)
      [~ ~]
    =/  =egg
      %=    u.yok
          code   ~
          agent
        :-  %|
        ?:  ?=(%| -.agent.u.yok)
          p.agent.u.yok
        on-save:p.agent.u.yok
      ==
    ``noun+!>(`egg-any`[%16 egg]) :: XX egg-18 same as 17 and 16
  ::
  ?:  ?&  =(%w care)
          =([%$ %da now] coin)
          =(our ship)
          ?=([%'1' *] path)
      ==
    =>  .(path t.path)
    =/  yok  (~(get by yokes.state) q.bem)
    ?.  ?=([~ %live *] yok)             [~ ~]
    ?~  ski=(~(get of-farm sky.u.yok) path)  [~ ~]
    ?~  las=(ram:on-path fan.u.ski)     [~ ~]
    ?.  (mo-authorized:mo lyc sky.u.yok q.bem path)
      ~
    ``case/!>(ud/key.u.las)
  ::
  =?  path  =(/whey path)  /1/whey
  ?:  &(?=(%x care) ?=([%'1' *] path))
    =>  .(path t.path)
    ?.  =(p.bem our)  ~
    ::
    ?:  ?=(%$ q.bem)  :: app %$ reserved
      ?+    path  ~
          [%whey ~]
        ?.  ?=([~ ~] lyc)  ~
        =/  blocked
          =/  queued  (~(run by blocked.state) |=((qeu blocked-move) [%.y +<]))
          (sort ~(tap by queued) aor)
        ::
        =/  running
          %+  turn  (sort ~(tap by yokes.state) aor)
          |=  [dap=term =yoke]
          ^-  mass
          =/  met=(list mass)
            =/  dat  (mo-peek:mo | dap [~ ship pov] %x /whey/mass)
            ?:  ?=(?(~ [~ ~]) dat)  ~
            (fall ((soft (list mass)) q.q.u.u.dat) ~)
          ?~  met
            dap^&+yoke
          dap^|+(welp met dot+&+yoke ~)
        ::
        =/  maz=(list mass)
          :~  [%foreign %.y contacts.state]
              [%blocked %.n blocked]
              [%active %.n running]
          ==
        ``mass+!>(maz)
      ==
    ::
    ?~  yok=(~(get by yokes.state) q.bem)  ~
    ?:  ?=(%nuke -.u.yok)  ~
    ?~  ski=(~(get of-farm sky.u.yok) path)
      ~
    ?.  (mo-authorized:mo lyc sky.u.yok q.bem path)
      ~
    =/  res=(unit (each page @uvI))
      ?+    -.r.bem  ~
          %ud  (bind (get:on-path fan.u.ski p.r.bem) tail)
          %da
        %-  head
        %^    (dip:on-path (unit (each page @uvI)))
            fan.u.ski
          ~
        |=  [res=(unit (each page @uvI)) @ud =@da val=(each page @uvI)]
        ^-  [new=(unit [@da _val]) stop=? res=(unit _val)]
        :-  `[da val]
        ?:((lte da p.r.bem) |/`val &/res)
      ==
    ?.  ?=([~ %& *] res)  ~
    ``p.u.res(q !>(q.p.u.res))
  ::
  ?:  ?&  =(%t care)
          =([%$ %da now] coin)
          =(our ship)
          ?=([%'1' *] path)
      ==
    =>  .(path t.path)
    =/  yok  (~(get by yokes.state) q.bem)
    ?.  ?=([~ %live *] yok)  ~
    =/  keys=(list coop)  (~(key-coops of-farm sky.u.yok) path)
    =/  authorized=?
      ?:  =([~ ~] lyc)  %.y
      |-
      ?~  keys  %.y
      ?<  ?=(~ lyc)
      ?.  (mo-authorized-coop:mo u.lyc sky.u.yok q.bem path i.keys)
        %.n
      $(keys t.keys)
    ?.  authorized  ~
    :^  ~  ~  %file-list  !>  ^-  (list ^path)
    %+  skim  (turn ~(tap-plot of-farm sky.u.yok) head)
    |=  =spur
    ?&  =(path (scag (lent path) spur))
        !=(path spur)
    ==
  ::
  ?:  ?&  =(%y care)
          =(~ path)
          =([%$ %da now] coin)
          =(our ship)
      ==
    :+  ~  ~
    :-  %yokes  !>  ^+  yokes.state
    %-  ~(rep by yokes.state)
    |=  [[=dude =yoke] acc=_yokes.state]
    ?:(?=(%live -.yoke) acc (~(del by acc) dude))
  ::
  ?:  ?&  =(%z care)
          =(our ship)
          ?=([%'1' *] path)
      ==
    =>  .(path t.path)
    =/  yok  (~(get by yokes.state) q.bem)
    ?.  ?=([~ %live *] yok)             ~
    ?~  ski=(~(get of-farm sky.u.yok) path)  ~
    ?.  (mo-authorized:mo lyc sky.u.yok q.bem path)
      ~
    =/  res=(unit (pair @da (each noun @uvI)))
      ?+  -.r.bem  ~
        %ud  (get:on-path fan.u.ski p.r.bem)
        %da  ?.(=(p.r.bem now) ~ (bind (ram:on-path fan.u.ski) tail))
      ==
    ?+  res  ~
      [~ @ %| *]  ``noun/!>(p.q.u.res)
      [~ @ %& *]  ``noun/!>(`@uvI`(shax (jam p.q.u.res)))
    ==
  ~
::  +stay: save without cache; suspend non-%base agents
::
++  stay
  ^-  spore-18
  =;  eggs=(map term egg)  state(yokes eggs)
  %-  ~(run by yokes.state)
  |=  =yoke
  ^-  egg
  ?:  ?=(%nuke -.yoke)  yoke
  %=    yoke
      code   ~
      agent
    :-  %|
    ?:  ?=(%| -.agent.yoke)
      p.agent.yoke
    on-save:p.agent.yoke
  ==
::  +take: response
::
++  take
  ~/  %gall-take
  |=  [=wire =duct dud=(unit goof) syn=sign-arvo]
  ^-  [(list move) _gall-payload]
  ~>  %spin.['take/gall']
  ?^  dud
    ~&(%gall-take-dud ((slog tang.u.dud) [~ gall-payload]))
  ?:  =(/nowhere wire)
    [~ gall-payload]
  ?:  =(/clear-huck wire)
    =/  =gift  ?>(?=([%behn %heck %gall *] syn) +>+.syn)
    [[duct %give gift]~ gall-payload]
  =/  mo-core  (mo-abed:mo duct)
  ?:  ?=([%key *] wire)
    ~|  [%gall-take-key-failed wire]
    mo-abet:(mo-handle-key:mo-core t.wire syn)
  ::
  ?:  ?=([%nacked-leaves ~] wire)
    =;  core=_mo-core:mo
      ::  next time a %leave gets nacked, the state and timer will be set again.
      ::
      mo-abet:core(leaves.state ~)
    %-  ~(rep by outstanding.state)
    |=  [[[=^wire =^duct] stand=(qeu remote-request)] core=_mo-core:mo]
    ?:  =(~ stand)  core
    =^  rr  stand  ~(get to stand)
    ::  sanity check in the outstanding queue:
    ::  if there's a %leave that was nacked, there should be a %missing request
    ::  otherwise %leave is the only request in the queue, and we haven't heard
    ::  anc %ack or a %nack, so %ames is still trying to send it.
    ::
    ~?  >>>  ?&  ?=(%leave rr)
                 !=(~ stand)
                 =^(rr stand ~(get to stand) !=(%missing rr))
              ==
      "extraneous request outstanding [{<wire>} {<duct>} {<stand>} {<rr>}]"
    =?  core  ?&  ?=(%leave rr)
                  !=(~ stand)
                  =^(rr stand ~(get to stand) &(=(%missing rr) =(~ stand)))
              ==
      =+  core=(mo-handle-nacked-leaves:(mo-abed:core duct) wire)
      ::  make sure that only the %leave remains in the queue
      ::
      %-  %:  trace  odd.veb.bug.state  *dude  ~
            leaf/"resending %nacked %leave {<(spud wire)>}"  ~
          ==
      %_    core
          outstanding.state
        %+  ~(put by outstanding.state)  [wire duct]
        (~(put to *(qeu remote-request)) %leave)
      ==
    core
  ::
  ~|  [%gall-take-failed wire]
  ?>  ?=([?(%sys %use) *] wire)
  =<  mo-abet
  %.  [t.wire ?:(?=([%behn %heck *] syn) syn.syn syn)]
  ?-  i.wire
    %sys  mo-handle-sys:mo-core
    %use  mo-handle-use:mo-core
  ==
--
