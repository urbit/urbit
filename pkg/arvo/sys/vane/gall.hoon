!:  ::  %gall, agent execution
!?  163
!:
::::
|=  pit=vase
=,  gall
=>
|%
::  +move: Arvo-level move
::
+$  move  [=duct move=(wind note-arvo gift-arvo)]
::  +state-5: overall gall state, versioned
::
+$  state-6  [%6 state]
::  +state: overall gall state
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
::  +watches: subscribers and publications
::
::    TODO: rename this, to $ties?
::    TODO: rename $boat and $bitt and document
::
+$  watches  [inbound=bitt outbound=boat]
::  +routes: new cuff; TODO: document
::
+$  routes
  $:  disclosing=(unit (set ship))
      attributing=ship
  ==
::  +yoke: agent runner state
::
::    control-duct: TODO document
::    live: is this agent running? TODO document better
::    stats: TODO document
::    watches: incoming and outgoing subscription state
::    agent: agent core
::    beak: compilation source
::    marks: mark conversion configuration
::    casts: enqueueed mark conversion requests
::
+$  yoke
  $:  control-duct=duct
      live=?
      =stats
      =watches
      =agent
      =beak
      marks=(map duct mark)
      casts=(qeu [=mars:clay =vase])
  ==
:: +blocked-move: enqueued move to an agent
::
+$  blocked-move  [=duct =routes =deal]
:: +stats: statistics
::
::   change: how many moves this agent has processed
::   eny: entropy
::   time: date of current event processing
::
+$  stats  [change=@ud eny=@uvJ time=@da]
::  +ames-response: network response message (%boon)
::
::    %d: fact
::    %x: quit
::
+$  ames-response
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
+$  ames-request
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
+$  remote-request
  $?  %watch
      %watch-as
      %poke
      %leave
      %missing
  ==
--
=|  state=state-6
|=  [our=ship now=@da eny=@uvJ ski=sley]
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
    |=  [dap=term bek=beak =cage]
    ^+  mo-core
    ::
    ?.  =(%vase p.cage)
      (mo-give %onto |+[leaf+"gall: bad mark {<p.cage>} for agent {<dap>}"]~)
    ::
    =/  maybe-new-agent  (mule |.(!<(agent !<(vase q.cage))))
    ?:  ?=(%| -.maybe-new-agent)
      =/  err  [[%leaf "{<dap>}: not valid agent"] p.maybe-new-agent]
      (mo-give %onto %.n err)
    =/  =agent  p.maybe-new-agent
    ::
    ?^  existing=(~(get by yokes.state) dap)
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
  ++  mo-subscribe-to-agent-builds
    |=  date=@da
    ^+  mo-core
    =.  mo-core  (mo-abed system-duct.state)
    =/  =wire  /sys/lyv  ::  TODO: add $aeon to wire as sanity check
    =.  mo-core  (mo-pass /sys/lyv %c %warp our %home ~)
    =/  =mool:clay
      :-  da+date
      %-  ~(gas in *(set [care:clay path]))
      %+  turn  ~(tap in ~(key by yokes.state))
      |=  dap=term
      ^-  [care:clay path]
      [%a /app/[dap]/hoon]
    (mo-pass wire %c %warp our %home ~ %mult mool)
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
    ?^  p.sign-arvo
      =.  mo-core  (mo-receive-core dap beak r.u.p.sign-arvo)
      (mo-subscribe-to-agent-builds tim)
    (mo-give %onto |+[leaf+"gall: failed to build agent {<dap>}"]~)
  ::
  ::
  ++  mo-handle-sys-lyv
    |=  [=path =sign-arvo]
    ^+  mo-core
    ?>  ?=([%lyv ~] path)
    ?>  ?=([?(%b %c) %wris *] sign-arvo)
    =/  bek=beak  [our %home p.sign-arvo]
    =/  nex=(list [=care:clay =^path])  ~(tap in q.sign-arvo)
    =;  cor  (mo-subscribe-to-agent-builds:cor p.p.sign-arvo)
    %+  roll  nex
    |=  [[=care:clay =^path] cor=_mo-core]
    ^+  cor
    ?>  =(%a care)
    =/  dap  dap:;;([%app dap=@tas %hoon ~] path)
    =/  cage  (need (need (ski [%141 %noun] ~ %ca bek (flop path))))
    (mo-receive-core:cor dap bek cage)
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
    ?>  ?=([?(%b %c) %writ *] sign-arvo)
    =/  dap=term  i.t.path
    =/  =routes  [disclosing=~ attributing=our]  ::  TODO is this right?
    %+  mo-give  %unto
    ?~  p.sign-arvo
      poke-ack+`[leaf+"gall: fact cast failed for agent {<dap>}"]~
    fact+r.u.p.sign-arvo
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
      =^  remote-request  outstanding.state
        ?~  t.t.t.wire
          =/  full-wire  sys+wire
          =/  stand
            %+  ~(gut by outstanding.state)  [full-wire hen]
            ::  default is do nothing; should only hit if cleared queue
            ::  in +load 3-to-4
            ::
            (~(put to *(qeu remote-request)) %missing)
          ~|  [full-wire=full-wire hen=hen stand=stand outs=outstanding.state]
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
  ::  +mo-apply: apply the supplied action to the specified agent.
  ::
  ++  mo-apply
    |=  [dap=term =routes =deal]
    ^+  mo-core
    ::  TODO: Remove this horrific hack when ford pinto comes!
    =>  |%
        +$  serial  @uvH
        ::
        +$  letter
          $%  [%text text=cord]
              [%url url=cord]
              [%code expression=cord output=(list tank)]
              [%me narrative=cord]
          ==
        ::
        +$  envelope
          $:  uid=serial
              number=@
              author=ship
              when=time
              =letter
          ==
        ::
        +$  config
          $:  length=@
              read=@
          ==
        ::
        +$  mailbox
          $:  =config
              envelopes=(list envelope)
          ==
        ::
        +$  inbox  (map path mailbox)
        ::
        +$  chat-configs  (map path config)
        ::
        +$  chat-base
          $%  [%create =path]
              [%delete =path]
              [%message =path =envelope]
              [%read =path]
          ==
        ::
        +$  chat-action
          $%  ::  %messages: append a list of messages to mailbox
              ::
              [%messages =path envelopes=(list envelope)]
              chat-base
          ==
        ::
        +$  chat-update
          $%  [%keys keys=(set path)]
              [%config =path =config]
              [%messages =path start=@ud end=@ud envelopes=(list envelope)]
              chat-base
          ==
        --
    ::
    =/  =path  /sys/val/(scot %p attributing.routes)/[dap]
    =/  [=ship =desk]  [p q]:(mo-beak dap)
    ::
    ?:  ?=(%raw-poke -.deal)
      ::  TODO: Remove this horrific hack when ford pinto comes!
      ?+  mark.deal
        =/  =schematic:ford  [%vale ship^desk +.deal]
        =/  =note-arvo  [%f %build live=%.n schematic]
        (mo-pass path note-arvo)
      ::
          %chat-action
        =/  chat-act=(unit chat-action)  ((soft chat-action) noun.deal)
        ?~  chat-act
          ~&  gall-raw-chat-poke-failed+[dap attributing.routes]
          mo-core
        =/  =cage  [%chat-action !>(u.chat-act)]
        =/  new-deal=^deal  [%poke cage]
        =/  app  (ap-abed:ap dap routes)
        =.  app  (ap-apply:app new-deal)
        ap-abet:app
      ==
    ::
    ?:  ?=(%poke-as -.deal)
      =/  =schematic:ford  [%cast ship^desk mark.deal [%$ cage.deal]]
      =/  =note-arvo  [%f %build live=%.n schematic]
      (mo-pass path note-arvo)
    ::
    =/  app  (ap-abed:ap dap routes)
    =.  app  (ap-apply:app deal)
    ap-abet:app
  ::  +mo-handle-local: handle locally.
  ::
  ::    If the agent is running or blocked, assign it the supplied +deal.
  ::    Otherwise simply apply the action to the agent.
  ::
  ++  mo-handle-local
    |=  [=ship dap=term =deal]
    ^+  mo-core
    ::
    =/  =routes  [disclosing=~ attributing=ship]
    =/  is-running  (~(has by yokes.state) dap)
    =/  is-blocked  (~(has by blocked.state) dap)
    ::
    ?:  &(is-running !is-blocked)
      (mo-apply dap routes deal)
    ::
    %-  (slog leaf+"gall: not running {<dap>} yet, got {<-.deal>}" ~)
    =.  blocked.state
      %+  ~(put by blocked.state)  dap
      ^-  (qeu blocked-move)
      %.  [hen routes deal]
      ~(put to (~(gut by blocked.state) dap *(qeu blocked-move)))
    mo-core
  ::  +mo-handle-ames-request: handle %ames request message.
  ::
  ++  mo-handle-ames-request
    |=  [=ship dap=term =ames-request]
    ^+  mo-core
    ::  %u/%leave gets automatically acked
    ::
    =.  mo-core  (mo-track-ship ship)
    =?  mo-core  ?=(%u -.ames-request)  (mo-give %done ~)
    ::
    =/  =wire  /sys/req/(scot %p ship)/[dap]
    ::
    =/  =deal
      ?-  -.ames-request
        %m  [%raw-poke [mark noun]:ames-request]
        %l  [%watch-as [mark path]:ames-request]
        %s  [%watch path.ames-request]
        %u  [%leave ~]
      ==
    (mo-pass wire %g %deal [ship our] dap deal)
  ::  +mo-handle-ames-response: handle ames response message.
  ::
  ++  mo-handle-ames-response
    |=  =ames-response
    ^+  mo-core
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
    |_  $:  ap-name=term
            ap-routes=routes
            ap-duct=duct
            ap-moves=(list move)
            ap-config=(list (each suss tang))
            ap-yoke=yoke
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
      ::
      =/  =yoke
        =/  running  (~(got by yokes.state) dap)
        =/  =stats
          :+  +(change.stats.running)
            (shaz (mix (add dap change.stats.running) eny))
          now
        running(stats stats)
      ::
      =.  ap-name  dap
      =.  ap-routes  routes
      =.  ap-yoke  yoke
      =.  ap-duct  hen
      ap-core
    ::  +ap-abet: resolve moves.
    ::
    ++  ap-abet
      ^+  mo-core
      ::
      =/  running  (~(put by yokes.state) ap-name ap-yoke)
      =/  moves
        =/  giver  |=(report=(each suss tang) [hen %give %onto report])
        =/  from-suss  (turn ap-config giver)
        :(weld ap-moves from-suss moves)
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
      ^-  [(list move) _ap-core]
      ::
      ?-    -.card
          %slip  !!
      ::
          %give
        =/  =gift:agent  p.card
        ?:  ?=(%kick -.gift)
          :_  ap-core
          %+  turn  (ap-ducts-from-paths paths.gift ship.gift)
          |=  =duct
          ^-  move
          ~?  &(=(duct system-duct.state) !=(ap-name %hood))
            [%agent-giving-on-system-duct ap-name -.gift]
          [duct %give %unto %kick ~]
        ::
        ?.  ?=(%fact -.gift)
          :_(ap-core [ap-duct %give %unto gift]~)
        ::
        =/  dux=(list duct)  (ap-ducts-from-paths paths.gift ~)
        =/  =cage  cage.gift
        =|  fex=(list move)
        |-  ^+  [fex ap-core]
        ?~  dux
          [fex ap-core]
        ~?  &(=(i.dux system-duct.state) !=(ap-name %hood))
          [%agent-giving-on-system-duct ap-name -.gift]
        =/  =mark  (~(gut by marks.ap-yoke) i.dux p.cage)
        ?:  =(mark p.cage)
          $(dux t.dux, fex :_(fex [i.dux %give %unto %fact cage]))
        =/  =mars:clay  [p.cage mark]
        =.  casts.ap-yoke  (~(put to casts.ap-yoke) [mars q.cage])
        =/  =move
          =/  =wire  /sys/pel/[ap-name]
          =/  [=ship =desk =case:clay]  (mo-beak ap-name)
          =/  =note-arvo
            [%c %warp ship desk ~ %sing %c case /[a.mars]/[b.mars]]
          [i.dux %pass wire note-arvo]
        $(dux t.dux, fex [move fex])
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
          [(scot %p attributing.ap-routes) wire]
        =.  wire
          [%use ap-name wire]
        =/  =note-arvo
          ?-  -.neat
            %arvo   note-arvo.neat
            %agent  [%g %deal [our ship.neat] [name deal]:neat]
          ==
        :_(ap-core [duct %pass wire note-arvo]~)
      ==
    ::  +ap-breach: ship breached, so forget about them
    ::
    ++  ap-breach
      |=  =ship
      ^+  ap-core
      =/  in=(list [=duct =^ship =path])
        ~(tap by inbound.watches.ap-yoke)
      |-  ^+  ap-core
      ?^  in
        =?  ap-core  =(ship ship.i.in)
          =/  core  ap-load-delete(ap-duct duct.i.in)
          core(ap-duct ap-duct)
        $(in t.in)
      ::
      =/  out=(list [[=wire =^ship =term] ? =path])
        ~(tap by outbound.watches.ap-yoke)
      |-  ^+  ap-core
      ?~  out
        ap-core
      =?  ap-core  =(ship ship.i.out)
        =/  core
          =.  ap-duct  system-duct.state
          =/  way  [%out (scot %p ship) term.i.out wire.i.out]
          (ap-specific-take way %kick ~)
        core(ap-duct ap-duct)
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
        ~(tap by inbound.watches.ap-yoke)
      |-  ^+  ap-core
      ?~  in  ap-core
      ::
      =?  ap-core  =(ship ship.i.in)
        =/  core  ap-kill-up(ap-duct duct.i.in)
        core(ap-duct ap-duct)
      $(in t.in)
    ::  +ap-agent-core: agent core with current bowl and state
    ::
    ++  ap-agent-core
      ~(. agent.ap-yoke ap-construct-bowl)
    ::  +ap-ducts-from-paths: get ducts subscribed to paths
    ::
    ++  ap-ducts-from-paths
      |=  [target-paths=(list path) target-ship=(unit ship)]
      ^-  (list duct)
      ?:  &(?=(~ target-paths) ?=(~ target-ship))
        ~[ap-duct]
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
        ~[ap-duct]
      %+  murn  ~(tap by inbound.watches.ap-yoke)
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
      ::  strip trailing mark off path for %x scrys
      ::
      =?  tyl  ?=(%x care)  (flop (tail (flop tyl)))
      =/  peek-result=(each (unit (unit cage)) tang)
        (ap-mule-peek |.((on-peek:ap-agent-core [care tyl])))
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
      ?:  is-ok
        ap-core
      (ap-kill-down wire [other-ship other-agent])
    ::  +ap-give: return result.
    ::
    ++  ap-give
      |=  =gift:agent
      ^+  ap-core
      =^  internal-moves  ap-core  (ap-from-internal %give gift)
      ap-core(ap-moves (weld internal-moves ap-moves))
    ::  +ap-pass: request action.
    ::
    ++  ap-pass
      |=  [=path =neat]
      ^+  ap-core
      =^  internal-moves  ap-core  (ap-from-internal %pass path neat)
      ap-core(ap-moves (weld internal-moves ap-moves))
    ::  +ap-construct-bowl: set up bowl.
    ::
    ++  ap-construct-bowl
      ^-  bowl
      :*  :*  our                                     ::  host
              attributing.ap-routes                ::  guest
              ap-name                              ::  agent
          ==                                          ::
          :*  wex=outbound.watches.ap-yoke  ::  outgoing
              sup=inbound.watches.ap-yoke  ::  incoming
          ==                                          ::
          :*  act=change.stats.ap-yoke          ::  tick
              eny=eny.stats.ap-yoke             ::  nonce
              now=time.stats.ap-yoke            ::  time
              byk=beak.ap-yoke                  ::  source
      ==  ==
    ::  +ap-reinstall: reinstall.
    ::
    ++  ap-reinstall
      ~/  %ap-reinstall
      |=  =agent
      ^+  ap-core
      =/  old-state=vase  ~(on-save agent.ap-yoke ap-construct-bowl)
      =^  error  ap-core
        (ap-install(agent.ap-yoke agent) `old-state)
      ?~  error
        ap-core
      (ap-error %load-failed u.error)
    ::  +ap-subscribe-as: apply %watch-as.
    ::
    ++  ap-subscribe-as
      |=  [=mark =path]
      ^+  ap-core
      =.  marks.ap-yoke  (~(put by marks.ap-yoke) ap-duct mark)
      (ap-subscribe path)
    ::  +ap-subscribe: apply %watch.
    ::
    ++  ap-subscribe
      ~/  %ap-subscribe
      |=  pax=path
      ^+  ap-core
      =/  incoming  [attributing.ap-routes pax]
      =.  inbound.watches.ap-yoke
        (~(put by inbound.watches.ap-yoke) ap-duct incoming)
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
      =?  outbound.watches.ap-yoke  ?=(%kick -.sign)
        %-  ~(del by outbound.watches.ap-yoke)
        [agent-wire dock]
      ?:  ?&  ?=(%watch-ack -.sign)
              !(~(has by outbound.watches.ap-yoke) [agent-wire dock])
          ==
        %-  %:  slog
              leaf+"{<ap-name>}: got ack for nonexistent subscription"
              leaf+"{<dock>}: {<agent-wire>}"
              >wire=wire<
              >out=outbound.watches.ap-yoke<
              ~
            ==
        ap-core
      ::
      =?  outbound.watches.ap-yoke  ?=(%watch-ack -.sign)
        ?^  p.sign
          %-  ~(del by outbound.watches.ap-yoke)
          [agent-wire dock]
        %+  ~(jab by outbound.watches.ap-yoke)  [agent-wire dock]
        |=  [acked=? =path]
        =.  .
          ?.  acked
            .
          %-  =/  =tape
                "{<ap-name>}: received 2nd watch-ack on {<wire dock path>}"
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
      =.  ap-config
        =/  =term  ?~(old-agent-state %boot %bump)
        =/  possibly-suss
          ?~  maybe-tang
            =/  =suss  [ap-name term now]
            [%.y suss]
          [%.n u.maybe-tang]
        [possibly-suss ap-config]
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
          inbound.watches.ap-yoke
        (~(del by inbound.watches.ap-yoke) ap-duct)
      ==
    ::  +ap-load-delete: load delete.
    ::
    ++  ap-load-delete
      ^+  ap-core
      ::
      =/  maybe-incoming
        (~(get by inbound.watches.ap-yoke) ap-duct)
      ?~  maybe-incoming
        ap-core
      ::
      =/  incoming  u.maybe-incoming
      =.  inbound.watches.ap-yoke
        (~(del by inbound.watches.ap-yoke) ap-duct)
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
      =^  ack-moves=(list move)  ap-core
        ?-  ack
          ~           [~ ap-core]
          %poke-ack   (ap-from-internal %give %poke-ack maybe-tang)
          %watch-ack  (ap-from-internal %give %watch-ack maybe-tang)
        ==
      =.  ap-moves  :(weld (flop new-moves) ack-moves ap-moves)
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
      =^  caz=(list card:agent)  agent.ap-yoke  p.result
      =^  moves  ap-core
        =|  fex=(list move)
        |-  ^+  [fex ap-core]
        ?~  caz  [fex ap-core]
        =^  fax  ap-core  (ap-from-internal i.caz)
        =.  fex  (weld fex fax)
        $(caz t.caz)
      ::
      =.  inbound.watches.ap-yoke
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
      (~(dif by inbound.watches.ap-yoke) quit-map)
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
        =.  outbound.watches.ap-yoke
          (~(del by outbound.watches.ap-yoke) [short-wire dock])
        $(moves t.moves, new-moves [move new-moves])
      ?.  ?=([* %pass * %g %deal * * %watch *] move)
        $(moves t.moves, new-moves [move new-moves])
      =/  =wire  p.move.move
      ?>  ?=([%use @ @ %out @ @ *] wire)
      =/  short-wire  t.t.t.t.t.t.wire
      =/  =dock  [q.p q]:q.move.move
      =/  =path  path.r.q.move.move
      ?:  (~(has by outbound.watches.ap-yoke) short-wire dock)
        =.  ap-core
          =/  =tang
            ~[leaf+"subscribe wire not unique" >ap-name< >short-wire< >dock<]
          %-  (slog >out=outbound.watches.ap-yoke< tang)
          (ap-error %watch-not-unique tang)
        $(moves t.moves)
      =.  outbound.watches.ap-yoke
        (~(put by outbound.watches.ap-yoke) [short-wire dock] [| path])
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
    =/  ap-name  i.t.path
    ::
    =/  =ames-request  ;;(ames-request noun)
    =>  (mo-handle-ames-request:mo-core ship ap-name ames-request)
    mo-abet
  ::
      %sear  mo-abet:(mo-filter-queue:mo-core ship.task)
      %trim  [~ gall-payload]
      %vega  [~ gall-payload]
      %wegh
    =/  blocked
      =/  queued  (~(run by blocked.state) |=((qeu blocked-move) [%.y +<]))
      (sort ~(tap by queued) aor)
    ::
    =/  running
      =/  active  (~(run by yokes.state) |=(yoke [%.y +<]))
      (sort ~(tap by active) aor)
    ::
    =/  =mass
      :+  %gall  %.n
      :~  [%foreign %.y contacts.state]
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
  =?  all-state  ?=(%4 -.all-state)
    (state-4-to-5 all-state)
  ::
  =?  all-state  ?=(%5 -.all-state)
    (state-5-to-6 all-state)
  ::
  ?>  ?=(%6 -.all-state)
  gall-payload(state all-state)
  ::
  ::  +all-state: upgrade path
  ::
  ++  all-state
    $%(state-0 state-1 state-2 state-3 state-4 state-5 state-6)
  ::
  ++  state-5-to-6
    |=  =state-5
    ^-  state-6
    %=  state-5
        -  %6
        yokes.agents-5
      %-  ~(run by yokes.agents-5.state-5)
      |=  yoke-5
      ^-  yoke
      :*  control-duct  live  stats  watches  agent  beak  marks
          casts=~
      ==
    ==
  ::
  ++  state-5
    $:  %5
        =agents-5
    ==
  ::
  ++  agents-5
    $:  system-duct=duct
        outstanding=(map [wire duct] (qeu remote-request))
        contacts=(set ship)
        yokes=(map term yoke-5)
        blocked=(map term (qeu blocked-move))
    ==
  ::
  ++  yoke-5
    $:  control-duct=duct
        live=?
        =stats
        =watches
        =agent
        =beak
        marks=(map duct mark)
    ==
  ::
  ++  state-4-to-5
    |=  =state-4
    ^-  state-5
    %=    state-4
        -  %5
        running.agents-4
      (~(run by running.agents-4.state-4) |=(yoke-3 +<+))
    ==
  ::
  ++  state-4
    $:  %4
        agents-4=agents-3  ::  agents-3 is unchanged in state-4
    ==
  ::
  ++  state-3-to-4
    |=  =state-3
    ^-  state-4
    %=    state-3
        -  %4
        outstanding.agents-3  ~
    ==
  ::
  ++  state-3
    $:  %3
        =agents-3
    ==
  ::
  ++  agents-3
    $:  system-duct=duct
        outstanding=(map [wire duct] (qeu remote-request))
        contacts=(set ship)
        running=(map term yoke-3)
        blocked=(map term (qeu blocked-move))
    ==
  ::
  ++  yoke-3
    $:  cache=worm
        control-duct=duct
        live=?
        =stats
        =watches
        =agent
        =beak
        marks=(map duct mark)
    ==
  ::
  ++  state-2-to-3
    |=  =state-2
    ^-  state-3
    %=    state-2
        -  %3
        running.agents-2
      %-  ~(run by running.agents-2.state-2)
      |=  =yoke-2
      ^-  yoke-3
      %=  yoke-2
        agent-2  (agent-2-to-3 agent-2.yoke-2)
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
        running=(map term yoke-2)
        blocked=(map term (qeu blocked-move))
    ==
  ::
  ++  yoke-2
    $:  cache=worm
        control-duct=duct
        live=?
        =stats
        =watches
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
        running=(map term yoke-2)
        blocked=(map term (qeu blocked-move))
    ==
  ::
  ++  state-0-to-1
    |=  =state-0
    ^-  state-1
    %=    state-0
        -  %1
        running.agents-0
      %-  ~(run by running.agents-0.state-0)
      |=  =yoke-0
      ^-  yoke-2
      %=  yoke-0
        agent-0  (agent-0-to-1 agent-0.yoke-0)
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
        running=(map term yoke-0)
        blocked=(map term (qeu blocked-move))
    ==
  ::
  ++  yoke-0
    $:  cache=worm
        control-duct=duct
        live=?
        =stats
        =watches
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
                $>(%wris gift:able:clay)
                $>(%writ gift:able:clay)
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
  |=  [fur=(unit (set monk)) care=term =shop dap=desk =coin =path]
  ^-  (unit (unit cage))
  ?.  ?=(%.y -.shop)
    ~
  =/  =ship  p.shop
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
++  stay  state
::  +take: response
::
++  take
  ~/  %gall-take
  |=  [=wire =duct dud=(unit goof) hin=(hypo sign-arvo)]
  ^-  [(list move) _gall-payload]
  ?^  dud
    ~&(%gall-take-dud ((slog tang.u.dud) [~ gall-payload]))
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
