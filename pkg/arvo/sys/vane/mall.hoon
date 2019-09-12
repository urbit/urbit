!:  ::  %mall, agent execution
!?  163
!:
::::
|=  pit=vase
=,  mall
=>  =~
|%
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
  $?  %subscribe
      %subscribe-translated
      %poke
      %unsubscribe
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
      meter=(map duct @ud)
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
++  blocked  (qeu (trel duct routes task:agent))
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
  ::  +mo-new-agent: create a new agent and add it to %mall's state.
  ::
  ::    %mall maintains a collection of running agents.  This arm creates a
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
  ::  +mo-handle-foreign-request: handle a foreign request.
  ::
  ::    Handles tasks received on a +call that have come from another ship.
  ::
  ++  mo-handle-foreign-request
    ~/  %mo-handle-foreign-request
    |=  [=ship =term =task:agent]
    ^+  mo-core
    ::
    ?:  ?=(%pump -.task)
      ::
      ::  you'd think this would send an ack for the diff that caused
      ::  this pump.  it would, but we already sent it when we got the
      ::  diff in +mo-handle-sys.  then we'd have to save the network
      ::  duct and connect it to this returning pump.
      ::
      mo-core
    ::
    =^  bone  mo-core  (mo-assign-bone ship)
    =/  =forward-ames
      ?-  -.task
        %poke                  [%m p.cage.task q.q.cage.task]
        %unsubscribe           [%u ~]
        %raw-poke              !!
        %poke-translated       !!
        %subscribe-translated  [%l task]
        %subscribe             [%s path.task]
      ==
    ::
    =/  sys-path
      =/  action  -.task
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
      %subscribe-translated  (mo-give %unto %subscription-ack result)
      %subscribe             (mo-give %unto %subscription-ack result)
      %poke                  (mo-give %unto %poke-ack result)
      %unsubscribe           mo-core
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
  ::    Validates a received %ford result and %gives an internal
  ::    %subscription-update.
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
      (mo-give %unto %poke-ack err)
    ::
    =/  build-result  build-result.result.sign-arvo
    ::
    ?:  ?=([%error *] build-result)
      =/  err  (some message.build-result)
      (mo-give %unto %poke-ack err)
    ::
    =/  =cage  (result-to-cage:ford build-result)
    (mo-give %unto %subscription-update ~ cage)
  ::  +mo-handle-sys-red: diff ack.
  ::
  ::    On receipt of a valid +sign from %ames, we simply pass a %pump
  ::    acknowledgement internally; otherwise we pass both an internal
  ::    unsubscribing %unsubscribe, plus a %want to %ames, before
  ::    complaining about a bad message acknowledgment.
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
        =/  =task:agent  [%pump ~]
        =/  =task:able  [%deal sock dap task]
        [%m task]
      (mo-pass sys-path note-arvo)
    ::
    =/  mall-move=note-arvo
      =/  =sock  [him our]
      =/  =task:agent  [%unsubscribe ~]
      =/  =task:able  [%deal sock dap task]
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
  ::    On receipt of a valid +sign from %ford, sets state to the
  ::    appropriate duct and gives an internal %subscription-update
  ::    containing the +sign payload.
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
    =/  =gift:able  [%unto [%subscription-update ~ cage]]
    (mo-give gift)
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
    =/  =gift:agent  +>.sign-arvo
    ::
    ?-    -.gift
        %poke-ack
      (mo-give %mack p.gift)
    ::
        %subscription-update
      =/  sys-path  [%sys %red t.path]
      =/  =note-arvo
        =/  path  [%m %gh dap ~]
        =/  noun  [num %d p.cage.gift q.q.cage.gift]
        [%a %want him path noun]
      (mo-pass sys-path note-arvo)
    ::
        %subscription-close
      =/  sys-path  [%sys path]
      =/  =note-arvo
        =/  path  [%m %gh dap ~]
        =/  noun  [num %x ~]
        [%a %want him path noun]
      (mo-pass sys-path note-arvo)
    ::
        %subscription-ack
      (mo-give %mack p.gift)
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
      (mo-give %unto %poke-ack err)
    ::
    =/  build-result  build-result.result.sign-arvo
    ?:  ?=([%error *] build-result)
      =/  err  (some message.build-result)
      (mo-give %unto %poke-ack err)
    ::
    =/  =routes  [disclosing=~ attributing=ship]
    =/  =cage  (result-to-cage:ford build-result)
    =/  =task:agent  [%poke cage]
    (mo-apply term routes task)
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
    ?.  ?=([@ @ *] path)
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
    ?.  ?=([%m %unto *] sign-arvo)
      =.  app  (ap-generic-take:app t.t.path sign-arvo)
      ap-abet:app
    =.  app
      =/  =gift:agent  +>.sign-arvo
      (ap-specific-take:app t.t.path gift)
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
    =/  =task:agent  r.task
    ::
    =/  move
      =/  =sock  [attributing.routes our]
      =/  card  [%slip %m %deal sock term task]
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
    |=  [=term =routes =task:agent]
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
    ?:  ?=(%raw-poke -.task)
      =/  =schematic:ford  [%vale ship-desk +.task]
      =/  =note-arvo  [%f %build live=%.n schematic]
      (mo-pass path note-arvo)
    ::
    ?:  ?=(%poke-translated -.task)
      =/  =schematic:ford  [%cast ship-desk mark.task [%$ cage.task]]
      =/  =note-arvo  [%f %build live=%.n schematic]
      (mo-pass path note-arvo)
    ::
    =/  app  (ap-abed:ap term routes)
    =.  app  (ap-apply:app task)
    ap-abet:app
  ::  +mo-handle-local: handle locally.
  ::
  ::    If the agent is running or blocked, assign it the supplied +task.
  ::    Otherwise simply apply the action to the agent.
  ::
  ++  mo-handle-local
    |=  [=ship =term =task:agent]
    ^+  mo-core
    ::
    =/  =routes  [disclosing=~ attributing=ship]
    =/  is-running  (~(has by running.agents.state) term)
    =/  is-blocked  (~(has by blocked.agents.state) term)
    ::
    ?:  |(!is-running is-blocked)
      =/  =blocked
        =/  waiting  (~(get by blocked.agents.state) term)
        =/  tasks  (fall waiting *blocked)
        =/  task  [hen routes task]
        (~(put to tasks) task)
      ::
      %_  mo-core
        blocked.agents.state  (~(put by blocked.agents.state) term blocked)
      ==
    (mo-apply term routes task)
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
          =/  =task:agent  [%raw-poke [mark noun]:forward-ames]
          [%deal sock term task]
        [%m task]
      ::
          %l
        =/  =task:able
          =/  =task:agent  [%subscribe-translated [mark path]:forward-ames]
          [%deal sock term task]
        [%m task]
      ::
          %s
        =/  =task:able
          =/  =task:agent  [%subscribe path.forward-ames]
          [%deal sock term task]
        [%m task]
      ::
          %u
        =/  =task:able
          =/  =task:agent  [%unsubscribe ~]
          [%deal sock term task]
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
      (mo-give:initialised %unto %subscription-close ~)
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
            agent-duct=duct
            agent-cards=(list card:agent)
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
      =.  agent-duct  hen
      ap-core
    ::  +ap-abet: resolve moves.
    ::
    ++  ap-abet
      ^+  mo-core
      ::
      =>  ap-track-queue
      =/  running  (~(put by running.agents.state) agent-name current-agent)
      =/  moves
        =/  giver  |=(report=(each suss tang) [hen %give %onto report])
        =/  from-internal=(list move)
          (zing (turn agent-cards ap-from-internal))
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
      =/  internal-cards  agent-cards
      =/  bad-ducts  *(set duct)
      |-  ^+  ap-core
      ?^  internal-cards
        =/  =card:agent  i.internal-cards
        ?.  ?=([%give %subscription-update *] card)
          $(internal-cards t.internal-cards)
        ::
        =/  ducts  (ap-ducts-from-path path.p.card)
        |-  ^+  ap-core
        ?~  ducts
          ^$(internal-cards t.internal-cards)
        =^  filled  ap-core  ap-enqueue(agent-duct i.ducts)
        =.  bad-ducts
          ?:  filled
            bad-ducts
          (~(put in bad-ducts) i.ducts)
        $(ducts t.ducts)
      ::
      =/  ducts  ~(tap in bad-ducts)
      ::
      |-  ^+  ap-core
      ?~  ducts
        ap-core
      ::
      =>  $(ducts t.ducts, agent-duct i.ducts)
      =/  incoming
        (~(get by incoming.subscribers.current-agent) agent-duct)
      ?~  incoming
        ~&  [%ap-track-queue-bad-duct agent-name agent-duct]
        ap-core
      ::
      =/  =ship  p.u.incoming
      ap-kill(attributing.agent-routes ship)
    ::  +ap-from-internal: internal move to move.
    ::
    ::    We convert from cards to duct-indexed moves when resolving
    ::    them in Arvo.
    ::
    ++  ap-from-internal
      ~/  %ap-from-internal
      |=  =card:agent
      ^-  (list move)
      ::
      ?-    -.card
          %slip  !!
      ::
          %give
        =/  =gift:agent  p.card
        ?.  ?=(%subscription-update -.gift)
          [agent-duct %give %unto gift]~
        ::
        =/  ducts=(list duct)  (ap-ducts-from-path path.gift)
        =/  =cage  cage.gift
        %+  turn  ducts
        |=  =duct
        ~?  =(duct system-duct.agents.state)
          [%agent-giving-on-system-duct agent-name -.gift]
        ^-  move
        =/  =mark
          (~(gut by marks.current-agent) duct p.cage)
        ::
        ?:  =(mark p.cage)
          [duct %give %unto gift]
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
        =/  =path  p.card
        =/  =note:agent  q.card
        =/  use-path
          ::  Is it bad that this includes attributing ship?  May create
          ::  spurious duct mismatches
          ::
          [%use agent-name (scot %p attributing.agent-routes) path]
        =/  =note-arvo
          ?-    -.note
              %arvo  note-arvo.note
              %agent
            =/  =task:able
              =/  =sock  [our ship.note]
              [%deal sock [name task]:note]
            [%m task]
          ==
        [duct %pass use-path note-arvo]~
      ==
    ::  +ap-agent-core: agent core with current bowl and state
    ::
    ++  ap-agent-core
      ~(. agent.current-agent ap-construct-bowl)
    ::  +ap-ducts-from-path: get ducts subscribed to path
    ::
    ++  ap-ducts-from-path
      |=  target-path=(unit path)
      ^-  (list duct)
      ?~  target-path
        ~[agent-duct]
      %+  murn  ~(tap by incoming.subscribers.current-agent)
      |=  [=duct =ship =path]
      ^-  (unit ^duct)
      ?:  =(target-path path)
        `duct
      ~
    ::  +ap-apply: apply effect.
    ::
    ++  ap-apply
      |=  =task:agent
      ^+  ap-core
      ::
      ?-  -.task
        %subscribe-translated  (ap-subscribe-translated +.task)
        %poke                  (ap-poke +.task)
        %subscribe             (ap-subscribe +.task)
        %raw-poke              !!
        %poke-translated       !!
        %unsubscribe           ap-load-delete
        %pump                  ap-dequeue
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
        =/  =note:agent  [%agent ship -.path %pump ~]
        (ap-pass way note)
      =.  ap-core  (ap-give %subscription-close ~)
      =/  =note:agent  [%agent ship -.path %unsubscribe ~]
      (ap-pass way note)
    ::  +ap-dequeue: drop from queue.
    ::
    ::    Dequeues along the current duct, deleting the queue entirely if it
    ::    drops to zero.
    ::
    ++  ap-dequeue
      ^+  ap-core
      ::
      ?.  (~(has by incoming.subscribers.current-agent) agent-duct)
        ap-core
      =/  level  (~(get by meter.subscribers.current-agent) agent-duct)
      ?:  |(?=(~ level) =(0 u.level))
        ap-core
      ::
      =.  u.level  (dec u.level)
      ?:  =(0 u.level)
        =/  deleted  (~(del by meter.subscribers.current-agent) agent-duct)
        ap-core(meter.subscribers.current-agent deleted)
      ::
      =/  dequeued
        (~(put by meter.subscribers.current-agent) agent-duct u.level)
      ap-core(meter.subscribers.current-agent dequeued)
    ::  +ap-enqueue: add to queue.
    ::
    ::    Every agent has a 'meter', that tracks the number of incoming
    ::    subscribers by duct.  We get both the meter and ship associated with
    ::    the current duct; if the meter has hit twenty for another ship, we
    ::    don't enqueue the subscriber.  Otherwise we increment the meter for
    ::    the current duct and update the agent's state with it.
    ::
    ::    Returns a yes if the meter has been incremented, and no otherwise.
    ::
    ++  ap-enqueue
      ^-  [? _ap-core]
      ::
      =/  meter  (~(gut by meter.subscribers.current-agent) agent-duct 0)
      =/  subscriber=(unit (pair ship path))
        (~(get by incoming.subscribers.current-agent) agent-duct)
      ::
      ?:  ?&  =(20 meter)
              ?|  ?=(~ subscriber)
                  !=(our p.u.subscriber)
              ==
          ==
        =/  incoming  (~(get by incoming.subscribers.current-agent) agent-duct)
        ~&  [%mall-pulling-20 agent-duct incoming]
        [%.n ap-core]
      ::
      =/  next
        =/  meter
          (~(put by meter.subscribers.current-agent) agent-duct +(meter))
        ap-core(meter.subscribers.current-agent meter)
      ::
      [%.y next]
    ::  +ap-give: return result.
    ::
    ++  ap-give
      |=  =gift:agent
      ^+  ap-core
      ::
      =/  internal-cards
        =/  =card:agent  [%give gift]
        [card agent-cards]
      ap-core(agent-cards internal-cards)
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
          :*  act=change.stats.current-agent          ::  tick
              eny=eny.stats.current-agent             ::  nonce
              now=time.stats.current-agent            ::  time
              byk=beak.current-agent                  ::  source
      ==  ==
    ::  +ap-pass: request action.
    ::
    ++  ap-pass
      |=  [=path =note:agent]
      ^+  ap-core
      ::
      =/  =card:agent  [%pass path note]
      =/  internal-cards  [card agent-cards]
      ap-core(agent-cards internal-cards)
    ::  +ap-reinstall: reinstall.
    ::
    ++  ap-reinstall
      ~/  %ap-reinstall
      |=  =vase
      ^+  ap-core
      ::
      =/  maybe-agent  !<(agent vase)
      ?~  maybe-agent
        (ap-error %new-core-not-agent ~)
      ::
      =/  prep
        =/  =agent  u.maybe-agent
        =/  running
          %-  some
          ~(handle-extract-state agent.current-agent ap-construct-bowl)
        =/  installed  ap-install(agent.current-agent agent)
        (installed running)
      ::
      =^  maybe-tang  ap-core  prep
      ?~  maybe-tang
        ap-core
      (ap-error %prep-failed u.maybe-tang)
    ::  +ap-subscribe-translated: apply %subscribe-translated.
    ::
    ++  ap-subscribe-translated
      |=  [=mark =path]
      ^+  ap-core
      ::
      =.  marks.current-agent  (~(put by marks.current-agent) agent-duct mark)
      (ap-subscribe path)
    ::  +ap-subscribe: apply %subscribe.
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
        %+  ap-ingest  %subscription-ack  |.
        (handle-subscribe:ap-agent-core pax)
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
        (handle-poke:ap-agent-core cage)
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
        (handle-error:ap-agent-core term (turn tang form))
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
        (handle-arvo-response:ap-agent-core wire sign-arvo)
      ?^  maybe-tang
        (ap-error %arvo-response u.maybe-tang)
      ap-core
    ::  +ap-specific-take: specific take.
    ::
    ++  ap-specific-take
      |=  [=path =gift:agent]
      ^+  ap-core
      ::
      =^  maybe-tang  ap-core
        %+  ap-ingest  ~  |.
        (handle-agent-response:ap-agent-core path gift)
      ?:  ?=(%subscription-update -.gift)
        (ap-update-subscription =(~ maybe-tang) attributing.agent-routes path)
      ?^  maybe-tang
        (ap-error -.gift u.maybe-tang)
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
          |.  handle-init:ap-agent-core
        |.  (handle-upgrade-state:ap-agent-core u.maybe-vase)
      [maybe-tang ap-core]
    ::  +ap-silent-delete: silent delete.
    ::
    ++  ap-silent-delete
      ^+  ap-core
      ::
      ?~  (~(get by incoming.subscribers.current-agent) agent-duct)
        ap-core
      ::
      =/  incoming  (~(del by incoming.subscribers.current-agent) agent-duct)
      =/  meter  (~(del by meter.subscribers.current-agent) agent-duct)
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
        (~(get by incoming.subscribers.current-agent) agent-duct)
      ?~  maybe-incoming
        ap-core
      ::
      =/  incoming  u.maybe-incoming
      =.  incoming.subscribers.current-agent
        (~(del by incoming.subscribers.current-agent) agent-duct)
      =.  meter.subscribers.current-agent
        (~(del by meter.subscribers.current-agent) agent-duct)
      ::
      =^  maybe-tang  ap-core
        %+  ap-ingest  ~  |.
        (handle-unsubscribe:ap-agent-core q.incoming)
      ?^  maybe-tang
        (ap-error %unsubscribe u.maybe-tang)
      ap-core
    ::  +ap-kill: queue kill.
    ::
    ++  ap-kill
      ^+  ap-core
      ::
      =>  ap-load-delete
      (ap-give %subscription-close ~)
    ::  +ap-ingest: call agent arm
    ::
    ::    Handle acks here because they need to be emitted before the
    ::    rest of the moves.
    ::
    ++  ap-ingest
      |=  [ack=?(%poke-ack %subscription-ack ~) run=_^?(|.(*step:agent))]
      ^-  [(unit tang) _ap-core]
      =/  result  (mule run)
      =^  new-cards  ap-core  (ap-handle-result result)
      =/  maybe-tang=(unit tang)
        ?:  ?=(%& -.result)
          ~
        `p.result
      =/  ack-cards
        ?-  ack
          ~      ~
          %poke-ack          [%give %poke-ack maybe-tang]~
          %subscription-ack  [%give %subscription-ack maybe-tang]~
        ==
      ::
      =.  agent-cards 
        :(weld new-cards ack-cards agent-cards)
      [maybe-tang ap-core]
    ::  +ap-handle-result: handle result.
    ::
    ++  ap-handle-result
      ~/  %ap-handle-result
      |=  result=(each step:agent tang)
      ^-  [(list card:agent) _ap-core]
      ?:  ?=(%| -.result)
        `ap-core
      ::
      =/  new-subs  (ap-handle-quits -.p.result)
      ::
      :-  (flop -.p.result)
      %_  ap-core
        agent.current-agent                 +.p.result
        incoming.subscribers.current-agent  new-subs
      ==
    ::  +ap-handle-quits: handle cancels of incoming subscriptions
    ::
    ++  ap-handle-quits
      ~/  %ap-handle-quits
      |=  moves=(list card:agent)
      ^-  bitt
      =/  quits=(list duct)
        %-  zing
        ^-  (list (list duct))
        %+  turn  moves
        |=  =card:agent
        ^-  (list duct)
        ?.  ?=([%give %subscription-close *] card)
          ~
        (ap-ducts-from-path path.p.card)
      ::
      =/  quit-map=bitt
        (malt (turn quits |=(=duct [duct *[ship path]])))
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
      ?(%conf %conf-mall)
    =/  =dock  p.task
    =/  =ship  p.dock
    ?.  =(our ship)
      ~&  [%mall-not-ours ship]
      [~ mall-payload]
    ::
    =>  (mo-boot:initialised q.dock q.task)
    mo-abet
  ::
      ?(%deal %deal-mall)
    =/  =sock  p.task
    =/  =term  q.task
    =/  =task:agent  r.task
    ?.  =(q.sock our)
      ?>  =(p.sock our)
      =>  (mo-handle-foreign-request:initialised q.sock term task)
      mo-abet
    ::
    =>  (mo-handle-local:initialised p.sock term task)
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
  ::  |=  *
  ::  mall-payload
  |=  =state-old
  ^+  mall-payload
  ::
  ?-  -.state-old
    %0  mall-payload(state state-old)
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
