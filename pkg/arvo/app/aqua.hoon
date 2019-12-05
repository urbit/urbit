::  An aquarium of virtual ships.  Put in some fish and watch them!
::
::  usage:
::  |start %aqua
::  /-  aquarium
::  :aqua &pill .^(pill:aquarium %cx %/urbit/pill)
::    OR
::  :aqua &pill +solid
::
::  Then try stuff:
::  :aqua [%init ~[~bud ~dev]]
::  :aqua [%dojo ~[~bud ~dev] "[our eny (add 3 5)]"]
::  :aqua [%dojo ~[~bud] "|hi ~dev"]
::  :aqua [%wish ~[~bud ~dev] '(add 2 3)']
::  :aqua [%peek ~[~bud] /cx/~bud/home/(scot %da now)/app/curl/hoon]
::  :aqua [%dojo ~[~bud ~dev] '|mount %']
::  :aqua [%file ~[~bud ~dev] %/sys/vane]
::  :aqua [%pause-events ~[~bud ~dev]]
::
::
::  We get ++unix-event and ++pill from /-aquarium
::
/-  aquarium
/+  pill, default-agent
=,  pill-lib=pill
=,  aquarium
=>  $~  |%
    +$  state
      $:  %0
          pil=pill
          assembled=*
          tym=@da
          fleet-snaps=(map term (map ship pier))
          piers=(map ship pier)
      ==
    ::
    +$  pier
      $:  snap=*
          event-log=(list unix-timed-event)
          next-events=(qeu unix-event)
          processing-events=?
      ==
    --
::
=|  state
=*  all-state  -
=<
  ^-  agent:gall
  |_  =bowl:gall
  +*  this       .
      aqua-core  +>
      ac         ~(. aqua-core bowl)
      def        ~(. (default-agent this %|) bowl)
  ++  on-init           `this
  ++  on-save  !>(all-state)
  ++  on-load
    |=  old-state=vase
    ^-  step:agent:gall
    ~&  prep=%aqua
    =+  new=((soft state) !<(* old-state))
    ?~  new
      `this
    `this(all-state u.new)
  ::
  ++  on-poke
    |=  [=mark =vase]
    ^-  step:agent:gall
    =^  cards  all-state
      ?+  mark  ~|([%aqua-bad-mark mark] !!)
          %aqua-events  (poke-aqua-events:ac !<((list aqua-event) vase))
          %pill         (poke-pill:ac !<(pill vase))
          %noun         (poke-noun:ac !<(* vase))
      ==
    [cards this]
  ::
  ++  on-watch
    |=  =path
    ^-  step:agent:gall
    ?:  ?=([?(%effects %effect) ~] path)
      `this
    ?:  ?=([%effect @ ~] path)
      `this
    ?.  ?=([?(%effects %effect %evens %boths) @ ~] path)
      ~|  [%aqua-bad-subscribe-path path]
      !!
    ?~  (slaw %p i.t.path)
      ~|  [%aqua-bad-subscribe-path-ship path]
      !!
    `this
  ::
  ++  on-leave  on-leave:def
  ++  on-peek   peek:ac
  ::
  ++  on-agent  on-agent:def
  ++  on-arvo   on-arvo:def
  ++  on-fail   on-fail:def
  --
::
::  unix-{effects,events,boths}: collect jar of effects and events to
::    brodcast all at once to avoid gall backpressure
::  moves: Hoist moves into state for cleaner state management
::
=|  unix-effects=(jar ship unix-effect)
=|  unix-events=(jar ship unix-timed-event)
=|  unix-boths=(jar ship unix-both)
=|  cards=(list card:agent:gall)
|_  hid=bowl:gall
::
::  Represents a single ship's state.
::
++  pe
  |=  who=ship
  =+  (~(gut by piers) who *pier)
  =*  pier-data  -
  |%
  ::
  ::  Done; install data
  ::
  ++  abet-pe
    ^+  this
    =.  piers  (~(put by piers) who pier-data)
    this
  ::
  ::  Initialize new ship
  ::
  ++  apex
    =.  pier-data  *pier
    =.  snap  assembled
    ~&  pill-size=(met 3 (jam snap))
    ..abet-pe
  ::
  ::  Enqueue events to child arvo
  ::
  ++  push-events
    |=  ues=(list unix-event)
    ^+  ..abet-pe
    =.  next-events  (~(gas to next-events) ues)
    ..abet-pe
  ::
  ::  Send cards to host arvo
  ::
  ++  emit-cards
    |=  ms=(list card:agent:gall)
    =.  this  (^emit-cards ms)
    ..abet-pe
  ::
  ::  Process the events in our queue.
  ::
  ++  plow
    |-  ^+  ..abet-pe
    ?:  =(~ next-events)
      ..abet-pe
    ?.  processing-events
      ..abet-pe
    =^  ue  next-events  ~(get to next-events)
    =/  poke-arm  (mox +47.snap)
    ?>  ?=(%0 -.poke-arm)
    =/  poke  p.poke-arm
    =.  tym  (max +(tym) now.hid)
    =/  poke-result  (mule |.((slum poke tym ue)))
    ?:  ?=(%| -.poke-result)
      %-  (slog >%aqua-crash< >guest=who< p.poke-result)
      $
    =.  snap  +.p.poke-result
    =.  ..abet-pe  (publish-event tym ue)
    =.  ..abet-pe  (handle-effects ((list ovum) -.p.poke-result))
    $
  ::
  ::  Peek
  ::
  ++  peek
    |=  p=*
    =/  res  (mox +46.snap)
    ?>  ?=(%0 -.res)
    =/  peek  p.res
    =/  pax  (path p)
    ?>  ?=([@ @ @ @ *] pax)
    =.  i.t.t.t.pax  (scot %da tym)
    =/  pek  (slum peek [tym pax])
    pek
  ::
  ::  Wish
  ::
  ++  wish
    |=  txt=@t
    =/  res  (mox +22.snap)
    ?>  ?=(%0 -.res)
    =/  wish  p.res
    ~&  [who=who %wished (slum wish txt)]
    ..abet-pe
  ::
  ++  mox  |=(* (mock [snap +<] scry))
  ::
  ::  Start/stop processing events.  When stopped, events are added to
  ::  our queue but not processed.
  ::
  ++  start-processing-events  .(processing-events &)
  ++  stop-processing-events  .(processing-events |)
  ::
  ::  Handle all the effects produced by a single event.
  ::
  ++  handle-effects
    |=  effects=(list ovum)
    ^+  ..abet-pe
    ?~  effects
      ..abet-pe
    =.  ..abet-pe
      =/  sof  ((soft unix-effect) i.effects)
      ?~  sof
        ~?  aqua-debug=&  [who=who %unknown-effect i.effects]
        ..abet-pe
      (publish-effect u.sof)
    $(effects t.effects)
  ::
  ::  Give effect to our subscribers
  ::
  ++  publish-effect
    |=  uf=unix-effect
    ^+  ..abet-pe
    =.  unix-effects  (~(add ja unix-effects) who uf)
    =.  unix-boths  (~(add ja unix-boths) who [%effect uf])
    ..abet-pe
  ::
  ::  Give event to our subscribers
  ::
  ++  publish-event
    |=  ute=unix-timed-event
    ^+  ..abet-pe
    =.  event-log  [ute event-log]
    =.  unix-events  (~(add ja unix-events) who ute)
    =.  unix-boths  (~(add ja unix-boths) who [%event ute])
    ..abet-pe
  --
::
++  this  .
::
::  ++apex-aqua and ++abet-aqua must bookend calls from gall
::
++  apex-aqua
  ^+  this
  =:  cards         ~
      unix-effects  ~
      unix-events   ~
      unix-boths    ~
    ==
  this
::
++  abet-aqua
  ^-  (quip card:agent:gall state)
  =.  this
    =/  =path  /effect
    %-  emit-cards
    %-  zing
    %+  turn  ~(tap by unix-effects)
    |=  [=ship ufs=(list unix-effect)]
    %-  zing
    %+  turn  ufs
    |=  uf=unix-effect
    :~  [%give %fact ~[/effect] %aqua-effect !>(`aqua-effect`[ship uf])]
        [%give %fact ~[/effect/[-.q.uf]] %aqua-effect !>(`aqua-effect`[ship uf])]
    ==
  ::
  =.  this
    =/  =path  /effects
    %-  emit-cards
    %+  turn  ~(tap by unix-effects)
    |=  [=ship ufs=(list unix-effect)]
    [%give %fact ~[path] %aqua-effects !>(`aqua-effects`[ship (flop ufs)])]
  ::
  =.  this
    %-  emit-cards
    %-  zing
    %+  turn  ~(tap by unix-effects)
    |=  [=ship ufs=(list unix-effect)]
    =/  =path  /effect/(scot %p ship)
    %+  turn  ufs
    |=  uf=unix-effect
    [%give %fact ~[path] %aqua-effect !>(`aqua-effect`[ship uf])]
  ::
  =.  this
    %-  emit-cards
    %+  turn  ~(tap by unix-effects)
    |=  [=ship ufs=(list unix-effect)]
    =/  =path  /effects/(scot %p ship)
    [%give %fact ~[path] %aqua-effects !>(`aqua-effects`[ship (flop ufs)])]
  ::
  =.  this
    %-  emit-cards
    %+  turn  ~(tap by unix-events)
    |=  [=ship ve=(list unix-timed-event)]
    =/  =path  /events/(scot %p ship)
    [%give %fact ~[path] %aqua-events !>(`aqua-events`[ship (flop ve)])]
  ::
  =.  this
    %-  emit-cards
    %+  turn  ~(tap by unix-boths)
    |=  [=ship bo=(list unix-both)]
    =/  =path  /boths/(scot %p ship)
    [%give %fact ~[path] %aqua-boths !>(`aqua-boths`[ship (flop bo)])]
  ::
  [(flop cards) all-state]
::
++  emit-cards
  |=  ms=(list card:agent:gall)
  =.  cards  (weld ms cards)
  this
::
::
::  Run all events on all ships until all queues are empty
::
++  plow-all
  |-  ^+  this
  =/  who
    =/  pers  ~(tap by piers)
    |-  ^-  (unit ship)
    ?~  pers
      ~
    ?:  &(?=(^ next-events.q.i.pers) processing-events.q.i.pers)
      `p.i.pers
    $(pers t.pers)
  ~?  aqua-debug=|  plowing=who
  ?~  who
    this
  =.  this  abet-pe:plow:(pe u.who)
  $
::
::  Load a pill and assemble arvo.  Doesn't send any of the initial
::  events.
::
++  poke-pill
  |=  p=pill
  ^-  (quip card:agent:gall state)
  =.  this  apex-aqua  =<  abet-aqua
  =.  pil  p
  ~&  lent=(met 3 (jam boot-ova.pil))
  =/  res=toon :: (each * (list tank))
    (mock [boot-ova.pil [2 [0 3] [0 2]]] scry)
  =.  fleet-snaps  ~
  ?-  -.res
      %0
    ~&  %suc
    =.  assembled  +7.p.res
    this
  ::
      %1
    ~&  [%vere-blocked p.res]
    this
  ::
      %2
    ~&  %vere-fail
    %-  (slog p.res)
    this
  ==
::
::  Handle commands from CLI
::
::    Should put some thought into arg structure, maybe make a mark.
::
::    Should convert some of these to just rewrite into ++poke-events.
::
++  poke-noun
  |=  val=*
  ^-  (quip card:agent:gall state)
  =.  this  apex-aqua  =<  abet-aqua
  ^+  this
  ::  Could potentially factor out the three lines of turn-ships
  ::  boilerplate
  ::
  ?+  val  ~|(%bad-noun-arg !!)
      [%swap-vanes vs=*]
    ?>  ?=([[%7 * %1 installed=*] ~] boot-ova.pil)
    =.  installed.boot-ova.pil
      %+  roll  (,(list term) vs.val)
      |=  [v=term =_installed.boot-ova.pil]
      %^  slum  installed  now.hid
      =/  vane
        ?+  v  ~|([%unknown-vane v] !!)
          %a  %ames
          %b  %behn
          %c  %clay
          %d  %dill
          %e  %eyre
          %f  %ford
          %g  %gall
          %j  %jael
          %g  %gall
        ==
      =/  pax
        /(scot %p our.hid)/home/(scot %da now.hid)/sys/vane/[vane]
      =/  txt  .^(@ %cx (weld pax /hoon))
      [/vane/[vane] [%veer v pax txt]]
    =>  .(this ^+(this this))
    =^  ms  all-state  (poke-pill pil)
    (emit-cards ms)
  ::
      [%swap-files ~]
    =.  userspace-ova.pil
      =/  slim-dirs=(list path)
        ~[/app /ted /gen /lib /mar /sur /hoon/sys /arvo/sys /zuse/sys]
      :_  ~
      %-  unix-event
      %-  %*(. file-ovum:pill-lib directories slim-dirs)
      /(scot %p our.hid)/home/(scot %da now.hid)
    =^  ms  all-state  (poke-pill pil)
    (emit-cards ms)
  ::
      [%wish hers=* p=@t]
    %+  turn-ships  ((list ship) hers.val)
    |=  [who=ship thus=_this]
    =.  this  thus
    (wish:(pe who) p.val)
  ::
      [%unpause-events hers=*]
    %+  turn-ships  ((list ship) hers.val)
    |=  [who=ship thus=_this]
    =.  this  thus
    start-processing-events:(pe who)
  ::
      [%pause-events hers=*]
    %+  turn-ships  ((list ship) hers.val)
    |=  [who=ship thus=_this]
    =.  this  thus
    stop-processing-events:(pe who)
  ::
      [%clear-snap lab=@tas]
    =.  fleet-snaps  ~  ::  (~(del by fleet-snaps) lab.val)
    this
  ==
::
::  Apply a list of events tagged by ship
::
++  poke-aqua-events
  |=  events=(list aqua-event)
  ^-  (quip card:agent:gall state)
  =.  this  apex-aqua  =<  abet-aqua
  %+  turn-events  events
  |=  [ae=aqua-event thus=_this]
  =.  this  thus
  ?-  -.ae
      %init-ship
    =.  this  abet-pe:(publish-effect:(pe who.ae) [/ %sleep ~])
    =/  initted
      =<  plow
      %-  push-events:apex:(pe who.ae)
      ^-  (list unix-event)
      :~  [/ %wack 0]  ::  eny
          [/ %whom who.ae]  ::  eny
          [//newt/0v1n.2m9vh %born ~]
          [//behn/0v1n.2m9vh %born ~]
          :^  //term/1  %boot  &
          ?~  keys.ae
            [%fake who.ae]
          [%dawn u.keys.ae]
          -.userspace-ova.pil
          [//http-client/0v1n.2m9vh %born ~]
          [//http-server/0v1n.2m9vh %born ~]
          [//http-server/0v1n.2m9vh %live 8.080 `8.445]
      ==
    =.  this  abet-pe:initted
    (pe who.ae)
  ::
      %pause-events
    stop-processing-events:(pe who.ae)
  ::
      %snap-ships
    =.  fleet-snaps
      %+  ~(put by fleet-snaps)  lab.ae
      %-  malt
      %+  murn  hers.ae
      |=  her=ship
      ^-  (unit (pair ship pier))
      =+  per=(~(get by piers) her)
      ?~  per
        ~
      `[her u.per]
    (pe -.hers.ae)
  ::
      %restore-snap
    =.  this
      %+  turn-ships  (turn ~(tap by piers) head)
      |=  [who=ship thus=_this]
      =.  this  thus
      (publish-effect:(pe who) [/ %sleep ~])
    =.  piers  (~(uni by piers) (~(got by fleet-snaps) lab.ae))
    =.  this
      %+  turn-ships  (turn ~(tap by piers) head)
      |=  [who=ship thus=_this]
      =.  this  thus
      (publish-effect:(pe who) [/ %restore ~])
    (pe ~bud)  ::  XX why ~bud?  need an example
  ::
      %event
    ~?  &(aqua-debug=| !?=(?(%belt %hear) -.q.ue.ae))
      raw-event=[who.ae -.q.ue.ae]
    ~?  &(debug=| ?=(%receive -.q.ue.ae))
      raw-event=[who.ae ue.ae]
    (push-events:(pe who.ae) [ue.ae]~)
  ==
::
::  Run a callback function against a list of ships, aggregating state
::  and plowing all ships at the end.
::
::    I think we should use patterns like this more often.  Because we
::    don't, here's some points to be aware.
::
::    `fun` must take `this` as a parameter, since it needs to be
::    downstream of previous state changes.  You could use `state` as
::    the state variable, but it muddles the code and it's not clear
::    whether it's better.  You could use the `_(pe)` core if you're
::    sure you'll never need to refer to anything outside of your pier,
::    but I don't think we can guarantee that.
::
::    The callback function must start with `=.  this  thus`, or else
::    you don't get the new state.  Would be great if you could hot-swap
::    that context in here, but we don't know where to put it unless we
::    restrict the callbacks to always have `this` at a particular axis,
::    and that doesn't feel right
::
++  turn-plow
  |*  arg=mold
  |=  [hers=(list arg) fun=$-([arg _this] _(pe))]
  |-  ^+  this
  ?~  hers
    plow-all
  =.  this
    abet-pe:plow:(fun i.hers this)
  $(hers t.hers, this this)
::
++  turn-ships   (turn-plow ship)
++  turn-events  (turn-plow aqua-event)
::
::  Check whether we have a snapshot
::
++  peek
  |=  =path
  ^-  (unit (unit cage))
  ?+  path  ~
      [%x %fleet-snap @ ~]  ``noun+!>((~(has by fleet-snaps) i.t.t.path))
      [%x %ships ~]         ``noun+!>((turn ~(tap by piers) head))
      [%x %pill ~]          ``pill+!>(pil)
      [%x %i @ @ @ @ @ *]
    =/  who  (slav %p i.t.t.path)
    =/  pier  (~(get by piers) who)
    ?~  pier
      ~
    :^  ~  ~  %noun  !>
    (peek:(pe who) t.t.t.path)
  ==
::
::  Trivial scry for mock
::
++  scry  |=([* *] ~)
--
