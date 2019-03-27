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
=,  aquarium
=>  $~  |%
    +$  move  (pair bone card)
    +$  card
      $%  [%diff diff-type]
      ==
    ::
    ::  Outgoing subscription updates
    ::
    +$  diff-type
      $%  [%aqua-effects aqua-effects]
          [%aqua-events aqua-events]
          [%aqua-boths aqua-boths]
      ==
    ::
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
=,  gall
::
::  unix-{effects,events,boths}: collect jar of effects and events to
::    brodcast all at once to avoid gall backpressure
::  moves: Hoist moves into state for cleaner state management
::
=|  unix-effects=(jar ship unix-effect)
=|  unix-events=(jar ship unix-timed-event)
=|  unix-boths=(jar ship unix-both)
=|  moves=(list move)
|_  $:  hid=bowl
        state
    ==
::
::  Represents a single ship's state.
::
++  pe
  |=  who=ship
  =+  (fall (~(get by piers) who) *pier)
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
  ::  Send moves to host arvo
  ::
  ++  emit-moves
    |=  ms=(list move)
    =.  this  (^emit-moves ms)
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
    =/  poke-result  (slum poke tym ue)
    =.  snap  +.poke-result
    =.  ..abet-pe  (publish-event tym ue)
    =.  ..abet-pe  (handle-effects ((list ovum) -.poke-result))
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
        ~?  aqua-debug=|  [who=who %unknown-effect i.effects]
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
  =:  moves         ~
      unix-effects  ~
      unix-events   ~
      unix-boths    ~
    ==
  this
::
++  abet-aqua
  ^-  (quip move _this)
  =.  this
    %-  emit-moves
    %-  zing  ^-  (list (list move))
    %+  turn  ~(tap by sup.hid)
    |=  [b=bone her=ship pax=path]
    ^-  (list move)
    ?+    pax  ~
        [%effects @ ~]
      =/  who  (slav %p i.t.pax)
      =/  ufs  (~(get ja unix-effects) who)
      ?~  ufs
        ~
      [b %diff %aqua-effects who (flop ufs)]~
    ::
        [%effects ~]
      %+  turn
        ~(tap by unix-effects)
      |=  [who=ship ufs=(list unix-effect)]
      [b %diff %aqua-effects who (flop ufs)]
    ::
        [%events @ ~]
      =/  who  (slav %p i.t.pax)
      =/  ve  (~(get ja unix-events) who)
      ?~  ve
        ~
      [b %diff %aqua-events who (flop ve)]~
    ::
        [%boths @ ~]
      =/  who  (slav %p i.t.pax)
      =/  bo  (~(get ja unix-boths) who)
      ?~  bo
        ~
      [b %diff %aqua-boths who (flop bo)]~
    ==
  [(flop moves) this]
::
++  emit-moves
  |=  ms=(list move)
  =.  moves  (weld ms moves)
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
::  Subscribe to effects from a ship
::
++  peer-effects
  |=  pax=path
  ^-  (quip move _this)
  ?.  ?=([@ *] pax)
    ~&  [%aqua-bad-peer-effects pax]
    `this
  ?~  (slaw %p i.pax)
    ~&  [%aqua-bad-peer-effects-ship pax]
    !!
  `this
::
::  Subscribe to events to a ship
::
++  peer-events
  |=  pax=path
  ^-  (quip move _this)
  ?.  ?=([@ ~] pax)
    ~&  [%aqua-bad-peer-events pax]
    `this
  ?~  (slaw %p i.pax)
    ~&  [%aqua-bad-peer-events-ship pax]
    !!
  `this
::
::  Subscribe to both events and effects of a ship
::
++  peer-boths
  |=  pax=path
  ^-  (quip move _this)
  ?.  ?=([@ ~] pax)
    ~&  [%aqua-bad-peer-boths pax]
    `this
  ?~  (slaw %p i.pax)
    ~&  [%aqua-bad-peer-boths-ship pax]
    !!
  `this
::
::  Load a pill and assemble arvo.  Doesn't send any of the initial
::  events.
::
++  poke-pill
  |=  p=pill
  ^-  (quip move _this)
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
  ^-  (quip move _this)
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
      |=  [v=term _installed.boot-ova.pil]
      %^  slum  installed.boot-ova.pil  now.hid
      =/  vane
        ?+  v  ~|([%unknown-vane v] !!)
          %a  %ames
          %b  %behn
          %c  %clay
          %d  %dill
          %e  %eyre
          %f  %ford
          %g  %gall
          %j  %ford
        ==
      =/  pax
        /(scot %p our.hid)/home/(scot %da now.hid)/sys/vane/[vane]
      =/  txt  .^(@ %cx (weld pax /hoon))
      [/vane/[vane] [%veer v pax txt]]
    =>  .(this ^+(this this))
    =^  ms  this  (poke-pill pil)
    (emit-moves ms)
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
  ^-  (quip move _this)
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
          [//newt/0v1n.2m9vh %barn ~]
          [//behn/0v1n.2m9vh %born ~]
          :+  //term/1  %boot
          ?~  keys.ae
            [%fake who.ae]
          [%dawn u.keys.ae]
          -.userspace-ova.pil
          [//http/0v1n.2m9vh %born ~]
          [//http/0v1n.2m9vh %live 8.080 `8.445]
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
    ~?  &(debug=& ?=(%they -.q.ue.ae))
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
++  peek-x-fleet-snap
  |=  pax=path
  ^-  (unit (unit [%noun noun]))
  ?.  ?=([@ ~] pax)
    ~
  :^  ~  ~  %noun
  (~(has by fleet-snaps) i.pax)
::
::  Pass scry into child ship
::
++  peek-x-i
  |=  pax=path
  ^-  (unit (unit [%noun noun]))
  ?.  ?=([@ @ @ *] pax)
    ~
  =/  who  (slav %p i.pax)
  =/  pier  (~(get by piers) who)
  ?~  pier
    ~
  :^  ~  ~  %noun
  (peek:(pe who) [%cx pax])
::
::  Get all created ships
::
++  peek-x-ships
  |=  pax=path
  ^-  (unit (unit [%noun (list ship)]))
  ?.  ?=(~ pax)
    ~
  :^  ~  ~  %noun
  `(list ship)`(turn ~(tap by piers) head)
::
::  Trivial scry for mock
::
++  scry  |=([* *] ~)
::
::  Throw away old state if it doesn't soft to new state.
::
++  prep
  |=  old/(unit noun)
  ^-  [(list move) _+>.$]
  ~&  prep=%aqua
  ?~  old
    `+>.$
  =+  new=((soft state) u.old)
  ?~  new
    `+>.$
  `+>.$(+<+ u.new)
--
