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
      $%  [%wait wire p=@da]
          [%rest wire p=@da]
          [%hiss wire p=(unit user:eyre) q=mark r=(cask hiss:eyre)]
          [%diff diff-type]
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
          init-cache=(map [ship (unit dawn-event)] pier)
          fleet-snaps=(map term (map ship pier))
          piers=(map ship pier)
      ==
    ::
    +$  pier
      $:  snap=*
          event-log=(list unix-timed-event)
          next-events=(qeu unix-event)
          processing-events=?
          next-timer=(unit @da)
          http-requests=(set @ud)
      ==
    --
=,  gall
::
::  aqua-effect-list: collect list of aqua effects to broadcast at once
::                    to avoid gall backpressure
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
  ++  abet-pe
    ^+  this
    =.  piers  (~(put by piers) who pier-data)
    this
  ::
  ++  apex
    =.  pier-data  *pier
    =.  snap  assembled
    ~&  r=(met 3 (jam snap))
    ..abet-pe
  ::
  ++  push-events
    |=  ova=(list unix-event)
    ^+  ..abet-pe
    =.  next-events  (~(gas to next-events) ova)
    ..abet-pe
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
    =^  ovo  next-events  ~(get to next-events)
    =/  res  (mox +47.snap)
    ?>  ?=(%0 -.res)
    =/  poke  p.res
    =.  tym  (max +(tym) now.hid)
    =/  res  (slum poke tym ovo)
    =.  snap  +3.res
    =.  ..abet-pe  (publish-event tym ovo)
    =.  ..abet-pe  (handle-effects ((list ovum) -.res))
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
    ~&  [who=who %peeking-in tym pax]
    ?>  ?=([@ @ @ @ *] pax)
    =.  i.t.t.t.pax  (scot %da tym)
    =/  pek  (slum peek [tym pax])
    ~&  [who=who %peeked]
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
  ::  Restart outstanding requests
  ::
  ++  restore
    ^+  ..abet-pe
    ::  Restore behn
    ::
    =.  ..abet-pe
      ?~  next-timer
        ..abet-pe
      (set-timer u.next-timer)
    ::  Restore eyre
    ::
    =.  http-requests  ~
    =.  ..abet-pe  (push-events [//http/0v1n.2m9vh %born ~]~)
    ..abet-pe
  ::
  ::  Cancel outstanding requests
  ::
  ++  sleep
    ^+  ..abet-pe
    ::  Sleep behn
    ::
    =.  ..abet-pe
      ?~  next-timer
        ..abet-pe
      cancel-timer
    ::  Sleep eyre
    ::
    ::    Eyre doesn't support cancelling HTTP requests from userspace.
    ::
    =.  http-requests  ~
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
        ~&  [who=who %unknown-effect i.effects]
        ..abet-pe
      =.  ..abet-pe
        ?-    -.q.u.sof
          %blit  (handle-blit u.sof)
          %send  (handle-send u.sof)
          %doze  (handle-doze u.sof)
          %thus  (handle-thus u.sof)
          %ergo  (handle-ergo u.sof)
        ==
      (publish-effect u.sof)
    $(effects t.effects)
  ::
  ::  Would love to see a proper stateful terminal handler.  Ideally,
  ::  you'd be able to ^X into the virtual ship, like the old ^W.
  ::
  ::  However, that's porbably not the primary way of interacting with
  ::  it.  In practice, most of the time you'll be running from a file
  ::  (eg for automated testing) or fanning the same command to multiple
  ::  ships or otherwise making use of the fact that we can
  ::  programmatically send events.
  ::
  ++  handle-blit
    |=  [way=wire %blit blits=(list blit:dill)]
    ^+  ..abet-pe
    =/  last-line
      %+  roll  blits
      |=  [b=blit:dill line=tape]
      ?-    -.b
          %lin  (tape p.b)
          %mor  ~&  "{<who>}: {line}"  ""
          %hop  line
          %bel  line
          %clr  ""
          %sag  ~&  [%save-jamfile-to p.b]  line
          %sav  ~&  [%save-file-to p.b]  line
          %url  ~&  [%activate-url p.b]  line
      ==
    ~&  last-line
    ..abet-pe
  ::
  ::  This needs a better SDN solution.  Every ship should have an IP
  ::  address, and we should eventually test changing those IP
  ::  addresses.
  ::
  ::  For now, we broadcast every packet to every ship and rely on them
  ::  to drop them.
  ::
  ++  handle-send
    |=  [way=wire %send lan=lane:ames pac=@]
    ^+  ..abet-pe
    =/  dest-ip
      |-  ^-  (unit @if)
      ?-  -.lan
        %if  `r.lan
        %is  ?~(q.lan ~ $(lan u.q.lan))
        %ix  `r.lan
      ==
    ?~  dest-ip
      ~&  [%sending-no-destination who lan]
      ..abet-pe
    ?.  &(=(0 (rsh 0 16 u.dest-ip)) =(1 (rsh 0 8 u.dest-ip)))
      ~&  [%havent-implemented-direct-lanes who lan]
      ..abet-pe
    ::  ~&  [who=who %blast-sending]
    =/  hear  [//newt/0v1n.2m9vh %hear lan pac]
    =.  this  (blast-event hear)
    ::  =/  her  ?:(=(~dev who) ~bud ~dev) ::ship  (dis u.dest-ip 0xff)
    ::  ?.  (~(has by piers) her)
    ::    ~&  [%dropping who=who her=her]
    ::    ..abet-pe
    ::  ~&  [%sending who=who her=her ip=`@ux`u.dest-ip]
    ::  =^  ms  this
    ::    abet-pe:(push-events:(pe her) ~[hear])
    ..abet-pe
  ::
  ::  Would love to be able to control time more precisely, jumping
  ::  forward and whatnot.
  ::
  ++  handle-doze
    |=  [way=wire %doze tim=(unit @da)]
    ^+  ..abet-pe
    ?~  tim
      ?~  next-timer
        ..abet-pe
      cancel-timer
    ?~  next-timer
      (set-timer u.tim)
    (set-timer:cancel-timer u.tim)
  ::
  ++  set-timer
    |=  tim=@da
    =.  tim  +(tim)  ::  nobody's perfect
    ~&  [who=who %setting-timer tim]
    =.  next-timer  `tim
    (emit-moves [ost.hid %wait /(scot %p who) tim]~)
  ::
  ++  cancel-timer
    ~&  [who=who %cancell-timer (need next-timer)]
    (emit-moves [ost.hid %rest /(scot %p who) (need next-timer)]~)
  ::
  ++  take-wake
    |=  [way=wire ~]
    ~&  [who=who %wakey now.hid]
    =.  next-timer  ~
    %-  push-events:(pe who)
    [//behn/0v1n.2m9vh %wake ~]~
  ::
  ::  Handle outgoing HTTP request
  ::
  ++  handle-thus
    |=  [way=wire %thus num=@ud req=(unit hiss:eyre)]
    ^+  ..abet-pe
    ?~  req
      ?.  (~(has in http-requests) num)
        ..abet-pe
      ::  Eyre doesn't support cancelling HTTP requests from userspace,
      ::  so we remove it from our state so we won't pass along the
      ::  response.
      ::
      ~&  [who=who %cant-cancel-thus num=num]
      =.  http-requests  (~(del in http-requests) num)
      ..abet-pe
    ~&  [who=who %requesting u.req]
    =.  http-requests  (~(put in http-requests) num)
    %-  emit-moves  :_  ~
    :*  ost.hid
        %hiss
        /(scot %p who)/(scot %ud num)
        ~
        %httr
        [%hiss u.req]
    ==
  ::
  ::  Pass HTTP response back to virtual ship
  ::
  ++  take-sigh-httr
    |=  [way=wire res=httr:eyre]
    ^+  ..abet-pe
    ?>  ?=([@ ~] way)
    =/  num  (slav %ud i.way)
    ?.  (~(has in http-requests) num)
      ~&  [who=who %ignoring-httr num=num]
      ..abet-pe
    =.  http-requests  (~(del in http-requests) num)
    (push-events [//http/0v1n.2m9vh %they num res]~)
  ::
  ::  Got error in HTTP response
  ::
  ++  take-sigh-tang
    |=  [way=wire tan=tang]
    ^+  ..abet-pe
    ?>  ?=([@ ~] way)
    =/  num  (slav %ud i.way)
    ?.  (~(has in http-requests) num)
      ~&  [who=who %ignoring-httr num=num]
      ..abet-pe
    =.  http-requests  (~(del in http-requests) num)
    %-  (slog tan)
    ..abet-pe
  ::
  ::  We should mirror a mount point of child to a clay desk of host.
  ::  For now, we just allow injecting a change to the child, so we
  ::  throw away ergos.
  ::
  ++  handle-ergo
    |=  [way=wire %ergo mount-point=@tas mod=mode:clay]
    ^+  ..abet-pe
    ~&  [who=who %file-changes (lent mod)] :: (turn mod head)]
    ..abet-pe
  ::
  ::  Give effect to our subscribers
  ::
  ++  publish-effect
    |=  ovo=unix-effect
    ^+  ..abet-pe
    =.  unix-effects  (~(add ja unix-effects) who ovo)
    =.  unix-boths  (~(add ja unix-boths) who [%effect ovo])
    ..abet-pe
  ::
  ::  Give event to our subscribers
  ::
  ++  publish-event
    |=  ovo=unix-timed-event
    ^+  ..abet-pe
    =.  event-log  [ovo event-log]
    =.  unix-events  (~(add ja unix-events) who ovo)
    =.  unix-boths  (~(add ja unix-boths) who [%event ovo])
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
    %+  murn  ~(tap by sup.hid)
    |=  [b=bone her=ship pax=path]
    ^-  (unit move)
    ?+    pax  ~
        [%effects @ ~]
      =/  who  (slav %p i.t.pax)
      =/  fx  (~(get ja unix-effects) who)
      ?~  fx
        ~
      `[b %diff %aqua-effects who fx]
    ::
        [%events @ ~]
      =/  who  (slav %p i.t.pax)
      =/  ve  (~(get ja unix-events) who)
      ?~  ve
        ~
      `[b %diff %aqua-events who ve]
    ::
        [%boths @ ~]
      =/  who  (slav %p i.t.pax)
      =/  bo  (~(get ja unix-boths) who)
      ?~  bo
        ~
      `[b %diff %aqua-boths who bo]
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
  ~&  plowing=who
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
  ?.  ?=([@ ~] pax)
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
  =.  init-cache  ~
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
      [%init hers=*]
    =/  hers  ((list ship) hers.val)
    ?~  hers
      this
    =^  ms  this  (poke-aqua-events [%init-ship i.hers ~]~)
    (emit-moves ms)
  ::
      [%dojo hers=* command=*]
    %+  turn-ships  ((list ship) hers.val)
    |=  [who=ship thus=_this]
    =.  this  thus
    %-  push-events:(pe who)
    ^-  (list unix-event)
    :~
        [//term/1 %belt %ctl `@c`%e]
        [//term/1 %belt %ctl `@c`%u]
        [//term/1 %belt %txt ((list @c) (tape command.val))]
        [//term/1 %belt %ret ~]
    ==
  ::
      [%raw-event hers=* ovo=*]
    =/  ovo  ((soft unix-event) ovo.val)
    ?~  ovo
      ~&  %ovo-not-an-event
      this
    %+  turn-ships  ((list ship) hers.val)
    |=  [who=ship thus=_this]
    =.  this  thus
    (push-events:(pe who) ~[u.ovo])
  ::
      [%file hers=* pax=*]
    =/  pax  (path pax.val)
    ?>  ?=([@ @ @ *] pax)
    =/  file  [/text/plain (as-octs:mimes:html .^(@ %cx pax))]
    %+  turn-ships  ((list ship) hers.val)
    |=  [who=ship thus=_this]
    =.  this  thus
    %-  push-events:(pe who)
    [//sync/0v1n.2m9vh %into i.t.pax | [t.t.t.pax `file]~]~
  ::
      [%peek hers=* p=*]
    %+  turn-ships  ((list ship) hers.val)
    |=  [who=ship thus=_this]
    =.  this  thus
    ~&  [who=who %peek-result (peek:(pe who) p.val)]
    (pe who)
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
      [%snap-fleet lab=@tas]
    =.  fleet-snaps  (~(put by fleet-snaps) lab.val piers)
    this
  ::
      [%restore-fleet lab=@tas]
    =^  ms  this  (poke-aqua-events [%restore-snap lab.val]~)
    (emit-moves ms)
  ::
      [%clear-snap lab=@tas]
    =.  fleet-snaps  ~  ::  (~(del by fleet-snaps) lab.val)
    =.  init-cache  ~
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
  |=  [ovo=aqua-event thus=_this]
  =.  this  thus
  ?-  -.ovo
      %init-ship
    ::  =/  prev  (~(get by init-cache) [who keys]:ovo)
    ::  ?:  &(?=(^ prev) (lth who.ovo ~marzod))
    ::    ~&  [%loading-cached-ship who.ovo]
    ::    =.  this  (restore-ship who.ovo u.prev)
    ::    (pe who.ovo)
    =.  this  abet-pe:sleep:(pe who.ovo)
    =/  initted
      =<  plow
      %-  push-events:apex:(pe who.ovo)
      ^-  (list unix-event)
      :~  [/ %wack 0]  ::  eny
          [/ %whom who.ovo]  ::  eny
          [//newt/0v1n.2m9vh %barn ~]
          [//behn/0v1n.2m9vh %born ~]
          :+  //term/1  %boot
          ?~  keys.ovo
            [%fake who.ovo]
          [%dawn u.keys.ovo]
          -.userspace-ova.pil
          [//http/0v1n.2m9vh %born ~]
          [//http/0v1n.2m9vh %live 8.080 `8.445]
      ==
    =.  this  abet-pe:initted
    =.  init-cache
      %+  ~(put by init-cache)  [who keys]:ovo
      (~(got by piers) who.ovo)
    (pe who.ovo)
  ::
      %pause-events
    stop-processing-events:(pe who.ovo)
  ::
      %snap-ships
    =.  fleet-snaps
      %+  ~(put by fleet-snaps)  lab.ovo
      %-  malt
      %+  murn  hers.ovo
      |=  her=ship
      ^-  (unit (pair ship pier))
      =+  per=(~(get by piers) her)
      ?~  per
        ~
      `[her u.per]
    (pe -.hers.ovo)
  ::
      %restore-snap
    =.  this
      %+  turn-ships  (turn ~(tap by piers) head)
      |=  [who=ship thus=_this]
      =.  this  thus
      sleep:(pe who)
    =.  piers  (~(uni by piers) (~(got by fleet-snaps) lab.ovo))
    =.  this
      %+  turn-ships  (turn ~(tap by piers) head)
      |=  [who=ship thus=_this]
      =.  this  thus
      restore:(pe who)
    (pe ~bud)  ::  XX why ~bud?  need an example
  ::
      %event
    ~&  ev=-.q.ovo.ovo
    (push-events:(pe who.ovo) [ovo.ovo]~)
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
::  Send the same event to all ships
::
++  blast-event
  |=  ovo=unix-event
  =/  pers  ~(tap by piers)
  |-  ^+  this
  ?~  pers
    this
  =.  this
    abet-pe:(push-events:(pe p.i.pers) ~[ovo])
  $(pers t.pers)
::
::  Restore ships
::
++  restore-ships
  |=  [hers=(list ship) from=(map ship pier)]
  =.  this
    %+  turn-ships  hers
    |=  [who=ship thus=_this]
    =.  this  thus
    sleep:(pe who)
  =.  piers
    %-  ~(gas by piers)
    %+  turn  hers
    |=  her=ship
    [her (~(got by from) her)]
  =.  this
    %+  turn-ships  hers
    |=  [who=ship thus=_this]
    =.  this  thus
    restore:(pe who)
  this
::
::  Restore ships from pier
::
++  restore-ship
  |=  [her=ship per=pier]
  =.  this  abet-pe:plow:sleep:(pe her)
  =.  piers  (~(put by piers) her per)
  =.  this  abet-pe:plow:restore:(pe her)
  this
::
::  Received timer wake
::
++  wake
  |=  [way=wire ~]
  ^-  (quip move _this)
  =.  this  apex-aqua  =<  abet-aqua
  ?>  ?=([@ *] way)
  =/  who  (,@p (slav %p i.way))
  %+  turn-ships  ~[who]
  |=  [who=ship thus=_this]
  =.  this  thus
  (take-wake:(pe who) t.way ~)
::
::  Received inbound HTTP response
::
++  sigh-httr
  |=  [way=wire res=httr:eyre]
  ^-  (quip move _this)
  =.  this  apex-aqua  =<  abet-aqua
  ?>  ?=([@ *] way)
  =/  who  (,@p (slav %p i.way))
  ~&  [%received-httr who]
  %+  turn-ships  ~[who]
  |=  [who=ship thus=_this]
  =.  this  thus
  (take-sigh-httr:(pe who) t.way res)
::
::  Received inbound HTTP response error
::
++  sigh-tang
  |=  [way=wire tan=tang]
  ^-  (quip move _this)
  =.  this  apex-aqua  =<  abet-aqua
  ?>  ?=([@ *] way)
  =/  who  (,@p (slav %p i.way))
  ~&  [%received-httr who]
  %+  turn-ships  ~[who]
  |=  [who=ship thus=_this]
  =.  this  thus
  (take-sigh-tang:(pe who) t.way tan)
::
::  Handle scry to aqua
::
++  peek-x-fleet-snap
  |=  pax=path
  ^-  (unit (unit [%noun noun]))
  ~&  [%peeking pax]
  ?.  ?=([@ ~] pax)
    ~
  :^  ~  ~  %noun
  (~(has by fleet-snaps) i.pax)
::
::
::
++  peek-x-i
  |=  pax=path
  ^-  (unit (unit [%noun noun]))
  ~&  [%peeking-i pax]
  ?.  ?=([@ @ @ *] pax)
    ~
  =/  who  (slav %p i.pax)
  =/  pier  (~(get by piers) who)
  ?~  pier
    ~
  :^  ~  ~  %noun
  (peek:(pe who) [%cx pax])
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
