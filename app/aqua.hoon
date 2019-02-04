::  usage:
::  |start %aqua
::  /-  pill
::  :aqua &pill .^(pill:pill %cx %/urbit/pill)
::  :aqua [%init ~[~bud ~dev]]
::  :aqua [%dojo ~[~bud ~dev] "[our eny (add 3 5)]"]
::  :aqua [%dojo ~[~bud] "|hi ~dev"]
::  :aqua [%pause-events ~[~bud ~dev]]
::
::
::  We get ++unix-event and ++pill from /-pill
::
/-  pill
=,  pill
=>  $~  |%
    ++  move  (pair bone card)
    ++  card
      $%  [%wait wire p=@da]
          [%rest wire p=@da]
          [%hiss wire p=(unit user:eyre) q=mark r=(cask hiss:eyre)]
      ==
    ++  unix-effect
      %+  pair  wire
      $%  [%blit p=(list blit:dill)]
          [%send p=lane:ames q=@]
          [%doze p=(unit @da)]
          [%thus p=@ud q=(unit hiss:eyre)]
      ==
    ++  state
      $:  %0
          pil=pill
          assembled=*
          fleet-snaps=(map term (map ship pier))
          piers=(map ship pier)
      ==
    ++  pier
      $:  snap=*
          event-log=(list unix-event)
          next-events=(qeu unix-event)
          processing-events=?
          next-timer=(unit @da)
          http-requests=(set @ud)
      ==
    --
=,  gall
|_  $:  hid/bowl
        state
    ==
::
::  Represents a single ship's state.
::
++  pe
  |=  who=ship
  =+  (fall (~(get by piers) who) *pier)
  =*  pier-data  -
  =|  moves=(list move)
  |%
  ++  abet
    ^-  (quip move _this)
    =.  piers  (~(put by piers) who pier-data)
    [(flop moves) this]
  ::
  ++  apex
    =.  pier-data  *pier
    =.  snap  assembled
    ~&  r=(met 3 (jam snap))
    ..abet
  ::
  ++  push-events
    |=  ova=(list unix-event)
    ^+  ..abet
    =.  next-events  (~(gas to next-events) ova)
    ..abet
  ::
  ++  emit-moves
    |=  ms=(list move)
    =.  moves  (weld ms moves)
    ..abet
  ::
  ::  Process the events in our queue.
  ::
  ++  plow
    |-  ^+  ..abet
    ?:  =(~ next-events)
      ..abet
    ?.  processing-events
      ..abet
    =^  ovo  next-events  ~(get to next-events)
    =/  res  (mox +47.snap)
    ?>  ?=(%0 -.res)
    =+  poke=p.res
    =+  res=(slum poke now.hid ovo)
    =.  event-log  [ovo event-log]
    =.  snap  +3.res
    =.  ..abet  (handle-effects ((list ovum) -.res))
    $
  ::
  ::  Restart outstanding requests
  ::
  ++  restore
    ^+  ..abet
    ::  Restore behn
    ::
    =.  ..abet
      ?~  next-timer
        ..abet
      (set-timer u.next-timer)
    ::  Restore eyre
    ::
    =.  http-requests  ~
    =.  ..abet  (push-events [//http/0v1n.2m9vh %born ~]~)
    ..abet
  ::
  ::  Cancel outstanding requests
  ::
  ++  sleep
    ^+  ..abet
    ::  Sleep behn
    ::
    =.  ..abet
      ?~  next-timer
        ..abet
      cancel-timer
    ::  Sleep eyre
    ::
    ::    Eyre doesn't support cancelling HTTP requests from userspace.
    ::
    =.  http-requests  ~
    ..abet
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
    ^+  ..abet
    ?~  effects
      ..abet
    =.  ..abet
      =/  sof  ((soft unix-effect) i.effects)
      ?~  sof
        ~&  [who=who %unknown-effect i.effects]
        ..abet
      ?-    -.q.u.sof
          %blit  (handle-blit u.sof)
          %send  (handle-send u.sof)
          %doze  (handle-doze u.sof)
          %thus  (handle-thus u.sof)
      ==
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
    ^+  ..abet
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
    ..abet
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
    ^+  ..abet
    =/  dest-ip
      |-  ^-  (unit @if)
      ?-  -.lan
        %if  `r.lan
        %is  ?~(q.lan ~ $(lan u.q.lan))
        %ix  `r.lan
      ==
    ?~  dest-ip
      ~&  [%sending-no-destination who lan]
      ..abet
    ?.  &(=(0 (rsh 0 16 u.dest-ip)) =(1 (rsh 0 8 u.dest-ip)))
      ~&  [%havent-implemented-direct-lanes who lan]
      ..abet
    ~&  [%blast-sending who=who]
    =/  hear  [//newt/0v1n.2m9vh %hear lan pac]
    =.  this  (blast-event hear)
    ::  =/  her  ?:(=(~dev who) ~bud ~dev) ::ship  (dis u.dest-ip 0xff)
    ::  ?.  (~(has by piers) her)
    ::    ~&  [%dropping who=who her=her]
    ::    ..abet
    ::  ~&  [%sending who=who her=her ip=`@ux`u.dest-ip]
    ::  =^  ms  this
    ::    abet:(push-events:(pe her) ~[hear])
    ..abet
  ::
  ::  Would love to be able to control time more precisely, jumping
  ::  forward and whatnot.
  ::
  ++  handle-doze
    |=  [way=wire %doze tim=(unit @da)]
    ^+  ..abet
    ?~  tim
      ?~  next-timer
        ..abet
      cancel-timer
    ?~  next-timer
      (set-timer u.tim)
    (set-timer:cancel-timer u.tim)
  ::
  ++  set-timer
    |=  tim=@da
    =.  tim  +(tim)  ::  nobody's perfect
    =.  next-timer  `tim
    ~&  [%sleeping-until who tim]
    (emit-moves [ost.hid %wait /(scot %p who) tim]~)
  ::
  ++  cancel-timer
    ~&  [%cancelling-timer who]
    (emit-moves [ost.hid %rest /(scot %p who) (need next-timer)]~)
  ::
  ++  take-wake
    |=  [way=wire ~]
    =.  next-timer  ~
    %-  push-events:(pe who)
    [//behn/0v1n.2m9vh %wake ~]~
  ::
  ::  Handle outgoing HTTP request
  ::
  ++  handle-thus
    |=  [way=wire %thus num=@ud req=(unit hiss:eyre)]
    ^+  ..abet
    ?~  req
      ?.  (~(has in http-requests) num)
        ..abet
      ::  Eyre doesn't support cancelling HTTP requests from userspace,
      ::  so we remove it from our state so we won't pass along the
      ::  response.
      ::
      ~&  [%cant-cancel-thus who=who num=num]
      =.  http-requests  (~(del in http-requests) num)
      ..abet
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
    ^+  ..abet
    ?>  ?=([@ ~] way)
    =/  num  (slav %ud i.way)
    ?.  (~(has in http-requests) num)
      ~&  [%ignoring-httr who=who num=num]
      ..abet
    =.  http-requests  (~(del in http-requests) num)
    (push-events [//http/0v1n.2m9vh %they num res]~)
  ::
  ::  Got error in HTTP response
  ::
  ++  take-sigh-tang
    |=  [way=wire tan=tang]
    ^+  ..abet
    ?>  ?=([@ ~] way)
    =/  num  (slav %ud i.way)
    ?.  (~(has in http-requests) num)
      ~&  [%ignoring-httr who=who num=num]
      ..abet
    =.  http-requests  (~(del in http-requests) num)
    %-  (slog tan)
    ..abet
  --
::
++  this  .
::
::  Run all events on all ships until all queues are empty
::
++  plow-all
  |-  ^-  (quip move _this)
  =/  who
    =/  pers  ~(tap by piers)
    |-  ^-  (unit ship)
    ?~  pers
      ~
    ?:  &(?=(^ next-events.q.i.pers) processing-events.q.i.pers)
      ~&  [%new-events p.i.pers]
      `p.i.pers
    $(pers t.pers)
  ~&  plowing=who
  ?~  who
    `this
  =^  moves  this  abet:plow:(pe u.who)
  =/  nex  $
  nex(- (weld -.nex moves))
::
::  Load a pill and assemble arvo.  Doesn't send any of the initial
::  events.
::
++  poke-pill
  |=  p=pill
  ^-  (quip move _this)
  =.  pil  p
  ~&  lent=(met 3 (jam boot-ova.pil))
  =/  res=toon :: (each * (list tank))
    (mock [boot-ova.pil [2 [0 3] [0 2]]] scry)
  ?-  -.res
      %0
    ~&  %suc
    =.  assembled  +7.p.res
    `this
  ::
      %1
    ~&  [%vere-blocked p.res]
    `this
  ::
      %2
    ~&  %vere-fail
    %-  (slog p.res)
    `this
  ==
::
::  Handle commands
::
::    Should put some thought into arg structure, maybe make a mark.
::
++  poke-noun
  |=  val=*
  ^-  (quip move _this)
  ::  Could potentially factor out the three lines of turn-ships
  ::  boilerplate
  ::
  ?+  val  ~|(%bad-noun-arg !!)
      [%init hers=*]
    %+  turn-ships  ((list ship) hers.val)
    |=  [who=ship thus=_this]
    =.  this  thus
    ~&  [%initting who]
    %-  push-events:apex:(pe who)
    ^-  (list unix-event)
    :~  `unix-event`[/ %wack 0]  ::  eny
        `unix-event`[/ %whom who]  ::  eny
        `unix-event`[//newt/0v1n.2m9vh %barn ~]
        `unix-event`[//behn/0v1n.2m9vh %born ~]
        `unix-event`[//term/1 %boot %fake who]
        `unix-event`-.userspace-ova.pil
        `unix-event`[//http/0v1n.2m9vh %born ~]
        `unix-event`[//http/0v1n.2m9vh %live 8.080 `8.445]
        `unix-event`[//term/1 %belt %ctl `@c`%x]
    ==
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
      `this
    %+  turn-ships  ((list ship) hers.val)
    |=  [who=ship thus=_this]
    =.  this  thus
    (push-events:(pe who) ~[u.ovo])
  ::
      [%snap-fleet lab=@tas]
    =.  fleet-snaps  (~(put by fleet-snaps) lab.val piers)
    `this
  ::
      [%restore-fleet lab=@tas]
    =^  moves-1  this
      %+  turn-ships  (turn ~(tap by piers) head)
      |=  [who=ship thus=_this]
      =.  this  thus
      sleep:(pe who)
    =.  piers  (~(got by fleet-snaps) lab.val)
    =^  moves-2  this
      %+  turn-ships  (turn ~(tap by piers) head)
      |=  [who=ship thus=_this]
      =.  this  thus
      restore:(pe who)
    [(weld moves-1 moves-2) this]
  ::
      [%peek who=@p p=*]
    ::  should resurrect
    ::  =+  res=(mox +46.snap)
    ::  ?>  ?=(%0 -.res)
    ::  =+  peek=p.res
    ::  ~&  (slum peek p.val)
    `this
  ::
      [%wish who=@p p=@t]
    ::  should resurrect
    ::  =+  res=(mox +22.snap)
    ::  ?>  ?=(%0 -.res)
    ::  =+  wish=p.res
    ::  ~&  (slum wish p.val)
    `this
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
++  turn-ships
  |=  [hers=(list ship) fun=$-([ship _this] _(pe))]
  |-  ^-  (quip move _this)
  ?~  hers
    =^  moves  this  plow-all
    [moves this]
  =^  moves  this
    abet:plow:(fun i.hers this)
  =^  nex-moves  this  $(hers t.hers, this this)
  [(weld moves nex-moves) this]
::
::  Send the same event to all ships
::
++  blast-event
  |=  ovo=unix-event
  =/  pers  ~(tap by piers)
  |-  ^+  this
  ?~  pers
    this
  =^  moves-dropped  this
    abet:(push-events:(pe p.i.pers) ~[ovo])
  $(pers t.pers)
::
::  Received timer wake
::
++  wake
  |=  [way=wire ~]
  ^-  (quip move _this)
  ?>  ?=([@ *] way)
  =/  who  (,@p (slav %p i.way))
  ~&  [%waking who]
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
  ?>  ?=([@ *] way)
  =/  who  (,@p (slav %p i.way))
  ~&  [%received-httr who]
  %+  turn-ships  ~[who]
  |=  [who=ship thus=_this]
  =.  this  thus
  (take-sigh-tang:(pe who) t.way tan)
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
  ?~  old
    `+>.$
  =+  new=((soft state) u.old)
  ?~  new
    `+>.$
  `+>.$(+<+ u.new)
--
