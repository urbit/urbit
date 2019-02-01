::  usage:
::  /-  pill
::  =p .^(pill:pill %cx %/urbit/pill)
::  |start %here
::  :here &pill p
::  :here %init
::  :here [%dojo "+ls %"]
::  :here [%dojo "our"]
::
::  TODO:
::  - proper ames routing
::  - save pier point by label
::  - allow cancelling timer
::  - snapshot should keep track of outstanding timers
::  - %init should cancel outstanding timers
::  - allow pausing timer
::  - all commands should allow multiple ships
/-  pill
=,  pill
=>  $~  |%
    ++  move  (pair bone card)
    ++  card
      $%  [%turf wire ~]
          [%vein wire]
          [%look wire src=(each ship purl:eyre)]
          [%wind wire p=@ud]
          [%snap wire snap=snapshot:jael kick=?]
          [%wait wire p=@da]
          [%rest wire p=@da]
      ==
    ++  unix-effect
      %+  pair  wire
      $%  [%blit p=(list blit:dill)]
          [%send p=lane:ames q=@]
          [%doze p=(unit @da)]
      ==
    ++  state
      $:  pil=pill
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
      ==
    --
=,  gall
|_  $:  hid/bowl
        state
    ==
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
  ++  start-processing-events  .(processing-events &)
  ++  stop-processing-events  .(processing-events |)
  ++  mox  |=(* (mock [snap +<] scry))
  ::
  ++  handle-effects
    |=  effects=(list ovum)
    ^+  ..abet
    ?~  effects
      ..abet
    =.  ..abet
      =/  sof  ((soft unix-effect) i.effects)
      ?~  sof
        ~&  [%unknown-effect i.effects]
        ..abet
      ?-    -.q.u.sof
          %blit
        =/  last-line
          %+  roll  p.q.u.sof
          |=  [b=blit:dill line=tape]
          ?-    -.b
              %lin  (tape p.b)
              %mor  ~&  line  ""
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
          %send  (handle-send u.sof)
          %doze  (handle-doze u.sof)
      ==
    $(effects t.effects)
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
    ?.  &(=(0 (rsh u.dest-ip 0 16)) =(1 (rsh u.dest-ip 0 8)))
      ~&  [%havent-implemented-direct-lanes who lan]
      ..abet
    =/  her=ship  (dis u.dest-ip 0xff)
    =/  hear  [//newt/0v1n.2m9vh %hear lan pac]~
    ~&  [%sending who=who her=her]
    =^  ms  this
      abet:(push-events:(pe her) hear)
    (emit-moves ms)
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
  --
++  this  .
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
    ~&  [%no-new-events p.i.pers]
    $(pers t.pers)
  ~&  plowing=who
  ?~  who
    `this
  =^  moves  this  abet:plow:(pe u.who)
  =/  nex  $
  nex(- (weld -.nex moves))
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
++  poke-noun
  |=  val=*
  ^-  (quip move _this)
  ?+  val  ~|(%bad-noun-arg !!)
      [%init whos=*]
    =/  whos  ((list ship) whos.val)
    |-  ^-  (quip move _this)
    ?~  whos
      `this
    ?~  userspace-ova.pil
      ~&  %no-userspace
      `this
    =+  who=i.whos
    ~&  [%initting who]
    =>  .(this ^+(this this))
    =^  moves  this
      =<  abet:plow
      %-  push-events:apex:(pe who)
      ^-  (list unix-event)
      :~
          `unix-event`[/ %wack 0]  ::  eny
          `unix-event`[/ %whom who]  ::  eny
          `unix-event`[//newt/0v1n.2m9vh %barn ~]
          `unix-event`[//behn/0v1n.2m9vh %born ~]
          `unix-event`[//term/1 %boot %fake who]
          `unix-event`-.userspace-ova.pil
          `unix-event`[//http/0v1n.2m9vh %live 8.080 `8.445]
          `unix-event`[//term/1 %belt %ctl `@c`%x]
      ==
    =^  moves-all  this  plow-all
    =/  nex  $(whos t.whos)
    nex(- (weld -.nex (weld moves moves-all)))
  ::
      [%dojo who=@p p=*]
    =^  moves  this
      =<  abet:plow
      %-  push-events:(pe who.val)
      ^-  (list unix-event)
      :~  
          [//term/1 %belt %ctl `@c`%e]
          [//term/1 %belt %ctl `@c`%u]
          [//term/1 %belt %txt ((list @c) (tape p.val))]
          [//term/1 %belt %ret ~]
      ==
    =^  moves-all  this  plow-all
    [(weld moves moves-all) this]
  ::
      [%snap-fleet lab=@tas]
    =.  fleet-snaps  (~(put by fleet-snaps) lab.val piers)
    `this
  ::
      [%restore-fleet lab=@tas]
    =.  piers  (~(got by fleet-snaps) lab.val)
    `this
  ::
      [%peek who=@p p=*]
    ::  =+  res=(mox +46.snap)
    ::  ?>  ?=(%0 -.res)
    ::  =+  peek=p.res
    ::  ~&  (slum peek p.val)
    `this
  ::
      [%wish who=@p p=@t]
    ::  =+  res=(mox +22.snap)
    ::  ?>  ?=(%0 -.res)
    ::  =+  wish=p.res
    ::  ~&  (slum wish p.val)
    `this
  ::
      %clear-next
    ::  =.  next-events  ~
    `this
  ::
      [%unpause-events hers=*]
    %+  execute-turn  ((list ship) hers.val)
    |=  who=ship
    start-processing-events:(pe who)
  ::
      [%pause-events hers=*]
    %+  execute-turn  ((list ship) hers.val)
    |=  who=ship
    stop-processing-events:(pe who)
  ==
::
++  execute-turn
  |=  [hers=(list ship) fun=$-([ship] _(pe))]
  |-  ^-  (quip move _this)
  ?~  hers
    =^  moves  this  plow-all
    [moves this]
  =^  moves  this
    abet:plow:(fun i.hers)
  =/  nex  $(hers t.hers)
  nex(- (weld moves -.nex))
::
++  wake
  |=  [way=wire ~]
  ^-  (quip move _this)
  ?>  ?=([@ ~] way)
  =/  who  (,@p (slav %p i.way))
  ~&  [%waking who]
  =^  moves  this
    =<  abet:plow
    %-  push-events:(pe who)
    ^-  (list unix-event)
    :~  [//behn/0v1n.2m9vh %wake ~]
    ==
  =^  moves-all  this  plow-all
  [(weld moves moves-all) this]
::
++  scry  |=([* *] ~)
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
