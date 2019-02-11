::  %behn, just a timer
!:
!?  164
::
=,  behn
|=  pit=vase
=>  |%
    +$  move  [p=duct q=(wind note:able gift:able)]
    +$  sign  ~
    ::
    +$  behn-state
      $:  timers=(list timer)
          unix-duct=duct
          next-wake=(unit @da)
      ==
    ::
    +$  timer  [date=@da =duct]
    --
::
=>  |%
++  per-event
  =|  moves=(list move)
  |=  [[our=ship now=@da =duct] state=behn-state]
  ::
  |%
  ::  %entry-points
  ::
  ::  +born: urbit restarted; refresh :next-wake and store wakeup timer duct
  ::
  ++  born  set-unix-wake(next-wake.state ~, unix-duct.state duct)
  ::  +crud: error report; hand off to %dill to be printed
  ::
  ++  crud
    |=  [p=@tas q=tang]
    ^+  [moves state]
    [[duct %slip %d %flog %crud p q]~ state]
  ::  +rest: cancel the timer at :date, then adjust unix wakeup
  ::  +wait: set a new timer at :date, then adjust unix wakeup
  ::
  ++  rest  |=(date=@da set-unix-wake(timers.state (unset-timer [date duct])))
  ++  wait  |=(date=@da set-unix-wake(timers.state (set-timer [date duct])))
  ::  +vega: learn of a kernel upgrade
  ::
  ++  vega  [moves state]
  ::  +wake: unix says wake up; process the elapsed timer and set :next-wake
  ::
  ++  wake
    ^+  [moves state]
    ::
    ?~  timers.state  ~|(%behn-wake-no-timer !!)
    ::  if unix woke us too early, retry by resetting the unix wakeup timer
    ::
    ?:  (gth date.i.timers.state now)
      ~?  debug=%.n  [%behn-wake-too-soon `@dr`(sub date.i.timers.state now)]
      set-unix-wake(next-wake.state ~)
    ::  pop first timer, tell vane it has elapsed, and adjust next unix wakeup
    ::
    =<  set-unix-wake
    (emit-vane-wake(timers.state t.timers.state) duct.i.timers.state)
  ::  +wegh: produce memory usage report for |mass
  ::
  ++  wegh
    ^+  [moves state]
    :_  state  :_  ~
    :^  duct  %give  %mass
    :+  %behn  %|
    :~  timers+&+timers.state
        dot+&+state
    ==
  ::  %utilities
  ::
  ::+|
  ::
  ++  event-core  .
  ::  +emit-vane-wake: produce a move to wake a vane; assumes no prior moves
  ::
  ++  emit-vane-wake  |=(=^duct event-core(moves [duct %give %wake ~]~))
  ::  +emit-doze: set new unix wakeup timer in state and emit move to unix
  ::
  ::    We prepend the unix %doze event so that it is handled first. Arvo must
  ::    handle this first because the moves %behn emits will get handled in
  ::    depth-first order. If we're handling a %wake which causes a move to a
  ::    different vane and a %doze event to send to unix, Arvo needs to process
  ::    the %doze first because otherwise if the move to the other vane calls
  ::    back into %behn and emits a second %doze, the second %doze would be
  ::    handled by unix first which is incorrect.
  ::
  ++  emit-doze
    |=  =date=(unit @da)
    ^+  event-core
    ::  make sure we don't try to wake up in the past
    ::
    =?  date-unit  ?=(^ date-unit)  `(max now u.date-unit)
    ::
    %_  event-core
      next-wake.state  date-unit
      moves            [[unix-duct.state %give %doze date-unit] moves]
    ==
  ::  +set-unix-wake: set or unset next unix wakeup timer based on :i.timers
  ::
  ++  set-unix-wake
    =<  [moves state]
    ^+  event-core
    ::
    =*  next-wake  next-wake.state
    =*  timers     timers.state
    ::  if no timers, cancel existing wakeup timer or no-op
    ::
    ?~  timers
      ?~  next-wake
        event-core
      (emit-doze ~)
    ::  if :next-wake is in the past or not soon enough, reset it
    ::
    ?^  next-wake
      ?:  &((gte date.i.timers u.next-wake) (lte now u.next-wake))
        event-core
      (emit-doze `date.i.timers)
    ::  there was no unix wakeup timer; set one
    ::
    (emit-doze `date.i.timers)
  ::  +set-timer: set a timer, maintaining the sort order of the :timers list
  ::
  ++  set-timer
    =*  timers  timers.state
    |=  t=timer
    ^+  timers
    ::
    ?~  timers
      ~[t]
    ::  ignore duplicates
    ::
    ?:  =(t i.timers)
      ~?  debug=%.n  [%behn-set-duplicate t]
      timers
    ::  timers at the same date form a fifo queue
    ::
    ?:  (lth date.t date.i.timers)
      [t timers]
    ::
    [i.timers $(timers t.timers)]
  ::  +unset-timer: cancel a timer; if it already expired, no-op
  ::
  ++  unset-timer
    =*  timers  timers.state
    |=  t=timer
    ^+  timers
    ::  if we don't have this timer, no-op
    ::
    ?~  timers
      ~?  debug=%.n  [%behn-unset-missing t]
      ~
    ?:  =(i.timers t)
      t.timers
    ::
    [i.timers $(timers t.timers)]
  --
--
::
=|  behn-state
=*  state  -
|=  [our=ship now=@da eny=@uvJ ski=sley]
=*  behn-gate  .
^?
|%
::  +call: handle a +task:able:behn request
::
++  call
  |=  $:  hen=duct
          type=*
          wrapped-task=(hobo task:able)
      ==
  ^-  [(list move) _behn-gate]
  ::
  =/  =task:able
    ?.  ?=(%soft -.wrapped-task)
      wrapped-task
    ((hard task:able) p.wrapped-task)
  ::
  =/  event-core  (per-event [our now hen] state)
  ::
  =^  moves  state
    ?-  -.task
      %born  born:event-core
      %crud  (crud:event-core [p q]:task)
      %rest  (rest:event-core date=p.task)
      %vega  vega:event-core
      %wait  (wait:event-core date=p.task)
      %wake  wake:event-core
      %wegh  wegh:event-core
    ==
  [moves behn-gate]
::  +load: migrate an old state to a new behn version
::
++  load
  |=  old=*
  ^+  behn-gate
  ::
  ~|  %behn-load-fail
  behn-gate(state (behn-state old))
::  +scry: view timer state
::
::    TODO: not referentially transparent w.r.t. elapsed timers,
::    which might or might not show up in the product
::
++  scry
  |=  [fur=(unit (set monk)) ren=@tas why=shop syd=desk lot=coin tyl=path]
  ^-  (unit (unit cage))
  ::
  ?.  ?=(%& -.why)
    ~
  [~ ~ %tank !>(>timers<)]
::
++  stay  state
++  take
  |=  [tea=wire hen=duct hin=(hypo sign)]
  ^-  [(list move) _behn-gate]
  ~|  %behn-take-not-implemented
  !!
--

