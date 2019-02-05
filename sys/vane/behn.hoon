::  %behn, just a timer
::
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
  ::  +born: handle urbit restart
  ::
  ++  born
    ^+  [moves state]
    ::  store this duct for setting unix wakeup timers
    ::
    =.  unix-duct.state  duct
    ::  process any elapsed timers and clear and reset :next-wake
    ::
    =>  notify-clients
    set-wake(next-wake.state ~)
  ::  +crud: error report; hand off to %dill to be printed
  ::
  ++  crud
    |=  [p=@tas q=tang]
    ^+  [moves state]
    [[duct %slip %d %flog %crud p q]~ state]
  ::  +rest: cancel the timer at :date, resetting :next-wake if needed
  ::
  ++  rest
    |=  date=@da
    ^+  [moves state]
    ::
    =.  timers.state  (unset-timer [date duct])
    set-wake
  ::  +vega: learn of a kernel upgrade
  ::
  ++  vega
    [moves state]
  ::  +wait: set a new timer at :date, resetting :next-wake if needed
  ::
  ++  wait
    |=  date=@da
    ^+  [moves state]
    ::  process elapsed timers first to maintain sort order
    ::
    =.  event-core    notify-clients
    =.  timers.state  (set-timer [date duct])
    set-wake
  ::  +wake: unix says we should wake up; notify clients and set :next-wake
  ::
  ++  wake
    ^+  [moves state]
    =>  notify-clients
    set-wake(next-wake.state ~)
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
  ::  +notify-clients: wake up vanes whose timers have expired
  ::
  ::    When we return the list moves to clients, we flop them so they're in
  ::    the same order as they were in :timers.
  ::
  ++  notify-clients
    =*  timers  timers.state
    |-  ^+  event-core
    ::
    ?~  timers
      =.  moves  (flop moves)
      event-core
    ::
    ?:  (gth date.i.timers now)
      =.  moves  (flop moves)
      event-core
    ::
    %_  $
      timers  t.timers
      moves   [[duct.i.timers %give %wake ~] moves]
    ==
  ::  +set-wake: set or unset a unix timer to wake us when next timer expires
  ::
  ::    We prepend the unix %doze event so that it is handled first. Arvo must
  ::    handle this first because the moves %behn emits will get handled in
  ::    depth-first order. If we're handling a %wake which causes a move to a
  ::    different vane and a %doze event to send to unix, Arvo needs to process
  ::    the %doze first because otherwise if the move to the other vane calls
  ::    back into %behn and emits a second %doze, the second %doze would be
  ::    handled by unix first which is incorrect.
  ::
  ++  set-wake
    ^+  [moves state]
    ::
    =*  next-wake  next-wake.state
    =*  timers     timers.state
    =*  unix-duct  unix-duct.state
    ::  if no timers, cancel existing wakeup timer or no-op
    ::
    ?~  timers
      ?~  next-wake
        [moves state]
      :_  state(next-wake ~)
      [[unix-duct %give %doze ~] moves]
    ::  if :next-wake is in the past or not soon enough, reset it
    ::
    ?^  next-wake
      ?:  &((gte date.i.timers u.next-wake) (lte now u.next-wake))
        [moves state]
      :_  state(next-wake `date.i.timers)
      [[unix-duct %give %doze `date.i.timers] moves]
    ::  there was no unix wakeup timer; set one
    ::
    :_  state(next-wake `date.i.timers)
    [[unix-duct %give %doze `date.i.timers] moves]
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
    |=  [t=timer]
    ^+  timers
    ::  if we don't have this timer, no-op; for debugging, add a printf here
    ::
    ?~  timers
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

