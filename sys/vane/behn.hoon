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
=|  behn-state
=*  state  -
|=  [our=ship now=@da eny=@uvJ ski=sley]                ::  current invocation
^?
|%                                                      ::  poke+peek pattern
++  call                                                ::  handle request
  |=  $:  hen=duct
          type=*
          wrapped-task=(hobo task:able)
      ==
  ^-  [(list move) _..^$]
  ::
  =/  =task:able
    ?.  ?=(%soft -.wrapped-task)
      wrapped-task
    ((hard task:able) p.wrapped-task)
  ::
  |^  =^  moves  state
        ::
        ?-    -.task
            ::  %crud: error report; hand off to %dill to be printed
            ::
            %crud
          [[hen %slip %d %flog task]~ state]
        ::
            ::  %born: handle urbit restart
            ::
            %born
          ::  store this duct for setting unix wakeup timers
          ::
          =.  unix-duct  hen
          ::  process any elapsed timers and clear and reset :next-wake
          ::
          =^  moves  timers  notify-clients
          (set-wake(next-wake ~) moves)
        ::
            ::  %rest: cancel a timer, resetting :next-wake if needed
            ::
            %rest
          =.  timers  (unset-timer [p.task hen])
          (set-wake ~)
        ::
            ::  %vega: learn of a kernel upgrade
            ::
            %vega
          [~ state]
        ::
            ::  %wait: set a new timer
            ::
            %wait
          ::  process elapsed timers first to maintain sort order
          ::
          =^  moves  timers  notify-clients
          ::  set the timer, then adjust :next-wake if needed
          ::
          =.  timers  (set-timer [p.task hen])
          (set-wake moves)
        ::
            ::  %wake: unix says wake up; notify clients and set next wakeup
            ::
            %wake
          =^  moves  timers  notify-clients
          (set-wake(next-wake ~) moves)
        ::
            ::  %wegh: produce memory usage report for |mass
            ::
            %wegh
          :_  state  :_  ~
          :^  hen  %give  %mass
          :+  %behn  %|
          :~  timers+&+timers
              dot+&+state
          ==
        ==
      ::
      [moves ..^^$]
  ::  +set-timer: set a timer, maintaining the sort order of the :timers list
  ::
  ++  set-timer
    |=  t=timer
    ^+  timers
    ::
    ?~  timers
      ~[t]
    ::  timers at the same date form a fifo queue
    ::
    ?:  (lth date.t date.i.timers)
      [t timers]
    ::
    [i.timers $(timers t.timers)]
  ::  +unset-timer: cancel a timer; if it already expired, no-op
  ::
  ++  unset-timer
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
  ::  +notify-clients: wake up vanes whose timers have expired
  ::
  ::    When we return the list moves to clients, we flop them so they're in
  ::    the same order as they were in :timers.
  ::
  ++  notify-clients
    =|  moves=(list move)
    |-  ^+  [moves timers]
    ::
    ?~  timers
      [(flop moves) timers]
    ::
    ?:  (gth date.i.timers now)
      [(flop moves) timers]
    ::
    %_  $
      timers  t.timers
      moves  [[duct.i.timers %give %wake ~] moves]
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
    |=  moves=(list move)
    ^+  [moves state]
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
  --
::
++  load
  |=  old=*
  ^+  ..^$
  ?^  new=((soft behn-state) old)
    ~&  %behn-load-new
    ..^$(state u.new)
  ~&  %behn-load-wipe
  ..^$(state *behn-state)
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
++  take                                                ::  process move
  |=  [tea=wire hen=duct hin=(hypo sign)]
  ^+  [*(list move) ..^$]
  ~|  %behn-take-not-implemented
  !!
--
