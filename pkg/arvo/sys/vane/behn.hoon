::  %behn, just a timer
!:
!?  164
::
=,  behn
|=  our=ship
=>  |%
    +$  move  [p=duct q=(wite note gift)]
    +$  note                                            ::  out request $->
      $~  [%b %wait *@da]                               ::
      $%  $:  %b                                        ::   to self
              $>(%wait task)                       ::  set timer
          ==                                            ::
          $:  %d                                        ::    to %dill
              $>(%flog task:dill)                  ::  log output
      ==  ==                                            ::
    +$  sign
      $~  [%behn %wake ~]
      $%  [%behn $>(%wake gift)]
      ==
    ::
    +$  behn-state
      $:  %2
          timers=(tree [key=@da val=(qeu duct)])
          unix-duct=duct
          next-wake=(unit @da)
          drips=drip-manager
      ==
    ::
    ++  timer-map  ((ordered-map ,@da ,(qeu duct)) lte)
    ::
    +$  drip-manager
      $:  count=@ud
          movs=(map @ud vase)
      ==
    ::
    +$  timer  [date=@da =duct]
    --
::
=>
~%  %behn  ..part  ~
|%
++  per-event
  =|  moves=(list move)
  |=  [[now=@da =duct] state=behn-state]
  ::
  |%
  ::
  +|  %helpers
  ::
  ++  this  .
  ++  emit  |=(m=move this(moves [m moves]))
  ++  abet
    ^+  [moves state]
    ::  moves are statefully pre-flopped to ensure that
    ::  any prepended %doze is emitted first
    ::
    =.  moves  (flop moves)
    =/  new=(unit @da)  (bind (pry:timer-map timers.state) head)
    ::  emit %doze if needed
    ::
    =?    ..this
        ?~  unix-duct.state  |
        =/  dif=[old=(unit @da) new=(unit @da)]  [next-wake.state new]
        ?+  dif  ~|([%unpossible dif] !!)
          [~ ~]  |                        :: no-op
          [~ ^]  &                        :: set
          [^ ~]  &                        :: clear
          [^ ^]  !=(u.old.dif u.new.dif)  :: set if changed
        ==
      (emit(next-wake.state new) [unix-duct.state %give %doze new])
    ::
    [moves state]
  ::
  +|  %entry-points
  ::
  ++  call
    |=  [=task error=(unit tang)]
    ^+  this
    ?:  ?&  ?=(^ error)
            !?=(%wake -.task)
        ==
      ::  XX more and better error handling
      ::
      ~&  %behn-crud-not-wake^-.task
      (emit [duct %slip %d %flog %crud -.task u.error])
    ::
    ?-  -.task
      %born  this(next-wake.state ~, unix-duct.state duct)
      %drip  (drip p.task)
      %huck  (emit [duct %give %heck syn.task])
      %rest  this(timers.state (unset-timer [p.task duct]))
      %trim  this
      %vega  this
      %wait  this(timers.state (set-timer [p.task duct]))
      %wake  (wake(next-wake.state ~) error)
    ==
  ::
  ::  +take-drip: the future is now, %give the deferred move
  ::
  ++  take-drip
    |=  [num=@ud error=(unit tang)]
    ^+  this
    =/  drip  (~(got by movs.drips.state) num)
    %-  emit(movs.drips.state (~(del by movs.drips.state) num))
    =/  card  [%give %meta drip]
    ?~  error
      [duct card]
    =/  =tang
      (weld u.error `tang`[leaf/"drip failed" ~])
    ::  XX we don't know the mote due to the %wake pattern
    ::
    [duct %hurl fail/tang card]
  ::
  +|  %tasks
  ::
  ::  +drip: enqueue a future gift (as a vase), %pass ourselves a %wait
  ::
  ++  drip
    |=  vax=vase
    ^+  this
    %.  [duct %pass /drip/(scot %ud count.drips.state) %b %wait +(now)]
    %=  emit
      movs.drips.state   (~(put by movs.drips.state) count.drips.state vax)
      count.drips.state  +(count.drips.state)
    ==
  ::
  ::  +wake: unix says wake up; process the elapsed timer (or forward error)
  ::
  ++  wake
    |=  error=(unit tang)
    ^+  this
    ?:  =(~ timers.state)
      ::  no-op on spurious but innocuous unix wakeups
      ::
      ~?  ?=(^ error)  %behn-wake-no-timer^u.error
      this
    =/  [=timer later-timers=_timers.state]  pop-timer
    ?:  (gth date.timer now)
      ::  no-op if timer is early, (+abet will reset)
      ::
      this
    ::  pop the first timer and notify client vane,
    ::  forwarding error if present
    ::
    ::    XX %wake errors should be signaled out-of-band
    ::    [duct.timer %hurl goof %give %wake ~]
    ::
    (emit(timers.state later-timers) [duct.timer %give %wake error])
  ::
  +|  %implementation
  ::
  ::  +pop-timer: dequeue and produce earliest timer
  ::
  ++  pop-timer
    ^+  [*timer timers.state]
    =^  [date=@da dux=(qeu ^duct)]  timers.state  (pop:timer-map timers.state)
    =^  dut  dux  ~(get to dux)
    :-  [date dut]
    ?:  =(~ dux)
      timers.state
    (put:timer-map timers.state date dux)
  ::  +set-timer: set a timer, maintaining order
  ::
  ++  set-timer
    ~%  %set-timer  ..part  ~
    |=  t=timer
    ^+  timers.state
    =/  found  (find-ducts date.t)
    (put:timer-map timers.state date.t (~(put to found) duct.t))
  ::  +find-ducts: get timers at date
  ::
  ::    TODO: move to +ordered-map
  ::
  ++  find-ducts
    |=  date=@da
    ^-  (qeu ^duct)
    ?~  timers.state  ~
    ?:  =(date key.n.timers.state)
      val.n.timers.state
    ?:  (lte date key.n.timers.state)
      $(timers.state l.timers.state)
    $(timers.state r.timers.state)
  ::  +unset-timer: cancel a timer; if it already expired, no-op
  ::
  ++  unset-timer
    |=  t=timer
    ^+  timers.state
    =/  [found=? dux=(qeu ^duct)]
      =/  dux  (find-ducts date.t)
      |-  ^-  [found=? dux=(qeu ^duct)]
      ?~  dux  |+~
      ?:  =(duct.t n.dux)  &+~(nip to `(qeu ^duct)`dux)
      =^  found-left=?  l.dux  $(dux l.dux)
      ?:  found-left  &+dux
      =^  found-rite=?  r.dux  $(dux r.dux)
      [found-rite dux]
    ?.  found  timers.state
    ?:  =(~ dux)
      +:(del:timer-map timers.state date.t)
    (put:timer-map timers.state date.t dux)
  --
--
::
=|  behn-state
=*  state  -
|=  [now=@da eny=@uvJ rof=roof]
=*  behn-gate  .
^?
|%
::  +call: handle a +task:behn request
::
++  call
  ~%  %behn-call  ..part  ~
  |=  $:  hen=duct
          dud=(unit goof)
          wrapped-task=(hobo task)
      ==
  ^-  [(list move) _behn-gate]
  =/  =task  ((harden task) wrapped-task)
  =/  event-core  (per-event [now hen] state)
  =^  moves  state
    abet:(call:event-core task ?~(dud ~ `tang.u.dud))
  [moves behn-gate]
::  +load: migrate an old state to a new behn version
::
++  load
  |=  old=behn-state
  ^+  behn-gate
  behn-gate(state old)
::  +scry: view timer state
::
::    TODO: not referentially transparent w.r.t. elapsed timers,
::    which might or might not show up in the product
::
++  scry
  ^-  roon
  |=  [lyc=gang pov=path car=term bem=beam]
  ^-  (unit (unit cage))
  =*  ren  car
  =*  why=shop  &/p.bem
  =*  syd  q.bem
  =*  lot=coin  $/r.bem
  =*  tyl  s.bem
  ::
  ::  only respond for the local identity, %$ desk, current timestamp
  ::
  ?.  ?&  =(&+our why)
          =([%$ %da now] lot)
          =(%$ syd)
      ==
    ~
  ::  /bx//whey         (list mass)        memory usage labels
  ::  /bx/debug/timers  (list [@da duct])  all timers and their ducts
  ::  /bx/timers        (list @da)         all timer timestamps
  ::  /bx/timers/next   (unit @da)         the very next timer to fire
  ::  /bx/timers/[da]   (list @da)         all timers up to and including da
  ::
  ?.  ?=(%x ren)  ~
  ?+  tyl  [~ ~]
      [%$ %whey ~]
    =/  maz=(list mass)
      :~  timers+&+timers.state
      ==
    ``mass+!>(maz)
  ::
      [%debug %timers ~]
    :^  ~  ~  %noun
    !>  ^-  (list [@da duct])
    %-  zing
    %+  turn  (tap:timer-map timers)
    |=  [date=@da q=(qeu duct)]
    %+  turn  ~(tap to q)
    |=(d=duct [date d])
  ::
      [%timers ~]
    :^  ~  ~  %noun
    !>  ^-  (list @da)
    %-  zing
    %+  turn  (tap:timer-map timers)
    |=  [date=@da q=(qeu duct)]
    (reap ~(wyt in q) date)
  ::
      [%timers %next ~]
    :^  ~  ~  %noun
    !>  ^-  (unit @da)
    (bind (pry:timer-map timers) head)
  ::
      [%timers @ ~]
    ?~  til=(slaw %da i.t.tyl)
      [~ ~]
    :^  ~  ~  %noun
    !>  ^-  (list @da)
    =/  tiz=(list [date=@da q=(qeu duct)])
      (tap:timer-map timers)
    |-  ^-  (list @da)
    ?~  tiz  ~
    ?:  (gth date.i.tiz u.til)  ~
    %+  weld
      (reap ~(wyt in q.i.tiz) date.i.tiz)
    $(tiz t.tiz)
  ==
::
++  stay  state
++  take
  |=  [tea=wire hen=duct dud=(unit goof) hin=sign]
  ^-  [(list move) _behn-gate]
  ?^  dud
    ~|(%behn-take-dud (mean tang.u.dud))
  ::
  ?>  ?=([%drip @ ~] tea)
  =/  event-core  (per-event [now hen] state)
  =^  moves  state
    abet:(take-drip:event-core (slav %ud i.t.tea) error.hin)
  [moves behn-gate]
--
