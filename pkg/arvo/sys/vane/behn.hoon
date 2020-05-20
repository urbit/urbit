::  %behn, just a timer
!:
!?  164
::
=,  behn
|=  pit=vase
=>  =<
    |%
    +$  move  [p=duct q=(wite note gift:able)]
    +$  note                                            ::  out request $->
      $~  [%b %wait *@da]                               ::
      $%  $:  %b                                        ::   to self
              $>(%wait task:able)                       ::  set timer
          ==                                            ::
          $:  %d                                        ::    to %dill
              $>(%flog task:able:dill)                  ::  log output
      ==  ==                                            ::
    +$  sign
      $~  [%b %wake ~]
      $%  [%b $>(%wake gift:able)]
      ==
    ::
    +$  behn-state
      $:  %1
          timers=(tree [timer ~])
          unix-duct=duct
          next-wake=(unit @da)
          drips=drip-manager
      ==
    ::
    ::  use lth instead of lte so that if same date, goes after
    ::
    ++  timer-map
      %-  (ordered-map ,timer ,~)
      |=  [a=timer b=timer]
      (lth date.a date.b)
    ::
    +$  drip-manager
      $:  count=@ud
          movs=(map @ud vase)
      ==
    ::
    +$  timer  [date=@da =duct]
    --
|%
::  $mk-item: constructor for +ordered-map item type
::
++  mk-item  |$  [key val]  [key=key val=val]
::  +ordered-map: treap with user-specified horizontal order
::
::    Conceptually smaller items go on the left, so the item with the
::    smallest key can be popped off the head. If $key is `@` and
::    .compare is +lte, then the numerically smallest item is the head.
::
++  ordered-map
  |*  [key=mold val=mold]
  =>  |%
      +$  item  (mk-item key val)
      --
  ::  +compare: item comparator for horizontal order
  ::
  |=  compare=$-([key key] ?)
  |%
  ::  +check-balance: verify horizontal and vertical orderings
  ::
  ++  check-balance
    =|  [l=(unit key) r=(unit key)]
    |=  a=(tree item)
    ^-  ?
    ::  empty tree is valid
    ::
    ?~  a  %.y
    ::  nonempty trees must maintain several criteria
    ::
    ?&  ::  if .n.a is left of .u.l, assert horizontal comparator
        ::
        ?~(l %.y (compare key.n.a u.l))
        ::  if .n.a is right of .u.r, assert horizontal comparator
        ::
        ?~(r %.y (compare u.r key.n.a))
        ::  if .a is not leftmost element, assert vertical order between
        ::  .l.a and .n.a and recurse to the left with .n.a as right
        ::  neighbor
        ::
        ?~(l.a %.y &((mor key.n.a key.n.l.a) $(a l.a, l `key.n.a)))
        ::  if .a is not rightmost element, assert vertical order
        ::  between .r.a and .n.a and recurse to the right with .n.a as
        ::  left neighbor
        ::
        ?~(r.a %.y &((mor key.n.a key.n.r.a) $(a r.a, r `key.n.a)))
    ==
  ::  +put: ordered item insert
  ::
  ++  put
    |=  [a=(tree item) =key =val]
    ^-  (tree item)
    ::  base case: replace null with single-item tree
    ::
    ?~  a  [n=[key val] l=~ r=~]
    ::  base case: overwrite existing .key with new .val
    ::
    ?:  =(key.n.a key)  a(val.n val)
    ::  if item goes on left, recurse left then rebalance vertical order
    ::
    ?:  (compare key key.n.a)
      =/  l  $(a l.a)
      ?>  ?=(^ l)
      ?:  (mor key.n.a key.n.l)
        a(l l)
      l(r a(l r.l))
    ::  item goes on right; recurse right then rebalance vertical order
    ::
    =/  r  $(a r.a)
    ?>  ?=(^ r)
    ?:  (mor key.n.a key.n.r)
      a(r r)
    r(l a(r l.r))
  ::  +peek: produce head (smallest item) or null
  ::
  ++  peek
    |=  a=(tree item)
    ^-  (unit item)
    ::
    ?~  a    ~
    ?~  l.a  `n.a
    $(a l.a)
  ::  +pop: produce .head (smallest item) and .rest or crash if empty
  ::
  ++  pop
    |=  a=(tree item)
    ^-  [head=item rest=(tree item)]
    ::
    ?~  a    !!
    ?~  l.a  [n.a r.a]
    ::
    =/  l  $(a l.a)
    :-  head.l
    ::  load .rest.l back into .a and rebalance
    ::
    ?:  |(?=(~ rest.l) (mor key.n.a key.n.rest.l))
      a(l rest.l)
    rest.l(r a(r r.rest.l))
  ::  +del: delete .key from .a if it exists, producing value iff deleted
  ::
  ++  del
    |=  [a=(tree item) =key]
    ^-  [(unit val) (tree item)]
    ::
    ?~  a  [~ ~]
    ::  we found .key at the root; delete and rebalance
    ::
    ?:  =(key key.n.a)
      [`val.n.a (nip a)]
    ::  recurse left or right to find .key
    ::
    ?:  (compare key key.n.a)
      =+  [found lef]=$(a l.a)
      [found a(l lef)]
    =+  [found rig]=$(a r.a)
    [found a(r rig)]
  ::  +nip: remove root; for internal use
  ::
  ++  nip
    |=  a=(tree item)
    ^-  (tree item)
    ::
    ?>  ?=(^ a)
    ::  delete .n.a; merge and balance .l.a and .r.a
    ::
    |-  ^-  (tree item)
    ?~  l.a  r.a
    ?~  r.a  l.a
    ?:  (mor key.n.l.a key.n.r.a)
      l.a(r $(l.a r.l.a))
    r.a(l $(r.a l.r.a))
  ::  +traverse: stateful partial inorder traversal
  ::
  ::    Mutates .state on each run of .f.  Starts at .start key, or if
  ::    .start is ~, starts at the head (item with smallest key).  Stops
  ::    when .f produces .stop=%.y.  Traverses from smaller to larger
  ::    keys.  Each run of .f can replace an item's value or delete the
  ::    item.
  ::
  ++  traverse
    |*  state=mold
    |=  $:  a=(tree item)
            =state
            f=$-([state item] [(unit val) ? state])
        ==
    ^+  [state a]
    ::  acc: accumulator
    ::
    ::    .stop: set to %.y by .f when done traversing
    ::    .state: threaded through each run of .f and produced by +abet
    ::
    =/  acc  [stop=`?`%.n state=state]
    =<  abet  =<  main
    |%
    ++  abet  [state.acc a]
    ::  +main: main recursive loop; performs a partial inorder traversal
    ::
    ++  main
      ^+  .
      ::  stop if empty or we've been told to stop
      ::
      ?~  a  .
      ?:  stop.acc  .
      ::  inorder traversal: left -> node -> right, until .f sets .stop
      ::
      =>  left
      ?:  stop.acc  .
      =>  node
      ?:  stop.acc  .
      right
    ::  +node: run .f on .n.a, updating .a, .state, and .stop
    ::
    ++  node
      ^+  .
      ::  run .f on node, updating .stop.acc and .state.acc
      ::
      =^  res  acc
        ?>  ?=(^ a)
        (f state.acc n.a)
      ::  apply update to .a from .f's product
      ::
      =.  a
        ::  if .f requested node deletion, merge and balance .l.a and .r.a
        ::
        ?~  res  (nip a)
        ::  we kept the node; replace its .val; order is unchanged
        ::
        ?>  ?=(^ a)
        a(val.n u.res)
      ::
      ..node
    ::  +left: recurse on left subtree, copying mutant back into .l.a
    ::
    ++  left
      ^+  .
      ?~  a  .
      =/  lef  main(a l.a)
      lef(a a(l a.lef))
    ::  +right: recurse on right subtree, copying mutant back into .r.a
    ::
    ++  right
      ^+  .
      ?~  a  .
      =/  rig  main(a r.a)
      rig(a a(r a.rig))
    --
  ::  +tap: convert to list, smallest to largest
  ::
  ++  tap
    |=  a=(tree item)
    ^-  (list item)
    ::
    =|  b=(list item)
    |-  ^+  b
    ?~  a  b
    ::
    $(a l.a, b [n.a $(a r.a)])
  ::  +gas: put a list of items
  ::
  ++  gas
    |=  [a=(tree item) b=(list item)]
    ^-  (tree item)
    ::
    ?~  b  a
    $(b t.b, a (put a i.b))
  ::  +uni: unify two ordered maps
  ::
  ::    .b takes precedence over .a if keys overlap.
  ::
  ++  uni
    |=  [a=(tree item) b=(tree item)]
    ^-  (tree item)
    ::
    ?~  b  a
    ?~  a  b
    ?:  =(key.n.a key.n.b)
      ::
      [n=n.b l=$(a l.a, b l.b) r=$(a r.a, b r.b)]
    ::
    ?:  (mor key.n.a key.n.b)
      ::
      ?:  (compare key.n.b key.n.a)
        $(l.a $(a l.a, r.b ~), b r.b)
      $(r.a $(a r.a, l.b ~), b l.b)
    ::
    ?:  (compare key.n.a key.n.b)
      $(l.b $(b l.b, r.a ~), a r.a)
    $(r.b $(b r.b, l.a ~), a l.a)
  --
--
::
::
=>
~%  %behn  ..is  ~
|%
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
  ::  +crud: handle failure of previous arvo event
  ::
  ++  crud
    |=  [tag=@tas error=tang]
    ^+  [moves state]
    ::  behn must get activated before other vanes in a %wake
    ::
    ?.  =(%wake tag)
      ~&  %behn-crud-not-wake^tag
      [[duct %slip %d %flog %crud tag error]~ state]
    ::
    ?:  =(~ timers.state)
      ~|(%behn-crud-no-timer^tag^error !!)
    ::
    (wake `error)
  ::  +rest: cancel the timer at :date, then adjust unix wakeup
  ::  +wait: set a new timer at :date, then adjust unix wakeup
  ::
  ++  rest  |=(date=@da set-unix-wake(timers.state (unset-timer [date duct])))
  ++  wait  |=(date=@da set-unix-wake(timers.state (set-timer [date duct])))
  ::  +huck: give back immediately
  ::
  ::    Useful if you want to continue working after other moves finish.
  ::
  ++  huck
    |=  mov=vase
    =<  [moves state]
    event-core(moves [duct %give %meta mov]~)
  ::  +drip:  XX
  ::
  ++  drip
    |=  mov=vase
    =<  [moves state]
    ^+  event-core
    =.  moves
      [duct %pass /drip/(scot %ud count.drips.state) %b %wait +(now)]~
    =.  movs.drips.state
      (~(put by movs.drips.state) count.drips.state mov)
    =.  count.drips.state  +(count.drips.state)
    event-core
  ::  +take-drip:  XX
  ::
  ++  take-drip
    |=  [num=@ud error=(unit tang)]
    =<  [moves state]
    ^+  event-core
    =/  drip  (~(got by movs.drips.state) num)
    =.  movs.drips.state  (~(del by movs.drips.state) num)
    =/  =move
      =/  card  [%give %meta drip]
      ?~  error
        [duct card]
      =/  =tang
        (weld u.error `tang`[leaf/"drip failed" ~])
      ::  XX should be
      ::  [duct %hurl fail/tang card]
      ::
      [duct %pass /drip-slog %d %flog %crud %drip-fail tang]
    event-core(moves [move moves])
  ::  +trim: in response to memory pressue
  ::
  ++  trim  [moves state]
  ::  +vega: learn of a kernel upgrade
  ::
  ++  vega  [moves state]
  ::  +wake: unix says wake up; process the elapsed timer and set :next-wake
  ::
  ++  wake
    |=  error=(unit tang)
    ^+  [moves state]
    ::  no-op on spurious but innocuous unix wakeups
    ::
    ?:  =(~ timers.state)
      ~?  ?=(^ error)  %behn-wake-no-timer^u.error
      [moves state]
    ::  if we errored, pop the timer and notify the client vane of the error
    ::
    ?^  error
      =<  set-unix-wake
      =^  [=timer ~]  timers.state  (pop:timer-map timers.state)
      (emit-vane-wake duct.timer error)
    ::  if unix woke us too early, retry by resetting the unix wakeup timer
    ::
    =/  [[=timer ~] timers-tail=(tree [timer ~])]
      (pop:timer-map timers.state)
    ?:  (gth date.timer now)
      set-unix-wake(next-wake.state ~)
    ::  pop first timer, tell vane it has elapsed, and adjust next unix wakeup
    ::
    =<  set-unix-wake
    (emit-vane-wake(timers.state timers-tail) duct.timer ~)
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
  ++  emit-vane-wake
    |=  [=^duct error=(unit tang)]
    event-core(moves [duct %give %wake error]~)
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
    ::  no-op if .unix-duct has not yet been set
    ::
    ?~  unix-duct.state
      event-core
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
    ~%  %set-unix-wake  ..is  ~  |-
    ^+  event-core
    ::
    =*  next-wake  next-wake.state
    =*  timers     timers.state
    ::  if no timers, cancel existing wakeup timer or no-op
    ::
    =/  timer=(unit [timer ~])  (peek:timer-map timers.state)
    ?~  timer
      ?~  next-wake
        event-core
      (emit-doze ~)
    ::  if :next-wake is in the past or not soon enough, reset it
    ::
    ?^  next-wake
      ?:  &((gte date.u.timer u.next-wake) (lte now u.next-wake))
        event-core
      (emit-doze `date.u.timer)
    ::  there was no unix wakeup timer; set one
    ::
    (emit-doze `date.u.timer)
  ::  +set-timer: set a timer, maintaining the sort order of the :timers list
  ::
  ++  set-timer
    ~%  %set-timer  ..is  ~
    |=  t=timer
    ^+  timers.state
    (put:timer-map timers.state t ~)
  ::  +unset-timer: cancel a timer; if it already expired, no-op
  ::
  ++  unset-timer
    |=  t=timer
    ^+  timers.state
    +:(del:timer-map timers.state t)
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
  ~%  %behn-call  ..is  ~
  |=  $:  hen=duct
          dud=(unit goof)
          type=*
          wrapped-task=(hobo task:able)
      ==
  ^-  [(list move) _behn-gate]
  ::
  =/  =task:able  ((harden task:able) wrapped-task)
  ::
  ::  error notifications "downcast" to %crud
  ::
  =?  task  ?=(^ dud)
    ~|  %crud-in-crud
    ?<  ?=(%crud -.task)
    [%crud -.task tang.u.dud]
  ::
  =/  event-core  (per-event [our now hen] state)
  ::
  =^  moves  state
    ?-  -.task
      %born  born:event-core
      %crud  (crud:event-core [p q]:task)
      %rest  (rest:event-core date=p.task)
      %drip  (drip:event-core move=p.task)
      %huck  (huck:event-core move=p.task)
      %trim  trim:event-core
      %vega  vega:event-core
      %wait  (wait:event-core date=p.task)
      %wake  (wake:event-core error=~)
      %wegh  wegh:event-core
    ==
  [moves behn-gate]
::  +load: migrate an old state to a new behn version
::
++  load
  |^
  |=  old=state
  ^+  behn-gate
  =?  old  ?=(^ -.old)
    (ket-to-1 old)
  =?  old  ?=(~ -.old)
    (load-0-to-1 old)
  ?>  ?=(%1 -.old)
  behn-gate(state old)
  ::
  ++  state
    $^  behn-state-ket
    $%  behn-state-0
        behn-state
    ==
  ::
  +$  behn-state-0
    $:  ~
        unix-duct=duct
        next-wake=(unit @da)
        drips=drip-manager
    ==
  ::
  +$  behn-state-ket
    $:  timers=(list timer)
        unix-duct=duct
        next-wake=(unit @da)
        drips=drip-manager
    ==
  ::
  ++  ket-to-1
    |=  old=behn-state-ket
    ^-  behn-state
    :-  %1
    %=    old
        timers
      %+  gas:timer-map  *(tree [timer ~])
      (turn timers.old |=(=timer [timer ~]))
    ==
  ::
  ++  load-0-to-1
    |=  old=behn-state-0
    ^-  behn-state
    [%1 old]
  --
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
  ?.  ?=(%timers syd)
    [~ ~]
  [~ ~ %noun !>((turn (tap:timer-map timers) head))]
::
++  stay  state
++  take
  |=  [tea=wire hen=duct dud=(unit goof) hin=(hypo sign)]
  ^-  [(list move) _behn-gate]
  ?^  dud
    ~|(%behn-take-dud (mean tang.u.dud))
  ::
  ?>  ?=([%drip @ ~] tea)
  =/  event-core  (per-event [our now hen] state)
  =^  moves  state
    (take-drip:event-core (slav %ud i.t.tea) error.q.hin)
  [moves behn-gate]
--
