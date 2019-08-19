/-  tapp-sur=tapp
/+  async
|*  $:  state-type=mold
        peek-data=mold
        in-poke-data=mold
        out-poke-data=mold
        in-peer-data=mold
        out-peer-data=mold
    ==
|%
++  tapp-sur  (^tapp-sur out-poke-data out-peer-data)
++  card  card:tapp-sur
++  sign  sign:tapp-sur
++  contract  contract:tapp-sur
+$  tapp-admin-in-poke-data
  [%tapp-admin tapp-admin=?(%cancel %restart)]
+$  tapp-in-poke-data
  $%  tapp-admin-in-poke-data
      in-poke-data
  ==
+$  command
  $%  [%init ~]
      [%poke =in-poke-data]
      [%peer =path]
      [%diff =dock =path =in-peer-data]
      [%take =sign]
  ==
::
++  async-lib  (^async sign card contract)
++  async  async:async-lib
::
+$  move  (pair bone card)
++  tapp-async  (async state-type)
+$  tapp-state
  $:  waiting=(qeu [=bone command])
      active=(unit eval-form:eval:tapp-async)
      app-state=state-type
  ==
+$  tapp-peek
  [%noun ?(? (set contract))]
::
::  The form of a tapp
::
+$  tapp-core-all
  $_  ^|
  |_  [bowl:gall state-type]
  ::
  ::  Initialization
  ::
  ++  handle-init
    *form:tapp-async
  ::
  ::  Input
  ::
  ++  handle-poke
    |~  in-poke-data
    *form:tapp-async
  ::
  ::  Read
  ::
  ++  handle-peek
    |~  path
    *(unit (unit peek-data))
  ::
  ::  Subscription request
  ::
  ++  handle-peer
    |~  path
    *form:tapp-async
  ::
  ::  Receive subscription result
  ::
  ++  handle-diff
    |~  [dock path in-peer-data]
    *form:tapp-async
  ::
  ::  Receive syscall result
  ::
  ++  handle-take
    |~  sign
    *form:tapp-async
  --
::
::  Default handlers for all comands
::
++  default-tapp
  =/  m  tapp-async
  ^-  tapp-core-all
  |_  [=bowl:gall state=state-type]
  ++  handle-init
    (pure:m state)
  ::
  ++  handle-poke
    |=(* (async-fail:async-lib %no-poke-handler ~))
  ::
  ++  handle-peek  _~
  ::
  ++  handle-peer
    |=  =path
    ^-  form:m
    ?:  ?=([%sole *] path)
      ~|  %default-tapp-no-sole  !!
    (async-fail:async-lib %no-peer-handler >path< ~)
  ::
  ++  handle-diff
    |=(* (async-fail:async-lib %no-diff-handler ~))
  ::
  ++  handle-take
    =>  |%
        ++  print-if-error
          |=  [msg=tape error=(unit tang)]
          %.  (pure:m state)
          ?~  error
            same
          (slog [leaf+msg u.error])
        --
    |=  =sign
    ^-  form:m
    ?:  ?=(%coup -.sign)
      (print-if-error "poke failed" error.sign)
    ?:  ?=(%reap -.sign)
      (print-if-error "peer {<path.sign>} failed" error.sign)
    (async-fail:async-lib %no-take-handler ~)
  --
::
::  The form of a tapp that only handles pokes
::
++  tapp-core-poke
  $_  ^|
  |_  [bowl:gall state-type]
  ++  handle-poke  handle-poke:*tapp-core-all
  --
::
++  create-tapp-poke
  |=  handler=tapp-core-poke
  %-  create-tapp-poke-peer
  |_  [=bowl:gall state=state-type]
  ++  handle-peer  ~(handle-peer default-tapp bowl state)
  ::
  ++  handle-poke  ~(handle-poke handler bowl state)
  --
::
::  The form of a tapp that only handles pokes and peers
::
++  tapp-core-poke-peer
  $_  ^|
  |_  [bowl:gall state-type]
  ++  handle-poke  handle-poke:*tapp-core-all
  ++  handle-peer  handle-peer:*tapp-core-all
  --
::
++  create-tapp-poke-peer
  |=  handler=tapp-core-poke-peer
  %-  create-tapp-all
  |_  [=bowl:gall state=state-type]
  ++  handle-init  ~(handle-init default-tapp bowl state)
  ++  handle-peek  ~(handle-peek default-tapp bowl state)
  ++  handle-diff  ~(handle-diff default-tapp bowl state)
  ++  handle-take  ~(handle-take default-tapp bowl state)
  ::
  ++  handle-poke  ~(handle-poke handler bowl state)
  ++  handle-peer  ~(handle-peer handler bowl state)
  --
::
::  The form of a tapp that only handles pokes and diffs
::
++  tapp-core-poke-diff
  $_  ^|
  |_  [bowl:gall state-type]
  ++  handle-poke  handle-poke:*tapp-core-all
  ++  handle-diff  handle-diff:*tapp-core-all
  --
::
++  create-tapp-poke-diff
  |=  handler=tapp-core-poke-diff
  %-  create-tapp-all
  |_  [=bowl:gall state=state-type]
  ++  handle-init  ~(handle-init default-tapp bowl state)
  ++  handle-peek  ~(handle-peek default-tapp bowl state)
  ++  handle-peer  ~(handle-peer default-tapp bowl state)
  ++  handle-take  ~(handle-take default-tapp bowl state)
  ::
  ++  handle-poke  ~(handle-poke handler bowl state)
  ++  handle-diff  ~(handle-diff handler bowl state)
  --
::
::  The form of a tapp that only handles pokes, peers, and takes
::
++  tapp-core-poke-peer-take
  $_  ^|
  |_  [bowl:gall state-type]
  ++  handle-poke  handle-poke:*tapp-core-all
  ++  handle-peer  handle-peer:*tapp-core-all
  ++  handle-take  handle-take:*tapp-core-all
  --
::
++  create-tapp-poke-peer-take
  |=  handler=tapp-core-poke-peer-take
  %-  create-tapp-all
  |_  [=bowl:gall state=state-type]
  ++  handle-init  ~(handle-init default-tapp bowl state)
  ++  handle-peek  ~(handle-peek default-tapp bowl state)
  ++  handle-diff  ~(handle-diff default-tapp bowl state)
  ::
  ++  handle-poke  ~(handle-poke handler bowl state)
  ++  handle-peer  ~(handle-peer handler bowl state)
  ++  handle-take  ~(handle-take handler bowl state)
  --
::
::  The form of a tapp that only handles pokes, peers, diffs, and takes
::
++  tapp-core-poke-peer-diff-take
  $_  ^|
  |_  [bowl:gall state-type]
  ++  handle-poke  handle-poke:*tapp-core-all
  ++  handle-peer  handle-peer:*tapp-core-all
  ++  handle-diff  handle-diff:*tapp-core-all
  ++  handle-take  handle-take:*tapp-core-all
  --
::
++  create-tapp-poke-peer-diff-take
  |=  handler=tapp-core-poke-peer-diff-take
  %-  create-tapp-all
  |_  [=bowl:gall state=state-type]
  ++  handle-init  ~(handle-init default-tapp bowl state)
  ++  handle-peek  ~(handle-peek default-tapp bowl state)
  ::
  ++  handle-poke  ~(handle-poke handler bowl state)
  ++  handle-peer  ~(handle-peer handler bowl state)
  ++  handle-diff  ~(handle-diff handler bowl state)
  ++  handle-take  ~(handle-take handler bowl state)
  --
::
++  create-tapp-all
  |=  handler=tapp-core-all
  |_  [=bowl:gall tapp-state]
  ++  this-tapp  .
  ::
  ::  Initialize or upgrade tapp
  ::
  ::    If state is upgraded incompatibly, hard-reset and cancel if active.
  ::    Otherwise, upgrade, cancel and restart if active.
  ::
  ++  prep
    |=  old-state=(unit)
    ^-  (quip move _this-tapp)
    ?~  old-state
      ~&  [%tapp-init dap.bowl]
      =.  waiting  (~(put to waiting) ost.bowl [%init ~])
      start-async
    ::
    =/  old   ((soft tapp-state) u.old-state)
    ?~  old
      ::  XX use only for development may break contracts!
      ::  XX if active clam contracts only to abort transaction?
      ::
      ::  ~&  [%tapp-reset dap.bowl]
      ::  `this-tapp
      ~|  [%tapp-load-incompatible dap.bowl]
      !!
    ::
    ::  because the clam replaces the active continuation with
    ::  the bunt of its mold, we must fail the transaction
    ::
    ~&  [%tapp-loaded dap.bowl]
    =.  +<+.this-tapp  u.old
    ?^  active
      =.  waiting  (~(put to waiting) (need ~(top to waiting)))
      (oob-fail-async %reset-restart ~)
    `this-tapp
  ::
  ::  Start a command
  ::
  ++  poke
    |=  =tapp-in-poke-data
    ^-  (quip move _this-tapp)
    ?:  ?=(tapp-admin-in-poke-data tapp-in-poke-data)
      ?~  active
        ~&  [%tapp-admin-idle dap.bowl]
        `this-tapp
      ?-  tapp-admin.tapp-in-poke-data
          %cancel
        (oob-fail-async %tapp-admin-cancel ~)
      ::
          %restart
        =.  waiting  (~(put to waiting) (need ~(top to waiting)))
        (oob-fail-async %tapp-admin-restart ~)
      ==
    ::
    =.  waiting  (~(put to waiting) ost.bowl [%poke tapp-in-poke-data])
    ?^  active
      ~&  [%waiting-until-current-async-finishes waiting]
      `this-tapp
    start-async
  ::
  ::  Receive acknowledgement of outgoing poke
  ::
  ::    XX these can be distinguished by dock, but, without a wire or
  ::    some alternative, that's not very useful if you poke the same
  ::    dock multiple times. %poke wires are not currently exposed ...
  ::    Wat do?
  ::
  ++  coup
    |=  [=wire error=(unit tang)]
    ^-  (quip move _this-tapp)
    ?>  ?=([@ @ *] wire)
    =/  her  (slav %p i.wire)
    =*  app  i.t.wire
    =.  waiting  (~(put to waiting) ost.bowl [%take %coup [her app] error])
    ?^  active
      `this-tapp
    start-async
  ::
  ::  Read from tapp state
  ::
  ++  peek
    |=  =path
    ^-  (unit (unit ?(tapp-peek peek-data)))
    ?-  path
        [%x %tapp %active ~]
      [~ ~ %noun ?=(^ active)]
    ::
        [%x %tapp %contracts ~]
      [~ ~ %noun ?~(active ~ ~(key by contracts.u.active))]
    ::
        *
      (~(handle-peek handler bowl app-state) path)
    ==
  ::
  ::  Receive subscription request
  ::
  ++  peer
    |=  =path
    ^-  (quip move _this-tapp)
    =.  waiting  (~(put to waiting) ost.bowl [%peer path])
    ?^  active
      `this-tapp
    start-async
  ::
  ::  Receive (involuntary) unsubscription
  ::
  ++  quit
    |=  =wire
    ^-  (quip move _this-tapp)
    ?>  ?=([@ @ *] wire)
    =/  her  (slav %p i.wire)
    =*  app  i.t.wire
    =*  pax  t.t.wire
    =.  waiting  (~(put to waiting) ost.bowl [%take %quit [her app] pax])
    ?^  active
      `this-tapp
    start-async
  ::
  ::  Receive acknowledgement of outgoing subscription request
  ::
  ++  reap
    |=  [=wire error=(unit tang)]
    ^-  (quip move _this-tapp)
    ?>  ?=([@ @ *] wire)
    =/  her  (slav %p i.wire)
    =*  app  i.t.wire
    =*  pax  t.t.wire
    =.  waiting  (~(put to waiting) ost.bowl [%take %reap [her app] pax error])
    ?^  active
      `this-tapp
    start-async
  ::
  ::  Receive subscription response
  ::
  ++  diff
    |=  [=wire =in-peer-data]
    ^-  (quip move _this-tapp)
    ?>  ?=([@ @ *] wire)
    =/  her  (slav %p i.wire)
    =*  app  i.t.wire
    =*  pax  t.t.wire
    =.  waiting  (~(put to waiting) ost.bowl [%diff [her app] pax in-peer-data])
    ?^  active
      `this-tapp
    start-async
  ::
  ::  Pass response to async
  ::
  ++  http-response
    |=  [=wire response=client-response:iris]
    ^-  (quip move _this-tapp)
    (take-async bowl `[wire %http-response response])
  ::
  ::  Pass timer to async, or fail
  ::
  ++  wake-note
    |=  [=wire error=(unit tang)]
    ^-  (quip move _this-tapp)
    ?^  error
      (oob-fail-async %timer-fire-failed u.error)
    (take-async bowl `[wire %wake ~])
  ::
  ::  Enqueue timer transaction
  ::
  ++  wake-effect
    |=  [=wire error=(unit tang)]
    ^-  (quip move _this-tapp)
    =.  waiting  (~(put to waiting) ost.bowl [%take %wake error])
    ?^  active
      `this-tapp
    start-async
  ::
  ::  Receive route binding notification
  ::
  ++  bound
    |=  [=wire success=? =binding:eyre]
    ^-  (quip move _this-tapp)
    (take-async bowl `[wire %bound success binding])
  ::
  ::  Receive source update from jael
  ::
  ++  source
    |=  [=wire whos=(set ship) =source:jael]
    ^-  (quip move _this-tapp)
    =.  waiting  (~(put to waiting) ost.bowl [%take %source whos source])
    ?^  active
      `this-tapp
    start-async
  ::
  ::  Continue computing async
  ::
  ++  take-async
    |=  =async-input:async-lib
    ^-  (quip move _this-tapp)
    =/  m  tapp-async
    =|  moves=(list move)
    =|  scrys=(list path)
    |-  ^-  (quip move _this-tapp)
    ?~  active
      ~|  %no-active-async
      ~|  ?~  in.async-input
            ~
          wire.u.in.async-input
      !!
    =^  r=[moves=(list move) =eval-result:eval:m]  u.active
      =/  out
        %-  mule  |.
        (take:eval:m u.active ost.bowl async-input)
      ?-  -.out
        %&  p.out
        %|  [[~ [%fail contracts.u.active %crash p.out]] u.active]
      ==
    =.  moves  (weld moves (skip moves.r |=(=move =(%scry -.q.move))))
    =.  scrys
      %+  weld  scrys
      ^-  (list path)
      %+  murn  moves.r
      |=  =move
      ^-  (unit path)
      ?.  ?=(%scry -.q.move)
        ~
      `path.q.move
    ?^  scrys
      =/  scry-result  .^(* i.scrys)
      $(scrys t.scrys, in.async-input `[i.scrys %scry-result scry-result])
    =>  .(active `(unit eval-form:eval:tapp-async)`active)  :: TMI
    =^  final-moves=(list move)  this-tapp
      ?-  -.eval-result.r
        %next  `this-tapp
        %fail  (fail-async [contracts err]:eval-result.r)
        %done  (done-async [contracts value]:eval-result.r)
      ==
    [(weld moves final-moves) this-tapp]
  ::
  ::  Fails currently-running async
  ::
  ++  oob-fail-async
    (cury fail-async contracts:(need active))
  ::
  ::  Called on async failure
  ::
  ++  fail-async
    |=  [contracts=(map contract bone) err=(pair term tang)]
    ^-  (quip move _this-tapp)
    %-  %-  slog
        :*  leaf+(weld "tapp command failed in app/" (trip dap.bowl))
            leaf+(weld "  %" (trip p.err))
            q.err
        ==
    (finish-async contracts)
  ::
  ::  Called on async success
  ::
  ++  done-async
    |=  [contracts=(map contract bone) state=state-type]
    ^-  (quip move _this-tapp)
    =.  app-state  state
    (finish-async contracts)
  ::
  ::  Called whether async failed or succeeded
  ::
  ++  finish-async
    |=  contracts=(map contract bone)
    ^-  (quip move _this-tapp)
    =^  moves-1  this-tapp  (cancel-contracts contracts)
    =.  active   ~
    =.  waiting  +:~(get to waiting)
    =^  moves-2  this-tapp  start-async
    [(weld moves-1 moves-2) this-tapp]
  ::
  ::  Try to start next command
  ::
  ++  start-async
    ^-  (quip move _this-tapp)
    ?.  =(~ active)
      ~|  %async-already-active  !!
    =/  next=(unit [=bone =command])  ~(top to waiting)
    ?~  next
      `this-tapp
    =.  active
      :-  ~
      %-  from-form:eval:tapp-async
      ^-  form:tapp-async
      =/  out
        %-  mule  |.
        =.  ost.bowl  bone.u.next
        =*  input  +.command.u.next
        ?-  -.command.u.next
          %init  ~(handle-init handler bowl app-state)
          %poke  (~(handle-poke handler bowl app-state) input)
          %peer  (~(handle-peer handler bowl app-state) input)
          %diff  (~(handle-diff handler bowl app-state) input)
          %take  (~(handle-take handler bowl app-state) input)
        ==
      ?-  -.out
        %&  p.out
        %|  |=  async-input:async-lib
            [~ ~ ~ %fail %crash p.out]
      ==
    (take-async bowl ~)
  ::
  ::  Cancel outstanding contracts
  ::
  ++  cancel-contracts
    |=  contracts=(map contract bone)
    ^-  (quip move this-tapp)
    [(zing (turn ~(tap by contracts) cancel-contract)) this-tapp]
  ::
  ::  Cancel individual contract
  ::
  ++  cancel-contract
    |=  [=contract =bone]
    ^-  (list move)
    ?-  -.contract
      %wait     [bone %rest /note/(scot %da at.contract) at.contract]~
      %request  [bone %cancel-request / ~]~
    ==
  --
--
