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
++  command
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
  $:  waiting=(qeu command)
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
  ^-  tapp-core-all
  |_  [bowl:gall state-type]
  ++  handle-init
    *form:tapp-async
  ::
  ++  handle-poke
    |=(* (async-fail:async-lib %no-poke-handler ~))
  ::
  ++  handle-peek  _~
  ::
  ++  handle-peer
    |=  =path
    ~|  %default-tapp-no-sole
    ?<  ?=([%sole *] path)
    (async-fail:async-lib %no-peer-handler >path< ~)
  ::
  ++  handle-diff
    |=(* (async-fail:async-lib %no-diff-handler ~))
  ::
  ++  handle-take
    |=(* (async-fail:async-lib %no-take-handler ~))
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
  ++  handle-poke  ~(handle-poke handler bowl state)
  ++  handle-peer  handle-peer:default-tapp
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
  ++  handle-init  handle-init:default-tapp
  ++  handle-poke  ~(handle-poke handler bowl state)
  ++  handle-peek  handle-peek:default-tapp
  ++  handle-peer  ~(handle-peer handler bowl state)
  ++  handle-diff  handle-diff:default-tapp
  ++  handle-take  handle-take:default-tapp
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
  ++  handle-init  handle-init:default-tapp
  ++  handle-poke  ~(handle-poke handler bowl state)
  ++  handle-peek  handle-peek:default-tapp
  ++  handle-peer  handle-peer:default-tapp
  ++  handle-diff  ~(handle-diff handler bowl state)
  ++  handle-take  handle-take:default-tapp
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
  ++  handle-init  handle-init:default-tapp
  ++  handle-poke  ~(handle-poke handler bowl state)
  ++  handle-peek  handle-peek:default-tapp
  ++  handle-peer  ~(handle-peer handler bowl state)
  ++  handle-diff  handle-diff:default-tapp
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
  ++  handle-init  handle-init:default-tapp
  ++  handle-poke  ~(handle-poke handler bowl state)
  ++  handle-peek  handle-peek:default-tapp
  ++  handle-peer  ~(handle-peer handler bowl state)
  ++  handle-diff  ~(handle-diff handler bowl state)
  ++  handle-take  ~(handle-take handler bowl state)
  --
::
++  create-tapp-all
  |=  handler=tapp-core-all
  |_  [=bowl:gall tapp-state]
  ++  this-tapp  .
  ++  prep
    |=  old-state=(unit)
    ^-  (quip move _this-tapp)
    ?~  old-state
      ~&  [%tapp-init dap.bowl]
      =.  waiting  (~(put to waiting) %init ~)
      start-async
    ::
    =/  old  ((soft tapp-state) u.old-state)
    ?~  old
      ~&  [%tapp-reset dap.bowl]
      `this-tapp
    ~&  [%tapp-loaded dap.bowl]
    `this-tapp(+<+ u.old)
  ::
  ::  Start a command
  ::
  ++  poke
    |=  =in-poke-data
    ^-  (quip move _this-tapp)
    =.  waiting  (~(put to waiting) %poke in-poke-data)
    ?^  active
      ~&  [%waiting-until-current-async-finishes waiting]
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
      [~ ~ %noun ?~(active ~ contracts.u.active)]
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
    =.  waiting  (~(put to waiting) %peer path)
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
    =.  waiting  (~(put to waiting) %diff [her app] pax in-peer-data)
    ?^  active
      `this-tapp
    start-async
  ::
  ::  Pass response to async
  ::
  ++  sigh-httr
    |=  [=wire =httr:eyre]
    ^-  (quip move _this-tapp)
    (take-async bowl `[wire %sigh httr])
  ::
  ::  Failed http request
  ::
  ++  sigh-tang
    |=  [=wire =tang]
    ^-  (quip move _this-tapp)
    (oob-fail-async %failed-sigh tang)
  ::
  ++  wake-note
    |=  [=wire error=(unit tang)]
    ^-  (quip move _this-tapp)
    ?^  error
      (oob-fail-async %timer-fire-failed u.error)
    (take-async bowl `[wire %wake ~])
  ::
  ++  wake-effect
    |=  [=wire error=(unit tang)]
    ^-  (quip move _this-tapp)
    =.  waiting  (~(put to waiting) %take %wake error)
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
    ?~  active
      ::  Can't cancel HTTP requests, so we might get answers after end
      ::  of computation
      ::
      ?:  ?=([~ @ %sigh *] in.async-input)
        `this-tapp
      ~|  %no-active-async
      ~|  ?~  in.async-input
            ~
          wire.u.in.async-input
      !!
    =^  r=[moves=(list move) =eval-result:eval:m]  u.active
      (take:eval:m u.active ost.bowl async-input)
    =>  .(active `(unit eval-form:eval:tapp-async)`active)  :: TMI
    =^  moves=(list move)  this-tapp
      ?-  -.eval-result.r
        %next  `this-tapp
        %fail  (fail-async [contracts err]:eval-result.r)
        %done  (done-async [contracts value]:eval-result.r)
      ==
    [(weld moves.r moves) this-tapp]
  ::
  ::  Fails currently-running async
  ::
  ++  oob-fail-async
    (cury fail-async contracts:(need active))
  ::
  ::  Called on async failure
  ::
  ++  fail-async
    |=  [contracts=(set contract) err=(pair term tang)]
    ^-  (quip move _this-tapp)
    %-  %-  slog
        :*  leaf+(trip dap.bowl)
            leaf+"tapp command failed"
            leaf+(trip p.err)
            q.err
        ==
    (finish-async contracts)
  ::
  ::  Called on async success
  ::
  ++  done-async
    |=  [contracts=(set contract) state=state-type]
    ^-  (quip move _this-tapp)
    =.  app-state  state
    (finish-async contracts)
  ::
  ::  Called whether async failed or succeeded
  ::
  ++  finish-async
    |=  contracts=(set contract)
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
    =/  next=(unit command)  ~(top to waiting)
    ?~  next
      `this-tapp
    =.  active
      :-  ~
      %-  from-form:eval:tapp-async
      ^-  form:tapp-async
      ?-  -.u.next
        %init  ~(handle-init handler bowl app-state)
        %poke  (~(handle-poke handler bowl app-state) +.u.next)
        %peer  (~(handle-peer handler bowl app-state) +.u.next)
        %diff  (~(handle-diff handler bowl app-state) +.u.next)
        %take  (~(handle-take handler bowl app-state) +.u.next)
      ==
    (take-async bowl ~)
  ::
  ::  Cancel outstanding contracts
  ::
  ++  cancel-contracts
    |=  contracts=(set contract)
    ^-  (quip move this-tapp)
    [(zing (turn ~(tap in contracts) cancel-contract)) this-tapp]
  ::
  ::  Cancel individual contract
  ::
  ++  cancel-contract
    |=  =contract
    ^-  (list move)
    ?-  -.contract
      %wait  [ost.bowl %rest /note/(scot %da at.contract) at.contract]~
      %hiss  ~  ::  can't cancel; will ignore response
    ==
  --
--
