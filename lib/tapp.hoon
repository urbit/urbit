/-  tapp-sur=tapp
/+  trad
|*  $:  state-type=mold
        command-type=mold
        poke-data=mold
        out-peer-data=mold
        in-peer-data=mold
    ==
|%
++  tapp-sur  (^tapp-sur poke-data out-peer-data)
++  card  card:tapp-sur
++  sign  sign:tapp-sur
++  contract  contract:tapp-sur
++  command
  $%  [%poke command=command-type]
      [%peer =path]
      [%diff =dock =path =in-peer-data]
      [%take =sign]
  ==
::
++  trad-lib  (^trad sign card contract)
++  trad  trad:trad-lib
::
+$  move  (pair bone card)
++  tapp-trad  (trad state-type)
+$  tapp-state
  $:  waiting=(qeu command)
      active=(unit eval-form:eval:tapp-trad)
      app-state=state-type
  ==
::
::  The form of a tapp that only handles pokes
::
++  tapp-core-poke
  $_  ^|
  |_  [bowl:gall state-type]
  ++  handle-command
    |~  command-type
    *form:tapp-trad
  --
::
++  create-tapp-poke
  |=  handler=tapp-core-poke
  %-  create-tapp-poke-peer
  |_  [=bowl:gall state=state-type]
  ++  handle-command  ~(handle-command handler bowl state)
  ++  handle-peer     |=(* (trad-fail:trad-lib %no-peer-handler >path< ~))
  --
::
::  The form of a tapp that only handles pokes and peers
::
++  tapp-core-poke-peer
  $_  ^|
  |_  [bowl:gall state-type]
  ++  handle-command
    |~  command-type
    *form:tapp-trad
  ::
  ++  handle-peer
    |~  path
    *form:tapp-trad
  --
::
++  create-tapp-poke-peer
  |=  handler=tapp-core-poke-peer
  %-  create-tapp-all
  |_  [=bowl:gall state=state-type]
  ++  handle-command  ~(handle-command handler bowl state)
  ++  handle-peer     ~(handle-peer handler bowl state)
  ++  handle-diff     |=(* (trad-fail:trad-lib %no-diff-handler >path< ~))
  ++  handle-take     |=(* (trad-fail:trad-lib %no-take-handler >path< ~))
  --
::
::  The form of a tapp that only handles pokes and diffs
::
++  tapp-core-poke-diff
  $_  ^|
  |_  [bowl:gall state-type]
  ++  handle-command
    |~  command-type
    *form:tapp-trad
  ::
  ++  handle-diff
    |~  [dock path in-peer-data]
    *form:tapp-trad
  --
::
++  create-tapp-poke-diff
  |=  handler=tapp-core-poke-diff
  %-  create-tapp-all
  |_  [=bowl:gall state=state-type]
  ++  handle-command  ~(handle-command handler bowl state)
  ++  handle-peer     |=(* (trad-fail:trad-lib %no-peer-handler >path< ~))
  ++  handle-diff     ~(handle-diff handler bowl state)
  ++  handle-take     |=(* (trad-fail:trad-lib %no-take-handler >path< ~))
  --
::
::  The form of a tapp that only handles pokes, peers, and takes
::
++  tapp-core-poke-peer-take
  $_  ^|
  |_  [bowl:gall state-type]
  ++  handle-command
    |~  command-type
    *form:tapp-trad
  ::
  ++  handle-peer
    |~  path
    *form:tapp-trad
  ::
  ++  handle-take
    |~  sign
    *form:tapp-trad
  --
::
++  create-tapp-poke-peer-take
  |=  handler=tapp-core-poke-peer-take
  %-  create-tapp-all
  |_  [=bowl:gall state=state-type]
  ++  handle-command  ~(handle-command handler bowl state)
  ++  handle-peer     ~(handle-peer handler bowl state)
  ++  handle-diff     |=(* (trad-fail:trad-lib %no-diff-handler >path< ~))
  ++  handle-take     ~(handle-take handler bowl state)
  --
::
::  The form of a tapp
::
++  tapp-core-all
  $_  ^|
  |_  [bowl:gall state-type]
  ::
  ::  Input
  ::
  ++  handle-command
    |~  command-type
    *form:tapp-trad
  ::
  ::  Subscription request
  ::
  ++  handle-peer
    |~  path
    *form:tapp-trad
  ::
  ::  Receive subscription result
  ::
  ++  handle-diff
    |~  [dock path in-peer-data]
    *form:tapp-trad
  ::
  ::  Receive syscall result
  ::
  ++  handle-take
    |~  sign
    *form:tapp-trad
  --
::
++  create-tapp-all
  |=  handler=tapp-core-all
  |_  [=bowl:gall tapp-state]
  ++  this-tapp  .
  ++  prep
    |=  old-state=*
    ^-  (quip move _this-tapp)
    ~&  [%tapp-loaded dap.bowl]
    =/  old  ((soft tapp-state) old-state)
    ?~  old
      `this-tapp
    `this-tapp(+<+ u.old)
  ::
  ::  Start a command
  ::
  ++  poke
    |=  command=command-type
    ^-  (quip move _this-tapp)
    =.  waiting  (~(put to waiting) %poke command)
    ?^  active
      ~&  [%waiting-until-current-trad-finishes waiting]
      `this-tapp
    start-trad
  ::
  ::  Receive subscription request
  ::
  ++  peer
    |=  =path
    ^-  (quip move _this-tapp)
    =.  waiting  (~(put to waiting) %peer path)
    ?^  active
      `this-tapp
    start-trad
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
    start-trad
  ::
  ::  Pass response to trad
  ::
  ++  sigh-httr
    |=  [=wire =httr:eyre]
    ^-  (quip move _this-tapp)
    (take-trad bowl `[wire %sigh httr])
  ::
  ::  Failed http request
  ::
  ++  sigh-tang
    |=  [=wire =tang]
    ^-  (quip move _this-tapp)
    (oob-fail-trad %failed-sigh tang)
  ::
  ++  wake-note
    |=  [=wire error=(unit tang)]
    ^-  (quip move _this-tapp)
    ?^  error
      (oob-fail-trad %timer-fire-failed u.error)
    (take-trad bowl `[wire %wake ~])
  ::
  ++  wake-effect
    |=  [=wire error=(unit tang)]
    ^-  (quip move _this-tapp)
    =.  waiting  (~(put to waiting) %take %wake error)
    ?^  active
      `this-tapp
    start-trad
  ::
  ::  Continue computing trad
  ::
  ++  take-trad
    |=  =trad-input:trad-lib
    ^-  (quip move _this-tapp)
    =/  m  tapp-trad
    ?~  active
      ::  Can't cancel HTTP requests, so we might get answers after end
      ::  of computation
      ::
      ?:  ?=([~ @ %sigh *] in.trad-input)
        `this-tapp
      ~|  %no-active-trad
      ~|  ?~  in.trad-input
            ~
          wire.u.in.trad-input
      !!
    =^  r=[moves=(list move) =eval-result:eval:m]  u.active
      (take:eval:m u.active ost.bowl trad-input)
    =>  .(active `(unit eval-form:eval:tapp-trad)`active)  :: TMI
    =^  moves=(list move)  this-tapp
      ?-  -.eval-result.r
        %next  `this-tapp
        %fail  (fail-trad [contracts err]:eval-result.r)
        %done  (done-trad [contracts value]:eval-result.r)
      ==
    [(weld moves.r moves) this-tapp]
  ::
  ::  Fails currently-running trad
  ::
  ++  oob-fail-trad
    (cury fail-trad contracts:(need active))
  ::
  ::  Called on trad failure
  ::
  ++  fail-trad
    |=  [contracts=(set contract) err=(pair term tang)]
    ^-  (quip move _this-tapp)
    %-  %-  slog
        :*  leaf+(trip dap.bowl)
            leaf+"tapp command failed"
            leaf+(trip p.err)
            q.err
        ==
    (finish-trad contracts)
  ::
  ::  Called on trad success
  ::
  ++  done-trad
    |=  [contracts=(set contract) state=state-type]
    ^-  (quip move _this-tapp)
    =.  app-state  state
    (finish-trad contracts)
  ::
  ::  Called whether trad failed or succeeded
  ::
  ++  finish-trad
    |=  contracts=(set contract)
    ^-  (quip move _this-tapp)
    =^  moves-1  this-tapp  (cancel-contracts contracts)
    =.  active   ~
    =.  waiting  +:~(get to waiting)
    =^  moves-2  this-tapp  start-trad
    [(weld moves-1 moves-2) this-tapp]
  ::
  ::
  ::  Try to start next command
  ::
  ++  start-trad
    ^-  (quip move _this-tapp)
    ?.  =(~ active)
      ~|  %trad-already-active  !!
    =/  next=(unit command)  ~(top to waiting)
    ?~  next
      `this-tapp
    =.  active
      :-  ~
      %-  from-form:eval:tapp-trad
      ^-  form:tapp-trad
      ?-  -.u.next
        %poke  (~(handle-command handler bowl app-state) command.u.next)
        %peer  (~(handle-peer handler bowl app-state) path.u.next)
        %diff  (~(handle-diff handler bowl app-state) +.u.next)
        %take  (~(handle-take handler bowl app-state) +.u.next)
      ==
    (take-trad bowl ~)
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
