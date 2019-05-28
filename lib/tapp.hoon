/-  tapp-sur=tapp
/+  trad
|*  [state-type=mold command-type=mold poke-data=mold]
|%
++  tapp-sur  (^tapp-sur poke-data)
++  card  card:tapp-sur
++  sign  sign:tapp-sur
++  contract  contract:tapp-sur
::
::  The form of a tapp
::
++  tapp-core
  $_  ^|
  |_  [bowl:gall state-type]
  ++  handle-command
    |~  command-type
    *form:tapp-trad
  --
::
++  trad-lib  (^trad sign card contract)
++  trad  trad:trad-lib
::
+$  move  (pair bone card)
++  tapp-trad  (trad state-type)
+$  tapp-state
  $:  waiting=(qeu command-type)
      active=(unit eval-form:eval:tapp-trad)
      app-state=state-type
  ==
++  create-tapp
  |=  handler=tapp-core
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
  ++  poke-noun
    |=  command=command-type
    ^-  (quip move _this-tapp)
    =.  waiting  (~(put to waiting) command)
    ?^  active
      ~&  [%waiting-until-current-trad-finishes waiting]
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
  ++  wake
    |=  [=wire error=(unit tang)]
    ^-  (quip move _this-tapp)
    ?^  error
      (oob-fail-trad %timer-fire-failed u.error)
    (take-trad bowl `[wire %wake ~])
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
      ~|  %no-active-trad  !!
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
    %-  (slog leaf+"tapp command failed" leaf+(trip p.err) q.err)
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
    =/  next=(unit command-type)  ~(top to waiting)
    ?~  next
      `this-tapp
    =.  active
      :-  ~
      ^-  eval-form:eval:tapp-trad
      %-  from-form:eval:tapp-trad
      ^-  form:tapp-trad
      (~(handle-command handler bowl app-state) u.next)
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
      %wait  [ost.bowl %rest /(scot %da at.contract) at.contract]~
      %hiss  ~  ::  can't cancel; will ignore response
    ==
  --
--
