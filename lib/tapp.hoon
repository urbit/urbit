/-  tapp
/+  trad
=,  card=card:tapp
=,  sign=sign:tapp
=,  trad-lib=trad
|*  [state-type=mold command-type=mold]
|%
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
++  trad-lib  (^trad-lib sign card)
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
    ~&  %tapp-loaded
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
    (take-trad `[%sigh httr])
  ::
  ::  Failed http request
  ::
  ++  sigh-tang
    |=  [=wire =tang]
    ^-  (quip move _this-tapp)
    (fail-trad %failed-sigh tang)
  ::
  ++  wake
    |=  [=wire error=(unit tang)]
    ^-  (quip move _this-tapp)
    ?^  error
      (fail-trad %timer-fire-failed u.error)
    (take-trad `[%wake ~])
  ::
  ::  Continue computing trad
  ::
  ++  take-trad
    |=  =trad-input:trad-lib
    ^-  (quip move _this-tapp)
    =/  m  tapp-trad
    ?~  active
      ~|  %no-active-trad  !!
    =^  r=[moves=(list move) =eval-result:eval:m]  u.active
      (take:eval:m u.active ost.bowl /trad trad-input)
    =>  .(active `(unit eval-form:eval:tapp-trad)`active)  :: TMI
    =^  moves=(list move)  this-tapp
      ?-  -.eval-result.r
        %next  `this-tapp
        %fail  (fail-trad err.eval-result.r)
        %done  (done-trad value.eval-result.r)
      ==
    [(weld moves.r moves) this-tapp]
  ::
  ::  Called on trad failure
  ::
  ++  fail-trad
    |=  err=(pair term tang)
    ^-  (quip move _this-tapp)
    %-  (slog leaf+"tapp command failed" leaf+(trip p.err) q.err)
    finish-trad
  ::
  ::  Called on trad success
  ::
  ++  done-trad
    |=  state=state-type
    ^-  (quip move _this-tapp)
    =.  app-state  state
    finish-trad
  ::
  ::  Called whether trad failed or succeeded
  ::
  ++  finish-trad
    ^-  (quip move _this-tapp)
    =.  active   ~
    =.  waiting  +:~(get to waiting)
    start-trad
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
    (take-trad ~)
  --
--
