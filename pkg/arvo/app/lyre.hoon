/-  *lyre
/+  *server
|%
+$  move  [bone card]
+$  card
  $%  [%connect wire binding:eyre term]
      [%disconnect wire binding:eyre]
      [%http-response =http-event:http]
  ==
::
+$  session
  $:  pax=path
  ==
+$  renderer  ~
::
+$  state
  $:  ses=(list session)
      cur=@u
      ren=(map path (unit renderer))
  ==
::
--
::
|_  [bol=bowl:gall state]
::
++  this  .
::
++  prep
  |=  old=(unit *)
  ^-  (quip move _this)
  ~&  prep+act.bol
  ?~  old
    :_  this
    [ost.bol %connect / [~ /'~lyre'] %lyre]~
  ::  XX  todo, populate renderer map, start clay watch
::  [~ this(+<+ *state)]           ::  flush state
  [~ this(+<+ ;;(state u.old))]  ::  keep state
::
++  poke-noun
  |=  a=*
  ^-  (quip move _this)
  ?+  a
    ~&  poked+a
    [~ this]
  ::
      %print-state
    ~&  +<+.this
    [~ this]
  ::
  ==
::
++  poke-lyre-action
  |=  act=action
  ^-  (quip move _this)
  ?-  -.act
      %new-session
    =/  new-pax
      ?~  pax.act  /  u.pax.act
    :-  ~
    %=  this
      ses  (snoc ses new-pax)
      cur  (lent ses)
    ==
  ::
      %delete-session
    :-  ~
    %=  this
      ses  (oust [id.act 1] ses)
      cur  ?:((gte cur id.act) (dec cur) cur)
    ==
  ::
      %switch-session
    ?>  (lth id.act (lent ses))
    :-  ~
    %=  this
      cur   id.act
    ==
  ::
      %set-path
    :-  ~
    %=  this
      ses   ;:(welp (scag cur ses) [pax.act]~ (slag +(cur) ses))
    ==
  ==
::
++  bound
  |=  [wir=wire suc=? bin=binding:eyre]
  ^-  (quip move _this)
  [~ this]
::
++  poke-handle-http-request
  %-  (require-authorization:app ost.bol move this)
  |=  =inbound-request:eyre
  ^-  (quip move _this)
  =/  current-path=path  (snag cur ses)
  =/  man=manx  ;div: {current-path}
  :_  this
  [ost.bol %http-response (manx-response:app man)]~
::
--

