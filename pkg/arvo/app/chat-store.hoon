/-  *group-store, *chat-store
|%
+$  move  [bone card]
::
+$  card
  $%  [%diff diff]
      [%quit ~]
  ==
::
+$  state
  $%  [%0 state-zero]
  ==
::
+$  state-zero
  $:  =inbox
  ==
::
+$  diff
  $%  [%chat-update chat-update]
      [%chat-initial inbox]
  ==
--
::
|_  [bol=bowl:gall state]
::
++  this  .
::
++  prep
  |=  old=(unit state)
  ^-  (quip move _this)
  ?~  old
    [~ this]
  [~ this(+<+ u.old)]
::
++  peek-x-all
  |=  pax=path
  ^-  (unit (unit [%noun (map path mailbox)]))
  [~ ~ %noun inbox]
::
++  peek-x-configs
  |=  pax=path
  ^-  (unit (unit [%noun configs]))
  :^  ~  ~  %noun
  %-  ~(run by inbox)
  |=  =mailbox
  ^-  [@ ship]
  [read.mailbox owner.mailbox]
::
++  peek-x-keys
  |=  pax=path
  ^-  (unit (unit [%noun (set path)]))
  [~ ~ %noun ~(key by inbox)]
::
++  peek-x-mailbox
  |=  pax=path
  ^-  (unit (unit [%noun (unit mailbox)]))
  ?~  pax
    ~
  =/  mailbox=(unit mailbox)  (~(get by inbox) pax)
  [~ ~ %noun mailbox]
::
++  peek-x-envelopes
  |=  pax=path
  ^-  (unit (unit [%noun (list envelope)]))
  ?+  pax
    ~
  ::
      [@ @ *]
    =/  mail-path  t.t.pax
    =/  mailbox  (~(get by inbox) mail-path)
    ?~  mailbox
      [~ ~ %noun ~]
    =*  envelopes  envelopes.u.mailbox
    =/  sign-test=[?(%neg %pos) @]
      %-  need
      %+  rush  i.pax
      ;~  pose
        %+  cook
          |=  n=@
          [%neg n]
        ;~(pfix hep dem:ag)
      ::
        %+  cook
          |=  n=@
          [%pos n]
        dem:ag
      ==
    =/  length  (lent envelopes)
    =*  start  +.sign-test
    ?:  =(-.sign-test %neg)
      ?:  (gth start length)
        [~ ~ %noun envelopes]
      [~ ~ %noun (swag [(sub length start) start] envelopes)]
    ::
    =/  end  (slav %ud i.t.pax)
    ?.  (lte start end)
      ~
    =.  end  ?:((lth end length) end length)
    [~ ~ %noun (swag [start (sub end start)] envelopes)]
  ==
::
++  peer-keys
  |=  pax=path
  ^-  (quip move _this)
  ?.  =(src.bol our.bol)
    [[ost.bol %quit ~]~ this]
  ::  we send the list of keys then send events when they change
  :_  this
  [ost.bol %diff %chat-update [%keys ~(key by inbox)]]~
::
++  peer-all
  |=  pax=path
  ^-  (quip move _this)
  ?.  =(src.bol our.bol)
    [[ost.bol %quit ~]~ this]
  :_  this
  [ost.bol %diff %chat-initial inbox]~
::
++  peer-updates
  |=  pax=path
  ^-  (quip move _this)
  ?.  =(src.bol our.bol)
    [[ost.bol %quit ~]~ this]
  ::  we now proxy all events to this path
  [~ this]
::
++  peer-mailbox
  |=  pax=path
  ^-  (quip move _this)
  ?.  =(src.bol our.bol)
    [[ost.bol %quit ~]~ this]
  =/  box=(unit mailbox)  (~(get by inbox) pax)
  ?~  box
    [[ost.bol %quit ~]~ this]
  :_  this
  [ost.bol %diff %chat-update [%create pax owner.u.box]]~
::
++  poke-chat-action
  |=  action=chat-action
  ^-  (quip move _this)
  ?.  =(src.bol our.bol)
    [~ this]
  ?-  -.action
      %create
    (handle-create action)
  ::
      %delete
    (handle-delete action)
  ::
      %message
    (handle-message action)
  ::
      %read
    (handle-read action)
  ::
  ==
::
++  handle-create
  |=  act=chat-action
  ^-  (quip move _this)
  ?>  ?=(%create -.act)
  ?:  (~(has by inbox) path.act)
    [~ this]
  =/  mailbox  *mailbox
  =.  owner.mailbox  owner.act
  =.  inbox  (~(put by inbox) path.act mailbox)
  :_  this(inbox inbox)
  (send-diff path.act act)
::
++  handle-delete
  |=  act=chat-action
  ^-  (quip move _this)
  ?>  ?=(%delete -.act)
  =/  mailbox=(unit mailbox)  (~(get by inbox) path.act)
  ?~  mailbox
    [~ this]
  =.  inbox  (~(del by inbox) path.act)
  :_  this(inbox inbox)
  (send-diff path.act act)
::
++  handle-message
  |=  act=chat-action
  ^-  (quip move _this)
  ?>  ?=(%message -.act)
  =/  mailbox=(unit mailbox)  (~(get by inbox) path.act)
  ?~  mailbox
    [~ this]
  ::  TODO: perf tests to see if (lent envelopes) is a perf drain
  =.  number.envelope.act  (add 1 (lent envelopes.u.mailbox))
  =.  envelopes.u.mailbox  (snoc envelopes.u.mailbox envelope.act)
  =.  inbox  (~(put by inbox) path.act u.mailbox)
  :_  this(inbox inbox)
  (send-diff path.act act)
::
++  handle-read
  |=  act=chat-action
  ^-  (quip move _this)
  ?>  ?=(%read -.act)
  =/  mailbox=(unit mailbox)  (~(get by inbox) path.act)
  ?~  mailbox
    [~ this]
  =.  read.u.mailbox  read.act
  =.  inbox  (~(put by inbox) path.act u.mailbox)
  :_  this(inbox inbox)
  (send-diff path.act act)
::
++  send-diff
  |=  [pax=path act=chat-action]
  ^-  (list move)
  ;:  weld
    ^-  (list move)
    %+  turn  (prey:pubsub:userlib /all bol)
    |=  [=bone *]
    [bone %diff %chat-update act]
    ^-  (list move)
    %+  turn  (prey:pubsub:userlib /updates bol)
    |=  [=bone *]
    [bone %diff %chat-update act]
  ::
    ^-  (list move)
    %+  turn  (prey:pubsub:userlib [%mailbox pax] bol)
    |=  [=bone *]
    [bone %diff %chat-update act]
  ::
    ^-  (list move)
    ?.  |(=(%create -.act) =(%delete -.act))
      ~
    %+  turn  (prey:pubsub:userlib /keys bol)
    |=  [=bone *]
    [bone %diff %chat-update act]
  ==
::
--

