:: service/inbox.hoon
/-  *groups, *inbox, *sync-hook
|%
+$  move  [bone card]
::
+$  card
  $%  [%diff [%inbox-update inbox-update]]
      [%quit ~]
  ==
::
+$  state
  $%  [%0 state-zero]
  ==
::
+$  state-zero
  $:  inbox=(map path mailbox)
  ==
::
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
  ^-  (unit (unit [%noun (unit (map path mailbox))]))
  [~ ~ %noun `inbox]
::
++  peek-x-mailbox
  |=  pax=path
  ^-  (unit (unit [%noun (unit mailbox)]))
  ?~  pax
    [~ ~ %noun ~]
  =/  mailbox=(unit mailbox)  (~(get by inbox) pax)
  [~ ~ %noun mailbox]
::
++  peer-keys
  |=  pax=path
  ^-  (quip move _this)
  ?.  =(src.bol our.bol)
    [[ost.bol %quit ~]~ this]
  ::  we send the list of keys then send events when they change
  :_  this
  [ost.bol %diff %inbox-update [%keys ~(key by inbox)]]~
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
  [ost.bol %diff %inbox-update [%create pax owner.u.box]]~
::
++  poke-inbox-action
  |=  action=inbox-action
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
  |=  act=inbox-action
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
  |=  act=inbox-action
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
  |=  act=inbox-action
  ^-  (quip move _this)
  ?>  ?=(%message -.act)
  =/  mailbox=(unit mailbox)  (~(get by inbox) path.act)
  ?~  mailbox
    [~ this]
  =.  envelopes.u.mailbox  (snoc envelopes.u.mailbox envelope.act)
  =.  inbox  (~(put by inbox) path.act u.mailbox)
  :_  this(inbox inbox)
  (send-diff path.act act)
::
++  handle-read
  |=  act=inbox-action
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
  |=  [pax=path act=inbox-action]
  ^-  (list move)
  %+  weld
    ^-  (list move)
    %+  turn  (prey:pubsub:userlib [%mailbox pax] bol)
    |=  [=bone *]
    [bone %diff %inbox-update act]
  ^-  (list move)
  ?.  |(=(%create -.act) =(%delete -.act))
    ~
  %+  turn  (prey:pubsub:userlib /keys bol)
  |=  [=bone *]
  [bone %diff %inbox-update act]
::
--

