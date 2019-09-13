:: service/inbox.hoon
/-  *groups, *inbox
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
  $%  [%inbox-update inbox-update]
      [%inbox-initial inbox]
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
        ;~(pfix hep dem)
      ::
        %+  cook
          |=  n=@
          [%pos n]
        dem
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
  [ost.bol %diff %inbox-update [%keys ~(key by inbox)]]~
::
++  peer-all
  |=  pax=path
  ^-  (quip move _this)
  ?.  =(src.bol our.bol)
    [[ost.bol %quit ~]~ this]
  ?~  pax
    ::  we now proxy all events to this path
    :_  this
    [ost.bol %diff %inbox-initial inbox]~
  ?:  =(pax /updates)
    [~ this]
  [[ost.bol %quit ~]~ this]
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
  ;:  weld
    ^-  (list move)
    %+  turn  (prey:pubsub:userlib /all bol)
    |=  [=bone *]
    [bone %diff %inbox-update act]
    ^-  (list move)
    %+  turn  (prey:pubsub:userlib /all/updates bol)
    |=  [=bone *]
    [bone %diff %inbox-update act]
  ::
    ^-  (list move)
    %+  turn  (prey:pubsub:userlib [%mailbox pax] bol)
    |=  [=bone *]
    [bone %diff %inbox-update act]
  ::
    ^-  (list move)
    ?.  |(=(%create -.act) =(%delete -.act))
      ~
    %+  turn  (prey:pubsub:userlib /keys bol)
    |=  [=bone *]
    [bone %diff %inbox-update act]
  ==
::
--

