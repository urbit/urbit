:: chat-store: data store that holds linear sequences of chat messages
::
/+  *chat-json
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
  $%  [%chat-initial inbox]
      [%chat-configs chat-configs]
      [%chat-update chat-update]
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
  ^-  (unit (unit [%noun chat-configs]))
  :^  ~  ~  %noun
  (inbox-to-configs inbox)
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
++  peek-x-config
  |=  pax=path
  ^-  (unit (unit [%noun config]))
  ?~  pax
    ~
  =/  mailbox  (~(get by inbox) pax)
  ?~  mailbox
    ~
  :^  ~  ~  %noun
  config.u.mailbox
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
    =*  length  length.config.u.mailbox
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
  ?>  (team:title our.bol src.bol)
  ::  we send the list of keys then send events when they change
  :_  this
  [ost.bol %diff %chat-update [%keys ~(key by inbox)]]~
::
++  peer-all
  |=  pax=path
  ^-  (quip move _this)
  ?>  (team:title our.bol src.bol)
  :_  this
  [ost.bol %diff %chat-initial inbox]~
::
++  peer-configs
  |=  pax=path
  ^-  (quip move _this)
  ?>  (team:title our.bol src.bol)
  :_  this
  [ost.bol %diff %chat-configs (inbox-to-configs inbox)]~
::
++  peer-updates
  |=  pax=path
  ^-  (quip move _this)
  ?>  (team:title our.bol src.bol)
  ::  we now proxy all events to this path
  [~ this]
::
++  peer-mailbox
  |=  pax=path
  ^-  (quip move _this)
  ?>  (team:title our.bol src.bol)
  =/  box=(unit mailbox)  (~(get by inbox) pax)
  ?~  box  !!
  :_  this
  [ost.bol %diff %chat-update [%create pax owner.config.u.box]]~
::
++  poke-json
  |=  jon=json
  ^-  (quip move _this)
  ?>  (team:title our.bol src.bol)
  (poke-chat-action (json-to-action jon))
::
++  poke-chat-action
  |=  action=chat-action
  ^-  (quip move _this)
  ?>  (team:title our.bol src.bol)
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
  =.  owner.config.mailbox  owner.act
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
  =.  length.config.u.mailbox  +(length.config.u.mailbox)
  =.  number.envelope.act  length.config.u.mailbox
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
  =.  read.config.u.mailbox  length.config.u.mailbox
  =.  inbox  (~(put by inbox) path.act u.mailbox)
  :_  this(inbox inbox)
  (send-diff path.act act)
::
++  update-subscribers
  |=  [pax=path act=chat-action]
  ^-  (list move)
  %+  turn  (prey:pubsub:userlib pax bol)
  |=  [=bone *]
  [bone %diff %chat-update act]
::
++  send-diff
  |=  [pax=path act=chat-action]
  ^-  (list move)
  %-  zing
  :~  (update-subscribers /all act)
      (update-subscribers /updates act)
      (update-subscribers [%mailbox pax] act)
      ?.  |(=(%read -.act) =(%message -.act))
        ~
      (update-subscribers /configs act)
      ?.  |(=(%create -.act) =(%delete -.act))
        ~
      (update-subscribers /keys act)
  ==
::
--
