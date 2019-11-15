:: chat-store: data store that holds linear sequences of chat messages
::
/+  *chat-json, *chat-eval
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
      [%chat-two-update chat-two-update]
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
  [~ ?~(old this this(+<+ u.old))]
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
  ?~  pax  ~
  =/  mailbox=(unit mailbox)  (~(get by inbox) pax)
  [~ ~ %noun mailbox]
::
++  peek-x-config
  |=  pax=path
  ^-  (unit (unit [%noun config]))
  ?~  pax  ~
  =/  mailbox  (~(get by inbox) pax)
  ?~  mailbox  ~
  :^  ~  ~  %noun
  config.u.mailbox
::
++  peek-x-envelopes
  |=  pax=path
  ^-  (unit (unit [%noun (list envelope)]))
  ?+  pax  ~
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
  ?>  (~(has by inbox) pax)
  =^  =ship  pax
    ?>  ?=([* ^] pax)
    [(slav %p i.pax) t.pax]
    :_  this
  [ost.bol %diff %chat-update [%create ship pax]]~
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
      %create    (handle-create action)
      %delete    (handle-delete action)
      %message   (handle-message action)
      %messages  (handle-messages action)
      %read      (handle-read action)
  ==
::
++  handle-create
  |=  act=chat-action
  ^-  (quip move _this)
  ?>  ?=(%create -.act)
  =/  pax  [(scot %p ship.act) path.act]
  ?:  (~(has by inbox) pax)
    [~ this]
  :-  (send-diff pax act)
  this(inbox (~(put by inbox) pax *mailbox))
::
++  handle-delete
  |=  act=chat-action
  ^-  (quip move _this)
  ?>  ?=(%delete -.act)
  =/  mailbox=(unit mailbox)  (~(get by inbox) path.act)
  ?~  mailbox
    [~ this]
  :-  (send-diff path.act act)
  this(inbox (~(del by inbox) path.act))
::
++  handle-message
  |=  act=chat-action
  ^-  (quip move _this)
  ?>  ?=(%message -.act)
  =/  mailbox=(unit mailbox)  (~(get by inbox) path.act)
  ?~  mailbox
    [~ this]
  =.  letter.envelope.act  (evaluate-letter letter.envelope.act)
  =.  u.mailbox  (append-envelope u.mailbox envelope.act)
  :-  (send-diff path.act act)
  this(inbox (~(put by inbox) path.act u.mailbox))
::
++  handle-messages
  |=  act=chat-action
  ^-  (quip move _this)
  ?>  ?=(%messages -.act)
  =/  mailbox=(unit mailbox)  (~(get by inbox) path.act)
  ?~  mailbox
    [~ this]
  =/  evaluated-envelopes=(list envelope)  ~
  |-  ^-  (quip move _this)
  ?~  envelopes.act
    :_  this(inbox (~(put by inbox) path.act u.mailbox))
    %+  send-two-diff  path.act
    :*  %messages
        path.act
        (sub length.config.u.mailbox (lent evaluated-envelopes))
        length.config.u.mailbox 
        evaluated-envelopes
    ==
  =.  letter.i.envelopes.act  (evaluate-letter letter.i.envelopes.act)
  =.  evaluated-envelopes  (snoc evaluated-envelopes i.envelopes.act)
  =.  u.mailbox  (append-envelope u.mailbox i.envelopes.act)
  $(envelopes.act t.envelopes.act)
::
++  handle-read
  |=  act=chat-action
  ^-  (quip move _this)
  ?>  ?=(%read -.act)
  =/  mailbox=(unit mailbox)  (~(get by inbox) path.act)
  ?~  mailbox
    [~ this]
  =.  read.config.u.mailbox  length.config.u.mailbox
  :-  (send-diff path.act act)
  this(inbox (~(put by inbox) path.act u.mailbox))
::
++  evaluate-letter
  |=  =letter
  ^-  ^letter
  =?  letter  &(?=(%code -.letter) ?=(~ output.letter))
    =/  =hoon  (ream expression.letter)
    letter(output (eval bol hoon))
  letter
::
++  append-envelope
  |=  [=mailbox =envelope]
  ^-  ^mailbox
  =.  number.envelope  +(length.config.mailbox)
  =:  length.config.mailbox  +(length.config.mailbox)
      envelopes.mailbox  (snoc envelopes.mailbox envelope)
  ==
  mailbox
::
++  update-subscribers
  |=  [pax=path upd=chat-update]
  ^-  (list move)
  %+  turn  (prey:pubsub:userlib pax bol)
  |=  [=bone *]
  [bone %diff %chat-update upd]
::
++  send-diff
  |=  [pax=path upd=chat-update]
  ^-  (list move)
  %-  zing
  :~  (update-subscribers /all upd)
      (update-subscribers /updates upd)
      (update-subscribers [%mailbox pax] upd)
      ?.  |(|(=(%read -.upd) =(%message -.upd)) =(%messages -.upd))
        ~
      (update-subscribers /configs upd)
      ?.  |(=(%create -.upd) =(%delete -.upd))
        ~
      (update-subscribers /keys upd)
  ==
::
++  send-two-diff
  |=  [pax=path upd=chat-two-update]
  ^-  (list move)
  %-  zing
  :~  (update-two-subscribers /all upd)
      (update-two-subscribers /updates upd)
      (update-two-subscribers [%mailbox pax] upd)
  ==
::
++  update-two-subscribers
  |=  [pax=path upd=chat-two-update]
  ^-  (list move)
  %+  turn  (prey:pubsub:userlib pax bol)
  |=  [=bone *]
  [bone %diff %chat-two-update upd]
--
