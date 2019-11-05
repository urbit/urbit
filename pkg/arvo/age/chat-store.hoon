:: chat-store: data store that holds linear sequences of chat messages
::
/+  *chat-json, *chat-eval, default-agent
|%
+$  card      card:agent:mall
+$  versioned-state
  $%  state-zero
  ==
::
+$  state-zero
  $:  %0
      =inbox
  ==
::
+$  diff
  $%  [%chat-initial inbox]
      [%chat-configs chat-configs]
      [%chat-update chat-update]
  ==
--
::
=|  state=state-zero
^-  agent:mall
=;  chat-core
  |_  =bowl:mall
  +*  this  .
      sc    ~(. chat-core bowl)
      def   ~(. default-agent bowl this)
  ::
  ++  handle-init            handle-init:def
  ++  handle-extract-state   !>(state)
  ++  handle-upgrade-state
    |=  old=vase
    `this(state !<(state-zero old))
  ::
  ++  handle-poke
    |=  [=mark =vase]
    ^-  (quip card _this)
    ?>  (team:title our.bowl src.bowl)
    =^  cards  state
      ?+  mark  (handle-poke:def mark vase)
        %json         (poke-json:sc !<(json vase))
        %chat-action  (poke-chat-action:sc !<(chat-action vase))
      ==
    [cards this]
  ::
  ++  handle-subscribe
    |=  =path
    ^-  (quip card _this)
    ?>  (team:title our.bowl src.bowl)
    |^
    =/  cards=(list card)
      ?+    path  (handle-subscribe:def path)
          [%keys ~]     (give %chat-update !>([%keys ~(key by inbox.state)]))
          [%all ~]      (give %chat-initial !>(inbox.state))
          [%configs ~]  (give %chat-configs !>((inbox-to-configs inbox.state)))
          [%updates ~]  ~
          [%mailbox @ *]
        ?>  (~(has by inbox.state) t.path)
        =/  =ship  (slav %p i.t.path)
        (give %chat-update !>([%create ship t.t.path]))
      ==
    [cards this]
    ::
    ++  give
      |=  =cage
      ^-  (list card)
      [%give %subscription-update ~ cage]~
    --
  ::
  ++  handle-unsubscribe     handle-unsubscribe:def
  ++  handle-peek
    |=  =path
    ^-  (unit (unit cage))
    ?+  path  (handle-peek:def path)
        [%x %all ~]        ``noun+!>(inbox.state)
        [%x %configs ~]    ``noun+!>((inbox-to-configs inbox.state))
        [%x %keys ~]       ``noun+!>(~(key by inbox.state))
        [%x %envelopes ~]  (peek-x-envelopes:sc t.t.path)
        [%x %mailbox *]
      ?~  t.t.path
        ~
      ``noun+!>((~(get by inbox.state) t.t.path))
    ::
        [%x %config *]
      ?~  t.t.path
        ~
      =/  mailbox  (~(get by inbox.state) t.t.path)
      ?~  mailbox
        ~
      ``noun+!>(config.u.mailbox)
    ==
  ::
  ++  handle-agent-response  handle-agent-response:def
  ++  handle-arvo-response   handle-arvo-response:def
  ++  handle-error           handle-error:def
  --
::
::
=,  state
|_  bol=bowl:mall
::
++  peek-x-envelopes
  |=  pax=path
  ^-  (unit (unit [%noun vase]))
  ?+  pax  ~
      [@ @ *]
    =/  mail-path  t.t.pax
    =/  mailbox  (~(get by inbox) mail-path)
    ?~  mailbox
      [~ ~ %noun !>(~)]
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
        [~ ~ %noun !>(envelopes)]
      [~ ~ %noun !>((swag [(sub length start) start] envelopes))]
    ::
    =/  end  (slav %ud i.t.pax)
    ?.  (lte start end)
      ~
    =.  end  ?:((lth end length) end length)
    [~ ~ %noun !>((swag [start (sub end start)] envelopes))]
  ==
::
++  poke-json
  |=  jon=json
  ^-  (quip card _state)
  (poke-chat-action (json-to-action jon))
::
++  poke-chat-action
  |=  action=chat-action
  ^-  (quip card _state)
  ?-  -.action
      %create   (handle-create action)
      %delete   (handle-delete action)
      %message  (handle-message action)
      %read     (handle-read action)
  ==
::
++  handle-create
  |=  act=chat-action
  ^-  (quip card _state)
  ?>  ?=(%create -.act)
  =/  pax  [(scot %p ship.act) path.act]
  ?:  (~(has by inbox) pax)
    [~ state]
  :-  (send-diff pax act)
  state(inbox (~(put by inbox) pax *mailbox))
::
++  handle-delete
  |=  act=chat-action
  ^-  (quip card _state)
  ?>  ?=(%delete -.act)
  =/  mailbox=(unit mailbox)  (~(get by inbox) path.act)
  ?~  mailbox
    [~ state]
  :-  (send-diff path.act act)
  state(inbox (~(del by inbox) path.act))
::
++  handle-message
  |=  act=chat-action
  ^-  (quip card _state)
  ?>  ?=(%message -.act)
  =/  mailbox=(unit mailbox)  (~(get by inbox) path.act)
  ?~  mailbox
    [~ state]
  =*  letter  letter.envelope.act
  =?  letter  &(?=(%code -.letter) ?=(~ output.letter))
    =/  =hoon  (ream expression.letter)
    letter(output (eval bol hoon))
  =:  length.config.u.mailbox  +(length.config.u.mailbox)
      number.envelope.act  +(length.config.u.mailbox)
      envelopes.u.mailbox  (snoc envelopes.u.mailbox envelope.act)
  ==
  :-  (send-diff path.act act)
  state(inbox (~(put by inbox) path.act u.mailbox))
::
++  handle-read
  |=  act=chat-action
  ^-  (quip card _state)
  ?>  ?=(%read -.act)
  =/  mailbox=(unit mailbox)  (~(get by inbox) path.act)
  ?~  mailbox
    [~ state]
  =.  read.config.u.mailbox  length.config.u.mailbox
  :-  (send-diff path.act act)
  state(inbox (~(put by inbox) path.act u.mailbox))
::
++  update-subscribers
  |=  [pax=path act=chat-action]
  ^-  (list card)
  [%give %subscription-update `pax %chat-update !>(act)]~
::
++  send-diff
  |=  [pax=path act=chat-action]
  ^-  (list card)
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
