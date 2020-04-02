:: chat-store: data store that holds linear sequences of chat messages
::
/+  *chat-json, *chat-eval, default-agent, verb, dbug
|%
+$  card  card:agent:gall
+$  versioned-state
  $%  state-zero
      state-one
  ==
::
+$  state-zero  [%0 =inbox]
+$  state-one   [%1 =inbox]
::
+$  diff
  $%  [%chat-initial inbox]
      [%chat-configs chat-configs]
      [%chat-update chat-update]
  ==
--
::
=|  state-one
=*  state  -
::
%-  agent:dbug
%+  verb  |
^-  agent:gall
=<
  |_  =bowl:gall
  +*  this       .
      chat-core  +>
      cc         ~(. chat-core bowl)
      def        ~(. (default-agent this %|) bowl)
  ::
  ++  on-init   on-init:def
  ++  on-save   !>(state)
  ++  on-load
    |=  old-vase=vase
    =/  old  !<(versioned-state old-vase)
    ?:  ?=(%1 -.old)
      [~ this(state old)]
    :_  this(state [%1 inbox.old])
    [%pass /lo-chst %agent [our.bowl %chat-hook] %poke %noun !>(%store-load)]~
  ::
  ++  on-poke
    |=  [=mark =vase]
    ^-  (quip card _this)
    ?>  (team:title our.bowl src.bowl)
    =^  cards  state
      ?+  mark  (on-poke:def mark vase)
        %json         (poke-json:cc !<(json vase))
        %chat-action  (poke-chat-action:cc !<(chat-action vase))
      ==
    [cards this]
  ::
  ++  on-watch
    |=  =path
    ^-  (quip card _this)
    |^
    ?>  (team:title our.bowl src.bowl)
    =/  cards=(list card)
      ?+    path  (on-watch:def path)
          [%keys ~]     (give %chat-update !>([%keys ~(key by inbox)]))
          [%all ~]      (give %chat-initial !>(inbox))
          [%configs ~]  (give %chat-configs !>((inbox-to-configs inbox)))
          [%updates ~]  ~
          [%mailbox @ *]
        ?>  (~(has by inbox) t.path)
        (give %chat-update !>([%create t.path]))
      ==
    [cards this]
    ::
    ++  give
      |=  =cage
      ^-  (list card)
      [%give %fact ~ cage]~
    --
  ::
  ++  on-leave  on-leave:def
  ++  on-peek
    |=  =path
    ^-  (unit (unit cage))
    ?+  path  (on-peek:def path)
        [%x %all ~]        ``noun+!>(inbox)
        [%x %configs ~]    ``noun+!>((inbox-to-configs inbox))
        [%x %keys ~]       ``noun+!>(~(key by inbox))
        [%x %envelopes *]  (peek-x-envelopes:cc t.t.path)
        [%x %mailbox *]
      ?~  t.t.path
        ~
      ``noun+!>((~(get by inbox) t.t.path))
    ::
        [%x %config *]
      ?~  t.t.path
        ~
      =/  mailbox  (~(get by inbox) t.t.path)
      ?~  mailbox
        ~
      ``noun+!>(config.u.mailbox)
    ==
  ::
  ++  on-agent  on-agent:def
  ++  on-arvo   on-arvo:def
  ++  on-fail   on-fail:def
  --
::
::
|_  bol=bowl:gall
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
      %create    (handle-create action)
      %delete    (handle-delete action)
      %read      (handle-read action)
      %messages  (handle-messages action)
      %message
        ?.  =(our.bol author.envelope.action)
          (handle-message action)
        =^  message-moves  state  (handle-message action)
        =^  read-moves  state  (handle-read [%read path.action])
        [(weld message-moves read-moves) state]
  ==
::
++  handle-create
  |=  act=chat-action
  ^-  (quip card _state)
  ?>  ?=(%create -.act)
  ?:  (~(has by inbox) path.act)  [~ state]
  :-  (send-diff path.act act)
  state(inbox (~(put by inbox) path.act *mailbox))
::
++  handle-delete
  |=  act=chat-action
  ^-  (quip card _state)
  ?>  ?=(%delete -.act)
  =/  mailbox=(unit mailbox)  (~(get by inbox) path.act)
  ?~  mailbox  [~ state]
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
  =.  letter.envelope.act  (evaluate-letter [author letter]:envelope.act)
  =^  envelope  u.mailbox  (append-envelope u.mailbox envelope.act)
  :-  (send-diff path.act act(envelope envelope))
  state(inbox (~(put by inbox) path.act u.mailbox))
::
++  handle-messages
  |=  act=chat-action
  ^-  (quip card _state)
  ?>  ?=(%messages -.act)
  =/  mailbox=(unit mailbox)  (~(get by inbox) path.act)
  ?~  mailbox
    [~ state]
  =/  evaluated-envelopes=(list envelope)  ~
  |-  ^-  (quip card _state)
  ?~  envelopes.act
    :_  state(inbox (~(put by inbox) path.act u.mailbox))
    %+  send-diff  path.act
    :*  %messages
        path.act
        (sub length.config.u.mailbox (lent evaluated-envelopes))
        length.config.u.mailbox
        evaluated-envelopes
    ==
  =.  letter.i.envelopes.act  (evaluate-letter [author letter]:i.envelopes.act)
  =^  envelope  u.mailbox  (append-envelope u.mailbox i.envelopes.act)
  =.  evaluated-envelopes  (snoc evaluated-envelopes envelope)
  $(envelopes.act t.envelopes.act)
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
++  evaluate-letter
  |=  [author=ship =letter]
  ^-  ^letter
  =?  letter
      ?&  ?=(%code -.letter)
          ?=(~ output.letter)
          (team:title our.bol author)
      ==
    =/  =hoon  (ream expression.letter)
    letter(output (eval bol hoon))
  letter
::
++  append-envelope
  |=  [=mailbox =envelope]
  ^+  [envelope mailbox]
  =.  number.envelope  +(length.config.mailbox)
  =:  length.config.mailbox  +(length.config.mailbox)
      envelopes.mailbox  (snoc envelopes.mailbox envelope)
  ==
  [envelope mailbox]
::
++  update-subscribers
  |=  [pax=path update=chat-update]
  ^-  (list card)
  [%give %fact ~[pax] %chat-update !>(update)]~
::
++  send-diff
  |=  [pax=path upd=chat-update]
  ^-  (list card)
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
--
