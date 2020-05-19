:: chat-store: data store that holds linear sequences of chat messages
::
/+  store=chat-store, default-agent, verb, dbug
~%  %chat-store-top  ..is  ~
|%
+$  card  card:agent:gall
+$  versioned-state
  $%  state-zero
      state-one
      state-two
  ==
::
+$  state-zero  [%0 =inbox:store]
+$  state-one   [%1 =inbox:store]
+$  state-two   [%2 =inbox:store]
::
+$  diff
  $%  [%chat-initial inbox:store]
      [%chat-configs configs:store]
      [%chat-update update:store]
  ==
--
::
=|  state-two
=*  state  -
::
%-  agent:dbug
%+  verb  |
^-  agent:gall
=<
  ~%  %chat-store-agent-core  ..peek-x-envelopes  ~
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
    ?:  ?=(%2 -.old)
      [~ this(state old)]
    =/  reversed-inbox=inbox:store
      %-  ~(run by inbox.old)
      |=  =mailbox:store
      ^-  mailbox:store
      [config.mailbox (flop envelopes.mailbox)]
    [~ this(state [%2 reversed-inbox])]
  ::
  ++  on-poke
    ~/  %chat-store-poke
    |=  [=mark =vase]
    ^-  (quip card _this)
    ?>  (team:title our.bowl src.bowl)
    =^  cards  state
      ?+  mark  (on-poke:def mark vase)
        %json         (poke-json:cc !<(json vase))
        %chat-action  (poke-chat-action:cc !<(action:store vase))
      ==
    [cards this]
  ::
  ++  on-watch
    ~/  %chat-store-watch
    |=  =path
    ^-  (quip card _this)
    |^
    ?>  (team:title our.bowl src.bowl)
    =/  cards=(list card)
      ?+    path  (on-watch:def path)
          [%keys ~]     (give %chat-update !>([%keys ~(key by inbox)]))
          [%all ~]      (give %chat-initial !>(inbox))
          [%configs ~]  (give %chat-configs !>((inbox-to-configs:store inbox)))
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
    ~/  %chat-store-peek
    |=  =path
    ^-  (unit (unit cage))
    ?+  path  (on-peek:def path)
        [%x %all ~]        ``noun+!>(inbox)
        [%x %configs ~]    ``noun+!>((inbox-to-configs:store inbox))
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
~%  %chat-store-library  ..card  ~
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
  (poke-chat-action (action:dejs:store jon))
::
++  poke-chat-action
  |=  =action:store
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
  |=  =action:store
  ^-  (quip card _state)
  ?>  ?=(%create -.action)
  ?:  (~(has by inbox) path.action)  [~ state]
  :-  (send-diff path.action action)
  state(inbox (~(put by inbox) path.action *mailbox:store))
::
++  handle-delete
  |=  =action:store
  ^-  (quip card _state)
  ?>  ?=(%delete -.action)
  =/  mailbox=(unit mailbox:store)
    (~(get by inbox) path.action)
  ?~  mailbox  [~ state]
  :-  (send-diff path.action action)
  state(inbox (~(del by inbox) path.action))
::
++  handle-message
  |=  =action:store
  ^-  (quip card _state)
  ?>  ?=(%message -.action)
  =/  mailbox=(unit mailbox:store)
    (~(get by inbox) path.action)
  ?~  mailbox
    [~ state]
  =.  letter.envelope.action  (evaluate-letter [author letter]:envelope.action)
  =^  envelope  u.mailbox  (prepend-envelope u.mailbox envelope.action)
  :-  (send-diff path.action action(envelope envelope))
  state(inbox (~(put by inbox) path.action u.mailbox))
::
++  handle-messages
  |=  act=action:store
  ^-  (quip card _state)
  ?>  ?=(%messages -.act)
  =/  mailbox=(unit mailbox:store)
    (~(get by inbox) path.act)
  ?~  mailbox
    [~ state]
  =.  envelopes.act  (flop envelopes.act)
  =|  evaluated-envelopes=(list envelope:store)
  |-  ^-  (quip card _state)
  ?~  envelopes.act
    :_  state(inbox (~(put by inbox) path.act u.mailbox))
    %+  send-diff  path.act
    [%messages path.act 0 (lent evaluated-envelopes) evaluated-envelopes]
  =.  letter.i.envelopes.act  (evaluate-letter [author letter]:i.envelopes.act)
  =^  envelope  u.mailbox  (prepend-envelope u.mailbox i.envelopes.act)
  =.  evaluated-envelopes  [envelope evaluated-envelopes]
  $(envelopes.act t.envelopes.act)
::
++  handle-read
  |=  act=action:store
  ^-  (quip card _state)
  ?>  ?=(%read -.act)
  =/  mailbox=(unit mailbox:store)  (~(get by inbox) path.act)
  ?~  mailbox
    [~ state]
  =.  read.config.u.mailbox  length.config.u.mailbox
  :-  (send-diff path.act act)
  state(inbox (~(put by inbox) path.act u.mailbox))
::
++  evaluate-letter
  |=  [author=ship =letter:store]
  ^-  letter:store
  =?  letter
      ?&  ?=(%code -.letter)
          ?=(~ output.letter)
          (team:title our.bol author)
      ==
    =/  =hoon  (ream expression.letter)
    letter(output (eval:store bol hoon))
  letter
::
++  prepend-envelope
  |=  [=mailbox:store =envelope:store]
  ^+  [envelope mailbox]
  =.  number.envelope  +(length.config.mailbox)
  =:  length.config.mailbox  +(length.config.mailbox)
      envelopes.mailbox  [envelope envelopes.mailbox]
  ==
  [envelope mailbox]
::
++  update-subscribers
  |=  [pax=path =update:store]
  ^-  (list card)
  [%give %fact ~[pax] %chat-update !>(update)]~
::
++  send-diff
  |=  [pax=path upd=update:store]
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
