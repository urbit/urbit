:: chat-store [landscape]:
::
:: data store that holds linear sequences of chat messages
::
/+  store=chat-store, default-agent, verb, dbug, group-store
~%  %chat-store-top  ..is  ~
|%
+$  card  card:agent:gall
+$  versioned-state
  $%  state-0
      state-1
      state-2
      state-3
  ==
::
+$  state-0  [%0 =inbox:store]
+$  state-1  [%1 =inbox:store]
+$  state-2  [%2 =inbox:store]
+$  state-3  [%3 =inbox:store]
+$  admin-action
  $%  [%trim ~]
  ==
--
::
=|  state-3
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
    ^-  (quip card _this)
    |^
    =/  old  !<(versioned-state old-vase)
    =|  cards=(list card)
    |-
    ^-  (quip card _this)
    ?-  -.old
        %3  [cards this(state old)]
      ::
        %2
      =/  =inbox:store
        (migrate-path-map:group-store inbox.old)
      =/  kick-paths
        %~  tap  in
        %+  roll
          ~(val by sup.bowl)
        |=  [[=ship sub=path] subs=(set path)]
        ^-  (set path)
        ?.  ?=([@ @ *] sub)
          subs
        ?.  &(=(%mailbox i.sub) =('~' i.t.sub))
          subs
        (~(put in subs) sub)
      =?  cards  ?=(^ kick-paths)
        :_  cards
        [%give %kick kick-paths ~ ~]
      $(old [%3 inbox])
    ::
      ?(%0 %1)  $(old (old-to-2 inbox.old))
    ::
    ==
    ::
    ++  old-to-2
      |=  =inbox:store
      ^-  state-2
      :-  %2
      %-  ~(run by inbox)
      |=  =mailbox:store
      ^-  mailbox:store
      [config.mailbox (flop envelopes.mailbox)]
    --
  ::
  ++  on-poke
    ~/  %chat-store-poke
    |=  [=mark =vase]
    ^-  (quip card _this)
    ?>  (team:title our.bowl ship.src.bowl)
    =^  cards  state
      ?+  mark  (on-poke:def mark vase)
        %json         (poke-json:cc !<(json vase))
        %chat-action  (poke-chat-action:cc !<(action:store vase))
        %noun         [~ (poke-noun:cc !<(admin-action vase))]
      ==
    [cards this]
  ::
  ++  on-watch
    ~/  %chat-store-watch
    |=  =path
    ^-  (quip card _this)
    |^
    ?>  (team:title our.bowl ship.src.bowl)
    =/  cards=(list card)
      ?+    path  (on-watch:def path)
          [%keys ~]     (give %chat-update !>([%keys ~(key by inbox)]))
          [%all ~]      (give %chat-update !>([%initial inbox]))
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
    |=  [prov=path =path]
    ^-  (unit (unit cage))
    ?+  path  (on-peek:def prov path)
        [%x %all ~]        ``noun+!>(inbox)
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
++  poke-noun
  |=  nou=admin-action
  ^-  _state
  ~&  %trimming-chat-store
  %_  state
      inbox
    %-  ~(urn by inbox)
    |=  [=path mailbox:store]
    ^-  mailbox:store
    =/  [a=* out=(list envelope:store)]
      %+  roll  envelopes
      |=  $:  =envelope:store
              o=[[hav=(set serial:store) curr=@] out=(list envelope:store)]
          ==
      ?:  (~(has in hav.o) uid.envelope)
        [[hav.o curr.o] out.o]
      :-
      ^-  [(set serial:store) @]
      [(~(put in hav.o) uid.envelope) +(curr.o)]
      ^-  (list envelope:store)
      [envelope(number curr.o) out.o]
    =/  len  (lent out)
    ~?  !=(len (lent envelopes))  [path [%old (lent envelopes)] [%new len]]
    [[len len] (flop out)]
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
  :_  state(inbox (~(put by inbox) path.action u.mailbox))
  (send-diff path.action action(envelope envelope))
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
      ?.  |(=(%create -.upd) =(%delete -.upd))
        ~
      (update-subscribers /keys upd)
  ==
--
