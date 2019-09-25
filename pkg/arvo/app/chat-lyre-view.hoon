/-  *chat-store, *chat-hook, *permission-store, lyre
::
|%
::
+$  move  [bone card]
::
+$  card
  $%  [%poke wire dock poke]
      [%diff diff]
      [%peer wire dock wire]
  ==
::
+$  poke
  $%  [%chat-action chat-action]
      [%permission-action permission-action]
      [%chat-hook-action chat-hook-action]
      [%lyre-action action:lyre]
  ==
::
+$  diff
  $%  [%chat-view configs path (list envelope)]
  ==
::
+$  poked-noun
  $%  [%create pax=path sus=(set ship)]
      [%join who=@p pax=path]
  ==
--
::
|_  [bol=bowl:gall ~]
::
++  this  .
::
++  prep
  |=  old=(unit *)
  ^-  (quip move _this)
  ?~  old
    =/  act=action:lyre
      [%new-view %chat [~ [%chat-lyre-view /chat/-50/0/chat1]~ ~ %chat]]
    :_  this
    [ost.bol %poke /create-chat [our.bol %lyre] %lyre-action act]~
  [~ this]
::
++  send-poke
  |=  [app=@tas pok=poke]
  ^-  move
  [ost.bol %poke /send-poke [our.bol app] pok]
::
++  poke-noun
  |=  a=poked-noun
  ^-  (quip move _this)
  ?-  -.a
      %create
    :_  this
    :~  (send-poke %chat-store %chat-action %create pax.a our.bol)
    ::
        %+  send-poke  %permission-store
        [%permission-action %create :(welp /chat pax.a /read) [%white sus.a]]
    ::
        %+  send-poke  %permission-store
        [%permission-action %create :(welp /chat pax.a /write) [%white sus.a]]
    ::
        (send-poke %chat-hook %chat-hook-action %add-owned pax.a %village)
    ==
  ::
      %join
    :_  this
    [(send-poke %chat-hook %chat-hook-action %add-synced who.a pax.a)]~
  ==
::
++  poke-json
  |=  jon=json
  ^-  (quip move _this)
  =/  parse
    %-  ot:dejs:format
    :~  message+so:dejs:format
        who+(su:dejs:format ;~(pfix sig fed:ag))
        chat+(su:dejs:format ;~(pfix net (more net urs:ab)))
    ==
  =/  dat=[mes=@t who=@p chat=path]  (parse jon)
  =/  env=envelope  [`@uvH`eny.bol 0 our.bol now.bol %text mes.dat]
  =/  act=chat-action  [%message chat.dat env]
  :_  this
  [ost.bol %poke /send-message [who.dat %chat-hook] %chat-action act]~
::
++  peer-chat
  |=  wir=wire
  ^-  (quip move _this)
  ?>  ?=([@tas @tas *] wir)
  :_  this
  :-  [ost.bol %peer /chat/updates [our.bol %chat-store] /updates]
  send-update
::
++  diff-chat-update
  |=  [wir=wire upd=chat-update]
  ^-  (quip move _this)
  :_  this
  send-update
::
++  send-update
  ^-  (list move)
  =/  bek=path  /=chat-store/[(scot %da now.bol)]
  =/  conf=configs  .^(configs %gx (weld bek /configs/noun))
  %+  turn  (prey:pubsub:userlib /chat bol)
  |=  [b=bone who=@p wir=wire]
  ^-  move
  ?>  ?=([@tas @tas @tas *] wir)  ::  /chat/x/y/chat-name
  =/  scr=path  ;:(welp bek /envelopes t.wir /noun)
  =/  envs=(list envelope)  .^((list envelope) %gx scr)
  [b %diff %chat-view conf t.t.t.wir envs]
::
--
