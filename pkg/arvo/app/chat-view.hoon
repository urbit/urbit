/-  *inbox
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
  $%  [%inbox-action inbox-action]
  ==
::
+$  diff
  $%  [%chat-view configs path (list envelope)]
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
  [~ this]
::
++  poke-noun
  |=  a=@t
  ^-  (quip move _this)
  =/  parse
    %-  ot:dejs:format
    :~  chat-input+so:dejs:format
    ==
  =/  env=envelope  [`@uvH`eny.bol our.bol now.bol %text a]
  =/  act=inbox-action  [%message /chat1 env]
  ~&  act+act
  :_  this
  [ost.bol %poke /send-message [our.bol %inbox] %inbox-action act]~
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
  ~&  dat+dat
  =/  env=envelope  [`@uvH`eny.bol our.bol now.bol %text mes.dat]
  =/  act=inbox-action  [%message chat.dat env]
  ?:  =(our.bol who.dat)
    :_  this
    [ost.bol %poke /send-message [our.bol %inbox] %inbox-action act]~
  :_  this
  [ost.bol %poke /send-message [who.dat %inbox-sync] %inbox-action act]~
::
++  peer-chat
  |=  wir=wire
  ^-  (quip move _this)
  ?>  ?=([@tas @tas *] wir)
  :_  this
  :-  [ost.bol %peer /inbox/all [our.bol %inbox] /all/updates]
  send-update
::
++  diff-inbox-update
  |=  [wir=wire upd=inbox-update]
  ^-  (quip move _this)
  :_  this
  send-update
::
++  send-update
  ^-  (list move)
  =/  conf=configs  .^(configs %gx /=inbox/[(scot %da now.bol)]/configs/noun)
  %+  turn  (prey:pubsub:userlib /chat bol)
  |=  [b=bone who=@p wir=wire]
  ^-  move
  ?>  ?=([@tas @tas @tas *] wir)  ::  /chat/x/y/chat-name
  =/  scr=path  ;:(welp /=inbox/[(scot %da now.bol)]/envelopes t.wir /noun)
  =/  envs=(list envelope)  .^((list envelope) %gx scr)
  [b %diff %chat-view conf t.t.t.wir envs]
::
--
