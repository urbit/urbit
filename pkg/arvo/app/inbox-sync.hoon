::  hook/sync-inbox.hoon
/-  *inbox, *groups, *sync-hook
|%
+$  move  [bone card]
::
+$  card
  $%  [%diff [%inbox-update inbox-update]]
      [%quit ~]
      [%poke wire dock poke]
      [%pull wire dock ~]
      [%peer wire dock path]
  ==
::
+$  state
  $%  [%0 state-zero]
  ==
::
+$  state-zero
  $:  synced=(set [ship path])
      owned=(set path)
  ==
::
+$  poke
  $%  [%group-action group-action]
      [%inbox-action inbox-action]
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
++  poke-sync-hook-action
  |=  act=sync-hook-action
  ^-  (quip move _this)
  ?-  -.act
      %add-owned
    =/  inbox-path  [%mailbox path.act]
    =/  inbox-wire  [(scot %p our.bol) inbox-path]
    ?:  (~(has in owned) path.act)
      [~ this]
    :_  this(owned (~(put in owned) path.act))
    [ost.bol %peer inbox-path [our.bol %inbox] inbox-path]~
  ::
      %remove-owned
    =/  inbox-wire  [(scot %p our.bol) %mailbox path.act]
    :_  this(owned (~(del in owned) path.act))
    :-  [ost.bol %pull inbox-wire [our.bol %inbox] ~]
    ^-  (list move)
    %+  turn  (prey:pubsub:userlib [%mailbox path.act] bol)
    |=  [=bone *]
    ^-  move
    [bone %quit ~]
  ::
      %add-synced
    =/  inbox-path  [%mailbox path.act]
    =/  inbox-wire  [(scot %p ship.act) inbox-path]
    ?:  (~(has in synced) [ship.act path.act])
      [~ this]
    :_  this(synced (~(put in synced) [ship.act path.act]))
    [ost.bol %peer inbox-wire [ship.act %inbox-sync] inbox-path]~
  ::
      %remove-synced
    =/  inbox-wire  [(scot %p ship.act) %mailbox path.act]
    :_  this(synced (~(del in synced) [ship.act path.act]))
    [ost.bol %pull inbox-wire [ship.act %inbox-sync] ~]~
  ::
  ==
::
++  peer-mailbox
  |=  pax=path
  ^-  (quip move _this)
  ?~  pax
    [[ost.bol %quit ~]~ this]
  ?.  (~(has in owned) pax)
    [[ost.bol %quit ~]~ this]
  =/  box=(unit mailbox)  (inbox-scry pax)
  ?^  box
    :_  this
    [ost.bol %diff [%inbox-update [%create pax owner.u.box]]]~
  [[ost.bol %quit ~]~ this]
::
++  diff-inbox-update
  |=  [wir=wire diff=inbox-update]
  ^-  (quip move _this)
  ?:  =(src.bol our.bol)
    (handle-local diff)
  (handle-foreign diff)
::
++  handle-local
  |=  diff=inbox-update
  ^-  (quip move _this)
  ~&  diff
  ?-  -.diff
      %keys
    [~ this]
  ::
      %create
    [~ this]
  ::
      %read
    [~ this]
  ::
      %delete
    ?.  (~(has in owned) path.diff)
      [~ this]
    =/  inbox-wire  [(scot %p our.bol) %mailbox path.diff]
    :_  this(owned (~(del in owned) path.diff))
    :-  (inbox-poke diff)
    [ost.bol %pull inbox-wire [our.bol %inbox] ~]~
  ::
      %message
    :_  this
    %+  turn  (prey:pubsub:userlib [%mailbox path.diff] bol)
    |=  [=bone *]
    ^-  move
    [bone %diff [%inbox-update diff]]
  ::
  ==
::
++  handle-foreign
  |=  diff=inbox-update
  ^-  (quip move _this)
  ?-  -.diff
      %keys
    [~ this]
  ::
      %create
    ::  send a create poke to local inbox
    ?~  path.diff
      [~ this]
    ?.  (~(has in synced) [owner.diff path.diff])
      [~ this]
    :_  this
    :~  (inbox-poke diff)
    ==
  ::
      %delete
    ::  send a delete poke to local inbox
    ?~  path.diff
      [~ this]
    ?.  (~(has in synced) [src.bol path.diff])
      [~ this]
    =/  inbox-wire  [(scot %p src.bol) %mailbox path.diff]
    :_  this(synced (~(del in synced) [src.bol path.diff]))
    :-  (inbox-poke diff)
    [ost.bol %pull inbox-wire [src.bol %inbox-sync] ~]~
  ::
      %message
    ?~  path.diff
      [~ this]
    ?.  (~(has in synced) [src.bol path.diff])
      [~ this]
    :_  this
    :~  (inbox-poke diff)
    ==
  ::
      %read
    ?~  path.diff
      [~ this]
    ?.  (~(has in synced) [src.bol path.diff])
      [~ this]
    :_  this
    :~  (inbox-poke diff)
    ==
  ::
  ==
::
++  quit
  |=  wir=wire
  ^-  (quip move _this)
  =.  wir  `(list @tas)`wir
  =/  =ship  (slav %p &1:wir)
  =.  wir  ?^  wir  t.wir  ~
  =.  wir  ?^  wir  t.wir  ~
  ?:  (~(has in synced) [ship wir])
    ::  resubscribe
    [~ this]
  ::  no-op
  [~ this]
::
++  inbox-poke
  |=  act=inbox-action
  ^-  move
  [ost.bol %poke /inbox-sync [our.bol %inbox] [%inbox-action act]]
::
++  inbox-scry
  |=  pax=path
  ^-  (unit mailbox)
  =.  pax  ;:  weld
    `path`/=inbox/(scot %da now.bol)
    pax
    `path`/noun
  ==
  .^((unit mailbox) %gx pax)
::
--

