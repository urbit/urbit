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
  $:  synced=(map path ship)
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
++  poke-inbox-action
  |=  act=inbox-action
  ^-  (quip move _this)
  ?>  ?=(%message -.act)
  ?:  =(src.bol our.bol)
    =/  ship  (~(get by synced) path.act)
    ?~  ship
      [~ this]
    :_  this
    [ost.bol %poke / [u.ship %inbox-sync] [%inbox-action act]]~
  =/  ship  (~(get by synced) path.act)
  ?~  ship
    [~ this]
  :_  this
  ?.  =(u.ship our.bol)
    ~
  =.  author.envelope.act  src.bol
  =.  when.envelope.act  now.bol
  [ost.bol %poke / [our.bol %inbox] [%inbox-action act]]~
::
++  poke-sync-hook-action
  |=  act=sync-hook-action
  ^-  (quip move _this)
  ?-  -.act
      %add-owned
    =/  inbox-path  [%mailbox path.act]
    =/  inbox-wire  [(scot %p our.bol) inbox-path]
    =/  sync  (~(get by synced) path.act)
    ?^  sync
      [~ this]
    :_  this(synced (~(put by synced) path.act our.bol))
    [ost.bol %peer inbox-path [our.bol %inbox] inbox-path]~
  ::
      %remove-owned
    =/  inbox-wire  [(scot %p our.bol) %mailbox path.act]
    :_  this(synced (~(del by synced) path.act))
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
    ?:  (~(has by synced) path.act)
      [~ this]
    :_  this(synced (~(put by synced) path.act ship.act))
    [ost.bol %peer inbox-wire [ship.act %inbox-sync] inbox-path]~
  ::
      %remove-synced
    =/  inbox-wire  [(scot %p ship.act) %mailbox path.act]
    :_  this(synced (~(del by synced) path.act))
    [ost.bol %pull inbox-wire [ship.act %inbox-sync] ~]~
  ::
  ==
::
++  peer-mailbox
  |=  pax=path
  ^-  (quip move _this)
  ?~  pax
    [[ost.bol %quit ~]~ this]
  ?.  (~(has by synced) pax)
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
    ?.  (~(has by synced) path.diff)
      [~ this]
    =/  inbox-wire  [(scot %p our.bol) %mailbox path.diff]
    :_  this(synced (~(del by synced) path.diff))
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
      %read
    [~ this]
  ::
      %create
    ::  send a create poke to local inbox
    ?~  path.diff
      [~ this]
    =/  sync  (~(get by synced) path.diff)
    ?~  sync
      [~ this]
    ?.  =(src.bol u.sync)
      [~ this]
    :_  this
    :~  (inbox-poke diff)
    ==
  ::
      %delete
    ::  send a delete poke to local inbox
    ?~  path.diff
      [~ this]
    =/  sync  (~(get by synced) path.diff)
    ?~  sync
      [~ this]
    ?.  =(src.bol u.sync)
      [~ this]
    =/  inbox-wire  [(scot %p src.bol) %mailbox path.diff]
    :_  this(synced (~(del by synced) path.diff))
    :-  (inbox-poke diff)
    [ost.bol %pull inbox-wire [src.bol %inbox-sync] ~]~
  ::
      %message
    ?~  path.diff
      [~ this]
    =/  sync  (~(get by synced) path.diff)
    ?~  sync
      [~ this]
    ?.  =(src.bol u.sync)
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
  ~&  quit+wir
  =.  wir  `(list @tas)`wir
  =/  =ship  (slav %p &1:wir)
  =.  wir  ?^  wir  t.wir  ~
  =.  wir  ?^  wir  t.wir  ~
  ?:  (~(has by synced) wir)
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
    `path`/=inbox/(scot %da now.bol)/mailbox
    pax
    `path`/noun
  ==
  .^((unit mailbox) %gx pax)
::
--

