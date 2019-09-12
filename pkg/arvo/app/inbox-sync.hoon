::  hook/sync-inbox.hoon
/-  *inbox, *permissions, *inbox-permission-hook
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
  $%  [%inbox-action inbox-action]
      [%permission-action permission-action]
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
++  poke-noun
  |=  a=*
  ^-  (quip move _this)
  ~&  synced
  [~ this]
::
++  poke-inbox-action
  |=  act=inbox-action
  ^-  (quip move _this)
  ?>  ?=(%message -.act)
  ::  local
  ?:  =(src.bol our.bol)
    =/  ship  (~(get by synced) path.act)
    ?~  ship
      [~ this]
    :_  this
    [ost.bol %poke / [u.ship %inbox-sync] [%inbox-action act]]~
  ::  foreign
  =/  ship  (~(get by synced) path.act)
  ?~  ship
    [~ this]
  :_  this
  ?.  =(u.ship our.bol)
    ~
  ::  scry permissions to check if write is permitted
  ?.  (permitted-scry [(scot %p src.bol) %inbox (weld path.act /write)])
    ~
  =.  author.envelope.act  src.bol
  =.  when.envelope.act  now.bol
  [ost.bol %poke / [our.bol %inbox] [%inbox-action act]]~
::
++  poke-permission-hook-action
  |=  act=permission-hook-action
  ^-  (quip move _this)
  ?-  -.act
      %add-owned
    =/  inbox-path  [%mailbox path.act]
    =/  inbox-wire  [(scot %p our.bol) inbox-path]
    ?:  (~(has by synced) path.act)
      [~ this]
    :_  this(synced (~(put by synced) path.act our.bol))
    :-  [ost.bol %peer inbox-path [our.bol %inbox] inbox-path]
    (create-permission [%inbox path.act] security.act)
  ::
      %remove-owned
    =/  inbox-wire  [(scot %p our.bol) %mailbox path.act]
    :_  this(synced (~(del by synced) path.act))
    ;:  weld
      [ost.bol %pull inbox-wire [our.bol %inbox] ~]~
    ::
      (delete-permission [%inbox path.act])    
    ::
      ^-  (list move)
      %+  turn  (prey:pubsub:userlib [%mailbox path.act] bol)
      |=  [=bone *]
      ^-  move
      [bone %quit ~]
    ==
  ::
      %add-synced
    ~&  'add-synced'
    =/  inbox-path  [%mailbox path.act]
    =/  inbox-wire  [(scot %p ship.act) inbox-path]
    ~&  inbox-wire
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
  ~&  pax
  ?~  pax
    [[ost.bol %quit ~]~ this]
  ?.  (~(has by synced) pax)
    ~&  'no has'
    [[ost.bol %quit ~]~ this]
  ::  scry permissions to check if read is permitted
  ~&  'is permitted?'
  ?.  (permitted-scry [(scot %p src.bol) %inbox (weld pax /write)])
    ~&  'no'
    [[ost.bol %quit ~]~ this]
  ~&  'yes'
  =/  box=(unit mailbox)  (inbox-scry pax)
  ?~  box
    ~&  'no has'
    [[ost.bol %quit ~]~ this]
  ~&  'we send'
  :_  this
  [ost.bol %diff [%inbox-update [%create pax owner.u.box]]]~
::
++  diff-inbox-update
  |=  [wir=wire diff=inbox-update]
  ^-  (quip move _this)
  ~&  diff
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
  ~&  foreign+diff
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
  =/  wir  `(list @tas)`wir
  =/  =ship  (slav %p &1:wir)
  =.  wir  ?^  wir  t.wir  ~
  =.  wir  ?^  wir  t.wir  ~
  ?:  (~(has by synced) wir)
    ::  resubscribe
    ::  could cause infinite loop. what do?
    ::  could implement exponential backoff in an arcane way, hook
    ::  would need timers and potentially many @da added to inbox-wire
    ::  this(synced (~(del by synced) path.act))
    =/  inbox-path  [%mailbox wir]
    =/  inbox-wire  [(scot %p ship) inbox-path]
    :_  this
    [ost.bol %peer inbox-wire [ship %inbox-sync] inbox-path]~
  ::  no-op
  [~ this]
::
++  inbox-poke
  |=  act=inbox-action
  ^-  move
  [ost.bol %poke /inbox-sync [our.bol %inbox] [%inbox-action act]]
::
++  permission-poke
  |=  act=permission-action
  ^-  move
  [ost.bol %poke /inbox-sync [our.bol %permissions] [%permission-action act]]
::
++  create-permission
  |=  [pax=path sec=inbox-security]
  ^-  (list move)
  =/  read-perm   (weld pax /read)
  =/  write-perm  (weld pax /write)
  ?-  sec
      %channel
    :~  (permission-poke (sec-to-perm read-perm %black))
        (permission-poke (sec-to-perm write-perm %black))
    ==
  ::
      %village
    :~  (permission-poke (sec-to-perm read-perm %white))
        (permission-poke (sec-to-perm write-perm %white))
    ==
  ::
      %journal
    :~  (permission-poke (sec-to-perm read-perm %black))
        (permission-poke (sec-to-perm write-perm %white))
    ==
  ::
      %mailbox
    :~  (permission-poke (sec-to-perm read-perm %white))
        (permission-poke (sec-to-perm write-perm %black))
    ==
  ::
  ==
::
++  delete-permission
  |=  pax=path
  ^-  (list move)
  =/  read-perm   (weld pax /read)
  =/  write-perm  (weld pax /write)
  :~  (permission-poke [%delete read-perm])
      (permission-poke [%delete write-perm])
  ==
::
++  sec-to-perm
  |=  [pax=path =kind]
  ^-  permission-action
  [%create pax kind *(set ship)]
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
++  permitted-scry
  |=  pax=path
  ^-  ?
  =.  pax  ;:  weld
    `path`/=permissions/(scot %da now.bol)/permitted
    pax
    `path`/noun
  ==
  .^(? %gx pax)
::
--

