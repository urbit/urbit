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
      boned=(map wire (list bone))
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
  ~&  bol
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
    ?.  =(src.bol our.bol)
      [~ this]
    =/  inbox-path  [%mailbox path.act]
    =/  inbox-wire  [(scot %p our.bol) inbox-path]
    ?:  (~(has by synced) path.act)
      [~ this]
    =.  synced  (~(put by synced) path.act our.bol)
    :_  (track-bone inbox-wire)
    %+  weld
      [ost.bol %peer inbox-path [our.bol %inbox] inbox-path]~
    (create-permission [%inbox path.act] security.act)
  ::
      %add-synced
    ?.  =(src.bol our.bol)
      [~ this]
    =/  inbox-path  [%mailbox path.act]
    =/  inbox-wire  [(scot %p ship.act) inbox-path]
    ?:  (~(has by synced) path.act)
      [~ this]
    =.  synced  (~(put by synced) path.act ship.act)
    :_  (track-bone inbox-wire)
    [ost.bol %peer inbox-wire [ship.act %inbox-sync] inbox-path]~
  ::
      %remove
    =/  ship  (~(get by synced) path.act)
    ?~  ship
      [~ this]
    ?:  &(=(u.ship our.bol) =(our.bol src.bol))
      ::  delete one of our own paths
      =/  inbox-wire  [(scot %p our.bol) %mailbox path.act]
      :_
      %_  this
        synced  (~(del by synced) path.act)
        boned  (~(del by boned) inbox-wire)
      ==
      ;:  weld
        (pull-wire inbox-wire path.act)
      ::
        (delete-permission [%inbox path.act])    
      ::
        ^-  (list move)
        %+  turn  (prey:pubsub:userlib [%mailbox path.act] bol)
        |=  [=bone *]
        ^-  move
        [bone %quit ~]
      ==
    ?:  |(=(u.ship src.bol) =(our.bol src.bol))
      ::  delete a foreign ship's path
      =/  inbox-wire  [(scot %p u.ship) %mailbox path.act]
      :_
      %_  this
        synced  (~(del by synced) path.act)
        boned  (~(del by boned) inbox-wire)
      ==
      (pull-wire inbox-wire path.act)
    :: don't allow
    [~ this]
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
  ::  scry permissions to check if read is permitted
  ?.  (permitted-scry [(scot %p src.bol) %inbox (weld pax /write)])
    [[ost.bol %quit ~]~ this]
  =/  box=(unit mailbox)  (inbox-scry pax)
  ?~  box
    [[ost.bol %quit ~]~ this]
  :_  this
  [ost.bol %diff [%inbox-update [%create pax owner.u.box]]]~
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
    :_  (track-bone inbox-wire)
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
++  track-bone
  |=  wir=wire
  ^+  this
  =/  bnd  (~(get by boned) wir)
  ?^  bnd
    this(boned (~(put by boned) wir (snoc u.bnd ost.bol)))
  this(boned (~(put by boned) wir [ost.bol]~))
::
++  pull-wire
  |=  [wir=wire pax=path]
  ^-  (list move)
  =/  bnd  (~(get by boned) wir)
  ~&  bnd
  ?~  bnd
    ~
  =/  shp  (~(get by synced) pax)
  ?~  shp
    ~
  %+  turn  u.bnd
  |=  ost=bone
  ^-  move
  ?:  =(u.shp our.bol)
    [ost %pull wir [our.bol %inbox] ~]
  [ost %pull wir [u.shp %inbox-sync] ~]
::
--

