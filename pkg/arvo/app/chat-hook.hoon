/-  *permission-store, *chat-hook
/+  *chat-json
|%
+$  move  [bone card]
::
+$  card
  $%  [%diff [%chat-update chat-update]]
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
  $%  [%chat-action chat-action]
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
++  poke-json
  |=  jon=json
  ^-  (quip move _this)
  (poke-chat-action (json-to-action jon))
::
++  poke-chat-action
  |=  act=chat-action
  ^-  (quip move _this)
  ?>  ?=(%message -.act)
  ::  local
  ?:  =(src.bol our.bol)
    =/  ship  (~(get by synced) path.act)
    ?~  ship
      [~ this]
    :_  this
    [ost.bol %poke / [u.ship %chat-store] [%chat-action act]]~
  ::  foreign
  =/  ship  (~(get by synced) path.act)
  ?~  ship
    [~ this]
  :_  this
  ?.  =(u.ship our.bol)
    ~
  ::  scry permissions to check if write is permitted
  ?.  (permitted-scry [(scot %p src.bol) %chat (weld path.act /write)])
    ~
  =.  author.envelope.act  src.bol
  =.  when.envelope.act  now.bol
  [ost.bol %poke / [our.bol %chat-store] [%chat-action act]]~
::
++  poke-chat-hook-action
  |=  act=chat-hook-action
  ^-  (quip move _this)
  ?-  -.act
      %add-owned
    ?.  =(src.bol our.bol)
      [~ this]
    =/  chat-path  [%mailbox path.act]
    =/  chat-wire  [(scot %p our.bol) chat-path]
    ?:  (~(has by synced) path.act)
      [~ this]
    =.  synced  (~(put by synced) path.act our.bol)
    :_  (track-bone chat-wire)
    %+  weld
      [ost.bol %peer chat-wire [our.bol %chat-store] chat-path]~
    (create-permission [%chat path.act] security.act)
  ::
      %add-synced
    ?.  =(src.bol our.bol)
      [~ this]
    =/  chat-path  [%mailbox path.act]
    =/  chat-wire  [(scot %p ship.act) chat-path]
    ?:  (~(has by synced) path.act)
      [~ this]
    =.  synced  (~(put by synced) path.act ship.act)
    :_  (track-bone chat-wire)
    [ost.bol %peer chat-wire [ship.act %chat-hook] chat-path]~
  ::
      %remove
    =/  ship  (~(get by synced) path.act)
    ?~  ship
      [~ this]
    ?:  &(=(u.ship our.bol) =(our.bol src.bol))
      ::  delete one of our own paths
      =/  chat-wire  [(scot %p our.bol) %mailbox path.act]
      :_
      %_  this
        synced  (~(del by synced) path.act)
        boned  (~(del by boned) chat-wire)
      ==
      ;:  weld
        (pull-wire chat-wire path.act)
      ::
        (delete-permission [%chat path.act])
      ::
        ^-  (list move)
        %+  turn  (prey:pubsub:userlib [%mailbox path.act] bol)
        |=  [=bone *]
        ^-  move
        [bone %quit ~]
      ==
    ?:  |(=(u.ship src.bol) =(our.bol src.bol))
      ::  delete a foreign ship's path
      =/  chat-wire  [(scot %p u.ship) %mailbox path.act]
      :_
      %_  this
        synced  (~(del by synced) path.act)
        boned  (~(del by boned) chat-wire)
      ==
      (pull-wire chat-wire path.act)
    :: don't allow
    [~ this]
  ::
  ==
::
++  peer-mailbox
  |=  pax=path
  ^-  (quip move _this)
  ?~  pax  !!
  ?.  (~(has by synced) pax)  !!
  ::  scry permissions to check if read is permitted
  ?.  (permitted-scry [(scot %p src.bol) %chat (weld pax /write)])
    !!
  =/  box=(unit mailbox)  (chat-scry pax)
  ?~  box  !!
  :_  this
  [ost.bol %diff [%chat-update [%create pax owner.config.u.box]]]~
::
++  diff-chat-update
  |=  [wir=wire diff=chat-update]
  ^-  (quip move _this)
  ?:  =(src.bol our.bol)
    (handle-local diff)
  (handle-foreign diff)
::
++  handle-local
  |=  diff=chat-update
  ^-  (quip move _this)
  ?-  -.diff
      %keys
    [~ this]
  ::
      %config
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
    =/  chat-wire  [(scot %p our.bol) %mailbox path.diff]
    :_  this(synced (~(del by synced) path.diff))
    :-  (chat-poke diff)
    [ost.bol %pull chat-wire [our.bol %chat-store] ~]~
  ::
      %message
    :_  this
    %+  turn  (prey:pubsub:userlib [%mailbox path.diff] bol)
    |=  [=bone *]
    ^-  move
    [bone %diff [%chat-update diff]]
  ::
  ==
::
++  handle-foreign
  |=  diff=chat-update
  ^-  (quip move _this)
  ?-  -.diff
      %keys
    [~ this]
  ::
      %config
    [~ this]
  ::
      %read
    [~ this]
  ::
      %create
    ::  send a create poke to local chat
    ?~  path.diff
      [~ this]
    =/  sync  (~(get by synced) path.diff)
    ?~  sync
      [~ this]
    ?.  =(src.bol u.sync)
      [~ this]
    :_  this
    :~  (chat-poke diff)
    ==
  ::
      %delete
    ::  send a delete poke to local chat
    ?~  path.diff
      [~ this]
    =/  sync  (~(get by synced) path.diff)
    ?~  sync
      [~ this]
    ?.  =(src.bol u.sync)
      [~ this]
    =/  chat-wire  [(scot %p src.bol) %mailbox path.diff]
    :_  this(synced (~(del by synced) path.diff))
    :-  (chat-poke diff)
    [ost.bol %pull chat-wire [src.bol %chat-hook] ~]~
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
    :~  (chat-poke diff)
    ==
  ::
  ==
::
++  quit
  |=  wir=wire
  ^-  (quip move _this)
  =/  wir  `(list @tas)`wir
  =/  =ship  (slav %p &1:wir)
  =.  wir  ?^  wir  t.wir  ~
  =.  wir  ?^  wir  t.wir  ~
  ?:  (~(has by synced) wir)
    =/  chat-path  [%mailbox wir]
    =/  chat-wire  [(scot %p ship) chat-path]
    :_  (track-bone chat-wire)
    [ost.bol %peer chat-wire [ship %chat-hook] chat-path]~
  ::  no-op
  [~ this]
::
++  reap
  |=  [wir=wire saw=(unit tang)]
  ^-  (quip move _this)
  ?~  saw
    [~ this]
  =/  wir  `(list @tas)`wir
  =/  =ship  (slav %p &1:wir)
  =.  wir  ?^  wir  t.wir  ~
  =.  wir  ?^  wir  t.wir  ~
  [~ this(synced (~(del by synced) wir))]
::
++  chat-poke
  |=  act=chat-action
  ^-  move
  [ost.bol %poke / [our.bol %chat-store] [%chat-action act]]
::
++  permission-poke
  |=  act=permission-action
  ^-  move
  [ost.bol %poke / [our.bol %permission-store] [%permission-action act]]
::
++  create-permission
  |=  [pax=path sec=chat-security]
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
++  chat-scry
  |=  pax=path
  ^-  (unit mailbox)
  =.  pax  ;:  weld
    `path`/=chat-store/(scot %da now.bol)/mailbox
    pax
    `path`/noun
  ==
  .^((unit mailbox) %gx pax)
::
++  permitted-scry
  |=  pax=path
  ^-  ?
  =.  pax  ;:  weld
    `path`/=permission-store/(scot %da now.bol)/permitted
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
  ?~  bnd
    ~
  =/  shp  (~(get by synced) pax)
  ?~  shp
    ~
  %+  turn  u.bnd
  |=  ost=bone
  ^-  move
  ?:  =(u.shp our.bol)
    [ost %pull wir [our.bol %chat-store] ~]
  [ost %pull wir [u.shp %chat-hook] ~]
::
--
