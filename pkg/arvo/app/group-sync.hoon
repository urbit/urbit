::  hook/sync-group.hoon
/-  *groups, *sync-hook
|%
+$  move  [bone card]
::
+$  card
  $%  [%diff [%group-update group-update]]
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
      [%sync-hook-action sync-hook-action]
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
    =/  group-path  [%group path.act]
    =/  group-wire  [(scot %p our.bol) group-path]
    ?:  (~(has by synced) path.act)
      [~ this]
    :_  this(synced (~(put by synced) path.act our.bol))
    [ost.bol %peer group-path [our.bol %groups] group-path]~
  ::
      %remove-owned

    =/  group-wire  [(scot %p our.bol) %group path.act]
    :_  this(synced (~(del by synced) path.act))
    :-  [ost.bol %pull group-wire [our.bol %groups] ~]
    ^-  (list move)
    %+  turn  (prey:pubsub:userlib [%group path.act] bol)
    |=  [=bone *]
    ^-  move
    [bone %quit ~]
  ::
      %add-synced
    =/  group-path  [%group path.act]
    =/  group-wire  [(scot %p ship.act) group-path]
    ?:  (~(has by synced) path.act)
      [~ this]
    :_  this(synced (~(put by synced) path.act ship.act))
    [ost.bol %peer group-wire [ship.act %group-sync] group-path]~
  ::
      %remove-synced
    =/  group-wire  [(scot %p ship.act) %group path.act]
    :_  this(synced (~(del by synced) path.act))
    [ost.bol %pull group-wire [ship.act %group-sync] ~]~
  ::
  ==
::
++  peer-group
  |=  pax=path
  ^-  (quip move _this)
  ?~  pax
    [[ost.bol %quit ~]~ this]
  ?.  (~(has by synced) pax)
    [[ost.bol %quit ~]~ this]
  =/  grp=(unit group)  (group-scry pax)
  ?^  grp
    :_  this
    [ost.bol %diff [%group-update [%path u.grp pax]]]~
  [[ost.bol %quit ~]~ this]
::
++  diff-group-update
  |=  [wir=wire diff=group-update]
  ^-  (quip move _this)
  ?:  =(src.bol our.bol)
    (handle-local diff)
  (handle-foreign diff)
::
++  handle-local
  |=  diff=group-update
  ^-  (quip move _this)
  ~&  diff
  ?-  -.diff
      %keys
    [~ this]
      %path
    [~ this]
      %bundle
    [~ this]
  ::
      %add
    :_  this
    %+  turn  (prey:pubsub:userlib [%group pax.diff] bol)
    |=  [=bone *]
    ^-  move
    [bone %diff [%group-update diff]]
  ::
      %remove
    ~&  diff
    :_  this
    %+  turn  (prey:pubsub:userlib [%group pax.diff] bol)
    |=  [=bone *]
    ^-  move
    [bone %diff [%group-update diff]]
  ::
      %unbundle
    :_  this(synced (~(del by synced) pax.diff))
    %+  weld
    ^-  (list move)
    %+  turn  (prey:pubsub:userlib [%group pax.diff] bol)
    |=  [=bone *]
    ^-  move
    [bone %diff [%group-update diff]]
    ^-  (list move)
    %+  turn  (prey:pubsub:userlib [%group pax.diff] bol)
    |=  [=bone *]
    ^-  move
    [bone %quit ~]
  ::
  ==
::
++  handle-foreign
  |=  diff=group-update
  ^-  (quip move _this)
  ?-  -.diff
      %keys
    [~ this]
      %bundle
    [~ this]
  ::
      %path
    ?~  pax.diff
      [~ this]
    =/  ship  (~(get by synced) pax.diff)
    ?~  ship
      [~ this]
    ?.  =(src.bol u.ship)
      [~ this]
    :_  this
    :~  (group-poke pax.diff [%unbundle pax.diff])
        (group-poke pax.diff [%bundle pax.diff])
        (group-poke pax.diff [%add members.diff pax.diff])
    ==
  ::
      %add
    ?~  pax.diff
      [~ this]
    =/  ship  (~(get by synced) pax.diff)
    ?~  ship
      [~ this]
    ?.  =(src.bol u.ship)
      [~ this]
    :_  this
    :~  (group-poke pax.diff diff)
    ==
  ::
      %remove
    ?~  pax.diff
      [~ this]
    =/  ship  (~(get by synced) pax.diff)
    ?~  ship
      [~ this]
    ?.  =(src.bol u.ship)
      [~ this]
    :_  this
    :~  (group-poke pax.diff diff)
    ==
  ::
      %unbundle
    ?~  pax.diff
      [~ this]
    =/  ship  (~(get by synced) pax.diff)
    ?~  ship
      [~ this]
    ?.  =(src.bol u.ship)
      [~ this]
    :_  this(synced (~(del by synced) pax.diff))
    :~  (group-poke pax.diff diff)
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
  ?:  (~(has by synced) wir)
    ::  resubscribe
    [~ this]
  ::  no-op
  [~ this]
::
++  group-poke
  |=  [pax=path action=group-action]
  ^-  move
  [ost.bol %poke pax [our.bol %groups] [%group-action action]]
::
++  group-scry
  |=  pax=path
  ^-  (unit group)
  =.  pax  ;:  weld
    `path`/=groups/(scot %da now.bol)
    pax
    `path`/noun
  ==
  .^((unit group) %gx pax)
::
--

