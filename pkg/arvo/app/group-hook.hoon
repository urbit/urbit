::  group-hook: allow syncing group data from foreign paths to local paths
::
/-  *group-store, *group-hook
|%
+$  move  [bone card]
::
+$  card
  $%  [%diff [%group-update group-update]]
      [%quit ~]
      [%poke wire dock [%group-action group-action]]
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
++  poke-group-hook-action
  |=  act=group-hook-action
  ^-  (quip move _this)
  ?-  -.act
      %add
    ?.  (team:title our.bol src.bol)
      [~ this]
    =/  group-path  [%group path.act]
    =/  group-wire  [(scot %p ship.act) group-path]
    ?:  (~(has by synced) path.act)
      [~ this]
    =.  synced  (~(put by synced) path.act ship.act)
    :_  (track-bone group-wire)
    ?:  =(ship.act our.bol)
      [ost.bol %peer group-wire [ship.act %group-store] group-path]~
    [ost.bol %peer group-wire [ship.act %group-hook] group-path]~
  ::
      %remove
    =/  ship  (~(get by synced) path.act)
    ?~  ship
      [~ this]
    ?:  &(=(u.ship our.bol) (team:title our.bol src.bol))
      ::  delete one of our own paths
      =/  group-wire  [(scot %p our.bol) %group path.act]
      :_  this(synced (~(del by synced) path.act))
      %+  weld
        (pull-wire group-wire path.act)
      ^-  (list move)
      %+  turn  (prey:pubsub:userlib [%group path.act] bol)
      |=  [=bone *]
      ^-  move
      [bone %quit ~]
    ?:  |(=(u.ship src.bol) (team:title our.bol src.bol))
      ::  delete a foreign ship's path
      =/  group-wire  [(scot %p u.ship) %group path.act]
      :_  this(synced (~(del by synced) path.act))
      (pull-wire group-wire path.act)
    :: don't allow
    [~ this]
  ::
  ==
::
++  peer-group
  |=  pax=path
  ^-  (quip move _this)
  ?~  pax  !!
  ?.  (~(has by synced) pax)  !!
  =/  grp=(unit group)  (group-scry pax)
  ?~  grp  !!
  :_  this
  [ost.bol %diff [%group-update [%path u.grp pax]]]~
::
++  diff-group-update
  |=  [wir=wire diff=group-update]
  ^-  (quip move _this)
  ?:  (team:title our.bol src.bol)
    (handle-local diff)
  (handle-foreign diff)
::
++  handle-local
  |=  diff=group-update
  ^-  (quip move _this)
  ?-  -.diff
      %keys    [~ this]
      %path    [~ this]
      %bundle  [~ this]
      %add
    :_  this
    %+  turn  (prey:pubsub:userlib [%group pax.diff] bol)
    |=  [=bone *]
    ^-  move
    [bone %diff [%group-update diff]]
  ::
      %remove
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
      %keys    [~ this]
      %bundle  [~ this]
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
  =^  =ship  wir
    ?>  ?=([* ^] wir)
    [(slav %p i.wir) t.t.wir]
  ?.  (~(has by synced) wir)
    ::  no-op
    [~ this]
  =/  group-path  [%group wir]
  =/  group-wire  [(scot %p ship) group-path]
  :_  (track-bone group-wire)
  [ost.bol %peer group-wire [ship %group-hook] group-path]~
::
++  reap
  |=  [wir=wire saw=(unit tang)]
  ^-  (quip move _this)
  ?~  saw
    [~ this]
  =^  =ship  wir
    ?>  ?=([* ^] wir)
    [(slav %p i.wir) t.t.wir]
  ~&  %insufficient-permissions-for-group
  [((slog u.saw) ~) this(synced (~(del by synced) wir))]
::
++  group-poke
  |=  [pax=path action=group-action]
  ^-  move
  [ost.bol %poke pax [our.bol %group-store] [%group-action action]]
::
++  group-scry
  |=  pax=path
  ^-  (unit group)
  .^((unit group) %gx ;:(weld /=group-store/(scot %da now.bol) pax /noun))
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
    [ost %pull wir [our.bol %group-store] ~]
  [ost %pull wir [u.shp %group-hook] ~]
::
--

