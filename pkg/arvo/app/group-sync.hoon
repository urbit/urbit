::  hook/sync-group.hoon
/-  *groups, *group-sync
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
  $:  synced=(set [ship path])
      owned=(set path)
  ==
::
+$  poke
  $%  [%group-action group-action]
      [%group-sync-action group-sync-action]
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
  |=  cmd=cord
  ^-  (quip move _this)
  ~&  owned+owned
  ~&  synced+synced
  [~ this]
::
++  poke-group-sync-action
  |=  act=group-sync-action
  ^-  (quip move _this)
  ?-  -.act
      %add-owned
    ?:  (~(has in owned) path.act)
      [~ this]
    :_  this(owned (~(put in owned) path.act))
    [ost.bol %peer [%group path.act] [our.bol %groups] [%group path.act]]~
  ::
      %remove-owned
    :_  this(owned (~(del in owned) path.act))
    :-  [ost.bol %pull [%group path.act] [our.bol %groups] ~]
    ^-  (list move)
    %+  turn  (prey:pubsub:userlib [%group path.act] bol)
    |=  [=bone *]
    ^-  move
    [bone %quit ~]
  ::
      %add-synced
    ?:  (~(has in synced) [ship.act path.act])
      [~ this]
    :_  this(synced (~(put in synced) [ship.act path.act]))
    [ost.bol %peer [%group path.act] [ship.act %group-sync] [%group path.act]]~
  ::
      %remove-synced
    :_  this(synced (~(del in synced) [ship.act path.act]))
    [ost.bol %pull [%group path.act] [ship.act %group-sync] ~]~
  ::
  ==
::
++  peer-group
  |=  pax=path
  ^-  (quip move _this)
  ~&  pax
  ?~  pax
    [[ost.bol %quit ~]~ this]
  ?.  (~(has in owned) pax)
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
    :_  this(owned (~(del in owned) pax.diff))
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
  :_  this
  ?-  -.diff
      %keys
    ~
  ::
      %path
    ?.  (~(has in synced) [src.bol pax.diff])
      ~
    :~  (group-poke pax.diff [%unbundle pax.diff])
        (group-poke pax.diff [%bundle pax.diff])
        (group-poke pax.diff [%add members.diff pax.diff])
    ==
  ::
      %add
    ?.  (~(has in synced) [src.bol pax.diff])
      ~
    =/  act=group-action  diff
    :~  (group-poke pax.diff act)
    ==
  ::
      %remove
    ?.  (~(has in synced) [src.bol pax.diff])
      ~
    =/  act=group-action  diff
    :~  (group-poke pax.diff act)
    ==
  ::
      %bundle
    ?.  (~(has in synced) [src.bol pax.diff])
      ~
    =/  act=group-action  diff
    :~  (group-poke pax.diff act)
    ==
  ::
      %unbundle
    ?.  (~(has in synced) [src.bol pax.diff])
      ~
    =/  act=group-action  diff
    :~  (group-poke pax.diff act)
    ==
  ::
  ==
::
++  quit
  |=  wir=wire
  ^-  (quip move _this)
  ~&  %quit-happened
  ~&  wir
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

