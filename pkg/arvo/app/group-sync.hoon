::  hook/sync-group.hoon
/-  *groups, *group-sync
|%
+$  move  [bone card]
::
+$  card
  $%  [%diff [%noun group-diff]]
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
++  poke-group-sync-action
  |=  act=group-sync-action
  ^-  (quip move _this)
  ?-  -.act
      %add-owned
    :_  this(owned (~(put in owned) path.act))
    [ost.bol %peer path.act [our.bol %groups] path.act]~
  ::
      %remove-owned
    ~&  path.act
    :_  this(owned (~(del in owned) path.act))
    :-  [ost.bol %pull path.act [our.bol %groups] ~]
    ^-  (list move)
    %+  turn  (prey:pubsub:userlib path.act bol)
    |=  [=bone *]
    ^-  move
    [bone %quit ~]
  ::
      %add-synced
    :_  this(synced (~(put in synced) [ship.act path.act]))
    [ost.bol %peer path.act [ship.act %group-sync] path.act]~
  ::
      %remove-synced
    :_  this(synced (~(put in synced) [ship.act path.act]))
    [ost.bol %pull path.act [ship.act %group-sync] ~]~
  ::
  ==
::
++  peer-group
  |=  pax=path
  ^-  (quip move _this)
  ?~  pax
    [[ost.bol %quit ~]~ this]
  =/  grp=(unit group)  (group-scry pax)
  ?^  grp
    :_  this
    [ost.bol %diff [%noun [%path u.grp pax]]]~
  [[ost.bol %quit ~]~ this]
::
++  diff-noun
  |=  diff=group-diff
  ^-  (quip move _this)
  ?:  =(src.bol our.bol)
    (handle-local diff)
  (handle-foreign diff)
::
++  handle-local
  |=  diff=group-diff
  ^-  (quip move _this)
  ~&  diff
  :_  this
  ?-  -.diff
      %keys
    ~
  ::
      %path
    :~  (group-poke pax.diff [%unbundle pax.diff])
        (group-poke pax.diff [%bundle pax.diff])
        (group-poke pax.diff [%bundle pax.diff])
        (group-poke pax.diff [%add members.diff pax.diff])
    ==
  ::
      %add
    =/  act=group-action  diff
    :~  (group-poke pax.diff act)
    ==
  ::
      %remove
    =/  act=group-action  diff
    :~  (group-poke pax.diff act)
    ==
  ::
      %bundle
    =/  act=group-action  diff
    :~  (group-poke pax.diff act)
    ==
  ::
      %unbundle
    =/  act=group-action  diff
    :~  (group-poke pax.diff act)
    ==
  ::
  ==
::
++  handle-foreign
  |=  diff=group-diff
  ^-  (quip move _this)
  ~&  diff
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
  .^((unit group) %gx /=group=/[pax]/noun)
::
--

