::  permission-group-hook:
::  mirror the ships in some group to some set of permission paths
::
/-  *group-store, *permission-group-hook
/+  *permission-json
|%
+$  move  [bone card]
::
+$  card
  $%  [%diff [%group-update group-update]]
      [%poke wire dock poke]
      [%pull wire dock ~]
      [%peer wire dock path]
  ==
::
+$  state
  $%  [%0 state-zero]
  ==
::
+$  group-path  path
::
+$  permission-path  path
::
+$  state-zero
  $:  relation=(map group-path (set permission-path))
  ==
::
+$  poke
  $%  [%permission-action permission-action]
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
  [~ ?~(old this this(+<+ u.old))]
::
++  poke-json
  |=  =json
  ^-  (quip move _this)
  ?>  (team:title our.bol src.bol)
  (poke-permission-group-hook-action (json-to-perm-group-hook-action json))
::
++  poke-permission-group-hook-action
  |=  act=permission-group-hook-action
  ^-  (quip move _this)
  ?>  (team:title our.bol src.bol)
  ?-  -.act
      %associate   (handle-associate group.act permissions.act)
      %dissociate  (handle-dissociate group.act permissions.act)
  ==
::
++  handle-associate
  |=  [group=path permission-paths=(set [path kind])]
  ^-  (quip move _this)
  =/  perms  (~(get by relation) group)
  ::  if relation does not exist, create it and subscribe.
  =/  permissions=(set path)
    %-  ~(run in permission-paths)
    |=([=path =kind] path)
  ?~  perms
    =/  group-path  [%group group]
    :_  this(relation (~(put by relation) group permissions))
    [ost.bol %peer group-path [our.bol %group-store] group-path]~
  ::
  =.  u.perms  (~(uni in u.perms) permissions)
  :_  this(relation (~(put by relation) group u.perms))
  %+  weld
    %+  turn  ~(tap in permissions)
    |=(=path (permission-poke path [%delete path]))
  %+  turn  ~(tap in permission-paths)
  |=  [=path =kind]
  =/  pem  *permission
  =.  kind.pem  kind
  (permission-poke path [%create path pem])
::
++  handle-dissociate
  |=  [group=path permissions=(set path)]
  ^-  (quip move _this)
  =/  perms  (~(get by relation) group)
  ?~  perms
    [~ this]
  ::
  =.  permissions  (~(del in u.perms) permissions)
  ?~  permissions
    :_  this(relation (~(del by relation) group))
    [(group-pull [%group group])]~
  [~ this(relation (~(put by relation) group permissions))]
::
++  diff-group-update
  |=  [wir=wire diff=group-update]
  ^-  (quip move _this)
  ?-  -.diff
      %keys
    [~ this]
      %bundle
    [~ this]
      %path
    ::  set all permissions paths
    =/  perms  (~(get by relation) pax.diff)
    ?~  perms
      [~ this]
    :_  this
    %+  turn  ~(tap in u.perms)
    |=  =path
    (permission-poke path [%add path members.diff])
  ::
      %add
    ::  set all permissions paths
    =/  perms  (~(get by relation) pax.diff)
    ?~  perms
      [~ this]
    :_  this
    %+  turn  ~(tap in u.perms)
    |=  =path
    (permission-poke path [%add path members.diff])
  ::
      %remove
    ::  set all permissions paths
    =/  perms  (~(get by relation) pax.diff)
    ?~  perms
      [~ this]
    :_  this
    %+  turn  ~(tap in u.perms)
    |=  =path
    (permission-poke path [%remove path members.diff])
  ::
      %unbundle
    ::  pull subscriptions
    =/  perms  (~(get by relation) pax.diff)
    ?~  perms
      :_  this(relation (~(del by relation) pax.diff))
      [(group-pull [%group pax.diff])]~
    :_  this(relation (~(del by relation) pax.diff))
    :-  (group-pull [%group pax.diff])
    %+  turn  ~(tap in u.perms)
    |=  =path
    (permission-poke path [%delete path])
  ==
::
++  quit
  |=  wir=wire
  ^-  (quip move _this)
  ::  no-op
  [~ this]
::
++  reap
  |=  [wir=wire saw=(unit tang)]
  ^-  (quip move _this)
  ?~  saw
    [~ this]
  =.  wir  ?^(wir t.wir ~)
  ~&  %reap-permission-group-hook
  [((slog u.saw) ~) this(relation (~(del by relation) wir))]
::
++  permission-poke
  |=  [pax=path action=permission-action]
  ^-  move
  [ost.bol %poke pax [our.bol %permission-store] [%permission-action action]]
::
++  group-pull
  |=  =path
  ^-  move
  [ost.bol %pull [%group path] [our.bol %group-store] ~]
::
--
