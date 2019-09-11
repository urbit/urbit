::  hook/group-permits.hoon
/-  *groups, *permissions, *group-permit
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
  ?~  old
    [~ this]
  [~ this(+<+ u.old)]
::
++  poke-noun
  |=  act=*
  ^-  (quip move _this)
  ~&  relation
  [~ this]
::
++  poke-group-permit-action
  |=  act=group-permit-action
  ^-  (quip move _this)
  ?.  =(src.bol our.bol)
    [~ this]
  ?-  -.act
      %associate
    (handle-associate group.act permissions.act)
  ::
      %dissociate
    (handle-dissociate group.act permissions.act)
  ::
  ==
::
++  handle-associate
  |=  [group=path permissions=(set path)]
  ^-  (quip move _this)
  ::  TODO: remove the requirement for permission paths to exist
  =/  perm-list  ~(tap in permissions)
  =/  perm-length  (lent perm-list)
  =/  checked-paths
    %+  skim  perm-list
    |=  pax=path
    ^-  ?
    ?~  (permission-scry pax)
      %.n
    %.y
  ?.  =((lent checked-paths) perm-length)
    ~&  checked+checked-paths
    ~&  perm+perm-length
    [~ this]
  =/  perms  (~(get by relation) group)
  ::  if relation does not exist, create it and subscribe.
  ?~  perms
    ~&  group+group
    =/  group-path  [%group group]
    :_  this(relation (~(put by relation) group permissions))
    [ost.bol %peer group-path [our.bol %groups] group-path]~
  =.  u.perms  (~(uni in u.perms) permissions)
  :-  ~
  this(relation (~(put by relation) group u.perms))
::
++  handle-dissociate
  |=  [group=path permissions=(set path)]
  ^-  (quip move _this)
  =/  perms  (~(get by relation) group)
  ?~  perms
    [~ this]
  =.  permissions  (~(del in u.perms) permissions)
  ?~  permissions
    :_  this(relation (~(del by relation) group))
    :~  (group-pull [%group group])
    ==
  :-  ~
  this(relation (~(put by relation) group permissions))
::
++  diff-group-update
  |=  [wir=wire diff=group-update]
  ^-  (quip move _this)
  ~&  diff
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
    ^-  move
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
    ^-  move
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
    ^-  move
    (permission-poke path [%remove path members.diff])
  ::
      %unbundle
    ::  pull subscriptions
    =/  perms  (~(get by relation) pax.diff)
    ?~  perms
      :_  this(relation (~(del by relation) pax.diff))
      :~  (group-pull [%group pax.diff])
      ==
    :_  this(relation (~(del by relation) pax.diff))
    :-  (group-pull [%group pax.diff])
    %+  turn  ~(tap in u.perms)
    |=  =path
    ^-  move
    (permission-poke path [%delete path])
  ::
  ==
::
++  permission-poke
  |=  [pax=path action=permission-action]
  ^-  move
  [ost.bol %poke pax [our.bol %permissions] [%permission-action action]]
::
++  group-pull
  |=  =path
  ^-  move
  [ost.bol %pull [%group path] [our.bol %groups] ~]
::
++  permission-scry
  |=  pax=path
  ^-  (unit permission)
  =.  pax  ;:  weld
    `path`/=permissions/(scot %da now.bol)/permission
    pax
    `path`/noun
  ==
  .^((unit permission) %gx pax)
::
--

