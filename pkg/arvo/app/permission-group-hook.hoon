::  permission-group-hook: groups into permissions
::
::    mirror the ships in specified groups to specified permission paths
::
/-  *group-store, *permission-group-hook
/+  *permission-json, default-agent, verb, dbug
::
|%
+$  state
  $%  [%0 state-0]
  ==
::
+$  group-path  path
::
+$  permission-path  path
::
+$  state-0
  $:  relation=(map group-path (set permission-path))
  ==
::
+$  card  card:agent:gall
--
::
=|  state-0
=*  state  -
::
%+  verb  |
%-  agent:dbug
^-  agent:gall
=<
  |_  =bowl:gall
  +*  this  .
      do    ~(. +> bowl)
      def   ~(. (default-agent this %|) bowl)
  ::
  ++  on-init  on-init:def
  ++  on-save  !>(state)
  ++  on-load
    |=  old=vase
    ^-  (quip card _this)
    [~ this(state !<(state-0 old))]
  ::
  ++  on-poke
    |=  [=mark =vase]
    ^-  (quip card _this)
    ?+  mark  (on-poke:def mark vase)
        %json
      ::  only accept json from the host team
      ::
      ?>  (team:title our.bowl src.bowl)
      =^  cards  state
        %-  handle-action:do
        %-  json-to-perm-group-hook-action
        !<(json vase)
      [cards this]
    ::
        %permission-group-hook-action
      =^  cards  state
        %-  handle-action:do
        !<(permission-group-hook-action vase)
      [cards this]
    ==
  ::
  ++  on-agent
    |=  [=wire =sign:agent:gall]
    ^-  (quip card _this)
    ?.  ?=([%group *] wire)
      (on-agent:def wire sign)
    ?-  -.sign
      %poke-ack  ~|([dap.bowl %unexpected-poke-ack wire] !!)
    ::
        %kick
      :_  this
      [(watch-group:do t.wire)]~
    ::
        %watch-ack
      ?~  p.sign  [~ this]
      =/  =tank  leaf+"{(trip dap.bowl)} failed subscribe at {(spud wire)}"
      %-  (slog tank u.p.sign)
      [~ this(relation (~(del by relation) t.wire))]
    ::
        %fact
      ?.  ?=(%group-update p.cage.sign)
        (on-agent:def wire sign)
      =^  cards  state
        %-  handle-group-update:do
        !<(group-update q.cage.sign)
      [cards this]
    ==
  ::
  ++  on-peek   on-peek:def
  ++  on-watch  on-watch:def
  ++  on-leave  on-leave:def
  ++  on-arvo   on-arvo:def
  ++  on-fail   on-fail:def
  --
::
|_  =bowl:gall
++  handle-action
  |=  act=permission-group-hook-action
  ^-  (quip card _state)
  ?>  (team:title our.bowl src.bowl)
  ?-  -.act
      %associate   (handle-associate group.act permissions.act)
      %dissociate  (handle-dissociate group.act permissions.act)
  ==
::
++  handle-associate
  |=  [group=group-path associate=(set [permission-path kind])]
  ^-  (quip card _state)
  =/  perms  (~(get by relation) group)
  ::  if relation does not exist, create it and subscribe.
  =/  perm-paths=(set path)
    (~(run in associate) head)
  ?~  perms
    :_  state(relation (~(put by relation) group perm-paths))
    (snoc (recreate-permissions perm-paths associate) (watch-group group))
  ::
  =/  grp  (group-scry group)
  =.  u.perms  (~(uni in u.perms) perm-paths)
  :_  state(relation (~(put by relation) group u.perms))
  %+  weld
    (recreate-permissions perm-paths associate)
  ?~  grp
    ~
  (add-members group u.grp u.perms)
::
++  handle-dissociate
  |=  [group=path remove=(set permission-path)]
  ^-  (quip card _state)
  =/  perms=(set permission-path)
    (fall (~(get by relation) group) *(set permission-path))
  ?:  =(~ perms)
    [~ state]
  ::  remove what we must. if that means we are no longer mirroring this group
  ::  into any permissions, remove it from state entirely.
  ::
  =.  perms  (~(del in perms) remove)
  ?~  perms
    :_  state(relation (~(del by relation) group))
    [(group-pull group)]~
  [~ state(relation (~(put by relation) group perms))]
::
++  handle-group-update
  |=  diff=group-update
  ^-  (quip card _state)
  ?-  -.diff
    %keys     [~ state]
    %bundle   [~ state]
  ::
      %path
    ::  set all permissions paths
    =/  perms  (~(got by relation) pax.diff)
    :_  state
    (add-members pax.diff members.diff perms)
  ::
      %add
    ::  set all permissions paths
    =/  perms  (~(get by relation) pax.diff)
    ?~  perms
      [~ state]
    :_  state
    %+  turn  ~(tap in u.perms)
    |=  =path
    (permission-poke path [%add path members.diff])
  ::
      %remove
    ::  set all permissions paths
    =/  perms  (~(get by relation) pax.diff)
    ?~  perms
      [~ state]
    :_  state
    %+  turn  ~(tap in u.perms)
    |=  =path
    (permission-poke path [%remove path members.diff])
  ::
      %unbundle
    ::  pull subscriptions
    =/  perms  (~(get by relation) pax.diff)
    ?~  perms
      :_  state(relation (~(del by relation) pax.diff))
      [(group-pull pax.diff)]~
    :_  state(relation (~(del by relation) pax.diff))
    :-  (group-pull pax.diff)
    %+  turn  ~(tap in u.perms)
    |=  =path
    (permission-poke path [%delete path])
  ==
::
++  permission-poke
  |=  [=wire action=permission-action]
  ^-  card
  :*  %pass
      [%write wire]
      %agent
      [our.bowl %permission-store]
      %poke
      [%permission-action !>(action)]
  ==
::
++  group-scry
  |=  pax=path
  ^-  (unit group)
  .^((unit group) %gx ;:(weld /=group-store/(scot %da now.bowl) pax /noun))
::
++  add-members
  |=  [pax=path mem=(set ship) perms=(set path)]
  ^-  (list card)
  %+  turn  ~(tap in perms)
  |=  =path
  (permission-poke path [%add path mem])
::
++  recreate-permissions
  |=  [perm-paths=(set path) associate=(set [permission-path kind])]
  ^-  (list card)
  %+  weld
    %+  turn  ~(tap in perm-paths)
    |=  =path
    (permission-poke path [%delete path])
  %+  turn  ~(tap in associate)
  |=  [=path =kind]
  =|  pem=permission
  =.  kind.pem  kind
  (permission-poke path [%create path pem])
::
::
++  watch-group
  |=  =group-path
  ^-  card
  =.  group-path  [%group group-path]
  [%pass group-path %agent [our.bowl %group-store] %watch group-path]
::
++  group-pull
  |=  =group-path
  ^-  card
  [%pass [%group group-path] %agent [our.bowl %group-store] %leave ~]
--
