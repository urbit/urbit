/-  *group, *metadata-store, hook=group-hook
/+  store=group-store, resource
::
|_  =bowl:gall
+$  card  card:agent:gall
++  scry-for
  |*  [=mold =path]
  .^  mold
    %gx
    (scot %p our.bowl)
    %group-store
    (scot %da now.bowl)
    (snoc `^path`path %noun)
  ==
++  scry-tag
  |=  [rid=resource =tag]
  ^-  (unit (set ship))
  =/  group
    (scry-group rid)
  ?~  group
    ~
  `(~(gut by tags.u.group) tag ~)
::
++  scry-group-path
  |=  =path
  %+  scry-for
    (unit group)
  [%groups path]
::
++  scry-group
  |=  rid=resource
  %-  scry-group-path
  (en-path:resource rid)
::
++  members
  |=  rid=resource
  %-  members-from-path
  (en-path:resource rid)
::
++  members-from-path
  |=  =group-path
  ^-  (set ship)
  =-  members:(fall - *group)
  (scry-group-path group-path)
::
++  is-member
  |=  [=ship =group-path]
  ^-  ?
  =-  (~(has in -) ship)
  (members-from-path group-path)
::  +role-for-ship: get role for user
::
::    Returns ~ if no such group exists or user is not
::    a member of the group. Returns [~ ~] if the user
::    is a member with no additional role.
++  role-for-ship
  |=  [rid=resource =ship]
  ^-  (unit (unit role-tag))
  =/  grp=(unit group)
    (scry-group rid)
  ?~  grp  ~
  =*  group   u.grp
  =*  policy  policy.group
  =*  tags    tags.group
  =/  admins=(set ^ship)
    (~(gut by tags) %admin ~)
  ?:  (~(has in admins) ship)
    ``%admin
  =/  mods
    (~(gut by tags) %moderator ~)
  ?:  (~(has in mods) ship)
    ``%moderator
  =/  janitors
    (~(gut by tags) %janitor ~)
  ?:  (~(has in janitors) ship)
    ``%janitor
  ?:  (~(has in members.group) ship)
    [~ ~]
  ~
++  can-join-from-path
  |=  [=path =ship]
  %+  scry-for
    ?
  %+  welp
    [%groups path]
  /join/[(scot %p ship)]
::
++  can-join
  |=  [rid=resource =ship]
  %+  can-join-from-path
    (en-path:resource rid)
  ship
::
++  is-managed-path
  |=  =path
  ^-  ?
  =/  group=(unit group)
    (scry-group-path path)
  ?~  group  %.n
  !hidden.u.group
::
++  is-managed
  |=  rid=resource
  %-  is-managed-path
  (en-path:resource rid)
::
--
