/-  *group, *metadata-store
/+  store=group-store
|_  =bowl:gall
++  scry-for
  |*  [=mold =path]
  .^  mold
    %gx
    (scot %p our.bowl)
    %group-store
    (scot %da now.bowl)
    (snoc `^path`path %noun)
  ==
::
++  scry-group-path
  |=  =path
  %+  scry-for
    (unit group)
  [%groups path]
::
++  scry-group
  |=  =group-id
  %-  scry-group-path
  (group-id:en-path:store group-id)
::
++  members
  |=  =group-id
  %-  members-from-path
  (group-id:en-path:store group-id)
::
++  members-from-path
  |=  =group-path
  ^-  (set ship)
  =-  members:(fall - *group)
  (scry-group-path group-path)
::
++  is-permitted
  |=  [=ship =group-path]
  ^-  ?
  =-  (~(has in -) ship)
  (members-from-path group-path)
::
++  role-for-ship
  |=  [=group-id =ship]
  ^-  (unit role-tag)
  =/  grp=(unit group)
    (scry-group group-id)
  ?~  grp  ~
  =*  group   u.grp
  =*  policy  policy.group
  =*  tag-queries  tag-queries.group
  =/  admins=(set ^ship)
    (~(gut by tag-queries) %admin ~)
  ?:  (~(has in admins) ship)
    `%admin
  =/  mods
    (~(gut by tag-queries) %moderator ~)
  ?:  (~(has in mods) ship)
    `%moderator
  =/  janitors
    (~(gut by tag-queries) %janitor ~)
  ?:  (~(has in janitors) ship)
    `%janitor
  ?:  (~(has in members.group) ship)
    `%member
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
  |=  [=group-id =ship]
  =-  ~&  -  -
  %+  can-join-from-path
    (group-id:en-path:store group-id)
  ship
++  is-managed-path
  |=  =path
  =/  contact
    .^  (unit *)
      %gx
      (scot %p our.bowl)
      %contact-store
      (scot %da now.bowl)
      (snoc `^path`[%contacts path] %noun)
    ==
  ?~  contact
    %.n
  %.y
++  is-managed
  |=  =group-id
  %-  is-managed-path
  (group-id:en-path:store group-id)

--
