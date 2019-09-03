::    hooks/group-permits
::  keeps a permissions (set ship) up to date with a group
::
/-  *groups, *permissions
::
|%
+$  action
  $:  permission=path
::
  $=  what
  $%  [%create kind=?(%black %white) group=path]
      [%delete ~]
  ==  ==
::
+$  state
  $:  group-permits=(map permission=path group=path)
      permit-groups=(map group=path permissions=(set path))
  ==
::
+$  allowed-action
  $%  $>(%add action:permissions)
      $>(%remove action:permissions)
  ==
::
+$  move  [bone card]
+$  card
  $%  [%peer wire [ship %groups] path]
      [%pull wire [ship %groups] ~]
      [%poke wire [ship %permissions] %permissions-action allowed-action]
  ==
::
|_  [=bowl:gall state]
++  this  .
::
::  state manipulation
::
++  add-group-permit
  |=  [permission=path group=path]
  %_  this
    group-permits  (~(put by group-permits) permission group)
    permit-groups  (~(put ju permit-groups) group permission)
  ==
::
++  remove-group-permit
  |=  permission=path
  =+  gop=(~(get by group-permits) permission)
  %_  this
    group-permits  (~(del by group-permits) permission)
    permit-groups  ?~  gop  permit-groups
                   (~(del ju group-permits) u.gop)
  ==
::
::  move creation
::
++  start-watching-group
  |=  [permission=path group=path]
  ^-  move
  [ost.bowl %peer permission [our.bowl %groups] group]
::
++  update-permissions
  |=  [=path act=allowed-action]
  [ost.bowl %poke path [our.bowl %permissions] %permissions-action path act]
::
++  diff-groups-group-update
  |=  [=wire =group-update
  ^-  (quip move _this)
  :_  this
  ?+  -.group-update  ~
      %add
    members pax
  ::
      %remove
    ..
  ==
--