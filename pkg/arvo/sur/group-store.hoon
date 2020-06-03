/-  *group
|%
::
++  state-zero
  |%
  +$  group  (set ship)
  ::
  +$  group-action
    $%  [%add members=group pax=path]                :: add member to group
        [%remove members=group pax=path]             :: remove member from group
        [%bundle pax=path]                           :: create group at path
        [%unbundle pax=path]                         :: delete group at path
    ==
  ::
  +$  group-update
    $%  [%keys keys=(set path)]                      :: keys have changed
        [%path members=group pax=path]
        group-action
    ==
::
  +$  groups  (map path group)
  --
::  $action: request to change group-store state
::
::    %add-group: add a group
::    %add-members: add members to a group
::    %remove-members: remove members from a group
::    %add-tag: add a tag to a set of ships
::    %remove-tag: remove a tag from a set of ships
::    %change-policy: change a group's policy
::    %remove-group: remove a group from the store
::    %groupify: unset .hidden flag
::
+$  action
  $%  [%add-group =group-id =policy hidden=?]
      [%add-members =group-id ships=(set ship)]
      [%remove-members =group-id ships=(set ship)]
      [%add-tag =group-id =tag ships=(set ship)]
      [%remove-tag =group-id =tag ships=(set ship)]
      [%change-policy =group-id =diff:policy]
      [%remove-group =group-id ~]
      [%groupify =group-id ~]
  ==
::  $update: a description of a processed state change
::
::    %initial: describe groups upon new subscription
::
+$  update
  $%  initial
      action
  ==
+$  initial
  $%  [%initial-group =group-id =group]
      [%initial =groups]
  ==
--

