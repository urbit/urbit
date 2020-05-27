/-  *group
|%
::  $action: request to change group-store state
::
::    %add-group: add a group
::    %add-members: add members to a group
::    %remove-members: remove members from a group
::    %add-tag: add a tag to a set of ships, creating the tag if it doesn't exist
::    %remove-tag:
::      remove a tag from a set of ships. If the set is empty remove the tag
::      from the group.
::    %change-policy: change a group's policy
::    %remove-group: remove a group from the store
::    %groupify: unset .hidden flag
::
+$  action
  $%  [%add-group =group-id =policy hidden=?]
      [%add-members =group-id ships=(set ship) tags=(set tag)]
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

