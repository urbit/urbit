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

