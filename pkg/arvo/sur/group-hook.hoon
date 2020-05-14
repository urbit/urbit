/-  *group, store=group-store
|%
::  $action: request to change group-hook state
::
::    %add:
::      if ship is ours make group available to sync, else sync foreign group
::      to group-store.
::    %remove:
::      if ship is ours make unavailable to sync, else stop syncing foreign
::      group.
::
+$  action
  $%  [%add =group-id]
      [%remove =group-id]
  ==
::  $update: description of state change
::
::    %no-perms:
::      Group is unavailable to sync
::
+$  update
  $%  [%no-perms =group-id]
      action
  ==

--
