/-  *group, store=group-store, *resource
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
  $%  [%add rid=resource]
      [%remove rid=resource]
  ==
--
