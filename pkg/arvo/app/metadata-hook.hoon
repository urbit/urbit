::  metadata-hook [landscape]:
::
::  allow syncing foreign metadata
::
::  watch paths:
::  /group/%group-path                      all updates related to this group
::
/-  *metadata-store, *metadata-hook
/+  default-agent, dbug, verb, grpl=group, *migrate
~%  %metadata-hook-top  ..part  ~
|%
+$  card  card:agent:gall
+$  versioned-state
  $%  state-zero
      state-one
  ==
::
+$  state-zero
  $:  %0
      synced=(map group-path ship)
  ==
+$   state-one
  $:  %1
      synced=(map group-path ship)
  ==
--
=|  state-one
=*  state  -
%-  agent:dbug
%+  verb  |
^-  agent:gall
(default-agent *agent:gall %|)
