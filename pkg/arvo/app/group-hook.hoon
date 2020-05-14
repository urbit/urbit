::  group-hook: allow syncing group data from foreign paths to local paths
::
/-  *group, store=group-store, hook=group-hook
/+  default-agent, verb, dbug
~%  %group-hook-top  ..is  ~
|%
+$  card  card:agent:gall
::
++  versioned-state
  $%  state-zero
      state-one
  ==
::
::
+$  state-zero
  $:  %0
      synced=(map path ship)
  ==
::
+$  state-one
  $:  %1
      synced=(map group-id ship)
  ==
::
--
::
=|  state-one
=*  state  -
::
%-  agent:dbug
%+  verb  |
^-  agent:gall
|_  =bowl:gall
+*  this        .
    group-core  +>
    gc          ~(. group-core bowl)
    def         ~(. (default-agent this %|) bowl)
::
++  on-init  on-init:def
++  on-save  !>(state)
++  on-load
  |=  =vase
  ^-  (quip card _this)
  =/  old  !<(state-one vase)
  :_  this(state old)
  %+  murn  ~(tap by synced.old)
  |=  [=path =ship]
  ^-  (unit card)
  =/  =wire  [(scot %p ship) %group path]
  =/  =term  ?:(=(our.bowl ship) %group-store %group-hook)
  ?:  (~(has by wex.bowl) [wire ship term])  ~
  `[%pass wire %agent [ship term] %watch [%group path]]
::
++  on-leave  on-leave:def
++  on-peek   on-peek:def
++  on-arvo   on-arvo:def
++  on-fail   on-fail:def
::
++  on-poke  on-poke:def
++  on-watch  on-watch:def
++  on-agent  on-agent:def
>>>>>>> 457bd4c3a... groups: begin rewrite
--
