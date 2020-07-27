::  permission-group-hook: groups into permissions
::
::    mirror the ships in specified groups to specified permission paths
::
/-  *group-store, *permission-group-hook
/+  *permission-json, default-agent, verb, dbug
::
|%
+$  state
  $%  [%0 state-0]
  ==
::
+$  group-path  path
::
+$  permission-path  path
::
+$  state-0
  $:  relation=(map group-path (set permission-path))
  ==
::
+$  card  card:agent:gall
--
::
=|  state-0
=*  state  -
::
%+  verb  |
%-  agent:dbug
^-  agent:gall
|_  =bowl:gall
+*  this  .
    do    ~(. +> bowl)
    def   ~(. (default-agent this %|) bowl)
::
++  on-init  on-init:def
++  on-save  !>(state)
++  on-load
  |=  old=vase
  ^-  (quip card _this)
  [~ this(state !<(state-0 old))]
::
++  on-poke  on-poke:def
++  on-agent  on-agent:def
++  on-peek   on-peek:def
++  on-watch  on-watch:def
++  on-leave  on-leave:def
++  on-arvo   on-arvo:def
++  on-fail   on-fail:def
--
