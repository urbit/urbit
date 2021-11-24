::  group-hook [landscape]:
::
::  allow syncing group data from foreign paths to local paths
::
/-  *group, *invite-store, *resource
/+  default-agent, verb, dbug, store=group-store, grpl=group, pull-hook
~%  %group-hook-top  ..part  ~
|%
+$  card  card:agent:gall
::
++  config
  ^-  config:pull-hook
  :*  %group-store
      update:store
      %group-update
      %group-push-hook
      0  0
      %.n
  ==
::
--
::
::
%-  agent:dbug
%+  verb  |
^-  agent:gall
%-  (agent:pull-hook config)
^-  (pull-hook:pull-hook config)
|_  =bowl:gall
+*  this        .
    def         ~(. (default-agent this %|) bowl)
    dep         ~(. (default:pull-hook this config) bowl)
    grp         ~(. grpl bowl)
::
++  on-init  on-init:def
++  on-save  !>(~)
++  on-load  on-load:def
++  on-poke  on-poke:def
++  on-agent  on-agent:def
++  on-watch  on-watch:def
++  on-leave  on-leave:def
++  on-peek   on-peek:def
++  on-arvo   on-arvo:def
++  on-fail   on-fail:def
++  on-pull-nack
  |=   [=resource =tang]
  ^-  (quip card _this)
  %-  (slog tang)
  :_  this
  =-  [%pass / %agent [our.bowl %group-store] %poke -]~
  group-update-0+!>([%remove-group resource ~])
::
++  on-pull-kick
  |=  =resource
  ^-  (unit path)
  `/
::
++  resource-for-update  resource-for-update:grp
--
