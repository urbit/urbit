::  metadata-pull-hook [landscape]:
::
::  allow syncing group data from foreign paths to local paths
::
/-  *group, *invite-store, *metadata-store
/+  default-agent, verb, dbug, store=group-store, grpl=group, pull-hook
/+  resource, mdl=metadata
~%  %group-hook-top  ..part  ~
|%
+$  card  card:agent:gall
::
++  config
  ^-  config:pull-hook
  :*  %metadata-store
      metadata-update
      %metadata-update
      %metadata-push-hook
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
    met         ~(. mdl bowl)
::
++  on-init  on-init:def
++  on-save  !>(~)
++  on-load  on-load:def
++  on-poke  on-poke:def
++  on-agent  
  |=  [=wire =sign:agent:gall]
  ?.  ?=([%preview @ @ @ ~] wire)
    (on-agent:def wire sign)
  :_  this
  ?+  -.sign  ~ 
      %fact  
    :~  [%give %fact ~[wire] cage.sign]
        [%give %kick ~[wire] ~]
    ==
  ::
      %watch-ack
    ?^(p.sign [%give %kick ~[wire] ~]~ ~)
  ==
::
++  on-watch  
  |=  =path
  ?.  ?=([%preview @ @ @ ~] path)
    (on-watch:def path)
  =/  rid=resource
    (de-path:resource t.path)
  :_  this
  =/  =dock
    [entity.rid %metadata-push-hook]
  [%pass path %agent dock %watch path]~
::
++  on-leave  on-leave:def
++  on-peek   on-peek:def
++  on-arvo   on-arvo:def
++  on-fail   on-fail:def
++  on-pull-nack
  |=   [=resource =tang]
  ^-  (quip card _this)
  =/  =associations
    (metadata-for-group:met resource)
  :_  this
  %+  turn  ~(tap by associations)
  |=  [=md-resource =association]
  =-  [%pass / %agent [our.bowl %metadata-store] %poke -]
  :-  %metadata-update
  !>  ^-  metadata-update
  [%remove resource md-resource]
::
++  on-pull-kick
  |=  =resource
  ^-  (unit path)
  `/
--
