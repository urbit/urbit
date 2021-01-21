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
+$  state-zero
  [%0 previews=(map resource group-preview)]
::
--
::
::
%-  agent:dbug
%+  verb  |
^-  agent:gall
%-  (agent:pull-hook config)
^-  (pull-hook:pull-hook config)
=|  state-zero
=*  state  -
|_  =bowl:gall
+*  this        .
    def         ~(. (default-agent this %|) bowl)
    dep         ~(. (default:pull-hook this config) bowl)
    met         ~(. mdl bowl)
::
++  on-init  on-init:def
++  on-save  !>(state)
++  on-load  
  |=  =vase
  =+  !<(old=state-zero vase)
  `this(state old)
::
++  on-poke  on-poke:def
++  on-agent  
  |=  [=wire =sign:agent:gall]
  ?.  ?=([%preview @ @ @ ~] wire)
    (on-agent:def wire sign)
  =/  rid=resource
    (de-path:resource t.wire)
  ?+  -.sign  `this 
      %fact  
    ?>  =(%metadata-update p.cage.sign)
    =+  !<(upd=metadata-update q.cage.sign)
    ?>  ?=(%preview -.upd)
    :_  this(previews (~(put by previews) rid +.upd))
    :~  [%give %fact ~[wire] cage.sign]
        [%give %kick ~[wire] ~]
    ==
  ::
      %watch-ack
    :_  this
    ?~  p.sign  ~
    :~  [%give %fact ~[wire] tang+!>(u.p.sign)]
        [%give %kick ~[wire] ~]
    ==
  ==
::
++  on-watch  
  |=  =path
  ?>  (team:title [our src]:bowl)
  ?.  ?=([%preview @ @ @ ~] path)
    (on-watch:def path)
  =/  rid=resource
    (de-path:resource t.path)
  =/  prev=(unit group-preview)
    (~(get by previews) rid)
  :_  this
  ?^  prev
    :~  [%give %fact ~ metadata-update+!>([%preview u.prev])]
        [%give %kick ~ ~]
    ==
  =/  =dock
    [entity.rid %metadata-push-hook]
  [%pass path %agent dock %watch path]~
::
++  on-leave  on-leave:def
++  on-peek   on-peek:def
++  on-arvo   on-arvo:def
::
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
