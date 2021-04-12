::  metadata-push-hook [landscape]:
::
/-  *group, *invite-store, store=metadata-store
/+  default-agent, verb, dbug, grpl=group, push-hook,
    resource, mdl=metadata, gral=graph
~%  %group-hook-top  ..part  ~
|%
+$  card  card:agent:gall
::
++  config
  ^-  config:push-hook
  :*  %metadata-store
      /all
      update:store
      %metadata-update
      %metadata-pull-hook
  ==
::
+$  agent  (push-hook:push-hook config)
--
::
::
%-  agent:dbug
%+  verb  |
^-  agent:gall
%-  (agent:push-hook config)
^-  agent
|_  =bowl:gall
+*  this        .
    def         ~(. (default-agent this %|) bowl)
    grp       ~(. grpl bowl)
    met       ~(. mdl bowl)
    gra       ~(. gral bowl)
::
++  on-init  on-init:def
++  on-save  !>(~)
++  on-load    on-load:def
++  on-poke   
  |=  [=mark =vase]
  ?.  ?=(%metadata-hook-update mark)
    (on-poke:def mark vase)
  =+  !<(=hook-update:store vase)
  ?.  ?=(%req-preview -.hook-update)
    (on-poke:def mark vase)
  ?>  =(entity.group.hook-update our.bowl)
  =/  =group-preview:store
    (get-preview:met group.hook-update)
  :_  this
  =-  [%pass / %agent [src.bowl %metadata-pull-hook] %poke -]~
  metadata-hook-update+!>(`hook-update:store`[%preview group-preview])
::
++  on-agent  on-agent:def
++  on-watch  on-watch:def
++  on-leave  on-leave:def
++  on-peek   on-peek:def
++  on-arvo   on-arvo:def
++  on-fail   on-fail:def
::
++  transform-proxy-update
  |=  vas=vase
  ^-  (unit vase)
  =/  =update:store  !<(update:store vas)
  ?.  ?=(?(%add %remove) -.update)
    ~
  =/  role=(unit (unit role-tag))
    (role-for-ship:grp group.update src.bowl)
  =/  =metadatum:store
    (need (peek-metadatum:met %groups group.update))
  ?~  role  ~
  ?^  u.role  
    ?:  ?=(?(%admin %moderator) u.u.role)
      `vas
    ~
  ?.  ?=(%add -.update)  ~
  ?:  ?&  =(src.bowl entity.resource.resource.update)
          ?=(%member-metadata vip.metadatum)
      ==
    `vas
  ~
::
++  resource-for-update  resource-for-update:met
++  take-update
  |=   =vase
  ^-  [(list card) agent]
  `this
::
++  initial-watch
  |=  [=path rid=resource]
  ^-  vase
  =/  group
    (scry-group:grp rid)
  =/  =associations:store
    (metadata-for-group:met rid)
  ?>  ?=(^ group)
  ?>  (~(has in members.u.group) src.bowl)
  !>  ^-  update:store
  [%initial-group rid associations]
::
--
