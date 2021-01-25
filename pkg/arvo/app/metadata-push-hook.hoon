::  metadata-push-hook [landscape]:
::
/-  *group, *invite-store, *metadata-store
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
      metadata-update
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
++  on-poke   on-poke:def
++  on-agent  on-agent:def
++  on-watch  on-watch:def
++  on-leave  on-leave:def
++  on-peek   on-peek:def
++  on-arvo   on-arvo:def
++  on-fail   on-fail:def
::
++  should-proxy-update
  |=  =vase
  =+  !<(upd=metadata-update vase)
  ?.  ?=(?(%add %remove %update) -.upd)
    %.n
  =/  role=(unit (unit role-tag))
    (role-for-ship:grp group.upd src.bowl)
  ?~  role  %.n
  ?~  u.role  %.n
  ?=(?(%admin %moderator) u.u.role)
::
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
  =/  =associations
    (metadata-for-group:met rid)
  ?>  =(entity.rid our.bowl)
  ?>  ?=(^ group)
  !>  ^-  metadata-update
  ?:  (~(has in members.u.group) src.bowl)  
    [%initial-group rid associations]
  |^
  ?>  (can-join:grp rid src.bowl)
  [%preview rid channels members channel-count metadata]
  ::
  ++  members
    ~(wyt in members.u.group)
  ::
  ++  channels
    %-  ~(gas by *^associations)
    %+  skim  ~(tap by (app-metadata-for-group:met rid %graph))
    |=([=md-resource group=resource =^metadata] preview.metadata)
  ::   
  ++  channel-count
    ~(wyt by (app-metadata-for-group:met rid %graph))
  ::
  ++  metadata
    (need (peek-metadata:met %contacts rid))
  --
--
