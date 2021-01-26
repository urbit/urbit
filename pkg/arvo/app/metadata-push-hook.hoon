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
++  on-poke   
  |=  [=mark =vase]
  ?.  ?=(%metadata-hook-update mark)
    (on-poke:def mark vase)
  =+  !<(upd=metadata-hook-update vase)
  ?.  ?=(%req-preview -.upd)
    (on-poke:def mark vase)
  =*  rid  group.upd
  |^
  ?>  =(entity.rid our.bowl)
  ?>  (can-join:grp rid src.bowl)
  =/  members
    ~(wyt in (members:grp rid))
  =/  =metadata
    (need (peek-metadata:met %contacts rid))
  :_  this
  =;  =cage
    [%pass / %agent [src.bowl %metadata-pull-hook] %poke cage]~
  :-  %metadata-hook-update
  !>  ^-  metadata-hook-update
  [%preview rid channels members channel-count metadata]
  ::
  ++  channels
    %-  ~(gas by *associations)
    %+  skim  ~(tap by (app-metadata-for-group:met rid %graph))
    |=([=md-resource group=resource =metadata] preview.metadata)
  ::   
  ++  channel-count
    ~(wyt by (app-metadata-for-group:met rid %graph))
  --

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
  ?.  ?=(?(%add %remove) -.upd)
    %.n
  =/  role=(unit (unit role-tag))
    (role-for-ship:grp group.upd src.bowl)
  =/  =metadata
    (need (peek-metadata:met %contacts group.upd))
  ?~  role  %.n
  ?^  u.role  
    ?=(?(%admin %moderator) u.u.role)
  ?.  ?=(%add -.upd)  %.n
  ?&  =(src.bowl entity.resource.resource.upd)
      ?=(%member-metadata vip.metadata)
  ==
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
  ?>  ?=(^ group)
  ?>  (~(has in members.u.group) src.bowl)
  !>  ^-  metadata-update
  [%initial-group rid associations]
::
--
