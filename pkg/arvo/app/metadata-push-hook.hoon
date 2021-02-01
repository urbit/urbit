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
  =*  rid  group.hook-update
  |^
  ?>  =(entity.rid our.bowl)
  ?>  (can-join:grp rid src.bowl)
  =/  members
    ~(wyt in (members:grp rid))
  =/  =metadatum:store
    %-  need
    %+  mate  (peek-metadatum:met %groups rid)
    (peek-metadatum:met %graph rid)
  :_  this
  =;  =cage
    [%pass / %agent [src.bowl %metadata-pull-hook] %poke cage]~
  :-  %metadata-hook-update
  !>  ^-  hook-update:store
  [%preview rid channels members channel-count metadatum]
  ::
  ++  channels
    %-  ~(gas by *associations:store)
    %+  skim  ~(tap by (app-metadata-for-group:met rid %graph))
    |=([=md-resource:store group=resource =metadatum:store] preview.metadatum)
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
  =+  !<(=update:store vase)
  ?.  ?=(?(%add %remove %update) -.update)
    %.n
  =/  role=(unit (unit role-tag))
    (role-for-ship:grp group.update src.bowl)
  ?~  role  %.n
  ?~  u.role  %.n
  ?=(?(%admin %moderator) u.u.role)
::
++  resource-for-update
  |=  =vase
  ^-  (list resource)
  =/  =update:store  !<(update:store vase)
  ?.  ?=(?(%add %remove %initial-group) -.update)  ~
  ~[group.update]
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
  =/  =associations:store
    (metadata-for-group:met rid)
  ?>  ?=(^ group)
  ?>  (~(has in members.u.group) src.bowl)
  !>  ^-  update:store
  [%initial-group rid associations]
::
--
