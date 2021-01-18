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
++  on-watch    
  |=  =path
  ~&  path
  ^-  (quip card _this)
  ?.  ?=([%preview @ @ @ ~] path)
    (on-watch:def path)
  =/  rid=resource
    (de-path:resource t.path)
  |^
  ?>  =(entity.rid our.bowl)
  ?>  (can-join:grp rid src.bowl)
  =/  members
    ~(wyt in (members:grp rid))
  =/  =metadata
    (need (peek-metadata:met %contacts rid))
  :_  this
  =;  =cage
    :~  [%give %fact ~ cage]
        [%give %kick ~ ~]
    ==
  :-  %metadata-update
  !>  ^-  metadata-update
  [%preview rid channels members channel-count metadata]
    ::  TODO: rank by popularity
  ::  how tho, if the group owner isn't in some graphs
  ++  channels
    %-  ~(gas by *associations)
    (scag 3 ~(tap by (app-metadata-for-group:met rid %graph)))
  ++  channel-count
    ~(wyt by (app-metadata-for-group:met rid %graph))
  --
::
++  on-leave    on-leave:def
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
  ?>  ?=(^ group)
  ?>  (~(has in members.u.group) src.bowl)
  !>  ^-  metadata-update
  [%initial-group rid associations]
::
--
