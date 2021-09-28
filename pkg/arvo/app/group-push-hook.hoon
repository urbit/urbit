::  group-hook [landscape]:
::
::  allow syncing group data from foreign paths to local paths
::
/-  *group, *invite-store
/+  default-agent, verb, dbug, store=group-store, grpl=group, push-hook,
    resource
~%  %group-hook-top  ..part  ~
|%
+$  card  card:agent:gall

::
++  config
  ^-  config:push-hook
  :*  %group-store
      /groups
      update:store
      %group-update
      %group-pull-hook
      0  0
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
::
++  on-init  on-init:def
++  on-save  !>(~)
++  on-load    on-load:def
++  on-poke    on-poke:def
++  on-agent   on-agent:def
++  on-watch   on-watch:def
++  on-leave   on-leave:def
++  on-peek    on-peek:def
++  on-arvo    on-arvo:def
++  on-fail    on-fail:def
::
++  transform-proxy-update
  |=  vas=vase
  ^-  (quip card (unit vase))
  =/  =update:store  !<(update:store vas)
  :-  ~
  ?:  ?=(%initial -.update)
    ~
  |^
  =/  role=(unit (unit role-tag))
    (role-for-ship:grp resource.update src.bowl)
  ?~  role
    non-member
  ?~  u.role
    member
  ?-  u.u.role
    %admin      admin
    %moderator  moderator
    %janitor    member
  ==
  ::
  ++  member
    ?:  ?|  ?&  ?=(%add-members -.update)
                =(~(tap in ships.update) ~[src.bowl])    
            ==
            ?&  ?=(%remove-members -.update)
                =(~(tap in ships.update) ~[src.bowl])    
        ==  ==
      `vas
    ~
  ::
  ++  admin
    ?.  ?=(?(%remove-group %add-group) -.update)
      `vas
    ~
  ::
  ++  moderator
    ?:  ?=(?(%add-members %remove-members %add-tag %remove-tag) -.update)
      `vas
    ~
  ::
  ++  non-member
    ?:  ?&  ?=(%add-members -.update)
            (can-join:grp resource.update src.bowl)
            =(~(tap in ships.update) ~[src.bowl])
        ==
      `vas
    ~
  --
::
++  resource-for-update  resource-for-update:grp
::
++  take-update
  |=   =vase
  ^-  [(list card) agent]
  =/  =update:store
    !<(update:store vase)
  ?:  ?=(%remove-group -.update)
    =/  paths
      ~[resource+(en-path:resource resource.update)]
    :_  this
    [%give %kick paths ~]~
  ?.  ?=(%remove-members -.update)
    [~ this]
  =/  paths
    ~[resource+(en-path:resource resource.update)]
  :_  this
  %+  turn
    ~(tap in ships.update)
  |=  =ship
  [%give %kick paths `ship]
::
++  initial-watch
  |=  [=path rid=resource]
  ^-  vase
  =/  group
    (scry-group:grp rid)
  ?>  ?=(^ group)
  ?>  (~(has in members.u.group) src.bowl)
  !>  ^-  update:store
  [%initial-group rid u.group]
::
--
