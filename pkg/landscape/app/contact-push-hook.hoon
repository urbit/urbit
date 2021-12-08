/-  pull-hook
/+  store=contact-store, res=resource, contact, group,
    default-agent, dbug, push-hook, agentio, verb
~%  %contact-push-hook-top  ..part  ~
|%
+$  card  card:agent:gall
++  config
  ^-  config:push-hook
  :*  %contact-store
      /updates
      update:store
      %contact-update
      %contact-pull-hook
      0  0
  ==
::
+$  agent  (push-hook:push-hook config)
::
+$  share  [%share =ship]
--
::
%-  agent:dbug
^-  agent:gall
%+  verb  |
%-  (agent:push-hook config)
^-  agent
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
    con   ~(. contact bowl)
    grp   ~(. group bowl)
    io    ~(. agentio bowl)
::
++  on-init
  ^-  (quip card _this)
  :_  this
  :-  %+  poke-our:pass:io  %contact-push-hook
      :-  %push-hook-action
      !>(`action:push-hook`[%add [our.bowl %'']])
  %+  murn  ~(tap in scry-groups:grp)
  |=  rid=res
  ?.  =(our.bowl entity.rid)  ~
  ?.  (is-managed:grp rid)    ~
  `(poke-self:pass:io push-hook-action+!>([%add rid]))
::
++  on-save   !>(~)
++  on-load   on-load:def
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  ?.  =(mark %contact-share)  (on-poke:def mark vase)
  =/  =share  !<(share vase)
  :_  this  :_  ~
  ?:  =(our.bowl src.bowl)
    ?<  =(ship.share our.bowl)
    ::  proxy poke
    %+  poke:pass:io  [ship.share dap.bowl]
    contact-share+!>([%share our.bowl])
  ::  accept share
  ?>  =(src.bowl ship.share)
  %+  poke-our:pass:io  %contact-pull-hook
  pull-hook-action+!>([%add src.bowl [src.bowl %$]])
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
  ^-  (quip card (unit vase))
  ::  TODO: should check if user is allowed to %add, %remove, %edit
  ::  contact
  =/  =update:store  !<(update:store vas)
  :-  ~
  ?-  -.update
    %initial     ~
    %add         `vas
    %remove      `vas
    %edit        `vas
    %allow       ~
    %disallow    ~
    %set-public  ~
  ==
::
++  resource-for-update  resource-for-update:con
::
++  initial-watch
  |=  [=path =resource:res]
  ^-  vase
  |^
  ?>  (is-allowed:con resource src.bowl)
  !>  ^-  update:store
  [%initial rolo %.n]
  ::
  ++  rolo
    ^-  rolodex:store
    =/  ugroup  (scry-group:grp resource)
    =/  =rolodex:store
      (scry-for:con rolodex:store /all)
    %-  ~(gas by *rolodex:store)
    ?~  ugroup
      =/  c=(unit contact:store)  (~(get by rolodex) our.bowl)
      ?~  c
        [our.bowl *contact:store]~
      [our.bowl u.c]~
    %+  murn  ~(tap in (members:grp resource))
    |=  s=ship
    ^-  (unit [ship contact:store])
    =/  c=(unit contact:store)  (~(get by rolodex) s)
    ?~(c ~ `[s u.c])
  --
::
++  take-update
  |=  =vase
  ^-  [(list card) agent]
  =/  =update:store  !<(update:store vase)
  ?+  -.update  [~ this]
      %disallow
    :_  this
    [%give %kick ~[resource+(en-path:res [our.bowl %''])] ~]~
  ::
      %set-public
    :_  this
    ?.  public.update
      [%give %kick ~[resource+(en-path:res [our.bowl %''])] ~]~
    %+  murn  ~(tap in scry-groups:grp)
    |=  rid=res
    ?:  =(our.bowl entity.rid)  ~
    ?.  (is-managed:grp rid)    ~
    `(poke-self:pass:io contact-share+!>([%share entity.rid]))
  ==
--
