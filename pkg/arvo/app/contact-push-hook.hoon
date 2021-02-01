/+  store=contact-store, res=resource, contact, group,
    default-agent, dbug, push-hook
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
  ==
::
+$  agent  (push-hook:push-hook config)
--
::
%-  agent:dbug
^-  agent:gall
%-  (agent:push-hook config)
^-  agent
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
    con   ~(. contact bowl)
    grp   ~(. group bowl)
::
++  on-init   on-init:def
++  on-save   !>(~)
++  on-load   on-load:def
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
  ^-  ?
  =/  =update:store  !<(update:store vase)
  ?-  -.update
    %initial     %.n
    %add         %.y
    %remove      %.y
    %edit        %.y
    %allow       %.n
    %disallow    %.n
    %set-public  %.n
  ==
::
++  resource-for-update
  |=  =vase
  ^-  (list resource:res)
  |^
  =/  =update:store  !<(update:store vase)
  ?-  -.update
    %initial     ~
    %add         (rids-for-ship ship.update)
    %remove      (rids-for-ship ship.update)
    %edit        (rids-for-ship ship.update)
    %allow       ~
    %disallow    ~
    %set-public  ~
  ==
  ::
  ++  rids-for-ship
    |=  s=ship
    ^-  (list resource:res)
    ::  if the ship is in any group that I am pushing updates for, push
    ::  it out to that resource.
    ::
    %+  skim  ~(tap in scry-sharing)
    |=  r=resource:res
    (is-member:grp s r)
  ::
  ++  scry-sharing
    .^  (set resource:res)
      %gx
      (scot %p our.bowl)
      %contact-push-hook
      (scot %da now.bowl)
      /sharing/noun
    ==
  --
::
++  initial-watch
  |=  [=path =resource:res]
  ^-  vase
  |^
  ?>  (is-allowed:con src.bowl)
  !>  ^-  update:store
  [%initial rolo %.n]
  ::
  ++  rolo
    ^-  rolodex:store
    =/  ugroup  (scry-group:grp resource)
    ?~  ugroup  *rolodex:store
    %-  ~(gas by *rolodex:store)
    %+  murn  ~(tap in (members:grp resource))
    |=  s=ship
    ^-  (unit [ship contact:store])
    =/  c=(unit contact:store)  (get-contact:con s)
    ?~(c ~ `[s u.c])
  --
::
++  take-update
  |=  =vase
  ^-  [(list card) agent]
  =/  =update:store  !<(update:store vase)
  ?.  ?=(%disallow -.update)  [~ this]
  :_  this
  [%give %kick ~[resource+(en-path:res [our.bowl %our])] ~]~
--
