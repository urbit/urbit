/+  store=contact-store, res=resource, contact, default-agent, dbug, push-hook
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
  =/  =update:store  !<(update:store vase)
  ~
  ::?-  -.update
  ::  %initial     !!
  ::  %add         !!
  ::  %remove      !!
  ::  %edit        !!
  ::  %allow       !!
  ::  %disallow    !!
  ::  %set-public  !!
  ::==
::
++  initial-watch
  |=  [=path =resource:res]
  ^-  vase
  ?>  (is-allowed:con src.bowl)
  !>  ^-  update:store
  =/  contact=(unit contact:store)  (get-contact:con our.bowl)
  :+  %add
    our.bowl
  ?^  contact  u.contact
  *contact:store
::
++  take-update
  |=  =vase
  ^-  [(list card) agent]
  =/  =update:store  !<(update:store vase)
  ?.  ?=(%disallow -.update)  [~ this]
  :_  this
  [%give %kick ~[resource+(en-path:res [our.bowl %our])] ~]~
--
