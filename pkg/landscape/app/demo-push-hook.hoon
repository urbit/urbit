/-  store=demo
/+  default-agent, verb, dbug, push-hook, resource, agentio
|%
+$  card  card:agent:gall
::
++  config
  ^-  config:push-hook
  :*  %demo-store
      /updates
      update:store
      %demo-update
      %demo-pull-hook
      ::
      0  
      0
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
    io        ~(. agentio bowl)
::
++  on-init  on-init:def
++  on-save  !>(~)
++  on-load    on-load:def
++  on-poke   on-poke:def
++  on-agent  on-agent:def
++  on-watch    on-watch:def
++  on-leave    on-leave:def
++  on-peek   on-peek:def
++  on-arvo   on-arvo:def
++  on-fail   on-fail:def
::
++  transform-proxy-update
  |=  vas=vase
  ^-  (quip card (unit vase))
  ``vas
::
++  resource-for-update  
  |=  =vase
  =+  !<(=update:store vase)
  ~[p.update]
::
++  take-update
  |=   =vase
  ^-  [(list card) agent]
  `this
::
++  initial-watch
  |=  [=path rid=resource]
  ^-  vase
  =+  .^(=update:store %gx (scry:io %demo-store (snoc `^path`log+(en-path:resource rid) %noun)))
  !>(update)
::
--
