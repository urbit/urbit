/-  store=demo
/+  default-agent, verb, dbug, pull-hook, agentio, resource
~%  %demo-pull-hook-top  ..part  ~
|%
+$  card  card:agent:gall
::
++  config
  ^-  config:pull-hook
  :*  %demo-store
      update:store
      %demo-update
      %demo-push-hook
      ::  do not change spacing, required by tests
      0  
      0
      %.n
  ==
::
--
::
::
%-  agent:dbug
%+  verb  |
^-  agent:gall
%-  (agent:pull-hook config)
^-  (pull-hook:pull-hook config)
|_  =bowl:gall
+*  this        .
    def         ~(. (default-agent this %|) bowl)
    dep         ~(. (default:pull-hook this config) bowl)
::
++  on-init  on-init:def
++  on-save  !>(~)
++  on-load  on-load:def
++  on-poke  on-poke:def
++  on-agent  on-agent:def
++  on-watch  on-watch:def
++  on-leave  on-leave:def
++  on-peek   on-peek:def
++  on-arvo   on-arvo:def
++  on-fail   on-fail:def
++  on-pull-nack
  |=   [=resource =tang]
  ^-  (quip card _this)
  ~&  "{<resource>}: nacked"
  %-  (slog tang)
  `this
::
++  on-pull-kick
  |=  =resource
  ^-  (unit path)
  ~&  "{<resource>}: kicked"
  `/
::
++  resource-for-update  
  |=  =vase
  =+  !<(=update:store vase)
  ~[p.update]
--

