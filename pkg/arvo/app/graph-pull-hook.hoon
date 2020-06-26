/-  *resource
/+  store=graph-store, default-agent, verb, dbug, pull-hook
~%  %graph-pull-hook-top  ..is  ~
|%
+$  card  card:agent:gall
++  config
  ^-  config:pull-hook
  :*  %graph-store
      update:store
      %graph-update
      %graph-push-hook
  ==
--
::
%-  agent:dbug
^-  agent:gall
%-  (agent:pull-hook config)
^-  (pull-hook:pull-hook config)
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
    dep   ~(. (default:pull-hook this config) bowl)
::
++  on-init       on-init:def
++  on-save       !>(~)
++  on-load       on-load:def
++  on-poke       on-poke:def
++  on-peek       on-peek:def
++  on-arvo       on-arvo:def
++  on-fail       on-fail:def
++  on-agent      on-agent:def
++  on-watch      on-watch:def
++  on-leave      on-leave:def
++  on-pull-kick  |=(=resource `(unit path)`[~ /])
++  on-pull-nack  on-pull-nack:dep
  |=   [=resource =tang]
  ^-  (quip card _this)
  [~ this]
--
