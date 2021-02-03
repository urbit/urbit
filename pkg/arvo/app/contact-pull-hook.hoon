/-  *resource
/+  store=contact-store, contact, default-agent, verb, dbug, pull-hook
~%  %contact-pull-hook-top  ..part  ~
|%
+$  card  card:agent:gall
++  config
  ^-  config:pull-hook
  :*  %contact-store
      update:store
      %contact-update
      %contact-push-hook
      %.y  :: necessary to enable p2p
  ==
--
::
%-  agent:dbug
%+  verb  |
^-  agent:gall
%-  (agent:pull-hook config)
^-  (pull-hook:pull-hook config)
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
    dep   ~(. (default:pull-hook this config) bowl)
    con   ~(. contact bowl)
::
++  on-init   on-init:def
++  on-save   !>(~)
++  on-load   on-load:def
++  on-poke   on-poke:def
++  on-peek   on-peek:def
++  on-arvo   on-arvo:def
++  on-fail   on-fail:def
++  on-agent  on-agent:def
++  on-watch  
  |=  =path
  ?.  ?=([%nacks ~] path)
    (on-watch:def path)
  ?>  (team:title [src our]:bowl)
  `this
::
++  on-leave  on-leave:def
++  resource-for-update  resource-for-update:con
++  on-pull-nack
  |=  [=resource =tang]
  ^-  (quip card _this)
  :_  this
  [%give %fact ~[/nacks] resource+!>(resource)]~
::
++  on-pull-kick  |=(=resource `/)
--
