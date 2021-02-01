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
++  on-watch  on-watch:def
++  on-leave  on-leave:def
++  on-pull-nack
  |=  [=resource =tang]
  ^-  (quip card _this)
  :_  this
  ?~  (get-contact:con entity.resource)  ~
  =-  [%pass /pl-nack %agent [our.bowl %contact-store] %poke %contact-update -]~
  !>  ^-  update:store
  [%remove entity.resource]
::
++  on-pull-kick  |=(=resource `/)
--
