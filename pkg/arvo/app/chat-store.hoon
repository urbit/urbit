:: chat-store [landscape]: deprecated
::
/-  store=chat-store
/+  default-agent
|%
+$  card  card:agent:gall
--
::
^-  agent:gall
|_  =bowl:gall
+*  this       .
    def        ~(. (default-agent this %|) bowl)
::
++  on-init   on-init:def
++  on-save   !>(~)
++  on-load
  |=  old-vase=vase
  ^-  (quip card _this)
  [~ this]
::
++  on-poke   on-poke:def
++  on-watch  on-watch:def
++  on-leave  on-leave:def
++  on-peek   on-peek:def
++  on-agent  on-agent:def
++  on-arvo   on-arvo:def
++  on-fail   on-fail:def
--
