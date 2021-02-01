::  contact-view [landscape]: deprecated
::
/+  default-agent
|%
+$  card  card:agent:gall
--
::
^-  agent:gall
|_  bol=bowl:gall
+*  this       .
    def        ~(. (default-agent this %|) bol)
::
++  on-init   on-init:def
++  on-poke   on-poke:def
++  on-watch  on-watch:def
++  on-agent  on-agent:def
++  on-arvo   on-arvo:def
++  on-save   !>(~)
++  on-load
  |=  old-vase=vase
  ^-  (quip card _this)
  [~ this]
::
++  on-leave  on-leave:def
++  on-peek   on-peek:def
++  on-fail   on-fail:def
--
