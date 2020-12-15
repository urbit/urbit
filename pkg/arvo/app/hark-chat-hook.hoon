::  hark-chat-hook: notifications for chat-store [landscape]
::
/+  default-agent
::
^-  agent:gall
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
++  on-init  [~ this]
++  on-save  !>(~)
++  on-load
  |=  =vase
  :_  this
  [%pass /chat %agent [our.bowl %chat-store] %leave ~]~
++  on-arvo   on-arvo:def
++  on-agent  on-agent:def
++  on-poke   on-poke:def
++  on-peek   on-peek:def
++  on-watch  on-watch:def
++  on-leave  on-leave:def
++  on-fail   on-fail:def
--
