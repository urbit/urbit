::  permission-hook [landscape]: deprecated
::
/+  default-agent
::
=|  [%1 ~]
=*  state  -
::
^-  agent:gall
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
::
++  on-init  on-init:def
++  on-save  !>(state)
++  on-load
  |=  old=vase
  [~ this]
++  on-poke   on-poke:def
++  on-watch  on-watch:def
++  on-agent  on-agent:def
++  on-leave  on-leave:def
++  on-peek   on-peek:def
++  on-arvo   on-arvo:def
++  on-fail   on-fail:def
--

