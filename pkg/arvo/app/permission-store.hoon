::  permission-store [landscape]: deprecated
::
/+  default-agent
|%
+$  card  card:agent:gall
+$  versioned-state
  $%  state-0
      state-1
  ==
::
+$  state-0  [%0 *]
+$  state-1  [%1 ~]
--
::
=|  state-1
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
::
++  on-poke   on-poke:def
++  on-peek   on-peek:def
++  on-watch  on-watch:def
++  on-leave  on-leave:def
++  on-agent  on-agent:def
++  on-arvo   on-arvo:def
++  on-fail   on-fail:def
--
