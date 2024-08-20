:: invite-view [landscape]:
::
::  deprecated
::
/+  default-agent
^-  agent:gall
|_  =bowl:gall
+*  this  .
    def  ~(. (default-agent this %|) bowl)
    step  `step:agent:gall`[~ this]
++  on-init   on-init:def
++  on-save   on-save:def
++  on-load   on-load:def
++  on-poke   |=(* step)
++  on-watch  on-watch:def
++  on-leave  |=(* step)
++  on-peek   |=(* ~)
++  on-agent  |=(* step)
++  on-arvo   |=(* step)
++  on-fail   on-fail:def
--
