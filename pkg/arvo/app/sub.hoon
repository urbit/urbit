/+  default-agent, verb
%+  verb  &
^-  agent:gall
|_  =bowl:gall
+*  this  .
    def  ~(. (default-agent this %|) bowl)
++  on-init   on-init:def
++  on-save   on-save:def
++  on-load   on-load:def
++  on-poke
  |=  [=mark =vase]
  ~&  >  bowl
  =+  !<(=ship vase)
  :_  this
  [%pass /subbb %agent [ship %pub] %watch /]~
::
++  on-watch  on-watch:def
++  on-leave  on-leave:def
++  on-peek   on-peek:def
++  on-agent
  |=  [=wire =sign:agent:gall]
  ^-  (quip card:agent:gall agent:gall)
  ?.  ?=(%fact -.sign)
    (on-agent:def wire sign)
  =+  (bex (bex 256))
  `this
::
++  on-arvo   on-arvo:def
++  on-fail   on-fail:def
--
