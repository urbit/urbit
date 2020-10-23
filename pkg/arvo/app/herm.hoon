::  herm: stand-in for term.c with http interface
::
/+  default-agent, dbug, verb
=,  able:jael
|%
+$  state-0  [%0 ~]
--
::
=|  state-0
=*  state  -
%+  verb  |
%-  agent:dbug
^-  agent:gall
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
::
++  on-init
  ^-  (quip card:agent:gall _this)
  :_  this
  [%pass /herm/1 %arvo %d %view [//term/1]~]~
::
++  on-save   !>([%0 ~])
++  on-load
  |=  old=vase
  ^-  (quip card:agent:gall _this)
  [~ this(state [%0 ~])]
::
++  on-watch
  |=  =path
  ^-  (quip card:agent:gall _this)
  ?.  ?=([%herm ~] path)  !!
  [~ this]
::
++  on-arvo
  |=  [=wire =sign-arvo]
  ^-  (quip card:agent:gall _this)
  ?.  =(/herm/1 wire)  !!
  ?.  ?=([%d %blit *] sign-arvo)
    ~|  [%unexpected-sign [- +<]:sign-arvo]
    !!
  :: ~&  [dap.bowl %blit (turn p.sign-arvo head)]
  :_  this
  %+  turn  p.sign-arvo
  |=  =blit:dill
  [%give %fact [/herm]~ %blit !>(blit)]
::
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card:agent:gall _this)
  ?.  ?=(%belt mark)
    ~|  [%unexpected-mark mark]
    !!
  :_  this
  [%pass /herm/1 %arvo %d %belt !<(belt:dill vase)]~
::
++  on-leave  on-leave:def
++  on-peek   on-peek:def
++  on-agent  on-agent:def
++  on-fail   on-fail:def
--
