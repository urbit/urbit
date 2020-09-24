::  link-listen-hook: no longer in use
::
/+  default-agent, verb, dbug
::
~%  %link-listen-hook-top  ..is  ~
|%
+$  versioned-state
  $%  [%0 *]
      [%1 *]
      [%2 *]
      [%3 *]
      [%4 ~]
  ==
+$  card  card:agent:gall
--
::
=|  [%4 ~]
=*  state  -
::
%-  agent:dbug
%+  verb  |
^-  agent:gall
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
::
++  on-init  [~ this]
++  on-save  !>(state)
++  on-load
  |=  =vase
  ^-  (quip card _this)
  :_  this
  :-  [%pass /groups %agent [our.bowl %group-store] %leave ~]
  %+  turn  ~(tap in ~(key by wex.bowl))
  |=  [=wire =ship =term]
  ^-  card
  [%pass wire %agent [ship term] %leave ~]
::
++  on-arvo
  |=  [=wire =sign-arvo]
  ^-  (quip card _this)
  ?+  sign-arvo  (on-arvo:def wire sign-arvo)
      [%b *]     [~ this]
  ==
::
++  on-agent  on-agent:def
++  on-poke   on-poke:def
++  on-peek   on-peek:def
++  on-watch  on-watch:def
++  on-leave  on-leave:def
++  on-fail   on-fail:def
--
