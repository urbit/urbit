::  link-proxy-hook: no longer in use
::
/+  default-agent, verb, dbug
~%  %link-proxy-hook-top  ..is  ~
|%
+$  versioned-state
  $%  [%0 *]
      [%1 *]
      [%2 ~]
  ==
::
+$  card  card:agent:gall
--
::
=|  [%2 ~]
=*  state  -
::
%-  agent:dbug
%+  verb  |
^-  agent:gall
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %&) bowl)
::
++  on-init  on-init:def
++  on-save  !>(state)
++  on-load
  |=  old-vase=vase
  ^-  (quip card _this)
  =/  paths
    %+  turn  ~(val by sup.bowl)
    |=([=ship =path] path)
  :_  this
  :-  [%pass /groups %agent [our.bowl %group-store] %leave ~]
  ?~  paths  ~
  [%give %kick paths ~]~
::
++  on-watch  on-watch:def
++  on-leave  on-leave:def
++  on-agent  on-agent:def
++  on-poke  on-poke:def
++  on-peek  on-peek:def
++  on-arvo  on-arvo:def
++  on-fail  on-fail:def
--

