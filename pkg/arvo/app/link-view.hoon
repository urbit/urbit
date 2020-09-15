::  link-view: no longer in use
/+  default-agent, verb, dbug
~%  %link-view-top  ..is  ~
|%
+$  versioned-state
  $%  [%0 ~]
      [%1 ~]
      [%2 ~]
  ==
::
+$  card  card:agent:gall
--
::
=|  [%2 ~]
=*  state  -
::
%+  verb  |
%-  agent:dbug
^-  agent:gall
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
::
++  on-init  [~ this]
++  on-save  !>(state)
++  on-load
  |=  old-vase=vase
  ^-  (quip card _this)
  :_  this(state [%2 ~])
  [%pass /connect %arvo %e %disconnect [~ /'~link']]~
::
++  on-poke   on-poke:def
++  on-watch  on-watch:def
++  on-agent  on-agent:def
++  on-arvo   on-arvo:def
++  on-peek   on-peek:def
++  on-leave  on-leave:def
++  on-fail   on-fail:def
--
