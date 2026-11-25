/+  default-agent, verb, dbug
::
=|  state=~
:-  %new-agent
%-  agent:dbug
%+  verb  &
^-  form:agent:gall
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
::
++  on-poke
  |=  [=mark =vase]
  (on-poke:def +<)
::
++  on-watch
  |=  =path
  `this
::
++  on-agent
  |=  [=wire =sign:agent:gall]
  (on-agent:def +<)
::
++  on-fail
  |=  [=frag:agent:gall =call:agent:gall]
  (mean leaf+":pub +on-fail {<from.frag>}" tang.frag)
::
++  on-leave
  |=  =path
  `this
::
++  on-init   `this
++  on-save   !>(state)
++  on-load   |=(old=vase `this(state !<(_state old)))
++  on-arvo   on-arvo:def
++  on-peek   on-peek:def
--
