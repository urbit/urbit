/+  default-agent, dbug
::
=|  state=~
%-  agent:dbug
^-  agent:gall
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
::
++  on-poke
  |=  [=mark =vase]
  :_  this
  [%pass /flog %arvo %d %flog %text "running-flub-received"]~
::
++  on-watch  on-watch:def
++  on-leave  on-leave:def
++  on-init   `this
++  on-save   !>(state)
++  on-load   |=(old=vase `this(state !<(_state old)))
++  on-agent  on-agent:def
++  on-arvo   on-arvo:def
++  on-peek   on-peek:def
++  on-fail   on-fail:def
--
