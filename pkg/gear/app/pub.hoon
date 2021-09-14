/+  default-agent, verb, dbug
%+  verb  &
%-  agent:dbug
^-  agent:gall
::
|_  =bowl:gall
+*  this  .
    def  ~(. (default-agent this %|) bowl)
++  on-init   on-init:def
++  on-save   on-save:def
++  on-load   on-load:def
++  on-poke
  |=  [=mark =vase]
  :_  this
  [%give %fact ~[/[mark]] mark vase]~
::
++  on-watch
  |=  =path
  `this
::
++  on-leave  on-leave:def
++  on-peek   on-peek:def
++  on-agent  on-agent:def
++  on-arvo   on-arvo:def
++  on-fail   on-fail:def
--
