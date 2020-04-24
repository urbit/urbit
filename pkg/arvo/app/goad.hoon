/+  default-agent, verb
%+  verb  |
^-  agent:gall
=>
  |%
  ++  goad
    |=  force=?
    :~  [%pass /gall %arvo %g %goad force ~]
    ==
  --
|_  =bowl:gall
+*  this  .
    def  ~(. (default-agent this %|) bowl)
++  on-init   on-init:def
++  on-save   on-save:def
++  on-load   on-load:def
++  on-poke
  |=  [=mark =vase]
  ?:  ?=([%noun * %go] +<)
    [(goad |) this]
  ?:  ?=([%noun * %force] +<)
    [(goad &) this]
  (on-poke:def mark vase)
::
++  on-watch  on-watch:def
++  on-leave  on-leave:def
++  on-peek   on-peek:def
++  on-agent  on-agent:def
++  on-arvo   on-arvo:def
++  on-fail   on-fail:def
--
