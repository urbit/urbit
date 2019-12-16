/+  default-agent, verb
%+  verb  &
^-  agent:gall
|_  =bowl:gall
+*  this  .
    def  ~(. (default-agent this %|) bowl)
++  on-init   on-init:def
++  on-save   on-save:def
++  on-load   on-load:def
++  on-poke   ~&  bowl  on-poke:def
++  on-watch
  |=  =path
  ~&  >>  bowl
  ?<  ?=([%sole *] path)
  :_  this  :_  ~
  [%give %fact ~ %noun !>(5)]
::
++  on-leave  on-leave:def
++  on-peek   on-peek:def
++  on-agent  on-agent:def
++  on-arvo   on-arvo:def
++  on-fail   on-fail:def
--
