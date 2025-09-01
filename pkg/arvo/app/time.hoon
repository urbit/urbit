::
/+  default-agent, verb
::
|%
::
+$  card  card:agent:gall
--
^-  agent:gall
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
::
++  on-init  [~ this]
++  on-save  !>(~)
++  on-load  _on-init
++  on-poke
  |=  [=mark =vase]
  ?>  (team:title [our src]:bowl)
  ?+    mark  !!
      %noun  :_  this
             [%pass /(scot %da now.bowl) %arvo %b %wait `@da`+(now.bowl)]~
  ==
::
++  on-watch  on-watch:def
++  on-leave  on-leave:def
++  on-peek   on-peek:def
++  on-agent  on-agent:def
++  on-arvo
  |=  [=wire sign=sign-arvo]
  ^-  (quip card _this)
  ?+    wire  !!
      [@ ~]
    ?>  ?=(%wake +<.sign)
    ~&  [%took `@dr`(sub now.bowl (slav %da i.wire))]
    [~ this]
  ==
++  on-fail   on-fail:def
--
