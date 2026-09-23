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
             [%pass /(scot %da now.bowl) %arvo %behn %wait `@da`+(now.bowl)]~
  ==
::
++  on-watch  on-watch:def
++  on-leave  on-leave:def
++  on-peek   on-peek:def
++  on-agent  on-agent:def
++  on-arvo
  |=  [=wire gift=gift-user-v1:gall]
  ^-  (quip card _this)
  =?  gift  ?=(%syscall -.gift)
    =+  !<(=sign-arvo [-:!>(*sign-arvo) sign-arvo.gift])
    ?.  ?=([%behn %wake *] sign-arvo)  gift
    [%behn %wake now.bowl]
  ?+    wire  !!
      [@ ~]
    ?>  ?=([%behn %wake *] gift)
    ~&  [%took `@dr`(sub now.bowl (slav %da i.wire))]
    [~ this]
  ==
++  on-fail   on-fail:def
--
