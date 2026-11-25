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
  =+  !<(=ship vase)
  :_  this
  ?+  mark  !!
    %leave  [%pass /sub-foo/(scot %p ship) %agent [ship %pub] %leave ~]~
    %watch  [%pass /sub-foo/(scot %p ship) %agent [ship %pub] %watch /foo]~
  ==
::
++  on-watch
  |=  =path
  (on-watch:def +<)
::
++  on-agent
  |=  [=wire =sign:agent:gall]
  =/  =ship  (slav %p &2.wire)
  ?+    -.sign  !!
      %fact       `this
      %watch-ack  `this
      %kick
    [[%pass /sub-foo/(scot %p ship) %agent [ship %pub] %watch /foo]~ this]
  ==
::
++  on-fail
  |=  [=frag:agent:gall =call:agent:gall]
  (mean leaf+":sub +on-fail {<from.frag>}" tang.frag)
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
