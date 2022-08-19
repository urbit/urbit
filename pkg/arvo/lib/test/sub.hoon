/+  default-agent, verb, dbug
::
=|  state=~
%-  agent:dbug
%+  verb  &
^-  agent:gall
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
::
++  on-poke
  |=  [=mark =vase]
  =+  !<(=ship vase)
  ?+  mark  !!
    %leave  [[%pass /sub %agent [ship %pub] %leave ~]~ this]
    %watch  [[%pass /sub %agent [ship %pub] %watch /foo]~ this]
  ==
::
++  on-watch
  |=  =path
  (on-watch:def +<)
::
++  on-agent
  |=  [=wire =sign:agent:gall]
  ?.  =(/sub wire)
    ~|  wire+wire  !!
  ~&  sub+-.sign
  ?+  -.sign  !!
    %kick  [[%pass /sub %agent [our.bowl %pub] %watch /foo]~ this]
    %fact       `this
    %watch-ack  `this
  ==
::
++  on-fail
  |=  [=term =tang]
  (mean ':sub +on-fail' term tang)
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
