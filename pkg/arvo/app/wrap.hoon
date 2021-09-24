/+  chic=chicken, default-agent, verb, dbug
%+  verb  &
%-  agent:dbug
^-  agent:gall
::
|_  =bowl:gall
+*  this  .
    def  ~(. (default-agent this %|) bowl)
    par  ~(. ..this:chic bowl)
++  on-init
  !@  on-init.chic
    =^  cards  ..this.chic  on-init:par
    [cards this]
  on-init:def
::
++  on-save
  !@  on-save.chic
    on-save:par
  on-save:def
::
++  on-load
  !@  on-load.chic
    |=  old-state=vase
    =^  cards  ..this.chic  (on-load:par old-state)
    [cards this]
  on-load:def
::
++  on-poke
  !@  on-poke.chic
    |=  [=mark =vase]
    =^  cards  ..this.chic  (on-poke:par mark !<(_+<+:on-poke.chic vase))
    [cards this]
  on-poke:def
::
++  on-watch
  !@  on-watch.chic
    |=  =path
    =^  cards  ..this.chic  (on-watch:par path)
    [cards this]
  on-watch:def
::
++  on-leave
  !@  on-leave.chic
    |=  =path
    =^  cards  ..this.chic  (on-leave:par path)
    [cards this]
  on-leave:def
::
++  on-peek
  !@  on-peek.chic
    on-peek:par
  on-peek:def
::
++  on-agent
  !@  on-agent.chic
    |=  [=wire =sign:agent:gall]
    =/  my-sign
      ?.  ?=(%fact -.sign)
        sign
      [%fact p.cage.sign !<(_+>:*$>(%fact _+<+:on-agent.chic) q.cage.sign)]
    =^  cards  ..this.chic  (on-agent:par wire my-sign)
    [cards this]
  on-agent:def
::
++  on-arvo
  !@  on-arvo.chic
    |=  [=wire =sign-arvo]
    =^  cards  ..this.chic  (on-arvo:par wire sign-arvo)
    [cards this]
  on-arvo:def
::
++  on-fail
  !@  on-fail.chic
    |=  [=term =tang]
    =^  cards  agent  (on-fail:par term tang)
    [cards this]
  on-fail:def
--
