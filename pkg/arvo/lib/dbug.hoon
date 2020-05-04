::  dbug: agent wrapper for generic debugging tools
::
::    usage: %-(agent:dbug your-agent)
::
|%
+$  poke
  $%  [%bowl ~]
      [%state grab=cord]
      [%incoming =about]
      [%outgoing =about]
  ==
::
+$  about
  $@  ~
  $%  [%ship =ship]
      [%path =path]
      [%wire =wire]
      [%term =term]
  ==
::
++  agent
  |=  =agent:gall
  ^-  agent:gall
  !.
  |_  =bowl:gall
  +*  this  .
      ag    ~(. agent bowl)
  ::
  ++  on-poke
    |=  [=mark =vase]
    ^-  (quip card:agent:gall agent:gall)
    ?.  ?=(%dbug mark)
      =^  cards  agent  (on-poke:ag mark vase)
      [cards this]
    =/  dbug
      !<(poke vase)
    =;  =tang
      ((%*(. slog pri 1) tang) [~ this])
    ?-  -.dbug
      %bowl   [(sell !>(bowl))]~
    ::
        %state
      =?  grab.dbug  =('' grab.dbug)  '-'
      =-  [(sell -)]~
      %+  slap
        (slop on-save:ag !>([bowl=bowl ..zuse]))
      (ream grab.dbug)
    ::
        %incoming
      =;  =tang
        ?^  tang  tang
        [%leaf "no matching subscriptions"]~
      %+  murn
        %+  sort  ~(tap by sup.bowl)
        |=  [[* a=[=ship =path]] [* b=[=ship =path]]]
        (aor [path ship]:a [path ship]:b)
      |=  [=duct [=ship =path]]
      ^-  (unit tank)
      =;  relevant=?
        ?.  relevant  ~
        `>[path=path from=ship duct=duct]<
      ?:  ?=(~ about.dbug)  &
      ?-  -.about.dbug
        %ship  =(ship ship.about.dbug)
        %path  ?=(^ (find path.about.dbug path))
        %wire  %+  lien  duct
               |=(=wire ?=(^ (find wire.about.dbug wire)))
        %term  !!
      ==
    ::
        %outgoing
      =;  =tang
        ?^  tang  tang
        [%leaf "no matching subscriptions"]~
      %+  murn
        %+  sort  ~(tap by wex.bowl)
        |=  [[[a=wire *] *] [[b=wire *] *]]
        (aor a b)
      |=  [[=wire =ship =term] [acked=? =path]]
      ^-  (unit tank)
      =;  relevant=?
        ?.  relevant  ~
        `>[wire=wire agnt=[ship term] path=path ackd=acked]<
      ?:  ?=(~ about.dbug)  &
      ?-  -.about.dbug
        %ship  =(ship ship.about.dbug)
        %path  ?=(^ (find path.about.dbug path))
        %wire  ?=(^ (find wire.about.dbug wire))
        %term  =(term term.about.dbug)
      ==
    ==
  ::
  ++  on-init
    ^-  (quip card:agent:gall agent:gall)
    =^  cards  agent  on-init:ag
    [cards this]
  ::
  ++  on-save   on-save:ag
  ::
  ++  on-load
    |=  old-state=vase
    ^-  (quip card:agent:gall agent:gall)
    =^  cards  agent  (on-load:ag old-state)
    [cards this]
  ::
  ++  on-watch
    |=  =path
    ^-  (quip card:agent:gall agent:gall)
    =^  cards  agent  (on-watch:ag path)
    [cards this]
  ::
  ++  on-leave
    |=  =path
    ^-  (quip card:agent:gall agent:gall)
    =^  cards  agent  (on-leave:ag path)
    [cards this]
  ::
  ++  on-peek  on-peek:ag
  ::
  ++  on-agent
    |=  [=wire =sign:agent:gall]
    ^-  (quip card:agent:gall agent:gall)
    =^  cards  agent  (on-agent:ag wire sign)
    [cards this]
  ::
  ++  on-arvo
    |=  [=wire =sign-arvo]
    ^-  (quip card:agent:gall agent:gall)
    =^  cards  agent  (on-arvo:ag wire sign-arvo)
    [cards this]
  ::
  ++  on-fail
    |=  [=term =tang]
    ^-  (quip card:agent:gall agent:gall)
    =^  cards  agent  (on-fail:ag term tang)
    [cards this]
  --
--
