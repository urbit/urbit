::  dbug: agent wrapper for generic debugging tools
::
::    usage: %-(agent:dbug your-agent)
::
|%
+$  what
  $?  %bowl
      %incoming
      %outgoing
  ==
::
+$  about
  $%  [%ship =ship]
      [%path =path]
      [%wire =wire]
      [%term =term]
  ==
::
++  agent
  |=  =agent:gall
  ^-  agent:gall
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
      !<([=what =about] vase)
    =;  out
      ((slog >out< ~) [~ this])
    ?-  what.dbug
      %bowl  bowl
    ::
        %incoming
      %+  murn  ~(tap by sup.bowl)
      |=  sub=[=duct [=ship =path]]
      ^-  (unit _sub)
      =;  relevant=?
        ?:(relevant `sub ~)
      ?-  -.about.dbug
        %ship  =(ship.sub ship.about.dbug)
        %path  ?=(^ (find path.about.dbug path.sub))
        %wire  %+  lien  duct.sub
               |=(=wire ?=(^ (find wire.about.dbug wire)))
        %term  !!
      ==
    ::
        %outgoing
      %+  murn  ~(tap by wex.bowl)
      |=  sub=[[=wire =ship =term] [acked=? =path]]
      ^-  (unit _sub)
      =;  relevant=?
        ?:(relevant `sub ~)
      ?-  -.about.dbug
        %ship  =(ship.sub ship.about.dbug)
        %path  ?=(^ (find path.about.dbug path.sub))
        %wire  ?=(^ (find wire.about.dbug wire.sub))
        %term  =(term.sub term.about.dbug)
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
