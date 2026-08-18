::  seer-to-scry: agent wrapper to transform seers into .^ scries
::
::    usage: %-(agent:seer-to-scry your-agent)
::
::  this wrapper should process each of the agent's ten arms by
::  "walking" through their seers until we hit a %done. if we find
::  %scry seers before hitting a %done, we need to replace its scry
::  with a .^ scry and then call its continuation gate to get the
::  next seer. when you hit the last %done, just return the p.
::
|%
++  seer-walk
  |*  [r=mold a=mold]
  |=  s=(seer r a)
  ^-  a
  ?-  -.s
    %done  p.s
    %scry  ((seer-walk r a) (k.s))
  ==
::
++  agent
  |=  =agent:gall
  ^-  agent:gall
  !.  ::  why?
  |_  =bowl:gall
  +*  this  .
      ag    ~(. agent bowl)
  ::
  ++  on-poke
    |=  [=mark =vase]
    ^-  (quip card:agent:gall agent:gall)
  ::
  ++  on-peek
    |=  =path
    ^-  (unit (unit cage))
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

