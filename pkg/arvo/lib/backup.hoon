::  backup: agent wrapper for import/export of state
::
::
|%
+$  poke
  $%
    [%export ~]
    [%import bunt=vase]
  ==

++  agent
  |=  =agent:gall
  ^-  agent:gall
  ::  !.
  |_  bol=bowl:gall
  +*  this  .
      ag    ~(. agent bol)
  ++  on-poke
    |=  [=mark =vase]
    ^-  (quip card:agent:gall agent:gall)
    ?.  ?=(%backup mark)
      =^  cards  agent   (on-poke:ag mark vase)
      [cards this]
    =/  backup
      !<(poke vase)
    ?-  -.backup
    ::
        %export
      =/  state=^vase
        on-save:ag
      :_  this
      =-  [%pass /backup/export %arvo %c %info -]~
      %+  foal:space:userlib
        /(scot %p our.bol)/[q.byk.bol]/(scot %da now.bol)/bak/[dap.bol]/bkup
      noun+state
    ::
         %import
      =/  state=^vase
        :-  p:bunt.backup
        .^(* %cx /(scot %p our.bol)/[q.byk.bol]/(scot %da now.bol)/bak/[dap.bol]/bkup)
      =^  cards  agent  (on-load:ag state)
      [cards this]
    ==
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
