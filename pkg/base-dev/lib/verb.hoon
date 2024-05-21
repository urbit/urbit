::  Print what your agent is doing.
::
/-  *verb
::
|=  [loud=? =agent:gall]
=|  bowl-print=_|
^-  agent:gall
|^  !.
|_  =bowl:gall
+*  this  .
    ag    ~(. agent bowl)
::
++  on-init
  ^-  (quip card:agent:gall agent:gall)
  %-  (print bowl |.("{<dap.bowl>}: on-init"))
  =^  cards  agent  on-init:ag
  :_  this
  :_  :_  cards
    (emit-event %on-init ~)
  (emit-event-plus bowl [%on-init ~] cards)
::
++  on-save
  ^-  vase
  %-  (print bowl |.("{<dap.bowl>}: on-save"))
  on-save:ag
::
++  on-load
  |=  old-state=vase
  ^-  (quip card:agent:gall agent:gall)
  %-  (print bowl |.("{<dap.bowl>}: on-load"))
  =^  cards  agent  (on-load:ag old-state)
  :_  this
  :_  :_  cards
    (emit-event %on-load ~)
  (emit-event-plus bowl [%on-load ~] cards)
::
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card:agent:gall agent:gall)
  %-  (print bowl |.("{<dap.bowl>}: on-poke with mark {<mark>}"))
  ?:  ?=(%verb mark)
    ?-  !<(?(%loud %bowl) vase)
      %loud  `this(loud !loud)
      %bowl  `this(bowl-print !bowl-print)
    ==
  =^  cards  agent  (on-poke:ag mark vase)
  :_  this
  :_  :_  cards
    (emit-event %on-poke mark)
  (emit-event-plus bowl [%on-poke mark (mug q.vase)] cards)
::
++  on-watch
  |=  =path
  ^-  (quip card:agent:gall agent:gall)
  %-  (print bowl |.("{<dap.bowl>}: on-watch on path {<path>}"))
  =^  cards  agent
    ?:  ?=([%verb ?(%events %events-plus) ~] path)
      [~ agent]
    (on-watch:ag path)
  :_  this
  :_  :_  cards
    (emit-event %on-watch path)
  (emit-event-plus bowl [%on-watch path] cards)
::
++  on-leave
  |=  =path
  ^-  (quip card:agent:gall agent:gall)
  %-  (print bowl |.("{<dap.bowl>}: on-leave on path {<path>}"))
  ?:  ?=([%verb %event ~] path)
    [~ this]
  =^  cards  agent  (on-leave:ag path)
  :_  this
  :_  :_  cards
    (emit-event %on-leave path)
  (emit-event-plus bowl [%on-leave path] cards)
::
++  on-peek
  |=  =path
  ^-  (unit (unit cage))
  %-  (print bowl |.("{<dap.bowl>}: on-peek on path {<path>}"))
  (on-peek:ag path)
::
++  on-agent
  |=  [=wire =sign:agent:gall]
  ^-  (quip card:agent:gall agent:gall)
  %-  (print bowl |.("{<dap.bowl>}: on-agent on wire {<wire>}, {<-.sign>}"))
  =^  cards  agent  (on-agent:ag wire sign)
  :_  this
  :_  :_  cards
    (emit-event %on-agent wire -.sign)
  =;  =^sign
    (emit-event-plus bowl [%on-agent wire sign] cards)
  ?-  -.sign
    %poke-ack   [%poke-ack ?=(~ p.sign)]
    %watch-ack  [%watch-ack ?=(~ p.sign)]
    %kick       [%kick ~]
    %fact       [%fact p.cage.sign (mug q.q.cage.sign)]
  ==
::
++  on-arvo
  |=  [=wire =sign-arvo]
  ^-  (quip card:agent:gall agent:gall)
  %-  %+  print  bowl  |.
      "{<dap.bowl>}: on-arvo on wire {<wire>}, {<[- +<]:sign-arvo>}"
  =^  cards  agent  (on-arvo:ag wire sign-arvo)
  :_  this
  :_  :_  cards
    (emit-event %on-arvo wire [- +<]:sign-arvo)
  (emit-event-plus bowl [%on-arvo wire [- +<]:sign-arvo] cards)
::
++  on-fail
  |=  [=term =tang]
  ^-  (quip card:agent:gall agent:gall)
  %-  (print bowl |.("{<dap.bowl>}: on-fail with term {<term>}"))
  =^  cards  agent  (on-fail:ag term tang)
  :_  this
  :_  :_  cards
    (emit-event %on-fail term)
  (emit-event-plus bowl [%on-fail term] cards)
--
::
++  print
  |=  [=bowl:gall render=(trap tape)]
  ^+  same
  =?  .  bowl-print
    %-  (slog >bowl< ~)
    .
  ?.  loud  same
  %-  (slog [%leaf $:render] ~)
  same
::
++  emit-event
  |=  =event
  ^-  card:agent:gall
  [%give %fact ~[/verb/events] %verb-event !>(event)]
::
++  emit-event-plus
  |=  [=bowl:gall =cause cards=(list card:agent:gall)]
  ^-  card:agent:gall
  =;  event=event-plus
    [%give %fact ~[/verb/events-plus] %verb-event-plus !>(event)]
  =-  [act.bowl now.bowl src.bowl sap.bowl cause -]
  %+  turn  cards
  |=  =card:agent:gall
  ^-  effect
  ::TODO  for %fact, %kick, could calculate how many ships affected
  ?-  card
      [%pass * %agent * ?(%poke %poke-as) *]
    =,  q.card
    =/  =cage  ?-(-.task.q.card %poke cage.task, %poke-as [mark.task q.cage.task])
    [%poke p.card [ship name] p.cage `@`(mug q.q.cage)]
  ::
      [%pass * %agent * ?(%watch %watch-as) *]
    =,  q.card
    =/  =path  ?-(-.task.q.card %watch path.task, %watch-as path.task)
    [%watch p.card [ship name] path]
  ::
      [%pass * %agent * %leave *]
    =,  q.card
    [%leave p.card [ship name]]
  ::
      [%give %fact *]
    =,  p.card
    [%fact paths p.cage (mug q.q.cage)]
  ::
      [%give %kick *]
    [%kick paths.p.card]
  ::
      [%give ?(%poke-ack %watch-ack) *]
    ~|  %explicit-ack
    !!  ::  shouldn't be given explicitly
  ::
      [%pass * %arvo *]
    [%arvo p.card -.q.card +<.q.card]
  ::
      [%pass *]
    [%arvo p.card %$ -.q.card]
  ::
      [%slip *]
    $(card [%pass //slip p.card])
  ==
--
