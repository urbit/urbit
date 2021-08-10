::  Print what your agent is doing.
::
/-  verb
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
  [[(emit-event %on-init ~) cards] this]
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
  [[(emit-event %on-load ~) cards] this]
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
  [[(emit-event %on-poke mark) cards] this]
::
++  on-watch
  |=  =path
  ^-  (quip card:agent:gall agent:gall)
  %-  (print bowl |.("{<dap.bowl>}: on-watch on path {<path>}"))
  =^  cards  agent
    ?:  ?=([%verb %events ~] path)
      [~ agent]
    (on-watch:ag path)
  [[(emit-event %on-watch path) cards] this]
::
++  on-leave
  |=  =path
  ^-  (quip card:agent:gall agent:gall)
  %-  (print bowl |.("{<dap.bowl>}: on-leave on path {<path>}"))
  ?:  ?=([%verb %event ~] path)
    [~ this]
  =^  cards  agent  (on-leave:ag path)
  [[(emit-event %on-leave path) cards] this]
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
  [[(emit-event %on-agent wire -.sign) cards] this]
::
++  on-arvo
  |=  [=wire =sign-arvo]
  ^-  (quip card:agent:gall agent:gall)
  %-  %+  print  bowl  |.
      "{<dap.bowl>}: on-arvo on wire {<wire>}, {<[- +<]:sign-arvo>}"
  =^  cards  agent  (on-arvo:ag wire sign-arvo)
  [[(emit-event %on-arvo wire [- +<]:sign-arvo) cards] this]
::
++  on-fail
  |=  [=term =tang]
  ^-  (quip card:agent:gall agent:gall)
  %-  (print bowl |.("{<dap.bowl>}: on-fail with term {<term>}"))
  =^  cards  agent  (on-fail:ag term tang)
  [[(emit-event %on-fail term) cards] this]
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
  |=  =event:verb
  ^-  card:agent:gall
  [%give %fact ~[/verb/events] %verb-event !>(event)]
--
