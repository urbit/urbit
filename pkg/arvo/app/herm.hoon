::  herm: stand-in for term.c with http interface
::
/-  herm
/+  default-agent, dbug, verb
::  keep relevant mark conversions in cache for performance
::
/$  blit-to-json  %blit  %json
/$  json-to-blit  %json  %blit
/$  json-to-task  %json  %herm-task
::
=,  jael
|%
+$  state-0  [%0 ~]
--
::
=|  state-0
=*  state  -
%+  verb  |
%-  agent:dbug
^-  agent:gall
=>  |%
    ++  pass-session
      |=  [ses=@tas tas=session-task:dill]
      [%pass /dill/[ses] %arvo %d %shot ses tas]
    --
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
::
++  on-init
  ^-  (quip card:agent:gall _this)
  [~ this]
::
++  on-save   !>([%0 ~])
++  on-load
  |=  old=vase
  ^-  (quip card:agent:gall _this)
  [~ this(state [%0 ~])]
::
++  on-watch
  |=  =path
  ^-  (quip card:agent:gall _this)
  :_  this
  ~|  path
  ?>  ?=([%session @ %view ~] path)
  =*  ses  i.t.path
  ::  subscribe to the requested session
  ::
  ::NOTE  multiple views do not result in multiple subscriptions
  ::      because they go over the same wire/duct
  ::
  [(pass-session ses %view ~)]~
::
++  on-arvo
  |=  [=wire =sign-arvo]
  ^-  (quip card:agent:gall _this)
  ~|  wire
  ?+  wire  (on-arvo:def wire sign-arvo)
    [%tube *]  [~ this]  ::  we no longer care about these
  ::
    ::  pass on dill blits for the session
    ::
      [%dill @ ~]
    =*  ses  i.t.wire
    ?.  ?=([%dill %blit *] sign-arvo)
      ~|  [%unexpected-sign [- +<]:sign-arvo]
      !!
    :_  this
    %+  turn  p.sign-arvo
    |=  =blit:dill
    [%give %fact [%session ses %view ~]~ %blit !>(blit)]
  ::
    ::  clean up old-style subscriptions
    ::
      [%view @ ~]
    =*  ses  i.t.wire
    :_  this
    [%pass wire %arvo %d %shot ses %flee ~]~
  ==
::
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card:agent:gall _this)
  ?>  (team:title [our src]:bowl)
  :_  this
  :_  ~
  ?+  mark  ~|([%unexpected-mark mark] !!)
    %belt       (pass-session %$ %belt !<(belt:dill vase))
    %herm-task  (pass-session !<(task:herm vase))
  ==
::
++  on-peek
  |=  =path
  ^-  (unit (unit cage))
  ?+  path  ~
      [%x %sessions ~]
    :+  ~  ~
    :-  %json
    !>  ^-  json
    =-  a+(turn ~(tap in -) (lead %s))
    .^((set @tas) %dy /(scot %p our.bowl)//(scot %da now.bowl)/sessions)
  ==
::
++  on-leave  on-leave:def
::
++  on-agent  on-agent:def
++  on-fail   on-fail:def
--
