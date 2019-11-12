::  Run an imp and receive the response in +on-watch
::
/-  spider
/+  verb
|%
++  poke-spider
  |=  [=wire our=@p =cage]
  ^-  card:agent:mall
  [%pass [%run-imp %running wire] %agent [our %spider] %poke cage]
::
++  watch-spider
  |=  [=wire our=@p =sub=path]
  ^-  card:agent:mall
  [%pass [%run-imp %running wire] %agent [our %spider] %watch sub-path]
--
|=  [our=ship =the=wire =start-args:spider =agent:mall]
^-  (quip card:agent:mall agent:mall)
:-  [%pass [%run-imp %starting the-wire] %agent [our %spider] %watch /next-iid]~
::  If null, we're trying to get an id.  Else, it's running and we're
::  waiting for the result.
::
=|  running=(unit =iid:spider)
%+  verb  &
^-  agent:mall
|_  =bowl:mall
+*  this  .
    ag    ~(. agent bowl)
::
++  on-init
  ^-  (quip card:agent:mall agent:mall)
  =^  cards  agent  on-init:ag
  [cards this]
::
++  on-save
  ^-  vase
  !>([on-save:ag the-wire running])
::
++  on-load
  |=  old-state=vase
  ^-  (quip card:agent:mall agent:mall)
  ~&  >  %old-1
  =+  !<([old=vase =old=wire =old=_running] old-state)
  ~&  >  %old-2
  =.  the-wire  old-wire
  =.  running   old-running
  =^  cards  agent  (on-load:ag old)
  [cards this]
::
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card:agent:mall agent:mall)
  =^  cards  agent  (on-poke:ag mark vase)
  [cards this]
::
++  on-watch
  |=  =path
  ^-  (quip card:agent:mall agent:mall)
  =^  cards  agent  (on-watch:ag path)
  [cards this]
::
++  on-leave
  |=  =path
  ^-  (quip card:agent:mall agent:mall)
  =^  cards  agent  (on-leave:ag path)
  [cards this]
::
++  on-peek
  |=  =path
  ^-  (unit (unit cage))
  (on-peek:ag path)
::
++  on-agent
  |=  [=wire =sign:agent:mall]
  ^-  (quip card:agent:mall agent:mall)
  ?:  =([%run-imp %starting the-wire] wire)
    ?+    -.sign  ~|(%run-imp-strange-sign !!)
        %watch-ack
      ?~  p.sign
        [~ this]
      %-  (slog leaf+"eth-watcher failed to get iid" u.p.sign)
      [~ agent]
    ::
        %kick
      ?^  running
        [~ this]
      %-  (slog leaf+"run-imp couldn't get iid" ~)
      [~ agent] 
    ::
        %fact
      ?>  ?=(%iid p.cage.sign)
      =+  !<(=new=iid:spider q.cage.sign)
      ::  already running imp
      ::
      ?^  running
        [~ this]
      :_  this(running `new-iid)
      :~  (watch-spider the-wire our.bowl /imp-result/[new-iid])
          (poke-spider the-wire our.bowl %spider-start !>(start-args(use `new-iid)))
      ==
    ==
  ?:  =([%run-imp %running the-wire] wire)
    ?-    -.sign
        %poke-ack
      ?~  p.sign
        [~ this]
      %-  (slog leaf+"eth-watcher couldn't start imp" u.p.sign)
      [~ agent]
    ::
        %watch-ack
      ?~  p.sign
        [~ this]
      %-  (slog leaf+"eth-watcher couldn't start listening to imp" u.p.sign)
      [~ agent]
    ::
        %kick
      [~ agent]
    ::
        %fact
      =^  cards  agent  (on-agent:ag the-wire sign)
      [cards this]
    ==
  =^  cards  agent  (on-agent:ag wire sign)
  [cards this]
::
++  on-arvo
  |=  [=wire =sign-arvo]
  ^-  (quip card:agent:mall agent:mall)
  =^  cards  agent  (on-arvo:ag wire sign-arvo)
  [cards this]
::
++  on-fail
  |=  [=term =tang]
  ^-  (quip card:agent:mall agent:mall)
  =^  cards  agent  (on-fail:ag term tang)
  [cards this]
--
