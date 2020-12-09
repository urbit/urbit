/+  resource, default-agent, dbug
|%
+$  card  card:agent:gall
::
+$  state-0
  $:  %0
      waiting=(jar resource vase)
      inner-state=vase
  ==
::
+$  config
  [foreign=term =mark]
::
::  TODO: move to +zuse
++  append-ju
  |*  [a=(jar) b=* c=*]
  =/  d=(list _c)
    (~(get ja a) b)
  (~(put by a) b (snoc d c))
::
++  tail-ju
  |*  [a=(jar) b=*] 
  =/  c=(list _?>(?=(^ a) ?>(?=(^ q.n.a) i.q.n.a)))
    (~(get ja a) b)
  %+  ~(put by a)  b 
  ?~(c c t.c)
::
++  poke-proxy-hook
  $_  ^|
  |_  bowl:gall
  ::  +on-proxied-poke: process a proxied poke
  ++  on-proxied-poke
    |~  vase
    *[(list card) _^|(..on-init)]
  ::
  ++  resource-for-update
    |~  vase
    *(unit resource)
  ::  from agent:gall
  ::
  ++  on-init
    *[(list card) _^|(..on-init)]
  ::
  ++  on-save
    *vase
  ::
  ++  on-load
    |~  vase
    *[(list card) _^|(..on-init)]
  ::
  ++  on-poke
    |~  cage
    *[(list card) _^|(..on-init)]
  ::
  ++  on-watch
    |~  path
    *[(list card) _^|(..on-init)]
  ::
  ++  on-leave
    |~  path
    *[(list card) _^|(..on-init)]
  ::
  ++  on-peek
    |~  path
    *(unit (unit cage))
  ::
  ++  on-agent
    |~  [wire sign:agent:gall]
    *[(list card) _^|(..on-init)]
  ::
  ++  on-arvo
    |~  [wire sign-arvo]
    *[(list card) _^|(..on-init)]
  ::
  ++  on-fail
    |~  [term tang]
    *[(list card) _^|(..on-init)]
  --
++  agent
  |=  =config
  |=  =poke-proxy-hook
  %-  agent:dbug
  ^-  agent:gall
  =|  state-0
  =*  state  -
  =<
  |_  =bowl:gall
  +*  this  .
      def  ~(. (default-agent this %|) bowl)
      og   ~(. poke-proxy-hook bowl)
      hc   ~(. +> bowl)
      
  ++  on-init  
    :_  this
    =/  =wire
      (make-wire:hc /validator)
    =/  =rave:clay  [%sing %c [%da now.bowl] /[mark.config]/json]
    [%pass wire %arvo %c %warp our.bowl [%home `rave]]~
  ::
  ++  on-save
    =.  inner-state  on-save:og
    !>(state)
  ::
  ++  on-load
    |=  old=vase
    ^-  (quip card _this)
    `this(state !<(state-0 old))
  ::
  ++  on-poke
    |=  [=mark =vase]
    ?>  (team:title [our src]:bowl)
    ?.  =(mark mark.config)
      =^  cards  poke-proxy-hook
        (on-poke:og mark vase)
      [cards this]
    =/  rid=(unit resource)
      (resource-for-update:og vase)
    ?>  ?=(^ rid)
    =^  cards=(list card)  poke-proxy-hook
      (on-proxied-poke:og vase)
    =/  wire
      (make-wire:hc proxy+(en-path:resource u.rid))
    =/  =dock
      [entity.u.rid foreign.config]
    :_  this(waiting (append-ju waiting u.rid vase))
    ^-  (list card)
    %+  welp
      cards
    :~  [%pass wire %agent dock %poke mark vase]
        %+  give:hc  ~[/all]
        (frond:enjs:format %waiting !<(json (json-tube vase)))
    ==
  ::
  ++  on-watch
    |=  =path
    ^-  (quip card _this)
    ?>  (team:title [our src]:bowl)
    ?.  =(/all path)
      =^  cards  poke-proxy-hook
        (on-watch:og path)
      [cards this]
    =/  updates=(list vase)
      (zing ~(val by waiting))
    :_  this
    =-  (give:hc ~ -)^~
    :-  %a
    %+  turn  updates
    |=  =vase
    ^-  json
    !<(json (json-tube vase))
  ::
  ++  on-agent
    |=  [=wire =sign:agent:gall]
    ^-  (quip card _this)
    ?.  ?=([%helper %poke-proxy *] wire)
      =^  cards  poke-proxy-hook
        (on-agent:og wire sign)
      [cards this]
    ?+  t.t.wire  (on-agent:def wire sign)
      ::
        [%proxy *]
      ?.  ?=(%poke-ack -.sign)
        (on-agent wire sign)
      =/  rid=resource
        (de-path:resource t.t.t.wire)
      :_  this(waiting (tail-ju waiting rid))
      =-  (give:hc ~[/all] -)^~
      %-  frond:enjs:format
      :_  s+(enjs-path:resource rid)
      ?~(p.sign %ack %nack)
    ==
  ::
  ++  on-leave  on-leave:def
  ++  on-arvo  
    |=  [=wire =sign-arvo]
    ^-  (quip card _this)
    ?.  ?=([%helper %poke-proxy *] wire)
      =^  cards  poke-proxy-hook
        (on-arvo:og wire sign-arvo)
      [cards this]
    ?+  t.t.wire  (on-arvo:def wire sign-arvo)
      ::
        [%validator ~]
      :_  this
      =/  =rave:clay  [%next %b [%da now.bowl] /[mark.config]/json]
      [%pass wire %arvo %c %warp our.bowl [%home `rave]]~
    ==

  ++  on-fail   on-fail:def
  ++  on-peek   on-peek:def
  --
  ::
  |_  =bowl:gall
  ++  json-tube
    .^(tube:clay (scry /cc/[q.byk.bowl]/[mark.config]/json))
  ::
  ++  scry
    |=  =path
    ?>  ?=([* * *] path)
    %+  weld
      /[i.path]/(scot %p our.bowl)/[i.t.path]/(scot %da now.bowl) 
    t.t.path
  ::
  ++  give
    |=  [paths=(list path) jon=json]
    ^-  card
    =-  [%give %fact paths json+!>(-)]
    (frond:enjs:format dap.bowl jon)
  ::
  ++  make-wire
    |=  =wire
    ^+  wire
    (weld /helper/poke-proxy wire)

  --
    


--
