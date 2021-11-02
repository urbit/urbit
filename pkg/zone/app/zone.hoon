/-  pals, zone
/+  agentio, default-agent, dbug, verb
|%
+$  card  card:agent:gall
+$  state-0
  [%0 ships=(map ship @t)]
--
=|  state-0
=*  state  -
=<
  %-  agent:dbug
  %+  verb  |
  |_  =bowl:gall
  +*  this  .
      wc    ~(. +> [bowl ~])
      def   ~(. (default-agent this %|) bowl)
  ::
  ++  on-init  
    :_  this
    :~  ~(watch pal:wc %targets)
        ~(watch pal:wc %leeches)
    ==
  ::
  ++  on-load
    |=  =vase
    =+  !<(old=state-0 vase)
    `this(state old)
  ::
  ++  on-save  !>(state)
  ::
  ++  on-poke
    |=  [=mark =vase]
    =^  cards  state
      abet:(poke:wc mark vase)
    [cards this]
  ::
  ++  on-watch
    |=  =path
    =^  cards  state
      abet:(watch:wc path)
    [cards this]
  ::
  ++  on-peek  peek:wc
  ::
  ++  on-agent
    |=  [=wire =sign:agent:gall]
    =^  cards  state
      abet:(agent:wc wire sign)
    [cards this]
  ::
  ++  on-arvo  on-arvo:def
  ::
  ++  on-fail  on-fail:def
  ::
  ++  on-leave  on-leave:def
  --
|_  [=bowl:gall cards=(list card)]
++  wh-core  .
++  io  ~(. agentio bowl)
++  pass  pass:io
++  emit  |=(=card wh-core(cards [card cards]))
++  abet
  [(flop cards) state]
++  poke
  |=  [=mark =vase]
  ?>  =(our.bowl src.bowl)
  ?+  mark  ~|(%bad-mark-poke !!)
    %zone-action  (action:here !<(action:zone vase))
  ==
::
++  watch
  |=  =path
  ?+  path  ~|(%nonexistent-path !!)
    [%zone ~]  (emit init:give:here)
  ==
::
++  peek
  |=  =path
  ^-  (unit (unit cage))
  ?+  path  [~ ~]
    [%x %ships ~]  ``zone-update+!>([%ini ships])
  ==
::
++  agent
  |=  [=wire =sign:agent:gall]
  ?+  wire  ~|(nonexistent-wire/wire !!)
    [%ships @ ~]  (take:(from-wire:there wire) sign) 
  ::
      [%pals @ ~]
    ?>  ?=(?(%leeches %targets) i.t.wire)
    (~(take pal i.t.wire) sign)
  ==
::  core for manipulating our zone
++  here
  |% 
  ++  give
    |%
    ++  act  |=(a=action:zone zone-action+!>(a))
    ++  us  
      %-  act
      ?~  zon=(~(get by ships) our.bowl)
        [%del our.bowl ~]
      [%add our.bowl u.zon]
    ++  init  (fact-init:io us)
    ++  diff  (fact:io us /zone ~)
    ++  kick  (kick:io /zone ~)
    --
  ::
  ++  part
    |=  =ship
    (emit (kick-only:io ship /zone ~))
  ++  action
    |=  =action:zone
    =.  ships
      ?-  -.action
        %add  (~(put by ships) our.bowl q.action)
        %del  (~(del by ships) our.bowl)
      ==
    (emit diff:give)
  --
++  there
  |_  =ship
  ++  there  .
  ++  wire  /ships/(scot %p ship)
  ++  from-wire  
    |=  wir=^wire
    ?>  ?=([%ships @ ~] wir)
    there(ship (slav %p i.t.wir))
  ++  give
    |%
    ++  diff  (fact:io ship+!>(ship) wire ~)
    --
  ++  pass
    |%
    ++  p  ~(. ^pass wire)
    ++  watch  (watch-clone:p ship /zone)
    ++  leave  (leave-clone:p ship)
    --
  ::
  ++  watching  (~(has by wex.bowl) [wire ship dap.bowl])
  ++  mutuals  .^(? %gx (scry:io %pals /mutuals//(scot %p ship)/noun))
  ::
  ++  action
    |=  =action:zone
    =.  ship  p.action
    =.  ships
      ?-  -.action
        %add  (~(put by ships) [p q]:action)
        %del  (~(del by ships) p.action)
      ==
    (emit diff:give)
  ::
  ++  meet  (emit watch:pass)
  ++  part  
    =.  wh-core  (part:here ship)
    =.  wh-core  (action %del ship ~)
    (emit leave:pass)
  ::
  ++  take
    |=  =sign:agent:gall
    ?+  -.sign  !!
      %watch-ack  ?~(p.sign wh-core (action %del ship ~))
      %kick       (emit watch:pass)
    ::
        %fact
      ?>  =(%zone-action p.cage.sign)
      (action !<(action:zone q.cage.sign))
    ==
  --
::
++  pal
  |_  kind=?(%targets %leeches)
  ++  wire  /pals/[kind]
  ++  watch  (~(watch-our pass wire) %pals /[kind])
  ::
  ++  hcf  |=(=tang ((slog leaf/"Unexpected error in {<wire>}" tang) wh-core))
  ++  take
    |=  =sign:agent:gall
    ?+  -.sign  !!
      %watch-ack  ?^(p.sign (hcf u.p.sign) wh-core)
      %kick      (emit watch)
    ::
        %fact
      ?.  =(%pals-effect p.cage.sign)  wh-core
      (effect !<(effect:pals q.cage.sign))
    ==
  ::
  ++  effect
    |=  =effect:pals
    =*  her  ~(. there ship.effect)
    ?-  -.effect
      ?(%part %away)  part:her
    ::
        ?(%meet %near)
      ?:  watching:her  wh-core
      ?.  mutuals:her   wh-core 
      meet:her
    ==
  --
--
