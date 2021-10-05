/-  store=demo
/+  default-agent, verb, dbug, resource, agentio
|%
+$  card  card:agent:gall
+$  state-0
  [%0 log=(jar resource update:store) counters=(map resource @ud)]
--
=|  state-0
=*  state  -
::
%-  agent:dbug
%+  verb  |
^-  agent:gall
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
    io    ~(. agentio bowl)
++  on-init
  `this
::
++  on-save
  !>(state)
::
++  on-load
  |=  =vase
  =+  !<(old=state-0 vase)
  `this(state old)
::
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  ?.  =(%demo-update-0 mark)
    (on-poke:def mark vase)
  ~&  mark
  =+  !<(=update:store vase)
  |^ 
  =.  log
    (~(add ja log) p.update update)
  =^  cards  state
    (upd update)
  [cards this]
  ::
  ++  upd
    |=  up=update:store
    ^-  (quip card _state)
    ?-  -.up
      %ini  (upd-ini +.up)
      %add  (upd-add +.up)
      %sub  (upd-sub +.up)
      %run  (upd-run +.up)
    ==
  ::
  ++  upd-ini
   |=  [rid=resource ~]
   :-  (fact:io mark^!>([%ini +<]) /updates ~)^~
   state(counters (~(put by counters) rid 0))
  ::
  ++  upd-add
    |=  [rid=resource count=@ud]
    :-  (fact:io mark^!>([%add +<]) /updates ~)^~
    state(counters (~(jab by counters) rid (cury add count)))
  ::
  ++  upd-sub
    |=  [rid=resource count=@ud]
    :-  (fact:io mark^!>([%sub +<]) /updates ~)^~
    state(counters (~(jab by counters) rid (cury sub count)))
  ::
  ++  upd-run
    =|  cards=(list card)
    |=  [rid=resource =(list update:store)]
    ?~  list  [cards state]
    =^  caz  state  
      (upd i.list)
    $(list t.list, cards (weld cards caz))
  --
::
++  on-watch
  |=  =path
  ?.  ?=([%updates ~] path)
    (on-watch:def path)
  `this
::
++  on-peek  
  |=  =path
  ?.  ?=([%x %log @ @ @ ~] path)
    (on-peek:def path)
  =/  rid=resource
    (de-path:resource t.t.path)
  =/  =update:store
    [%run rid (flop (~(get ja log) rid))]
  ``noun+!>(update)
::
++  on-agent  on-agent:def
::
++  on-arvo  on-arvo:def
::
++  on-leave  on-leave:def
::
++  on-fail  on-fail:def
--
