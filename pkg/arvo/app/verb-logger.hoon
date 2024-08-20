::  verb-logger: serializes verb-plus events to unix-side json
::
::    watches specified agents for "verb plus" events, buffers those, and
::    periodically (+write-interval) flushes them out to unix, under the
::    .urb/put/verb-logger/[agent] directory of the pier.
::
/+  verb, dbug, verb-json
::
|%
+$  state-0
  $:  %0
      events=(jar dude:gall event-plus:verb)
  ==
::
+$  card  card:agent:gall
::
++  write-interval  ~h1
::
++  write-events
  |=  [our=ship =dude:gall events=(list event-plus:verb)]
  ^-  (list card)
  ?:  =(~ events)  ~  ::NOTE  tmi
  =/  first=event-plus:verb
    (rear events)
  =/  pax=path
    /verb-logger/[dude]/(crip (a-co:co (unm:chrono:userlib now.first)))/json
  =/  vex=@
    (en:json:html (events:enjs our dude events))
  [%pass /write/[dude] %agent [our %hood] %poke %drum-put !>([pax vex])]~
::
++  ingest-event
  |=  $:  our=ship
          events=(jar dude:gall event-plus:verb)
          [=dude:gall event=event-plus:verb]
      ==
  ^-  (quip card _events)
  ?~  ves=(~(get ja events) dude)
    :-  ~
    (~(put by events) dude [event ~])
  ?:  .=  (sub now.i.ves (mod now.i.ves write-interval))
          (sub now.event (mod now.event write-interval))
    :-  ~
    (~(put by events) dude [event ves])
  :-  (write-events our dude ves)
  (~(put by events) dude [event ~])
::
++  enjs
  =,  enjs:format
  |%
  ++  events
    |=  [our=@p =dude:gall events=(list event-plus:verb)]  ::  latest-first
    =/  first=event-plus:verb  (rear events)
    %-  pairs
    :~  'ship'^s+(scot %p our)
        'dude'^s+dude
        'from'^(time now.first)
      ::
        :-  'events'
        :-  %a
        %+  roll  events  ::NOTE  we +roll to +turn & +flop simultaneously
        |=  [event=event-plus:verb out=(list json)]
        [(event:enjs:verb-json event) out]
    ==
  --
--
::
=|  state-0
=*  state  -
::
%+  verb  |
%-  agent:dbug
|_  =bowl:gall
+*  this  .
::
++  on-init
  ^-  (quip card _this)
  [~ this]
::
++  on-save
  !>(state)
::
++  on-load
  |=  ole=vase
  ^-  (quip card _this)
  [~ this(state !<(state-0 ole))]
::
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  ?>  =(%noun mark)
  ?+  q.vase  !!
      [%watch =dude:gall]
    =*  dude  dude.q.vase
    :_  this
    [%pass /log/[dude] %agent [our.bowl dude] %watch /verb/events-plus]~
  ::
      [%leave =dude:gall]
    =*  dude  dude.q.vase
    :-  :-  [%pass /log/[dude] %agent [our.bowl dude] %leave ~]
        (write-events our.bowl dude (~(get ja events) dude))
    this(events (~(del by events) dude))
  ::
      [%flush =dude:gall]
    |-
    =*  dude  dude.q.vase
    ?.  =(%$ dude)
      :-  (write-events our.bowl dude (~(get ja events) dude))
      this(events (~(del by events) dude))
    =|  cards=(list card)
    =/  dudes=(list dude:gall)  ~(tap in ~(key by events))
    |-  ^-  (quip card _this)
    ?~  dudes  [cards this]
    =^  caz  this  ^$(dude.q.vase i.dudes)
    =.  cards  (weld cards caz)
    $(dudes t.dudes)
  ==
::
++  on-agent
  |=  [=wire =sign:agent:gall]
  ^-  (quip card _this)
  ~|  wire
  ?+  wire  !!
      [%write @ ~]
    =*  dude  i.t.wire
    ?>  ?=(%poke-ack -.sign)
    ?~  p.sign  [~ this]
    %.  [~ this]
    %-  %*(. slog pri 3)
    [(cat 3 'verb-logger: lost export for %' dude) u.p.sign]
  ::
      [%log @ ~]
    =*  dude  i.t.wire
    ?-  -.sign
      %poke-ack   !!
      %kick       =-  [[-]~ this]
                  [%pass /log/[dude] %agent [our.bowl dude] %watch /verb/events-plus]
      %watch-ack  ?~  p.sign  [~ this]
                  %.  [~ this]
                  %-  %*(. slog pri 2)
                  [(cat 3 'verb-logger: failed verb watch for %' dude) u.p.sign]
      %fact       ?>  =(%verb-event-plus p.cage.sign)
                  =^  caz  events
                    %-  ingest-event
                    [our.bowl events dude !<(event-plus:verb q.cage.sign)]
                  [caz this]
    ==
  ==
::
++  on-fail
  |=  [=term =tang]
  ^-  (quip card _this)
  %.  [~ this]
  %-  %*(. slog pri 3)
  :_  tang
  (cat 3 'verb-logger: dropping the ball: ' term)
::
++  on-watch  |=(* !!)
++  on-leave  |=(* !!)
++  on-arvo   |=(* !!)
++  on-peek   |=(* ~)
--
