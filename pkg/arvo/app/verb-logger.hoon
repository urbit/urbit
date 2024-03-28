::  verb-logger: serializes verb-plus events to unix-side json
::
::    watches specified agents for "verb plus" events, buffers those, and
::    periodically (+write-interval) flushes them out to unix, under the
::    .urb/put/verb-logger/[agent] directory of the pier.
::
/+  verb, dbug
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
        [(event:enjs event) out]
    ==
  ++  event
    |=  event-plus:verb
    %-  pairs
    :~  'act'^(numb act)
        'now'^(time now)  ::  ms timestamp, lossy-ness is fine here
        'src'^s+(scot %p src)
        'kind'^s+-.cause
        'deets'^(^cause cause)
        'effects'^a+(turn effects effect)
    ==
  ::
  ++  cause
    |=  =cause:verb
    ^-  json
    ?-  -.cause
      %on-init   b+&
      %on-load   b+&
      %on-poke   (pairs 'mark'^s+mark.cause 'mug'^(mug mug.cause) ~)
      %on-watch  (path path.cause)
      %on-leave  (path path.cause)
      %on-agent  %-  pairs
                 :~  'wire'^(path wire.cause)
                     'sign'^s+-.sign.cause
                   ::
                     :-  'deets'
                     ?-  -.sign.cause
                       %poke-ack   b+ack.sign.cause
                       %watch-ack  b+ack.sign.cause
                       %kick       ~
                       %fact       %-  pairs
                                   :~  'mark'^s+mark.sign.cause
                                       'mug'^(mug mug.sign.cause)
                                   ==
                     ==
                 ==
      %on-arvo   %-  pairs
                 :~  'wire'^(path wire.cause)
                     'vane'^s+vane.cause
                     'sign'^s+sign.cause
                 ==
      %on-fail   s+term.cause
    ==
  ::
  ++  effect
    |=  effect:verb
    ^-  json
    %-  pairs
    :-  'kind'^s++<-
    :_  ~
    :-  'deets'
    %-  pairs
    ^-  (list [@t json])
    ?-  +<-
      %poke   :~  'wire'^(path wire)
                  'gill'^(^gill gill)
                  'mark'^s+mark
                  'mug'^(^mug mug)
              ==
      %watch  ~['wire'^(^path wire) 'gill'^(^gill gill) 'path'^(^path path)]
      %leave  ~['wire'^(path wire) 'gill'^(^gill gill)]
      %fact   ~['paths'^a+(turn paths path) 'mark'^s+mark 'mug'^(^mug mug)]
      %kick   ~['paths'^a+(turn paths path)]
      %arvo   ~['wire'^(path wire) 'vane'^s+vane 'task'^s+task]
    ==
  ::
  ++  gill  |=(=gill:gall `json`s+(rap 3 (scot %p p.gill) '/' q.gill ~))
  ++  mug   |=(mug=@ux `json`s+(crip ((x-co:co 8) mug)))
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
    =*  dude  dude.q.vase
    :-  (write-events our.bowl dude (~(get ja events) dude))
    this(events (~(del by events) dude))
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
