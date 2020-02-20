/-  spider
/+  strandio, *azimuth
=,  strand=strand:spider
=,  able:jael
|%
+$  pending-udiffs  (map number:block udiffs:point)
+$  app-state
  $:  %2
      url=@ta
      =number:block
      =pending-udiffs
      blocks=(list block)
      whos=(set ship)
  ==
+$  in-poke-data
  $%  [%listen whos=(list ship) =source:jael]
      [%watch url=@ta]
  ==
+$  in-peer-data   ~
--
::
::  Async helpers
::
|%
++  topics
  |=  ships=(set ship)
  ^-  (list ?(@ux (list @ux)))
  ::  The first topic should be one of these event types
  ::
  :-  =>  azimuth-events:azimuth
      :~  broke-continuity
          changed-keys
          lost-sponsor
          escape-accepted
      ==
  ::  If we're looking for a specific set of ships, specify them as
  ::  the second topic.  Otherwise don't specify the second topic so
  ::  we will match all ships.
  ::
  ?:  =(~ ships)
    ~
  [(turn ~(tap in ships) ,@) ~]
::
++  get-logs-by-hash
  |=  [url=@ta whos=(set ship) =hash:block]
  =/  m  (strand udiffs:point)
  ^-  form:m
  ;<  =json  bind:m
    %+  request-rpc  url
    :*  `'logs by hash'
        %eth-get-logs-by-hash
        hash
        ~[azimuth:contracts:azimuth]
        (topics whos)
    ==
  =/  event-logs=(list event-log:rpc:ethereum)
    (parse-event-logs:rpc:ethereum json)
  =/  =udiffs:point  (event-logs-to-udiffs event-logs)
  (pure:m udiffs)
::
++  get-logs-by-range
  |=  [url=@ta whos=(set ship) =from=number:block =to=number:block]
  =/  m  (strand udiffs:point)
  ^-  form:m
  ;<  =json  bind:m
    %+  request-rpc  url
    :*  `'logs by range'
        %eth-get-logs
        `number+from-number
        `number+to-number
        ~[azimuth:contracts:azimuth]
        (topics whos)
    ==
  =/  event-logs=(list event-log:rpc:ethereum)
    (parse-event-logs:rpc:ethereum json)
  =/  =udiffs:point  (event-logs-to-udiffs event-logs)
  (pure:m udiffs)
::
++  event-logs-to-udiffs
  |=  event-logs=(list =event-log:rpc:ethereum)
  ^-  =udiffs:point
  %+  murn  event-logs
  |=  =event-log:rpc:ethereum
  ^-  (unit [=ship =udiff:point])
  ?~  mined.event-log
    ~
  ?:  removed.u.mined.event-log
    ~&  [%removed-log event-log]
    ~
  =/  =id:block  [block-hash block-number]:u.mined.event-log
  =,  azimuth-events:azimuth
  =,  abi:ethereum
  ?:  =(broke-continuity i.topics.event-log)
    =/  who=@  (decode-topics t.topics.event-log ~[%uint])
    =/  num=@  (decode-results data.event-log ~[%uint])
    `[who id %rift num]
  ?:  =(changed-keys i.topics.event-log)
    =/  who=@  (decode-topics t.topics.event-log ~[%uint])
    =/  [enc=octs aut=octs sut=@ud rev=@ud]
        %+  decode-results  data.event-log
        ~[[%bytes-n 32] [%bytes-n 32] %uint %uint]
    `[who id %keys rev sut (pass-from-eth:azimuth enc aut sut)]
  ?:  =(lost-sponsor i.topics.event-log)
    =/  [who=@ pos=@]
        (decode-topics t.topics.event-log ~[%uint %uint])
    `[who id %spon ~]
  ?:  =(escape-accepted i.topics.event-log)
    =/  [who=@ wer=@]
        (decode-topics t.topics.event-log ~[%uint %uint])
    `[who id %spon `wer]
  ~&  [%bad-topic event-log]
  ~
::
++  jael-update
  |=  =udiffs:point
  =/  m  (strand ,~)
  |-  ^-  form:m
  =*  loop  $
  ?~  udiffs
    (pure:m ~)
  =/  =path  /(scot %p ship.i.udiffs)
  =/  cards
    :~  [%give %fact ~[/] %azimuth-udiff !>(i.udiffs)]
        [%give %fact ~[path] %azimuth-udiff !>(i.udiffs)]
    ==
  ;<  ~  bind:m  (send-raw-cards:strandio cards)
  loop(udiffs t.udiffs)
::
++  handle-azimuth-tracker-poke
  =/  m  (strand ,in-poke-data)
  ^-  form:m
  ;<  =vase  bind:m
    ((handle:strandio ,vase) (take-poke:strandio %azimuth-tracker-poke))
  =/  =in-poke-data  !<(in-poke-data vase)
  (pure:m in-poke-data)
--
::
::  Main loop
::
|%
::
::  Switch eth node
::
++  handle-watch
  |=  state=app-state
  =/  m  (strand ,app-state)
  ^-  form:m
  ;<  =in-poke-data  bind:m  handle-azimuth-tracker-poke
  ?.  ?=(%watch -.in-poke-data)
    ignore:strandio
  (pure:m state(url url.in-poke-data))
::
::  Send %listen to jael
::
++  handle-listen
  |=  state=app-state
  =/  m  (strand ,app-state)
  ^-  form:m
  ;<  =in-poke-data  bind:m  handle-azimuth-tracker-poke
  ?.  ?=(%listen -.in-poke-data)
    ignore:strandio
  =/  card
    [%pass /lo %arvo %j %listen (silt whos.in-poke-data) source.in-poke-data]
  ;<  ~  bind:m  (send-raw-card:strandio card)
  (pure:m state)
::
::  Start watching a node
::
++  handle-peer
  |=  state=app-state
  =/  m  (strand ,app-state)
  ;<  =path  bind:m  ((handle:strandio ,path) take-watch:strandio)
  =:    number.state          0
        pending-udiffs.state  *pending-udiffs
        blocks.state          *(list block)
        whos.state
      =/  who=(unit ship)  ?~(path ~ `(slav %p i.path))
      ?~  who
        ~
      (~(put in whos.state) u.who)
    ==
  ::
  ;<  ~      bind:m  send-cancel-request:strandio
  (get-updates state)
::
::  Get more blocks
::
++  handle-wake
  |=  state=app-state
  =/  m  (strand ,app-state)
  ^-  form:m
  ;<  ~  bind:m  ((handle:strandio ,~) (take-wake:strandio ~))
  (get-updates state)
::
::  Get updates since last checked
::
++  get-updates
  |=  state=app-state
  =/  m  (strand ,app-state)
  ^-  form:m
  ;<  =latest=block    bind:m  (get-latest-block url.state)
  ;<  state=app-state  bind:m  (zoom state number.id.latest-block)
  |-  ^-  form:m
  =*  walk-loop  $
  ?:  (gth number.state number.id.latest-block)
    ;<  now=@da  bind:m  get-time:strandio
    ;<  ~        bind:m  (send-wait:strandio (add now ~m5))
    (pure:m state)
  ;<  =block  bind:m  (get-block-by-number url.state number.state)
  ;<  [=new=pending-udiffs new-blocks=(lest ^block)]  bind:m
    %-  take-block
    [url.state whos.state pending-udiffs.state block blocks.state]
  =:  pending-udiffs.state  new-pending-udiffs
      blocks.state          new-blocks
      number.state          +(number.id.i.new-blocks)
    ==
  walk-loop
::
::  Process a block, detecting and handling reorgs
::
++  take-block
  |=  [url=@ta whos=(set ship) =a=pending-udiffs =block blocks=(list block)]
  =/  m  (strand ,[pending-udiffs (lest ^block)])
  ^-  form:m
  ?:  &(?=(^ blocks) !=(parent-hash.block hash.id.i.blocks))
    (rewind url a-pending-udiffs block blocks)
  ;<  =b=pending-udiffs  bind:m
    (release-old-events a-pending-udiffs number.id.block)
  ;<  =new=udiffs:point  bind:m  (get-logs-by-hash url whos hash.id.block)
  =.  b-pending-udiffs  (~(put by b-pending-udiffs) number.id.block new-udiffs)
  (pure:m b-pending-udiffs block blocks)
::
::  Release events if they're more than 30 blocks ago
::
++  release-old-events
  |=  [=pending-udiffs =number:block]
  =/  m  (strand ,^pending-udiffs)
  ^-  form:m
  =/  rel-number  (sub number 30)
  =/  =udiffs:point  (~(get ja pending-udiffs) rel-number)
  ;<  ~  bind:m  (jael-update udiffs)
  (pure:m (~(del by pending-udiffs) rel-number))
::
::  Reorg detected, so rewind until we're back in sync
::
++  rewind
  |=  [url=@ta =pending-udiffs =block blocks=(list block)]
  =/  m  (strand ,[^pending-udiffs (lest ^block)])
  |-  ^-  form:m
  =*  loop  $
  ?~  blocks
    (pure:m pending-udiffs block blocks)
  ?:  =(parent-hash.block hash.id.i.blocks)
    (pure:m pending-udiffs block blocks)
  ;<  =next=^block  bind:m  (get-block-by-number url number.id.i.blocks)
  ?:  =(~ pending-udiffs)
    ;<  ~  bind:m  (disavow block)
    loop(block next-block, blocks t.blocks)
  =.  pending-udiffs  (~(del by pending-udiffs) number.id.block)
  loop(block next-block, blocks t.blocks)
::
::  Tell subscribers there was a deep reorg
::
++  disavow
  |=  =block
  =/  m  (strand ,~)
  ^-  form:m
  (jael-update [*ship id.block %disavow ~]~)
::
::  Zoom forward to near a given block number.
::
::    Zooming doesn't go forward one block at a time.  As a
::    consequence, it cannot detect and handle reorgs.  Only use it
::    at a safe distance -- 500 blocks ago is probably sufficient.
::
++  zoom
  |=  [state=app-state =latest=number:block]
  =/  m  (strand ,app-state)
  ^-  form:m
  =/  zoom-margin=number:block  100
  ?:  (lth latest-number (add number.state zoom-margin))
    (pure:m state)
  =/  to-number=number:block  (sub latest-number zoom-margin)
  ;<  =udiffs:point  bind:m
    (get-logs-by-range url.state whos.state number.state to-number)
  ;<  ~  bind:m  (jael-update udiffs)
  =.  number.state  +(to-number)
  =.  blocks.state  ~
  (pure:m state)
--
::
::  Main
::
^-  thread:spider
|=  args=vase
=/  m  (strand ,vase)
^-  form:m
;<  ~  bind:m
  %-  (main-loop:strandio ,app-state)
  :~  handle-listen
      handle-watch
      handle-wake
      handle-peer
  ==
(pure:m *vase)
