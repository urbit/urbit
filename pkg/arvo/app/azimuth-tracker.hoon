/+  tapp, stdio
=,  able:kale
=>  |%
    +$  pending-udiffs  (map number:block udiffs:point)
    +$  config
      $:  url=@ta
          from-number=number:block
      ==
    +$  app-state
      $:  url=@ta
          =number:block
          =pending-udiffs
          blocks=(list block)
      ==
    +$  peek-data  ~
    +$  in-poke-data
      $:  %azimuth-tracker-poke
          $%  [%init ~]
              [%listen whos=(set ship) =source:kale]
              [%watch =config]
          ==
      ==
    +$  out-poke-data  ~
    +$  in-peer-data   ~
    +$  out-peer-data  ~
    ++  tapp
      %:  ^tapp
        app-state
        peek-data
        in-poke-data
        out-poke-data
        in-peer-data
        out-peer-data
      ==
    ++  tapp-async  tapp-async:tapp
    ++  stdio  (^stdio out-poke-data out-peer-data)
    --
::
::  Async helpers
::
=>  |%
    ++  topics
      =>  azimuth-events:azimuth
      :_  ~
      :~  broke-continuity
          changed-keys
          lost-sponsor
          escape-accepted
      ==
    ::
    ++  request-rpc
      |=  [url=@ta id=(unit @t) req=request:rpc:ethereum]
      =/  m  (async:stdio ,json)
      ^-  form:m
      %+  (retry json)  `10
      =/  m  (async:stdio ,(unit json))
      ^-  form:m
      |^
      =/  =request:http
        :*  method=%'POST'
            url=url
            header-list=['Content-Type'^'application/json' ~]
            ^=  body
            %-  some  %-  as-octt:mimes:html
            %-  en-json:html
            (request-to-json:rpc:ethereum id req)
        ==
      ;<  ~  bind:m  (send-request:stdio request)
      ;<  rep=(unit client-response:iris)  bind:m
        take-maybe-response:stdio
      ?~  rep
        (pure:m ~)
      (parse-response u.rep)
      ::
      ++  parse-response
        |=  =client-response:iris
        =/  m  (async:stdio ,(unit json))
        ^-  form:m
        ?>  ?=(%finished -.client-response)
        =/  body=@t  q.data:(need full-file.client-response)
        =/  jon=(unit json)  (de-json:html body)
        ?~  jon
          (pure:m ~)
        =,  dejs-soft:format
        =/  array=(unit (list response:rpc:jstd))
          ((ar parse-one-response) u.jon)
        ?~  array
          =/  res=(unit response:rpc:jstd)  (parse-one-response u.jon)
          ?~  res
            (async-fail:stdio %request-rpc-parse-error >id< ~)
          ?:  ?=(%error -.u.res)
            (async-fail:stdio %request-rpc-error >id< >+.res< ~)
          ?.  ?=(%result -.u.res)
            (async-fail:stdio %request-rpc-fail >u.res< ~)
          (pure:m `res.u.res)
        (async-fail:stdio %request-rpc-batch >%not-implemented< ~)
        ::  (pure:m `[%batch u.array])
      ::
      ++  parse-one-response
        |=  =json
        ^-  (unit response:rpc:jstd)
        =/  res=(unit [@t ^json])
          %.  json
          =,  dejs-soft:format
          (ot id+so result+some ~)
        ?^  res  `[%result u.res]
        ~|  parse-one-response=json
        :+  ~  %error  %-  need
        %.  json
        =,  dejs-soft:format
        (ot id+so error+(ot code+no message+so ~) ~)
      --
    ::
    ++  retry
      |*  result=mold
      |=  [crash-after=(unit @ud) computation=_*form:(async:stdio (unit result))]
      =/  m  (async:stdio ,result)
      =|  try=@ud
      |^
      |-  ^-  form:m
      =*  loop  $
      ?:  =(crash-after `try)
        (async-fail:stdio %retry-too-many ~)
      ;<  ~                  bind:m  (backoff try ~m1)
      ;<  res=(unit result)  bind:m  computation
      ?^  res
        (pure:m u.res)
      loop(try +(try))
      ::
      ++  backoff
        |=  [try=@ud limit=@dr]
        =/  m  (async:stdio ,~)
        ^-  form:m
        ;<  eny=@uvJ  bind:m  get-entropy:stdio
        ;<  now=@da   bind:m  get-time:stdio
        %-  wait:stdio
        %+  add  now
        %+  min  limit
        ?:  =(0 try)  ~s0
        %+  add
          (mul ~s1 (bex (dec try)))
        (mul ~s0..0001 (~(rad og eny) 1.000))
      --
    ::
    ++  get-latest-block
      |=  url=@ta
      =/  m  (async:stdio ,block)
      ^-  form:m
      ;<  =json  bind:m  (request-rpc url `'block number' %eth-block-number ~)
      (get-block-by-number url (parse-eth-block-number:rpc:ethereum json))
    ::
    ++  get-block-by-number
      |=  [url=@ta =number:block]
      =/  m  (async:stdio ,block)
      ^-  form:m
      |^
      ;<  =json  bind:m
        (request-rpc url `'block by number' %eth-get-block-by-number number |)
      =/  =block  (parse-block json)
      ?.  =(number number.id.block)
        (async-fail:stdio %reorg-detected >number< >block< ~)
      (pure:m block)
      ::
      ++  parse-block
        |=  =json
        ^-  block
        =<  [[&1 &2] |2]
        ^-  [@ @ @]
        ~|  json
        %.  json
        =,  dejs:format
        %-  ot
        :~  hash+parse-hex-result:rpc:ethereum
            number+parse-hex-result:rpc:ethereum
            'parentHash'^parse-hex-result:rpc:ethereum
        ==
      --
    ::
    ++  get-logs-by-hash
      |=  [url=@ta =hash:block]
      =/  m  (async:stdio udiffs:point)
      ^-  form:m
      ;<  =json  bind:m
        %+  request-rpc  url
        :*  `'logs by hash'
            %eth-get-logs-by-hash
            hash
            ~[azimuth:contracts:azimuth]
            topics
        ==
      =/  event-logs=(list event-log:rpc:ethereum)
        (parse-event-logs:rpc:ethereum json)
      =/  =udiffs:point  (event-logs-to-udiffs event-logs)
      (pure:m udiffs)
    ::
    ++  get-logs-by-range
      |=  [url=@ta =from=number:block =to=number:block]
      =/  m  (async:stdio udiffs:point)
      ^-  form:m
      ;<  =json  bind:m
        %+  request-rpc  url
        :*  `'logs by range'
            %eth-get-logs
            `number+from-number
            `number+to-number
            ~[azimuth:contracts:azimuth]
            topics
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
        =+  ^-  [enc=octs aut=octs sut=@ud rev=@ud]
            %+  decode-results  data.event-log
            ~[[%bytes-n 32] [%bytes-n 32] %uint %uint]
        `[who id %keys rev sut (pass-from-eth:azimuth enc aut sut)]
      ?:  =(lost-sponsor i.topics.event-log)
        =+  ^-  [who=@ pos=@]
            (decode-topics t.topics.event-log ~[%uint %uint])
        `[who id %spon ~]
      ?:  =(escape-accepted i.topics.event-log)
        =+  ^-  [who=@ wer=@]
            (decode-topics t.topics.event-log ~[%uint %uint])
        `[who id %spon `wer]
      ~&  [%bad-topic event-log]
      ~
    ::
    ++  jael-update
      |=  =udiffs:point
      =/  m  (async:stdio ,~)
      |-  ^-  form:m
      =*  loop  $
      ?~  udiffs
        (pure:m ~)
      ~&  [%sending-event i.udiffs]
      ;<  ~  bind:m  (send-effect:stdio %new-event /ne i.udiffs)
      loop(udiffs t.udiffs)
    --
::
::  Main loop
::
=>  |%
    ::
    ::  Subscribe to %sources from kale
    ::
    ++  init
      |=  state=app-state
      =/  m  (async:stdio ,app-state)
      ^-  form:m
      ;<  ~  bind:m  (send-effect:stdio %sources /se ~)
      (pure:m state)
    ::
    ::  Send %listen to kale
    ::
    ++  listen
      |=  [state=app-state whos=(set ship) =source:kale]
      =/  m  (async:stdio ,app-state)
      ^-  form:m
      ;<  ~  bind:m  (send-effect:stdio %listen /lo whos source)
      (pure:m state)
    ::
    ::  Take %source from kale
    ::
    ++  take-source
      |=  [state=app-state whos=(set ship) =source:kale]
      =/  m  (async:stdio ,app-state)
      ^-  form:m
      ?:  ?=(%& -.source)
        (pure:m state)
      =/  a-purl=purl:eyre  node.p.source
      =.  url.state  (crip (en-purl:html a-purl))
      (watch state url.state launch:contracts:azimuth)  ::  8.069.650)
    ::
    ::  Start watching a node
    ::
    ++  watch
      |=  [state=app-state =config]
      =/  m  (async:stdio ,app-state)
      ^-  form:m
      =:  url.state             url.config
          number.state          from-number.config
          pending-udiffs.state  *pending-udiffs
          blocks.state          *(list block)
        ==
      (get-updates state)
    ::
    ::  Get updates since last checked
    ::
    ++  get-updates
      |=  state=app-state
      =/  m  (async:stdio ,app-state)
      ^-  form:m
      ~&  [%get-updates number.state]
      ;<  =latest=block      bind:m  (get-latest-block url.state)
      ;<  =new=number:block  bind:m  (zoom state number.id.latest-block)
      =.  number.state  new-number
      |-  ^-  form:m
      =*  walk-loop  $
      ~&  [%walk-loop number.state]
      ?:  (gth number.state number.id.latest-block)
        ;<  now=@da  bind:m  get-time:stdio
        ;<  ~        bind:m  (wait-effect:stdio (add now ~s10))
        (pure:m state)
      ;<  =block  bind:m  (get-block-by-number url.state number.state)
      ;<  [=new=pending-udiffs new-blocks=(lest ^block)]  bind:m
        (take-block url.state pending-udiffs.state block blocks.state)
      =:  pending-udiffs.state  new-pending-udiffs
          blocks.state          new-blocks
          number.state          +(number.id.i.new-blocks)
        ==
      walk-loop
    ::
    ::  Process a block, detecting and handling reorgs
    ::
    ++  take-block
      |=  [url=@ta =a=pending-udiffs =block blocks=(list block)]
      =/  m  (async:stdio ,[pending-udiffs (lest ^block)])
      ^-  form:m
      ~&  [%taking id.block]
      ?:  &(?=(^ blocks) !=(parent-hash.block hash.id.i.blocks))
        ~&  %rewinding
        (rewind url a-pending-udiffs block blocks)
      ;<  =b=pending-udiffs  bind:m
        (release-old-events a-pending-udiffs number.id.block)
      ;<  =new=udiffs:point  bind:m  (get-logs-by-hash url hash.id.block)
      ~?  !=(~ new-udiffs)  [%adding-diffs new-udiffs]
      =.  b-pending-udiffs  (~(put by b-pending-udiffs) number.id.block new-udiffs)
      (pure:m b-pending-udiffs block blocks)
    ::
    ::  Release events if they're more than 30 blocks ago
    ::
    ++  release-old-events
      |=  [=pending-udiffs =number:block]
      =/  m  (async:stdio ,^pending-udiffs)
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
      =/  m  (async:stdio ,[^pending-udiffs (lest ^block)])
      |-  ^-  form:m
      =*  loop  $
      ~&  [%wind block ?~(blocks ~ i.blocks)]
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
      =/  m  (async:stdio ,~)
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
      =/  m  (async:stdio ,number:block)
      ^-  form:m
      ?:  (lth latest-number (add number.state 500))
        (pure:m latest-number)
      =/  to-number=number:block  (sub latest-number 500)
      ;<  =udiffs:point  bind:m
        (get-logs-by-range url.state number.state to-number)
      ;<  ~  bind:m  (jael-update udiffs)
      (pure:m to-number)
    --
::
::  Main
::
=*  default-tapp  default-tapp:tapp
%-  create-tapp-poke-peer-take:tapp
|_  [=bowl:gall state=app-state]
++  handle-poke
  |=  =in-poke-data
  =/  m  tapp-async
  ^-  form:m
  ?-  +<.in-poke-data
    %init   (init state)
    %listen   (listen state +>.in-poke-data)
    %watch  (watch state +>.in-poke-data)
  ==
::
++  handle-take
  |=  =sign:tapp
  =/  m  tapp-async
  ^-  form:m
  ?+  -.sign  ~|([%strange-sign -.sign] !!)
    %source  (take-source state +.sign)
    %wake    (get-updates state)
  ==
::
++  handle-peer  ~(handle-peer default-tapp bowl state)
--
