/+  tapp, stdio
=,  ethereum-types
=,  able:jael
=>  |%
    +$  app-state
      $:  %0
          dogs=(map path watchdog)
      ==
    ::
    +$  watchdog
      $:  config
          =number:block
          =pending-logs
          blocks=(list block)
      ==
    ::
    +$  config
      $:  url=@ta
          from-block=@ud
          to-block=(unit @ud)  ::TODO  use or remove
          contracts=(list address:ethereum)
          =topics
      ==
    ::
    +$  pending-logs  (map number:block loglist)
    +$  loglist       (list event-log:rpc:ethereum)
    ::
    +$  topics  (list ?(@ux (list @ux)))
    ::
    +$  context  [=path dog=watchdog]
    ::
    +$  peek-data
      [%atom =next-block=number:block]
    +$  in-poke-data
      $:  %eth-watcher-poke
          $%  [%watch =path =config]
              [%clear =path]
      ==  ==
    +$  out-poke-data  ~
    +$  in-peer-data   ~
    +$  out-peer-data
      $%  ::  %history: full event log history
          ::
          [%history loglist]
          ::  %log: newly added log
          ::
          [%log event-log:rpc:ethereum]
          ::  %disavow: forget logs
          ::
          ::    this is sent when a reorg happens that invalidates
          ::    previously-sent logs
          ::
          [%disavow id:block]
      ==
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
        ?~  full-file.client-response
          (pure:m ~)
        =/  body=@t  q.data.u.full-file.client-response
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
      |=  [url=@ta =hash:block contracts=(list address) =topics]
      =/  m  (async:stdio loglist)
      ^-  form:m
      ;<  =json  bind:m
        %+  request-rpc  url
        :*  `'logs by hash'
            %eth-get-logs-by-hash
            hash
            contracts
            topics
        ==
      %-  pure:m
      (parse-event-logs:rpc:ethereum json)
    ::
    ++  get-logs-by-range
      |=  $:  url=@ta
              contracts=(list address)
              =topics
              =from=number:block
              =to=number:block
          ==
      =/  m  (async:stdio loglist)
      ^-  form:m
      ;<  =json  bind:m
        %+  request-rpc  url
        :*  `'logs by range'
            %eth-get-logs
            `number+from-number
            `number+to-number
            contracts
            topics
        ==
      %-  pure:m
      (parse-event-logs:rpc:ethereum json)
    ::
    ++  send-logs
      |=  [=path =loglist]
      =/  m  (async:stdio ,~)
      |-  ^-  form:m
      =*  loop  $
      ?~  loglist
        (pure:m ~)
      ;<  ~  bind:m  (send-update path %log i.loglist)
      loop(loglist t.loglist)
    ::
    ++  send-update
      |=  [=path out=out-peer-data]
      =/  m  (async:stdio ,~)
      ^-  form:m
      =.  path  [%logs path]
      (give-result:stdio path out)
    --
::
::  Main loop
::
=>  |%
    ::
    ::  Update watchdog configuration, then look for updates
    ::
    ++  configure
      |=  [context =config]
      =/  m  (async:stdio ,watchdog)
      ^-  form:m
      %+  get-updates  path
      %_  dog
        -       config
        number  from-block.config
      ==
    ::
    ::  Get updates since last checked
    ::
    ++  get-updates
      |=  context
      =/  m  (async:stdio ,watchdog)
      ^-  form:m
      ;<  =latest=block  bind:m  (get-latest-block url.dog)
      ;<  dog=watchdog   bind:m  (zoom [path dog] number.id.latest-block)
      |-  ^-  form:m
      =*  loop  $
      ?:  (gth number.dog number.id.latest-block)
        ;<  now=@da  bind:m  get-time:stdio
        ::TODO  will set duplicate timers when multiple watchdogs, right?
        ;<  ~        bind:m  (wait-effect:stdio (add now ~s30))
        (pure:m dog)
      ;<  =block  bind:m  (get-block-by-number url.dog number.dog)
      ;<  [=new=pending-logs new-blocks=(lest ^block)]  bind:m
        (take-block [path dog] block)
      %_  loop
        pending-logs.dog  new-pending-logs
        blocks.dog        new-blocks
        number.dog        +(number.id.i.new-blocks)
      ==
    ::
    ::  Process a block, detecting and handling reorgs
    ::
    ++  take-block
      |=  [context =block]
      =/  m  (async:stdio ,[pending-logs (lest ^block)])
      ^-  form:m
      ?:  &(?=(^ blocks.dog) !=(parent-hash.block hash.id.i.blocks.dog))
        (rewind path url.dog pending-logs.dog block blocks.dog)
      ;<  =new=pending-logs  bind:m
        (release-old-events path pending-logs.dog number.id.block)
      ;<  =new=loglist  bind:m
        (get-logs-by-hash url.dog hash.id.block contracts.dog topics.dog)
      =.  new-pending-logs
        (~(put by new-pending-logs) number.id.block new-loglist)
      (pure:m new-pending-logs [block blocks.dog])
    ::
    ::  Release events if they're more than 30 blocks ago
    ::
    ++  release-old-events
      |=  [=path =pending-logs =number:block]
      =/  m  (async:stdio ,^pending-logs)
      ^-  form:m
      ?:  (lth number 30)  (pure:m pending-logs)
      =/  rel-number  (sub number 30)
      =/  =loglist  (~(get ja pending-logs) rel-number)
      ;<  ~  bind:m  (send-logs path loglist)
      (pure:m (~(del by pending-logs) rel-number))
    ::
    ::  Reorg detected, so rewind until we're back in sync
    ::
    ++  rewind
      |=  [=path url=@ta =pending-logs =block blocks=(list block)]
      =/  m  (async:stdio ,[^pending-logs (lest ^block)])
      |-  ^-  form:m
      =*  loop  $
      ?~  blocks
        (pure:m pending-logs block blocks)
      ?:  =(parent-hash.block hash.id.i.blocks)
        (pure:m pending-logs block blocks)
      ;<  =next=^block  bind:m  (get-block-by-number url number.id.i.blocks)
      ?:  =(~ pending-logs)
        ;<  ~  bind:m  (disavow path block)
        loop(block next-block, blocks t.blocks)
      =.  pending-logs  (~(del by pending-logs) number.id.block)
      loop(block next-block, blocks t.blocks)
    ::
    ::  Tell subscribers there was a deep reorg
    ::
    ++  disavow
      |=  [=path =block]
      =/  m  (async:stdio ,~)
      ^-  form:m
      (send-update path %disavow id.block)
    ::
    ::  Zoom forward to near a given block number.
    ::
    ::    Zooming doesn't go forward one block at a time.  As a
    ::    consequence, it cannot detect and handle reorgs.  Only use it
    ::    at a safe distance -- 500 blocks ago is probably sufficient.
    ::
    ++  zoom
      |=  [context =latest=number:block]
      =/  m  (async:stdio ,watchdog)
      ^-  form:m
      =/  zoom-margin=number:block  100
      ?:  (lth latest-number (add number.dog zoom-margin))
        (pure:m dog)
      =/  to-number=number:block  (sub latest-number zoom-margin)
      ;<  =loglist  bind:m
        %:  get-logs-by-range
          url.dog
          contracts.dog
          topics.dog
          number.dog
          to-number
        ==
      ;<  ~  bind:m  (send-logs path loglist)
      =.  number.dog  +(to-number)
      =.  blocks.dog  ~
      (pure:m dog)
    --
::
::  Main
::
=*  default-tapp  default-tapp:tapp
%-  create-tapp-all:tapp
^-  tapp-core-all:tapp
|_  [=bowl:gall state=app-state]
++  handle-init
  =/  m  tapp-async
  ^-  form:m
::
++  handle-diff  handle-diff:default-tapp
::
++  handle-poke
  |=  =in-poke-data
  =/  m  tapp-async
  ^-  form:m
  ?-  +<.in-poke-data
      %watch
    =/  dog=watchdog
      (~(gut by dogs.state) path.in-poke-data *watchdog)
    ;<  dog=watchdog  bind:m
      (configure [path.in-poke-data dog] config.in-poke-data)
    =.  dogs.state  (~(put by dogs.state) path.in-poke-data dog)
    (pure:m state)
  ::
      %clear
    ::TODO
    (pure:m state)
  ==
::
++  handle-take
  |=  =sign:tapp
  =/  m  tapp-async
  ^-  form:m
  ?+  -.sign  ~|([%strange-sign -.sign] !!)
      %wake
    ::TODO  ideally we'd process these in parallel. this seems possible,
    ::      but requires non-trivial work, as it deviates from tapp's flow.
    ::      (when making that change, take note of rpc request id's.)
    =/  dogs=(list [=path dog=watchdog])  ~(tap by dogs.state)
    |-  ^-  form:m
    =*  loop  $
    ?~  dogs
      (pure:m state)
    =,  i.dogs
    ;<  dog=watchdog  bind:m  (get-updates path dog)
    =.  dogs.state  (~(put by dogs.state) path dog)
    loop(dogs t.dogs)
  ==
::
++  handle-peer
  |=  =path
  =/  m  tapp-async
  ^-  form:m
  ::TODO
  (pure:m state)
::
::  +handle-peek: get diagnostics data
::
::    /block/some-path: get next block number to check for /some-path
::
++  handle-peek
  |=  =path
  ^-  (unit (unit peek-data))
  ?.  ?=([%x %block ^] path)  ~
  ?.  (~(has by dogs.state) t.t.path)  ~
  :+  ~  ~
  :-  %atom
  number:(~(got by dogs.state) t.t.path)
--
