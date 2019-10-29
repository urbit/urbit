::  eth-watcher: ethereum event log collector
::
/-  *eth-watcher
/+  tapp, stdio
=,  ethereum-types
=,  able:jael
=>  |%
    +$  app-state
      $:  %0
          dogs=(map path watchdog)
      ==
    ::
    +$  context  [=path dog=watchdog]
    +$  watchdog
      $:  config
          =number:block
          =pending-logs
          =history
          blocks=(list block)
      ==
    ::
    +$  history       (list loglist)
    +$  pending-logs  (map number:block loglist)
    ::
    +$  peek-data
      [%atom =next-block=number:block]
    +$  in-poke-data
      $:  %eth-watcher-poke
          poke
      ==
    +$  out-poke-data  ~
    +$  in-peer-data   ~
    +$  out-peer-data
      $:  %eth-watcher-diff
          diff
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
      |=  [=path =diff]
      =/  m  (async:stdio ,~)
      ^-  form:m
      =.  path  [%logs path]
      (give-result:stdio path %eth-watcher-diff diff)
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
        number  from.config
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
        (pure:m dog)
      ;<  =block  bind:m  (get-block-by-number url.dog number.dog)
      ;<  dog=watchdog  bind:m
        (take-block [path dog] block)
      loop(dog dog)
    ::
    ::  Process a block, detecting and handling reorgs
    ::
    ++  take-block
      |=  [context =block]
      =/  m  (async:stdio ,watchdog)
      ^-  form:m
      ::  if this next block isn't direct descendant of our logs, reorg happened
      ?:  &(?=(^ blocks.dog) !=(parent-hash.block hash.id.i.blocks.dog))
        (rewind [path dog] block)
      ;<  [=new=pending-logs =released=loglist]  bind:m
        (release-old-events path pending-logs.dog number.id.block)
      ;<  =new=loglist  bind:m
        (get-logs-by-hash url.dog hash.id.block contracts.dog topics.dog)
      =.  new-pending-logs
        (~(put by new-pending-logs) number.id.block new-loglist)
      %-  pure:m
      %_  dog
        number        +(number.id.block)
        pending-logs  new-pending-logs
        history       [released-loglist history.dog]
        blocks        [block blocks.dog]
      ==
    ::
    ::  Release events if they're more than 30 blocks ago
    ::
    ++  release-old-events
      |=  [=path =pending-logs =number:block]
      =/  m  (async:stdio ,[^pending-logs loglist])
      ^-  form:m
      ?:  (lth number 30)  (pure:m pending-logs ~)
      =/  rel-number  (sub number 30)
      =/  =loglist  (~(get ja pending-logs) rel-number)
      ;<  ~  bind:m  (send-logs path loglist)
      (pure:m (~(del by pending-logs) rel-number) loglist)
    ::
    ::  Reorg detected, so rewind until we're back in sync
    ::
    ++  rewind
      ::  block: wants to be head of blocks.dog, but might not match
      |=  [context =block]
      =/  m  (async:stdio ,watchdog)
      =*  blocks  blocks.dog
      |-  ^-  form:m
      =*  loop  $
      ::  if we have no further history to rewind, we're done
      ?~  blocks
        (pure:m dog(blocks [block blocks]))
      ::  if target block is directly after "latest", we're done
      ?:  =(parent-hash.block hash.id.i.blocks)
        (pure:m dog(blocks [block blocks]))
      ::  next-block: the new target block
      ;<  =next=^block  bind:m
        (get-block-by-number url.dog number.id.i.blocks)
      ::  remove from either pending-logs or history
      ?:  =(~ pending-logs.dog)
        ::  if no more pending logs, start deleting from history instead
        ::NOTE  this assumes there's one history entry per item in blocks.
        ::      while +zoom breaks that assumption by clearing blocks, we won't
        ::      run out of history before running out of blocks, allowing us to
        ::      skip the =(number.id.block number.id.i.i.history) check.
        ?~  history.dog
          loop(block next-block, blocks t.blocks)
        ;<  ~  bind:m
          ::  don't bother sending a disavow if there were no logs there
          ?~  i.history.dog  (pure:(async:stdio ,~) ~)
          (disavow path block)
        loop(block next-block, blocks t.blocks, history.dog t.history.dog)
      =.  pending-logs.dog
        (~(del by pending-logs.dog) number.id.block)
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
      =.  history.dog  [loglist history.dog]
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
  ::  start update timer loop
  ;<  now=@da  bind:m  get-time:stdio
  ;<  ~  bind:m  (wait-effect:stdio (add now ~m5))
  (pure:m state)
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
    ;<  ~  bind:m
      ;<  now=@da  bind:(async:tapp ,~)  get-time:stdio
      =/  next=@da  (add now ~m5)
      ::NOTE  we use +send-raw-card here to ensure we always set a new timer,
      ::      regardless of what happens further on in the flow.
      (send-raw-card:stdio %wait /effect/(scot %da next) next)
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
::  +handle-peer: subscribe & get initial subscription data
::
::    /logs/some-path:
::
++  handle-peer
  |=  =path
  =/  m  tapp-async
  ^-  form:m
  ?.  ?=([%logs ^] path)
    ~|  [%invalid-subscription-path path]
    !!
  ;<  ~  bind:m
    %+  send-effect-on-bone:stdio  ost.bowl
    :+  %diff  %eth-watcher-diff
    :-  %history
    ^-  loglist
    ~|  [%no-such-watchdog t.path]
    (zing history:(~(got by dogs.state) t.path))
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
