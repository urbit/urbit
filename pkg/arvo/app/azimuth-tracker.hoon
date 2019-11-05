/-  eth-watcher
/+  tapp, stdio
=,  able:jael
=>  |%
    +$  app-state
      $:  %3
          url=@ta
          whos=(set ship)
      ==
    +$  peek-data  ~
    +$  in-poke-data
      $:  %azimuth-tracker-poke
          $%  ::  %listen
              ::
              [%listen whos=(list ship) =source:jael]
              ::  %watch: configure node url
              ::
              [%watch url=@ta]
          ==
      ==
    +$  out-poke-data
      $:  %eth-watcher-poke
          poke:eth-watcher
      ==
    +$  in-peer-data
      $:  %eth-watcher-diff
          diff:eth-watcher
      ==
    +$  out-peer-data
      [%azimuth-udiff =ship =udiff:point]
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
      =/  =path  /(scot %p ship.i.udiffs)
      ;<  ~  bind:m  (give-result:stdio / %azimuth-udiff i.udiffs)
      ;<  ~  bind:m  (give-result:stdio path %azimuth-udiff i.udiffs)
      loop(udiffs t.udiffs)
    --
::
::  Main loop
::
=>  |%
    ::
    ::  Send %listen to jael
    ::
    ++  listen
      |=  [state=app-state whos=(list ship) =source:jael]
      =/  m  (async:stdio ,app-state)
      ^-  form:m
      ;<  ~  bind:m  (send-effect:stdio %listen /lo (silt whos) source)
      (pure:m state)
    ::
    ::  Start watching a node
    ::
    ++  start
      |=  [state=app-state our=ship dap=term]
      =/  m  (async:stdio ,app-state)
      ^-  form:m
      ;<  ~  bind:m
        %+  poke-app:stdio
          [our %eth-watcher]
        :+  %eth-watcher-poke  %watch
        :-  /[dap]
        :*  url.state
            launch:contracts:azimuth
            ~[azimuth:contracts:azimuth]
            (topics whos.state)
        ==
      (pure:m state)
    ::
    ::  +history: Tell subscribers about many changes
    ::
    ++  history
      |=  =loglist:eth-watcher
      =/  m  (async:stdio ,~)
      |-  ^-  form:m
      %-  jael-update
      (event-logs-to-udiffs loglist)
    ::
    ::  +log: Tell subscribers about a new change
    ::
    ++  log
      |=  =event-log:rpc:ethereum
      =/  m  (async:stdio ,~)
      (history [event-log ~])
    ::
    ::  +disavow: Tell subscribers there was a deep reorg
    ::
    ++  disavow
      |=  =id:block
      =/  m  (async:stdio ,~)
      ^-  form:m
      (jael-update [*ship id %disavow ~]~)
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
  ::  set up subscription once, listen forever
  ::
  ;<  ~  bind:m
    %+  peer-app:stdio
      [our.bowl %eth-watcher]
    /logs/[dap.bowl]
  (pure:m state)
::
++  handle-peek  handle-peek:default-tapp
++  handle-take  handle-take:default-tapp
::
++  handle-poke
  |=  in=in-poke-data
  =/  m  tapp-async
  ^-  form:m
  ?-  +<.in
    %listen  (listen state +>.in)
    %watch   (start state(url url.in) [our dap]:bowl)
  ==
::
++  handle-diff
  |=  [=dock =path in=in-peer-data]
  =/  m  tapp-async
  ^-  form:m
  ;<  ~  bind:m
    ?-  +<.in
      %history  (history +>.in)
      %log      (log +>.in)
      %disavow  (disavow +>.in)
    ==
  (pure:m state)
::
::  +handle-peer: handle incoming subscriptions (generally from jael)
::
::    /~some-ship: listen to events for this ship
::    /: listen to events for all ships azimuth-tracker is observing
::
::    note that incoming subscriptions affect application state.
::
++  handle-peer
  |=  =path
  =/  m  tapp-async
  ^-  form:m
  ?.  ?=(?(~ [@ ~]) path)  !!
  =/  who=(unit ship)
    ?~  path  ~
    `(slav %p i.path)
  =.  whos.state
    ?~  who
      ~
    (~(put in whos.state) u.who)
  (start state [our dap]:bowl)
--
