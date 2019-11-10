::  eth-watcher: ethereum event log collector
::
/-  *eth-watcher
/+  tapp, stdio, ethio
=,  ethereum-types
=,  able:jael
::
=>  |%
    ++  refresh-rate  ~m5
    --
::
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
    ::  history: newest block first, oldest event first
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
    ++  ethio  (^ethio out-poke-data out-peer-data)
    --
::
::  Async helpers
::
=>  |%
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
      ;<  =latest=block  bind:m  (get-latest-block:ethio url.dog)
      ;<  dog=watchdog   bind:m  (zoom [path dog] number.id.latest-block)
      |-  ^-  form:m
      =*  loop  $
      ?:  (gth number.dog number.id.latest-block)
        (pure:m dog)
      ;<  =block  bind:m  (get-block-by-number:ethio url.dog number.dog)
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
      ;<  =new=loglist  bind:m  ::  oldest first
        (get-logs-by-hash:ethio url.dog hash.id.block contracts.dog topics.dog)
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
        (get-block-by-number:ethio url.dog number.id.i.blocks)
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
      ;<  =loglist  bind:m  ::  oldest first
        %:  get-logs-by-range:ethio
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
  ;<  ~  bind:m  (wait-effect:stdio (add now refresh-rate))
  (pure:m state)
::
++  handle-diff  handle-diff:default-tapp
::
++  handle-poke
  |=  in=in-poke-data
  =/  m  tapp-async
  ^-  form:m
  ?-  +<.in
      %watch
    ::  fully restart the watchdog if it doesn't exist yet,
    ::  or if the new config changes more than just the url.
    =/  restart=?
      ?|  !(~(has by dogs.state) path.in)
          ?!  .=  ->:(~(got by dogs.state) path.in)
                  +.config.in
      ==
    ~?  &((~(has by dogs.state) path.in) restart)
      [dap.bowl 'overwriting existing watchdog on' path.in]
    ;<  dog=watchdog  bind:m
      =/  dog=watchdog
        ?:  restart  *watchdog
        (~(got by dogs.state) path.in)
      (configure [path.in dog] config.in)
    =.  dogs.state  (~(put by dogs.state) path.in dog)
    (pure:m state)
  ::
      %clear
    =.  dogs.state  (~(del by dogs.state) path.in)
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
      =/  next=@da  (add now refresh-rate)
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
    %-  zing
    %-  flop
    =<  history
    (~(gut by dogs.state) t.path *watchdog)
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
