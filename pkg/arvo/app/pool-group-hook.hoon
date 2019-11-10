::  pool-group-hook: maintain groups based on invite pool
::
/-  group-store
/+  tapp, stdio, ethio
=,  ethereum-types
=,  able:jael
::
=>  |%
    ++  group-path    /invite-peers
    ++  refresh-rate  ~m15
    --
::
=>  |%
    +$  app-state
      $:  %0
          url=_'http://eth-mainnet.urbit.org:8545'
          inviter=ship
          invited=(set ship)
      ==
    ::
    +$  peek-data  ~
    +$  in-poke-data  ~
    +$  out-poke-data
      [%group-action group-action:group-store]
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
    ++  ethio  (^ethio out-poke-data out-peer-data)
    --
::
::  Async helpers
::
=>  |%
    ++  get-invited-by
      |=  [url=@t who=ship]
      =/  m  (async:stdio ,ship)
      ^-  form:m
      ;<  res=@t  bind:m
        %+  read-contract:ethio  url
        :+  `'invitedBy'
          delegated-sending:contracts:azimuth
        :-  'invitedBy(uint32)'
        :~  [%uint `@`who]
        ==
      %-  pure:m
      ^-  ship  ^-  @
      %+  decode-results:abi:ethereum  res
      [%uint]~
    ::
    ++  get-invited
      |=  [url=@ta who=ship]
      =/  m  (async:stdio ,(list ship))
      ^-  form:m
      ;<  res=@t  bind:m
        %+  read-contract:ethio  url
        :+  `'getInvited'
          delegated-sending:contracts:azimuth
        :-  'getInvited(uint32)'
        :~  [%uint `@`who]
        ==
      %-  pure:m
      ;;  (list ship)
      %+  decode-results:abi:ethereum  res
      [%array %uint]~
    ::
    ++  send-poke
      |=  [our=ship =group-action:group-store]
      =/  m  (async:stdio ,~)
      ^-  form:m
      %+  poke-app:stdio
        [our %group-store]
      [%group-action group-action]
    --
::
::  Main loop
::
=>  |%
    ++  start
      |=  [state=app-state our=ship]
      =/  m  tapp-async
      ^-  form:m
      ;<  inviter=ship  bind:m  (get-invited-by url.state our)
      ?:  =(0 inviter)
        ::  we're done here, don't do anything ever again
        (pure:m state)
      =.  inviter.state  inviter
      ::  create the group
      ;<  ~  bind:m  (send-poke our %bundle group-path)
      ::  start update timer loop
      ;<  ~  bind:m  set-timer
      ::  go ahead and update for the first time
      (update state our)
    ::
    ::  Get updates since last checked
    ::
    ++  update
      |=  [state=app-state our=ship]
      =/  m  tapp-async
      ^-  form:m
      ;<  invited=(list ship)  bind:m  (get-invited [url inviter]:state)
      =/  new=(list ship)
        %+  skip  invited
        ~(has in invited.state)
      ;<  ~  bind:m
        ?:  =(~ new)  (pure:(async:stdio ,~) ~)
        (send-poke our %add (sy new) group-path)
      %-  pure:m
      state(invited (~(gas in invited.state) new))
    ::
    ::  Set update timer
    ::
    ++  set-timer
      =/  m  (async:tapp ,~)
      ^-  form:m
      ;<  now=@da  bind:m  get-time:stdio
      =/  next=@da  (add now refresh-rate)
      ::NOTE  we use +send-raw-card here to ensure we always set a new timer,
      ::      regardless of what happens further on in the flow.
      (send-raw-card:stdio %wait /effect/(scot %da next) next)
    --
::
::  Main
::
=*  default-tapp  default-tapp:tapp
%-  create-tapp-all:tapp
|_  [=bowl:gall state=app-state]
++  handle-init
  =/  m  tapp-async
  ^-  form:m
  (start state our.bowl)
::
++  handle-take
  |=  =sign:tapp
  =/  m  tapp-async
  ^-  form:m
  ?+  -.sign  ~|([%strange-sign -.sign] !!)
      %coup
    ?~  error.sign  (pure:m state)
    %-  (slog [leaf+"pool-group-hook effect failed" u.error.sign])
    (pure:m state)
  ::
      %wake
    ;<  ~  bind:m
      set-timer
    (update state our.bowl)
  ==
::
++  handle-poke  handle-poke:default-tapp
++  handle-diff  handle-diff:default-tapp
++  handle-peer  handle-peer:default-tapp
++  handle-peek  handle-peek:default-tapp
--
