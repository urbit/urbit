::  pool-group-hook: maintain groups based on invite pool
::
::    looks at our invite tree, adds our siblings to group at +group-path
::
/-  group-store, spider
/+  default-agent, verb
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
          running=(unit =tid:spider)
          url=_'http://eth-mainnet.urbit.org:8545'
          inviter=(unit ship)
          invited=(set ship)
      ==
    ::
    +$  card  card:agent:gall
    --
::
::  Main
::
=|  state=app-state
::
%+  verb  |
^-  agent:gall
=<
  |_  =bowl:gall
  +*  this  .
      do    ~(. +> bowl)
      def   ~(. (default-agent this %|) bowl)
  ::
  ++  on-init
    ^-  (quip card _this)
    =^  cards  state
      poll-inviter:do
    [cards this]
  ::
  ++  on-save   !>(state)
  ++  on-load
    |=  old=vase
    ^-  (quip card _this)
    [~ this(state !<(app-state old))]
  ::
  ++  on-agent
    |=  [=wire =sign:agent:gall]
    ^-  (quip card agent:gall)
    ?.  ?=([%running *] wire)
      (on-agent:def wire sign)
    ?-  -.sign
        %poke-ack
      ?~  p.sign
        [~ this]
      %-  (slog leaf+"{(trip dap.bowl)} couldn't start thread" u.p.sign)
      :_  this(running.state ~)
      ~[(leave-spider:do t.wire) set-timer:do]
    ::
        %watch-ack
      ?~  p.sign
        [~ this]
      =/  =tank  leaf+"{(trip dap.bowl)} couldn't start listen to thread"
      %-  (slog tank u.p.sign)
      [[set-timer:do]~ this(running.state ~)]
    ::
        %kick  [~ this(running.state ~)]
        %fact
      ?+  p.cage.sign  (on-agent:def wire sign)
          %thread-fail
        =+  !<([=term =tang] q.cage.sign)
        %-  (slog leaf+"{(trip dap.bowl)} failed; will retry" leaf+<term> tang)
        [[set-timer:do]~ this(running.state ~)]
      ::
          %thread-done
        ?+  t.wire  ~|([dap.bowl %unexpected-thread-done wire] !!)
            [%inviter ~]
          =+  !<(res=@t q.cage.sign)
          =/  inviter=ship
            `@`(decode-results:abi:ethereum res [%uint]~)
          ::  if we weren't invited by anyone, don't do anything anymore.
          ::
          ?:  =(0 inviter)  [~ this(state state(running ~, inviter ~))]
          =.  inviter.state  `inviter
          =^  cards  state
            poll-invited:do
          [cards this]
        ::
            [%invited ~]
          =+  !<(res=@t q.cage.sign)
          =/  invited=(list ship)
            ;;  (list ship)
            (decode-results:abi:ethereum res [%array %uint]~)
          =^  cards  state
            (process-invited:do invited)
          [cards this(running.state ~)]
        ==
      ==
    ==
  ::
  ++  on-arvo
    |=  [=wire =sign-arvo]
    ^-  (quip card _this)
    ?+  +<.sign-arvo  ~|([dap.bowl %strange-arvo-sign +<.sign-arvo] !!)
        %wake
      =^  cards  state
        ?~  inviter.state
          poll-inviter:do
        poll-invited:do
      [cards this]
    ==
  ::
  ++  on-poke   on-poke:def
  ++  on-peek   on-peek:def
  ++  on-watch  on-watch:def
  ++  on-leave  on-leave:def
  ++  on-fail   on-fail:def
  --
::
|_  =bowl:gall
++  poke-spider
  |=  [=path =cage]
  ^-  card
  [%pass [%running path] %agent [our.bowl %spider] %poke cage]
::
++  watch-spider
  |=  [=path =sub=path]
  ^-  card
  [%pass [%running path] %agent [our.bowl %spider] %watch sub-path]
::
++  leave-spider
  |=  [=path]
  ^-  card
  [%pass [%running path] %agent [our.bowl %spider] %leave ~]
::
++  new-tid
  |=  eny=@uv
  ^-  @t
  %+  scot  %ta
  :((cury cat 3) dap.bowl '_' (scot %uv eny))
::
++  start-contract-read
  |=  [=wire req=proto-read-request:rpc:ethereum]
  ^-  (quip card _state)
  =/  new-tid  (new-tid eny.bowl)
  =/  args
    [~ `new-tid %eth-read-contract !>([url.state req])]
  :_  state(running `new-tid)
  :~  (watch-spider wire /thread-result/[new-tid])
      (poke-spider wire %spider-start !>(args))
  ==
::
++  poll-inviter
  ^-  (quip card _state)
  %+  start-contract-read  /inviter
  :+  `'invitedBy'
    delegated-sending:contracts:azimuth
  :-  'invitedBy(uint32)'
  :~  [%uint `@`our.bowl]
  ==
::
++  poll-invited
  ^-  (quip card _state)
  ?~  inviter.state
    ~&  [dap.bowl %skipping-poll-invited]
    [~ state]
  %+  start-contract-read  /invited
  :+  `'getInvited'
    delegated-sending:contracts:azimuth
  :-  'getInvited(uint32)'
  :~  [%uint `@`u.inviter.state]
  ==
::
++  process-invited
  |=  invited=(list ship)
  =/  new=(list ship)
    %+  skip  invited
    ~(has in invited.state)
  :_  state(invited (~(gas in invited.state) new))
  :~  set-timer
    ::
      :*  %pass
          /write
          %agent
          [our.bowl %group-store]
          %poke
          %group-action
          !>([%add (sy new) group-path])
      ==
  ==
::
++  set-timer
  ^-  card
  [%pass /timer %arvo %b %wait (add now.bowl refresh-rate)]
--
