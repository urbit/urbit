::  roller: Azimuth L2 roll aggregator
::
::    general flow is as described below, to ensure transactions actually go
::    through once we start sending it out, in the dumbest reasonable way.
::
::    periodic timer fires:
::    if there are no pending l2 txs, do nothing.
::    else kick off tx submission flow:
::    "freeze" pending txs, store alongside nonce, then increment nonce,
::    kick off thread for sending the corresponding l1 tx:
::      if nonce doesn't match on-chain expected nonce, bail.
::      if we can't afford the tx fee, bail.
::      construct, sign, submit the l1 tx.
::    if thread bailed, retry in five minutes.
::    if thread succeeded, retry in five minutes with higher gas price.
::    when retrying, only do so if l2 txs remain in the "frozen" txs group.
::    on %tx diff from naive, remove the matching tx from the frozen group.
::
::TODO  questions:
::  - it's a bit weird how we just assume the raw and tx in raw-tx to match...
::
/-  *dice
/+  azimuth,
    naive,
    dice,
    lib=naive-transactions,
    default-agent,
    ethereum,
    dbug,
    verb
::
|%
+$  app-state
  $:  %6
      ::  pending: the next l2 txs to be sent
      ::  sending: l2 txs awaiting l2 confirmation, ordered by nonce
      ::  finding: sig+raw-tx hash reverse lookup for txs in sending map
      ::  history: status of l2 txs by ethereum address, timestamp sorted
      ::  ship-quota: number of txs submited per ship in the current slice
      ::  allowances: specific no of allowed transactions per given ship
      ::  next-nonce: next l1 nonce to use
      ::  next-batch: when then next l2 batch will be sent
      ::  next-slice: when the global quota will be reset
      ::  pre: predicted l2 state
      ::  own: ownership of azimuth points
      ::  spo: residents and escapees, per sponsor
      ::
      pending=(list pend-tx)
      sending=(tree [l1-tx-pointer send-tx])
      finding=(map keccak ?(%confirmed %failed [=time l1-tx-pointer]))
      history=(map address:ethereum (tree hist-tx))
      ship-quota=(map ship @ud)
      allowances=(map ship (unit @ud))
      next-nonce=(unit @ud)
      next-batch=time
      next-slice=time
      pre=^state:naive
      own=owners
      spo=sponsors
    ::
      ::  pk: private key to send the roll
      ::  quota: max numbers of transactions per unit of time (slice)
      ::  slice: unit of time where txs are allowed to be added to pending
      ::  derive: defer derivation of predicted/ownership state
      ::  frequency: time to wait between sending batches (TODO fancier)
      ::  endpoint: ethereum rpc endpoint to use
      ::  contract: ethereum contract address
      ::  chain-id: mainnet, goerli, local (https://chainid.network/)
      ::  resend-time: time to resend a batch with higher gas prie
      ::  update-rate: frequency to update the roller's predicted state
      ::  fallback-gas-price: default batch gas price
      ::
      pk=@
      slice=@dr
      quota=@ud
      derive=?
      frequency=@dr
      endpoint=(unit @t)
      contract=@ux
      chain-id=@
      resend-time=@dr
      update-rate=@dr
      fallback-gas-price=@ud
  ==
::
+$  action
  $%  ::  submit: request to add a l2 tx to the pending queue
      ::
      [%submit force=? =address:naive sig=@ tx=part-tx]
      ::  cancel: cancels a pending transaction
      ::
      ::   a signed message ("cancel: 0xkeccak") is used as ownership validation
      ::
      [%cancel sig=@ keccak=@ =l2-tx =ship]
      ::  commit: manually commit a batch of pending txs
      ::
      ::   TODO: maybe pk=(unit @) later
      ::
      [%commit ~]
      ::  config: configure the roller
      ::
      [%config config]
      ::  assign: assign an allowance to a ship for submitting l2 txs
      ::
      [%assign =ship quota=(unit @ud)]
      ::  refuel: bumps the next-gas-price of a sending tx
      ::
      [%refuel nonce=@ address=(unit address:ethereum) gas=@ud]
  ==
::
+$  card   card:agent:gall
++  lverb  &
--
::  Helpers
::
=>  |%
    ::  TODO  /lib/sys.hoon?
    ::
    ++  sys
      |%
      ++  b
        |%
        ++  wait
          |=  [=wire =time]
          ^-  card
          [%pass wire %arvo %b %wait time]
        --
      --
    ::  TODO  /lib/spider.hoon?
    ::
    ++  spider
      |%
      ++  start-thread
        |=  [=bowl:gall =wire thread=term arg=vase]
        ^-  (list card)
        =/  =beak    byk.bowl(r da+now.bowl)
        =/  tid=@ta  (rap 3 thread '--' (scot %uv eny.bowl) ~)
        =/  args     [~ `tid beak thread arg]
        :~  [%pass wire %agent [our.bowl %spider] %watch /thread-result/[tid]]
            [%pass wire %agent [our.bowl %spider] %poke %spider-start !>(args)]
        ==
      ::
      ++  leave
        |=  [agent=@p =path]
        ^-  card
        [%pass path %agent [agent %spider] %leave ~]
      --
    ::
    ++  get-l1-address
      |=  [=tx:naive nas=^state:naive]
      ^-  (unit address:ethereum)
      ?~  point=(get:orp:dice points.nas ship.from.tx)  ~
      =<  `address
      (proxy-from-point:naive proxy.from.tx u.point)
    ::
    ++  timer
      |%
      ::  +set-roller: %wait until next whole frequency
      ::
      ++  set-roller
        |=  [frequency=@dr now=@da]
        ^-  [=card =time]
        =+  time=(mul +((div now frequency)) frequency)
        [(wait:b:sys /timer time) time]
      ::  +set-roller: %wait until next whole :slice
      ::
      ++  set-quota
        |=  [slice=@dr now=@da]
        ^-  [=card =time]
        =+  time=(mul +((div now slice)) slice)
        [(wait:b:sys /quota-timer time) time]
      --
    ::  TODO: move address to state?
    ::
    ++  get-address
      |=  pk=@
      ^-  address:ethereum
      (address-from-prv:key:ethereum pk)
    --
::  Cards
::
=>  |%
    ++  emit
      |=  updates=(list update)
      =|  cards=(list card)
      |-  ^-  (list card)
      ?~  updates  (flop cards)
      =*  up  i.updates
      =/  [address=@t last-owner=(unit @t)]
        ?-    -.up
            %tx
          :_  ~
          (scot %ux address.pend-tx.up)
        ::
            %point
        :-  (scot %ux address.to.up)
        ?~(from.up ~ `(scot %ux address.u.from.up))
        ==
      =.  cards
        %+  welp
          ^-  (list card)
          ?-  -.i.updates
              %tx
            [%give %fact ~[/txs/[address]] tx+!>(up)]~
          ::
              %point
            =/  =cage  point+!>(up)
            %+  weld
              [%give %fact ~[/points/[address]] cage]~
            ?~  last-owner  ~
            [%give %fact ~[/points/[u.last-owner]] cage]~
          ==
        cards
      ::
      $(updates t.updates)
    --
::
=|  app-state
=*  state  -
::
%-  agent:dbug
%+  verb  |
^-  agent:gall
::
=<
  |_  =bowl:gall
  +*  this  .
      do    ~(. +> bowl)
      def   ~(. (default-agent this %|) bowl)
  ::
  ++  on-init
    ^-  (quip card _this)
    =:  frequency           ~h1
        quota               25
        slice               ~d7
        resend-time         ~m5
        update-rate         ~m5
        contract            naive:local-contracts:azimuth
        chain-id            chain-id:local-contracts:azimuth
        fallback-gas-price  10.000.000.000
      ==
    =^  card-1  next-batch  (set-roller:timer frequency now.bowl)
    =^  card-2  next-slice  (set-quota:timer slice now.bowl)
    :_  this
    :~  card-1
        card-2
        [%pass /azimuth-events %agent [our.bowl %azimuth] %watch /event]
    ==
  ::
  ++  on-save  !>(state)
  ++  on-load
    |=  old=vase
    ^-  (quip card _this)
    =|  cards=(list card)
    |^
    =+  !<(old-state=app-states old)
    =?  cards  ?=(%0 -.old-state)
      [card:(set-quota:timer slice now.bowl)]~
    =?  old-state  ?=(%0 -.old-state)
      ^-  state-1
      =|  ship-quota=(map ship @ud)
      =/  [slice=@dr quota=@ud resend-time=@dr update-rate=@dr]
        [~d7 7 ~m5 ~m1]
      =,  old-state
      :*  %1
          pending  ^-((tree [l1-tx-pointer old-send-tx-4]) sending)
          finding  history  ship-quota  next-nonce  next-batch
          pre  own  pk  slice  quota  derive
          frequency  endpoint  contract  chain-id
          resend-time  update-rate
      ==
    =?  old-state  ?=(%1 -.old-state)
      ^-  state-2
      =|  spo=(map ship [residents=(set ship) requests=(set ship)])
      =,  old-state
      :*  %2
          pending  sending  finding  history
          ship-quota  next-nonce  next-batch
          pre  own  spo  pk  slice  quota  derive
          frequency  endpoint  contract  chain-id
          resend-time  update-rate
      ==
    =?  old-state  ?=(%2 -.old-state)
      ^-  state-3
      =,  old-state
      =|  allowances=(map ship (unit @ud))
      =/  next-slice=time  (mul +((div now.bowl slice)) slice)
      :*  %3
          pending  sending  finding  history
          ship-quota  allowances
          next-nonce  next-batch  next-slice
          pre  own  spo  pk  slice  quota  derive
          frequency  endpoint  contract  chain-id
          resend-time  update-rate
      ==
    =?  old-state  ?=(%3 -.old-state)
      ^-  state-4
      =,  old-state
      =/  fallback-gas-price=@ud  10.000.000.000
      :*  %4
          pending  sending  finding  history
          ship-quota  allowances
          next-nonce  next-batch  next-slice
          pre  own  spo  pk  slice  quota  derive
          frequency  endpoint  contract  chain-id
          resend-time  update-rate  fallback-gas-price
      ==
    =?  old-state  ?=(%4 -.old-state)
      ^-  state-5
      =/  new-sending=(tree [l1-tx-pointer old-send-tx-5])
        %+  run:ors:dice  sending.old-state
        |=  old=old-send-tx-4
        ^-  old-send-tx-5
        old(txs (turn txs.old (lead |)))
      =,  old-state
      :*  %5
          pending  new-sending  finding  history
          ship-quota  allowances
          next-nonce  next-batch  next-slice
          pre  own  spo  pk  slice  quota  derive
          frequency  endpoint  contract  chain-id
          resend-time  update-rate  fallback-gas-price
      ==
    =?  old-state  ?=(%5 -.old-state)
      ^-  app-state
      =/  new-sending=(tree [l1-tx-pointer send-tx])
        %+  run:ors:dice  sending.old-state
        |=  old=old-send-tx-5
        ^-  send-tx
        %=    old
            txs
          %+  turn  txs.old
          |=  [force=? =raw-tx:naive]
          =/  sign-address=(unit @ux)
            (extract-address:lib raw-tx pre chain-id)
          :_  [force raw-tx]
          ?.  ?=(^ sign-address)
            0x0
          u.sign-address
        ==
      =,  old-state
      :*  %6
          pending  new-sending  finding  history
          ship-quota  allowances
          next-nonce  next-batch  next-slice
          pre  own  spo  pk  slice  quota  derive
          frequency  endpoint  contract  chain-id
          resend-time  update-rate  fallback-gas-price
      ==
    ?>  ?=(%6 -.old-state)
    [cards this(state old-state)]
    ::
    ++  app-states
      $%  state-0
          state-1
          state-2
          state-3
          state-4
          state-5
          app-state
      ==
    ::
    ++  state-0
      $:  %0
          pending=(list pend-tx)
          sending=(tree [l1-tx-pointer old-send-tx-4])
          finding=(map keccak ?(%confirmed %failed [=time l1-tx-pointer]))
          history=(map address:ethereum (tree hist-tx))
          next-nonce=(unit @ud)
          next-batch=time
          pre=^state:naive
          own=owners
          derive=?
          pk=@
          frequency=@dr
          endpoint=(unit @t)
          contract=@ux
          chain-id=@
      ==
    ::
    ++  state-1
      $:  %1
          pending=(list pend-tx)
          sending=(tree [l1-tx-pointer old-send-tx-4])
          finding=(map keccak ?(%confirmed %failed [=time l1-tx-pointer]))
          history=(map address:ethereum (tree hist-tx))
          ship-quota=(map ship @ud)
          next-nonce=(unit @ud)
          next-batch=time
          pre=^state:naive
          own=owners
          pk=@
          slice=@dr
          quota=@ud
          derive=?
          frequency=@dr
          endpoint=(unit @t)
          contract=@ux
          chain-id=@
          resend-time=@dr
          update-rate=@dr
      ==
    ::
    ++  state-2
      $:  %2
          pending=(list pend-tx)
          sending=(tree [l1-tx-pointer old-send-tx-4])
          finding=(map keccak ?(%confirmed %failed [=time l1-tx-pointer]))
          history=(map address:ethereum (tree hist-tx))
          ship-quota=(map ship @ud)
          next-nonce=(unit @ud)
          next-batch=time
          pre=^state:naive
          own=owners
          spo=sponsors
          pk=@
          slice=@dr
          quota=@ud
          derive=?
          frequency=@dr
          endpoint=(unit @t)
          contract=@ux
          chain-id=@
          resend-time=@dr
          update-rate=@dr
      ==
    ::
    ++  state-3
      $:  %3
          pending=(list pend-tx)
          sending=(tree [l1-tx-pointer old-send-tx-4])
          finding=(map keccak ?(%confirmed %failed [=time l1-tx-pointer]))
          history=(map address:ethereum (tree hist-tx))
          ship-quota=(map ship @ud)
          allowances=(map ship (unit @ud))
          next-nonce=(unit @ud)
          next-batch=time
          next-slice=time
          pre=^state:naive
          own=owners
          spo=sponsors
          pk=@
          slice=@dr
          quota=@ud
          derive=?
          frequency=@dr
          endpoint=(unit @t)
          contract=@ux
          chain-id=@
          resend-time=@dr
          update-rate=@dr
      ==
    ::
    +$  old-send-tx-4  [next-gas-price=@ud sent=? txs=(list =raw-tx:naive)]
    ::
    ++  state-4
      $:  %4
          pending=(list pend-tx)
          sending=(tree [l1-tx-pointer old-send-tx-4])
          finding=(map keccak ?(%confirmed %failed [=time l1-tx-pointer]))
          history=(map address:ethereum (tree hist-tx))
          ship-quota=(map ship @ud)
          allowances=(map ship (unit @ud))
          next-nonce=(unit @ud)
          next-batch=time
          next-slice=time
          pre=^state:naive
          own=owners
          spo=sponsors
          pk=@
          slice=@dr
          quota=@ud
          derive=?
          frequency=@dr
          endpoint=(unit @t)
          contract=@ux
          chain-id=@
          resend-time=@dr
          update-rate=@dr
          fallback-gas-price=@ud
      ==
    ::
    +$  old-send-tx-5
      [next-gas-price=@ud sent=? txs=(list [force=? =raw-tx:naive])]
    ::
    ++  state-5
      $:  %5
          pending=(list pend-tx)
          sending=(tree [l1-tx-pointer old-send-tx-5])
          finding=(map keccak ?(%confirmed %failed [=time l1-tx-pointer]))
          history=(map address:ethereum (tree hist-tx))
          ship-quota=(map ship @ud)
          allowances=(map ship (unit @ud))
          next-nonce=(unit @ud)
          next-batch=time
          next-slice=time
          pre=^state:naive
          own=owners
          spo=sponsors
          pk=@
          slice=@dr
          quota=@ud
          derive=?
          frequency=@dr
          endpoint=(unit @t)
          contract=@ux
          chain-id=@
          resend-time=@dr
          update-rate=@dr
          fallback-gas-price=@ud
      ==
    --
  ::
  ++  on-poke
    |=  [=mark =vase]
    ^-  (quip card _this)
    =^  cards  state
      ?+    mark  (on-poke:def mark vase)
          %roller-action
        =+  !<(poke=action vase)
        (on-action:do poke)
      ==
    [cards this]
  ::  +on-peek: scry paths
  ::
  ::    /x/pending                     ->  %noun  (list pend-tx)
  ::    /x/pending/[~ship]             ->  %noun  (list pend-tx)
  ::    /x/pending/[0xadd.ress]        ->  %noun  (list pend-tx)
  ::    /x/tx/[0xke.ccak]/status       ->  %noun  tx-status
  ::    /x/history/[0xadd.ress]        ->  %noun  (list hist-tx)
  ::    /x/nonce/[~ship]/[proxy]       ->  %noun  (unit @)
  ::    /x/spawned/[~star]             ->  %noun  (list ship)
  ::    /x/unspawned/[~star]           ->  %noun  (list ship)
  ::    /x/sponsored/[~point]          ->  %noun  [(list ship) (list ship)]
  ::    /x/next-batch                  ->  %atom  time
  ::    /x/next-slice                  ->  %atom  time
  ::    /x/point/[~ship]               ->  %noun  point:naive
  ::    /x/ships/[0xadd.ress]          ->  %noun  (list ship)
  ::    /x/config                      ->  %noun  config
  ::    /x/chain-id                    ->  %atom  @
  ::    /x/owned/[0xadd.ress]          ->  %noun  (list ship)
  ::    /x/transfers/[0xadd.ress]      ->  %noun  (list ship)
  ::    /x/manager/[0xadd.ress]        ->  %noun  (list ship)
  ::    /x/voting/[0xadd.ress]         ->  %noun  (list ship)
  ::    /x/spawning/[0xadd.ress]       ->  %noun  (list ship)
  ::    /x/predicted                   ->  %noun  state:naive
  ::    /x/quota                       ->  %atom  @ud
  ::    /x/slice                       ->  %atom  @dr
  ::    /x/over-quota/[~ship]          ->  %atom  ?
  ::    /x/ship-quota/[~ship]          ->  %atom  @ud
  ::    /x/allowances                  ->  %noun  (map @p (unit @ud))
  ::    /x/allowance/[~ship]           ->  %noun  (unit @ud)
  ::    /x/ready                       ->  %atom  ?
  ::
  ++  on-peek
    |=  =path
    ^-  (unit (unit cage))
    ?+  path  ~
      [%x %pending ~]         ``noun+!>(pending)
      [%x %pending @ ~]       (pending-by:on-peek:do i.t.t.path)
      [%x %tx @ %status ~]    (status:on-peek:do i.t.t.path)
      [%x %pending-tx @ ~]    (transaction:on-peek:do i.t.t.path)
      [%x %history @ ~]       (history:on-peek:do i.t.t.path)
      [%x %nonce @ @ ~]       (nonce:on-peek:do i.t.t.path i.t.t.t.path)
      [%x %spawned @ ~]       (spawned:on-peek:do i.t.t.path)
      [%x %unspawned @ ~]     (unspawned:on-peek:do i.t.t.path)
      [%x %sponsored @ ~]     (sponsored:on-peek:do i.t.t.path)
      [%x %next-batch ~]      ``atom+!>(next-batch)
      [%x %next-slice ~]      ``atom+!>(next-slice)
      [%x %point @ ~]         (point:on-peek:do i.t.t.path)
      [%x %ships @ ~]         (ships:on-peek:do i.t.t.path)
      [%x %config ~]          config:on-peek:do
      [%x %chain-id ~]        ``atom+!>(chain-id)
      [%x %owned @ ~]         (points-proxy:on-peek:do %own i.t.t.path)
      [%x %transfers @ ~]     (points-proxy:on-peek:do %transfer i.t.t.path)
      [%x %manager @ ~]       (points-proxy:on-peek:do %manage i.t.t.path)
      [%x %voting @ ~]        (points-proxy:on-peek:do %vote i.t.t.path)
      [%x %spawning @ ~]      (points-proxy:on-peek:do %spawn i.t.t.path)
      [%x %predicted ~]       ``noun+!>(pre)
      [%x %quota ~]           ``atom+!>(quota)
      [%x %slice ~]           ``atom+!>(slice)
      [%x %over-quota @ ~]    (over-quota:on-peek:do i.t.t.path)
      [%x %ship-quota @ ~]    (ship-quota:on-peek:do i.t.t.path)
      [%x %allowances ~]      ``noun+!>(allowances)
      [%x %allowance @ ~]     (allowance:on-peek:do i.t.t.path)
      [%x %ready ~]           ``atom+!>(?=(^ points.pre))
    ==
  ::
  ++  on-arvo
    |=  [=wire =sign-arvo]
    ^-  (quip card _this)
    ?+    wire  (on-arvo:def wire sign-arvo)
        [%timer ~]
      ?+  +<.sign-arvo  (on-arvo:def wire sign-arvo)
        %wake  =^(cards state (on-timer:do &) [cards this])
      ==
        [%quota-timer ~]
      ?+  +<.sign-arvo  (on-arvo:def wire sign-arvo)
        %wake  =^(cards state on-quota-timer:do [cards this])
      ==
    ::
        [%predict ~]
      ?+    +<.sign-arvo  (on-arvo:def wire sign-arvo)
          %wake
        =^  effects  state
          (predicted-state canonical):do
        [(emit effects) this(derive &)]
      ==
    ::
        [%resend @ @ ~]
      =/  [address=@ux nonce=@ud]
        [(slav %ux i.t.wire) (slav %ud i.t.t.wire)]
      ?+    +<.sign-arvo  (on-arvo:def wire sign-arvo)
          %wake
        =/  cards=(list card)  (send-roll:do address nonce)
        =?  sending
          ?&  ?=(~ cards)
              (has:ors:dice sending [address nonce])
              =(0 (lent txs:(got:ors:dice sending [address nonce])))
          ==
          ~&  >  "empty sending, removing {<[nonce address]>}"
          =^  *  sending
            (del:ors:dice sending [address nonce])
          sending
        [cards this]
      ==
    ==
  ::
  ++  on-fail
    |=  [=term =tang]
    ::TODO  if crashed during timer, set new timer? how to detect?
    (on-fail:def term tang)
  ::
  ++  on-watch
    |=  =path
    ^-  (quip card _this)
    :_  this
    |^
    ?+  path  (on-watch:def path)
        [%txs @ ~]      ?>(?=(^ (slaw %ux i.t.path)) ~)
        [%points @ ~]   ?>(?=(^ (slaw %ux i.t.path)) ~)
        [%connect @ ~]  [%give %fact ~ (init i.t.path)]~
    ==
    ::
    ++  init
      |=  wat=@t
      ^-  cage
      :-  %roller-data
      !>  ^-  roller-data
      ?~  addr=(slaw %ux wat)  !!
      =/  [=owners =sponsors =points:naive]
       (give-points u.addr)
      =/  txs=(tree hist-tx)   (give-history u.addr)
      [chain-id points txs owners sponsors]
    ::
    ++  give-points
      |=  =address:ethereum
      ^-  [owners sponsors points:naive]
      =/  controlled=(list [proxy:naive ship])
        (controlled-ships:dice address own)
      %+  roll  controlled
      |=  [[=proxy:naive =ship] =owners =sponsors =points:naive]
      =/  sponsoring  (~(get by spo.state) ship)
      :+  (~(put ju owners) [proxy address] ship)
        ::
        ?~  sponsoring  sponsors
        (~(put by sponsors) ship u.sponsoring)
      ::
      %+  put:orp:dice  points
      [ship (need (get:orp:dice points.pre ship))]
    ::
    ++  give-history
      |=  =address:ethereum
      ^-  (tree hist-tx)
      ?~  hist=(~(get by history) address)
        ~
      u.hist
    --
  ::
  ++  on-leave  on-leave:def
  ::
  ++  on-agent
    |=  [=wire =sign:agent:gall]
    ^-  (quip card _this)
    |^
    ?+  wire  (on-agent:def wire sign)
      [%send @ @ *]         (send-batch i.t.wire i.t.t.wire sign)
      [%azimuth-events ~]   (azimuth-event sign)
      [%nonce ~]            (nonce sign)
      [%refresh-nonce @ ~]  (refresh i.t.wire sign)
    ==
    ::
    ++  send-batch
      |=  [address=@t nonce=@t =sign:agent:gall]
      ^-  (quip card _this)
      =/  [address=@ux nonce=@ud]
        [(slav %ux address) (slav %ud nonce)]
      ?-  -.sign
          %poke-ack
        ?~  p.sign
          %-  (slog leaf+"Send batch thread started successfully" ~)
          [~ this]
        %-  (slog leaf+"{(trip dap.bowl)} couldn't start thread" u.p.sign)
        :_  this
        [(leave:spider:do our.bowl wire)]~
      ::
          %watch-ack
        ?~  p.sign
          [~ this]
        =/  =tank  leaf+"{(trip dap.bowl)} couldn't start listen to thread"
        %-  (slog tank u.p.sign)
        [~ this]
      ::
          %kick
        [~ this]
      ::
          %fact
        ?+  p.cage.sign  (on-agent:def wire sign)
            %thread-fail
          =+  !<([=term =tang] q.cage.sign)
          %-  (slog leaf+"{(trip dap.bowl)} failed" leaf+<term> tang)
          =^  cards  state
            (on-batch-result:do address nonce %.n^[%crash 'thread failed'])
          [cards this]
        ::
            %thread-done
          =+   !<(result=(each [@ud @ud] [term @t]) q.cage.sign)
          =^  cards  state
            (on-batch-result:do address nonce result)
          [cards this]
        ==
      ==
    ::
    ++  azimuth-event
      |=  =sign:agent:gall
      ^-  (quip card _this)
      ?+  -.sign  [~ this]
          %watch-ack
        ?~  p.sign  [~ this]
        =/  =tank  leaf+"{(trip dap.bowl)} couldn't start listen to %azimuth"
        %-  (slog tank u.p.sign)
        [~ this]
      ::
          %fact
        ?+  p.cage.sign  (on-agent:def wire sign)
            %naive-diffs
          =+   !<(=diff:naive q.cage.sign)
          =^  cards  state
            (on-naive-diff:do diff)
          [cards this]
        ::
            %naive-state
          ~&  >  %received-azimuth-state
          =+  !<([nas=^state:naive =indices] q.cage.sign)
          =^  effects  state
            (predicted-state:do nas indices)
          [(emit effects) this]
        ==
      ==
    ::
    ++  nonce
      |=  =sign:agent:gall
      ^-  (quip card _this)
      ?-  -.sign
          %poke-ack
        ?~  p.sign
          %-  (slog leaf+"Nonce thread started successfully" ~)
          [~ this]
        %-  (slog leaf+"{(trip dap.bowl)} couldn't start thread" u.p.sign)
        :_  this
        [(leave:spider:do our.bowl wire)]~
      ::
          %watch-ack
        ?~  p.sign
          [~ this]
        =/  =tank  leaf+"{(trip dap.bowl)} couldn't start listen to thread"
        %-  (slog tank u.p.sign)
        [~ this]
      ::
          %kick
        [~ this]
      ::
          %fact
        ?+  p.cage.sign  (on-agent:def wire sign)
            %thread-fail
          =+  !<([=term =tang] q.cage.sign)
          %-  (slog leaf+"{(trip dap.bowl)} failed" leaf+<term> tang)
          [~ this]
        ::
            %thread-done
          =+   !<(nonce=@ud q.cage.sign)
          [~ this(next-nonce `nonce)]
        ==
      ==
    ::
    ++  refresh
      |=  [nonce=@t =sign:agent:gall]
      ^-  (quip card _this)
      =/  failed-nonce=@ud  (slav %ud nonce)
      ?-  -.sign
          %poke-ack
        ?~  p.sign
          %-  (slog leaf+"Refresh Nonce thread started successfully" ~)
          [~ this]
        %-  (slog leaf+"{(trip dap.bowl)} couldn't start thread" u.p.sign)
        :_  this
        [(leave:spider:do our.bowl wire)]~
      ::
          %watch-ack
        ?~  p.sign
          [~ this]
        =/  =tank  leaf+"{(trip dap.bowl)} couldn't start listen to thread"
        %-  (slog tank u.p.sign)
        [~ this]
      ::
          %kick
        [~ this]
      ::
          %fact
        ?+  p.cage.sign  (on-agent:def wire sign)
            %thread-fail
          =+  !<([=term =tang] q.cage.sign)
          %-  (slog leaf+"{(trip dap.bowl)} failed" leaf+<term> tang)
          [~ this]
        ::
            %thread-done
          =+   !<(nonce=@ud q.cage.sign)
          =^  cards  state
            (on-out-of-sync:do nonce failed-nonce)
          [cards this]
        ==
      ==
    --
  --
::
|_  =bowl:gall
::  +canonical: current naive, ownership, and sponsorship state
::
++  canonical
  |^  nas^own^spo
  ::
  ++  nas
    .^  ^state:naive
      %gx
      (scot %p our.bowl)
      %azimuth
      (scot %da now.bowl)
      /nas/noun
    ==
  ::
  ++  own
    .^  owners
      %gx
      (scot %p our.bowl)
      %azimuth
      (scot %da now.bowl)
      /own/noun
    ==
  ::
  ++  spo
    .^  sponsors
      %gx
      (scot %p our.bowl)
      %azimuth
      (scot %da now.bowl)
      /spo/noun
    ==
  --
::  +predicted-state
::
::    derives predicted state from applying pending & sending txs to
::    the provided naive state, discarding invalid txs in the process
::
++  predicted-state
  |=  [nas=^state:naive =indices]
  ^-  (quip update _state)
  =:  pre  nas
      own  own.indices
      spo  spo.indices
    ==
  |^
  =^  [nes=_sending updates-1=(list update)]  state
    apply-sending
  =^  [nep=_pending updates-2=(list update)]  state
    apply-pending
  :-  (welp updates-1 updates-2)
  state(sending nes, pending nep)
  ::
  ++  apply-pending
    (apply-txs pending %pending next-nonce.state)
  ::
  ++  apply-sending
    =|  ups=(list update)
    =/  valid=_sending  ~
    =+  sorted=(tap:ors:dice sending)
    |-  ^+  [[valid ups] state]
    ?~  sorted  [[valid ups] state]
    ::
    =*  key  key.i.sorted
    =*  val  val.i.sorted
    =/  txs=(list pend-tx)
      %+  turn  txs.val
      |=([addr=@ux force=? =raw-tx:naive] force^addr^*time^raw-tx)
    =^  [new-valid=_txs nups=_ups]  state
      (apply-txs txs %sending `nonce.key)
    =/  new-sending
     (turn new-valid |=([force=? addr=@ux * =raw-tx:naive] addr^force^raw-tx))
    ::  we only hear updates for this nonce if it has been sent
    ::
    =.  valid  ::=?  valid  sent.val
      (put:ors:dice valid key val(txs new-sending))
    $(sorted t.sorted, ups (welp ups nups))
  ::
  ++  apply-txs
    |=  [txs=(list pend-tx) type=?(%pending %sending) nonce=(unit @ud)]
    =/  valid=_txs  ~
    =|  updates=(list update)
    |-  ^+  [[valid updates] state]
    ?~  txs
      :_  state
      [(flop valid) (flop updates)]
    ::
    =*  tx       i.txs
    =*  raw-tx   raw-tx.i.txs
    =*  ship     ship.from.tx.raw-tx.i.txs
    =/  =keccak  (hash-raw-tx:lib raw-tx)
    =^  [gud=? up-1=_updates]  state
      (try-apply pre force.tx raw-tx)
    =/  =roll-tx  [ship type keccak (l2-tx +<.tx.raw-tx)]
    =?  valid     gud  [tx valid]
    =^  up-2  history
      ?:  gud  [~ history]
      =.  time.tx
        ?:  ?=(%pending type)  time.tx
        ?~  wer=(~(get by finding) keccak)
          ~&  >>>  "missing %sending tx in finding"^[ship raw-tx]
          now.bowl
        ?@  u.wer
          ~&  >>>  "weird tx in finding gud: {<gud>} {<u.wer>}"^[ship raw-tx]
          now.bowl
        time.u.wer
      ~?  =(0x0 address.tx)  %weird-null-tx-address^'apply-txs'
      (update-history:dice history [tx]~ %failed)
    =?  finding  !gud  (~(put by finding) keccak %failed)
    =.  updates  :(welp up-2 up-1 updates)
    $(txs t.txs)
  --
::  +try-apply: maybe apply the given l2 tx to the naive state
::
++  try-apply
  |=  [nas=^state:naive force=? =raw-tx:naive]
  ^-  [[? ups=(list update)] _state]
  =/  [success=? updates=(list update) predicted=_nas =indices]
    (apply-raw-tx:dice force chain-id raw-tx nas own spo)
  =:  pre  predicted
      own  own.indices
      spo  spo.indices
    ==
  [[success updates] state]
::
++  on-action
  |=  =action
  ^-  (quip card _state)
  =+  local=(team:title our.bowl src.bowl)
  |^
  ?-  -.action
    %commit  ?>(local (on-timer |))
    %config  ?>(local (on-config +.action))
    %assign  ?>(local `state(allowances (~(put by allowances) +.action)))
    %refuel  ?>(local (refuel-tx +.action))
    %cancel  (cancel-tx +.action)
  ::
      %submit
    ::  TODO: return [~ state] instead of crashing
    ::  if naive state hasn't being retrieved yet?
    ::
    ?>  ?=(^ points.pre)
    %-  take-tx
    :*  force.action
        address.action
        now.bowl
        sig.action
        (part-tx-to-full tx.action)
    ==
  ==
  ::
  ++  part-tx-to-full
    |=  =part-tx
    ^-  [octs tx:naive]
    ?-    -.part-tx
        %raw
      ?~  batch=(parse-raw-tx:naive 0 q.raw.part-tx)
        ~?  lverb  [dap.bowl %parse-failed]
        ::  TODO: maybe return a unit if parsing fails?
        ::
        !!
      [raw tx]:-.u.batch
    ::
      %don  [(gen-tx-octs:lib +.part-tx) +.part-tx]
      %ful  +.part-tx
    ==
  --
::
++  on-config
  |=  =config
  ^-  (quip card _state)
  ?-  -.config
    %frequency    [~ state(frequency frequency.config)]
    %fallback     [~ state(fallback-gas-price gas.config)]
    %resend-time  [~ state(resend-time time.config)]
    %update-rate  [~ state(update-rate rate.config)]
    %slice        [~ state(slice slice.config)]
    %quota        [~ state(quota quota.config)]
  ::
      %endpoint
    :-  ~
    =/  [contract=@ux chain-id=@]
      =<  [naive chain-id]
      =,  azimuth
      ?+  net.config  !!
        %mainnet  mainnet-contracts
        %goerli   goerli-contracts
        %local    local-contracts
        %default  contracts
      ==
    %_  state
      contract   contract
      chain-id   chain-id
      endpoint   `endpoint.config
    ==
  ::
      %setkey
    =?  pk.config  =((end [3 2] pk.config) '0x')
      (rsh [3 2] pk.config)
    ?~  pk=(de:base16:mimes:html pk.config)
      `state
    [(get-nonce q.u.pk /nonce) state(pk q.u.pk)]
  ==
::  +cancel-tx: cancel a pending transaction
::
++  cancel-tx
  |=  [sig=@ =keccak =l2-tx =ship]
  ^-  (quip card _state)
  ?^  status=(~(get by finding) keccak)
    ~?  lverb  [dap.bowl %tx-not-pending status+u.status]
    [~ state]
  ::  "cancel: 0x1234abcd"
  ::
  =/  message=octs
    %:  cad:naive  3
      8^'cancel: '
    ::
      =;  hash=@t
        (met 3 hash)^hash
      (crip "0x{((x-co:co 65) keccak)}")
    ::
      ~
    ==
  ?~  addr=(verify-sig:lib sig message)
    ~?  lverb  [dap.bowl %cancel-sig-fail]
    [~ state]
  =^  time  pending
    =|  nep=(list pend-tx)
    |-  ^-  [(unit time) _nep]
    ?~  pending  [~ (flop nep)]
    ?:  =(keccak (hash-raw-tx:lib raw-tx.i.pending))
      [`time.i.pending (weld (flop nep) t.pending)]
    $(pending t.pending, nep [i.pending nep])
  ?~  time
    ~?  lverb  [dap.bowl %weird-tx-not-pending]
    [~ state]
  :-  ~
  %_    state
      history
    =+  txs=(~(got by history) u.addr)
    =.  txs  +:(del:orh:dice txs u.time)
    %+  ~(put by history)  u.addr
    %^  put:orh:dice  txs
      u.time
    [ship %cancelled keccak l2-tx]
  ==
::  +refuel-tx: bumps the gas price for a sending tx
::
++  refuel-tx
  |=  [nonce=@ud address=(unit address:ethereum) gas=@ud]
  ^-  (quip card _state)
  =/  batch=[address:ethereum @ud]
    :_  nonce
    ?^(address u.address (get-address pk.state))
  =.  sending
    ?~  send-tx=(get:ors:dice sending batch)
      sending
    %^  put:ors:dice  sending
      batch
    u.send-tx(next-gas-price gas)
  `state
::  +take-tx: accept submitted l2 tx into the :pending list
::
++  take-tx
  |=  =pend-tx
  ^-  (quip card _state)
  =*  ship  ship.from.tx.raw-tx.pend-tx
  =/  [exceeded=? next-quota=@]  (quota-exceeded ship)
  ?:  exceeded  [~ state]
  =/  sign-address=(unit @ux)
    (extract-address:lib raw-tx.pend-tx pre chain-id)
  =?  address.pend-tx  ?=(^ sign-address)
    (need sign-address)
  =^  [gud=? cards-1=(list update)]  state
    (try-apply pre [force raw-tx]:pend-tx)
  =^  cards-2  history
    (update-history:dice history [pend-tx]~ ?:(gud %pending %failed))
  ?.  gud
    :_  state
    ::  %point and (%failed) %tx updates
    ::
    (emit cards-1)
  =:  pending     (snoc pending pend-tx)
      ship-quota  (~(put by ship-quota) ship next-quota)
    ==
  ::  toggle derivation
  ::
  :_  state(derive ?:(derive | derive))
  ;:  welp
    (emit cards-1)  :: %point updates
    (emit cards-2)  :: %tx updates
  ::
    ?.  derive  ~
    ::  defer updating predicted state from canonical
    ::
    [(wait:b:sys /predict (add update-rate now.bowl))]~
  ==
::  +on-timer: every :frequency, freeze :pending txs roll and start sending it
::
++  on-timer
  |=  new=?
  ^-  (quip card _state)
  =^  updates-1  state
    (predicted-state canonical)
  =^  cards  state
    ?:  =(~ pending)  [~ state]
    ?~  next-nonce
      ~?  lverb  [dap.bowl %missing-roller-nonce]  [~ state]
    ::  this guarantees that next-nonce is only incremented
    ::  when the thread that's sending the previous batch
    ::  has come back and confirms that it was sent to L1
    ::
    ?:  pending-batch
      ::  this would postpone sending the batch for a whole "frequency"
      ::  TODO: set up a timer to retry this in ~mX ?
      ::
      ~?  lverb  [dap.bowl %nonce-out-sync]  [~ state]
    =/  nonce=@ud  u.next-nonce
    =^  updates-2  history  (update-history:dice history pending %sending)
    =/  =address:ethereum   (get-address pk)
    =:  pending     ~
        derive      &
        next-nonce  `+(u.next-nonce)
      ::
          sending
        %^  put:ors:dice  sending
          [address nonce]
        :+  0  |
        (turn pending |=([force=? addr=@ux * =raw-tx:naive] addr^force^raw-tx))
      ::
          finding
        %-  ~(gas by finding)
        %+  turn  pending
        |=  pend-tx
        (hash-raw-tx:lib raw-tx)^[time address nonce]
      ==
    :_  state
    ;:  welp
      (emit updates-1)
      (emit updates-2)
      (send-roll address nonce)
    ==
  ?.  new   cards^state
  =^  card  next-batch
    (set-roller:timer frequency now.bowl)
  [card cards]^state
::  +on-quota-timer: resets tx quota for all ships
::
++  on-quota-timer
  ^-  (quip card _state)
  =^  card  next-slice  (set-quota:timer slice now.bowl)
  :-  [card]~
  state(ship-quota *(map ship @ud))
::  +get-nonce: retrieves the latest nonce
::
++  get-nonce
  |=  [pk=@ =wire]
  ^-  (list card)
  ?~  endpoint  ~?(lverb [dap.bowl %no-endpoint] ~)
  (start-thread:spider bowl wire [%roller-nonce !>([u.endpoint pk])])
::
++  quota-exceeded
  |=  =ship
  ^-  [exceeded=? next-quota=@ud]
  =/  quota=(unit @ud)         (~(get by ship-quota) ship)
  =/  allow=(unit (unit @ud))  (~(get by allowances) ship)
  ?~  quota
    :_  1
    ?~  allow  |
    ?~(u.allow | =(u.u.allow 0))
  :_  +(u.quota)
  ?~  allow
    (gte u.quota quota.state)
  ::  ship has been whitelisted ("?~ u.allow" means no quota restrictions)
  ::
  ?~(u.allow | (gte u.quota u.u.allow))
::  +pending-batch: checks if the previous nonce has been sent
::
::    If %.y, the roller has been trying to send a batch for a whole frequency.
::
::    The cause of not sending the previous batch can happen because
::    of thread failure or because the private key loaded onto
::    the roller was used for something other than signing L2 batches right
::    after the send-batch thread started.
::
::    After reaching this state, any subsequents attempts have failed
::    (prior to updating the sending nonce if we hit the on-out-of-sync case)
::    which would possibly require a manual intervention (e.g. changing the
::    ethereum node URL, adding funds to the roller's address, manually bumping
::    the fall-back-gas-price or refueling the current batch with higher gas)
::
++  pending-batch
  ^-  ?
  ?~  newest-batch=(ram:ors:dice sending)  |
  !=(sent.val.u.newest-batch &)
::  +on-out-of-sync: handles a mismatch between current and expected l1 nonce
::
++  on-out-of-sync
  |=  [nonce=@ud failed-nonce=@ud]
  ^-  (quip card _state)
  ~&  >  %begin-on-out-of-sync
  =/  =address:ethereum   (get-address pk)
  ::  we only consider nonces >= than the one that failed
  ::
  =/  failed-sending=(list [l1-tx-pointer send-tx])
    %-  tap:ors:dice
    ::  (range exclusive)
    ::
    (lot:ors:dice sending [`[address (dec failed-nonce)] ~])
  =/  confirmed-sending=_sending
    (lot:ors:dice sending [~ `[address failed-nonce]])
  =/  [nes=_sending nif=_finding sih=_history]
    %-  tail
    %+  roll  failed-sending
    |=  $:  [p=l1-tx-pointer q=send-tx]
            new-nonce=_nonce
            sending=_confirmed-sending
            finding=_finding
            history=_history
        ==
    |^
    =*  nonce  nonce.p
    =*  txs    txs.q
    ::  TODO: this shouldn't be needed
    ?:  (lth nonce.p failed-nonce)
      ~&  >>>  [%on-out-of-sync nonce+nonce.p failed+failed-nonce]
      [new-nonce sending finding history]
    :+  +(new-nonce)
      fix-sending
    process-l2-txs
    ::
    ++  fix-sending
      (put:ors:dice sending [p(nonce new-nonce) q(sent %.n)])
    ::
    ++  process-l2-txs
      %+  roll  txs.q
      |=  [[@ @ =raw-tx:naive] nif=_finding sih=_history]
      =/  =keccak  (hash-raw-tx:lib raw-tx)
      |^
      ?~  val=(~(get by nif) keccak)
        [nif sih]
      ?.  ?=(^ u.val)  [nif sih]
      :-  (fix-finding u.val)
      (fix-history time.u.val address.u.val)
      ::
      ++  fix-finding
        |=  val=[time l1-tx-pointer]
        ^+  nif
        (~(put by nif) keccak val(nonce.+ new-nonce))
      ::
      ++  fix-history
        |=  [=time =address:ethereum]
        ^+  sih
        =*  ship      ship.from.tx.raw-tx
        =/  l2-tx     (l2-tx +<.tx.raw-tx)
        =/  =roll-tx  [ship %sending keccak l2-tx]
        =+  txs=(~(got by sih) address)
        =.  txs  +:(del:orh:dice txs time)
        %+  ~(put by sih)  address
        (put:orh:dice txs [time roll-tx])
      --
    --
  =:  sending     nes
      finding     nif
      history     sih
      next-nonce  `+(nonce)
    ==
  ~&  >  %end-on-out-of-sync
  [(send-roll address nonce) state]
::  +send-roll: start thread to submit roll from :sending to l1
::
++  send-roll
  |=  [=address:ethereum =nonce:naive]
  ^-  (list card)
  ?~  endpoint
    ~?  lverb  [dap.bowl %no-endpoint]
    ~
  ::  if this nonce isn't in the sending queue anymore, it's done
  ::
  ?.  (has:ors:dice sending [address nonce])
    ~?  lverb  [dap.bowl %done-sending [address nonce]]
    ~
  ::  if there are no txs for this nonce, don't send it
  ::
  ?:  =(0 (lent txs:(got:ors:dice sending [address nonce])))
    ~&  >>>  [dap.bowl %empty-nonce]
    ~
  ::  start the thread, passing in the l2 txs to use
  ::  TODO  should go ahead and set resend timer in case thread hangs, or nah?
  ::
  %^    start-thread:spider
      bowl
    /send/(scot %ux address)/(scot %ud nonce)
  :-  %roller-send
  !>  ^-  rpc-send-roll
  :*  u.endpoint
      contract
      chain-id
      pk
      nonce
      fallback-gas-price
    ::
      =<  [next-gas-price (turn txs (cork tail tail))]
      [. (got:ors:dice sending [address nonce])]
  ==
::  +on-batch-result: await resend after thread success or failure
::
++  on-batch-result
  |=  [=address:ethereum nonce=@ud result=(each [@ud @ud] [term @t])]
  ^-  (quip card _state)
  |^
  ::  print error if there was one
  ::
  ~?  ?=(%| -.result)  [dap.bowl %send-error nonce+nonce +.p.result]
  ::  if this nonce was removed from the queue by a
  ::  previous resend-with-higher-gas thread, it's done
  ::
  ?.  (has:ors:dice sending [address nonce])
    ~?  lverb  [dap.bowl %done-sending [address nonce]]
    `state
  ?:  ?=([%| %not-sent %batch-parse-error] result)
    ::  if we tried to send a malformed batch, remove it from the queue
    ::
    ~&  >>>  [dap.bowl %removing-malformed-batch]
    =^  *  sending
      (del:ors:dice sending [address nonce])
    `state
  =/  =send-tx  (got:ors:dice sending [address nonce])
  ::  if the number of txs sent is less than the ones in sending, we remove
  ::  them from the latest sending batch and add them on top of the pending list
  ::
  =/  n-txs=@ud  ?:(?=(%& -.result) -.p.result (lent txs.send-tx))
  =/  not-sent=(list [=address:naive force=? =raw-tx:naive])
    (slag n-txs txs.send-tx)
  =/  partial-send=?  &(?=(%& -.result) (lth n-txs (lent txs.send-tx)))
  =?  txs.send-tx   partial-send
    (oust [n-txs (lent txs.send-tx)] txs.send-tx)
  =?  pending       partial-send
    (fix-not-sent-pending not-sent)
  =/  [nif=_finding sih=_history]
    (fix-not-sent-status not-sent)
  =:  finding  nif
      history  sih
    ==
  ~?  partial-send  [%extracting-txs-from-batch (lent not-sent)]
  ::
  =?  sending  ?|  ?=(%& -.result)
                   ?=([%| %crash *] result)
               ==
    %^  put:ors:dice  sending
      [address nonce]
    ::  update gas price for this tx in state
    ::
    ?:  ?=(%& -.result)
      send-tx(next-gas-price +.p.result, sent &)
    ::  if the thread crashed, we don't know the gas used, so we udpate it
    ::  manually, same as the thread would do. this has the problem of causing
    ::  the batch to be blocked if the thread keeps crashing, and we don't have
    ::  enough funds to pay.
    ::
    ::  on the other hand if the thread fails because +fetch-gas-price fails
    ::  (e.g. API change), and our fallback gas price is too low, the batch will
    ::  also be blocked, if we don't increase the next-gas-price, so either way
    ::  the batch will be stuck because of another underlying issue.
    ::
    %_    send-tx
        next-gas-price
      ?:  =(0 next-gas-price.send-tx)
        fallback-gas-price
      (add next-gas-price.send-tx 5.000.000.000)
    ==
  :_  state
  ?:  ?&  !sent.send-tx
          ?=([%| %not-sent %behind-nonce] result)
      ==
    ::  start out-of-sync flow if our L1 nonce is behind
    ::  and this transaction hasn't been sent out yet
    ::
    ~&  >  [dap.bowl %start-refresh-nonce-thread]
    (get-nonce pk.state /refresh-nonce/(scot %ud nonce))
  ::  resend the l1 tx in five minutes if:
  ::
  ::  - the thread succeeds and returns the next gas price
  ::  - the thread failed because:
  ::    - the roll's eth addres doesn't have enough funds to pay
  ::    - the thread crashes
  ::    - the sending L1 nonce is ahead of the expected one
  ::    - a general ethereum error
  ::
  :_  ~
  %+  wait:b:sys
    /resend/(scot %ux address)/(scot %ud nonce)
  (add resend-time now.bowl)
  ::
  ++  fix-not-sent-pending
    |=  not-sent=(list [=address:naive force=? =raw-tx:naive])
    =;  txs=(list pend-tx)
      (weld txs pending)
    ::  TODO: this would not be needed if txs.send-tx was a (list pend-tx)
    ::
    %+  murn  not-sent
    |=  [=address:naive force=? =raw-tx:naive]
    =/  =keccak  (hash-raw-tx:lib raw-tx)
    ?~  wer=(~(get by finding) keccak)
      ~&  >>>  %missing-tx-in-finding
      ~
    ?@  u.wer
      ~&  >>>  %missing-tx-in-finding
      ~
    `[force address time.u.wer raw-tx]
  ::
  ++  fix-not-sent-status
    |=  not-sent=(list [=address:naive force=? =raw-tx:naive])
    %+  roll  not-sent
    |=  [[@ @ =raw-tx:naive] nif=_finding sih=_history]
    =/  =keccak  (hash-raw-tx:lib raw-tx)
    ?~  val=(~(get by nif) keccak)
      [nif sih]
    ?.  ?=(^ u.val)
      [nif sih]
    =*  time      time.u.val
    =*  address   address.u.val
    =*  ship      ship.from.tx.raw-tx
    =/  l2-tx     (l2-tx +<.tx.raw-tx)
    =/  =roll-tx  [ship %pending keccak l2-tx]
    =+  txs=(~(got by sih) address)
    =.  txs  +:(del:orh:dice txs time)
    :-  (~(del by nif) keccak)
    %+  ~(put by sih)  address
    (put:orh:dice txs [time roll-tx])
  --
::  +on-naive-diff: process l2 tx confirmations
::
++  on-naive-diff
  |=  =diff:naive
  ^-  (quip card _state)
  ?.  |(?=(%point -.diff) ?=(%tx -.diff))
    [~ state]
  =;  [cards=(list card) =_state]
    :_  state(derive ?:(derive | derive))
    %+  weld  cards
    ?.  derive  ~
    ::  defer updating state from canonical
    ::
    [(wait:b:sys /predict (add update-rate now.bowl))]~
  ::
  ?:  ?=(%point -.diff)  [~ state]
  ?>  ?=(%tx -.diff)
  =/  =keccak  (hash-raw-tx:lib raw-tx.diff)
  ?~  wer=(~(get by finding) keccak)
    ::  tx not submitted by this roller
    ::
    [~ state]
  ?@  u.wer
    ~?  &(?=(%confirmed u.wer) ?=(~ err.diff))
      [dap.bowl %weird-double-confirm from.tx.raw-tx.diff]
    [~ state]
  =*  nonce       nonce.u.wer
  =*  tx-address  address.u.wer
  =*  ship        ship.from.tx.raw-tx.diff
  =*  time        time.u.wer
  =*  tx          tx.raw-tx.diff
  =/  l2-tx       (l2-tx +<.tx)
  ::  remove the tx from the sending map
  ::
  =.  sending
    =/  =address:ethereum   (get-address pk)
    ?~  sen=(get:ors:dice sending [address nonce])
      ~?  lverb  [dap.bowl %weird-double-remove nonce+nonce]
      sending
    ?~  nin=(find [raw-tx.diff]~ (turn txs.u.sen (cork tail tail)))
      ~?  lverb  [dap.bowl %weird-unknown nonce+nonce]
      sending
    =.  txs.u.sen  (oust [u.nin 1] txs.u.sen)
    ?~  txs.u.sen
      ~?  lverb
       [dap.bowl %done-with-nonce [address nonce]]
      =^  *  sending
        (del:ors:dice sending [address nonce])
      sending
    ^+  sending
    (put:ors:dice sending [address nonce] u.sen)
  ::  update the finding map with the new status
  ::
  =.  finding
    %+  ~(put by finding)  keccak
    ?~  err.diff  %confirmed
    ::  if we kept the forced flag around for longer, we could notify of
    ::  unexpected tx failures here. would that be useful? probably not?
    ::  ~?  !forced  [dap.bowl %aggregated-tx-failed-anyway err.diff]
    %failed
  ::
  ~?  =(0x0 tx-address)  %weird-null-tx-address^'on-naive-diff'
  =^  updates  history
    %^    update-history:dice
        history
    [| tx-address time raw-tx.diff]~
  ?~(err.diff %confirmed %failed)
  [(emit updates) state]
::
++  on-peek
  |%
  ++  pending-by
    |=  wat=@t
    ?~  who=(slaw %p wat)
      ::  by-address
      ::
      ?~  wer=(slaw %ux wat)
        [~ ~]
      =;  pending=(list pend-tx)
        ``noun+!>(pending)
      %+  skim  pending
      |=  pend-tx
      ::  TODO: use this instead? =(u.wer address)
      ::
      ?~  addr=(get-l1-address tx.raw-tx pre)  |
      =(u.wer u.addr)
    ::  by-ship
    ::
    =;  pending=(list pend-tx)
      ``noun+!>(pending)
    %+  skim  pending
    |=  pend-tx
    =(u.who ship.from.tx.raw-tx)
  ::
  ++  status
    |=  wat=@t
    ?~  keccak=(slaw %ux wat)
      [~ ~]
    :+  ~  ~
    :-  %noun
    !>  ^-  tx-status
    ?^  status=(~(get by finding) u.keccak)
      ?@  u.status  [u.status ~]
      [%sending `+.u.status]
    :: TODO: potentially slow!
    =;  known=?
      [?:(known %pending %unknown) ~]
    %+  lien  pending
    |=  pend-tx
    =(u.keccak (hash-raw-tx:lib raw-tx))
  ::
  ++  transaction
    |=  wat=@t
    ?~  keccak=(slaw %ux wat)
      [~ ~]
    :+  ~  ~
    :-  %noun
    !>  ^-  (unit pend-tx)
    :: TODO: potentially slow!
    |-
    ?~  pending  ~
    =*  tx  i.pending
    ?:  =(u.keccak (hash-tx:lib raw.raw-tx.tx))
      `tx
    $(pending t.pending)
  ::
  ++  history
    |=  wat=@t
    :+  ~  ~
    :-  %noun
    !>  ^-  (list hist-tx)
    ?~  addr=(slaw %ux wat)  ~
    ?~  hist=(~(get by ^history) u.addr)  ~
    (tap:orh:dice u.hist)
  ::
  ++  nonce
    |=  [who=@t proxy=@t]
    ?~  who=(slaw %p who)
      [~ ~]
    ?.  ?=(proxy:naive proxy)
      [~ ~]
    :+  ~  ~
    :-  %noun
    !>  ^-  (unit @)
    ?~  point=(get:orp:dice points.pre u.who)
      ~
    =<  `nonce
    (proxy-from-point:naive proxy u.point)
  ::
  ++  spawned
    |=  wat=@t
    :+  ~  ~
    :-  %noun
    !>  ^-  (list @p)
    ?~  star=(slaw %p wat)  ~
    =;  range
      (turn range head)
    ::  range exclusive [star first-moon-last-planet]
    ::
    %-  tap:orp:dice
    (lot:orp:dice points.pre [`u.star `(cat 3 u.star 0x1.ffff)])
  ::
  ++  unspawned
    |=  wat=@t
    :+  ~  ~
    :-  %noun
    !>  ^-  (list @p)
    ?~  star=(slaw %p wat)  ~
    =/  spawned=(set @p)
      =;  points
        (~(gas in *(set @p)) (turn points head))
      %-  tap:orp:dice
      (lot:orp:dice points.pre [`u.star `(cat 3 u.star 0x1.ffff)])
    =/  children=(list @p)
      (turn (gulf 0x1 0xffff) |=(a=@ (cat 3 u.star a)))
    %+  murn  children
    |=  =ship
    ?:  (~(has in spawned) ship)  ~
    `ship
  ::
  ++  sponsored
    |=  wat=@t
    :+  ~  ~
    :-  %noun
    !>  ^-  [(list ship) (list ship)]
    ?~  who=(slaw %p wat)  [~ ~]
    ?~  sponsor=(~(get by spo) u.who)
      [~ ~]
    :-  ~(tap in residents.u.sponsor)
    ~(tap in requests.u.sponsor)
  ::
  ++  point
    |=  wat=@t
    ?~  ship=(rush wat ;~(pfix sig fed:ag))
      ``noun+!>(*(unit point:naive))
    ``noun+!>((get:orp:dice points.pre u.ship))
  ::
  ++  ships
    |=  wat=@t
    :+  ~  ~
    :-  %noun
    !>  ^-  (list ship)
    ?~  addr=(slaw %ux wat)  ~
    (turn (controlled-ships:dice u.addr own) tail)
  ::
  ++  config
    :+  ~  ~
    :-  %noun
    !>  ^-  roller-config
    :*  next-batch
        frequency
        resend-time
        update-rate
        contract
        chain-id
        slice
        quota
    ==
  ::
  ++  points-proxy
    |=  [=proxy:naive wat=@t]
    :+  ~  ~
    :-  %noun
    !>  ^-  (list ship)
    ?~  addr=(slaw %ux wat)
      ~
    ~(tap in (~(get ju own) [proxy u.addr]))
  ::
  ++  over-quota
    |=  wat=@t
    ?~  who=(slaw %p wat)  [~ ~]
    =/  [exceeded=? *]  (quota-exceeded u.who)
    ``atom+!>(exceeded)
  ::
  ++  ship-quota
    |=  wat=@t
    ?~  who=(slaw %p wat)  [~ ~]
    =/  [exceeded=? next-quota=@ud]  (quota-exceeded u.who)
    =/  allow=(unit (unit @ud))      (~(get by allowances) u.who)
    :+  ~  ~
    :-  %atom
    !>  ^-  @ud
    ?:  exceeded     0
    =/  max-quota=@  quota.state
    ?:  &(?=(^ allow) ?=(~ u.allow))
      max-quota
    =?  max-quota  &(?=(^ allow) ?=(^ u.allow))
      u.u.allow
    (sub max-quota (dec next-quota))
  ::
  ++  allowance
    |=  wat=@t
    ?~  who=(slaw %p wat)  [~ ~]
    :+  ~  ~
    :-  %noun
    !>  ^-  (unit @ud)
    ?^  allow=(~(get by allowances) u.who)
      u.allow
    `quota.state
  --
--
