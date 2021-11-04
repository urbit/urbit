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
+$  state-0
  $:  %0
      ::  pending: the next l2 txs to be sent
      ::  sending: l2 txs awaiting l2 confirmation, ordered by nonce
      ::  finding: sig+raw-tx hash reverse lookup for txs in sending map
      ::  history: status of l2 txs by ethereum address, timestamp sorted
      ::  next-nonce: next l1 nonce to use
      ::  next-batch: when then next l2 batch will be sent
      ::  pre: predicted l2 state
      ::  own: ownership of azimuth points
      ::  derive: deferred derivation of predicted/ownership state
      ::
      pending=(list pend-tx)
      sending=(tree [l1-tx-pointer send-tx])
      finding=(map keccak ?(%confirmed %failed [=time l1-tx-pointer]))
      history=(map address:ethereum (tree hist-tx))
      next-nonce=(unit @ud)
      next-batch=time
      pre=^state:naive
      own=owners
      derive=?
    ::
      ::  pk: private key to send the roll
      ::  frequency: time to wait between sending batches (TODO fancier)
      ::  endpoint: ethereum rpc endpoint to use
      ::  contract: ethereum contract address
      ::  chain-id: mainnet, ropsten, local (https://chainid.network/)
      ::
      pk=@
      frequency=@dr
      endpoint=(unit @t)
      contract=@ux
      chain-id=@
  ==
::  orp: ordered points in naive state by parent ship
::
++  orp  ((on ship point:naive) por:naive)
::  ors: ordered sending map by (increasing) L1 nonce
::
++  ors  ((on l1-tx-pointer send-tx) nonce-order:dice)
::  orh: ordered tx history by (decreasing) timestamp
::
++  orh  ((on time roll-tx) gth)
+$  net  ?(%mainnet %ropsten %local)
::
+$  config
  $%  [%frequency frequency=@dr]
      [%setkey pk=@]
      [%endpoint endpoint=@t =net]
  ==
::
+$  action
  $%  ::  we need to include the address in submit so pending txs show up
      ::  in the tx history, but because users can send the wrong
      ::  address, in +apply-tx:predicted state, we just replace
      ::  the provided address, with the one used when the message was signed;
      ::
      ::  we need to do it there to know the correct nonce that the signed
      ::  message should have included.
      ::
      [%submit force=? =address:naive sig=@ tx=part-tx]
      [%cancel sig=@ keccak=@ =l2-tx =ship]
      [%commit ~]  ::TODO  maybe pk=(unit @) later
      [%config config]
  ==
::
+$  card  card:agent:gall
::
::  TODO: add to config
::
++  resend-time  ~m5
++  update-rate  ~m1
::
++  lverb  &
--
::
=|  state-0
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
    =.  frequency  ~h1
    =.  contract  naive:local-contracts:azimuth
    =.  chain-id  chain-id:local-contracts:azimuth
    =^  card  next-batch  set-timer
    :_  this
    :~  card
        [%pass /azimuth-events %agent [our.bowl %azimuth] %watch /event]
    ==
  ::
  ++  on-save  !>(state)
  ++  on-load
    |=  old=vase
    ^-  (quip card _this)
    [~ this(state !<(state-0 old))]
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
  ::    /x/tx/[0xke.ccak]/[@ud]status  ->  %noun  tx-status
  ::    /x/history/[0xadd.ress]        ->  %noun  (list hist-tx)
  ::    /x/nonce/[~ship]/[proxy]       ->  %noun  (unit @)
  ::    /x/spawned/[~star]             ->  %noun  (list ship)
  ::    /x/unspawned/[~star]           ->  %noun  (list ship)
  ::    /x/next-batch                  ->  %atom  time
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
  ::
  ++  on-peek
    |=  =path
    ^-  (unit (unit cage))
    |^
    ?+  path  ~
      [%x %pending ~]         ``noun+!>(pending)
      [%x %pending @ ~]       (pending-by i.t.t.path)
      [%x %tx @ %status ~]    (status i.t.t.path)
      [%x %pending-tx @ ~]    (transaction i.t.t.path)
      [%x %history @ ~]       (history i.t.t.path)
      [%x %nonce @ @ ~]       (nonce i.t.t.path i.t.t.t.path)
      [%x %spawned @ ~]       (spawned i.t.t.path)
      [%x %unspawned @ ~]     (unspawned i.t.t.path)
      [%x %next-batch ~]      ``atom+!>(next-batch)
      [%x %point @ ~]         (point i.t.t.path)
      [%x %ships @ ~]         (ships i.t.t.path)
      [%x %config ~]          config
      [%x %chain-id ~]        ``atom+!>(chain-id)
      [%x %owned @ ~]         (points-proxy %own i.t.t.path)
      [%x %transfers @ ~]     (points-proxy %transfer i.t.t.path)
      [%x %manager @ ~]       (points-proxy %manage i.t.t.path)
      [%x %voting @ ~]        (points-proxy %vote i.t.t.path)
      [%x %spawning @ ~]      (points-proxy %spawn i.t.t.path)
      [%x %predicted ~]       ``noun+!>(pre)
    ==
    ::
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
      (tap:orh u.hist)
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
      ?~  point=(get:orp points.pre u.who)
        ~
      =/  nonce=@
        =<  nonce
        (proxy-from-point:naive proxy u.point)
      %-  some
      %+  roll  pending
      |=  [pend-tx nonce=_nonce]
      ?:(=([u.who proxy] from.tx.raw-tx) +(nonce) nonce)
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
      %-  tap:orp
      (lot:orp points.pre [`u.star `(cat 3 u.star 0x1.ffff)])
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
        %-  tap:orp
        (lot:orp points.pre [`u.star `(cat 3 u.star 0x1.ffff)])
      =/  children=(list @p)
        (turn (gulf 0x1 0xffff) |=(a=@ (cat 3 u.star a)))
      %+  murn  children
      |=  =ship
      ?:  (~(has in spawned) ship)  ~
      `ship
    ::
    ++  point
      |=  wat=@t
      ?~  ship=(rush wat ;~(pfix sig fed:ag))
        ``noun+!>(*(unit point:naive))
      ``noun+!>((get:orp points.pre u.ship))
    ::
    ++  ships
      |=  wat=@t
      :+  ~  ~
      :-  %noun
      !>  ^-  (list ship)
      ?~  addr=(slaw %ux wat)
        ~
      =/  proxies=(list proxy:naive)
        ~[%own %spawn %manage %vote %transfer]
      %+  roll  proxies
      |=  [=proxy:naive ships=(list ship)]
      %+  weld  ships
      ~(tap in (~(get ju own) [proxy u.addr]))
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
    --
  ::
  ++  on-arvo
    |=  [=wire =sign-arvo]
    ^-  (quip card _this)
    ?+    wire  (on-arvo:def wire sign-arvo)
        [%timer ~]
      ?+  +<.sign-arvo  (on-arvo:def wire sign-arvo)
        %wake  =^(cards state on-timer:do [cards this])
      ==
    ::
        [%predict ~]
      ?+    +<.sign-arvo  (on-arvo:def wire sign-arvo)
          %wake
        =.  own.state  canonical-owners:do
        =^  effects  state
          (predicted-state canonical-state):do
        [(emit effects) this(derive &)]
      ==
    ::
        [%resend @ @ ~]
      =/  [address=@ux nonce=@ud]
        [(slav %ux i.t.wire) (rash i.t.t.wire dem)]
      ?+  +<.sign-arvo  (on-arvo:def wire sign-arvo)
        %wake  [(send-roll:do address nonce) this]
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
        [%txs @ ~]     [%give %fact ~ (give-txs i.t.path)]~
        [%points @ ~]  [%give %fact ~ (give-points i.t.path)]~
    ==
    ::
    ++  give-points
      |=  wat=@t
      ^-  cage
      :-  %points
      !>  ^-  (list [ship point:naive])
      ?~  addr=(slaw %ux wat)  ~
      =/  proxies=(list proxy:naive)
        ~[%own %spawn %manage %vote %transfer]
      %+  roll  proxies
      |=  [=proxy:naive points=(list [ship point:naive])]
      %+  weld  points
      ::
      %+  roll  ~(tap in (~(get ju own) [proxy u.addr]))
      |=  [=ship points=_points]
      %+  snoc  points
      [ship (need (get:orp points.pre ship))]
    ::
    ++  give-txs
      |=  wat=@t
      ^-  cage
      :-  %txs
      !>  ^-  (list hist-tx)
      ?~  addr=(slaw %ux wat)  ~
      ?~  hist=(~(get by history) u.addr)  ~
      (tap:orh u.hist)
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
        [(slav %ux address) (rash nonce dem)]
      ?-  -.sign
          %poke-ack
        ?~  p.sign
          %-  (slog leaf+"Send batch thread started successfully" ~)
          [~ this]
        %-  (slog leaf+"{(trip dap.bowl)} couldn't start thread" u.p.sign)
        :_  this
        [(leave:spider:do wire)]~
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
            (on-batch-result:do address nonce %.n^[%error 'thread failed'])
          [cards this]
        ::
            %thread-done
          =+   !<(result=(each @ud [term @t]) q.cage.sign)
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
          ::  cache naive and ownership state
          ::
          =^  nas  own.state
            !<([^state:naive owners] q.cage.sign)
          =^  effects  state
            (predicted-state:do nas)
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
        [(leave:spider:do wire)]~
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
      =/  failed-nonce=@ud  (rash nonce dem)
      ?-  -.sign
          %poke-ack
        ?~  p.sign
          %-  (slog leaf+"Refresh Nonce thread started successfully" ~)
          [~ this]
        %-  (slog leaf+"{(trip dap.bowl)} couldn't start thread" u.p.sign)
        :_  this
        [(leave:spider:do wire)]~
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
::TODO  /lib/sys.hoon?
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
::TODO  /lib/spider.hoon?
++  spider
  |%
  ++  start-thread
    |=  [=wire thread=term arg=vase]
    ^-  (list card)
    =/  =beak    byk.bowl(r da+now.bowl)
    =/  tid=@ta  (rap 3 thread '--' (scot %uv eny.bowl) ~)
    =/  args     [~ `tid beak thread arg]
    :~  [%pass wire %agent [our.bowl %spider] %watch /thread-result/[tid]]
        [%pass wire %agent [our.bowl %spider] %poke %spider-start !>(args)]
    ==
  ::
  ++  leave
    |=  =path
    ^-  card
    [%pass path %agent [our.bowl %spider] %leave ~]
  --
::
::
++  emit
  |=  updates=(list update)
  |-  ^-  (list card)
  ?~  updates  ~
  =*  up          i.updates
  =/  [address=@t last-owner=(unit @t)]
    ?-    -.up
        %tx
      :_  ~
      (scot %ux address.up)
    ::
        %point
    :-  (scot %ux address.new.up)
    ?~(old.up ~ `(scot %ux address.u.old.up))
    ==
  %+  weld
    $(updates t.updates)
  ^-  (list card)
  ?-  -.i.updates
      %tx
    [%give %fact ~[/txs/[address]] tx+!>(roll-tx.up)]~
  ::
      %point
    %+  weld
      [%give %fact ~[/points/[address]] point+!>([ship point]:up)]~
    ?~  last-owner  ~
    [%give %fact ~[/points/[u.last-owner]] point+!>([ship point]:up)]~
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
::  +canonical-state: current l2 state from /app/azimuth
::
++  canonical-state
  .^  ^state:naive
    %gx
    (scot %p our.bowl)
    %azimuth
    (scot %da now.bowl)
    /nas/noun
  ==
::  +canonical-owners: current azimuth point ownership
::
++  canonical-owners
  .^  owners
    %gx
    (scot %p our.bowl)
    %azimuth
    (scot %da now.bowl)
    /own/noun
  ==
::  +predicted-state
::
::    derives predicted state from applying pending/sending txs to
::    the canonical state, discarding invalid txs in the process.
::
++  predicted-state
  |=  nas=^state:naive
  ^-  (quip update _state)
  =:  pre  nas
      own  canonical-owners
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
    =+  sorted=(tap:ors sending)
    |-  ^+  [[valid ups] state]
    ?~  sorted  [[valid ups] state]
    ::
    =*  key  key.i.sorted
    =*  val  val.i.sorted
    =+  txs=(turn txs.val |=(=raw-tx:naive [| 0x0 *time raw-tx]))
    =^  [new-valid=_txs nups=_ups]  state
      (apply-txs txs %sending `nonce.key)
    ::  we only hear updates for this nonce if it has been sent
    ::
    =.  valid  ::=?  valid  sent.val
      %^  put:ors  valid
        key
      ::  TODO: too much functional hackery?
      val(txs (turn new-valid (cork tail (cork tail tail))))
    $(sorted t.sorted, ups (welp ups nups))
  ::
  ++  apply-txs
    |=  [txs=(list pend-tx) type=?(%pending %sending) nonce=(unit @ud)]
    =/  valid=_txs  ~
    =|  ups=(list update)
    |-  ^+  [[valid ups] state]
    ?~  txs  [[(flop valid) ups] state]
    ::
    =*  tx       i.txs
    =*  raw-tx   raw-tx.i.txs
    =*  ship     ship.from.tx.raw-tx.i.txs
    =/  =keccak  (hash-raw-tx:lib raw-tx)
    =/  sign-address=(unit @ux)
      (extract-address:lib raw-tx pre chain-id)
    =^  [gud=? nups=_ups]  state
      (try-apply pre force.tx raw-tx)
    ::  TODO: only replace address if !=(address.tx sign-address)?
    ::
    =?  tx  &(gud ?=(^ sign-address))
      tx(address u.sign-address)
    =/  =roll-tx  [ship type keccak (l2-tx +<.tx.raw-tx)]
    =?  nups  !gud
      %+  snoc  nups
      [%tx address.tx roll-tx(status %failed)]
    =?  valid     gud  [tx valid]
    =?  history  !gud
      =/  =time
        ?:  ?=(%pending type)  time.tx
        =+  wer=(~(got by finding) keccak)
        ?>(?=(^ wer) time.wer)
      =+  txs=(~(got by history) address.tx)
      =.  txs  +:(del:orh txs time)
      %+  ~(put by history)  address.tx
      %+  put:orh  txs
      [time roll-tx(status %failed)]
    =?  finding  !gud  (~(put by finding) keccak %failed)
    $(txs t.txs, ups (weld ups nups))
  ::
  ++  try-apply
    |=  [nas=^state:naive force=? =raw-tx:naive]
    ^-  [[? ups=(list update)] _state]
    =/  [success=? predicted=_nas ups=(list update) owners=_own]
      (apply-raw-tx:dice force raw-tx nas own chain-id)
    :-  [success ups]
    state(pre predicted, own owners)
  --
::
++  get-l1-address
  |=  [=tx:naive nas=^state:naive]
  ^-  (unit address:ethereum)
  ?~  point=(get:orp points.nas ship.from.tx)  ~
  =<  `address
  (proxy-from-point:naive proxy.from.tx u.point)
::
++  on-action
  |=  =action
  ^-  (quip card _state)
  ?-  -.action
    %commit  on-timer
    %config  (on-config +.action)
    %cancel  (cancel-tx +.action)
  ::
      %submit
    %-  take-tx
    :*  force.action
        address.action
        now.bowl
        sig.action
        (part-tx-to-full tx.action)
    ==
  ==
::
++  on-config
  |=  =config
  ^-  (quip card _state)
  ?-  -.config
    %frequency  [~ state(frequency frequency.config)]
  ::
      %endpoint
    :-  ~
    =/  [contract=@ux chain-id=@]
      =<  [naive chain-id]
      =,  azimuth
      ?-  net.config
        %mainnet  mainnet-contracts
        %ropsten  ropsten-contracts
        %local    local-contracts
      ==
    %_  state
      contract   contract
      chain-id   chain-id
      endpoint   `endpoint.config
    ==
  ::
      %setkey
    ?~  pk=(de:base16:mimes:html pk.config)
      `state
    [(get-nonce q.u.pk /nonce) state(pk q.u.pk)]
  ==
::  TODO: move address to state?
::
++  get-address
  ^-  address:ethereum
  (address-from-prv:key:ethereum pk)
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
      (crip "0x{((x-co:co 20) keccak)}")
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
    =.  txs  +:(del:orh txs u.time)
    %+  ~(put by history)  u.addr
    %^  put:orh  txs
      u.time
    [ship %cancelled keccak l2-tx]
  ==
::  +take-tx: accept submitted l2 tx into the :pending list
::
++  take-tx
  |=  =pend-tx
  ^-  (quip card _state)
  =.  pending  (snoc pending pend-tx)
  =^  cards  history  (update-history [pend-tx]~ %pending)
  ::  toggle derivation
  ::
  :_  state(derive ?:(derive | derive))
  %+  weld  (emit cards)
  ?.  derive  ~
  ::  derive predicted state in 1m.
  ::
  [(wait:b:sys /predict (add update-rate now.bowl))]~
::  +set-timer: %wait until next whole :frequency
::
++  set-timer
  ^-  [=card =time]
  =+  time=(mul +((div now.bowl frequency)) frequency)
  [(wait:b:sys /timer time) time]
::  +on-timer: every :frequency, freeze :pending txs roll and start sending it
::
++  on-timer
  ^-  (quip card _state)
  =^  updates-1  state
    (predicted-state canonical-state)
  =^  cards  state
    ?:  =(~ pending)
      ~?  lverb  [dap.bowl %pending-empty]  [~ state]
    ?~  next-nonce
      ~?  lverb  [dap.bowl %missing-roller-nonce]  [~ state]
    ::  this guarantees that next-nonce is only incremented
    ::  when the thread that's sending the previous batch
    ::  has come back and confirms that it was sent to L1
    ::
    ?:  out-of-sync
      ::  this would postpone sending the batch for a whole "frequency"
      ::  TODO: set up a timer to retry this in ~mX ?
      ::
      ~?  lverb  [dap.bowl %nonce-out-sync]  [~ state]
    =/  nonce=@ud   u.next-nonce
    =^  updates-2  history  (update-history pending %sending)
    =:  pending     ~
        derive      &
        next-nonce  `+(u.next-nonce)
      ::
          sending
        %^  put:ors  sending
          [get-address nonce]
        [0 | (turn pending (cork tail (cork tail tail)))]
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
      (send-roll get-address nonce)
    ==
  =^  card  next-batch  set-timer
  [[card cards] state]
::
++  update-history
  |=  [txs=(list pend-tx) =status]
  ^-  [(list update) _history]
  %+  roll  txs
  |=  [pend-tx ups=(list update) sih=_history]
  =/  =roll-tx
    :*  ship.from.tx.raw-tx
        status
        (hash-raw-tx:lib raw-tx)
        (l2-tx +<.tx.raw-tx)
    ==
  =/  txs=(tree hist-tx)
    ?~  txs=(~(get by sih) address)  ~
    u.txs
  =?  txs  ?=(^ txs)  +:(del:orh txs time)
  :-  (snoc ups tx+[address roll-tx])
  %+  ~(put by sih)  address
  (put:orh txs [time roll-tx])
::  +get-nonce: retrieves the latest nonce
::
++  get-nonce
  |=  [pk=@ =wire]
  ^-  (list card)
  ?~  endpoint  ~?(lverb [dap.bowl %no-endpoint] ~)
  (start-thread:spider wire [%roller-nonce !>([u.endpoint pk])])
::  +out-of-sync: checks if the previous nonce has been sent
::
++  out-of-sync
  ^-  ?
  ?~  newest-batch=(ram:ors sending)  |
  !=(sent.val.u.newest-batch &)
::  +on-out-of-sync
::
++  on-out-of-sync
  |=  [nonce=@ud failed-nonce=@ud]
  ::  we only care about nonces >= than the one that failed
  ::
  =/  failed-sending=(list [l1-tx-pointer send-tx])
    %-  tap:ors
    ::  (range exclusive)
    ::
    (lot:ors sending [`[get-address (dec failed-nonce)] ~])
  =/  confirmed-sending=_sending
    (lot:ors sending [~ `[get-address failed-nonce]])
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
      ~&  ["weird case" nonce+nonce.p]
      [new-nonce sending finding history]
    :+  +(new-nonce)
      update-sending
    process-l2-txs
    ::
    ++  update-sending
      (put:ors sending [p(nonce new-nonce) q(sent %.n)])
    ::
    ++  process-l2-txs
      %+  roll  txs.q
      |=  [=raw-tx:naive nif=_finding sih=_history]
      =/  =keccak  (hash-raw-tx:lib raw-tx)
      |^
      ?~  val=(~(get by nif) keccak)
        [nif sih]
      ?.  ?=(^ u.val)  [nif sih]
      :-  (update-finding u.val)
      (update-history time.u.val address.u.val)
      ::
      ++  update-finding
        |=  val=[time l1-tx-pointer]
        ^+  nif
        (~(put by nif) keccak val(nonce.+ new-nonce))
      ::
      ++  update-history
        |=  [=time =address:ethereum]
        ^+  sih
        =*  ship      ship.from.tx.raw-tx
        =/  l2-tx     (l2-tx +<.tx.raw-tx)
        =/  =roll-tx  [ship %sending keccak l2-tx]
        =+  txs=(~(got by sih) address)
        =.  txs  +:(del:orh txs time)
        %+  ~(put by sih)  address
        (put:orh txs [time roll-tx])
      --
    --
  =:  sending     nes
      finding     nif
      history     sih
      next-nonce  `+(nonce)
    ==
  [(send-roll get-address nonce) state]
::  +send-roll: start thread to submit roll from :sending to l1
::
++  send-roll
  |=  [=address:ethereum =nonce:naive]
  ^-  (list card)
  ::  if this nonce isn't in the sending queue anymore, it's done
  ::
  ?.  (has:ors sending [address nonce])
    ~?  lverb  [dap.bowl %done-sending [address nonce]]
    ~
  ?~  endpoint
    ~?  lverb  [dap.bowl %no-endpoint]
    ~
  ::  start the thread, passing in the l2 txs to use
  ::  TODO  should go ahead and set resend timer in case thread hangs, or nah?
  ::
  %+  start-thread:spider
    /send/(scot %ux address)/(scot %ud nonce)
  :-  %roller-send
  !>  ^-  rpc-send-roll
  :*  u.endpoint
      contract
      chain-id
      pk
      nonce
    ::
      =<  [next-gas-price txs]
      (got:ors sending [address nonce])
  ==
::  +on-batch-result: await resend after thread success or failure
::
++  on-batch-result
  |=  [=address:ethereum nonce=@ud result=(each @ud [term @t])]
  ^-  (quip card _state)
  ::  print error if there was one
  ::
  ~?  ?=(%| -.result)  [dap.bowl %send-error +.p.result]
  =/  =send-tx  (got:ors sending [address nonce])
  =?  sending  ?=(%& -.result)
    %^  put:ors  sending
      [address nonce]
    ::  update gas price for this tx in state
    ::  and set it as sent to L1
    ::
    send-tx(next-gas-price p.result, sent &)
  :_  state
  ?:  ?|  ?=(%& -.result)
        ::  a general error shouldn't innitiate
        ::  the out-of-sync nonce thread
        ::
          ?=([%| %error *] result)
        ::  this accounts for a resend with higher gas
        ::  for a previous nonce, so we shouldn't start
        ::  the out-of-sync nonce thread
        ::
          ?&  sent.send-tx
              ?=([%| %not-sent *] result)
      ==  ==
    :_  ~
    ::  resend the l1 tx in five minutes
    ::
    %+  wait:b:sys
      /resend/(scot %ux address)/(scot %ud nonce)
    (add resend-time now.bowl)
  ::  TODO: this only accounts for the case where the nonce is out of sync,
  ::  reaching this because of lower funds needs to be addressed manually
  ::
  ?>  ?=(%not-sent -.p.result)
  (get-nonce pk.state /refresh-nonce/(scot %ud nonce))
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
    ::  derive predicted/ownership state in 1m.
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
  =*  nonce    nonce.u.wer
  =*  address  address.u.wer
  =*  ship     ship.from.tx.raw-tx.diff
  =*  time     time.u.wer
  =*  tx       tx.raw-tx.diff
  =/  l2-tx    (l2-tx +<.tx)
  ::  remove the tx from the sending map
  ::
  =.  sending
    ?~  sen=(get:ors sending [get-address nonce])
      ~?  lverb  [dap.bowl %weird-double-remove nonce+nonce]
      sending
    ?~  nin=(find [raw-tx.diff]~ txs.u.sen)
      ~?  lverb  [dap.bowl %weird-unknown nonce+nonce]
      sending
    =.  txs.u.sen  (oust [u.nin 1] txs.u.sen)
    ?~  txs.u.sen
      ~?  lverb  [dap.bowl %done-with-nonce [get-address nonce]]
      =^  *  sending
        (del:ors sending [get-address nonce])
      sending
    ^+  sending
    (put:ors sending [get-address nonce] u.sen)
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
  =^  updates  history
    %+  update-history
      [| address time raw-tx.diff]~
    ?~(err.diff %confirmed %failed)
  [(emit updates) state]
::
--
