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
      ::  finding: raw-tx-hash reverse lookup per nonce in sending map
      ::  history: status of l2 txs by ethereum address
      ::  next-nonce: next l1 nonce to use
      ::  next-batch: when then next l2 batch will be sent
      ::  pre: predicted l2 state
      ::  own: ownership of azimuth points
      ::  derive: deferred derivation of predicted/ownership state
      ::
      pending=(list pend-tx)
      sending=(tree [l1-tx-pointer send-tx])
    ::
      $=  finding
      ::  nonce is a unit to account for the case where a roller
      ::  operator hasn't provided a pk, and some pending transactions
      ::  that will be marked as "%failed" have to go to "finding" as such,
      ::  but we don't know the nonce for the batch they'd belong to
      ::
      %+  map  [=keccak nonce=(unit @ud)]
      ?(%confirmed %failed [=time =address:ethereum])
    ::
      history=(jug address:ethereum roll-tx)
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
::
++  orm   ((ordered-map l1-tx-pointer send-tx) nonce-order:dice)
+$  ini   [nas=^state:naive own=owners]
+$  net   ?(%mainnet %ropsten %local)
::
+$  l2-status
  ?(%confirmed %failed [=time =address:ethereum])
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
  ::    /x/history/[0xadd.ress]        ->  %noun  (list roll-tx)
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
      [%x %tx @ @ %status ~]  (status i.t.t.path i.t.t.t.path)
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
      |=  wat=[@t @t]
      ?~  keccak=(slaw %ux -.wat)
        [~ ~]
      ?~  nonce=(slaw %ud +.wat)
        [~ ~]
      :+  ~  ~
      :-  %noun
      !>  ^-  tx-status
      ?^  status=(~(get by finding) [u.keccak nonce])
        ?@  u.status  [u.status ~]
        [%sending `[+.u.status u.nonce]]
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
      !>  ^-  (list roll-tx)
      ?~  addr=(slaw %ux wat)  ~
      %~  tap  in
      (~(get ju ^history) u.addr)
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
      ?~  point=(get:orm:naive points.pre u.who)
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
      =,  orm:naive
      ::  range exclusive [star first-moon-last-planet]
      ::
      %-  tap
      (lot points.pre [`u.star `(cat 3 u.star 0x1.ffff)])
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
        =,  orm:naive
        %-  tap
        (lot points.pre [`u.star `(cat 3 u.star 0x1.ffff)])
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
      ``noun+!>((get:orm:naive points.pre u.ship))
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
      [ship (need (get:orm:naive points.pre ship))]
    ::
    ++  give-txs
      |=  wat=@t
      ^-  cage
      :-  %txs
      !>  ^-  (list roll-tx)
      ?~  addr=(slaw %ux wat)  ~
      %~  tap  in
      (~(get ju history) u.addr)
    --
  ::
  ++  on-leave  on-leave:def
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
          =^  nas  own.state  !<(ini q.cage.sign)
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
      =/  old-nonce=@ud  (rash nonce dem)
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
          ::  we only care about nonces >= than the one we received
          ::
          =/  sorted
            %-  tap:orm
            ::  (range exclusive)
            ::
            (lot:orm sending [`[get-address:do (dec old-nonce)] ~])
          ~&  sort-lent+(lent sorted)
          =/  confirmed-sending=_sending
            (lot:orm sending [~ `[get-address:do old-nonce]])
          =/  [nes=_sending nif=_finding]
            =<  [sending finding]
            %+  roll  sorted
            |=  $:  [p=l1-tx-pointer q=send-tx]
                    new-nonce=_nonce
                    sending=_confirmed-sending
                    finding=_finding
                ==
            |^
            =*  nonce  nonce.p
            =*  txs    txs.q
            ::  TODO: this shouldn't be needed
            ?:  (lth nonce.p old-nonce)
              ~&  ["weird case" nonce+nonce.p]
              [new-nonce sending finding]
            :+  +(new-nonce)
              update-sending
            update-finding
            ::
            ++  update-sending
              (put:orm sending [p(nonce new-nonce) q(sent %.n)])
            ::
            ++  update-finding
              %+  roll  txs.q
              |=  [=raw-tx:naive finding=_finding]
              =/  hash=keccak
                (hash-raw-tx:lib raw-tx)
              =/  old-key=[keccak (unit @ud)]
                [hash `nonce.p]
              =/  new-key=[keccak (unit @ud)]
                [hash `new-nonce]
              ?~  val=(~(get by finding) old-key)
                finding
              ?.  ?=(^ u.val)  finding
              %.  [new-key u.val]
              ~(put by (~(del by finding) old-key))
            --
          ::
          =:  sending     nes
              finding     nif
              next-nonce  `+(nonce)
            ==
          [(send-roll get-address nonce) this]
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
    =/  tid=@ta  (rap 3 thread '--' (scot %uv eny.bowl) ~)
    =/  args     [~ `tid thread arg]
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
  =:  pre.state  nas
      own.state  canonical-owners
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
    =+  sorted=(tap:orm sending)
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
      %^  put:orm  valid
        key
      ::  TODO: too much functional hackery?
      val(txs (turn new-valid (cork tail (cork tail tail))))
    $(sorted t.sorted, ups (welp ups nups))
  ::
  ++  apply-txs
    |=  [txs=(list pend-tx) type=?(%pending %sending) nonce=(unit @ud)]
    =/  valid=_txs  ~
    =|  ups=(list update)
    =|  local=(set keccak)
    |-  ^+  [[valid ups] state]
    ?~  txs  [[valid ups] state]
    ::
    =*  tx        i.txs
    =*  raw-tx    raw-tx.i.txs
    =*  ship      ship.from.tx.raw-tx.i.txs
    =*  time      time.i.txs
    =/  hash=@ux  (hash-raw-tx:lib raw-tx)
    =/  sign-address=(unit @ux)
      (extract-address:lib raw-tx pre.state chain-id)
    =^  [gud=? nups=_ups]  state
      (try-apply pre.state force.tx raw-tx)
    ::  TODO: only replace address if !=(address.tx sign-address)?
    ::
    =?  tx  &(gud ?=(^ sign-address))
      tx(address u.sign-address)
    ::
    =/  =roll-tx
      [ship type hash time (l2-tx +<.tx.raw-tx)]
    =?  nups  !gud
      %+  snoc  nups
      [%tx address.tx roll-tx(status %failed)]
    =?  valid  gud  (snoc valid tx)
    =?  finding.state  !gud
      (~(put by finding) [hash nonce] %failed)
    ::
    =?  history.state  !gud
      %.  [address.tx roll-tx(status %failed)]
      ~(put ju (~(del ju history.state) address.tx roll-tx))
    $(txs t.txs, ups (weld ups nups), local (~(put in local) hash))
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
  ?~  point=(get:orm:naive points.nas ship.from.tx)  ~
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
  ::  TODO: use an index for tracking pending-tx?
  ::
  :: ?^  status=(~(get by finding) keccak next-nonce)
  ::   ~?  lverb  [dap.bowl %tx-not-pending status+u.status]
  ::   [~ state]
  =^  time  pending
    =|  nep=(list pend-tx)
    |-  ^-  [(unit time) _nep]
    ?~  pending  [~ nep]
    ?:  =(keccak (hash-raw-tx:lib raw-tx.i.pending))
      [`time.i.pending (weld (flop nep) t.pending)]
    $(pending t.pending, nep [i.pending nep])
  ?~  time
    ~?  lverb  [dap.bowl %tx-not-pending]
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
  ::  TODO: mark as failed instead? add a %cancelled to tx-status?
  ::
  =.  history
    %+  ~(del ju history)  u.addr
    [ship %pending keccak u.time l2-tx]
  [~ state]
::  +take-tx: accept submitted l2 tx into the :pending list
::
++  take-tx
  |=  pend-tx
  ^-  (quip card _state)
  =/  hash=@ux  (hash-raw-tx:lib raw-tx)
  ::  TODO: what if this hash/tx is already in the history?
  ::    e.g. if previously failed, but now it will go through
  ::    a) check in :finding that hash doesn't exist and if so, skip ?
  ::    b) extract the status from :finding, use it to delete
  ::      the entry in :history, and then insert it as %pending ?
  ::
  :: =/  not-sent=?  !(~(has by finding) hash)
  :: =?  pending  not-sent
  =.  pending  (snoc pending [force address time raw-tx])
  :: =?  history  not-sent
  =^  update-cards  history
    =/  =roll-tx
      :*  ship.from.tx.raw-tx
          %pending
          hash
          time
          (l2-tx +<.tx.raw-tx)
      ==
    :-  [%tx address roll-tx]~
    (~(put ju history) [address roll-tx])
  :: ?.  not-sent  ~&  "skip"  [~ state]
  ::  toggle derivation
  ::
  :_  state(derive ?:(derive | derive))
  %+  weld  (emit update-cards)
  ?.  derive  ~
  ::  derive predicted state in 1m.
  ::
  [(wait:b:sys /predict (add ~m1 now.bowl))]~
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
    ::  when the thread that handles sending the previous one
    ::  has come back and confirms that it was sent to L1
    ?:  out-of-sync
      ::  TODO: this would postpone sending the batch for a whole "frequency"
      ::    set up a timer to retry this in ~mX ?
      ::
      ~?  lverb  [dap.bowl %nonce-out-sync]  [~ state]
    =/  nonce=@ud   u.next-nonce
    =^  updates-2  history  update-history
    ::  TODO: move to +on-batch-result to prevent the case the
    ::  tx succeds but we get a "Runtime Error: revert"?
    ::
    =:  pending     ~
        derive      &
        next-nonce  `+(u.next-nonce)
      ::
          sending
        %^  put:orm  sending
          [get-address nonce]
        [0 | (turn pending (cork tail (cork tail tail)))]
      ::
          finding
        %-  ~(gas by finding)
        %+  turn  pending
        |=  pend-tx
        :_  [time address]
        [(hash-raw-tx:lib raw-tx) `nonce]
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
  %+  roll  pending
  |=  [pend-tx ups=(list update) his=_history]
  =/  =roll-tx
    :*  ship.from.tx.raw-tx
        %pending
        (hash-raw-tx:lib raw-tx)
        time
        (l2-tx +<.tx.raw-tx)
    ==
  =+  tx=[address roll-tx(status %sending)]
  :-  (snoc ups tx+tx)
  %.  tx
  ~(put ju (~(del ju his) address roll-tx))
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
  ?~  newest-batch=(ram:orm sending)  |
  !=(sent.val.u.newest-batch &)
::  +send-roll: start thread to submit roll from :sending to l1
::
++  send-roll
  |=  [=address:ethereum =nonce:naive]
  ^-  (list card)
  ::  if this nonce isn't in the sending queue anymore, it's done
  ::
  ?.  (has:orm sending [address nonce])
    ~?  lverb  [dap.bowl %done-sending [address nonce]]
    ~
  ::  start the thread, passing in the l2 txs to use
  ::
  ?~  endpoint
    ~?  lverb  [dap.bowl %no-endpoint]
    ~
  ::TODO  should go ahead and set resend timer in case thread hangs, or nah?
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
      (got:orm sending [address nonce])
  ==
::  +on-batch-result: await resend after thread success or failure
::
++  on-batch-result
  |=  [=address:ethereum nonce=@ud result=(each @ud [term @t])]
  ^-  (quip card _state)
  ::  print error if there was one
  ::
  ~?  ?=(%| -.result)  [dap.bowl %send-error +.p.result]
  =/  =send-tx  (got:orm sending [address nonce])
  =?  sending  ?=(%& -.result)
    %^  put:orm  sending
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
    [(wait:b:sys /predict (add ~m1 now.bowl))]~
  ::
  ?:  ?=(%point -.diff)  [~ state]
  ?>  ?=(%tx -.diff)
  =/  =keccak  (hash-raw-tx:lib raw-tx.diff)
  ::  assumes that l2 txs come in order, belonging to the oldest nonce in sending
  ::
  ?~  oldest-batch=(pry:orm sending)
    ~?  lverb  [dap.bowl %no-nonce-in-sending keccak]
    [~ state]
  =*  nonce  nonce.key.u.oldest-batch
  ?~  wer=(~(get by finding) [keccak `nonce])
    ::  tx not submitted by this roller
    ::
    [~ state]
  ?@  u.wer
    ~?  &(?=(%confirmed u.wer) ?=(~ err.diff))
      [dap.bowl %weird-double-confirm from.tx.raw-tx.diff]
    [~ state]
  =*  address  address.u.wer
  =*  ship     ship.from.tx.raw-tx.diff
  =*  time     time.u.wer
  =*  tx       tx.raw-tx.diff
  =/  l2-tx    (l2-tx +<.tx)
  ::  remove the tx from the sending map
  ::
  =.  sending
    ?~  sen=(get:orm sending [get-address nonce])
      ~?  lverb  [dap.bowl %weird-double-remove nonce+nonce]
      sending
    ?~  nin=(find [raw-tx.diff]~ txs.u.sen)
      ~?  lverb  [dap.bowl %weird-unknown nonce+nonce]
      sending
    =.  txs.u.sen  (oust [u.nin 1] txs.u.sen)
    ?~  txs.u.sen
      ~?  lverb  [dap.bowl %done-with-nonce [get-address nonce]]
      =^  *  sending
        (del:orm sending [get-address nonce])
      sending
    ^+  sending
    (put:orm sending [get-address nonce] u.sen)
  ::  update the finding map with the new status
  ::
  =.  finding
    %+  ~(put by finding)  [keccak `nonce]
    ?~  err.diff  %confirmed
    ::  if we kept the forced flag around for longer, we could notify of
    ::  unexpected tx failures here. would that be useful? probably not?
    ::  ~?  !forced  [dap.bowl %aggregated-tx-failed-anyway err.diff]
    %failed
  ::
  =^  updates  history
    =/  =roll-tx  [ship %sending keccak time l2-tx]
    =.  history
      (~(del ju history) address roll-tx)
    =.  status.roll-tx
      ?~(err.diff %confirmed %failed)
    :-  [%tx address roll-tx]~
    (~(put ju history) [address roll-tx])
  [(emit updates) state]
::
--
