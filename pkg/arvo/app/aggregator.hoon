::  aggregator: Azimuth L2 roll aggregator
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
::TODO  remaining general work:
::
::TODO  questions:
::  - it's a bit weird how we just assume the raw and tx in raw-tx to match...
::
/-  *aggregator
/+  azimuth,
    naive,
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
      ::  sending: the l2 txs currently sending/awaiting l2 confirmation
      ::  finding: raw-tx-hash reverse lookup for sending map
      ::  history: status of l2 txs by ethereum address
      ::  next-nonce: next l1 nonce to use
      ::  next-batch: when then next l2 batch will be sent
      ::  pre: predicted l2 state
      ::  flush: flag for deriving predicted state
      ::
      pending=(list pend-tx)
    ::
      $=  sending
      %+  map  l1-tx-pointer
      [next-gas-price=@ud txs=(list raw-tx:naive)]
    ::
      finding=(map keccak $?(%confirmed %failed l1-tx-pointer))
      history=(jug address:ethereum roller-tx)
      next-nonce=(unit @ud)
      next-batch=time
      pre=^state:naive
      flush=?
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
+$  config
  $%  [%frequency frequency=@dr]
      [%setkey pk=@]
      [%endpoint endpoint=@t]
      [%network net=?(%mainnet %ropsten %local)]
  ==
::
+$  action
  $%  [%submit force=? sig=@ tx=part-tx]
      [%cancel sig=@ keccak=@ =l2-tx]
      [%commit ~]  ::TODO  maybe pk=(unit @) later
      [%config config]
  ==
::
+$  card  card:agent:gall
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
        [%pass /azimuth-txs %agent [our.bowl %azimuth] %watch /txs]
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
          %aggregator-action
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
  ::    /x/nonce/[~ship]/[proxy]       ->  %noun  (unit @)
  ::    /x/spawned/[~ship]             ->  %noun  (list [ship address])
  ::    /x/next-batch                  ->  %atom  time
  ::
  ++  on-peek
    |=  =path
    ^-  (unit (unit cage))
    |^
    ?+  path  ~
      [%x %pending ~]       ``noun+!>(pending)
      [%x %pending @ ~]     (pending-by i.t.t.path)
      [%x %tx @ %status ~]  (status i.t.t.path)
      [%x %history @ ~]     (history i.t.t.path)
      [%x %nonce @ @ ~]     (nonce i.t.t.path i.t.t.t.path)
      [%x %spawned @ ~]     (spawned i.t.t.path)
      [%x %next-batch ~]    ``noun+!>(next-batch)
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
        =(u.wer (get-l1-address tx.raw-tx pre))
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
        [%sending status]
      ::TODO  potentially slow!
      =;  known=?
        [?:(known %pending %unknown) ~]
      %+  lien  pending
      |=  pend-tx
      =(u.keccak (hash-tx raw.raw-tx))
    ::
    ++  history
      |=  wat=@t
      :+  ~  ~
      :-  %noun
      !>  ^-  (list roller-tx)
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
      =<  `nonce
      (proxy-from-point:naive proxy u.point)
    ::
    ++  spawned
      |=  wat=@t
      :+  ~  ~
      :-  %noun
      !>  ^-  (list [=^ship =address:ethereum])
      ?~  star=(slaw %p wat)  ~
      =/  range
        %+  lot:orm:naive  points.pre
        ::  range exclusive [star first-planet-next-star]
        ::  TODO: make range inclusive? [first-planet last-planet]
        ::
        [`u.star `(cat 3 +(u.star) 0x1)]
      %+  turn  (tap:orm:naive range)
      |=  [=ship =point:naive]
      ^-  [=^ship =address:ethereum]
      :-  ship
      address:(proxy-from-point:naive %own point)
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
        =^  pending  pre  (predicted-state:do canonical-state:do)
        [~ this(pending pending, flush &)]
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
  ++  on-watch  on-watch:def
  ++  on-leave  on-leave:def
  ++  on-agent
    |=  [=wire =sign:agent:gall]
    ^-  (quip card _this)
    |^
    ?+  wire  (on-agent:def wire sign)
      [%send @ @ *]     (send-batch i.t.wire i.t.t.wire sign)
      [%azimuth-txs ~]  (azimuth-update sign)
      [%nonce ~]        (nonce sign)
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
            (on-batch-result:do address nonce %.n^'thread failed')
          [cards this]
        ::
            %thread-done
          =+   !<(result=(each @ud @t) q.cage.sign)
          =^  cards  state
            (on-batch-result:do address nonce result)
          [cards this]
        ==
      ==
    ::
    ++  azimuth-update
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
          ::  cache naive state, received upon innitializing subscription
          ::
          ~&  >  %get-naive-state
          ::  this assumes that %azimuth has already processed eth data
          ::
          =^  pending  pre
            (predicted-state:do !<(^state:naive q.cage.sign))
          [~ this(pending pending)]
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
++  hash-tx  keccak-256:keccak:crypto
::
++  hash-raw-tx
  |=  =raw-tx:naive
  ^-  @ux
  (hash-tx raw.raw-tx)
::
++  part-tx-to-full
  |=  =part-tx
  ^-  [octs tx:naive]
  ?-    -.part-tx
      %raw
    ?~  batch=(parse-raw-tx:naive q.raw.part-tx)
      ~&  %parse-failed
      ::  TODO: maybe return a unit if parsing fails?
      ::
      !!
    [raw tx]:-.u.batch
  ::
    %don  [(gen-tx-octs:lib +.part-tx) +.part-tx]
    %ful  +.part-tx
  ==
::  +canonical-state: load current l2 state instead
::
++  canonical-state
  .^  ^state:naive
    %gx
    (scot %p our.bowl)
    %azimuth
    (scot %da now.bowl)
    /nas/nas
  ==
::  +predicted-state
::
::    derives predicted state from pending/sending txs and
::    canonical state, discarding invalid txs in the process.
::
++  predicted-state
  |=  nas=^state:naive
  ^-  [_pending _nas]
  |^
  =^  new-sending  nas  apply-sending
  =.  sending  new-sending
  (update-txs pending)
  ::
  ++  apply-sending
    =|  valid=_sending
    =+  sending=~(tap by sending)
    |-  ^+  [valid nas]
    ?~  sending  [valid nas]
    ::
    =*  key  p.i.sending
    =*  val  q.i.sending
    =^  new-valid  nas
      %-  update-txs
      (turn txs.val |=(=raw-tx:naive [| 0x0 raw-tx]))
    =.  valid
      %+  ~(put by valid)  key
      val(txs (turn new-valid (cork tail tail)))
    $(sending t.sending)
  ::
  ++  update-txs
    |=  txs=(list pend-tx)
    =/  valid=_txs  ~
    |-  ^+  [valid nas]
    ?~  txs  [valid nas]
    =*  tx  i.txs
    =^  gud=?  nas  (try-apply nas [force raw-tx]:tx)
    =?  valid  gud  (snoc valid tx)
    =?  finding  =(gud %.n)
      %-  ~(put by finding)
      [(hash-raw-tx raw-tx.tx) %failed]
    $(txs t.txs)
  ::
  ++  try-apply
    |=  [nas=^state:naive force=? =raw-tx:naive]
    ^-  [success=? _nas]
    =/  chain-t=@t  (ud-to-ascii:naive chain-id)
    ?.  (verify-sig-and-nonce:naive verifier:lib chain-t nas raw-tx)
      ~&  [%verify-sig-and-nonce %failed]
      [force nas]
    ::
    =^  *  points.nas
      (increment-nonce:naive nas from.tx.raw-tx)
    ::
    ?~  nex=(receive-tx:naive nas tx.raw-tx)
      [force nas]
    [& +.u.nex]
  --
::
++  get-l1-address
  |=  [=tx:naive nas=^state:naive]
  ^-  address:ethereum
  ?~  point=(get:orm:naive points.nas ship.from.tx)
    !!
  =<  address
  (proxy-from-point:naive proxy.from.tx u.point)
::
++  on-action
  |=  =action
  ^-  (quip card _state)
  ?-  -.action
    %commit  on-timer
    %config  (on-config +.action)
    %cancel  (cancel-tx +.action)
    %submit  (take-tx force.action sig.action (part-tx-to-full tx.action))
  ==
::
++  on-config
  |=  =config
  ^-  (quip card _state)
  ?-  -.config
    %frequency  [~ state(frequency frequency.config)]
    %endpoint   [~ state(endpoint `endpoint.config)]
  ::
      %network
    :-  ~
    =/  [contract=@ux chain-id=@]
      =<  [naive chain-id]
      =,  azimuth
      ?-  net.config
        %mainnet  mainnet-contracts
        %ropsten  ropsten-contracts
        %local    local-contracts
      ==
    state(contract contract, chain-id chain-id)
  ::
      %setkey
    ?~  pk=(de:base16:mimes:html pk.config)
      `state
    [(get-nonce q.u.pk) state(pk q.u.pk)]
  ==
::  TODO: move address to state?
::
++  get-address
  ^-  address:ethereum
  (address-from-prv:key:ethereum pk)
::  +cancel-tx: cancel a pending transaction
::
++  cancel-tx
  |=  [sig=@ =keccak =l2-tx]
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
  ?~  addr=(verify-sig sig message)
    ~?  lverb  [dap.bowl %cancel-sig-fail]
    [~ state]
  =.  history
    %+  ~(del ju history)  u.addr
    [[%pending ~] keccak l2-tx]
  =.  pending
    %+  skip  pending
    |=  pend-tx
    =(keccak (hash-raw-tx raw-tx))
  [~ state]
::  TODO: move to /lib/naive-transactions
::
++  verify-sig
  |=  [sig=@ txdata=octs]
  ^-  (unit address:naive)
  |^
  ::  Reversed of the usual r-s-v order because Ethereum integers are
  ::  big-endian
  ::
  =^  v  sig  (take 3)
  =^  s  sig  (take 3 32)
  =^  r  sig  (take 3 32)
  ::  In Ethereum, v is generally 27 + recid, and verifier expects a
  ::  recid.  Old versions of geth used 0 + recid, so most software
  ::  now supports either format.  See:
  ::
  ::  https://github.com/ethereum/go-ethereum/issues/2053
  ::
  =?  v  (gte v 27)  (sub v 27)
  (verifier:lib txdata v r s)
  ::
  ++  take
    |=  =bite
    [(end bite sig) (rsh bite sig)]
  --
::  +take-tx: accept submitted l2 tx into the :pending list
::
++  take-tx
  |=  [force=? =raw-tx:naive]
  ^-  (quip card _state)
  =/  =address:ethereum
    (get-l1-address tx.raw-tx pre)
  =.  pending  (snoc pending [force address raw-tx])
  =.  history
    %+  ~(put ju history)  address
    :+  [%pending ~]
      (hash-raw-tx raw-tx)
    (l2-tx +<.tx.raw-tx)
  ::  toggle flush flag
  ::
  :_  state(flush ?:(flush | &))
  ?.  flush  ~
  ::  derive predicted state in 5m.
  ::
  [(wait:b:sys /predict (add ~m5 now.bowl))]~
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
  =^  new-pending  pre  (predicted-state canonical-state)
  =.  pending  new-pending
  =^  cards  state
    ?:  =(~ pending)  [~ state]
    ?~  next-nonce
      ~&([dap.bowl %no-nonce] [~ state])
    =/  nonce=@ud   u.next-nonce
    =:  pending     ~
        flush       &
        next-nonce  `+(u.next-nonce)
      ::
          sending
        %+  ~(put by sending)
          [get-address nonce]
        [0 (turn pending (cork tail tail))]
      ::
          finding
        %-  ~(gas by finding)
        %+  turn  pending
        |=  pend-tx
        (hash-raw-tx raw-tx)^[address nonce]
      ::
          history
        %+  roll  pending
        |=  [pend-tx hist=_history]
        =/  tx=roller-tx
          :+  [%pending ~]
            (hash-raw-tx raw-tx)
          (l2-tx +<.tx.raw-tx)
        %+  ~(put ju (~(del ju hist) address tx))
          address
        tx(status [%sending ~])
      ==
    [(send-roll get-address nonce) state]
  =^  card  next-batch  set-timer
  [[card cards] state]
::  +get-nonce: retrieves the latest nonce
::
++  get-nonce
  |=  pk=@
  ^-  (list card)
  ?~  endpoint  ~&([dap.bowl %no-endpoint] ~)
  (start-thread:spider /nonce [%aggregator-nonce !>([u.endpoint pk])])
::
::  +send-roll: start thread to submit roll from :sending to l1
::
++  send-roll
  |=  [=address:ethereum nonce=@ud]
  ^-  (list card)
  ::  if this nonce isn't in the sending queue anymore, it's done
  ::
  ?.  (~(has by sending) [address nonce])
    ~?  lverb  [dap.bowl %done-sending [address nonce]]
    ~
  ::  start the thread, passing in the l2 txs to use
  ::
  ?~  endpoint  ~&([dap.bowl %no-endpoint] ~)
  ::TODO  should go ahead and set resend timer in case thread hangs, or nah?
  %+  start-thread:spider
    /send/(scot %ux address)/(scot %ud nonce)
  :-  %aggregator-send
  !>  ^-  rpc-send-roll
  :*  u.endpoint
      contract
      chain-id
      pk
      nonce
      (~(got by sending) [address nonce])
  ==
::  +on-batch-result: await resend after thread success or failure
::
++  on-batch-result
  |=  [=address:ethereum nonce=@ud result=(each @ud @t)]
  ^-  (quip card _state)
  ::  update gas price for this tx in state
  ::
  =?  sending  ?=(%& -.result)
    %+  ~(jab by sending)  [address nonce]
    (cork tail (lead p.result))
  ::  print error if there was one
  ::
  ~?  ?=(%| -.result)  [dap.bowl %send-error p.result]
  ::  resend the l1 tx in five minutes
  ::
  :_  state
  :_  ~
  %+  wait:b:sys
    /resend/(scot %ux address)/(scot %ud nonce)
  (add resend-time now.bowl)
::  +on-naive-diff: process l2 tx confirmations
::
++  on-naive-diff
  |=  =diff:naive
  ^-  (quip card _state)
  ?.  ?=(%tx -.diff)
    [~ state]
  =/  =keccak  (hash-raw-tx raw-tx.diff)
  ?~  wer=(~(get by finding) keccak)
    [~ state]
  ::  if we had already seen the tx, no-op
  ::
  ?@  u.wer
    ~?  &(?=(%confirmed u.wer) ?=(~ err.diff))
      [dap.bowl %weird-double-confirm from.tx.raw-tx.diff]
    [~ state]
  =*  nonce  nonce.u.wer
  ::  remove the tx from the sending map
  ::
  =.  sending
    ?~  sen=(~(get by sending) [get-address nonce])
      ~&  [dap.bowl %weird-double-remove]
      sending
    ?~  nin=(find [raw-tx.diff]~ txs.u.sen)
      ~&  [dap.bowl %weird-unknown]
      sending
    =.  txs.u.sen  (oust [u.nin 1] txs.u.sen)
    ?~  txs.u.sen
      ~?  lverb  [dap.bowl %done-with-nonce [get-address nonce]]
      (~(del by sending) [get-address nonce])
    (~(put by sending) [get-address nonce] u.sen)
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
  =.  history
    =/  tx=roller-tx
      :+  [%sending ~]
        keccak
      (l2-tx +<.tx.raw-tx.diff)
    =/  =address:ethereum
      (get-l1-address tx.raw-tx.diff pre)
    %+  ~(put ju (~(del ju history) address tx))
      address
    %_  tx
      status  ?~(err.diff [%confirmed ~] [%failed ~])
    ==
  :_  state(flush ?:(flush | &))
  ?.  flush  ~
  ::  derive predicted state in 5m.
  ::
  [(wait:b:sys /predict (add ~m5 now.bowl))]~
::
--
