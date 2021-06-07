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
::  - hook up timer callbacks
::
::TODO  questions:
::  - it's a bit weird how we just assume the raw and tx in raw-tx to match...
::
/-  *aggregator
/+  azimuth, naive, default-agent, ethereum, dbug, verb, lib=naive-transactions
::
|%
+$  state-0
  $:  %0
      ::  pending: the next l2 txs to be sent
      ::  sending: the l2 txs currently sending/awaiting l2 confirmation
      ::  finding: raw-tx-hash reverse lookup for sending map
      ::  next-nonce: next l1 nonce to use
      ::  nas: predicted naive state
      ::
      pending=(list pend-tx)
    ::
      $=  sending
      %+  map  l1-tx-pointer
      [next-gas-price=@ud txs=(list raw-tx:naive)]
    ::
      finding=(map keccak $?(%confirmed %failed l1-tx-pointer))
      next-nonce=@ud
      nas=^state:naive
    ::
      ::  pk: private key to send the roll
      ::  frequency: time to wait between sending batches (TODO fancier)
      ::  endpoint: ethereum rpc endpoint to use
      ::  contract: ethereum contract address
      ::  chain-id: mainnet, ropsten, local (https://chainid.network/)
      ::
      pk=@
      frequency=@dr
      endpoint=@t
      contract=@ux
      chain-id=@
  ==
::
+$  config
  $%  [%frequency frequency=@dr]
      [%setkey pk=@]
      [%endpoint endpoint=@t]
      [%network net=?(%mainnet %ropsten %local)]
      [%nonce nonce=@ud]
  ==
::
+$  action
  $%  [%submit force=? sig=@ tx=part-tx]
      [%cancel sig=@ keccak=@]
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
    ::TODO  set endpoint?
    =.  frequency  ~h1
    =.  contract  naive:local-contracts:azimuth
    =.  chain-id  chain-id:local-contracts:azimuth
    :_  this
    [%pass /azimuth-txs %agent [our.bowl %azimuth] %watch /txs]~
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
  ::TODO  reevaluate wrt recent flow changes
  ::
  ::    /x/pending                     ->  %noun  (list pend-tx)
  ::    /x/pending/[~ship]             ->  %noun  (list pend-tx)
  ::    /x/pending/[0xadd.ress]        ->  %noun  (list pend-tx)
  ::    /x/tx/[0xke.ccak]/status       ->  %noun  tx-status
  ::    /x/nonce/[~ship]/[0xadd.ress]  ->  %atom  @
  ::
  ++  on-peek
    |=  =path
    ^-  (unit (unit cage))
    |^
    ?+  path  ~
      [%x %pending ~]       pending
      [%x %pending @ ~]     (pending-by i.t.t.path)
      [%x %tx @ %status ~]  (status i.t.t.path)
      [%x %nonce @ @ ~]     (nonce i.t.t.path i.t.t.t.path)
      [%x %spawned @ ~]     (spawned i.t.t.path)
    ==
    ::
    ++  pending  ``noun+!>(^pending)
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
        %+  skim  ^pending
        |=  pend-tx
        ::TODO  deduce address from sig.raw-tx ?
        !!
      ::  by-ship
      ::
      =;  pending=(list pend-tx)
        ``noun+!>(pending)
      %+  skim  ^pending
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
      %+  lien  ^pending
      |=  [* raw-tx:naive]
      =(u.keccak (hash-tx raw))
    ::
    ++  nonce
      |=  [who=@t proxy=@t]
      ?~  who=(slaw %p who)
        [~ ~]
      ?.  ?=(proxy:naive proxy)
        [~ ~]
      ::  uses cached naive state
      ::
      ::TODO  or should we ~ when !(~(has by points.nas) who) ?
      =/  =point:naive  (~(gut by points.nas) u.who *point:naive)
      =+  (proxy-from-point:naive proxy point)
      ``atom+!>(nonce)
    ::
    ++  spawned
      |=  wat=@t
      :+  ~  ~
      :-  %noun
      !>  ^-  (list [=^ship =address:ethereum])
      ?~  star=(slaw %p wat)  ~
      %+  murn  ~(tap by points.nas)
      |=  [=ship =point:naive]
      ^-  (unit [=^ship =address:ethereum])
      ?.  =(star (^sein:title ship))  ~
      %-  some
      :-  ship
      address:(proxy-from-point:naive %own point)
    --
  ::
  ++  on-arvo
    |=  [=wire =sign-arvo]
    ^-  (quip card _this)
    ?+  +<.sign-arvo  (on-arvo:def wire sign-arvo)
      %wake  =^(cards state on-timer:do [cards this])
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
          =^  pending  nas
            (predicted-state !<(^state:naive q.cage.sign))
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
          [~ this(next-nonce nonce)]
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
::  +canonical-state: load current state instead of cached state
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
  ::  apply our pending transactions
  ::
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
      ::  prepends force=%.n to all txs
      ::
      (update-txs (turn txs.val (lead |)))
    =.  valid
      %+  ~(put by valid)  key
      val(txs (turn new-valid tail))
    $(sending t.sending)
  ::
  ++  update-txs
    |=  txs=(list [force=? =raw-tx:naive])
    =/  valid=_txs  ~
    |-  ^+  [valid nas]
    ?~  txs  [valid nas]
    =*  tx  i.txs
    =^  gud=?  nas  (try-apply nas tx)
    =?  valid  gud  (snoc valid tx)
    =?  finding  =(gud %.n)
      %-  ~(put by finding)
      [(hash-raw-tx raw-tx.tx) %failed]
    $(txs t.txs)
  --
::  +try-apply:
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
::
++  get-l1-pointer
  |=  [=tx:naive nas=^state:naive]
  ^-  l1-tx-pointer
  ?~  point=(~(get by points.nas) ship.from.tx)
    !!
  :_  next-nonce
  =<  address
  (proxy-from-point:naive proxy.from.tx u.point)
::
++  on-action
  |=  =action
  ^-  (quip card _state)
  ?-  -.action
    %commit  on-timer
    %config  (on-config +.action)
    %cancel  !!  ::TODO
  ::
      %submit
    =^  success  state
      ^-  [? _state]
      %^    take-tx
          force.action
        sig.action
      (part-tx-to-full tx.action)
    ~?  =(success |)
      [dap.bowl %submit-failed action]
    [~ state]
  ==
::
++  on-config
  |=  =config
  ^-  (quip card _state)
  ?-  -.config
    %frequency  [~ state(frequency frequency.config)]
    %nonce      [~ state(next-nonce nonce.config)]
    %endpoint   [~ state(endpoint endpoint.config)]
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
    ::TODO  what about existing sending entries?
    ?~  pk=(de:base16:mimes:html pk.config)
      `state
    [(get-nonce q.u.pk) state(pk q.u.pk)]
  ==
::  TODO: move address to state?
::
++  get-address
  ^-  address:ethereum
  (address-from-prv:key:ethereum pk)
::  +take-tx: accept submitted l2 tx into the :pending list
::TODO  rewrite
::
++  take-tx
  |=  [force=? =raw-tx:naive]
  ^-  [success=? _state]
  =/  [nep=_pending nas=_nas]  (pending-state canonical-state)
  =^  success  nas  (try-apply nas force raw-tx)
  =/  [nep=_pending pred=_nas]  (predicted-state canonical-state)
  =^  success  nas  (try-apply pred force raw-tx)
  ::TODO  want to notify about dropped pendings, or no? client prolly polls...
  =?  pending  success  (snoc nep [force raw-tx])
  [success state]
::  +set-timer: %wait until next whole :frequency
::
++  set-timer
  ^-  card
  %+  wait:b:sys  /timer
  (mul +((div now.bowl frequency)) frequency)
::  +on-timer: every :frequency, freeze :pending txs roll and start sending it
::
++  on-timer
  ^-  (quip card _state)
  =^  cards  state
    ?:  =(~ pending)  [~ state]
    =/  nonce=@ud  next-nonce
    =:  pending     ~
        next-nonce  +(next-nonce)
      ::
          sending
        %+  ~(put by sending)
          [get-address nonce]
        [0 (turn pending tail)]
      ::
          finding
        %-  ~(gas by finding)
        %+  turn  pending
        |=  [* =raw-tx:naive]
        [(hash-raw-tx raw-tx) (get-l1-pointer tx.raw-tx nas)]
      ==
    [(send-roll get-address nonce) state]
  [[set-timer cards] state]
::  +get-nonce: retrieves the latest nonce
::
++  get-nonce
  |=  pk=@
  ^-  (list card)
  (start-thread:spider /nonce [%aggregator-nonce !>([endpoint pk])])
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
  ::TODO  should go ahead and set resend timer in case thread hangs, or nah?
  %+  start-thread:spider
    /send/(scot %ux address)/(scot %ud nonce)
  :-  %aggregator-send
  !>  ^-  rpc-send-roll
  :*  endpoint
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
  ::  because we update the predicted state upon receiving
  ::  a new L2 tx via the rpc-api, this will only succeed when
  ::  we hear about a L2 tx that hasn't been submitted by us
  ::
  =^  *  nas  (try-apply nas | raw-tx.diff)
  [~ state]
::
--
