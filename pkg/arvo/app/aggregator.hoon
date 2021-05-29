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
::  - cache state, upate after every azimuth %fact
::  - properly support private key changes
::
::TODO  questions:
::  - it's a bit weird how we just assume the raw and tx in raw-tx to match...
::
/-  *aggregator
/+  azimuth, naive, default-agent, ethereum, dbug, verb, lib=naive-transactions
::
::TODO  /sur file for public types
|%
+$  state-0
  $:  %0
      ::  pending: the next l2 txs to be sent
      ::  sending: the l2 txs currently sending/awaiting l2 confirmation
      ::TODO  should maybe key by [address nonce] instead. same for wires
      ::  finding: raw-tx-hash reverse lookup for sending map
      ::  next-nonce: next l1 nonce to use
      ::
      pending=(list pend-tx)
      sending=(map nonce:naive [next-gas-price=@ud txs=(list raw-tx:naive)])
      finding=(map keccak $?(%confirmed %failed l1-tx-pointer))
      next-nonce=@ud
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
+$  action
  $%  [%submit force=? sig=@ tx=part-tx]
      [%cancel sig=@ keccak=@]
    ::
      [%commit ~]  ::TODO  maybe pk=(unit @) later
      [%frequency frequency=@dr]
      [%setkey pk=@]
      [%endpoint endpoint=@t]
      [%network net=?(%mainnet %ropsten %local)]
      [%nonce nonce=@ud]
      [%subs ~]
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
    [%pass /azimuth %agent [our.bowl %azimuth] %watch /(scot %p our.bowl)]~
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
    ?+  path  ~
      [%x %pending ~]  ``noun+!>(pending)
    ::
        [%x %pending @ ~]
      =*  wat  i.t.t.path
      ?~  who=(slaw %p wat)
        ::  by-address
        ::
        ?~  wer=(slaw %ux wat)
          [~ ~]
        =;  pending=(list pend-tx)
          ``noun+!>(pending)
        %+  skim  pending
        |=  pend-tx
        ::TODO  deduce address from sig.raw-tx ?
        !!
      ::  by-ship
      ::
      =;  pending=(list pend-tx)
        ``noun+!>(pending)
      %+  skim  pending
      |=  pend-tx
      =(u.who ship.from.tx.raw-tx)
    ::
        [%x %tx @ %status ~]
      ?~  keccak=(slaw %ux i.t.t.path)
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
      |=  [* raw-tx:naive]
      =(u.keccak (hash-tx raw))
    ::
        [%x %nonce @ @ ~]
      ?~  who=(slaw %p i.t.t.path)
        [~ ~]
      =+  proxy=i.t.t.t.path
      ?.  ?=(proxy:naive proxy)
        [~ ~]
      =/  [* nas=^state:naive]  pending-state:do
      ::TODO  or should we ~ when !(~(has by points.nas) who) ?
      =/  =point:naive  (~(gut by points.nas) u.who *point:naive)
      =+  (proxy-from-point:naive proxy point)
      ``atom+!>(nonce)
    ==
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
      [%send @t *]  (process-send-batch i.t.wire sign)
      [%azimuth ~]  (process-azimuth-update sign)
      [%nonce ~]    (process-nonce sign)
    ==
    ::
    ++  process-send-batch
      |=  [nonce=@t =sign:agent:gall]
      ^-  (quip card _this)
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
            (on-batch-result:do (rash nonce dem) %.n^'thread failed')
          [cards this]
        ::
            %thread-done
          =+   !<(result=(each @ud @t) q.cage.sign)
          =^  cards  state
            (on-batch-result:do (rash nonce dem) result)
          [cards this]
        ==
      ==
    ::
    ++  process-azimuth-update
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
        ==
      ==
    ::
    ++  process-nonce
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
  (hash-tx raw.raw-tx)
::
++  part-tx-to-full
  |=  =part-tx
  ^-  [octs tx:naive]
  ?+    -.part-tx  !!
      %raw
    ?~  batch=(parse-raw-tx:naive q.raw.part-tx)
      ~&  %parse-failed
      ::  TODO: maybe return a unit if parsing fails?
      ::
      !!
    [raw tx]:-.u.batch
    :: %don  [(encode-tx:naive +.part-tx) +.part-tx]
    %ful  +.part-tx
  ==
::  +pending-state
::
::    derives tentative state from pending txs and canonical state,
::    discarding invalid pending txs in the process.
::
::TODO  maybe want to cache locally, refresh on %fact from azimuth?
::
++  pending-state
  ^-  [_pending ^state:naive]
  ::  load current, canonical state
  ::
  =+  .^  nas=^state:naive
      %gx
      (scot %p our.bowl)
      %azimuth
      (scot %da now.bowl)
      /nas/nas
    ==
  ::  apply our pending transactions
  ::TODO  should also apply txs from sending map!
  ::
  =|  valid=_pending
  |-  ^+  [valid nas]
  ?~  pending  [(flop valid) nas]
  ::
  =^  gud=?    nas  (try-apply nas i.pending)
  =?  valid    gud  [i.pending valid]
  =?  finding  =(gud %.n)
    %-  ~(put by finding)
    [(hash-raw-tx raw-tx.i.pending) %failed]
  $(pending t.pending)
::  +try-apply:
::
++  try-apply
  |=  [nas=^state:naive force=? =raw-tx:naive]
  ^-  [success=? _nas]
  =/  chain-id=@t  (scot %ud chain-id)
  ?.  (verify-sig-and-nonce:naive verifier:lib chain-id nas raw-tx)
    [force nas]
  ::
  =^  out  points.nas  (increment-nonce:naive nas from.tx.raw-tx)
  ::
  ?~  nex=(receive-tx:naive nas tx.raw-tx)
    [force nas]
  [& +.u.nex]
::
++  on-action
  |=  =action
  ^-  (quip card _state)
  ?-  -.action
    %commit     on-timer
    %frequency  [~ state(frequency frequency.action)]
    %nonce      [~ state(next-nonce nonce.action)]
    %endpoint   [~ state(endpoint endpoint.action)]
  ::
      %network
    :-  ~
    =/  [contract=@ux chain-id=@]
      =<  [naive chain-id]
      =,  azimuth
      ?-  net.action
        %mainnet  mainnet-contracts
        %ropsten  ropsten-contracts
        %local    local-contracts
      ==
    state(contract contract, chain-id chain-id)
  ::
      %subs
    :_  state
    [%pass /azimuth %agent [our.bowl %azimuth] %watch /(scot %p our.bowl)]~
  ::
      %setkey
    ::TODO  what about existing sending entries?
    :-  get-nonce
    ?~  pk=(de:base16:mimes:html pk.action)
      state
    state(pk q.u.pk)
  ::
      %submit
    =^  success  state
      ^-  [? _state]
      %^    take-tx
          force.action
        sig.action
      (part-tx-to-full tx.action)
    ::  TODO:  consider failure case
    ?>  success
    [~ state]
  ::
      %cancel
    !!  ::TODO
  ==
::  +take-tx: accept submitted l2 tx into the :pending list
::TODO  rewrite
::
++  take-tx
  |=  [force=? =raw-tx:naive]
  ^-  [success=? _state]
  =/  [nep=_pending nas=^state:naive]  pending-state
  =|  success=?
  ::  TODO: actually use try-apply when proper Tx signing in place
  ::
  :: =^  success  nas
  ::   (try-apply nas force raw-tx)
  ::TODO  want to notify about dropped pendings, or no? client prolly polls...
  =?  pending  success  (snoc nep [force raw-tx])
  ::TODO  cache nas?
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
        %+  ~(put by sending)  nonce
        [0 (turn pending tail)]
      ==
    [(send-roll nonce) state]
  [[set-timer cards] state]
::  +get-nonce: retrieves the latest nonce
::
++  get-nonce
  ^-  (list card)
  (start-thread:spider /nonce [%aggregator-nonce !>([endpoint pk])])
::
::  +send-roll: start thread to submit roll from :sending to l1
::
++  send-roll
  |=  nonce=@ud
  ^-  (list card)
  ::  if this nonce isn't in the sending queue anymore, it's done
  ::
  ?.  (~(has by sending) nonce)
    ~?  lverb  [dap.bowl %done-sending nonce]
    ~
  ::  start the thread, passing in the l2 txs to use
  ::
  ::TODO  should go ahead and set resend timer in case thread hangs, or nah?
  %+  start-thread:spider
    /send/(scot %ud nonce)
  :-  %aggregator-send
  !>  ^-  rpc-send-roll
  :*  endpoint
      contract
      chain-id
      pk
      nonce
      (~(got by sending) nonce)
  ==
::  +on-batch-result: await resend after thread success or failure
::
++  on-batch-result
  |=  [nonce=@ud result=(each @ud @t)]
  ^-  (quip card _state)
  ::  update gas price for this tx in state
  ::
  =?  sending  ?=(%& -.result)
    %+  ~(jab by sending)  nonce
    (cork tail (lead p.result))
  ::  print error if there was one
  ::
  ~?  ?=(%| -.result)  [dap.bowl %send-error p.result]
  ::  resend the l1 tx in five minutes
  ::
  :_  state
  [(wait:b:sys /resend/(scot %ud nonce) (add resend-time now.bowl))]~
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
    ?~  sen=(~(get by sending) nonce)
      ~&  [dap.bowl %weird-double-remove]
      sending
    ?~  nin=(find [raw-tx.diff]~ txs.u.sen)
      ~&  [dap.bowl %weird-unknown]
      sending
    =.  txs.u.sen  (oust [u.nin 1] txs.u.sen)
    ?~  txs.u.sen
      ~?  lverb  [dap.bowl %done-with-nonce nonce]
      (~(del by sending) nonce)
    (~(put by sending) nonce u.sen)
  ::  update the finding map with the new status
  ::
  =.  finding
    %+  ~(put by finding)  keccak
    ?~  err.diff  %confirmed
    ::  if we kept the forced flag around for longer, we could notify of
    ::  unexpected tx failures here. would that be useful? probably not?
    ::  ~?  !forced  [dap.bowl %aggregated-tx-failed-anyway err.diff]
    %failed
  [~ state]
::
--
