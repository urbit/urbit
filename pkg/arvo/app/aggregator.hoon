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
::  - hook up subscription to azimuth for %tx diffs
::  - hook up thread updates/results
::  - hook up timer callbacks
::  - cache state, upate after every azimuth %fact
::  - properly support private key changes
::
::TODO  questions:
::  - it's a bit weird how we just assume the raw and tx in raw-tx to match...
::
/+  naive, ethereum
/=  ttttt  /tests/lib/naive  ::TODO  use new lib
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
      sending=(map nonce [next-gas-price=@ud txs=(list raw-tx:naive)])
      finding=(map keccak $?(%confirmed %failed l1-tx-pointer))
      next-nonce=@ud
    ::
      ::  pk: private key to send the roll
      ::  frequency: time to wait between sending batches (TODO fancier)
      ::  endpoint: ethereum rpc endpoint to use
      ::
      pk=@
      frequency=@dr
      endpoint=@t
  ==
::
+$  keccak  @ux
::
+$  tx-status
  $:  status=?(%unknown %pending %sending %confirmed %failed)
      pointer=(unit l1-tx-pointer)
  ==
::
+$  l1-tx-pointer
  $:  =address:ethereum
      nonce=@ud
  ==
::
::TODO  cache sender address?
+$  pend-tx  [force=? =raw-tx:naive]
::
+$  part-tx
  $%  [%raw raw=octs]
      [%don =tx:naive]
      [%ful raw=octs =tx:naive]  ::TODO  redundant?
  ==
::
+$  action
  $%  [%submit force=? sig=@ tx=part-tx]
      [%cancel sig=@ keccak=@]
    ::
      [%commit ~]  ::TODO  maybe pk=(unit @) later
      [%config frequency=@dr]
      [%setkey pk=@]
      ::TODO  configure endpoint, contract address, chain..?
  ==
::
+$  card  card:agent:gall
::
::TODO  config?
++  contract  0xb581.01cd.3bbb.cc6f.a40b.cdb0.4bb7.1623.b5c7.d39b
++  chain-id  0x1
::
++  resend-time  ~m5
::
++  lverb  &
--
::
=|  state=state-0
%-  agent:dbug
%+  verb  |
^-  agent:gall
::
=<
  |_  =bowl:gall
  +*  this  .
      def   ~(. (default-agent this %|) bowl)
  ::
  ++  on-init
    ^-  (quip card _this)
    ::TODO  set default frequency and endpoint?
    =.  frequency  ~h1
    [~ this]
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
    ?+  mark  (on-poke:def mark vase)
      %noun  $(mark %aggregator-action)
    ::
        %aggregator-action
      !!
      ::TODO
    ==
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
        ?~  wer=(slaw %ux wat)
          [~ ~]
        %+  skim  pending
        |=  pend-tx
        ::TODO  deduce address from sig.raw-tx ?
        !!
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
        [%sending u.status]
      ::TODO  potentially slow!
      =;  known=?
        [?:(known %pending %unknown) ~]
      %+  lien  pending
      |=  [* raw-tx]
      =(u.keccak (hash-tx raw))
    ::
        [%x %nonce @ @ ~]
      ?~  who=(slaw %p i.t.t.path)
        [~ ~]
      =+  proxy=i.t.t.t.path
      ?.  ?=(proxy:naive proxy)
        [~ ~]
      =^  *  nas  pending-state:do
      ::TODO  or should we ~ when !(~(has by points.nas) who) ?
      =/  =point:naive  (~(gut by points.nas) who *point)
      =+  (proxy-from-point:naive proxy point)
      ``atom+!>(nonce)
    ==
  ::
  ++  on-arvo
    |=  [=wire =sign-arvo]
    ^-  (quip card _this)
    ?+  +<.sign  (on-arvo:def wire sign)
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
  ++  on-agent  on-agent:def
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
  --
::
++  hash-tx  keccak-256:keccak:crypto
::
++  hash-raw-tx
  |=  raw-tx
  (hash-tx raw)
::
++  part-tx-to-full
  |=  =part-tx
  ^-  [octs tx:naive]
  ?-  -.part-tx
    %raw  [+.part-tx (decode-tx:naive +.part-tx)]
    %don  [(encode-tx:naive +.part-tx) +.part-tx]
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
  ^-  [_pending state:naive]
  ::  load current, canonical state
  ::
  =+  .^  nas=state:naive
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
  |-  ^+  nas
  ?~  pending  [(flop valid) nas]
  ::
  =^  gud=?  nas  (try-apply nas i.pending)
  =?  valid  gud  [i.pending valid]
  $(pending t.pending)
::  +try-apply:
::
++  try-apply
  |=  [nas=state:naive force=? =raw-tx:naive]
  ^-  [success=? _nas]
  ?.  (verify-sig-and-nonce:naive verifier:ttttt nas raw-tx)
    [force nas]
  ::
  =^  *  nas  (increment-nonce:naive nas from.tx.raw-tx)
  ::
  ?~  nex=(receive-tx:naive nas tx.raw-tx)
    [force nas]
  [& +.u.nex]
::
++  on-action
  |=  =action
  ?-  -.action
    %commit  send-roll
    %config  [~ state(frequency frequency.action)]
    %setkey  [~ state(pk pk.action)]  ::TODO  what about existing sending entries?
  ::
      %submit
    =^  success  state
      %+  take-tx  force.action
      (part-tx-to-full tx.action)
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
  =/  [nep=_pending nas=state:naive]  pending-state
  =^  success  nas
    (try-apply nas force raw-tx)
  ::TODO  want to notify about dropped pendings, or no? client prolly polls...
  =?  pending  success  [[force raw-tx] nep]  ::TODO  probably +snoc instead?
  ::TODO  cache nas?
  [success state]
::  +set-timer: %wait until next whole :frequency
::
++  set-timer
  ^-  card
  %+  wait  /timer
  (mul +((div now.bowl frequency)) frequency)
::  +on-timer: every :frequency, freeze :pending txs roll and start sending it
::
++  on-timer
  =^  cards  state
    ?~  pending  [~ state]
    =/  nonce=@ud  next-nonce
    =:  pending     ~
        next-nonce  +(next-nonce)
      ::
          sending
        %+  ~(put by sending)  nonce
        %+  turn  pending
        (cork tail (lead 0))
      ==
    [(send-roll nonce) state]
  [[set-timer cards] state]
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
  !>([nonce ~(got by sending) nonce)])  ::TODO  other args
::  +on-thread-result: await resend after thread success or failure
::
++  on-thread-result
  |=  [nonce=@ud result=(each @ud term)]
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
  [(wait /resend/(scot %ud nonce) (add resend-time now.bowl))]~
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
      [dap.bowl %weird-double-confirm from.tx]
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
