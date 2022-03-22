:: This thread grabs the latest Ethereum logs and produces a csv file
:: containing the following data on L2 transactions:
::
::  - block number
::  - timestamp
::  - roller address
::  - roll hash
::  - tx hash
::  - sending ship
::  - sending proxy
::  - nonce
::  - gas price
::  - length of input data
::  - success or failure
::  - function name
::
::  A lot of the data-scrounging here is stuff that %roller already keeps track
::  of. We could just scry it from there, but then this thread needs to be run
::  on the roller ship. So we rebuild the list of historical transactions
::  ourselves so that this can run from any ship.
::
::  TODO: change block maps to ordered maps
::
/-  dice,
    spider
::
/+  dice,
    ethereum,
    ethio,
    naive,
    naive-tx=naive-transactions,
    *strandio
::
::  imports most recent downloaded logs
::/*  logs  %eth-logs  /app/azimuth/logs/eth-logs
::/*  logs  %eth-logs  /app/azimuth/tid/eth-logs
::
::  starting snapshot. this may not be the right starting point once we have
::  clay tombstoning and the snapshot may be updated
/*  snap  %azimuth-snapshot  /app/azimuth/version-0/azimuth-snapshot
::
=,  strand=strand:spider
=,  jael
::
::  takes in the network to use. note that this filters logs for transactions to
::  the appropriate contract address - this won't get you e.g. ropsten logs if
::  you run it with net=%ropsten on a mainnet ship
::
^-  thread:spider
=<  process-logs
=>
  |%
  ::  imported logs is cast as $events
  +$  events    (list event-log:rpc:ethereum)
  +$  address   address:naive  :: @ux
  +$  keccak    @ux            :: used for transaction and roll hashes
  +$  blocknum  number:block   :: @udblocknumber
  ::
  +$  net       net:dice       ::?(%mainnet %ropsten %local %default)
  ::
  +$  block-map  %+  map  blocknum
    [timestamp=@da rolls=(map keccak [[gas=@ud sender=address] =effects:naive])]
  ::
  +$  action  $?  %transfer-point
                  %spawn
                  %configure-keys
                  %escape
                  %cancel-escape
                  %adopt
                  %reject
                  %detach
                  %set-management-proxy
                  %set-spawn-proxy
                  %set-transfer-proxy
               ==
  ::
  +$  tx-data  $:  block=blocknum
                   timestamp=@da
                   roller=address
                   roll-hash=keccak
                   tx-hash=keccak
                   sender=ship
                   proxy=proxy:naive
                   nonce=nonce:naive
                   gas=@ud
                   length=@
                   suc=?
                   =action
                ==
  ::++  node-url  'http://eth-mainnet.urbit.org:8545'
  ++  node-url  'https://mainnet.infura.io/v3/13a985885cd243cc886062ad2f345e16'  :: infura free tier node
  --
::
|%
  ++  process-logs
    |=  arg=vase
    =+  !<([~ =net] arg)
    =/  m  (strand ,vase)
    ^-  form:m
    ;<  logs=events  bind:m  (scry events /gx/azimuth/logs/noun)
    =/  logs  (scag 50 logs)  :: to make debugging faster
    =/  [naive-contract=address chain-id=@]
      [naive chain-id]:(get-network:dice net)
    =/  snap=snap-state:dice  snap
    ::
    =/  l2-logs=events  (filter-l2 logs naive-contract)
    %-  %-  slog  :_  ~
        leaf+"processing {<net>} ethereum logs with {<(lent logs)>} total events, of which {<(lent l2-logs)>} are l2 events"
    =/  blocks=(list blocknum)           (get-block-numbers l2-logs)
    =/  tx-hashes=(list keccak)          (get-tx-hashes l2-logs)
    =/  block-jar=(jar blocknum keccak)  (block-tx-jar l2-logs)
    ;<  timestamps=(map blocknum @da)                  bind:m
      (get-timestamps blocks)
    ;<  tx-data=(map keccak [gas=@ud sender=address])  bind:m
      (get-tx-data tx-hashes)
    =/  rolls-map=(map blocknum (map keccak effects:naive))
      (run-logs-from-state-map nas.snap logs net naive-contract chain-id)
    =/  rolling=block-map  (collate-roll-data blocks block-jar rolls-map timestamps tx-data)
    =/  flat  (flatten-data rolling)
    =/  csv=(list cord)  (make-csv flat)
    ;<  ~  bind:m  (export-csv csv)
    ::
    (pure:m !>(csv))
  ::
  ++  collate-roll-data
    |=  $:  blocks=(list blocknum)
            block-jar=(jar blocknum keccak)
            rolls-map=(map blocknum (map keccak effects:naive))
            timestamps=(map blocknum @da)
            tx-data=(map keccak [gas=@ud sender=address])
        ==
    ::  =|  $=  block-map
    ::      %+  map  blocknum
    ::      [timestamp=@da rolls=(map keccak [[gas=@ud sender=address] =effects:naive])]
    =|  =block-map
    ^+  block-map
    |-
    ?~  blocks  block-map
    =/  block=blocknum  i.blocks
    =/  tx-hashes=(list keccak)  (~(get ja block-jar) block)
    =/  rolls=(map keccak [[gas=@ud sender=address] =effects:naive])
      %-  ~(gas by *(map keccak [[gas=@ud sender=address] =effects:naive]))
      %+  turn  tx-hashes
      |=  txh=keccak
      :-  txh
      [(~(got by tx-data) txh) (~(got by (~(got by rolls-map) block)) txh)]
    %=  $
      blocks     t.blocks
      block-map  %+  ~(put by block-map)  block
                 [(~(got by timestamps) block) rolls]
    ==
  ::
  ++  flatten-data
    |=  =block-map
    =|  tx-list=(list tx-data)
    ^+  tx-list
    =/  blocks=(list blocknum)  ~(tap in ~(key by block-map))
    ::  recurse through each block, getting the rolls submitted in that block,
    ::  their timestamp, and the gas price of that roll
    |-  ^-  (list tx-data)
    ?~  blocks  tx-list
    =/  block=blocknum  i.blocks
    =/  bor  (~(got by block-map) block)
    =/  roll-list=(list keccak)
      ~(tap in ~(key by rolls.bor))
    =|  block-tx-list=(list tx-data)
    ::  recurse through each roll, getting the transaction data from the effects
    |-
    ?~  roll-list
      %=  ^$
        blocks  t.blocks
        tx-list  (welp tx-list block-tx-list)
      ==
    =/  roll-hash=keccak  i.roll-list
    =/  roll=[[gas=@ud sender=address] =effects:naive]
      (~(got by rolls.bor) roll-hash)
    ::  recurse through the list of effects, building up transaction data as we
    ::  go. there's a choice here to use the effects, or the submitted
    ::  raw-tx. the effects include whether or not a transaction failed,
    ::  which is important data not a part of the submitted raw-tx. we
    ::  could determine this ourselves, but we build the effects anyways when
    ::  computing the state transitions, so we may as well use them.
    ::
    ::  an individual transaction results in up to 3 diffs: a %nonce, a %tx, and
    ::  a %point. they always appear in this order. successful transactions
    ::  always have all 3, while failed transactions only have %nonce and %tx.
    ::  note that the nonce listed is always the expected nonce - we can't know
    ::  what nonce was actually submitted without the private key of the signer.
    =|  roll-tx-list=(list tx-data)
    =|  nonce-and-tx=[_| _|]
    =/  =tx-data  :*  block  timestamp.bor  sender.roll  roll-hash  *keccak
                      *ship  *proxy:naive  *nonce:naive  gas.roll  *@  |  *action
                  ==
    |-
    ::  if we've gotten both the %nonce and %tx diff from a transaction, add the
    ::  tx-data to the list of tx for the roll
    ?:  =([& &] nonce-and-tx)
      %=  $  :: should this be %_ ?
        roll-tx-list  (snoc roll-tx-list tx-data)
        nonce-and-tx  [| |]
        tx-data       *_tx-data  :: reset tx-data
      ==
    ::  if we've finished looping through the effects, add the tx list from the
    ::  roll to the list of tx for the block
    ::
    ?~  effects.roll
       %=  ^$
         roll-list      t.roll-list
         block-tx-list  (welp block-tx-list roll-tx-list)
       ==
    ::
    =/  =diff:naive  i.effects.roll
    ?+    diff
      $(effects.roll t.effects.roll)  :: we ignore %operator, %dns, and %point diffs
    ::
        [%nonce *]
      %=  $
        -.nonce-and-tx  &
        nonce.tx-data   nonce.diff
        sender.tx-data  ship.diff
        proxy.tx-data   proxy.diff
        effects.roll    t.effects.roll
      ==
    ::
      ::  the conditional here is to ensure that a %tx diff is from
      ::  the same submitted tx as the nonce that should have come
      ::  before it. this should never happen, but maybe you need
      ::  to flop effects to get things in the right order. this also
      ::  isn't perfect - the tx could be from the same ship and proxy
      ::  but different nonce.
        [%tx *]
      ?.  ?&  =(sender.tx-data ship.from.tx.raw-tx.diff)
              =(proxy.tx-data proxy.from.tx.raw-tx.diff)
          ==
        ~&  >>  '%tx associated to a different ship than %nonce!'
        !!
      %=  $
        +.nonce-and-tx   &
        effects.roll     t.effects.roll
        length.tx-data   -.raw.raw-tx.diff
        tx-hash.tx-data  (hash-raw-tx:naive-tx raw-tx.diff)
        action.tx-data   +<.tx.raw-tx.diff
        suc.tx-data      ?~  err.diff  &  |
      ==
    ==
  ::
  ++  export-csv
    |=  in=(list cord)
    =/  m  (strand ,~)
    ^-  form:m
    ;<  =bowl:spider  bind:m  get-bowl
    =-  (send-raw-card %pass / %arvo %c %info -)
    %+  foal:space:userlib
      /(scot %p our.bowl)/base/(scot %da now.bowl)/naive-exports/csv/txt
    [%txt !>(in)]
  ::
  ++  make-csv
    ::  Takes in rolls-map and makes it suitable to be saved as a csv
    |=  in=(list tx-data)
    ^-  (list cord)
    :-  %-  crip
        ;:  weld
          "block number,"
          "timestamp,"
          "roller addres,"
          "roll hash,"
          "tx hash,"
          "sending ship,"
          "sending proxy,"
          "nonce,"
          "gas price,"
          "length of input data,"
          "success or failure,"
          "function name"
        ==
    %+  turn  in
      |=  =tx-data
      %-  crip
      ;:  weld
        (scow %ux block.tx-data)      ","
        (scow %da timestamp.tx-data)  ","
        (scow %ux roller.tx-data)     ","
        (scow %ux roll-hash.tx-data)  ","
        (scow %ux tx-hash.tx-data)    ","
        (scow %p sender.tx-data)      ","
        (scow %tas proxy.tx-data)     ","
        (scow %ud nonce.tx-data)      ","
        (scow %ud gas.tx-data)        ","
        (scow %$ length.tx-data)      ","
        (scow %f suc.tx-data)         ","
        (scow %tas action.tx-data)
      ==
  ::
  ++  get-tx-data
    :: retrieves transaction receipts for rolls, extracting the gas cost and sender
    |=  tx-hashes=(list keccak)
    =/  m  (strand ,(map keccak [gas=@ud sender=address]))
    ^-  form:m
    =|  out=(map keccak [gas=@ud sender=address])
    |^  ^-  form:m
      =*  loop  $
      ?:  =(~ tx-hashes)  (pure:m out)
      ;<  res=(list [@t json])  bind:m
        (request-receipts (scag 100 tx-hashes))
      %_  loop
        out        (~(gas by out) (parse-results res))
        tx-hashes  (slag 100 tx-hashes)
      ==
    ::
    ++  request-receipts
      |=  tx-hashes=(list keccak)
      %+  request-batch-rpc-strict:ethio  node-url
      %+  turn  tx-hashes
      |=  txh=keccak
      ^-  [(unit @t) request:rpc:ethereum]
      :-  `(crip '0' 'x' ((x-co:co 64) txh))
      [%eth-get-transaction-receipt txh]
    ::
    ++  parse-results
      |=  res=(list [@t json])
      ^-  (list [txh=keccak [gas=@ud sender=address]])
      %+  turn  res
      |=  [id=@t =json]
      ^-  [txh=keccak [gas=@ud sender=address]]
      :-  (hex-to-num:ethereum id)
      :-  %-  parse-hex-result:rpc:ethereum
        ~|  json
        ?>  ?=(%o -.json)
        (~(got by p.json) 'gasUsed')  :: returns the amount of gas used, not gas price
      %-  parse-hex-result:rpc:ethereum
      ~|  json
      ?>  ?=(%o -.json)
      (~(got by p.json) 'from')
    --
  ::
  ++  get-timestamps
    ::  TODO: would be better to call the eth-get-timestamps thread directly
    ::  rather than copy and paste the code for it here
    |=  blocks=(list blocknum)
    =/  m  (strand ,(map blocknum @da))
    ^-  form:m
    =|  out=(map blocknum @da)
    |^  ^-  form:m
      =*  loop  $
      ?:  =(~ blocks)  (pure:m out)
      ;<  res=(list [@t json])  bind:m
        (request-blocks (scag 100 blocks))
      %_  loop
        out     (~(gas by out) (parse-results res))
        blocks  (slag 100 blocks)
      ==
    ::
    ++  request-blocks
      |=  blocks=(list blocknum)
      %+  request-batch-rpc-strict:ethio  node-url
      %+  turn  blocks
      |=  block=blocknum
      ^-  [(unit @t) request:rpc:ethereum]
      :-  `(scot %ud block)
      [%eth-get-block-by-number block |]
    ::
    ++  parse-results
      |=  res=(list [@t json])
      ^-  (list [blocknum @da])
      %+  turn  res
      |=  [id=@t =json]
      ^-  [blocknum @da]
      :-  (slav %ud id)
      %-  from-unix:chrono:userlib
      %-  parse-hex-result:rpc:ethereum
      ~|  json
      ?>  ?=(%o -.json)
      (~(got by p.json) 'timestamp')
    --
  ::
  ++  run-logs-from-state-map
    |=  $:  nas=^state:naive
            logs=events
            =net
            naive-contract=address
            chain-id=@ud
        ==
    =|  out=(map blocknum (map keccak effects:naive))
    ^+  out
    ::  We need to run the state transitions to see what the individual
    ::  transactions were, as well as whether they succeeded or failed.
    ::
    %-  %-  slog  :_  ~
      leaf+"processing state transitions beginning from stored snapshot"
    ::
    |-
    ?~  logs  out
    =/  log=event-log:rpc:ethereum  i.logs
    ?~  mined.log
      ~&  >>  'empty log'
      $(logs t.logs)
    =/  block=blocknum  block-number.u.mined.log
    =/  =^input:naive
      :-  block
      ?.  =(naive-contract address.log)
        :-  %log
        [address.log (data-to-hex:dice data.log) topics.log]
      ?~  input.u.mined.log
        ~&  >>  'empty L2 transaction'
        [%bat *@]
      [%bat u.input.u.mined.log]
    =^  =effects:naive  nas
      (%*(. naive lac |) verifier:naive-tx chain-id nas input)
    %=  $
      logs  t.logs
      out   ?.  =(%bat +<.input)
              out  ::  skip L1 logs
            :: there's probably a better way to do this
            =/  cur-map=(unit (map keccak effects:naive))
              (~(get by out) block)
            ?~  cur-map
              %+  ~(put by out)  block
              ^-  (map keccak effects:naive)
              (my [[transaction-hash.u.mined.log effects]~])
            =.  u.cur-map
              (~(put by u.cur-map) transaction-hash.u.mined.log effects)
            (~(put by out) block u.cur-map)
    ==
  ::
  ++  filter-l2
    |=  [logs=events naive-contract=address]  ^-  events
    %+  skim  logs
    |=  log=event-log:rpc:ethereum  ^-  ?
    ?~  mined.log  %.n
    =(naive-contract address.log)
  ::
  ++  block-tx-jar
    |=  logs=events  ^-  (jar blocknum keccak)
    =|  block-jar=(jar blocknum keccak)
    |-
    ?~  logs  block-jar
    :: shouldn't crash since +filter-l2 already checks if mined.log is empty
    =+  (need mined.i.logs)
    %=  $
      logs  t.logs
      block-jar  (~(add ja block-jar) [block-number transaction-hash]:-)
    ==
  ::
  ++  get-block-numbers
    |=  logs=events  ^-  (list blocknum)
    %+  turn  logs
    |=  log=event-log:rpc:ethereum
    :: shouldn't crash since +filter-l2 already checks if mined.log is empty
    block-number:(need mined.log)
  ::
  ++  get-tx-hashes
    |=  logs=events  ^-  (list keccak)
    %+  turn  logs
    |=  log=event-log:rpc:ethereum
    :: shouldn't crash since +filter-l2 already checks if mined.log is empty
    transaction-hash:(need mined.log)
--
