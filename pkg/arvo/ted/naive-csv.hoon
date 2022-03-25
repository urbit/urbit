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
::  - spawning ship (^sein:title)
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
  +$  net       net:dice       ::?(%mainnet %ropsten %local %default)
  +$  block-map  %+  map  blocknum
    [timestamp=@da rolls=(map keccak [[gas=@ud sender=address] =effects:naive])]
  +$  rolls-map  (map blocknum (map keccak effects:naive))
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
                   length=@ux
                   suc=?
                   =action
                   parent=ship
                ==
  --
::
|%
  ::  +process-logs is the main process. it grabs the azimuth snapshop, runs
  ::  +naive on the logs, grabs the timestamps and gas costs for each roll,
  ::  then flattens them into a list of $tx-data and saves them to disk.
  ++  process-logs
    |=  arg=vase
    =+  !<([~ =net node-url=@t] arg)
    =/  pax=path  /naive-exports/csv  :: data will be saved here
    =/  m  (strand ,vase)
    ^-  form:m
    ;<  =events  bind:m  (scry events /gx/azimuth/logs/noun)
    =/  events  (scag 50 events)  :: to make debugging faster
    =/  [naive-contract=address chain-id=@]
      [naive chain-id]:(get-network:dice net)
    =/  snap=snap-state:dice  snap
    %-  %-  slog  :_  ~
        leaf+"processing {<net>} ethereum logs with {<(lent events)>} events"
    ::
    =/  =rolls-map
      (compute-effects nas.snap events net naive-contract chain-id)
    ::  I think this should work, but child threads seem to be broken
    ::  ;<  =thread-result  bind:m
    ::    %+  await-thread
    ::      %eth-get-timestamps
    ::    !>([node-url ~(tap in ~(key by rolls-map))])
    ;<  timestamps=(map blocknum @da)  bind:m
      (get-timestamps node-url ~(tap in ~(key by rolls-map)))
    ;<  roll-receipts=(map keccak [gas=@ud sender=address])  bind:m
      (get-roll-receipts node-url (get-roll-hashes rolls-map))
    =/  csv=(list cord)
      (make-csv (flatten (collate-roll-data rolls-map timestamps roll-receipts)))
    ;<  ~  bind:m  (export-csv csv pax)
    ::
    (pure:m !>((crip :(weld "data saved to %" (spud pax) "/txt"))))
  ::
  ::  +collate-roll-data throws naive:effects, timestamps, and gas costs into
  ::  one $block-map
  ++  collate-roll-data
    |=  $:  =rolls-map
            timestamps=(map blocknum @da)
            roll-receipts=(map keccak [gas=@ud sender=address])
        ==
    =/  blocks=(list blocknum)  ~(tap in ~(key by rolls-map))
    =|  =block-map
    ^+  block-map
    |-
    ?~  blocks  block-map
    =/  block=blocknum  i.blocks
    =/  rolls=(map keccak [[gas=@ud sender=address] =effects:naive])
      %-  ~(gas by *(map keccak [[gas=@ud sender=address] =effects:naive]))
      %+  turn  ~(tap in ~(key by (~(got by rolls-map) block)))
      |=  txh=keccak
      :+  txh
        (~(got by roll-receipts) txh)
      (~(got by (~(got by rolls-map) block)) txh)
    %=  $
      blocks     t.blocks
      block-map  %+  ~(put by block-map)
                   block
                 [(~(got by timestamps) block) rolls]
    ==
  ::
  ::  +flatten takes a $block-map and creates a $tx-data for every transaction
  ::  in every roll, returned as a (list tx-data)
  ++  flatten
    |=  =block-map
    =/  blocks=(list blocknum)  ~(tap in ~(key by block-map))
    =|  tx-list=(list tx-data)
    ^+  tx-list
    ::  recurse through the list of blocks, getting the rolls submitted in that
    ::  block, their timestamp, and the gas price of that roll
    |-
    ?~  blocks  tx-list
    =/  block=blocknum  i.blocks
    =/  bok  (~(got by block-map) block)
    =/  roll-list=(list keccak)  ~(tap in ~(key by rolls.bok))
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
      (~(got by rolls.bok) roll-hash)
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
    =/  =tx-data  :*  block  timestamp.bok  sender.roll  roll-hash  *keccak  *ship
                      *proxy:naive  *nonce:naive  gas.roll  *@  |  *action  *ship
                  ==
    |-
    ::  if we've gotten both the %nonce and %tx diff from a transaction, add the
    ::  tx-data to the list of tx for the roll
    ?:  =([& &] nonce-and-tx)
      %=  $
        nonce-and-tx  [| |]
        tx-data       *_tx-data  :: reset tx-data TODO: why does this seem to work?
        roll-tx-list  (snoc roll-tx-list tx-data)
      ==
    ::  if we've finished looping through the effects, add the tx list from the
    ::  roll to the list of tx for the block
    ?~  effects.roll
       %=  ^$
         roll-list      t.roll-list
         block-tx-list  (welp block-tx-list roll-tx-list)
       ==
    ::
    =/  =diff:naive  i.effects.roll
    ?+    diff
      $(effects.roll t.effects.roll)  :: we ignore %operator, %dns, %point diffs
    ::
    ::  %nonce is always the first diff from a given transaction.
        [%nonce *]
      %=  $
        -.nonce-and-tx  &
        sender.tx-data  ship.diff
        nonce.tx-data   nonce.diff
        proxy.tx-data   proxy.diff
        effects.roll    t.effects.roll
        parent.tx-data  (^sein:title ship.diff)
      ==
    ::
    ::  %tx is always the second diff from a given transaction.
        [%tx *]
      %=  $
        +.nonce-and-tx   &
        effects.roll     t.effects.roll
        action.tx-data   +<.tx.raw-tx.diff
        suc.tx-data      ?~  err.diff  &  |
        length.tx-data   `@`-.raw.raw-tx.diff
        tx-hash.tx-data  (hash-raw-tx:naive-tx raw-tx.diff)
      ==
    ==
  ::
  ::  +get-roll-receipts retrieves transaction receipts for rolls, extracting
  ::  the gas cost and sender, then returns a map from tx hashes to [gas sender]
  ++  get-roll-receipts
    ::  TODO: this should be made into a separate thread for use by others, but
    ::  it has the same issue with child threads as get-timestamps
    |=  [node-url=@t tx-hashes=(list keccak)]
    %-  %-  slog  :_  ~
      leaf+"getting l2 roll receipts from ethereum node"
    ::=/  tx-hashes=(list keccak)  (get-roll-hashes rolls-map)
    =/  m  (strand ,(map keccak [gas=@ud sender=address]))
    ^-  form:m
    =|  out=(map keccak [gas=@ud sender=address])
    |^  ^-  form:m
      =*  loop  $
      ?:  =(~ tx-hashes)  (pure:m out)
      ;<  res=(list [@t json])  bind:m
        (request-receipts (scag 100 tx-hashes) node-url)
      %_  loop
        out        (~(gas by out) (parse-results res))
        tx-hashes  (slag 100 tx-hashes)
      ==
    ::
    ++  request-receipts
      |=  [tx-hashes=(list keccak) node-url=@t]
      %+  request-batch-rpc-strict:ethio  node-url
      %+  turn  tx-hashes
      |=  =keccak
      ^-  [(unit @t) request:rpc:ethereum]
      :-  `(crip '0' 'x' ((x-co:co 64) keccak))
      [%eth-get-transaction-receipt keccak]
    ::
    ++  parse-results
      |=  res=(list [@t json])
      ^-  (list [=keccak [gas=@ud sender=address]])
      %+  turn  res
      |=  [id=@t =json]
      ^-  [=keccak [gas=@ud sender=address]]
      :-  (hex-to-num:ethereum id)
      :-  %-  parse-hex-result:rpc:ethereum
        ~|  json
        ?>  ?=(%o -.json)
        (~(got by p.json) 'gasUsed')                    :: gas used in wei
      %-  parse-hex-result:rpc:ethereum
      ~|  json
      ?>  ?=(%o -.json)
      (~(got by p.json) 'from')
    --
  ::
  ::  +get-roll-hashes makes a list of hashes of all transactions from $rolls-map
  ++  get-roll-hashes
    |=  =rolls-map  ^-  (list keccak)
    %-  zing
    %+  turn  ~(val by rolls-map)
    |=  a=(map keccak effects:naive)
    ~(tap in ~(key by a))
  ::
  ::  +get-timestamps retrieves the timestamps for a list of block numbers
  ++  get-timestamps
    ::  TODO: would be better to call the eth-get-timestamps thread directly
    ::  rather than copy and paste the code for it here, but child threads seem
    ::  to be broken.
    |=  [node-url=@t blocks=(list blocknum)]
    %-  %-  slog  :_  ~
      leaf+"getting timestamps from ethereum node"
    =/  m  (strand ,(map blocknum @da))
    ^-  form:m
    =|  out=(map blocknum @da)
    |^  ^-  form:m
      =*  loop  $
      ?:  =(~ blocks)  (pure:m out)
      ;<  res=(list [@t json])  bind:m
        (request-blocks (scag 100 blocks) node-url)
      %_  loop
        out     (~(gas by out) (parse-results res))
        blocks  (slag 100 blocks)
      ==
    ::
    ++  request-blocks
      |=  [blocks=(list blocknum) node-url=@t]
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
  ::  +compute-effects calls +naive to compute the state transitions for all
  ::  logs, but it returns a map that only has the effects for L2 transactions,
  ::  leaving out L1 transactions. we need to compute all of them in order to
  ::  determine whether the transactions were valid.
  ++  compute-effects
    |=  $:  nas=^state:naive
            =events
            =net
            naive-contract=address
            chain-id=@ud
        ==
    =|  out=rolls-map
    ^+  out
    %-  %-  slog  :_  ~
      leaf+"processing state transitions beginning from stored snapshot"
    ::
    |-
    ?~  events  out
    =/  log=event-log:rpc:ethereum  i.events
    ?~  mined.log
      ~&  >>  'empty log'
      $(events t.events)
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
      events  t.events
      out   ?.  =(%bat +<.input)
              out  ::  skip L1 logs
            :: there's probably a better way to do this
            =/  cur  (~(get by out) block)
            ?~  cur
              %+  ~(put by out)  block
              (my [[transaction-hash.u.mined.log effects]~])
            %+  ~(jab by out)  u.cur
            |=  m=(map keccak effects:naive)
            (~(put by m) transaction-hash.u.mined.log effects)
    ==
  ::
  ::  +export-csv writes a (list cord) as csv to disk at .pax
  ++  export-csv
    |=  [in=(list cord) pax=path]
    =/  m  (strand ,~)
    ^-  form:m
    ;<  =bowl:spider  bind:m  get-bowl
    =-  (send-raw-card %pass / %arvo %c %info -)
    %+  foal:space:userlib
      :(weld /(scot %p our.bowl)/base/(scot %da now.bowl) pax /txt)
    [%txt !>(in)]
  ::
  ::  +make-csv takes in a (list tx-data) and makes it into a (list cord) to be
  ::  saved as a csv file
  ++  make-csv
    |=  in=(list tx-data)
    ^-  (list cord)
    :-  %-  crip
        ;:  weld
          "block number,"
          "timestamp,"
          "roller address,"
          "roll hash,"
          "tx hash,"
          "sending ship,"
          "sending proxy,"
          "nonce,"
          "gas price,"
          "length of input data,"
          "success or failure,"
          "function name,"
          "parent"
        ==
    %+  turn  in
      |=  =tx-data
      %-  crip
      ;:  weld
        (scow %ud block.tx-data)      ","
        (scow %da timestamp.tx-data)  ","
        (scow %ux roller.tx-data)     ","
        (scow %ux roll-hash.tx-data)  ","
        (scow %ux tx-hash.tx-data)    ","
        (scow %p sender.tx-data)      ","
        (scow %tas proxy.tx-data)     ","
        (scow %ud nonce.tx-data)      ","
        (scow %ud gas.tx-data)        ","
        (scow %ux length.tx-data)     ","
        (scow %f suc.tx-data)         ","
        (scow %tas action.tx-data)    ","
        (scow %p parent.tx-data)
      ==
--
