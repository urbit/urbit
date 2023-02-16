::  naive-csv: produces csv file containing L2 transaction data
::
::    takes in the network to use and the ethereum node url to grab data from.
::    it starts with the azimuth snapshot and scries the logs from %azimuth.
::    it then produces a csv file containing the following data on L2
::    transactions:
::
::    - block number
::    - timestamp
::    - roller address
::    - roll hash
::    - tx hash
::    - sending ship
::    - sending proxy
::    - nonce
::    - gas price
::    - length of input data
::    - success or failure
::    - function name
::    - spawning ship (^sein:title)
::
::    A lot of the data-scrounging here is stuff that %roller already keeps track
::    of. We could just scry it from there, but then this thread needs to be run
::    on the roller ship. So we rebuild the list of historical transactions
::    ourselves so that this can run from any ship.
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
=,  strand=strand:spider
=,  jael
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
  +$  net       net:dice       :: ?(%mainnet %goerli %local %default)
  +$  roll-dat                 :: all data required for each roll
    [[gas=@ud sender=address] =effects:naive]
  +$  block-dat                :: all data required for each block
    [timestamp=@da rolls=(map keccak roll-dat)]
  +$  block-map  (map blocknum block-dat)
  +$  rolls-map  (map blocknum (map keccak effects:naive))
  ::
  +$  action
    $?  %transfer-point
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
  +$  tx-data
    $:  =blocknum
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
  ::
  ++  process-logs
    |=  arg=vase
    =+  !<([~ =net node-url=@t] arg)
    =/  pax=path  /naive-exports/csv  :: data will be saved here
    =/  m  (strand ,vase)
    ^-  form:m
    ;<  =events  bind:m  (scry events /gx/azimuth/logs/noun)
    =/  [naive-contract=address chain-id=@]
      [naive chain-id]:(get-network:dice net)
    ;<  =bowl:spider  bind:m  get-bowl
    =/  snap=snap-state:dice
      .^  snap-state:dice  %gx
          /(scot %p our.bowl)/azimuth/(scot %da now.bowl)/last-snap/noun
      ==
    ::
    ;<  ~  bind:m
      %-  flog-text  %+  weld  "naive-csv: processing {<net>} ethereum logs "
                               "with {<(lent events)>} events"
    =/  =rolls-map
      (compute-effects nas.snap events net naive-contract chain-id)
    ;<  ~  bind:m  (flog-text "naive-csv: getting timestamps")
    ;<  tim=thread-result  bind:m
      %+  await-thread  %eth-get-timestamps
      !>([node-url ~(tap in ~(key by rolls-map))])
    =/  timestamps  %-  ~(gas by *(map blocknum @da))
                    ?-  tim
                      [%.y *]  ;;((list [@ud @da]) q.p.tim)
                      [%.n *]
                        =>  (mean 'naive-csv: %eth-get-timestamps failed' p.tim)
                        !!
                    ==
    ;<  ~  bind:m  (flog-text "naive-csv: got timestamps")
    ;<  ~  bind:m  (flog-text "naive-csv: getting tx receipts")
    ;<  gaz=thread-result  bind:m
      %+  await-thread  %eth-get-tx-receipts
      !>([node-url (get-roll-hashes rolls-map)])
    =/  gas-sender  %-  ~(gas by *(map keccak [gas=@ud sender=address]))
                    ?-  gaz
                      [%.y *]  (parse-gas-sender ;;((list [@t json]) q.p.gaz))
                      [%.n *]
                        =>  (mean 'naive-csv: %eth-tx-receipts failed' p.gaz)
                        !!
                    ==
    ;<  ~  bind:m  (flog-text "naive-csv: got tx receipts")
    =/  csv=(list cord)
      (make-csv (flatten (collate-roll-data rolls-map timestamps gas-sender)))
    ;<  ~  bind:m  (export-csv csv pax)
    ;<  ~  bind:m  (flog-text :(weld "naive-csv: csv saved to %" (spud pax) "/"))
    ::
    (pure:m !>(~))
  ::  +collate-roll-data throws naive:effects, timestamps, and gas costs into
  ::  one $block-map
  ::
  ++  collate-roll-data
    |=  $:  =rolls-map
            timestamps=(map blocknum @da)
            roll-receipts=(map keccak [gas=@ud sender=address])
        ==
    =/  blocknums=(list blocknum)  ~(tap in ~(key by rolls-map))
    =|  =block-map
    ^+  block-map
    |-
    ?~  blocknums  block-map
    =/  =blocknum  i.blocknums
    =/  rolls=(map keccak [[gas=@ud sender=address] =effects:naive])
      %-  ~(gas by *(map keccak [[gas=@ud sender=address] =effects:naive]))
      %+  turn  ~(tap in ~(key by (~(got by rolls-map) blocknum)))
      |=  txh=keccak
      :+  txh
        (~(got by roll-receipts) txh)
      (~(got by (~(got by rolls-map) blocknum)) txh)
    %=  $
      blocknums     t.blocknums
      block-map  %+  ~(put by block-map)
                   blocknum
                 [(~(got by timestamps) blocknum) rolls]
    ==
  ::  +flatten takes a $block-map and creates a $tx-data for every transaction
  ::  in every roll, returned as a (list tx-data)
  ::
  ++  flatten
    |=  =block-map
    =/  blocks=(list [blocknum block-dat])  ~(tap by block-map)
    =|  tx-list=(list tx-data)
    ^+  tx-list
    ::  recurse through the list of blocks, getting the rolls submitted in that
    ::  block, their timestamp, and the gas price of that roll
    ::
    |-
    =*  block-loop  $
    ?~  blocks  tx-list
    =/  block=[=blocknum =block-dat]  i.blocks
    =/  roll-list=(list [=keccak =roll-dat])  ~(tap by rolls.block-dat.block)
    =|  block-tx-list=(list tx-data)
    ::  recurse through each roll, getting the transaction data from the effects
    ::
    |-
    =*  roll-loop  $
    ?~  roll-list
      %=  block-loop
        blocks   t.blocks
        tx-list  (welp tx-list block-tx-list)
      ==
    =/  roll=[=keccak =roll-dat]  i.roll-list
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
    ::
    =|  roll-tx-list=(list tx-data)
    =|  =tx-data
    =|  nonce-and-tx=[_| _|]
    |-
    =*  effect-loop   $
    ::  if we are processing a new transaction, initialize the parts of tx-data
    ::  that are identical for every transaction in the roll
    =?  tx-data  =([| |] nonce-and-tx)
      :*  blocknum.block  timestamp.block-dat.block  sender.roll-dat.roll
          keccak.roll  *keccak  *ship  *proxy:naive  *nonce:naive
          gas.roll-dat.roll  *@  |  *action  *ship
      ==
    ::  if we've gotten both the %nonce and %tx diff from a transaction, add the
    ::  tx-data to the list of tx for the roll
    ::
    ?:  =([& &] nonce-and-tx)
      %=  effect-loop
        nonce-and-tx  [| |]
        roll-tx-list  (snoc roll-tx-list tx-data)
      ==
    ::  if we've finished looping through the effects, add the tx list from the
    ::  roll to the list of tx for the block
    ::
    ?~  effects.roll-dat.roll
       %=  roll-loop
         roll-list      t.roll-list
         block-tx-list  (welp block-tx-list roll-tx-list)
       ==
    ::
    =/  =diff:naive  i.effects.roll-dat.roll
    ::  we ignore %operator, %dns, %point diffs
    ::
    ?+    diff
      $(effects.roll-dat.roll t.effects.roll-dat.roll)
    ::  %nonce is always the first diff from a given transaction.
    ::
        [%nonce *]
      %=  effect-loop
        -.nonce-and-tx         &
        sender.tx-data         ship.diff
        nonce.tx-data          nonce.diff
        proxy.tx-data          proxy.diff
        parent.tx-data         (^sein:title ship.diff)
        effects.roll-dat.roll  t.effects.roll-dat.roll
      ==
    ::  %tx is always the second diff from a given transaction.
    ::
        [%tx *]
      %=  effect-loop
        +.nonce-and-tx   &
        effects.roll-dat.roll     t.effects.roll-dat.roll
        action.tx-data            +<.tx.raw-tx.diff
        suc.tx-data               ?~  err.diff  &  |
        length.tx-data            `@`-.raw.raw-tx.diff
        tx-hash.tx-data           (hash-raw-tx:naive-tx raw-tx.diff)
      ==
    ==
  ::
  ++  parse-gas-sender
    |=  res=(list [@t json])
    ^-  (list [=keccak [gas=@ud sender=address]])
    %+  turn  res
    |=  [id=@t =json]
    ^-  [=keccak [gas=@ud sender=address]]
    :-  (hex-to-num:ethereum id)
    :-  %-  parse-hex-result:rpc:ethereum
      ~|  json
      ?>  ?=(%o -.json)
      (~(got by p.json) 'effectiveGasPrice')                   :: gas used in wei
    %-  parse-hex-result:rpc:ethereum
    ~|  json
    ?>  ?=(%o -.json)
    (~(got by p.json) 'from')
  ::  +get-roll-hashes makes a list of hashes of all transactions from $rolls-map
  ::
  ++  get-roll-hashes
    |=  =rolls-map  ^-  (list keccak)
    %-  zing
    %+  turn  ~(val by rolls-map)
    |=  a=(map keccak effects:naive)
    ~(tap in ~(key by a))
  ::  +compute-effects calls +naive to compute the state transitions for all
  ::  logs, but it returns a map that only has the effects for L2 transactions,
  ::  leaving out L1 transactions. we need to compute all of them in order to
  ::  determine whether the transactions were valid.
  ::
  ++  compute-effects
    |=  $:  nas=^state:naive
            =events
            =net
            naive-contract=address
            chain-id=@ud
        ==
    =|  out=rolls-map
    ^+  out
    ::
    |-
    ?~  events  out
    =/  log=event-log:rpc:ethereum  i.events
    ?~  mined.log
      ~&  >>  'naive-csv: empty log'
      $(events t.events)
    =/  =blocknum  block-number.u.mined.log
    =/  =^input:naive
      :-  blocknum
      ?.  =(naive-contract address.log)
        :-  %log
        [address.log (data-to-hex:dice data.log) topics.log]
      ?~  input.u.mined.log
        ~&  >>  'naive-csv: empty L2 transaction'
        [%bat *@]
      [%bat u.input.u.mined.log]
    =^  =effects:naive  nas
      (%*(. naive lac |) verifier:naive-tx chain-id nas input)
    %=  $
      events  t.events
      out   ?.  =(%bat +<.input)
              out  ::  skip L1 logs
            =/  cur  (~(get by out) blocknum)
            ?~  cur
              %+  ~(put by out)  blocknum
              (my [[transaction-hash.u.mined.log effects]~])
            %+  ~(put by out)  blocknum
            (~(put by u.cur) transaction-hash.u.mined.log effects)
    ==
  ::  +export-csv writes a (list cord) as csv to disk at .pax
  ::
  ++  export-csv
    |=  [in=(list cord) pax=path]
    =/  m  (strand ,~)
    ^-  form:m
    ;<  =bowl:spider  bind:m  get-bowl
    =-  (send-raw-card %pass / %arvo %c %info -)
    %+  foal:space:userlib
      ;:  weld
        /(scot %p our.bowl)/base/(scot %da now.bowl)
        pax
        /(scot %da now.bowl)/txt
      ==
    [%txt !>(in)]
  ::  +make-csv takes in a (list tx-data) and makes it into a (list cord) to be
  ::  saved as a csv file
  ::
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
        (scow %ud blocknum.tx-data)   ","
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
