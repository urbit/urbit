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
::  TODO: change block maps to ordered maps
::
/-  spider
::
/+  dice,
    ethereum,
    ethio,
    naive,
    *strandio
::
::  imports most recent downloaded logs
::/*  logs  %eth-logs  /app/azimuth/logs/eth-logs
::/*  logs  %eth-logs  /app/azimuth/tid/eth-logs
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
  +$  events  (list event-log:rpc:ethereum)
  +$  address   address:naive  :: @ux
  +$  hash      @ux            :: used for transaction and roll hashes
  +$  blocknum  number:block   :: @udblocknumber
  ::
  +$  net  ?(%mainnet %ropsten %local %default)
  ::
  +$  roll-data
    $:  roller-address=address
        roll-hash=hash
        tx-hash=hash
        gas-price=@ud
    ==
  ::
  +$  l2-event-data
    $:  =roll-data
        sending-ship=@p
        sending-proxy=proxy:naive
        nonce=nonce:naive
        data-length=* :: maybe octs?
        action=*
        suc=?
    ==
  ::
  +$  events-time  (list [event-log:rpc:ethereum timestamp=@da])
  ::
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
    =/  l2-logs=events  (filter-l2 logs naive-contract)
    %-  %-  slog  :_  ~
        leaf+"processing {<net>} ethereum logs with {<(lent logs)>} total events, of which {<(lent l2-logs)>} are l2 events"
    =/  blocks=(list blocknum)        (get-block-numbers l2-logs)
    =/  tx-hashes=(list hash)     (get-tx-hashes l2-logs)
    =/  block-jar=(jar blocknum hash)  (block-tx-jar l2-logs)
    ;<  timestamps=(map blocknum @da)  bind:m  (get-timestamps blocks)
    ;<  tx-data=(map hash [gas=@ud sender=address])  bind:m  (get-tx-data tx-hashes)
    =/  rolling  (collate-roll-data blocks block-jar timestamps tx-data)
    (pure:m !>(rolling))
  ::
  ++  collate-roll-data
    |=  $:  blocks=(list blocknum)
            block-jar=(jar blocknum hash)
            ::l2-logs=events
            timestamps=(map blocknum @da)
            tx-data=(map hash [gas=@ud sender=address])
        ==
    =|  block-map=(map blocknum [timestamp=@da tx=(map hash [gas=@ud sender=address])])
    |-
    ?~  blocks  block-map
    =/  block  i.blocks
    =/  tx-hashes  (~(get ja block-jar) block)
    =/  tx=(map hash [gas=@ud sender=address])
      %-  ~(gas by *(map hash [gas=@ud sender=address]))
      %+  turn  tx-hashes
      |=  txh=hash  ^-  [txh=hash [gas=@ud sender=address]]
      [txh (~(got by tx-data) txh)]
    %=  $
      blocks     t.blocks
      block-map  (~(put by block-map) block [(~(got by timestamps) block) tx])
    ==
  ::
  ++  get-tx-data
    :: retrieves transaction receipts for rolls, extracting the gas cost and sender
    |=  tx-hashes=(list hash)
    =/  m  (strand ,(map hash [gas=@ud sender=address]))
    ^-  form:m
    =|  out=(map hash [gas=@ud sender=address])
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
      |=  tx-hashes=(list hash)
      %+  request-batch-rpc-strict:ethio  node-url
      %+  turn  tx-hashes
      |=  txh=hash
      ^-  [(unit @t) request:rpc:ethereum]
      :-  `(crip '0' 'x' ((x-co:co 64) txh))
      [%eth-get-transaction-receipt txh]
    ::
    ++  parse-results
      |=  res=(list [@t json])
      ^-  (list [txh=hash [gas=@ud sender=address]])
      %+  turn  res
      |=  [id=@t =json]
      ^-  [txh=hash [gas=@ud sender=address]]
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
    ::  TODO: make this return a list of processed events
    ::  along with gas costs and timestamps
    ::  |-
    ::  ?~  events
    ::    'no events!'
    ::  =/  log=event-log:rpc:ethereum  i.events
    ::  ?~  mined.log
    ::    'empty log!'
    ::  =/  =^input:naive
    ::    :-  block-number.u.mined.log
    ::    ?.  =(naive-contract address.log)
    ::      :-  %log
    ::      [address.log (data-to-hex:dice data.log) topics.log]
    ::    ?~  input.u.mined.log
    ::      ~&  >  'empty L2 transaction'
    ::      [%bat *@]
    ::    [%bat u.input.u.mined.log]
    ::  ?:  ?=(%log +<.input)
    ::    $(events t.events)
    ::  $(events t.events)
    ::  ?~  l2-logs
    ::    (pure:m !>(net))
    ::  (pure:m !>(blocks))
  ::
  ++  filter-l2
    |=  [logs=events naive-contract=address]  ^-  events
    %+  skim  logs
    |=  log=event-log:rpc:ethereum  ^-  ?
    ?~  mined.log  %.n
    =(naive-contract address.log)
  ::
  ++  block-tx-jar
    |=  logs=events  ^-  (jar blocknum hash)
    =|  block-jar=(jar blocknum hash)
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
    |=  logs=events  ^-  (list hash)
    %+  turn  logs
    |=  log=event-log:rpc:ethereum
    :: shouldn't crash since +filter-l2 already checks if mined.log is empty
    transaction-hash:(need mined.log)
--
