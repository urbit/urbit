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
/-  rpc=json-rpc,
    spider
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
  ::  id=[@uxblockhash @udblocknumber]
  +$  id      id:block
  ::
  +$  net  ?(%mainnet %ropsten %local %default)
  ::
  +$  roll-data
    $:  block-number=number:block
        timestamp=@da
        roller-address=@ux
        roll-hash=*
        tx-hash=*
        gas-price=@ud
    ==
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
|%
  ++  process-logs
    |=  arg=vase
    =+  !<([~ =net] arg)
    =/  m  (strand ,vase)
    ^-  form:m
    ;<  logs=events  bind:m  (scry events /gx/azimuth/logs/noun)
    =/  [naive-contract=@ux chain-id=@]
      [naive chain-id]:(get-network:dice net)
    =/  l2-logs=events  (filter-l2 logs naive-contract)
    %-  %-  slog  :_  ~
        leaf+"processing {<net>} ethereum logs with {<(lent logs)>} total events, of which {<(lent l2-logs)>} are l2 events"
    =/  blocks=(list @ud)  (get-block-numbers l2-logs)
    ::;<  out=(list [block=@ud timestamp=@da])  bind:m  (get-timestamps blocks)
    =/  tx-hashes=(list @ux)  (get-tx-hashes l2-logs)
    ;<  out=(list [@t json])  bind:m  (get-tx-receipts tx-hashes)
    (pure:m !>(-.out))
  ::
  ++  get-tx-receipts
    |=  tx-hashes=(list @ux)
    =/  m  (strand ,(list [@t json]))
    ^-  form:m
    =|  out=(list [@t json])
    |^  ^-  form:m
      =*  loop  $
      ?:  =(~ tx-hashes)  (pure:m out)
      ;<  res=(list [@t json])  bind:m
        (request-receipts (scag 100 tx-hashes))
      %_  loop
        out        (weld out res)
        tx-hashes  (slag 100 tx-hashes)
      ==
    ::
    ++  request-receipts
      |=  tx-hashes=(list @ux)
      %+  request-batch-rpc-strict:ethio  node-url
      %+  turn  tx-hashes
      |=  txh=@ux
      ^-  [(unit @t) request:rpc:ethereum]
      :-  `(crip '0' 'x' ((x-co:co 64) txh))
      [%eth-get-transaction-receipt txh]
    ::
    ::  ++  parse-results
    ::    |=  res=(list [@t json])
    ::    ^+  out
    ::    %+  turn  res
    ::    |=  [id=@t =json]
    ::    ^-  [@ux @ud]
    ::    :-  (slav %ux id)
    ::    %-  parse-hex-result:rpc:ethereum
    ::    ~|  json
    ::    ?>  ?=(%o -.json)
    ::    (~(got by p.json) 'gasUsed')
    --
  ::
  ++  get-timestamps
    ::  TODO: would be better to call the eth-get-timestamps thread directly
    ::  rather than copy and paste the code for it here
    |=  blocks=(list @ud)
    =/  m  (strand ,(list [@ud @da]))
    ^-  form:m
    =|  out=(list [block=@ud timestamp=@da])
    |^  ^-  form:m
      =*  loop  $
      ?:  =(~ blocks)  (pure:m out)
      ;<  res=(list [@t json])  bind:m
        (request-blocks (scag 100 blocks))
      %_  loop
        out     (weld out (parse-results res))
        blocks  (slag 100 blocks)
      ==
    ::
    ++  request-blocks
      |=  blocks=(list @ud)
      %+  request-batch-rpc-strict:ethio  node-url
      %+  turn  blocks
      |=  block=@ud
      ^-  [(unit @t) request:rpc:ethereum]
      :-  `(scot %ud block)
      [%eth-get-block-by-number block |]
    ::
    ++  parse-results
      |=  res=(list [@t json])
      ^+  out
      %+  turn  res
      |=  [id=@t =json]
      ^-  [@ud @da]
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
    |=  [logs=events naive-contract=@ux]  ^-  events
    %+  skim  logs
    |=  log=event-log:rpc:ethereum  ^-  ?
    ?~  mined.log  %.n
    =(naive-contract address.log)
  ::
  ++  get-block-numbers
    |=  logs=events  ^-  (list @ud)
    %+  turn  logs
    |=  log=event-log:rpc:ethereum
    :: shouldn't crash since +filter-l2 already checks if mined.log is empty
    block-number:(need mined.log)
  ::
  ++  get-tx-hashes
    |=  logs=events  ^-  (list @ux)
    %+  turn  logs
    |=  log=event-log:rpc:ethereum
    :: shouldn't crash since +filter-l2 already checks if mined.log is empty
    transaction-hash:(need mined.log)
--
