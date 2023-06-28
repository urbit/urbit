/-  spider, ethdata=eth-provider, json-rpc
/+  strandio, ethereum, eth-provider
=,  strand=strand:spider
=,  dejs-soft:format
=,  strand-fail=strand-fail:libstrand:spider
=,  jael
=<
^-  thread:spider
|=  arg=vase
=/  m  (strand ,vase)
^-  form:m
=/  address  0x1d54.e0b2.8269.645e.75b1.1baa.9845.d8b0.c6ea.4147
=/  pk  0x50af.3cb5.6f61.9458.9e30.b645.4017.e4ca.7760.2459.2c1a.263e.c458.3bdd.3ece.69ce
:: You might need to create your own raw tx for testing in your environment
=/  raw-tx  
0xf868.8086.0918.4e72.a000.8303.0000.94fa.3caa.bc8e.efec.2b5e.2895.e5af.bf79.379e.7268.a701.8082.0a95.a01b.454a.f567.4416.0fed.c195.0282.8de3.e3c6.a14e.b927.66ec.f494.bd22.ce11.9641.d3a0.44c2.d606.55b1.b052.bd00.d354.d1c2.b1c6.bc2c.52fa.b7ff.556f.c879.e5f7.0114.26de
=/  raw-tx  0xd4.6e8d.d67c.5d32.be8d.46e8.dd67.c5d3.2be8.058b.b8eb.9708.70f0.7244.5675.058b.b8eb.9708.70f0.7244.5675
:: =/  url  'https://mainnet.infura.io/v3/ab02ee0a33254083969f8550b02de4e1'
=/  url  'http://eth-mainnet.urbit.org:8545'
=/  get-timestamps  [%eth-get-timestamps !>(['http://localhost:8545' ~[1]])]
=/  get-tx-receipts  [%eth-get-tx-receipts !>(['http://localhost:8545' ~[0x123]])]
:: =/  send-txs  [%eth-send-txs !>(['http://localhost:8545' 0 ~[raw-tx]])]
=/  send-txs  [%eth-send-txs !>([url 0 ~[raw-tx]])]
=/  roller-send  [%roller-send !>(['http://localhost:8545' address 0 pk 0 0 0 ~[[0 [0 0] [[~zod %spawn] [%spawn ~zod address]]]]])]
=/  roller-nonce  [%roller-nonce !>(['http://localhost:8545' pk])]
=/  read-contract  [%eth-read-contract !>(['http://localhost:8545' [[~ 'unitid'] address ['func' ~[[%address %.y]]]]])]
=/  prep-command  [%claz-prep-command !>(['http://localhost:8545' [%generate /updates %mainnet address [%invites ~nut /~zod/base/197/out/txt]]])]  :: bad input
=/  prep-command2  [%claz-prep-command !>(['http://localhost:8545' [%generate /updates %mainnet address [%deed '{}']]])]
=/  eth-watcher  [%eth-watcher !>([~ [['http://localhost:8545' %.n ~h1 ~m1 0 [~ 0] ~[0x123] ~[0x123] ~[0x123]] 0 ~ ~[[[0x123 123] 0x123]]]])]
=/  proto-read  `proto-read-request:rpc:ethereum`[[~ 'unitid'] address ['eth_getBlockByNumber' ~[[%uint 13] [%bool %.y]]]]


:: WORKING
:: ;<  t=vase  bind:m  (test-thread prep-command2)
:: ;<  t=vase  bind:m  (test-thread get-timestamps)  :: timestamps issue
:: ;<  t=vase  bind:m  (test-thread [%eth-read-contract !>(['http://localhost:8545' proto-read])]) :: bad input to thread

:: TO BE TESTED


:: NOT WORKING YET
:: ;<  t=vase  bind:m  (test-thread get-tx-receipts)  :: (list [@t json])
:: ;<  t=vase  bind:m  (test-thread send-txs)  ::  bad input

:: Ethio handles these (for now?)
:: ;<  t=vase  bind:m  (test-thread roller-send)
:: ;<  t=vase  bind:m  (test-thread roller-nonce)
:: ;<  t=vase  bind:m  (test-thread eth-watcher)

:: CHANGED FILES
:: arvo/ted/eth/read-contract
:: landscape/ted/eth/read-contract
:: CHANGED FILES
:: arvo/ted/claz/prep-command
:: arvo/ted/eth/send-txs
:: landscape/ted/eth/send-txs
:: arvo/ted/eth/get-tx-receipts
:: arvo/ted/naive-csv
:: arvo/app/eth-sender
:: landscape/ted/eth/get-timestamps
:: arvo/app/gaze

(pure:m !>(t))
::
|%
++  test-thread
|=  [thread-name=@t args=vase]
  =/  m  (strand ,vase)
  ;<  =bowl:spider  bind:m  get-bowl:strandio
  =/  tid  `@ta`(cat 3 'strand_' (scot %uv (sham thread-name eny.bowl)))
  ;<  ~             bind:m  (watch-our:strandio /awaiting/[tid] %spider /thread-result/[tid])
  ;<  ~             bind:m  %-  poke-our:strandio
                            :*  %spider
                                %spider-start
                                !>([`tid.bowl `tid byk.bowl(r da+now.bowl) thread-name args])
                            ==
  ;<  =cage         bind:m  (take-fact:strandio /awaiting/[tid])
  ;<  ~             bind:m  (take-kick:strandio /awaiting/[tid])
  ?+  p.cage  ~|([%strange-thread-result p.cage %child tid] !!)
  %thread-done  (pure:m q.cage)
  %thread-fail  :: ~&  [%cage q.cage]
                !!
                ::(strand-fail !<([term tang] q.cage))
                
  ==
  :: ~&  q.cage
  :: ~&  '======================'
  :: (pure:m !>(q.cage))
--
