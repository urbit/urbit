::
::  there's a small state machine here that goes like this (happy path):
::  =/  wen  ~
::  apex
::  ->  [if =(~ wen)]
::    ->  apex
::      [else]
::    ->  wen=`(add now ~s10)
::    ->  send-next-batch
::        [n times]
::      ->  eth-send-raw-transaction
::      ->  sigh-send
::    ->  wait 30s in behn
::      ->  wake-see
::          [n times]
::        ->  wen=~
::        ->  eth-get-transaction-receipt
::        ->  sigh-see
::        ->  apex
::
/+  ceremony
::
|%
++  state
  $:  txs=(list @ux)
      see=(set @ux)
      wen=(unit @da)
  ==
::
++  move  (pair bone card)
++  card
  $%  [%hiss wire ~ mark %hiss hiss:eyre]
      [%info wire ship desk nori:clay]
      [%rest wire @da]
      [%wait wire @da]
  ==
--
::
|_  [bol=bowl:gall state]
::
++  this  .
++  pretty-see  (turn (sort (turn ~(tap in see) mug) lth) @p)
::
++  prep
  |=  old=(unit *)
  ?:  ?=([~ * * ~ @da] old)
    ~&  [%cancelling +>+>.old]
    [[ost.bol %rest /see +>+>.old]~ ..prep]
  [~ ..prep]
::
::  usage:
::
::  generate txs starting from nonce 0 on fake chain at 11 gwei
::  from address; store at path
::    :send-txs [%gen %/txs/eth-txs %fake 0 11 '0x0000000']
::
::  generate txs starting from nonce 0 on fake chain at 11 gwei;
::  store at path
::    :send-txs [%sign %/txs/txt %/txs/eth-txs %/pk/txt]
::
::  send all but first 50 txs from path
::    :send-txs [%send %/txs/txt 50]
::
++  poke-noun
  |=  $%  $:  %gen
              pax=path
              net=?(%fake %main %ropsten)
              nonce=@ud
              gas-price=@ud
              addr=@t
          ==
          [%sign out=path in=path key=path]
          [%send pax=path skip=@ud]
      ==
  ^-  [(list move) _this]
  ?-    +<-
      %gen
    =/  addr  (rash addr ;~(pfix (jest '0x') hex))
    =/  tox  (sequence:ceremony now.bol net gas-price nonce addr)
    [[(write-file-transactions pax tox) ~] this]
  ::
      %sign
    =/  tox=(list cord)  (sign:ceremony now.bol in key)
    [[(write-file-wain out tox) ~] this]
  ::
      %send
    ~&  'loading txs...'
    =/  tox=(list cord)  .^((list cord) %cx pax)
    =.  tox  (slag skip tox)
    =.  txs
      %+  turn  tox
      (cork trip tape-to-ux:ceremony)
    ~&  [(lent txs) 'loaded txs']
    apex
  ==
::
++  write-file-wain
  |=  [pax=path tox=(list cord)]
  ^-  move
  ?>  ?=([@ desk @ *] pax)
  :*  ost.bol
      %info
      (weld /write pax)
      our.bol
      i.t.pax
      =-  &+[t.t.t.pax -]~
      =/  y  .^(arch %cy pax)
      ?~  fil.y
        ins+txt+!>(tox)
      mut+txt+!>(tox)
  ==
::
++  write-file-transactions
  |=  [pax=path tox=(list transaction:ethereum)]
  ^-  move
  ?>  ?=([@ desk @ *] pax)
  :*  ost.bol
      %info
      (weld /write pax)
      our.bol
      i.t.pax
      =-  &+[t.t.t.pax -]~
      =/  y  .^(arch %cy pax)
      ?~  fil.y
        ins+eth-txs+!>(tox)
      mut+eth-txs+!>(tox)
  ==
::
++  fan-requests
  |=  [wir=wire nodes=(list [tag=@tas url=purl:eyre]) jon=json]
  ::  =-  ~&  [batch=((list ,[bone * wire]) (turn - |=(* [- +< +>-]:+<))) jon=jon]  -
  ^-  (list move)
  %+  turn  nodes
  |=  [tag=@tas url=purl:eyre]
  ^-  move
  :-  ost.bol
  :^  %hiss  (weld wir ~[tag])  ~
  :+  %json-rpc-response  %hiss
  (json-request:rpc:ethereum url jon)
::
++  batch-requests
  |=  [wir=wire req=(list [(unit @t) request:rpc:ethereum])]
  ^-  (list move)
  %^    fan-requests
      wir
    :~  =>  (need (de-purl:html 'http://localhost:8545'))
        geth+.(p.p |)
      ::
        =>  (need (de-purl:html 'http://localhost:8555'))
        parity+.(p.p |)
    ==
  a+(turn req request-to-json:rpc:ethereum)
::
++  send-next-batch
  ^-  [(list move) _this]
  ?:  =(0 (lent txs))
    ~&  'all sent!'
    [~ this(txs ~, see ~, wen ~)]
  ::  ~&  send-next-batch=pretty-see
  =/  new-count  (sub 500 ~(wyt in see))
  ?:  =(0 new-count)
    ~&  %no-new-txs-yet
    `this
  :_  this(txs (slag new-count txs))
  ~&  ['remaining txs: ' (lent txs)]
  ~&  ['sending txs...' new-count]
  %+  batch-requests  /send
  %+  turn  (scag new-count txs)
  |=  tx=@ux
  :-  `(crip 'id-' (scot %ux (end 3 10 tx)) ~)
  [%eth-send-raw-transaction tx]
::
++  sigh-json-rpc-response-send
  |=  [wir=wire res=response:rpc:jstd]
  ^-  [(list move) _this]
  ?:  ?=(%fail -.res)
    ~&  %send-failed
    `this
  ?>  ?=(%batch -.res)
  ::  ~&  sigh-send-a=pretty-see
  =.  see
    %-  ~(uni in see)
    %-  silt
    ^-  (list @ux)
    %+  murn  bas.res
    |=  r=response:rpc:jstd
    ^-  (unit @ux)
    ?:  ?=(%error -.r)
      ?:  ?|  =('known transaction' (end 3 17 message.r))
              =('Known transaction' (end 3 17 message.r))
              =('Transaction with the same ' (end 3 26 message.r))
          ==
        ~&  [%sent-a-known-transaction--skipping wir r]
        ~
      ?:  =('Nonce too low' message.r)
        ~&  %nonce-too-low--skipping
        ~
      ~|  :-  'transaction send failed, game over'
          [code.r message.r]
      !!
    ?>  ?=(%result -.r)
    :-  ~
    %-  tape-to-ux:ceremony
    (sa:dejs:format res.r)
  ::  ~&  sigh-send-b=pretty-see
  `this
::
++  apex
  ^-  [(list move) _this]
  ~&  :_  ~(wyt in see)
      'waiting for transaction confirms... '
  ?.  =(~ wen)  [~ this]
  =.  wen  `(add now.bol ~s10)
  ::  ~&  apex=[wen pretty-see]
  =^  moves  this  send-next-batch
  [[[ost.bol %wait /see (need wen)] moves] this]
::
++  wake-see
  |=  [wir=wire ~]
  ^-  [(list move) _this]
  =.  wen  ~
  ::  ~&  wake-see=[wen pretty-see]
  ?:  =(~ see)
    apex
  :_  this
  %+  batch-requests  /see
  %+  turn  ~(tap in see)
  |=  txh=@ux
  :-  `(crip 'see-0x' ((x-co:co 64) txh))
  [%eth-get-transaction-receipt txh]
::
++  sigh-json-rpc-response-see
  |=  [wir=wire res=response:rpc:jstd]
  ^-  [(list move) _this]
  ?:  ?|  ?=(%error -.res)
          ?=(%fail -.res)
      ==
    ~&  [%bad-rpc-response--kicking res]
    apex
    ::  `this
  ?>  ?=(%batch -.res)
  ?:  =(~ see)
    apex
  ?:  =(0 (lent bas.res))
    ::TODO  node lost our txs?
    ~&  [%txs-lost-tmp wir '!!']
    apex
  ::  ~&  sigh-see-a=pretty-see
  =.  see
    %-  ~(dif in see)
    %-  silt
    ^-  (list @ux)
    %+  murn  bas.res
    |=  r=response:rpc:jstd
    ^-  (unit @ux)
    ?<  ?=(%batch -.r)
    ?<  ?=(%fail -.r)
    ~|  [id.r res]
    =+  txh=(tape-to-ux:ceremony (trip (rsh 3 4 id.r)))
    ~&  see-tx=[(@p (mug txh)) `@ux`txh]
    =*  done  `txh
    =*  wait  ~
    ?:  ?=(%error -.r)
      ~&  :-  'receipt fetch error'
          [code.r message.r]
      wait
    ?~  res.r  wait
    ?>  ?=(%o -.res.r)
    =/  status
      %-  tape-to-ux:ceremony
      %-  sa:dejs:format
      (~(got by p.res.r) 'status')
    ?:  =(1 status)
      done
    ~&  [%see-bad-status status]
    wait
  ::  ~&  sigh-see-b=pretty-see
  apex
::
++  sigh-tang
  |=  [wir=wire err=tang]
  ~&  [%sigh-tang wir]
  ~&  (slog err)
  ?:  =(~ wen)  [~ this]
  =.  wen  `(add now.bol ~s10)
  [[ost.bol %wait /see (need wen)]~ this]
--
