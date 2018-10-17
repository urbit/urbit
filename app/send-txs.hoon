::
/+  ceremony
::
|%
++  state
  $:  txs=(list @ux)
      see=(set @ux)
      wen=@da
  ==
::
++  move  (pair bone card)
++  card
  $%  [%hiss wire [~ ~] mark %hiss hiss:eyre]
      [%wait wire @da]
  ==
--
::
|_  [bol=bowl:gall state]
::
++  this  .
::
++  prep
  |=  old=(unit *)
  [~ ..prep]
::
++  poke-noun
  |=  skip=@ud
  ^-  [(list move) _this]
  ~&  'loading txs...'
  =/  tox=(list cord)
    .^  (list cord)  %cx
        /(scot %p our.bol)/home/(scot %da now.bol)/txs/txt
    ==
  =.  tox  (slag skip tox)
  =.  txs
    %+  turn  tox
    (cork trip tape-to-ux:ceremony)
  ~&  [(lent txs) 'loaded txs']
  send-next-batch
::
++  batch-requests
  |=  [wir=wire req=(list [(unit @t) request:ethe])]
  ^-  move
  :-  ost.bol
  :^  %hiss  wir  [~ ~]
  :+  %json-rpc-response  %hiss
  %+  json-request:ethereum
    =+  (need (de-purl:html 'http://localhost:8545'))
    -(p.p |)
  a+(turn req request-to-json:ethereum)
::
++  send-next-batch
  ^-  [(list move) _this]
  ?:  =(0 (lent txs))
    ~&  'all sent!'
    [~ this]
  :_  .(txs (slag 50 txs))
  ~&  ['remaining txs: ' (lent txs)]
  ~&  'sending 50 txs...'
  :_  ~
  %+  batch-requests  /send
  %+  turn  (scag 50 txs)
  |=  tx=@ux
  :-  `'id'
  [%eth-send-raw-transaction tx]
::
++  sigh-json-rpc-response-send
  |=  [wir=wire res=response:rpc:jstd]
  ^-  [(list move) _this]
  ?>  ?=(%batch -.res)
  =.  see
    %-  ~(gas in see)
    %+  murn  bas.res
    |=  r=response:rpc:jstd
    ^-  (unit @ux)
    ?:  ?=(%error -.r)
      ?:  =('Known transaction' (end 3 17 message.r))
        ~&  %sent-a-known-transaction--skipping
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
  (wake-see ~ ~)
::
++  kick-timer
  ^-  [(list move) _this]
  ~&  :_  ~(wyt in see)
      'waiting for transaction confirms... '
  ?:  (gth wen now.bol)  [~ this]
  =.  wen  (add now.bol ~s30)  ::TODO  more sane/polite value
  [[ost.bol %wait /see wen]~ this]
::
++  wake-see
  |=  [wir=wire ~]
  ^-  [(list move) _this]
  :_  this(see ~)
  :_  ~
  ^-  move
  %+  batch-requests  /see
  %+  turn  ~(tap in see)
  |=  txh=@ux
  :-  `(crip '0' 'x' ((x-co:co 64) txh))
  [%eth-get-transaction-receipt txh]
::
++  sigh-json-rpc-response-see
  |=  [wir=wire res=response:rpc:jstd]
  ^-  [(list move) _this]
  ?>  ?=(%batch -.res)
  ?:  =(0 (lent bas.res))
    ::TODO  node lost our txs?
    ~|  %txs-lost-tmp
    !!
  =.  see
    %-  ~(gas in see)
    %+  murn  bas.res
    |=  r=response:rpc:jstd
    ^-  (unit @ux)
    ?<  ?=(%batch -.r)
    =+  txh=(tape-to-ux:ceremony (trip id.r))
    =*  done  ~
    =*  wait  `txh
    ?:  ?=(%error -.r)
      ~&  :-  'receipt fetch error'
          [code.r message.r]
      wait
    ?~  res.r  wait
    ?>  ?=(%o -.res.r)
    ?:  .=  1
        %-  tape-to-ux:ceremony
        %-  sa:dejs:format
        (~(got by p.res.r) 'status')
      done
    wait
  ?~  see
    ~&  'batch confirmed, next!'
    send-next-batch
  kick-timer
::
++  sigh-tang
  |=  [wir=wire err=tang]
  ~&  %sigh-tang
  ~&  (slog err)
  ?:  (gth wen now.bol)  [~ this]
  =.  wen  (add now.bol ~s30)  ::TODO  more sane/polite value
  [[ost.bol %wait /see wen]~ this]
--
