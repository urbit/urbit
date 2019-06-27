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
      outstanding-send=_|
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
::  sign txs for gasses of 2 and 11 gwei; (~ for default gwei set)
::  store at path
::    :send-txs [%sign %/txs %/txs/eth-txs %/pk/txt ~[2 0]]
::
::  read nonce range from signed transactions at path
::    :send-txs [%read %txs/txt]
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
        ::
          [%sign bout=path in=path key=path gasses=(list @ud)]
        ::
          [%read pax=path]
        ::
          $:  %send
              pax=path
              how=?(%nonce %number)                ::  tx nonce / index in file
              range=(unit $@(@ud (pair @ud @ud)))  ::  inclusive. end optional
          ==
      ==
  ^-  [(list move) _this]
  ?-    +<-
      %gen
    =/  addr  (rash addr ;~(pfix (jest '0x') hex))
    =/  tox  (sequence:ceremony now.bol net gas-price nonce addr)
    [[(write-file-transactions pax tox) ~] this]
  ::
      %sign
    :_  this
    %+  turn
      ?.  =(~ gasses)  gasses
      ::  default gwei set
      ~[3 4 6 9 11 21 31]
    |=  gas=@ud
    %+  write-file-wain
      ::  add gas amount to path
      =+  end=(dec (lent bout))
      =-  (weld (scag end bout) -)
      ?:  =(0 gas)  [(snag end bout) /txt]
      :_  /txt
      (cat 3 (snag end bout) (crip '-' ((d-co:co 1) gas)))
    %-  sign:ceremony
    :^  now.bol  in  key
    ::  modify tx gas if non-zero gwei specified
    ?:  =(0 gas)  ~
    `(mul gas 1.000.000.000)
  ::
      %read
    =+  tox=.^((list cord) %cx pax)
    =+  [first last]=(read-nonces tox)
    ~&  %+  weld
          "Found nonces {(scow %ud first)} through {(scow %ud last)}"
        " in {(scow %ud (lent tox))} transactions."
    [~ this]
  ::
      %send
    ~&  'loading txs...'
    =.  see  ~
    =/  tox=(list cord)  .^((list cord) %cx pax)
    =.  tox
      ?~  range  tox
      =*  r  u.range
      ?:  ?=(%number how)
        ?@  r
          (slag r tox)
        %+  slag  p.r
        (scag q.r tox)
      =+  [first last]=(read-nonces tox)
      ?:  !=((lent tox) +((sub last first)))
        ~|  'woah, probably non-contiguous set of transactions'
        !!
      ?@  r
        (slag (sub r first) tox)
      (slag (sub p.r first) (scag (sub +(q.r) first) tox))
    =.  txs
      %+  turn  tox
      (cork trip tape-to-ux:ceremony)
    ~&  [(lent txs) 'loaded txs']
    ~&  [%clearing-see ~(wyt in see)]
    =.  see  ~
    =.  outstanding-send  |
    apex
  ==
::
++  read-nonces
  |=  tox=(list cord)
  ^-  [@ud @ud]
  ?:  =(~ tox)  ::  not ?~ because fucking tmi
    [0 0]
  :-  (read-nonce (snag 0 tox))
  (read-nonce (snag (dec (lent tox)) tox))
::
++  read-nonce
  |=  tex=cord
  ^-  @ud
  ::NOTE  this is profoundly stupid but should work well enough
  =+  (find "82" (trip tex))
  ?>  ?=(^ -)
  (rash (rsh 3 (add u 2) (end 3 (add u 6) tex)) hex)
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
  |=  [pax=path tox=(list transaction:rpc:ethereum)]
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
    :~  =>  (need (de-purl:html 'http://35.226.110.143:8545'))
        geth+.(p.p |)
      ::
        =>  (need (de-purl:html 'http://104.198.35.227:8545'))
        parity+.(p.p |)
    ==
  a+(turn req request-to-json:rpc:ethereum)
::
++  send-next-batch
  ^-  [(list move) _this]
  ?:  outstanding-send
    ~&  'waiting for previous send to complete'
    `this
  ?:  =(0 (lent txs))
    ~&  'all sent!'
    [~ this(txs ~, see ~, wen ~, outstanding-send |)]
  ::  ~&  send-next-batch=pretty-see
  =/  new-count  (sub 500 ~(wyt in see))
  ?:  =(0 new-count)
    ~&  %no-new-txs-yet
    `this
  :_  this(txs (slag new-count txs), outstanding-send &)
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
        ~&  [%sent-a-known-transaction--skipping wir]
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
  =.  outstanding-send  |
  ::  ~&  sigh-send-b=pretty-see
  `this
::
++  apex
  ^-  [(list move) _this]
  ~&  :_  ~(wyt in see)
      'waiting for transaction confirms... '
  ?.  =(~ wen)  [~ this]
  =.  wen  `(add now.bol ~s30)
  ::  ~&  apex=[wen pretty-see]
  =^  moves  this  send-next-batch
  ::  timer got un-set, meaning we're done here
  ?~  wen  [moves this]
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
    ::  ~&  see-tx=[(@p (mug txh)) `@ux`txh]
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
  =.  wen  `(add now.bol ~s30)
  [[ost.bol %wait /see (need wen)]~ this]
--
