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
  $%  [%hiss wire ~ mark %hiss hiss:eyre]
      [%info wire ship desk nori:clay]
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
::  usage:
::
::  send all but first 50 txs from path
::    :send-txs [%/txs/txt 50]
::
::  generate txs starting from nonce 0 on fake chain at various
::  gas prices; store at path (one file per gas price)
::
::    :send-txs [% %fake 0]
::
::  generate txs starting from nonce 0 on fake chain at 11 gwei;
::  store at path
::    :send-txs [%/txs/txt %fake 0 11]
::
++  poke-noun
  |=  $:  pax=path
          $=  argv
            $@  skip=@ud
            $:  net=?(%fake %main %ropsten)
                $=  conf
                  $@  nonce=@ud
                  [nonce=@ud gas-price=@ud]
            ==
      ==
  ^-  [(list move) _this]
  ?^  argv
    ?^  conf.argv
      =/  tox
        (sequence:ceremony now.bol [net [gas-price nonce]:conf]:argv)
      [[(write-file pax tox) ~] this]
    =-  [- this]
    %+  turn
      `(list @ud)`~[2 3 5] :: 11 21 31 51 91]
    |=  gas-price=@ud
    %+  write-file  (weld pax /(scot %ud gas-price)/txt)
    (sequence:ceremony now.bol net.argv gas-price nonce.conf.argv)
  ~&  'loading txs...'
  =/  tox=(list cord)  .^((list cord) %cx pax)
  ~&  ?>(?=(^ tox) i.tox)
  =.  tox  (slag +(skip.argv) tox)
  =.  txs
    %+  turn  tox
    (cork trip tape-to-ux:ceremony)
  ~&  [(lent txs) 'loaded txs']
  send-next-batch
::
++  write-file
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
  (json-request:ethereum url jon)
::
++  batch-requests
  |=  [wir=wire fan=? req=(list [(unit @t) request:ethe])]
  ^-  (list move)
  %^    fan-requests
      wir
    ?:  fan
      :~  =>  (need (de-purl:html 'http://localhost:8545'))
          geth+.(p.p |)
        ::
          ::  =>  (need (de-purl:html 'http://localhost:8555'))
          ::  parity+.(p.p |)
      ==
    :_  ~
    =>  (need (de-purl:html 'http://localhost:8545'))
    geth+.(p.p |)
  a+(turn req request-to-json:ethereum)
::
++  send-next-batch
  ^-  [(list move) _this]
  ?:  =(0 (lent txs))
    ~&  'all sent!'
    [~ this(txs ~, see ~, wen `@`0)]
  =/  new-count  (sub 500 ~(wyt in see))
  :_  this(txs (slag new-count txs))
  ~&  ['remaining txs: ' (lent txs)]
  ~&  ['sending txs...' new-count]
  %^  batch-requests  /send  &
  %+  turn  (scag new-count txs)
  |=  tx=@ux
  :-  `(crip 'id-' (scot %ux (end 2 10 tx)) ~)
  [%eth-send-raw-transaction tx]
::
++  sigh-json-rpc-response-send
  |=  [wir=wire res=response:rpc:jstd]
  ^-  [(list move) _this]
  ?.  ?=([%geth *] wir)
    ::  ~&  [%forget-parity wir res]
    `this
  ?>  ?=(%batch -.res)
  =/  saw  ?=(~ see)
  =.  see
    %-  ~(gas in see)
    %+  murn  bas.res
    |=  r=response:rpc:jstd
    ^-  (unit @ux)
    ?:  ?=(%error -.r)
      ?:  =('Known transaction' (end 3 17 message.r))
        ~&  %sent-a-known-transaction--skipping-cap
        ~
      ?:  =('known transaction' (end 3 17 message.r))
        ~&  [%sent-a-known-transaction--skipping wir r]
        ~
      ?:  =('Transaction with the same ' (end 3 26 message.r))
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
  ?.  saw
    `this
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
  =/  s  ~(tap in see)
  =/  extra  (slag 50 s)
  =/  check  (scag 50 s)
  :_  this(see (silt extra))
  %^  batch-requests  /see  |
  %+  turn  check
  |=  txh=@ux
  :-  `(crip 'see-' '0' 'x' ((x-co:co 64) txh))
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
    ?<  ?=(%fail -.r)
    ~|  [id.r res]
    =+  txh=(tape-to-ux:ceremony (trip (rsh 3 4 id.r)))
    ~&  `@ux`txh
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
  =^  moves-a  this  send-next-batch
  =^  moves-b  this  kick-timer
  [(weld moves-a moves-b) this]
::
++  sigh-tang
  |=  [wir=wire err=tang]
  ~&  %sigh-tang
  ~&  (slog err)
  ?:  (gth wen now.bol)  [~ this]
  =.  wen  (add now.bol ~s30)  ::TODO  more sane/polite value
  [[ost.bol %wait /see wen]~ this]
--
