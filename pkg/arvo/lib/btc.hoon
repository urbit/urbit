::  lib/btc.hoon
::
/-  *btc-wallet, json-rpc, bp=btc-provider
/+  bip32, bc=bitcoin, bcu=bitcoin-utils
=,  secp:crypto
=+  ecc=secp256k1
|%
::
::  Formerly lib/btc-wallet.hoon
::
::
++  defaults
  |%
  ++  max-gap  20
  ++  confs    6
  --
  ::  +fam: planet parent if s is a moon
::
++  fam
  |=  [our=ship now=@da s=ship]
  ^-  ship
  ?.  =(%earl (clan:title s))  s
  (sein:title our now s)
::
++  num-confs
  |=  [last-block=@ud =utxo:bc]
  ?:  =(0 height.utxo)  0
  (add 1 (sub last-block height.utxo))
::
++  from-xpub
  |=  $:  =xpub:bc
          =fprint:bc
          scan-to=(unit scon)
          max-gap=(unit @ud)
          confs=(unit @ud)
      ==
  ^-  walt
  =/  [=bipt =network]  (xpub-type:bc xpub)
  :*  xpub
      network
      fprint
      +6:(from-extended:bip32 (trip xpub))
      bipt
      *wach
      [0 0]
      %.n
      (fall scan-to *scon)
      (fall max-gap max-gap:defaults)
      (fall confs confs:defaults)
  ==
::  +address-coords: find wallet info for the address, if any
::
++  address-coords
  |=  [a=address ws=(list walt)]
  ^-  (unit [w=walt =chyg =idx])
  |^
  |-  ?~  ws  ~
  =/  res=(unit [=chyg =idx])
    (lookup i.ws)
  ?^  res  `[i.ws chyg.u.res idx.u.res]
  $(ws t.ws)
  ::
  ++  lookup
    |=  w=walt
    ^-  (unit [=chyg =idx])
    =/  ad=(unit addi)  (~(get by wach.w) a)
    ?~(ad ~ `[chyg.u.ad idx.u.ad])
  --
::
++  new-txbu
  |=  $:  w=walt
          payee=(unit ship)
          =vbytes:bc
          is=(list insel)
          txos=(list txo)
      ==
  ^-  txbu
  :*  xpub.w
      payee
      vbytes
      %+  turn  is
        |=  i=insel
        [utxo.i ~ (~(hdkey wad w chyg.i) idx.i)]
      txos
      ~
  ==
::  txb: transaction builder helpers
::
++  txb
  |_  t=txbu
  ++  value
    ^-  [in=sats out=sats]
    :-  %+  roll
          %+  turn  txis.t
          |=(=txi value.utxo.txi)
        add
    (roll (turn txos.t |=(=txo value.txo)) add)
  ::
  ++  fee
    ^-  sats:bc
    =/  [in=sats out=sats]  value
    (sub in out)
  ::
  ++  vbytes
    ^-  vbytes:bc
    %+  add  overhead-weight:bc
    %+  add
      %+  roll
      (turn txis.t |=(t=txi (input-weight:bc bipt.hdkey.t)))
      add
    %+  roll
      (turn txos.t |=(t=txo (output-weight:bc (get-bipt:adr:bc address.t))))
    add
  ++  tx-data
    |^
    ^-  data:tx:bc
    :*  (turn txis.t txi-data)
        (turn txos.t txo-data)
        0  1  `1
    ==
    ::
    ++  txi-data
      |=  =txi
      :*  txid.utxo.txi  pos.utxo.txi
          4^0xffff.ffff  ~  ~  value.utxo.txi
      ==
    ++  txo-data
      |=  =txo
      :-  (to-script-pubkey:adr:bc address.txo)
      value.txo
    --
  ::
  ++  get-txid
    ^-  txid
    (get-id:txu:bc tx-data)
  ::
  ++  get-rawtx
    (basic-encode:txu:bc tx-data)
  ::  +add-output: append output (usually change) to txos
  ::
  ++  add-output
    |=  =txo
    ^-  txbu
    :: todo update vbytes
    t(txos (snoc [txos.t] txo))
  ::  +to-psbt: returns a based 64 PSBT if
  ::   - all inputs have an associated rawtx
  ::
  ++  to-psbt
    ^-  (unit base64:psbt:bc)
    =/  ins=(list in:psbt:bc)
      %+  murn  txis.t
      |=  =txi
      ?~  rawtx.txi  ~
      `[utxo.txi u.rawtx.txi hdkey.txi]
    ?:  (lth (lent ins) (lent txis.t))
      ~
    =/  outs=(list out:psbt:bc)
      %+  turn  txos.t
      |=(=txo [address.txo hk.txo])
    `(encode:pbt:bc %.y get-rawtx get-txid ins outs)
  --
::  wad: door for processing walts (wallets)
::        parameterized on a walt and it's chyg account 
::
++  wad
  |_  [w=walt =chyg]
  ++  pubkey
    |=  =idx:bc
    ^-  hexb:bc
    =/  pk=@ux
      %-  compress-point:ecc
      pub:(derive-public:(~(derive-public bip32 wamp.w) chyg) idx)
    [(met 3 pk) pk]
  ::
  ++  hdkey
    |=  =idx:bc
    ^-  hdkey:bc
    [fprint.w (~(pubkey wad w chyg) idx) network.w bipt.w chyg idx]
  ::
  ++  mk-address
    |=  =idx:bc
    ^-  address:bc
    (from-pubkey:adr:bc bipt.w network.w (pubkey idx))
  ::  +nixt-address: used to get change addresses
  ::   - gets the current next available address
  ::   - doesn't bump nixt-address if it's unused
  ::   - if used, fall back to gen-address and make a new one
  ::
  ++  nixt-address
    ^-  (trel address:bc idx:bc walt)
    =/  addr  (mk-address nixt-idx)
    ~|  "lib/btc-wallet-store: get-next-address: nixt shouldn't be blank"
    =/  =addi  (~(got by wach.w) addr)
    ?.  used.addi
      [addr nixt-idx w]
    gen-address
  ::
  ::  +gen-address:
  ::   - generates the next available address
  ::   - watches it (using update address)
  ::
  ++  gen-address
    ^-  (trel address:bc idx:bc walt)
    =/  addr  (mk-address nixt-idx)
    :*  addr
        nixt-idx
        %+  update-address  addr
          [%.n chyg nixt-idx *(set utxo:bc)]
    ==
  ::  +update-address
  ::   - insert a new address
  ::   - if it's used, move "nixt" to the next free address
  ::   - watch address
  ::
  ++  update-address
    |=  [a=address:bc =addi]
    ^-  walt
    ?>  =(chyg chyg.addi)
    ?>  =(a (mk-address idx.addi))
    =?  w  ?&(used.addi (is-nixt addi))
      bump-nixt
    w(wach (~(put by wach.w) a addi))
  ::
  ++  is-nixt
    |=  =addi  ^-  ?
    ?:  ?=(%0 chyg.addi)
      =(idx.addi p.nixt.w)
    =(idx.addi q.nixt.w)
  ++  nixt-idx
    ?:(?=(%0 chyg) p.nixt.w q.nixt.w)
  ::  +bump-nixt: return wallet with bumped nixt
  ::   - find next unused address
  ::   - watches that address
  ::   - crashes if max-index is passed
  ::
  ++  bump-nixt
    |^  ^-  walt
    =/  new-idx=idx:bc  +(nixt-idx)
    |-  ?>  (lte new-idx max-index)
    =+  addr=(mk-address new-idx)
    =/  =addi
      %+  ~(gut by wach.w)  addr
      [%.n chyg new-idx *(set utxo:bc)]
    ?.  used.addi
      %=  w
          nixt  (set-nixt new-idx)
          wach  (~(put by wach.w) addr addi)
      ==
    $(new-idx +(new-idx))
    ::
    ++  set-nixt
      |=  =idx:bc  ^-  nixt
      ?:(?=(%0 chyg) [idx q.nixt.w] [p.nixt.w idx])
    --
  --
::  sut: select utxos
::
++  sut
|_  [w=walt eny=@uvJ last-block=@ud payee=(unit ship) =feyb txos=(list txo)]
  ++  dust-sats  3
  ++  dust-threshold
    |=  output-bipt=bipt:bc
    ^-  vbytes
    (mul dust-sats (input-weight:bc output-bipt))
  ::
  ++  target-value
    ^-  sats
    %+  roll  (turn txos |=(=txo value.txo))
    |=([a=sats b=sats] (add a b))
  ::
  ++  base-weight
    ^-  vbytes
    %+  add  overhead-weight:bc
    %+  roll
      %+  turn  txos
      |=(=txo (output-weight:bc (get-bipt:adr:bc address.txo)))
    add
  ::
  ++  total-vbytes
    |=  selected=(list insel)
    ^-  vbytes
    %+  add  base-weight
    (mul (input-weight:bc bipt.w) (lent selected))
  ::  value of an input after fee
  ::  0 if net is <= 0
  ::
  ++  net-value
    |=  val=sats
    ^-  sats
    =/  cost  (mul (input-weight:bc bipt.w) feyb)
    ?:  (lte val cost)  0
    (sub val cost)
  ::
  ::  +spendable: whether utxo has enough confs to spend
  ::
  ++  spendable
    |=  =utxo:bc  ^-  ?
    (gte (num-confs last-block utxo) confs.w)
  ::  +with-change:
  ::    - choose UTXOs, if there are enough
  ::    - return txbu and amount of change (if any)
  ::
  ++  with-change
    ^-  [tb=(unit txbu) chng=(unit sats)]
    =/  tb=(unit txbu)  select-utxos
    ?~  tb  [~ ~]
    =+  excess=~(fee txb u.tb)        ::  (inputs - outputs)
    =/  new-fee=sats                   ::  cost of this tx + one more output
      (mul feyb (add (output-weight:bc bipt.w) vbytes.u.tb))
    ?.  (gth excess new-fee)
      [tb ~]
    ?.  (gth (sub excess new-fee) (dust-threshold bipt.w))
      [tb ~]
    :-  tb
    `(sub excess new-fee)
  ::  Uses naive random selection. Should switch to branch-and-bound later.
  ::
  ++  select-utxos
    |^  ^-  (unit txbu)
    ?.  %+  levy  txos
        |=  =txo
        %+  gth  value.txo
        (dust-threshold (get-bipt:adr:bc address.txo))
      ~|("One or more suggested outputs is dust." !!)
    =/  is=(unit (list insel))
      %-  single-random-draw
      %-  zing
      (turn ~(val by wach.w) to-insels)
    ?~  is  ~
    `(new-txbu w payee (total-vbytes u.is) u.is txos)
    ::
    ++  to-insels
      |=  =addi
      ^-  (list insel)
      %+  turn  ~(tap in utxos.addi)
      |=(=utxo:bc [utxo chyg.addi idx.addi])
    --
  ::  single-random-draw
  ::    randomly choose utxos until target is hit
  ::    only use an insel if its net-value > 0
  ::
  ++  single-random-draw
    |=  is=(list insel)
    ^-  (unit (list insel))
    =/  rng  ~(. og eny)
    =/  target  (add target-value (mul feyb base-weight))   ::  add base fees to target
    =|  [select=(list insel) total=sats:bc]
    |-
    ?:  =(~ is)  ~
    =^  n  rng  (rads:rng (lent is))
    =/  i=insel  (snag n is)
    ?.  (spendable utxo.i)
      $(is (oust [n 1] is))
    =/  net-val  (net-value value.utxo.i)
    =?  select  (gth net-val 0)
      [i select]
    =/  new-total  (add total net-val)
    ?:  (gte new-total target)  `select
    %=  $
        is  (oust [n 1] is)
        total  new-total
    ==
  ::
  --
::
::
::  Formerly lib/btc-provider
::
::
++  from-epoch
  |=  secs=@ud
  ^-  (unit @da)
  ?:  =(0 secs)  ~
  [~ (add ~1970.1.1 `@dr`(mul secs ~s1))]
::
++  get-request
  |=  url=@t
  ^-  request:http
  [%'GET' url ~ ~]
::
++  post-request
  |=  [url=@t body=json]
  ^-  request:http
  :*  %'POST'
      url
      ~[['Content-Type' 'application/json']]
      =,  html
      %-  some
        %-  as-octt:mimes
        (en-json body)
  ==
::
++  gen-request
 |=  [=host-info:bp ract=action:rpc-types:bp]
  ^-  request:http
  %+  rpc-action-to-http
  api-url.host-info  ract
::
++  rpc
  =,  dejs:format
  |%
  ++  parse-result
    |=  res=response:json-rpc
    |^  ^-  result:rpc-types:bp
    ~|  -.res
    ?>  ?=(%result -.res)
    ?+  id.res  ~|([%unsupported-result id.res] !!)
        %get-address-info
      [id.res (address-info res.res)]
      ::
        %get-tx-vals
      [id.res (tx-vals res.res)]
      ::
        %get-raw-tx
      [id.res (raw-tx res.res)]
      ::
        %broadcast-tx
      [%broadcast-tx (broadcast-tx res.res)]
      ::
        %get-block-count
      [id.res (ni res.res)]
      ::
        %get-block-info
      [id.res (block-info res.res)]
    ==
    ::
    ++  address-info
      %-  ot
      :~  [%address (cu from-cord:adr:bc so)]
          [%utxos (as utxo)]
          [%used bo]
          [%block ni]
      ==
    ++  utxo
    %-  ot
      :~  ['tx_pos' ni]
          ['tx_hash' (cu from-cord:hxb:bcu so)]
          [%height ni]
          [%value ni]
          [%recvd (cu from-epoch ni)]
      ==
    ++  tx-vals
      %-  ot
      :~  [%included bo]
          [%txid (cu from-cord:hxb:bcu so)]
          [%confs ni]
          [%recvd (cu from-epoch ni)]
          [%inputs (ar tx-val)]
          [%outputs (ar tx-val)]
      ==
    ++  tx-val
      %-  ot
      :~  [%txid (cu from-cord:hxb:bcu so)]
          [%pos ni]
          [%address (cu from-cord:adr:bc so)]
          [%value ni]
      ==
    ++  raw-tx
      %-  ot
      :~  [%txid (cu from-cord:hxb:bcu so)]
          [%rawtx (cu from-cord:hxb:bcu so)]
      ==
    ++  broadcast-tx
      %-  ot
      :~  [%txid (cu from-cord:hxb:bcu so)]
          [%broadcast bo]
          [%included bo]
      ==
    ++  block-info
      %-  ot
      :~  [%block ni]
          [%fee (mu ni)]
          [%blockhash (cu from-cord:hxb:bcu so)]
          [%blockfilter (cu from-cord:hxb:bcu so)]
      ==
    --
  --
::
++  rpc-action-to-http
  |=  [endpoint=@t ract=action:rpc-types:bp]
  |^  ^-  request:http
  ?-  -.ract
      %get-address-info
    %-  get-request
    %+  mk-url  '/addresses/info/'
    (to-cord:adr:bc address.ract)
    ::
      %get-tx-vals
    %-  get-request
    %+  mk-url  '/gettxvals/'
    (to-cord:hxb:bcu txid.ract)
    ::
      %get-raw-tx
    %-  get-request
    %+  mk-url  '/getrawtx/'
    (to-cord:hxb:bcu txid.ract)
    ::
      %broadcast-tx
    %-  get-request
    %+  mk-url  '/broadcasttx/'
    (to-cord:hxb:bcu rawtx.ract)
    ::
      %get-block-count
    %-  get-request
    (mk-url '/getblockcount' '')
    ::
      %get-block-info
    =/  param=@t
      ?~(block.ract '' (rsh [3 2] (scot %ui u.block.ract)))
    %-  get-request
    (mk-url '/getblockinfo/' param)
  ==
  ++  mk-url
    |=  [base=@t params=@t]
    %^  cat  3
    (cat 3 endpoint base)  params
  --
::  RPC/HTTP Utilities
::
++  httr-to-rpc-response
  |=  hit=httr:eyre
  ^-  response:json-rpc
  ~|  hit
  =/  jon=json  (need (de-json:html q:(need r.hit)))
  ?.  =(%2 (div p.hit 100))
    (parse-rpc-error jon)
  =,  dejs-soft:format
  ^-  response:json-rpc
  =;  dere
    =+  res=((ar dere) jon)
    ?~  res  (need (dere jon))
    [%batch u.res]
  |=  jon=json
  ^-  (unit response:json-rpc)
  =/  res=[id=(unit @t) res=(unit json) err=(unit json)]
    %.  jon
    =,  dejs:format
    =-  (ou -)
    :~  ['id' (uf ~ (mu so))]
        ['result' (uf ~ (mu same))]
        ['error' (uf ~ (mu same))]
    ==
  ?:  ?=([^ * ~] res)
    `[%result [u.id.res ?~(res.res ~ u.res.res)]]
  ~|  jon
  `(parse-rpc-error jon)
::
++  get-rpc-response
  |=  response=client-response:iris
  ^-  response:json-rpc
  ?>  ?=(%finished -.response)
  %-  httr-to-rpc-response
    %+  to-httr:iris
      response-header.response
    full-file.response
::
++  parse-rpc-error
  |=  =json
  ^-  response:json-rpc
  :-  %error
  ?~  json  ['' '' '']
  %.  json
  =,  dejs:format
  =-  (ou -)
  :~  =-  ['id' (uf '' (cu - (mu so)))]
      |*(a=(unit) ?~(a '' u.a))
      :-  'error'
      =-  (uf ['' ''] -)
      =-  (cu |*(a=(unit) ?~(a ['' ''] u.a)) (mu (ou -)))
      :~  ['code' (uf '' no)]
          ['message' (uf '' so)]
  ==  ==
--
