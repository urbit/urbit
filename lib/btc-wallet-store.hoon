::
::
/-  *btc-wallet-store
/+  bip32, btc, bp=btc-provider
=,  secp:crypto
=+  ecc=secp256k1
|%
++  defaults
  |%
  ++  max-gap  20
  ++  confs    6
  --
::
++  num-confs
  |=  [last-block=@ud =utxo:btc]
  ?:  =(0 height.utxo)  0
  (add 1 (sub last-block height.utxo))
::
++  from-xpub
  |=  [=xpub:btc =fprint:btc scan-to=(unit scon) max-gap=(unit @ud) confs=(unit @ud)]
  ^-  walt
  :*  xpub
      fprint
      (from-extended:bip32 (trip xpub))
      (xpub-type:btc xpub)
      *wach
      [0 0]
      %.n
      (fall scan-to *scon)
      (fall max-gap max-gap:defaults)
      (fall confs confs:defaults)
  ==
::
++  new-txbu
  |=  $:  w=walt
          payee=(unit ship)
          =vbytes:btc
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
    %+  roll
      (turn txos.t |=(=txo value.txo))
    add
  ::
  ++  tx-data
    |^  ^-  data:tx:btc
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
      [(script-pubkey:btc address.txo) value.txo]
    --
  ::
  ++  fee
    =/  [in=sats out=sats]  value
    (sub in out)
  ::
  ++  get-txid
    ^-  txid
    (get-id:txu:btc tx-data)
  ::
  ++  get-rawtx
    (encode:txu:btc tx-data)
  ::
  ++  add-output
    |=  =txo
    ^-  txbu
    t(txos [txo txos.t])
  ::  +to-psbt: returns a based 64 PSBT if
  ::   - all inputs have an associated rawtx
  ::
  ++  to-psbt
    ^-  (unit base64:psbt:btc)
    =/  ins=(list in:psbt:btc)
      %+  murn  txis.t
      |=  =txi
      ?~  ur.txi  ~
      `[utxo.txi u.ur.txi hdkey.txi]
    ?:  (lth (lent ins) (lent txis.t))
      ~
    =/  outs=(list out:psbt:btc)
      %+  turn  txos.t
      |=(=txo [address.txo hk.txo])
    `(encode:pbt:btc get-rawtx get-txid ins outs)
  --
::  wad: door for processing walts (wallets)
::        parameterized on a walt and it's chyg account 
::
++  wad
  |_  [w=walt =chyg]
  ++  pubkey
    |=  =idx:btc
    ^-  bytc:btc
    =/  pk=@ux
      %-  compress-point:ecc
      pub:(derive-public:(derive-public:wilt.w (@ chyg)) idx)
    [(met 3 pk) pk]
  ::
  ++  hdkey
    |=  =idx:btc
    ^-  hdkey:btc
    [fprint.w (~(pubkey wad w chyg) idx) bipt.w chyg idx]
  ::
  ++  mk-address
    |=  =idx:btc
    ^-  address:btc
    ?:  ?=(%84 bipt.w)
      (need (encode-pubkey:bech32:btc %main dat:(pubkey idx)))
    ~|("legacy addresses not supported yet " !!)
  ::  +nixt-address: used to get change addresses
  ::   - gets the current next available address
  ::   - doesn't bump nixt-address if it's unused
  ::   - if used, fall back to gen-address and make a new one
  ::
  ++  nixt-address
    ^-  (trel address:btc idx:btc walt)
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
    ^-  (trel address:btc idx:btc walt)
    =/  addr  (mk-address nixt-idx)
    :*  addr
        nixt-idx
        %+  update-address  addr
          [%.n chyg nixt-idx *(set utxo:btc)]
    ==
  ::  +update-address
  ::   - insert a new address
  ::   - if it's used, move "nixt" to the next free address
  ::   - watch address
  ::
  ++  update-address
    |=  [a=address:btc =addi]
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
    =/  new-idx=idx:btc  +(nixt-idx)
    |-  ?>  (lte new-idx max-index)
    =+  addr=(mk-address new-idx)
    =/  =addi
      %+  ~(gut by wach.w)  addr
      [%.n chyg new-idx *(set utxo:btc)]
    ?.  used.addi
      %=  w
          nixt  (set-nixt new-idx)
          wach  (~(put by wach.w) addr addi)
      ==
    $(new-idx +(new-idx))
    ::
    ++  set-nixt
      |=  =idx:btc  ^-  nixt
      ?:(?=(%0 chyg) [idx q.nixt.w] [p.nixt.w idx])
    --
  --
::  sut: door to select utxos
::
++  sut
|_  [w=walt eny=@uvJ last-block=@ud payee=(unit ship) =feyb txos=(list txo)]
  ++  meta-weight  10
  ++  output-weight  31
  ++  n-txos  (lent txos)
  ::
  ++  target-value
    ^-  sats
    %+  roll  (turn txos |=(=txo value.txo))
    |=([a=sats b=sats] (add a b))
  ::
  ++  base-weight
    |=  num-txos=@ud
    ^-  vbytes
    %+  add  meta-weight
    (mul num-txos output-weight)
  ::
  ++  input-weight
    ^-  vbytes
    ?.  ?=(%84 bipt.w)
      ~|("Only bech32 wallets supported" !!)
    102
  ::
  ++  min-tx-fee
    ^-  sats
    %+  mul  feyb
    (add (base-weight 1) input-weight)
  ::
  ++  total-vbytes
    |=  selected=(list insel)
    ^-  vbytes
    %+  add  (base-weight n-txos)
    (mul input-weight (lent selected))
  ::  value of an input after fee
  ::  0 if net is <= 0
  ::
  ++  net-value
    |=  val=sats  ^-  sats
    =/  cost  (mul input-weight feyb)
    ?:  (lte val cost)  0
    (sub val cost)
  ::
  ::  +spendable: whether utxo has enough confs to spend
  ::
  ++  spendable
    |=  =utxo:btc  ^-  ?
    (gte (num-confs last-block utxo) confs.w)
  ::  +with-change:
  ::    - choose UTXOs, if there are enough
  ::    - return txbu and amount of change (if any)
  ::
  ++  with-change
    ^-  [tb=(unit txbu) chng=(unit sats)]
    =+  tb=select-utxos
    ?~  tb  [~ ~]
    =+  fee=~(fee txb u.tb)
    =/  costs=sats                      ::  cost of this tx + sending another
      %+  add  min-tx-fee
      (mul feyb vbytes.u.tb)
    ?.  (gth fee costs)
      [tb ~]
    :-  tb
    `(sub fee costs)
  ::  Uses naive random selection. Should switch to branch-and-bound later.
  ::
  ++  select-utxos
    |^  ^-  (unit txbu)
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
      |=(=utxo:btc [utxo chyg.addi idx.addi])
    --
  ::  single-random-draw
  ::    randomly choose utxos until target is hit
  ::    only use an insel if its net-value > 0
  ::
  ++  single-random-draw
    |=  is=(list insel)
    ^-  (unit (list insel))
    =/  rng  ~(. og eny)
    =/  target  (add target-value (mul feyb (base-weight n-txos)))   ::  add base fees to target
    =|  [select=(list insel) total=sats:btc]
    |-  ?:  =(~ is)  ~
    =^  n  rng  (rads:rng (lent is))
    =/  i=insel  (snag n is)
    =/  net-val  (net-value value.utxo.i)
    =?  select  ?&((spendable utxo.i) (gth net-val 0))
      [i select]                                           ::  select if net-value > 0
    =/  new-total  (add total net-val)
    ?:  (gte new-total target)  `select
    %=  $
        is  (oust [n 1] is)
        total  new-total
    ==
  ::
  --
--
