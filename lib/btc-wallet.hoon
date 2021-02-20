::
::
/-  *btc-wallet, bc=bitcoin
/+  bip32, bcu=bitcoin-utils, bp=btc-provider
=,  secp:crypto
=+  ecc=secp256k1
|%
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
  =/  [=bipt =network]  (xpub-type:bcu xpub)
  :*  xpub
      network
      fprint
      (from-extended:bip32 (trip xpub))
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
    %+  add  overhead-weight:bcu
    %+  add
      %+  roll
      (turn txis.t |=(t=txi (input-weight:bcu bipt.hdkey.t)))
      add
    %+  roll
      (turn txos.t |=(t=txo (output-weight:bcu (get-bipt:adr:bcu address.t))))
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
      :-  (to-script-pubkey:adr:bcu address.txo)
      value.txo
    --
  ::
  ++  get-txid
    ^-  txid
    (get-id:txu:bcu tx-data)
  ::
  ++  get-rawtx
    (basic-encode:txu:bcu tx-data)
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
      ?~  ur.txi  ~
      `[utxo.txi u.ur.txi hdkey.txi]
    ?:  (lth (lent ins) (lent txis.t))
      ~
    =/  outs=(list out:psbt:bc)
      %+  turn  txos.t
      |=(=txo [address.txo hk.txo])
    `(encode:pbt:bcu %.y get-rawtx get-txid ins outs)
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
      pub:(derive-public:(derive-public:wilt.w (@ chyg)) idx)
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
    (from-pubkey:adr:bcu bipt.w network.w (pubkey idx))
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
    (mul dust-sats (input-weight:bcu output-bipt))
  ::
  ++  target-value
    ^-  sats
    %+  roll  (turn txos |=(=txo value.txo))
    |=([a=sats b=sats] (add a b))
  ::
  ++  base-weight
    ^-  vbytes
    %+  add  overhead-weight:bcu
    %+  roll
      %+  turn  txos
      |=(=txo (output-weight:bcu (get-bipt:adr:bcu address.txo)))
    add
  ::
  ++  total-vbytes
    |=  selected=(list insel)
    ^-  vbytes
    %+  add  base-weight
    (mul (input-weight:bcu bipt.w) (lent selected))
  ::  value of an input after fee
  ::  0 if net is <= 0
  ::
  ++  net-value
    |=  val=sats
    ^-  sats
    =/  cost  (mul (input-weight:bcu bipt.w) feyb)
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
      (mul feyb (add (output-weight:bcu bipt.w) vbytes.u.tb))
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
        (dust-threshold (get-bipt:adr:bcu address.txo))
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
--
