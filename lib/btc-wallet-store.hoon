::
::
/-  *btc-wallet-store
/+  bip32, btc
=,  secp:crypto
=+  ecc=secp256k1
|%
++  defaults
  |%
  ++  max-gap  20
  ++  confs    6
  --
::
++  hash-xpub
  |=  [=xpub:btc =chyg =idx]
  ^-  @ux
  =/  chygidx=@  (cat 3 ?:(=(%0 chyg) '0' '1') idx)
  =/  dat=@  (cat 3 xpub chygidx)
  %-  ripemd-160:ripemd:crypto
  [(met 3 dat) dat]
::
++  from-xpub
  |=  [=xpub:btc scan-to=(unit scon) max-gap=(unit @ud) confs=(unit @ud)]
  ^-  walt
  :*  (from-extended:bip32 (trip xpub))
      (xpub-type:btc xpub)
      *wach
      [0 0]
      %.n
      (fall scan-to *scon)
      (fall max-gap max-gap:defaults)
      (fall confs confs:defaults)
  ==
::  wad: door for processing walts (wallets)
::        parameterized on a walt and it's chyg account
::
++  wad
  |_  [w=walt =chyg]
  ++  mk-address
    |=  =idx
    ^-  address:btc
    =/  pubkey=@ux
      %-  compress-point:ecc
      pub:(derive-public:(derive-public:wilt.w (@ chyg)) idx)
    ?:  ?=(%bip84 bipt.w)
      (need (encode-pubkey:bech32:btc %main pubkey))
    ~|("legacy addresses not supported yet " !!)
  ::  generates and watches the next available address
  ::
  ++  gen-address
    ^-  (pair address:btc walt)
    =/  addr  (mk-address nixt-idx)
    :-  addr
    (update-address addr [chyg nixt-idx *(set utxo:btc)])
  ::  insert a new address; update "nixt" free address if this one was it
  ::
  ++  update-address
    |=  [a=address:btc =addi]
    ^-  walt
    ?>  =(chyg chyg.addi)
    ?>  =(a (mk-address idx.addi))
    =?  nixt.w  (is-nixt addi)
      new:bump-nixt
    w(wach (~(put by wach.w) a addi))
  ::
  ++  is-nixt
    |=  =addi  ^-  ?
    ?:  ?=(%0 chyg.addi)
      =(idx.addi p.nixt.w)
    =(idx.addi q.nixt.w)
  ++  nixt-idx
    ?:(?=(%0 chyg) p.nixt.w q.nixt.w)
  ::  Returns: the prior idx in the account
  ::           nixt with account idx bumped
  ::  Increments idx until an unwatched address is found
  ::  Crashes if max-index is passed
  ::
  ++  bump-nixt
    |^  ^-  [old=idx new=nixt]
    :-  nixt-idx
    =/  new-idx=idx  +(nixt-idx)
    |-  ?>  (lte new-idx max-index)
    ?.  (~(has by wach.w) (mk-address new-idx))
        (set-nixt new-idx)
    $(new-idx +(new-idx))
    ::
    ++  set-nixt
      |=  idx=@  ^-  nixt
      ?:(?=(%0 chyg) [idx q.nixt.w] [p.nixt.w idx])
    --
  --
::  sut: door to select utxos
::
++  sut
|_  [w=walt eny=@uvJ =feyb txos=(list txo)]
  ++  meta-weight  10
  ++  output-weight  31
  ::
  ++  target-value
    ^-  sats
    %+  roll  (turn txos |=(=txo value.txo))
    |=([a=sats b=sats] (add a b))
  ::
  ++  base-weight
    ^-  vbytes
    %+  add  meta-weight
    (mul (lent txos) output-weight)
  ::
  ++  input-weight
    ^-  vbytes
    ?.  ?=(%bip84 bipt.w)
      ~|("Only bech32 wallets supported" !!)
    102
  ::
  ++  total-vbytes
    |=  selected=(list input)
    ^-  vbytes
    %+  add  base-weight
    (mul input-weight (lent selected))
  ::  value of an input after fee
  ::  0 if net is <= 0
  ::
  ++  net-value
    |=  val=sats  ^-  sats
    =/  cost  (mul input-weight feyb)
    ?:  (lte val cost)  0
    (sub val cost)
  ::  Uses naive random selection. Should switch to branch-and-bound later. 
  ::
  ++  inputs-to-txbu
    |=  is=(list input)  ^-  txbu
    :_  txos
    %+  turn  is
    |=(i=input [utxo.i ~ [bipt.w chyg.i idx.i]])
  ::
  ++  select-utxos
    |^  ^-  (unit [=vbytes =txbu])
    =/  uis=(unit (list input))
      %-  single-random-draw
      %-  zing
      (turn ~(val by wach.w) to-inputs)
    ?~  uis  ~
    `[(total-vbytes u.uis) (inputs-to-txbu u.uis)]
    ++  to-inputs
      |=  =addi  ^-  (list input)
      %+  turn  ~(tap in utxos.addi)
      |=(=utxo:btc [utxo chyg.addi idx.addi])
    --
  ::  single-random-draw
  ::    randomly choose utxos until target is hit
  ::    only use an input if its net-value > 0
  ::
  ++  single-random-draw
    |=  is=(list input)
    ^-  (unit (list input))
    =/  rng  ~(. og eny)
    =/  target  (add target-value (mul feyb base-weight))   ::  add base fees to target
    =|  [select=(list input) total=sats:btc]
    |-  ?:  =(~ is)  ~
    =^  n  rng  (rads:rng (lent is))
    =/  i=input  (snag n is)
    =/  net-val  (net-value value.utxo.i)
    =?  select  (gth net-val 0)  [i select]                 ::  select if net-value > 0
    =/  new-total  (add total net-val)
    ?:  (gte new-total target)  `select
    %=  $
        is  (oust [n 1] is)
        total  new-total
    ==
  ::
  --
--
