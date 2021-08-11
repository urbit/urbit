::  BIP174: PSBTs
::  https://github.com/bitcoin/bips/blob/master/bip-0174.mediawiki
::
/-  sur=bitcoin
/+  bcu=bitcoin-utils
=,  sur
=,  bcu
|%
++  en
  |%
  ++  globals
    |=  rawtx=hexb
    ^-  map:psbt
    :~  [[1 0x0] rawtx]
    ==
  ::
  ++  input
    |=  [only-witness=? i=in:psbt]
    ^-  map:psbt
    %+  weld
      ?:  only-witness  ~
      ~[[1^0x0 rawtx.i]]
    :~  (witness-tx i)
        (hdkey %input hdkey.i)
    ==
  ::
  ++  output
    |=  =out:psbt
    ^-  map:psbt
    ?~  hk.out  ~
    :~  (hdkey %output u.hk.out)
    ==
  ::
  ++  witness-tx
    |=  i=in:psbt
    ^-  keyval:psbt
    :-  [1 0x1]
    %-  cat:byt
    :~  (flip:byt 8^value.utxo.i)
        1^0x16
        2^0x14
        (hash-160 pubkey.hdkey.i)
    ==
  ::
  ++  hdkey
    |=  [=target:psbt h=^hdkey]
    ^-  keyval:psbt
    =/  typ=@ux
      ?-  target
        %input   0x6
        %output  0x2
      ==
    =/  coin-type=hexb
      ?-  network.h
          %main
        1^0x0
          %testnet
        1^0x1
      ==
    :-  (cat:byt ~[1^typ pubkey.h])
    %-  cat:byt
    :~  fprint.h
        1^`@ux`bipt.h  3^0x80
        coin-type      3^0x80
        4^0x80
        1^`@ux`chyg.h  3^0x0
        (flip:byt 4^idx.h)
    ==
  ::
  ++  keyval-byts
    |=  kv=keyval:psbt
    ^-  hexb
    %-  cat:byt
    :~  1^wid.key.kv
        key.kv
        1^wid.val.kv
        val.kv
    ==
  ::
  ++  map-byts
    |=  m=map:psbt
    ^-  (unit hexb)
    ?~  m  ~
    :-  ~
    %-  cat:byt
    (turn m keyval-byts)
  --
  ++  base64
    |=  b=hexb
    ^-  base64:psbt
    %-  en:base64:mimes:html
    (flip:byt b)
::  +encode: make base64 cord of PSBT
::   - only-witness: don't include non-witness UTXO
::
++  encode
  |=  $:  only-witness=?
          rawtx=hexb
          txid=hexb
          inputs=(list in:psbt)
          outputs=(list out:psbt)
      ==
  ^-  base64:psbt
  =/  sep=(unit hexb)  `1^0x0
  =/  final=(list (unit hexb))
    %+  join  sep
    %+  turn
      %-  zing
      :~  ~[(globals:en rawtx)]
          (turn inputs (cury input:en only-witness))
          (turn outputs output:en)
      ==
    map-byts:en
  %-  base64:en
  ^-  byts
  %-  cat:byt
  %+  weld  ~[[5 0x70.7362.74ff]]
  (murn (snoc final sep) same)
::
++  parse
  |=  psbt-base64=cord
  ^-  (list map:psbt)
  =/  todo=hexb
    (drop:byt 5 (to-byts psbt-base64))
  =|  acc=(list map:psbt)
  =|  m=map:psbt
  |-
  ?:  =(wid.todo 0)
    (snoc acc m)
  ::  0x0: map separator
  ::
  ?:  =(1^0x0 (take:byt 1 todo))
    $(acc (snoc acc m), m *map:psbt, todo (drop:byt 1 todo))
  =^  kv  todo  (next-keyval todo)
  $(m (snoc m kv))
::  +get-txid: extract txid from a valid PSBT
::
++  get-txid
  |=  psbt-base64=cord
  ^-  hexb
  =/  tx=hexb
    %-  raw-tx
    %+  drop:byt  5
    (to-byts psbt-base64)
  %-  flip:byt
  (dsha256 tx)
::  +raw-tx: extract hex transaction
::    looks for key 0x0 in global map
::    crashes if tx not in hex
::
++  raw-tx
  |=  b=hexb
  ^-  hexb
  |-
  ?:  =(wid.b 0)  !!
  ?:  =(1^0x0 (take:byt 1 b))  !!
  =/  nk  (next-keyval b)
  ?:  =(0x0 dat.key.kv.nk)
    val.kv.nk
  $(b rest.nk)
:: +next-keyval: returns next key-val in a PSBT map
::   input first byte must be a map key length
::
++  next-keyval
  |=  b=hexb
  ^-  [kv=keyval:psbt rest=hexb]
  =/  klen  dat:(take:byt 1 b)
  =/  k  (take:byt klen (drop:byt 1 b))
  =/  vlen  dat:(take:byt 1 (drop:byt (add 1 klen) b))
  =/  v  (take:byt vlen (drop:byt (add 2 klen) b))
  ?>  ?&((gth wid.k 0) (gth wid.v 0))
  :-  [k v]
  (drop:byt ;:(add 2 klen vlen) b)
::
++  to-byts
  |=  psbt-base64=cord
  ^-  hexb
  ~|  "Invalid PSBT"
  =+  p=(de:base64:mimes:html psbt-base64)
  ?~  p  !!
  (flip:byt u.p)
--
