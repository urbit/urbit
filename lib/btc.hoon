::  lib/btc.hoon
::  Utilities for working with BTC data types and transactions
::
/-  sur=btc
^?
=<  [sur .]
=,  sur
|%
++  to-hexb
  |=  h=@t
  ^-  hexb
  ?:  =('' h)  1^0x0
  ::  Add leading 00
  ::
  =+  (lsh [3 2] h)
  ::  Group by 4-size block
  ::
  =+  (rsh [3 2] -)
  ::  Parse hex to atom
  ::
  :-  (div (lent (trip h)) 2)
  `@ux`(rash - hex)
::
++  overhead-weight  ^-(vbytes 11)
++  input-weight
  |=  =bipt
  ^-  vbytes
  ?-  bipt
    %44  148
    %49  91
    %84  68
  ==
++  output-weight
  |=  =bipt
  ^-  vbytes
  ?-  bipt
    %44  34
    %49  32
    %84  31
  ==
::
++  xpub-type
  |=  =xpub
  ^-  [=bipt =network]
  =/  prefix=tape  (scag 4 (trip xpub))
  ?:  =("tpub" prefix)  [%44 %testnet]
  ?:  =("upub" prefix)  [%49 %testnet]
  ?:  =("vpub" prefix)  [%84 %testnet]
  ?:  =("xpub" prefix)  [%44 %main]
  ?:  =("ypub" prefix)  [%49 %main]
  ?:  =("zpub" prefix)  [%84 %main]
  ~|("invalid xpub: {<xpub>}" !!)
::
++  address-bipt
  |=  a=address
  ^-  bipt
  =/  spk=hexb  (script-pubkey a)
  ?:  =(25 wid.spk)  %44
  ?:  =(23 wid.spk)  %49
  ?:  =(22 wid.spk)  %84
  ?:  =(34 wid.spk)  %84
  ~|("Invalid address" !!)
::  big endian sha256: input and output are both MSB first (big endian)
::
++  sha256
  |=  =byts
  ^-  hexb
  %-  flip:byt
  [32 (shay (flip:byt byts))]
::
++  dsha256
  |=  =byts
  (sha256 (sha256 byts))
::
++  hash-160
  |=  val=byts
  ^-  hexb
  =,  ripemd:crypto
  :-  20
  %-  ripemd-160
  (sha256 val)
::  +csiz: handle compact-size integers
::   - encode: big endian to little endian
::   - decode: little endian to big endian
::
++  csiz
  |%
  ++  en
    |=  a=@
    ^-  hexb
    =/  l=@  (met 3 a)
    ?:  =(l 1)  1^a
    ?:  =(l 2)  (cat:byt ~[1^0xfd (flip:byt 2^a)])
    ?:  (lte l 4)  (cat:byt ~[1^0xfe (flip:byt 4^a)])
    ?:  (lte l 8)  (cat:byt ~[1^0xff (flip:byt 8^a)])
    ~|("Cannot encode CompactSize longer than 8 bytes" !!)
  ::
  ++  de
    |=  h=hexb
    ^-  [n=hexb rest=hexb]
    =/  s=@ux  dat:(take:byt 1 h)
    ?:  (lth s 0xfd)  [1^s (drop:byt 1 h)]
    ~|  "Invalid compact-size at start of {<h>}"
    =/  len=bloq
    ?+  s  !!
      %0xfd  1
      %0xfe  2
      %0xff  3
    ==
    :_  (drop:byt (add 1 len) h)
    %-  flip:byt
    (take:byt (bex len) (drop:byt 1 h))
  ::  +dea: return atom instead of hexb for parsed CompactSize
  ::
  ++  dea
    |=  h=hexb
    ^-  [a=@ rest=hexb]
    =>  (de h)
    [dat.n rest]
  --
::
++  pubkey-to-address
  |=  [=bipt =network pubkey=hexb]
  ^-  address
  ?-  bipt
      %44
    :-  %base58
    =<  ^-(@uc dat)
    %-  cat:byt
    :-  ?-  network
            %main     1^0x0
            %testnet  1^0x6f
        ==
    ~[(hash-160 pubkey)]
    ::
      %49
    :-  %base58
    =<  ^-(@uc dat)
    %-  cat:byt
    :~  ?-  network
          %main     1^0x5
          %testnet  1^0xc4
        ==
        %-  hash-160
        (cat:byt ~[2^0x14 (hash-160 pubkey)])
    ==
    ::
      %84
    :-  %bech32
    (need (encode-pubkey:bech32 network pubkey))
  ==
::
++  script-pubkey
  |=  =address
  ^-  hexb
  ?-  -.address
      %bech32
    =+  h=(from-address:bech32 +.address)
    %-  cat:byt
    :~  1^0x0
        1^wid.h
        h
    ==
    ::
      %base58
    =/  h=hexb  [21 `@ux`+.address]
    =+  lead-byt=dat:(take:byt 1 h)
    =/  version-network=[bipt network]
      ?:  =(0x0 lead-byt)   [%44 %main]
      ?:  =(0x6f lead-byt)  [%44 %testnet]
      ?:  =(0x5 lead-byt)   [%49 %main]
      ?:  =(0xc4 lead-byt)  [%49 %testnet]
      ~|("Invalid base58 address: {<+.address>}" !!)
    %-  cat:byt
    ?:  ?=(%44 -.version-network)
      :~  3^0x76.a914
          (drop:byt 1 h)
          2^0x88ac
      ==
    :~  2^0xa914
        (drop:byt 1 h)
        1^0x87
    ==
  ==
::  +txu: transaction utility core
::   - primarily used for calculating txids
::   - ignores signatures in inputs
::
++  txu
  |%
  ++  en
    |%
    ++  input
      |=  i=input:tx
      ^-  hexb
      %-  cat:byt
      :~  (flip:byt txid.i)
          (flip:byt 4^pos.i)
          ?~  script-sig.i  1^0x0
        %-  cat:byt
        ~[(en:csiz wid.u.script-sig.i) u.script-sig.i]
          (flip:byt sequence.i)
      ==
    ::
    ++  output
      |=  o=output:tx
      ^-  hexb
      %-  cat:byt
      :~  (flip:byt 8^value.o)
          1^wid.script-pubkey.o
          script-pubkey.o
      ==
    --
  ::
  ++  de
    |%
    ++  nversion
      |=  b=hexb
      ^-  [nversion=@ud rest=hexb]
      :-  dat:(flip:byt (take:byt 4 b))
      (drop:byt 4 b)
    ::
    ++  segwit
      |=  b=hexb
      ^-  [segwit=(unit @ud) rest=hexb]
      ?.  =(1^0x0 (take:byt 1 b))
        [~ b]
      :-  [~ dat:(take:byt 2 b)]
      (drop:byt 2 b)
    ::
    ++  script-sig
      |=  b=hexb
      ^-  [sig=hexb rest=hexb]
      =^  siglen=hexb  b  (de:csiz b)
      :-  (take:byt dat.siglen b)
      (drop:byt dat.siglen b)
    ::
    ++  sequence
      |=  b=hexb
      ^-  [seq=hexb rest=hexb]
      [(flip:byt (take:byt 4 b)) (drop:byt 4 b)]
    ::
    ++  inputs
      |=  b=hexb
      ^-  [is=(list input:tx) rest=hexb]
      |^
      =|  acc=(list input:tx)
      =^  count  b  (dea:csiz b)
      |-
      ?:  =(0 count)  [acc b]
      =^  i  b  (input b)
      $(acc (snoc acc i), count (dec count)) 
      ::
      ++  input
        |=  b=hexb
        ^-  [i=input:tx rest=hexb]
        =/  txid  (flip:byt (take:byt 32 b))
        =/  pos   dat:(flip:byt (take:byt 4 (drop:byt 32 b)))
        =^  sig=hexb  b  (script-sig (drop:byt 36 b))
        =^  seq=hexb  b  (sequence b)
        :_  b
        [txid pos seq ?:((gth wid.sig 0) `sig ~) ~ 0]
      --
    ::
    ++  outputs
      |=  b=hexb
      ^-  [os=(list output:tx) rest=hexb]
      =|  acc=(list output:tx)
      =^  count  b  (dea:csiz b)
      |-
      ?:  =(0 count)  [acc b]
      =/  value  (flip:byt (take:byt 8 b))
      =^  scriptlen  b  (dea:csiz (drop:byt 8 b))
      %=  $
          acc  %+  snoc  acc
               :-  (take:byt scriptlen b)
               dat.value
          b  (drop:byt scriptlen b)
          count  (dec count)
      ==
    --
  ::  +basic-encode: encodes data in a format suitable for hashing
  ::
  ++  basic-encode
    |=  =data:tx
    ^-  hexb
    %-  cat:byt
    %-  zing
    :~  ~[(flip:byt 4^nversion.data)]
        ~[(en:csiz (lent is.data))]
        (turn is.data input:en)
        ~[(en:csiz (lent os.data))]
        (turn os.data output:en)
        ~[(flip:byt 4^locktime.data)]
    ==
  ++  get-id
    |=  =data:tx
    ^-  hexb
    %-  flip:byt
    %-  dsha256
    (basic-encode data)
  ::
  ++  decode
    |=  b=hexb
    ^-  data:tx
    =^  nversion  b
      (nversion:de b)
    =^  segwit  b
      (segwit:de b)
    =^  inputs  b
      (inputs:de b)
    =^  outputs  b
      (outputs:de b)
    =/  locktime=@ud
      dat:(take:byt 4 (flip:byt b))
    [inputs outputs locktime nversion segwit]
  --
::  core to handle BIP174 PSBTs
::
++  pbt
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
::
++  byt
  |%
  ::  +cat: concat byts, preserving MSB order
  ::  (cat:byt ~[4^0xaa00 4^0xbb00]) => [8 0xaa00.00bb.0000]
  ::
  ++  cat
    |=  bs=(list byts)
    ^-  byts
    :-  (roll (turn bs |=(b=byts -.b)) add)
    (can 3 (flop bs))
  --
  ::  +flip:byt: flip endianness while preserving lead/trail zeroes
  ::
  ++  flip
    |=  b=byts
    ^-  byts
    [wid.b (rev 3 b)]
  ::  +take: take n bytes from front of byts
  ::  pads front with extra zeroes if n is longer than byts
  ::
  ++  take
    |=  [n=@ b=byts]
    ^-  byts
    ?:  (gth n wid.b)
      [n dat.b]
    [n (rsh [3 (sub wid.b n)] dat.b)]
  ::  +drop: drop n bytes from the front of byts
  ::  returns 0^0x0 if n >= wid.byts
  ::
  ++  drop
    |=  [n=@ b=byts]
    ^-  byts
    ?:  (gte n wid.b)
      0^0x0
    =+  n-take=(sub wid.b n)
    [n-take (end [3 n-take] dat.b)]
::
++  bit
  |%
  ++  cat
    |=  bs=(list bits)
    ^-  bits
    :-  (roll (turn bs |=(b=bits wid.b)) add)
    (can 0 (flop bs))
  ::
  ++  take
    |=  [n=@ b=bits]
    ^-  bits
    ?:  (gth n wid.b)
    [n dat.b]
    [n (rsh [0 (sub wid.b n)] dat.b)]
  ::
  ++  drop
    |=  [n=@ b=byts]
    ^-  bits
    ?:  (gte n wid.b)
      0^0b0
    =+  n-take=(sub wid.b n)
    [n-take (end [0 n-take] dat.b)]
  ::  +from-atoms: convert atoms of bitwidth to bits
  ::
  ++  from-atoms
    |=  [bitwidth=@ digits=(list @)]
    ^-  bits
    %-  cat:bit
    %+  turn  digits
    |=  a=@
    ?>  (lte (met 0 a) bitwidth)
    [bitwidth `@ub`a]
  ::  +to-atoms: convert bits to atoms of bitwidth
  ::
   ++  to-atoms
    |=  [bitwidth=@ bs=bits]
    ^-  (list @)
    =|  res=(list @)
    ?>  =(0 (mod wid.bs bitwidth))
    |-
    ?:  =(0 wid.bs)  res
    %=  $
        res  (snoc res dat:(take bitwidth bs))
        bs   (drop bitwidth bs)
    ==
  --
::
++  bech32
  |%
  ++  prefixes
    ^-  (map network tape)
    (my [[%main "bc"] [%testnet "tb"] ~])
  ++  charset  "qpzry9x8gf2tvdw0s3jn54khce6mua7l"
  +$  raw-decoded  [hrp=tape data=(list @) checksum=(list @)]
  ::  below is a port of: https://github.com/bitcoinjs/bech32/blob/master/index.js
  ::
  ++  polymod
    |=  values=(list @)
    |^  ^-  @
    =/  gen=(list @ux)
      ~[0x3b6a.57b2 0x2650.8e6d 0x1ea1.19fa 0x3d42.33dd 0x2a14.62b3]
    =/  chk=@  1
    |-  ?~  values  chk
    =/  top  (rsh [0 25] chk)
    =.  chk
      (mix i.values (lsh [0 5] (dis chk 0x1ff.ffff)))
    $(values t.values, chk (update-chk chk top gen))
  ::
    ++  update-chk
      |=  [chk=@ top=@ gen=(list @ux)]
      =/  is  (gulf 0 4)
      |-  ?~  is  chk
      ?:  =(1 (dis 1 (rsh [0 i.is] top)))
        $(is t.is, chk (mix chk (snag i.is gen)))
      $(is t.is)
    --
  ::
  ++  expand-hrp
    |=  hrp=tape
    ^-  (list @)
    =/  front  (turn hrp |=(p=@tD (rsh [0 5] p)))
    =/  back   (turn hrp |=(p=@tD (dis 31 p)))
    (zing ~[front ~[0] back])
  ::
  ++  verify-checksum
    |=  [hrp=tape data-and-checksum=(list @)]
    ^-  ?
    %-  |=(a=@ =(1 a))
    %-  polymod
    (weld (expand-hrp hrp) data-and-checksum)
  ::
  ++  checksum
    |=  [hrp=tape data=(list @)]
    ^-  (list @)
    ::  xor 1 with the polymod
    ::
    =/  pmod=@
      %+  mix  1
      %-  polymod
      (zing ~[(expand-hrp hrp) data (reap 6 0)])
    %+  turn  (gulf 0 5)
    |=(i=@ (dis 31 (rsh [0 (mul 5 (sub 5 i))] pmod)))
  ::
  ++  charset-to-value
    |=  c=@tD
    ^-  (unit @)
    (find ~[c] charset)
  ++  value-to-charset
    |=  value=@
    ^-  (unit @tD)
    ?:  (gth value 31)  ~
    `(snag value charset)
  ::
  ++  is-valid
    |=  [bech=tape last-1-pos=@]  ^-  ?
    ?&  ?|(=((cass bech) bech) =((cuss bech) bech))       ::  to upper or to lower is same as bech
        (gte last-1-pos 1)
        (lte (add last-1-pos 7) (lent bech))
        (lte (lent bech) 90)
        (levy bech |=(c=@tD (gte c 33)))
        (levy bech |=(c=@tD (lte c 126)))
    ==
  ::  data should be 5bit words
  ::
  ++  encode-raw
    |=  [hrp=tape data=(list @)]
    ^-  cord
    =/  combined=(list @)
      (weld data (checksum hrp data))
    %-  crip
    (zing ~[hrp "1" (tape (murn combined value-to-charset))])
  ++  decode-raw
    |=  body=cord
    ^-  (unit raw-decoded)
    =/  bech  (cass (trip body))              ::  to lowercase
    =/  pos  (flop (fand "1" bech))
    ?~  pos  ~
    =/  last-1=@  i.pos
    ?.  (is-valid bech last-1)         ::  check bech32 validity (not segwit validity or checksum)
      ~
    =/  hrp  (scag last-1 bech)
    =/  encoded-data-and-checksum=(list @)
      (slag +(last-1) bech)
    =/  data-and-checksum=(list @)
      %+  murn  encoded-data-and-checksum
      charset-to-value
    ?.  =((lent encoded-data-and-checksum) (lent data-and-checksum))    ::  ensure all were in CHARSET
      ~
    ?.  (verify-checksum hrp data-and-checksum)
      ~
    =/  checksum-pos  (sub (lent data-and-checksum) 6)
    `[hrp (scag checksum-pos data-and-checksum) (slag checksum-pos data-and-checksum)]
  ::  +from-address: BIP173 bech32 address encoding to hex
  ::  https://github.com/bitcoin/bips/blob/master/bip-0173.mediawiki
  ::  expects to drop a leading 5-bit 0 (the witness version)
  ::
  ++  from-address
    |=  body=cord
    ^-  hexb
    ~|  "Invalid bech32 address"
    =/  d=(unit raw-decoded)  (decode-raw body)
    ?>  ?=(^ d)
    =/  bs=bits  (from-atoms:bit 5 data.u.d)
    =/  byt-len=@  (div (sub wid.bs 5) 8)
    ?>  =(5^0b0 (take:bit 5 bs))
    ?>  ?|  =(20 byt-len)
            =(32 byt-len)
        ==
    [byt-len `@ux`dat:(take:bit (mul 8 byt-len) (drop:bit 5 bs))]
  ::  pubkey is the 33 byte ECC compressed public key
  ::
  ++  encode-pubkey
    |=  [=network pubkey=byts]
    ^-  (unit cord)
    ?.  =(33 wid.pubkey)
      ~|('pubkey must be a 33 byte ECC compressed public key' !!)
    =/  prefix  (~(get by prefixes) network)
    ?~  prefix  ~
    :-  ~
    %+  encode-raw  u.prefix
    [0v0 (to-atoms:bit 5 [160 `@ub`dat:(hash-160 pubkey)])]
  --
::
--
