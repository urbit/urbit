/-  sur=btc
^?
=<  [sur .]
=,  sur
|%
++  xpub-type
  |=  =xpub
  ^-  bipt
  =/  prefix=tape  (scag 4 (trip xpub))
  ?:  =("xpub" prefix)  %44
  ?:  =("ypub" prefix)  %49
  ?:  =("zpub" prefix)  %84
  ~|("invalid xpub: {<xpub>}" !!)
::  big endian sha256: input and output are both MSB first (big endian)
::
++  sha256
  |=  =byts
  ^-  hash256
  %-  hash256
  %-  flip:byt
  [32 (shay (flip:byt byts))]
::
++  dsha256
  |=  =byts
  (sha256 (sha256 byts))
::
++  hash-160
  |=  pubkey=@ux
  ^-  hash160
  =,  ripemd:crypto
  %-  hash160
  :-  20
  %-  ripemd-160
  %-  sha256  [(met 3 pubkey) pubkey]
::
++  to-script-pubkey
  |=  script=buffer  ^-  buffer
  %-  zing
  :~  ~[0x19 0x76 0xa9 0x14]
      script
      ~[0x88 0xac]
  ==
++  address-to-script-pubkey
  |=  =address  ^-  buffer
  =/  hex=byts
    ?-  -.address
        %bech32
      (to-hex:bech32 address)
        %legacy
      =/  h=@ux  (@ux +.address)
      [(met 3 h) h]
    ==
  ?.  =(wid.hex 20)
    ~|("Only 20-byte addresses supported" !!)
  (to-script-pubkey (from-byts:buf hex))
::  arms to handle BIP174 PSBTs
::
++  pbt
  |%
  ++  en
    |%
    ++  globals
      |=  =rawtx
      ^-  map:psbt
      *map:psbt
    ++  inputs
      |=  (list in:psbt)
      ^-  map:psbt
      *map:psbt
    ++  outputs
      |=  (list out:psbt)
      ^-  map:psbt
      *map:psbt
    --
    ++  hdkey
      |=  [pubkey=bytc =target:psbt h=^hdkey]
      ^-  keyval:psbt
      =/  typ=@ux
        ?-  target
          %input   0x6
          %output  0x2
        ==
      :-  (cat:byt ~[1^typ pubkey])
      %-  cat:byt
      :~  fprint.h
          %-  to-byts:buf
            :~   `@ux`bipt.h  0x0  0x0  0x80
                 0x0  0x0  0x0  0x80
                 0x0  0x0  0x0  0x80
                 `@ux`chyg.h  0x0  0x0  0x0
            ==
          (flip:byt [(met 3 idx.h) idx.h])
      ==
  ::  +encode: make base64 cord of PSBT
  ::
  ++  encode
  |=  [=rawtx =txid inputs=(list in:psbt) outputs=(list out:psbt)]
  ^-  cord
  :: TODO
  ::  make global map
  ::  raw tx hex
  :: turn each input and output into a map (or ~)
  ::  put the 0x0 separator between all
  ::  parse to hex
  ::  encode as base64!
  *cord
  ::  +create: make base64 cord of PSBT
  ::
  ++  create
    |=  [=rawtx =txid inputs=(list in:psbt) outputs=(list out:psbt)]
    ^-  cord
    :: TODO
    ::  make global map
    :: turn each input and output into a map (or ~)
    ::  put the 0x0 separator between all
    ::  parse to hex
    ::  encode as base64!
    *cord
  ::
  ++  parse
    |=  psbt-base64=cord
    ^-  (list map:psbt)
    =/  todo=buffer
      %+  slag  5  (to-buffer psbt-base64)
    =|  acc=(list map:psbt)
    =|  m=map:psbt
    |-
    ?~  todo  (snoc acc m)
    ::  0x0: map separator
    ?:  =(0x0 i.todo)
      $(acc (snoc acc m), m *map:psbt, todo t.todo)
    =+  [kv rest]=(next-keyval todo)
    $(m (snoc m kv), todo rest)
  ::  +get-txid: extract txid from a valid PSBT
  ::
  ++  get-txid
    |=  psbt-base64=cord
    ^-  txid
    =/  tx=bytc
      %-  raw-tx
      %+  slag  5
      (to-buffer psbt-base64)
    %-  txid
    %-  flip:byt
    %-  sha256
    (sha256 tx)
  ::  +raw-tx: extract hex transaction
  ::    looks for key 0x0 in global map
  ::    crashes if tx not in buffer
  ::
  ++  raw-tx
    |=  b=buffer
    |-  ^-  bytc
    ?~  b  !!
    ?:  =(0x0 i.b)  !!
    =+  nk=(next-keyval b)
    ?:  =(0x0 dat.key.kv.nk)
      val.kv.nk
    $(b rest.nk)
  :: +next-keyval: returns next key-val in a PSBT map
  ::   input buffer head must be a map key length
  ::
  ++  next-keyval
    |=  b=buffer
    ^-  [kv=keyval:psbt rest=buffer]
    =+  klen=(snag 0 b)
    =+  k=(swag [1 klen] b)
    =+  vlen=(snag (add 1 klen) b)
    =+  v=(swag [(add 2 klen) vlen] b)
    =+  len=(add 2 (add klen vlen))
    ?>  ?=([^ ^] [k v])
    :_  (slag len b)
    :-  (to-byts:buf k)
        (to-byts:buf v)
  ::
  ++  to-buffer
    |=  psbt-base64=cord
    ^-  buffer
    ~|  "Invalid PSBT"
    =+  p=(de:base64:mimes:html psbt-base64)
    ?~  p  !!
    =/  bigend=@ux  (swp 3 q.u.p)
    (from-byts:buf [(met 3 bigend) bigend])
  --
::  buffer: byte buffer utilities
::  list of @ux that is big endian for hashing purposes
::  used to preserve 0s when concatenating byte sequences
::
++  buf
  |%
  ++  from-byts
    |=  =byts  ^-  buffer
    =/  b=(list @ux)
      (flop (rip 3 dat.byts))
    =/  pad=@  (sub wid.byts (lent b))
    (weld (reap pad 0x0) b)
    ::  converts byts to a little endian buffer with wid length (trailing 0s)
  ::  atom 1 with wid=4 becomes ~[0x1 0x0 0x0 0x0]
  ::  0xff11 with wid=8 becomes ~[0x11 0xff 0x0 0x0 0x0 0x0 0x0 0x0]
  ::
  ++  from-byts-le
    |=  =byts  ^-  buffer
    =/  b=(list @ux)  (rip 3 dat.byts)
    =/  pad=@  (sub wid.byts (lent b))
    (weld b (reap pad 0x0))
  ::
  ++  to-byts
    |=  b=buffer  ^-  byts
    [(lent b) (rep 3 (flop b))]
  ::
  ++  concat-as-byts
    |=  bs=(list buffer)  ^-  byts
    %-  to-byts  (zing bs)
  --
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
    :-  wid.b
    %+  lsh  [3 (sub wid.b (met 3 dat.b))]
    (swp 3 dat.b)
::  Converts a list of bits to a list of n-bit numbers 
::  input-bits should be big-endian
::
++  bit
  |%
  ::  rip atom a with num-bits. Preserve leading 0s, big endian
  ::  returns a list of bits
  ::
  ++  zeros-brip
    |=  [num-bits=@ a=@]
    ^-  (list @)
    =/  bits=(list @)  (flop (rip 0 a))
    =/  pad=@  (sub num-bits (lent bits))
    (weld (reap pad 0) bits)
  ::  +convert: list of bits to a list of atoms each with bitwidth d(est)
  ::
  ++  convert
    |=  [d=@ bits=(list @)]
    ^-  (list @)
    =|  ret=(list @)
    |-  ?~  bits  ret
    =/  dest-bits  (scag d ((list @) bits))
    ::  left-shift the "missing" number of bits
    =/  num=@
      %+  lsh  [0 (sub d (lent dest-bits))]
      (rep 0 (flop dest-bits))
    $(ret (snoc ret num), bits (slag d ((list @) bits)))
  ::  Converts e.g. ~[0 0 31 31 31 31 0 0] in base32 (5 bitwidth)
  ::  to ~[0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0]
  ::
  ++  from-digits
    |=  [bitwidth=@ digits=(list @)]
    ^-  (list @)
    %-  zing
    %+  turn  digits
    |=  d=@  (zeros-brip bitwidth d)
  ::  converts 40 bits: ~[0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0]
  ::  to 0x3fff.fc00 when cast to hex
  ::
  ++  to-atom
    |=  bits=(list @)
    ^-  @
    %+  rep  0
    %-  flop  bits
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
    %+  |=([a=@ b=@] =(a b))
      1
    %-  polymod
    (weld (expand-hrp hrp) data-and-checksum)
  ::
  ++  checksum
    |=  [hrp=tape data=(list @)]
    ^-  (list @)
    ::  xor 1 with the polymod
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
    ^-  bech32-address
    =/  combined=(list @)
      (weld data (checksum hrp data))
    :-  %bech32
    %-  crip
    (zing ~[hrp "1" (tape (murn combined value-to-charset))])
  ++  decode-raw
    |=  b=bech32-address
    ^-  (unit raw-decoded)
    =/  bech  (cass (trip +.b))              ::  to lowercase
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
  ::  goes from a bech32 address to hex. Returns byts to preserve leading 0s
  ::
  ++  to-hex
    |=  b=bech32-address  ^-  hash
    =/  d=(unit raw-decoded)  (decode-raw b)
    ?~  d  ~|("Invalid bech32 address" !!)
    =/  bs=(list @)
      (from-digits:bit 5 (slag 1 data.u.d))
    =/  byt-len=@  (div (lent bs) 8)
    ?.  =(0 (mod (lent bs) 8))
      ~|("Invalid bech32 address: not 8bit" !!)
    ?.  ?|(?=(%20 byt-len) ?=(%32 byt-len))
      ~|("Invalid bech32 address: must be 20 (P2WPKH) or 32 (P2WSH) bytes" !!)
    %-  hash
    [byt-len (to-atom:bit bs)]
  ::  pubkey is the 33 byte ECC compressed public key
  ::
  ++  encode-pubkey
    |=  [=network pubkey=@ux]
    ^-  (unit bech32-address)
    ?.  =(33 (met 3 pubkey))
      ~|('pubkey must be a 33 byte ECC compressed public key' !!)
    =/  prefix  (~(get by prefixes) network)
    ?~  prefix  ~
    :-  ~
    %+  encode-raw  u.prefix
    [0 (convert:bit 5 (zeros-brip:bit 160 dat:(hash-160 pubkey)))]
  ++  encode-hash-160
    |=  [=network h160=byts]
    ^-  (unit bech32-address)
    =/  prefix  (~(get by prefixes) network)
    ?~  prefix  ~
    :-  ~
    %+  encode-raw  u.prefix
    [0 (convert:bit 5 (zeros-brip:bit 160 dat.h160))]
  --
::
--
