/-  *btc
|%
::  big endian sha256: input and output are both MSB first (big endian)
::
++  sha256
  |=  =byts  ^-  hash
  ::  if there are leading 0s, lshift by their amount after flip to little endian to preserve
  =/  pad=@  (sub wid.byts (met 3 dat.byts))
  =/  little-endian=@
    (lsh 3 pad (swp 3 dat.byts))
  :-  32
  %+  swp  3
  (shay wid.byts little-endian)
::
++  dsha256
  |=  =byts
  (sha256 (sha256 byts))
::
++  hash-160
  |=  pubkey=@ux  ^-  hash
  =,  ripemd:crypto
  :-  20
  %-  ripemd-160
  %-  sha256  [(met 3 pubkey) pubkey]
::
++  address-to-script-pubkey
  |=  =address  ^-  buffer:tx
  ?.  ?=(%bech32 -.address)
    ~|("Only bech32 addressess supported right now" !!)
  =/  hex=byts  (to-hex:bech32 (trip +.address))
  ?.  =(wid.hex 20)
    ~|("Only 20-byte P2WPKH bech32 supported" !!)
  %-  zing
    :~  ~[0x19 0x76 0xa9 0x14]
        (from-byts:buffer hex)
        ~[0x88 0xac]
    ==
::
::  list of @ux that is big endian for hashing purposes
::  used to preserve 0s when concatenating byte sequences
::
++  buffer
  |%
  ++  from-byts
    |=  =byts  ^-  buffer:tx
    =/  b=(list @ux)
      (flop (rip 3 dat.byts))
    =/  pad=@  (sub wid.byts (lent b))
    (weld (reap pad 0x0) b)
  ::  converts an atom to a little endian buffer with wid length (trailing 0s)
  ::  atom 1 with wid=4 becomes ~[0x1 0x0 0x0 0x0]
  ::  0xff11 with wid=8 becomes ~[0x11 0xff 0x0 0x0 0x0 0x0 0x0 0x0]
  ::
  ++  from-atom-le
    |=  [wid=@ a=@]  ^-  buffer:tx
    =/  b=(list @ux)  (rip 3 a)
    =/  pad=@  (sub wid (lent b))
    (weld b (reap pad 0x0))
  ::
  ++  to-byts
    |=  b=buffer:tx  ^-  byts
    [(lent b) (rep 3 (flop b))]
  ++  concat-as-byts
    |=  bs=(list buffer:tx)  ^-  byts
    %-  to-byts  (zing bs)
  --
::
++  unsigned-tx
  =,  buffer
  |_  ut=unsigned:tx
  ++  prevouts-buffer
    |=  =input:tx  ^-  buffer:tx
    %+  weld
      (from-byts tx-hash.input)
    (from-atom-le 4 witness-ver.input)
  ::
  ++  sequence-buffer
    |=  =input:tx  ^-  buffer:tx
    (from-byts sequence.input)
  ::
  ++  outputs-buffer
    |=  =output:tx  ^-  buffer:tx
    %+  weld
      (from-atom-le 8 value.output)
    (address-to-script-pubkey address.output)
  ::
  ++  sighash
    |=  input-index=@  ^-  hash
    ?:  (gte input-index (lent inputs.ut))
      ~|("Input index out of range" !!)
    =/  =input:tx  (snag input-index inputs.ut)
    ?:  =(1 witness-ver.input)
      (sighash-witness input)
    (sighash-legacy input)
  ::
  ++  sighash-witness
    |=  =input:tx  ^-  hash
    =/  prevouts=byts
      %-  concat-as-byts  (turn inputs.ut prevouts-buffer)
    =/  sequences=byts
      %-  concat-as-byts
      (turn inputs.ut sequence-buffer)
    =/  outputs=byts
      %-  concat-as-byts  (turn outputs.ut outputs-buffer)
    ~&  >  [prevouts=(dsha256 prevouts) sequences=(dsha256 sequences) outputs=(dsha256 outputs)]
    [0 0x0]
  ::
  ++  sighash-legacy
    |=  =input:tx  ^-  hash
    [0 0x0]
  --
::
::  Converts a list of bits to a list of n-bit numbers
::  input-bits should be big-endian
::
++  bits
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
  ::  converts from bit list to a list of atoms each with bitwidth d(est)
  ::
  ++  convert
    |=  [d=@ bits=(list @)]
    ^-  (list @)
    =|  ret=(list @)
    |-  ?~  bits  ret
    =/  dest-bits  (scag d ((list @) bits))
    ::  left-shift the "missing" number of bits
    =/  num=@
      %:  lsh  0
          (sub d (lent dest-bits))
          (rep 0 (flop dest-bits))
      ==
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
  ::
  ++  polymod
    |=  values=(list @)
    |^  ^-  @
    =/  gen=(list @ux)
      ~[0x3b6a.57b2 0x2650.8e6d 0x1ea1.19fa 0x3d42.33dd 0x2a14.62b3]
    =/  chk=@  1
    |-  ?~  values  chk
    =/  top  (rsh 0 25 chk)
    =.  chk
      (mix i.values (lsh 0 5 (dis chk 0x1ff.ffff)))
    $(values t.values, chk (update-chk chk top gen))
  ::
    ++  update-chk
      |=  [chk=@ top=@ gen=(list @ux)]
      =/  is  (gulf 0 4)
      |-  ?~  is  chk
      ?:  =(1 (dis 1 (rsh 0 i.is top)))
        $(is t.is, chk (mix chk (snag i.is gen)))
      $(is t.is)
    --
  ::
  ++  expand-hrp
    |=  hrp=tape
    ^-  (list @)
    =/  front  (turn hrp |=(p=@tD (rsh 0 5 p)))
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
    |=(i=@ (dis 31 (rsh 0 (mul 5 (sub 5 i)) pmod)))
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
    ^-  tape
    =/  combined=(list @)
    (weld data (checksum hrp data))
    (zing ~[hrp "1" (tape (murn combined value-to-charset))])
  ++  decode-raw
    |=  bech=tape
    ^-  (unit raw-decoded)
    =.  bech  (cass bech)              ::  to lowercase
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
    |=  bech=tape
    ^-  byts
    =/  d=(unit raw-decoded)  (decode-raw bech)
    ?~  d  ~|("Invalid bech32 address" !!)
    =/  bs=(list @)
      (from-digits:bits 5 (slag 1 data.u.d))
    ?.  =(0 (mod (lent bs) 8))
      ~|("Invalid bech32 address: not 8bit" !!)
    [(div (lent bs) 8) (to-atom:bits bs)]
  ::  pubkey is the 33 byte ECC compressed public key
  ::
  ++  encode-pubkey
    |=  [=network pubkey=@ux]
    ^-  (unit tape)
    ?.  =(33 (met 3 pubkey))
      ~|('pubkey must be a 33 byte ECC compressed public key' !!)
    =/  prefix  (~(get by prefixes) network)
    ?~  prefix  ~
    :-  ~
    %+  encode-raw  u.prefix
    [0 (convert:bits 5 (zeros-brip:bits 160 dat:(hash-160 pubkey)))]
  ++  encode-hash-160
    |=  [=network h160=byts]
    ^-  (unit tape)
    =/  prefix  (~(get by prefixes) network)
    ?~  prefix  ~
    :-  ~
    %+  encode-raw  u.prefix
    [0 (convert:bits 5 (zeros-brip:bits 160 dat.h160))]
  --
::
--
