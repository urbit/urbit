/-  *btc
|%
++  hash-160
  |=  pubkey=@ux
  ^-  @ux
  =,  ripemd:crypto
  ::  use shay to preserve byte length when there are trailing zeros
  =/  sha256  (swp 3 (shay 33 (swp 3 pubkey)))
    (ripemd-160 [32 sha256])
::  Converts a list of bits to a list of n-bit numbers
::  input-bits should be big-endian
::
++  bits
  |%
  ::  rip atom of bitwidth wordlen. Preserve leading 0s, big endian
  ::
  ++  zeros-brip
    |=  [bitwidth=@ a=@]
    ^-  (list @)
    =/  bits=(list @)  (flop (rip 0 a))
    =/  r=@  (mod (lent bits) bitwidth)
    ?:  =(0 r)  bits                                         ::  no remainder, i.e. no leading zeros
    (weld (reap (sub bitwidth r) 0) bits)
  ::  converts from atom a with bitwidth s to a list of atoms each with bitwidth d
  ::
  ++  convert
    |=  [s=@ d=@ a=@]
    ^-  (list @)
    =|  ret=(list @)
    =/  bits  (zeros-brip s a)
    |-  ?~  bits  ret
    =/  dest-bits  (scag d ((list @) bits))
    ::  left-shift the "missing" number of bits
    =/  num=@
      %:  lsh  0
          (sub d (lent dest-bits))
          (rep 0 (flop dest-bits))
      ==
    $(ret (snoc ret num), bits (slag d ((list @) bits)))
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
  ::  pubkey is the 33 byte ECC compressed public key
  ++  encode-pubkey
    |=  [=network pubkey=@ux]
    ^-  (unit tape)
    ?.  =(33 (met 3 pubkey))
      ~|('pubkey must be a 33 byte ECC compressed public key' !!)
    =/  prefix  (~(get by prefixes) network)
    ?~  prefix  ~
    :-  ~
    %+  encode-raw  u.prefix
    (weld ~[0] (convert:bits 8 5 (hash-160 pubkey)))
  --

::
--
