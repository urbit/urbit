::  BIP173: Bech32 Addresses
::  https://github.com/bitcoin/bips/blob/master/bip-0173.mediawiki
::
::  Heavily copies:
::  https://github.com/bitcoinjs/bech32/blob/master/index.js
::
/-  sur=bitcoin
/+  bcu=bitcoin-utils
=,  sur
=,  bcu
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
