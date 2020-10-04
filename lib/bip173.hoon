|%
+$  address  ?([@uc [%bech32 @t]])
+$  decoded-bech32  [hrp=tape data=(list @) checksum=(list @)]
::
++  charset  "qpzry9x8gf2tvdw0s3jn54khce6mua7l"
::
++  polymod
  |=  values=(list @)
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
  %-  polymod
  (weld (expand-hrp hrp) data-and-checksum)
::
++  charset-to-value
  |=  c=@tD
  ^-  (unit @)
  (find ~[c] charset)
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
::
++  decode-bech32
  |=  bech=tape
  ^-  (unit decoded-bech32)
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
  ?.  =(1 (verify-checksum hrp data-and-checksum))
    ~
  =/  checksum-pos  (sub (lent data-and-checksum) 6)
  `[hrp (scag checksum-pos data-and-checksum) (slag checksum-pos data-and-checksum)]
::
++  hash-160
  |=  pubkey=@ux
  ^-  @ux
  =/  sha  (swp 3 (shax (swp 3 pubkey)))
    %-  ripemd-160:ripemd:crypto
    [(met 3 sha) sha]
::
++  decode-segwit
  |=  segwit=tape
  ^-  (unit [witver=@ witprog=@ux])
  ~
--
