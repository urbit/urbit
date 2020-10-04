|%
+$  address  ?([@uc [%bech32 @t]])
++  charset  "qpzry9x8gf2tvdw0s3jn54khce6mua7l"
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
  |=  [hrp=tape data=(list @) checksum=(list @)]
  %-  polymod
    (zing ~[(expand-hrp hrp) data checksum])
::
++  charset-to-value
  |=  c=@tD
  ^-  (unit @)
  (find ~[c] charset)
::
++  decode-bech32
  |=  bech=tape
  ^-  (unit [hrp=tape data=(list @) checksum=(list @)])
  ::  TODO: error handling and checksum verify
  =.  bech  (cass bech)         ::  to lowercase
  =/  pos  (flop (fand "1" bech))
  ?~  pos  ~
  =/  last-1=@  i.pos
  =/  data-and-checksum=(list @)
    (slag +(last-1) bech)
  =/  from-base32=(list @)
    %+  murn  data-and-checksum
    charset-to-value
  ?.  =((lent data-and-checksum) (lent from-base32))
    ~
  =/  checksum-pos  (sub (lent from-base32) 6)
  :-  ~
  :*  hrp=(scag last-1 bech)
      data=(scag checksum-pos from-base32)
      checksum=(slag checksum-pos from-base32)
  ==
::
++  decode-segwit
  |=  segwit=tape
  ^-  (unit [witver=@ witprog=@ux])
  ~
--
