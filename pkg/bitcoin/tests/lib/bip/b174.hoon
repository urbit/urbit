/-  *bitcoin
/+  *test, *bip-b158, bcu=bitcoin-utils, pbt=bip-b174
|%
+$  psbt-vector
  $:  =hdkey
      hdkey-hex=hexb
  ==
++  fprint  4^0xdead.beef
++  psbt-vectors
  ^-  (list psbt-vector)
  :~  :*  [fprint 33^0x1 %testnet %44 %0 1]
          20^0x2c00.0080.0100.0080.0000.0080.0000.0000.0100.0000
      ==
      ::
      :*  [fprint 33^0x1 %testnet %49 %0 1]
          20^0x3100.0080.0100.0080.0000.0080.0000.0000.0100.0000
      ==
      ::
      :*  [fprint 33^0x1 %testnet %84 %0 1]
          20^0x5400.0080.0100.0080.0000.0080.0000.0000.0100.0000
      ==
      ::
      :*  [fprint 33^0x1 %main %44 %0 1]
          20^0x2c00.0080.0000.0080.0000.0080.0000.0000.0100.0000
      ==
      ::
      :*  [fprint 33^0x1 %main %49 %0 1]
          20^0x3100.0080.0000.0080.0000.0080.0000.0000.0100.0000
      ==
      ::
      :*  [fprint 33^0x1 %main %84 %0 1]
          20^0x5400.0080.0000.0080.0000.0080.0000.0000.0100.0000
      ==
  ==
++  test-all-vectors
^-  tang
  |^  ;:  weld
          %+  category  "check PSBT"
          (zing (turn psbt-vectors check-psbt))
      ==
  ++  check-psbt
    |=  v=psbt-vector
    =/  key=hexb
      (cat:byt:bcu ~[1^0x6 pubkey.hdkey.v])          ::  %input target
    %+  expect-eq
      !>([key (cat:byt:bcu ~[fprint.hdkey.v hdkey-hex.v])])
      !>((hdkey:en:pbt %input hdkey.v))
  --
--
