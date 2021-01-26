/+  *test, *btc
|%
+$  key-address  [pubkey=bytc =address]
++  vectors
  |%
  ++  p2wsh1
    ^-  key-address
    :*  33^0x3.dd26.dbea.6fe8.35be.59d4.21fb.2732.1e4b.dc28.2cbd.a227.db63.4a6d.93bd.8f93.dd4a
        [%base58 '32qeqDRxe2YStNzTkojBdB8taQFzDdhgnJ']
    ==
  --
++  pk-p2wsh
  |=  pubkey=byts
  ^-  byts
  %-  hash-160
  %-  cat:byt
  :~  1^0x0
      1^0x14
      (hash-160 pubkey)
  ==
::
++  run
  ::  test base58check encode/decode
  =/  p2wsh1=key-address
    p2wsh1:vectors
  =/  w-version=byts
    (cat:byt ~[1^0x5 (pk-p2wsh pubkey.p2wsh1)])
  =/  encoding=tape
    (encode:base58check w-version)
  ?.  =(address.p2wsh1 [%base58 (crip encoding)])
    ~|("pubkey doesn't encode to base58" !!)
  ?.  =(w-version (decode:base58check encoding))
    ~|("'can't decode base58 encoding" !!)
  "All tests passed."
--
