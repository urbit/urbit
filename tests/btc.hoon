/+  *test, *btc
|%
+$  key-address  [pubkey=hexb =address]
++  vectors
  |%
  ++  p2wsh1
    ::  abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about
    ^-  key-address
    :*  33^0x3.9b3b.694b.8fc5.b5e0.7fb0.69c7.83ca.c754.f5d3.8c3e.08be.d196.0e31.fdb1.dda3.5c24
        [%base58 '37VucYSaXLCAsxYyAPfbSi9eh4iEcbShgf']
    ==
  ++  psbt1
    ^-  base64:psbt
    'cHNidP8BAHEBAAAAAeECXCgB7Co1v8MkjfseiHiAmD6f5IGHggCodBxdcE6DAQAAAAD/////AtAHAAAAAAAAFgAUmEQgSFlZ0Noj5/6QK/MVgUzHtksw/AAAAAAAABYAFCj6E0N2r50pGUjGeWDtDrP3JxeQAAAAAAABAR+EJQEAAAAAABYAFDi7rsJdW0ystnpbESNGOx1ypK7lIgYDXS/0FkRiWJny6+uNNGt7tpCWk96YQDJnUKqZLWGx9JYYvu/erVQAAIAAAACAAAAAgAEAAAADAAAAAAAiAgOQCPy++fOAW9XIFBqcaec8QU0qPeSDFLD1PR/QrWTSrBi+796tVAAAgAAAAIAAAACAAQAAAAQAAAAA'
  --
++  pk-to-p2wsh
  |=  pubkey=hexb
  ^-  hexb
  %-  hash-160
  %-  cat:byt
  :~  1^0x0
      1^0x14
      (hash-160 pubkey)
  ==
::
++  run
  :: base58check encode/decode
  ::
  =/  p2wsh1=key-address
    p2wsh1:vectors
  =/  with-version=hexb
    (cat:byt ~[1^0x5 (pk-to-p2wsh pubkey.p2wsh1)])
  =/  encoding=tape
    (encode:base58check with-version)
  ?.  =(address.p2wsh1 [%base58 (crip encoding)])
    ~|("pubkey doesn't encode to base58" !!)
  ?.  =(with-version (decode:base58check encoding))
    ~|("'can't decode base58 encoding" !!)
  "All tests passed."
--
