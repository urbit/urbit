## NOTE
The below requires norsyr's fix to `decompress-point` in order to work.

## base58
Converts a base58 zpub to hex
```
+bip84 "zpub6rFR7y4Q2AijBEqTUquhVz398htDFrtymD9xYYfG1m4wAcvPhXNfE3EfH1r1ADqtfSdVCToUG868RvUUkgDKf31mGDtKsAYz2oz2AGutZYs"
```

### btc-address
**Import lib; optionally set up env**
```
=btca -build-file %/lib/btc-address/hoon
=bip32 -build-file %/lib/bip32/hoon
=xpub2 "xpub6DnWFmBQfQm1wxvKkCJjXwE6H4v8FTwUuhjDQ9ZpJnFDfhA8Dwmg71yPKyjUE93D2CB6MdnWNvGmwsb3fpd4oRJ2YcyMZoMpLU3BjpmQAny"
```

**Test child public key from xpub**
```
`@ux`(child-from-xpub:btca xpub2 0)
`@ux`(child-from-xpub:btca xpub 1)
(child-from-xpub:btca xpub (dec (bex 31)))

::  should error as index is too high (hardened key range)
(child-from-xpub:btca xpub (bex 31))
```

**Same, with Jose's bip32 library**
```
`@ux`(compress-point:secp256k1:secp:crypto pub:(derive-public:(from-extended:bip32 xpub2) 0))
```

**Test xpub parsing**
```
(parse-xpub:btca xpub)
```

**Test addition and ECC point checking**
```
=px (parse-xpub:btca xpub2)
=pubk ?~  px  ~  pubk.u.px
(is-point:btca pubk)
(pubkey-to-point:btca pubk)
=index 256
`@ux`(add (lsh 3 4 (big-endian-brap:btca pubk)) index)
```

**Test computing I**
```
(bind px |=(px=parsed-xpub:btca (compute-i:btca px 1)))
```
