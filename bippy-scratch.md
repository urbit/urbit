## NOTE
The below requires norsyr's fix to `decompress-point` in order to work.

## Working with BTC RPC Library
```
|start :btc-bridge
:btc-node-hook|command [%credentials 'http://127.0.0.1:18443' 'poopman' 'chAiM31eeJ1MK3y8BC3mR9q2']
:btc-node-hook|command [%ping ~]
:btc-node-hook|command [%watch %get-block-count]
:btc-node-hook|action [%get-block-count ~]
:btc-node-hook|command [%unwatch %get-block-count]
```

## btc-address
**Import lib; optionally set up env**
```
=btca -build-file %/lib/btc-address/hoon
=mnemonic="abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about"

=bip32 -build-file %/lib/bip32/hoon
=ecc secp256k1:secp:crypto
=zpub "zpub6rFR7y4Q2AijBEqTUquhVz398htDFrtymD9xYYfG1m4wAcvPhXNfE3EfH1r1ADqtfSdVCToUG868RvUUkgDKf31mGDtKsAYz2oz2AGutZYs"
```

**Test child public key from xpub**
```
`@ux`(child-from-xpub:btca zpub 0)
(child-from-xpub:btca xpub (dec (bex 31)))

::  should error as index is too high (hardened key range)
(child-from-xpub:btca zpub (bex 31))
```

**Same, with Jose's bip32 library**
```
::  get 0 index in non-change account
`@ux`(compress-point:ecc pub:(derive-public:(derive-public:(from-extended:bip32 zpub) 0) 0))
```

## BIP 173 (Bech32 Addresses)
```
=bip173 -build-file %/lib/bip173/hoon
```

### Bech32 Algo
- hash = hash160(pubkey)
- words = convert(hash, 8bitTo5bit)
- encode('bc', [0x00 words])

### BTC pubkey -> address hashing (Hash-160)
Uses the example data here:
https://en.bitcoin.it/wiki/Technical_background_of_version_1_Bitcoin_addresses

That one starts with pubkey below. The following runs it through sha256 and ripemd160 to yield the hash:
```
0xf54a.5851.e937.2b87.810a.8e60.cdd2.e7cf.d80b.6e31
```

Use `@uc` to make the Hash-160 into a BTC P2PKH address
```
=pubkey 0x2.5086.3ad6.4a87.ae8a.2fe8.3c1a.f1a8.403c.b53f.53e4.86d8.511d.ad8a.0488.7e5b.2352
(p2pkh:bip173 pubkey)
`@uc`(p2pkh:bip173 pubkey)
```

## Deprecated: `btc-address` Child Derivations

**Test xpub parsing**
```
(parse-xpub:btca xpub)
```

**Test addition and ECC point checking**
```
=px (parse-xpub:btca xpub)
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


0xf54a.5851.e937.2b87.810a.8e60.cdd2.e7cf.d80b.6e31

0x2.b092.22c4.98c3.ab97.ce2b.6c4d.e68f.07d4.b031.589c.4d6b.bd31.7791.068f.7347.5201

0x2.5086.3ad6.4a87.ae8a.2fe8.3c1a.f1a8.403c.b53f.53e4.86d8.511d.ad8a.0488.7e5b.2352
