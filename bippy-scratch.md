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

## Handling XPubs
**Import lib; optionally set up env**
XPub is BIP84, mnemonic:
abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about
```
=b -build-file %/lib/btc-scratch/hoon
=xpub "zpub6rFR7y4Q2AijBEqTUquhVz398htDFrtymD9xYYfG1m4wAcvPhXNfE3EfH1r1ADqtfSdVCToUG868RvUUkgDKf31mGDtKsAYz2oz2AGutZYs"
```

### Getting BIP84 Address from `xpub`
```
(~(address bip84:b %main xpub))
```

### with `~norsyr-torryn`'s bip32 library
```
=bip32 -build-file %/lib/bip32/hoon
=ecc secp256k1:secp:crypto

::  get 0 index in non-change account
`@ux`(compress-point:ecc pub:(derive-public:(derive-public:(from-extended:bip32 xpub) 0) 0))
```

## BIP 173 (Bech32 Addresses)
```
=btc -build-file %/lib/btc/hoon
```

### Bech32 Algo
- hash = hash160(pubkey)
- 5-bit-words = convert-bits(8, 5, hash-atom)
- encode("bc", [0, 5-bit-words])

### BTC pubkey -> address hashing (Hash-160)
Uses the example data here:
https://en.bitcoin.it/wiki/Technical_background_of_version_1_Bitcoin_addresses

That one starts with pubkey below. The following runs it through sha256 and ripemd160 to get Hash-160:
```
0xf54a.5851.e937.2b87.810a.8e60.cdd2.e7cf.d80b.6e31
```

Use `@uc` to make the Hash-160 into a BTC P2PKH address
```
=pubkey 0x2.5086.3ad6.4a87.ae8a.2fe8.3c1a.f1a8.403c.b53f.53e4.86d8.511d.ad8a.0488.7e5b.2352
(hash-160:btc pubkey)
`@uc`(hash-160:btc pubkey)`@uc`(hash-160:btc pubkey)
```

### trailing zero
s
Need to test with this because it shows need to input num bytes
```
0x3.f3c1.3839.3683.93e7.0caf.4148.4775.b805.312d.58be.d157.1308.3d27.5cf5.6998.0100
```

###  bip173 test pubkey
Pubkey
0x2.79be.667e.f9dc.bbac.55a0.6295.ce87.0b07.029b.fcdb.2dce.28d9.59f2.815b.16f8.1798

Hash-160 (has leading 0s, so good to check)
751e76e8199196d454941c45d1b3a323f1433bd6
0xf54a.5851.e937.2b87.810a.8e60.cdd2.e7cf.d80b.6e31

###  bip84 public keys
From seed mnemonic:
```
process child keen cargo design install parrot hold pole unveil dance reason drink cash fix

0x2.88b5.a58a.5c26.6cef.d41b.f329.9165.46cc.1703.c4d9.a32e.1ea3.ef3d.1823.c493.05ac
0x3.289a.4e24.4381.8992.fe20.0831.3551.a3af.2266.ef3d.2038.5df9.6daa.92e3.4df2.16c4
0x3.109a.2082.eaa6.8925.1465.5393.d635.7fb9.d9b5.e191.3826.8837.69cd.db88.7a4b.b4f0
```

## Deprecated: `btc-address` library
Left here just for reference

**Test child public key from xpub**
```
=btca -build-file %/lib/btc-address/hoon
=ecc secp256k1:secp:crypto
`@ux`(child-from-xpub:btca zpub 0)
(child-from-xpub:btca xpub (dec (bex 31)))

::  should error as index is too high (hardened key range)
(child-from-xpub:btca zpub (bex 31))
```

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
