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
