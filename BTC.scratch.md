## NOTE
The below requires norsyr's fix to `decompress-point` in order to work.

## Set Credentials and Ping Servers
(don't need password if all run on localhost)
```
=rpc-pass 'c7dc0698a2e3a5d66096152b34c273b8fbcc821e9d49f2ac706e38307d3441c5'
:btc-provider|command [%set-credentials [rpc-url='http://localhost:8332' rpc-user='__cookie__' rpc-pass] [rpc-url='http://localhost:50002']]

:btc-provider|action [%address-info [%bech32 'bc1q59u5epktervh6fxqay2dlph0wxu9hjnx6v8n66']]
:btc-provider|action [%address-info [%bech32 'bc1qlwd7mw33uea5m8r2lsnsrkc7gp2qynrxsfxpfm']]
:btc-provider|action [%address-info [%bech32 'bc1qglkc9zfcn04vcc88nn0ljtxcpu5uxfznc3829k']]
::  first is an address w balance
::  second has no balance but is used
::  third is unused
```

## Transactions
bc1q59u5epktervh6fxqay2dlph0wxu9hjnx6v8n66

```
createrawtransaction '[{"txid" : "033f693fdf995a5ea7fe5c951ab6858c7e6a5fffc58579922cd4fc319c614c5b", "vout" : 0}]' '{"bc1qwsqxh3sdjqgdxl7ewgxftdfm8jjajta5xmv8eu" : 0.00001}'
```

## Handling XPubs
**Import lib; optionally set up env**
XPub is BIP84, mnemonic:
abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about
```
=b -build-file %/lib/btc-scratch/hoon
=xpub "zpub6rFR7y4Q2AijBEqTUquhVz398htDFrtymD9xYYfG1m4wAcvPhXNfE3EfH1r1ADqtfSdVCToUG868RvUUkgDKf31mGDtKsAYz2oz2AGutZYs"
(~(address bip84:b %main xpub))
```

### with `~norsyr-torryn`'s bip32 library
```
=bip32 -build-file %/lib/bip32/hoon
=ecc secp256k1:secp:crypto

=xpub "zpub6rFR7y4Q2AijBEqTUquhVz398htDFrtymD9xYYfG1m4wAcvPhXNfE3EfH1r1ADqtfSdVCToUG868RvUUkgDKf31mGDtKsAYz2oz2AGutZYs"
`@ux`(compress-point:ecc pub:(derive-public:(derive-public:(from-extended:bip32 xpub) 0) 0))
::  gives 0x3.30d5.4fd0.dd42.0a6e.5f8d.3624.f5f3.482c.ae35.0f79.d5f0.753b.f5be.ef9c.2d91.af3c
::  gets 0 index in non-change account
```

## Legacy BTC Address Parsing
```
::  yields the initial addresss
`@uc`(scan "17xg1BZLn63zCxdTxbsFLoWpQeSnD7zSHW" fim:ag)

::  as a cord
`@t`(scot %uc `@uc`(scan "17xg1BZLn63zCxdTxbsFLoWpQeSnD7zSHW" fim:ag))
```

## sha256 Implementation
This is needed for hashing to/from big endian, possibly with leading and trailing 0s.

We use 20 byte value `0x7f5a.997b.83f5.1f79.3b89.10be.9950.8b00.a136.f900`

### JS Reference Values
```
val = "7f5a997b83f51f793b8910be99508b00a136f900";
b = Buffer.from(val, "hex");
createHash('sha256').update(b).digest();
// yields: <Buffer 4d 9a 8e 9b 96 6c f8 3a 69 5c 60 7b c3 23 74 a0 d2 70 35 aa 52 a0 b6 73 8d c4 73 fe 95 c6 8f b3>

val2 = "007f5a997b83f51f793b8910be99508b00a136f900";
b2 = Buffer.from(val2, "hex");
createHash('sha256').update(b2).digest();
// yields: <Buffer 66 ee cb 4f b5 0e 9d 93 f8 5e 78 69 6a 34 09 7a b8 59 fb c9 b7 f6 f0 87 33 29 42 3b 5f dd 80 72>
```

In the Hoon version, we indicate leading zeros by increasing the byte count, rather than with a string
```
::  trailing 0s
=val 0x7f5a.997b.83f5.1f79.3b89.10be.9950.8b00.a136.f900
`@ux`dat:(sha256:btc 20 val)
:: gives 0x4d9a.8e9b.966c.f83a.695c.607b.c323.74a0.d270.35aa.52a0.b673.8dc4.73fe.95c6.8fb3

::  leading 0s--pass 21 as byte count
`@ux`dat:(sha256:btc 21 val)
::  gives 0x66ee.cb4f.b50e.9d93.f85e.7869.6a34.097a.b859.fbc9.b7f6.f087.3329.423b.5fdd.8072
```

## Byte Buffers
Makes it easy to concatenate hex sequences with leading 0s, and then convert them back to bytes for hashing.

### JS Reference
```
b = Buffer.from("000000000000ff00", "hex");
createHash('sha256').update(b).digest()
//  yields: <Buffer e8 83 e8 6b ff eb ac 72 cc 7d 32 0a b7 2f e1 e5 5c 19 9d 55 d8 41 bf 43 cb d8 03 c5 55 a7 10 06>
```

Hoon:
```
=b (from-byts:buffer:btc [8 0xff00])
`[@ @ux]`(sha256:btc (to-byts:buffer:btc b))

::  gives [32 0xe883.e86b.ffeb.ac72.cc7d.320a.b72f.e1e5.5c19.9d55.d841.bf43.cbd8.03c5.55a7.1006]
```

## 8bit to 5bit conversion, and back again
Start with a Hash160 (8bit hex). Convert to bech32. Decode the bech32, drop the leading 0, and run through `digits-to-atom`.

### 8 bits to 5 bits

### 5 bits to 8 bits
```
::  base32:
=val ~[0 0 31 31 31 31 0 0]
(from-digits:bits:btc 5 val)
::  gives ~[0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0]

`@ux`(to-atom:bits:btc (from-digits:bits:btc 5 val))
::  gives 0x3fff.fc00
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
```

Use `@uc` to make the Hash-160 into a BTC P2PKH address
```
=pubkey 0x2.5086.3ad6.4a87.ae8a.2fe8.3c1a.f1a8.403c.b53f.53e4.86d8.511d.ad8a.0488.7e5b.2352
(hash-160:btc pubkey)
::  gives 0xf54a.5851.e937.2b87.810a.8e60.cdd2.e7cf.d80b.6e31

`@uc`(hash-160:btc pubkey)
:: gives 1PMycacnJaSqwwJqjawXBErnLsZ7RkXUAs
```

### trailing zero
Need to test with this because it shows need to input num bytes
```
0x3.f3c1.3839.3683.93e7.0caf.4148.4775.b805.312d.58be.d157.1308.3d27.5cf5.6998.0100
```

###  bip173 test pubkey
The below code also shows how to convert from bech32 back to a 20-byte hash. This is used for transaction building.
```
=pubkey 0x2.79be.667e.f9dc.bbac.55a0.6295.ce87.0b07.029b.fcdb.2dce.28d9.59f2.815b.16f8.1798
(encode-pubkey:bech32:btc %main pubkey)bbt
::  gives [~ %bech32 'bc1qw508d6qejxtdg4y5r3zarvary0c5xw7kv8f3t4']

`[@ @ux]`(hash-160:btc pubkey)
::  gives [20 0x751e.76e8.1991.96d4.5494.1c45.d1b3.a323.f143.3bd6]

(encode-hash-160:bech32:btc %main (hash-160:btc pubkey))
::  gives [~ %bech32 'bc1qw508d6qejxtdg4y5r3zarvary0c5xw7kv8f3t4']
```

### Decode Bech32 to hex
Return val as `byts` to preserve leading zeros.
```
(decode-raw:bech32:btc [%bech32 'bc1qw508d6qejxtdg4y5r3zarvary0c5xw7kv8f3t4'])
:: prints data: ~[0 14 20 15 7 13 26 0 25 18 6 11 13 8 21 4 20 3 17 2 29 3 12 29 3 4 15 24 20 6 14 30 22]

(to-hex:bech32:btc [%bech32 'bc1qw508d6qejxtdg4y5r3zarvary0c5xw7kv8f3t4'])
:: gives [20 0x751e.76e8.1991.96d4.5494.1c45.d1b3.a323.f143.3bd6]
```

###  bip84 public keys
From seed mnemonic:
```
process child keen cargo design install parrot hold pole unveil dance reason drink cash fix

0x2.88b5.a58a.5c26.6cef.d41b.f329.9165.46cc.1703.c4d9.a32e.1ea3.ef3d.1823.c493.05ac
0x3.289a.4e24.4381.8992.fe20.0831.3551.a3af.2266.ef3d.2038.5df9.6daa.g92e3.4df2.16c4
0x3.109a.2082.eaa6.8925.1465.5393.d635.7fb9.d9b5.e191.3826.8837.69cd.db88.7a4b.b4f0
```

## Unsigned Transactions
Using [BIP 143](https://github.com/bitcoin/bips/blob/master/bip-0143.mediawiki#P2SHP2WPKH) as a reference.

Native P2WPKH
```
=btc -build-file %/lib/btc/hoon
=input0 (input:tx:btc [[32 0xfff7.f788.1a80.99af.a694.0d42.d1e7.f636.2bec.3817.1ea3.edf4.3354.1db4.e4ad.969f] 0 0 [4 0xeeff.ffff] [35 0x21.03c9.f483.6b9a.4f77.fc0d.81f7.bcb0.1b7f.1b35.9168.64b9.476c.241c.e9fc.198b.d254.32ac] ~ ~ 625.000.000])
=input1 (input:tx:btc[[32 0xef51.e1b8.04cc.89d1.82d2.7965.5c3a.a89e.815b.1b30.9fe2.87d9.b2b5.5d57.b90e.c68a] 0 1 [4 0xffff.ffff] [22 0x14.1d0f.172a.0ecb.48ae.e1be.1f26.87d2.963a.e33f.71a1] ~ `[33 0x2.5476.c2e8.3188.368d.a1ff.3e29.2e7a.cafc.db35.66bb.0ad2.53f6.2fc7.0f07.aeee.6357] 600.000.000])

=output0 (output:tx:btc [[%bech32 'bc1qs2qtxl0n0rdenan0shy457p6w6k85m2e36f7ze'] 112.340.000])
=output1 (output:tx:btc [[%bech32 'bc1q800y9klw0exmu63pkt2sechszel64q2enddkt4'] 223.450.000])
=utx (unsigned:tx:btc [1 0x11 ~[input0 input1] ~[output0 output1]])

(~(sighash unsigned-tx:btc utx) 1)
:: gives [wid=32 dat=0xc37a.f311.16d1.b27c.af68.aae9.e3ac.82f1.4779.2901.4d5b.9176.57d0.eb49.478c.b670]
```

### Signing the above
Signing input index 1 (witness)
```
=ecc secp256k1:secp:crypto
=h dat:(~(sighash unsigned-tx:btc utx) 1)
=privkey 0x619c.3350.25c7.f401.2e55.6c2a.58b2.506e.30b8.511b.53ad.e95e.a316.fd8c.3286.feb9

`@ux`(compress-point:ecc (priv-to-pub:ecc privkey))
`[@ r=@ux s=@ux]`(ecdsa-raw-sign:ecc (@uvI h) privkey)
::  compress-point gives 0x2.5476.c2e8.3188.368d.a1ff.3e29.2e7a.cafc.db35.66bb.0ad2.53f6.2fc7.0f07.aeee.6357
::  pubkey gives
::  desired r of sig:
::  0x3609.e17b.84f6.a7d3.0c80.bfa6.10b5.b454.2f32.a8a0.d544.7a12.fb13.66d7.f01c.c44a
::
::  desired s of sig:
::  573a954c4518331561406f90300e8f3358f51928d43c212a8caed02de67eebee
```

Signing input index 0 (non-witness)
```
=h2 dat:(~(sighash unsigned-tx:btc utx) 0)
=privkey2 0xbbc2.7228.ddcb.9209.d7fd.6f36.b02f.7dfa.6252.af40.bb2f.1cbc.7a55.7da8.027f.f866
`[@ r=@ux s=@ux]`(ecdsa-raw-sign:ecc (@uvI h2) privkey2)

::  desired r;
::  0x8b9d.1dc2.6ba6.a9cb.6212.7b02.742f.a9d7.54cd.3beb.f337.f7a5.5d11.4c8e.5cdd.30be
```

### A sample legacy address-only transaction
```
=linput0 (input:tx:btc [[32 0xeccf.7e30.3418.9b85.1985.d871.f913.84b8.ee35.7cd4.7c30.2473.6e56.76eb.2deb.b3f2] 1 0 [4 0xffff.ffff] [25 0x76.a914.0109.6677.6006.953d.5567.439e.5e39.f86a.0d27.3bee.88ac] ~ ~ 100.000.000])
=loutput0 (output:tx:btc [[%legacy 0c1runeksijzfVxyrpiyCY2LCBvYsSiFsCm] 99.900.000])
=lutx (unsigned:tx:btc [1 0x0 ~[linput0] ~[loutput0]])

=lh dat:(~(sighash unsigned-tx:btc lutx) 0)
=lprivkey 0x18e1.4a7b.6a30.7f42.6a94.f811.4701.e7c8.e774.e7f9.a47e.2c20.35db.29a2.0632.1725
`[@ r=@ux s=@ux]`(ecdsa-raw-sign:ecc (@uvI lh) lprivkey)
```
0x970.7252.4438.d003.d23a.2f23.edb6.5aae.1bb3.e469

### BIP143 Reference
https://github.com/bitcoin/bips/blob/master/bip-0143.mediawiki#P2SHP2WPKH

## more scratch

