## NOTE
The below requires norsyr's fix to `decompress-point` in order to work.

## Set Credentials and Ping Servers
```
:btc-bridge|command [%become-host [rpc-url='http://localhost:8332' rpc-user='__cookie__' rpc-password='2cce52a532a078764cd9e56630603adaedfbb130ccf3b2f0a5f0dd718a5c35e0'] [rpc-url='http://localhost:50002']]

:btc-bridge|rpc-action [%brpc %get-block-count ~]
:btc-bridge|rpc-action [%erpc %get-address-balance [%bech32 'bc1q59u5epktervh6fxqay2dlph0wxu9hjnx6v8n66']]
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
(encode-pubkey:bech32:btc %main pubkey)
::  should be [~ "bc1qw508d6qejxtdg4y5r3zarvary0c5xw7kv8f3t4"]

`[@ @ux]`(hash-160:btc pubkey)
::  gives [20 0x751e.76e8.1991.96d4.5494.1c45.d1b3.a323.f143.3bd6]

(encode-hash-160:bech32:btc %main (hash-160:btc pubkey))
:: gives [~ "bc1qw508d6qejxtdg4y5r3zarvary0c5xw7kv8f3t4"]
```

### Decode Bech32 to hex
Return val as `byts` to preserve leading zeros.
```
(decode-raw:bech32:btc "bc1qw508d6qejxtdg4y5r3zarvary0c5xw7kv8f3t4")
:: prints data: ~[0 14 20 15 7 13 26 0 25 18 6 11 13 8 21 4 20 3 17 2 29 3 12 29 3 4 15 24 20 6 14 30 22]

`[@ @ux]`(to-hex:bech32:btc "bc1qw508d6qejxtdg4y5r3zarvary0c5xw7kv8f3t4")
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
=btcs -build-file %/sur/btc/hoon
=input0 (input:tx:btcs [[32 0xfff7.f788.1a80.99af.a694.0d42.d1e7.f636.2bec.3817.1ea3.edf4.3354.1db4.e4ad.969f] 0 0 [4 0xeeff.ffff] [35 0x21.03c9.f483.6b9a.4f77.fc0d.81f7.bcb0.1b7f.1b35.9168.64b9.476c.241c.e9fc.198b.d254.32ac] ~ ~ 625.000.000])
=input1 (input:tx:btcs [[32 0xef51.e1b8.04cc.89d1.82d2.7965.5c3a.a89e.815b.1b30.9fe2.87d9.b2b5.5d57.b90e.c68a] 0 1 [4 0xffff.ffff] [22 0x14.1d0f.172a.0ecb.48ae.e1be.1f26.87d2.963a.e33f.71a1] ~ `[33 0x2.5476.c2e8.3188.368d.a1ff.3e29.2e7a.cafc.db35.66bb.0ad2.53f6.2fc7.0f07.aeee.6357] 600.000.000])

=output0 (output:tx:btcs [[%bech32 'bc1qs2qtxl0n0rdenan0shy457p6w6k85m2e36f7ze'] 112.340.000])
=output1 (output:tx:btcs [[%bech32 'bc1q800y9klw0exmu63pkt2sechszel64q2enddkt4'] 223.450.000])
=utx (unsigned:tx:btcs [~[input0 input1] ~[output0 output1]])
```

## cutting off the last 20 bytes (for bech32 address outputs):
```
`@ux`(end 3 2 0x14.6655)
::  gives 0x6655
```

### BIP143 Reference
https://github.com/bitcoin/bips/blob/master/bip-0143.mediawiki#P2SHP2WPKH

## more scratch
7f5a997b83f51f793b8910be99508b00a136f922
0x7f5a.997b.83f5.1f79.3b89.10be.9950.8b00.a136.f922

db6b1b20aa0fd7b23880be2ecbd4a98130974cf4748fb66092ac4d3ceb1a547701000000


0x46.c323.0000.0000

bip143 hash preimage:
0x100.0000.96b8.27c8.483d.4e9b.9671.2b67.13a7.b68d.6e80.03a7.81fe.ba36.c311.4347.0b4e.fd37.52b0.a642.eea2.fb7a.e638.c36f.6252.b675.0293.dbe5.74a8.0698.4b8e.4d85.4833.9a3b.ef51.e1b8.04cc.89d1.82d2.7965.5c3a.a89e.815b.1b30.9fe2.87d9.b2b5.5d57.b90e.c68a.0100.0000.1976.a914.1d0f.172a.0ecb.48ae.e1be.1f26.87d2.963a.e33f.71a1.88ac.0046.c323.0000.0000.ffff.ffff.863e.f3e1.a92a.fbfd.b97f.31ad.0fc7.683e.e943.e9ab.cf25.0159.0ff8.f655.1f47.e5e5.1100.0000.0100.0000


