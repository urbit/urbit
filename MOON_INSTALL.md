# Installing on a Moon
Requires base hash at least: `rd3oe`

## Create Moon
In your Urbit:
```
|moon
```
Copy the key and note the moon name.

## Install New `zuse.hoon`
```
./urbit -w $MOON_NAME -G $COPIED_KEY
```
The moon will compile and apply OTAs. After that is done, run:
```
|mount %
```

Back outside:
```
cd $BTC_AGENTS_DIR
./install-zuse.sh $MOON_PIER
```

In moon:
```
|commit %home
|reset
```

Install the rest of the files:
```
./install.sh $MOON_PIER
```

The kernel will recompile. Then test that the new `decompress-point` is included.
The below should yield: `0x3.30d5.4fd0.dd42.0a6e.5f8d.3624.f5f3.482c.ae35.0f79.d5f0.753b.f5be.ef9c.2d91.af3c`
```
=bip32 -build-file %/lib/bip32/hoon
=ecc secp256k1:secp:crypto
=xpub "zpub6rFR7y4Q2AijBEqTUquhVz398htDFrtymD9xYYfG1m4wAcvPhXNfE3EfH1r1ADqtfSdVCToUG868RvUUkgDKf31mGDtKsAYz2oz2AGutZYs"
`@ux`(compress-point:ecc pub:(derive-public:(derive-public:(from-extended:bip32 xpub) 0) 0))
```

## Start `btc-provider`
```
:btc-provider|command [%set-credentials api-url='http://localhost:50002']
```
