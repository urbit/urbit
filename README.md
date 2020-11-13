# btc-agents - Bitcoin on Urbit

## Architecture
[Written up here](ARCH.md)

## Notes
Tested to work with `urbit/urbit` commit `d527b420580fb019db8aab397431180e6e6428eb`
Uses custom:
- `sys/zuse.hoon`
  * new `decompress-key` from [https://github.com/yosoyubik/urbit/blob/decompress-point/pkg/arvo/sys/zuse.hoon]
- `lib/bip32.hoon` from [https://github.com/urbit/urbit/blob/c473a4a35f2fdfde7b31f8b0ba5cbd7f54b0f223/pkg/arvo/lib/bip32.hoon]

## BTC and ElectRS requirements
- BTC fully sync'd node
- ElectRS with built index
- Node API server to handle provider requests

## Starting Up
First, install new zuse and bip32:
```
|mount %
|commit %home
::  should see gall molt with the new zuse
```

### Verify ECC and BIP32 Libs
Verify `ecc` has the correct decompress-point gate. The below should yield: `0x3.30d5.4fd0.dd42.0a6e.5f8d.3624.f5f3.482c.ae35.0f79.d5f0.753b.f5be.ef9c.2d91.af3c`
```
=bip32 -build-file %/lib/bip32/hoon
=ecc secp256k1:secp:crypto
=xpub "zpub6rFR7y4Q2AijBEqTUquhVz398htDFrtymD9xYYfG1m4wAcvPhXNfE3EfH1r1ADqtfSdVCToUG868RvUUkgDKf31mGDtKsAYz2oz2AGutZYs"
`@ux`(compress-point:ecc pub:(derive-public:(derive-public:(from-extended:bip32 xpub) 0) 0))
```

### Startup'
Set credentials and start agents
```
=provider $PROVIDER_@p
|start %btc-wallet-store
|start %btc-wallet-hook
|start %btc-provider
:btc-wallet-hook|action [%set-provider provider]
:btc-provider|command [%set-credentials api-url='http://localhost:50002']
```

### Test `%address-info` Calls
The below calls will print RPC results.
```
:btc-provider|action ['addr0' %address-info [%bech32 'bc1q59u5epktervh6fxqay2dlph0wxu9hjnx6v8n66']]
:btc-provider|action ['addr1' %address-info [%bech32 'bc1qlwd7mw33uea5m8r2lsnsrkc7gp2qynrxsfxpfm']]
:btc-provider|action ['addr2' %address-info [%bech32 'bc1qglkc9zfcn04vcc88nn0ljtxcpu5uxfznc3829k']]
::  first is an address w balance
::  second has no balance but is used
::  third is unused
```
