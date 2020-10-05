# btc-agents - Bitcoin on Urbit

Uses the fork [btc-urbit](https://github.com/timlucmiptev/btc-urbit), with custom:
- `sys/zuse.hoon`
  * new `decompress-key` from [https://github.com/yosoyubik/urbit/blob/decompress-point/pkg/arvo/sys/zuse.hoon]
- `lib/bip32.hoon` from [https://github.com/urbit/urbit/blob/c473a4a35f2fdfde7b31f8b0ba5cbd7f54b0f223/pkg/arvo/lib/bip32.hoon]

## `btc-node` Patch
Includes a custom version of `btc-node` that supports access control and returning responses to a local subscriber.

### Changes


### New Commands
```
::  send results of this RPC call to subscribers, if watched
:btc-node-hook|command [%watch %get-block-count]
:btc-node-hook|command [%unwatch %get-block-count]

::  send a message to subscribers as to whether the external node is live
:btc-node-hook|command [%ping ~]
```

## Bech32 Support
Commit the files, and then run:
```
=btc -build-file %/lib/btc/hoon
=pubkey 0x2.79be.667e.f9dc.bbac.55a0.6295.ce87.0b07.029b.fcdb.2dce.28d9.59f2.815b.16f8.1798
(hash-160:btc pubkey)
=result (encode-pubkey:bech32:btc %main pubkey)
```

