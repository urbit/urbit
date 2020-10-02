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
:btc-node-hook|command [%watch %get-block-count]
:btc-node-hook|command [%unwatch %get-block-count]
```

## Data Flow

