# btc-agents - Bitcoin on Urbit

## Architecture
[Written up here](ARCH.md)

## Notes
Tested to work with `urbit/urbit` commit `f37bf826848625450819e6ab0848bab1a07c1404

## BTC and ElectRS requirements
- BTC fully sync'd node
- ElectRS with built index
- Node API server to handle provider requests

## Starting Up

Do `./install.sh -w PIER_DIR` for dev work.

### Startup'
Set credentials and start agents. Use `~dopzod` and `~zod`
```
=provider ~zod
|start %btc-wallet-store
|start %btc-wallet-hook
|start %btc-provider
:btc-wallet-hook|command [%set-provider provider]
:btc-provider|command [%set-credentials api-url='http://localhost:50002']
```

### Test `%address-info` Calls
The below calls will print RPC results.
```
:btc-provider|action ['addr0' %address-info [%bech32 'bc1qm7cegwfd0pvv9ypvz5nhstage00xkxevtrpshc']]
:btc-provider|action ['addr1' %address-info [%bech32 'bc1qlwd7mw33uea5m8r2lsnsrkc7gp2qynrxsfxpfm']]
:btc-provider|action ['addr2' %address-info [%bech32 'bc1qglkc9zfcn04vcc88nn0ljtxcpu5uxfznc3829k']]
::  first is an address w balance
::  second has no balance but is used
::  third is unused

```

## Running Tests
```
|commit %home
-test %/tests/lib/bip158 ~
-test %/tests/lib/btc-wallet-store ~
-test %/tests/lib/btc ~
```

