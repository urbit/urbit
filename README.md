# btc-agents - Bitcoin on Urbit

## BTC and ElectRS requirements
Utilities located [here](https://github.com/timlucmiptev/urbit-bitcoin-rpc). Use the provided `mainnet.sh` and `testnet.sh` scripts.
- BTC fully sync'd node
- ElectRS with built index
- Node API server to handle provider requests

## Notes
- wallets can be on testnet or mainnet
- if a wallet is on a different network than the provider, its addresses will not update on new provider blocks being sent.

## Future Upgrades
- Block filters
- Better UTXO Selection

## Starting Up

Do `./install.sh -w PIER_DIR` for dev work.

### Startup'
Set credentials and start agents. Use `~dopzod` and `~zod`
```
=provider ~zod
=network %main
|start %btc-wallet
|start %btc-provider
:btc-wallet|command [%set-provider provider network]
:btc-provider|command [%set-credentials api-url='http://localhost:50002' network]
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
-test %/tests/lib/bip/b158 ~
-test %/tests/lib/bip/b174 ~
-test %/tests/lib/btc-wallet ~
-test %/tests/lib/bitcoin ~
```

