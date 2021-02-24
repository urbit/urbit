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

Do `./install.sh -w PIER_DIR` for dev work.  `PIER_DIR` is just where your ship is located (**without** `/home`).  E.g. 

```
./install.sh -w ~/urbit/ships/timluc-miptev
```

### Startup
Set credentials and start agents. Use `~dopzod` and `~zod`
```
=provider ~zod
=network %main
|start %btc-wallet
|start %btc-provider
:btc-wallet|command [%set-provider provider]
:btc-provider|command [%set-credentials api-url='http://localhost:50002' network]
```

### Adding a wallet
Uses a dummy fingerprint

```
=fprint [%4 0xbeef.dead]
=xpubmain 'zpub6rFR7y4Q2AijBEqTUquhVz398htDFrtymD9xYYfG1m4wAcvPhXNfE3EfH1r1ADqtfSdVCToUG868RvUUkgDKf31mGDtKsAYz2oz2AGutZYs'
:btc-wallet|command [%add-wallet xpubmain fprint ~ [~ 8] [~ 1]]
```

## Running Tests
```
|commit %home
-test %/tests/lib/bip/b158 ~
-test %/tests/lib/bip/b174 ~
-test %/tests/lib/btc ~
-test %/tests/lib/bitcoin ~
```

