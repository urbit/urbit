# Bitcoin Demo/Integration Test
Dec 9, 2020
Can use as the base of an integration test.

## Start Services
Runs the full node API services.
```
./local-start.sh
```

## Start Agents
On `~zod`
```
|commit %home
|start %btc-provider
|start %btc-wallet-hook
|start %btc-wallet-store

:btc-provider|command [%set-credentials api-url='http://localhost:50002']
:btc-wallet-hook|action [%set-provider ~zod]
:btc-provider|command [%whitelist-clients `(set ship)`(sy ~[~dopzod])]
```

On `~dopzod`
```
|commit %home
|start %btc-wallet-hook
|start %btc-wallet-store
:btc-wallet-hook|action [%set-provider ~zod]
```

### Add Wallets
`~dopzod`
XPUB is the "absurd sick..." mnemonic
```
=xpubp 'zpub6r8dKyWJ31XF6n69KKeEwLjVC5ruqAbiJ4QCqLsrV36Mvx9WEjUaiPNPGFLHNCCqgCdy6iZC8ZgHsm6a1AUTVBMVbKGemNcWFcwBGSjJKbD'
=fprint [%4 0xbeef.dead]
:btc-wallet-store|action [%add-wallet xpubp fprint ~ [~ 8] [~ 6]]
```

`~zod`
```
=xpubzod 'zpub6rFR7y4Q2AijBEqTUquhVz398htDFrtymD9xYYfG1m4wAcvPhXNfE3EfH1r1ADqtfSdVCToUG868RvUUkgDKf31mGDtKsAYz2oz2AGutZYs'
=fprint [%4 0xbeef.dead]
:btc-wallet-store|action [%add-wallet xpubzod fprint ~ [~ 8] [~ 6]]
```

## Check Balance
`~dopzod`
```
.^(@ud %gx /=btc-wallet-store=/balance/[xpubp]/noun)
```

## Pay a Ship
`~dopzod` will pay `~zod`. Both are acting as clients here (and use `~zod` as the provider).

`~dopzod`
```
:btc-wallet-hook|action [%req-pay-address ~zod 3.000 feyb=[~ 30]]
```

### Check State on ~zod/~dopzod
`~dopzod`: outgoing
```
:btc-wallet-hook +dbug [%state 'poym']
```

`~zod`: incoming
```
:btc-wallet-hook +dbug [%state 'piym']
```

### Idempotent
`~dopzod`
```
:btc-wallet-hook|action [%req-pay-address ~zod 3.000 feyb=[~ 100]]
```
Or can change amount:
```
:btc-wallet-hook|action [%req-pay-address ~zod 3.000 feyb=[~ 100]]
```


## Scan a Real Xpub
```
=realxpub 'zpub6qvniDfrk9sRxz7H9Cbr8fccuGNd4RGMmifPVvbQtqtsG7VwCUrNsnNt8DiCH8kxh3vsDuJkfNqZQspVq2xEbE64fgXT5hVJiD8WkRhvuJc'
=fprint [%4 0xc93d.865c]
:btc-wallet-store|action [%add-wallet realxpub fprint ~ [~ 6] [~ 6]]

.^(@ud %gx /=btc-wallet-store=/balance/[realxpub]/noun)
```
