# Bitcoin Demo/Integration Test
Dec 9, 2020
Can use as the base of an integration test.

## Start Services
Runs the full node API services.
```
./local-start.sh
```

## Start Agents and set XPUBs
On `~zod`. Uses "abandon abandon..." mnemonic
```
|commit %home
|start %btc-provider
|start %btc-wallet

:btc-provider|command [%set-credentials api-url='http://localhost:50002' %main]
:btc-wallet|command [%set-provider ~zod %main]
:btc-provider|command [%add-whitelist %users `(set ship)`(sy ~[~dopzod])]

=fprint [%4 0xbeef.dead]
=xpubmain 'zpub6rFR7y4Q2AijBEqTUquhVz398htDFrtymD9xYYfG1m4wAcvPhXNfE3EfH1r1ADqtfSdVCToUG868RvUUkgDKf31mGDtKsAYz2oz2AGutZYs'
=xpubtest 'vpub5Y6cjg78GGuNLsaPhmYsiw4gYX3HoQiRBiSwDaBXKUafCt9bNwWQiitDk5VZ5BVxYnQdwoTyXSs2JHRPAgjAvtbBrf8ZhDYe2jWAqvZVnsc'
```

On `~dopzod`. Uses "absurd sick..." mnemonic from PRIVATE.scratch.md
```
|commit %home
|start %btc-wallet

:btc-wallet|command [%set-provider ~zod %main]

=fprint [%4 0xdead.beef]
=xpubmain 'zpub6r8dKyWJ31XF6n69KKeEwLjVC5ruqAbiJ4QCqLsrV36Mvx9WEjUaiPNPGFLHNCCqgCdy6iZC8ZgHsm6a1AUTVBMVbKGemNcWFcwBGSjJKbD'
=xpubtest 'vpub5ZpY66FvUHYMSST9uWoUSFYBYxGx3aSFfQP8qg2HqGUuL8k9ReiWuKxSKZBwFmBKug8YStuGTmxsnL8ySc9dfPJQdJTM4dYAZcgJhSfRWKL'
```

### Add Wallets
On both `~zod`/`dopzod`, choose depending on whether you're on test or main
```
:btc-wallet|command [%add-wallet xpubmain fprint ~ [~ 8] [~ 6]]

:btc-wallet|command [%add-wallet xpubtest fprint ~ [~ 8] [~ 6]]
```

## Check Balance
`~dopzod`
```
.^((unit @ud) %gx /=btc-wallet=/balance/[xpubmain]/noun)

.^((unit @ud) %gx /=btc-wallet=/balance/[xpubtest]/noun)
```

## Pay a Ship
`~dopzod` will pay `~zod`. Both are acting as clients here (and use `~zod` as the provider).

`~dopzod`
```
:btc-wallet-hook|command [%req-pay-address ~zod 30.000 feyb=10
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
:btc-wallet-hook|command [%req-pay-address ~zod 3.000 feyb=100
```
Or can change amount:
```
:btc-wallet-hook|command [%req-pay-address ~zod 3.000 feyb=100
```

### Broadcast the Signed TX
```
:btc-wallet-hook|command [%broadcast-tx tx]
```


## Scan a Real Xpub
```
=realxpub 'zpub6qvniDfrk9sRxz7H9Cbr8fccuGNd4RGMmifPVvbQtqtsG7VwCUrNsnNt8DiCH8kxh3vsDuJkfNqZQspVq2xEbE64fgXT5hVJiD8WkRhvuJc'
=fprint [%4 0xc93d.865c]
:btc-wallet|command [%add-wallet realxpub fprint ~ [~ 6] [~ 6]]

.^(@ud %gx /=btc-wallet=/balance/[realxpub]/noun)
```
