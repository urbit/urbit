# btc-wallet-* Scratch Code

## xpub
xpub1 is from mnemonic from PRIVATE.scratch
```
=xpub1 'zpub6r8dKyWJ31XF6n69KKeEwLjVC5ruqAbiJ4QCqLsrV36Mvx9WEjUaiPNPGFLHNCCqgCdy6iZC8ZgHsm6a1AUTVBMVbKGemNcWFcwBGSjJKbD'
=xpub2 'xpub6D7yaZieZEeG617UcKXDhbsDeso6bmxSAiGWkvkASoiwcjaRtrH5HeNRnDT25s7zmxYzj6MtFe32dVqcf9YcBKKgn9THHjwn2uSjkvobK4e'
=bl -build-file %/lib/btc-wallet-store/hoon
```

### get address at indices
```
=walt1 (from-xpub:bl xpub1 ~ ~)
(~(mk-address wad:bl walt1 %0) 0)
::  gives [%bech32 'bc1q0adfj7ur750hjwufzzlfj5ytqzsnd7fz9fjuzc']
```

### update address data
```
=walt1 (from-xpub:bl xpub1 ~ ~)
(~(mk-address wad:bl walt1 %0) 0)
::  gives bc1q0adfj7ur750hjwufzzlfj5ytqzsnd7fz9fjuzc
=walt1 (~(watch-address wad:bl walt1 %0) [%bech32 'bc1q0adfj7ur750hjwufzzlfj5ytqzsnd7fz9fjuzc'] [%0 0 ~])
nixt.walt1
::  gives [p=1 q=0] (nixt updated since it was 0)
=walt1 (~(watch-address wad:bl walt1 %0) [%bech32 'bc1qa5jcdww8u8493zttjjf6q5wu89e6knpvmsh7x4'] [%0 2 ~])
nixt.walt1=walt1 (~(watch-address wad:bl walt1 %0) [%bech32 'bc1qa5jcdww8u8493zttjjf6q5wu89e6knpvmsh7x4'] [%0 2 ~])
nixt.walt1
::  gives [p=1 q=0] (no update)
=walt1 (~(watch-address wad:bl walt1 %0) [%bech32 'bc1qvqrdh8suyv63ntaa0d7hmcamavv8283sngh6e5'] [%0 1 ~])
nixt.walt1
::  gives [p=3 q=0]  (skips index 2, since already a used address there)

=walt1 (~(watch-address wad:bl walt1 %0) [%bech32 'bc1qvqrdh8suyv63ntaa0d7hmcamavv8283sngh6e5'] [%0 4 ~])
::  gives error, because address is inserted at index that doesn't match it
```

### generate new address (for receiving payment)
```
=walt1 (from-xpub:bl xpub1 ~ ~)
=res ~(gen-address wad:bl walt1 %0)
p.res
nixt.q.res
::  gives 
::  [%bech32 'bc1q0adfj7ur750hjwufzzlfj5ytqzsnd7fz9fjuzc']
::  [p=1 q=0]
```

## Scanning
Mnemonic
```
abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about
```

### manual scanning of empty wallet
Uses `btc-wallet-hook`, with max-gap=3
```
:btc-provider|command [%set-credentials api-url='http://localhost:50002']
:btc-wallet-hook|action [%set-provider ~dopzod]
=scan-xpub 'zpub6rFR7y4Q2AijBEqTUquhVz398htDFrtymD9xYYfG1m4wAcvPhXNfE3EfH1r1ADqtfSdVCToUG868RvUUkgDKf31mGDtKsAYz2oz2AGutZYs'
:btc-wallet-store|action [%add-wallet scan-xpub ~ [~ 3] [~ 6]]
:btc-wallet-store +dbug
:: shows scans with the xpub and {0 1 2} todos

::  %0 account has no used
=btc -build-file %/lib/btc/hoon
:btc-wallet-store|action [%watch-address scan-xpub %0 1 *(set utxo:btc) used=%.n]
:btc-wallet-store|action [%watch-address scan-xpub %0 2 *(set utxo:btc) used=%.n]
:btc-wallet-store|action [%watch-address scan-xpub %0 0 *(set utxo:btc) used=%.n]
::  dbug should give empty for scans: [xpub %0]
:btc-wallet-store|action [%watch-address scan-xpub %1 2 *(set utxo:btc) used=%.n]
:btc-wallet-store|action [%watch-address scan-xpub %1 0 *(set utxo:btc) used=%.n]
:btc-wallet-store|action [%watch-address scan-xpub %1 1 *(set utxo:btc) used=%.y]
:: dbug should show re-filled scans: [xpub %1]
```

### utxo selection
Creates dummy inputs and outputs. Builds a TX with them.
```
=btc -build-file %/lib/btc/hoon
=bwsl -build-file %/lib/btc-wallet-store/hoon
=u (utxo:btc [pos=0 (hash256:btc [wid=32 dat=0xc493.f6f1.4668.5f76.b44f.0c77.ca88.120c.b8bc.89f5.34fe.69b6.8288.27b9.74e6.8849]) height=3 value=0])
=val0 200.000
=val1 500.000
=val2 30
=val3 235.000

=eny1 0v3uc.iuebi.5qilc.l8d87.c1k6n.7iksq.nkobs.8s5he.raq40.9ff0b.5tj3u.kjtg7.aq59e.hatv7.oioam.mlsr4.pqqcd.cnbjn.pnpi2.1m5rt.k4scg
=eny2 0v1gt.mc4ca.lfs0m.q1dal.lqobu.mmlbd.2umnp.lj9dr.4pf4s.pvclr.dps96.4a6i8.rt6n9.krp0r.11kqu.ckqe4.1tmat.gr754.463aj.a4b41.jj7qg
=inputs ~[[u(value val0) %0 0] [u(value val1) %0 2] [u(value val2) %0 1] [u(value val3) %1 2]]
=outputs ~[[[%bech32 'bc1q59u5epktervh6fxqay2dlph0wxu9hjnx6v8n66'] value=200.100] [[%bech32 'bc1qlwd7mw33uea5m8r2lsnsrkc7gp2qynrxsfxpfm'] value=200.000]]
=w *walt:bwsl
=w w(bipt %bip84)

(~(single-random-draw sut:bwsl [w eny1 100 outputs]) inputs)
(~(single-random-draw sut:bwsl [w eny2 100 outputs]) inputs)

```
Above tests w 2 outputs, total fees with 2 inputs of 27.500. Gives:
1. 500.000 input
2. Inputs 0 and 3


## scrys
```
.^((list @t) %gx /=btc-wallet-store=/scanned/noun)

.^(@ud %gx /=btc-wallet-store=/balance/[xpub]/noun)
```

## Algos

### Monitor addresses
- nixt also stores next 50 addresses for each account.
- every update-address call also checks those

### make a payment
* make payment
  - get address
  - on-agent gets return value, construct tx
  - store tx (view or other command can get it)

::
++  send-address-update
  |=  [xpub=tape =walt a=address:btc us=(set utxo)]
  ^-  (quip card _state)
  :_  state(walts (~(put by walts.state) xpub walt))
  ~[[%give %fact ~[/wallets] %btc-wallet-store-update !>([%address a us])]]
::
