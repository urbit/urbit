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
=walt1 (from-xpub:bl xpub1 [%4 0x5566.7788] ~ ~ ~)
(~(mk-address wad:bl walt1 %0) 0)
::  gives [%bech32 'bc1q0adfj7ur750hjwufzzlfj5ytqzsnd7fz9fjuzc']
```

### update address data
```
=walt1 (from-xpub:bl xpub1 ~ ~ ~)
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

### DEPRECATED manual scanning of empty wallet
SEE Address Generation instead
Uses `btc-wallet-hook`, with max-gap=3
```
:btc-provider|command [%set-credentials api-url='http://localhost:50002']
:btc-wallet-hook|action [%set-provider ~zod]
=scan-xpub 'zpub6rFR7y4Q2AijBEqTUquhVz398htDFrtymD9xYYfG1m4wAcvPhXNfE3EfH1r1ADqtfSdVCToUG868RvUUkgDKf31mGDtKsAYz2oz2AGutZYs'
=fprint [%4 0xdead.beef]
:btc-wallet-store|action [%add-wallet scan-xpub fprint ~ [~ 3] [~ 6]]
:btc-wallet-store +dbug
:: shows scans with the xpub and {0 1 2} todos

::  %0 account has no used
=btc -build-file %/lib/btc/hoon
:btc-wallet-store|action [%address-info scan-xpub %0 1 *(set utxo:btc) used=%.n 655000]
:btc-wallet-store|action [%address-info scan-xpub %0 2 *(set utxo:btc) used=%.n 655000]
:btc-wallet-store|action [%address-info scan-xpub %0 0 *(set utxo:btc) used=%.n 655000]
::  dbug should give empty for scans: [xpub %0]
:btc-wallet-store|action [%address-info scan-xpub %1 2 *(set utxo:btc) used=%.n 655000]
:btc-wallet-store|action [%address-info scan-xpub %1 0 *(set utxo:btc) used=%.n 655000]
:btc-wallet-store|action [%address-info scan-xpub %1 1 *(set utxo:btc) used=%.y 655000]
:: dbug should show re-filled scans: [xpub %1]
```


## Address Generation Integration Test
All on `~zod`:
```
:btc-provider|command [%set-credentials api-url='http://localhost:50002']
:btc-wallet-hook|action [%set-provider ~zod]
=scan-xpub 'zpub6rFR7y4Q2AijBEqTUquhVz398htDFrtymD9xYYfG1m4wAcvPhXNfE3EfH1r1ADqtfSdVCToUG868RvUUkgDKf31mGDtKsAYz2oz2AGutZYs'
=fprint [%4 0xdead.beef]
:btc-wallet-store|action [%add-wallet scan-xpub fprint ~ [~ 3] [~ 6]]

:btc-wallet-store|action [%generate-address scan-xpub %0 meta=~]
```
:: make sure that the above address is watched


## Make a full TXBU
Provider is `~zod`, `~dopzod` is a client.  Use the xpub from PRIVATE.md to have a balance

### setup
On `~zod`:
```
|start %btc-provider
|start %btc-wallet-store
|start %btc-wallet-hook
:btc-provider|command [%set-credentials api-url='http://localhost:50002']
:btc-provider|command [%whitelist-clients `(set ship)`(sy ~[~dopzod])]
=xpubzod 'zpub6rFR7y4Q2AijBEqTUquhVz398htDFrtymD9xYYfG1m4wAcvPhXNfE3EfH1r1ADqtfSdVCToUG868RvUUkgDKf31mGDtKsAYz2oz2AGutZYs'
:btc-wallet-hook|action [%set-provider ~zod]
:btc-wallet-store|action [%add-wallet xpubzod ~ [~ 20] [~ 6]]
```

on `~dopzod`:
```
=xpubp PRIVATE.md

|start %btc-wallet-store
|start %btc-wallet-hook
:btc-wallet-hook|action [%set-provider ~zod]
:btc-wallet-store|action [%add-wallet xpubp ~ [~ 20] [~ 6]]
```

### request address
on `~dopzod`:
```
:btc-wallet-hook|action [%req-pay-address payee=~zod value=2.000 [~ 10]]
```


## scrys
```
.^((list @t) %gx /=btc-wallet-store=/scanned/noun)

.^(@ud %gx /=btc-wallet-store=/balance/[xpub]/noun)
```

