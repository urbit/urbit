# btc-wallet-* Scratch Code

## xpub
Mnemonic
```
absurd sick rose mask magnet know slide spell rent casual someone grant giant inhale toward
```

```
=xpub1 'zpub6r8dKyWJ31XF6n69KKeEwLjVC5ruqAbiJ4QCqLsrV36Mvx9WEjUaiPNPGFLHNCCqgCdy6iZC8ZgHsm6a1AUTVBMVbKGemNcWFcwBGSjJKbD'
=xpub2 'xpub6D7yaZieZEeG617UcKXDhbsDeso6bmxSAiGWkvkASoiwcjaRtrH5HeNRnDT25s7zmxYzj6MtFe32dVqcf9YcBKKgn9THHjwn2uSjkvobK4e'
=bl -build-file %/lib/btc-wallet-store/hoon
```

### add
```
:btc-wallet-store|action [%add-wallet xpub ~ ~]
```

### get address at indices
```
=walt1 (from-xpub:walt:bl xpub1 ~ ~)
(get-address:walt1 %0 0)
```

### update address data
```
=walt1 (from-xpub:walt:bl xpub1 ~ ~)
(~(mk-address walt1 %0) 0)
::  gives bc1q0adfj7ur750hjwufzzlfj5ytqzsnd7fz9fjuzc
=walt1 (~(insert-address walt1 %0) [%bech32 'bc1q0adfj7ur750hjwufzzlfj5ytqzsnd7fz9fjuzc'] [%0 0 ~])
nixt.st.walt1
::  gives [p=1 q=0] (nixt updated since it was 0)
=walt1 (~(insert-address walt1 %0) [%bech32 'bc1qa5jcdww8u8493zttjjf6q5wu89e6knpvmsh7x4'] [%0 2 ~])
nixt.st.walt1
::  gives [p=1 q=0] (no update)
=walt1 (~(insert-address walt1 %0) [%bech32 'bc1qvqrdh8suyv63ntaa0d7hmcamavv8283sngh6e5'] [%0 1 ~])
nixt.st.walt1
::  gives [p=3 q=0]  (skips index 2, since already a used address there)

=walt1 (insert-address:walt1 [%bech32 'bc1qvqrdh8suyv63ntaa0d7hmcamavv8283sngh6e5'] [%0 4 ~])
::  gives error, because address is inserted at the wrong index
```

### generate new address (for receiving payment)
```
=walt1 (from-xpub:walt:bl xpub1 ~ ~)
=res ~(gen-address walt1 %0)
p.res
nixt.st.q.res
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
With max-gap=3
```
:btc-wallet-hook|action [%set-provider ~dopzod]
=btc -build-file %/lib/btc/hoon
=scan-xpub 'zpub6rFR7y4Q2AijBEqTUquhVz398htDFrtymD9xYYfG1m4wAcvPhXNfE3EfH1r1ADqtfSdVCToUG868RvUUkgDKf31mGDtKsAYz2oz2AGutZYs'
:btc-wallet-store|action [%add-wallet scan-xpub ~ [~ 3]]
:btc-wallet-store +dbug
:: shows scans with the xpub and {0 1 2} todos

::  %0 account has no used
:btc-wallet-store|action [%watch-address scan-xpub %0 1 *(set utxo:btc) used=%.n]
:btc-wallet-store|action [%watch-address scan-xpub %0 2 *(set utxo:btc) used=%.n]
:btc-wallet-store|action [%watch-address scan-xpub %0 0 *(set utxo:btc) used=%.n]
::  dbug should give empty for scans: [xpub %0]
:btc-wallet-store|action [%watch-address scan-xpub %1 2 *(set utxo:btc) used=%.n]
:btc-wallet-store|action [%watch-address scan-xpub %1 0 *(set utxo:btc) used=%.n]
:btc-wallet-store|action [%watch-address scan-xpub %1 1 *(set utxo:btc) used=%.y]
:: dbug should show re-filled scans: [xpub %1]
```

## Algos

### Scan addresses
#### in wallet-store
* maps:
  - scans ([xpub chyg] -> waltscan)
  
* start scan
  - params: xpub
  - get `nixt` from wallet
  - choose the next N indices WITHOUT generating
  - store their indexes in batch
  - new entry in scans for this xpub+both chyg
  - send address+ idx [xpub chyg] to wallet-hook for processing
  
* on `:watch-address` action
  - watch-address in the wallet IF used
  - delete `idx` from todo.batch
  - if the `todo`s in this xpub+chyg batch are empty, check whether wallet is scanned

#### in wallet-hook
* types
  - req-id=@ux: hash160 of (cat xpub chyg)
  - pend/fail: (req-id -> [=idx key=[xpub chyg]])
  - timeouts: (req-id -> @da) -- store Behns for each req

* send address-watch req
  - send address to provider with req-id
  - set a Behn for 30s, put in timeouts

* on response from server
  - insert the address into the wallet
  
* on error

* on timeout

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
