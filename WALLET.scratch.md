# btc-wallet-* Scratch Code

## xpub
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

## Algos

### Scan addresses
* types
  - req-id=@ux: hash160 of (cat xpub chyg)
* maps:
  - scans ([xpub chyg] -> waltscan)
  - pend/fail: (req-id -> [=idx key=[xpub chyg]])
  - timeouts: (req-id -> @da) -- store Behns for each req

* send address-watch req
  - send address to provider with req-id
  - set a Behn for 30s, put in timeouts

* on response from server
  - check whether idx in `scanning`--ignore if not (old response)
  - insert the address into the wallet **if it's used**
  - if used, update `has-used` for this xpub to be true
  - delete idx from `scanning` jug
  - check whether scanning is now empty. If it is, check whether has-used is true
  
* on error

* on timeout


::
++  send-address-update
  |=  [xpub=tape =walt a=address:btc us=(set utxo)]
  ^-  (quip card _state)
  :_  state(walts (~(put by walts.state) xpub walt))
  ~[[%give %fact ~[/wallets] %btc-wallet-store-update !>([%address a us])]]
::
