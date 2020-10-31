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
(mk-address:walt1 %0 0)
::  gives bc1q0adfj7ur750hjwufzzlfj5ytqzsnd7fz9fjuzc
=walt1 (insert-address:walt1 [%bech32 'bc1q0adfj7ur750hjwufzzlfj5ytqzsnd7fz9fjuzc'] [%0 0 ~])
nixt:walt1
::  gives [p=1 q=0] (nixt updated since it was 0)
=walt1 (insert-address:walt1 [%bech32 'bc1qa5jcdww8u8493zttjjf6q5wu89e6knpvmsh7x4'] [%0 2 ~])
nixt:walt1
::  gives [p=1 q=0] (no update)
=walt1 (insert-address:walt1 [%bech32 'bc1qvqrdh8suyv63ntaa0d7hmcamavv8283sngh6e5'] [%0 1 ~])
nixt:walt1
::  gives [p=3 q=0]  (skips index 2, since already a used address there)
```

## Algos

Scan addresses. (map xpub (pair list list))
outgoing wire is /scan/xpub/change/idx
- keep (jug cord idx) of xpub+chyg -> idx
- keep (map cord ?) of xpub+chyg-> has-used?
- every time we get a response
  - check whether idx in `scanning`--ignore if not (old response)
  - insert the address into the wallet **if it's used**
  - if used, update `has-used` for this xpub to be true
  - delete idx from `scanning` jug
  - check whether scanning is now empty. If it is, check whether has-used is true

## scratch code, refactor
++  update-utxos
  |=  [a=address:btc us=(set utxo)]
  ^-  (quip card _state)
  =/  xpubs=(list tape)
    %~  tap  in
    ~(key by walts.state)
  |-  ?~  xpubs  `state
  =/  w=walt  (~(got by walts.state) i.xpubs)
  ?:  (~(has by wach.w) a)
    %:  send-address-update
        i.xpubs
        (update-wallet w a us)
        a
        us
    ==
  $(xpubs t.xpubs)
::
++  send-address-update
  |=  [xpub=tape =walt a=address:btc us=(set utxo)]
  ^-  (quip card _state)
  :_  state(walts (~(put by walts.state) xpub walt))
  ~[[%give %fact ~[/wallets] %btc-wallet-store-update !>([%address a us])]]
::
