::  wallets are compatible with BIPs 44, 49, and 84
::  m / purpose' / coin_type' / account' / change / address_index
::  change can be 0 or 1
::
/-  *btc
/+  bip32
|%
::  chyg: stores the state of a change/non-change path
::  addi: address with metadata inside a change path
::  wach: map for watched addresses
::  scon: indices to initially scan to in non-change/change accounts respectively
::        defaults to 2^32-1 (i.e. all the addresses)
::  wilt: stores xpub; copulates with thousands of indices to form addresses
::  walt: wallet metadata
::
+$  chyg  $?(%0 %1)
+$  addi  [=chyg idx=@ used=? utxos=(set utxo)]
+$  wach  (map address addi)
++  max-index  (dec (pow 2 32))
+$  scon  $~([max-index max-index] (pair @ @))
+$  wilt  _bip32
+$  walt
  $:  ::  bipt: BIP44/49/84
      ::  next: next index to generate address for in non-change/change accounts respectively
      ::  scanned: whether the wallet's addresses have been checked for prior activity
      ::           if unscanned, 'next' values won't be valid
      ::
      =wilt
      =bipt
      =wach
      next=(pair @ @)
      scanned=?
      scan-to=scon
      max-gap=@
   ==
+$  action
  $%  [%add-wallet xpub=tape scan-to=(unit scon) max-gap=(unit @)]
      [%update-address =address utxos=(set utxo)]
  ==
+$  update
  $%  [%address =address utxos=(set utxo)]
  ==
--
