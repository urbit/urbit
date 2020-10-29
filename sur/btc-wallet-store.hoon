::  wallets are compatible with BIPs 44, 49, and 84
::  m / purpose' / coin_type' / account' / change / address_index
::  change can be 0 or 1
::
/-  *btc
/+  bip32
|%
::  addi: address with metadata inside a change path
::  wach: map for watched addresses
::  chyg: stores the state of a change/non-change path
::  wilt: stores xpub; copulates with thousands of indices to form addresses
::  walt: wallet metadata
::
+$  chyg  $?(%0 %1)
+$  addi  [=chyg idx=@ used=? utxos=(set utxo)]
+$  wach  (map address addi)
+$  wilt  _bip32
+$  walt
  $:  ::  bipt: BIP44/49/84
      ::  ching: next index in non-change addresses
      ::  chang: next index in change addresses
      ::
      =wilt
      =bipt
      =wach
      scanned=?
      [ching=@ chang=@]
    ==
+$  action
  $%  [%add-wallet xpub=tape]
      [%update-address =address utxos=(set utxo)]
  ==
+$  update
  $%  [%address =address utxos=(set utxo)]
  ==
--
