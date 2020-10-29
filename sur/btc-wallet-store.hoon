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
+$  addi  [idx=@ used=? utxos=(set utxo)]
+$  wach  (map address addi)
+$  chyg  [scanned=? next=@ =wach]
+$  wilt  _bip32
+$  walt
  $:  ::  bipt: BIP44/49/84
      ::  ching: non-change addresses
      ::  chang: change addresses
      ::
      =wilt
      =bipt
      ching=chyg
      chang=chyg
  ==
+$  action
  $%  [%add-wallet xpub=tape]
  ==
--
