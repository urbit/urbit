::  wallets are compatible with BIPs 44, 49, and 84
::  m / purpose' / coin_type' / account' / change / address_index
::  change can be 0 or 1
::
/-  *btc
/+  bip32
|%
++  max-index  (dec (pow 2 32))
::  chyg: whether account is non-change/change
::  idxs: pair of indices (non-change/change)
::  addi: address with metadata inside a change path
::  wach: map for watched addresses
::  scon: indices to initially scan to in non-change/change accounts respectively
::        defaults to 2^32-1 (i.e. all the addresses, ~4B)
::  wilt: stores xpub; copulates with thousands of indices to form addresses
::  walt: wallet metadata
::
+$  chyg  $?(%0 %1)
+$  idxs  (pair @u @u)
+$  addi  [=chyg idx=@u used=? utxos=(set utxo)]
+$  wach  (map address addi)
+$  scon  $~([max-index max-index] idxs)
+$  wilt  _bip32
+$  action 
  $%  [%add-wallet xpub=tape scan-to=(unit scon) max-gap=(unit @u)]
      [%update-address a=address utxos=(set utxo)]
  ==
+$  update
  $%  [%address a=address utxos=(set utxo)]
  ==
--
