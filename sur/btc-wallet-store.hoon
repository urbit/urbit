::  wallets are compatible with BIPs 44, 49, and 84
::  m / purpose' / coin_type' / account' / change / address_index
::  change can be 0 or 1
::
::  TODO: explain state tracking (used maps, next unused idx)
::
/-  *btc
/+  bip32
|%
++  max-index  (dec (pow 2 32))
::  chyg: whether account is non-change/change
::  idx:  path of [change address_index]
::  idxs: pair of indices (non-change/change)
::  addi: address with metadata inside a change path
::  wach: map for watched addresses
::  scon: indices to initially scan to in non-change/change accounts respectively
::        defaults to 2^32-1 (i.e. all the addresses, ~4B)
::  wilt: stores xpub; copulates with thousands of indices to form addresses
::  walt: wallet metadata
::
+$  chyg  $?(%0 %1)
+$  idx   (pair chyg @u)
+$  idxs  (pair idx idx)
+$  addi  [=idx used=? utxos=(set utxo)]
+$  wach  (map address addi)
+$  scon  $~([[%0 max-index] [%1 max-index]] idxs)
+$  wilt  _bip32
+$  action
  $%  [%add-wallet =xpub scan-to=(unit scon) max-gap=(unit @u)]
      [%update-address a=address utxos=(set utxo)]
  ==
+$  update
  $%  [%address a=address utxos=(set utxo)]
  ==
--
