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
::  chyg: whether account is (non-)change. 0 or 1
::  nixt: next indices to generate addresses from (non-change/change)
::  addi: address with metadata inside a change path
::  wach: map for watched addresses.
::        Membership implies the address is known by outside parties or had prior activity
::  scon: indices to initially scan to in (non-)change accounts
::        defaults to 2^32-1 (i.e. all the addresses, ~4B)
::  wilt: stores xpub; copulates with thousands of indices to form addresses
::  walt: wallet metadata
::
+$  chyg  $?(%0 %1)
+$  idx   @
+$  nixt  (pair idx idx)
+$  addi  [=chyg =idx utxos=(set utxo)]
+$  wach  (map address addi)
+$  scon  $~([max-index max-index] (pair idx idx))
+$  wilt  _bip32
::  scanning: maps xpub+chyg to set of indices.
::            once it's full, we can check whether address in it were blank or not
::  seen:     holds addresses whose scan results we've seen already
::
+$  scanning  (jug cord idx)
+$  has-used  (map cord ?)
::
+$  action
  $%  [%add-wallet =xpub scan-to=(unit scon) max-gap=(unit @)]
      [%update-address a=address utxos=(set utxo)]
  ==
+$  update
  $%  [%address a=address utxos=(set utxo)]
  ==
--
