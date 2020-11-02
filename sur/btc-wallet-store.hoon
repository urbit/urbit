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
::  idx:  an address_index
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
::  todo: Set of indices; empty it out until none are left--means scanning of that batch is done
::  start:     index this batch started scanning from
::
+$  scan-batch  [todo=(set idx) start=idx]
+$  scans  (map [xpub chyg] scan-batch)
::
::  %add-wallet: add wallet to state and initiate a scan
::  %scan: start a scan of the next address batch in a wallet
::         if the scan is complete, update the wallet and remove from scans
::  %watch-address: watch an address if used, remove from scans batch if not
::  %update-address: update info of an address if we're watching it
::
+$  action
  $%  [%add-wallet =xpub scan-to=(unit scon) max-gap=(unit @)]
      [%scan =xpub]
      [%watch-address =xpub =chyg =idx utxos=(set utxo) used=?]
      [%update-address a=address utxos=(set utxo)]
  ==
::
+$  update
  $%  [%address a=address utxos=(set utxo)]
  ==
--
