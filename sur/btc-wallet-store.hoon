::  wallets are compatible with BIPs 44, 49, and 84
::  m / purpose' / coin_type' / account' / change / address_index
::
/-  *btc, bip32
|%
::  wilt: copulates with thousands of indices to form addresses
::  walt: wallet metadata
::
+$  wilt  _bip32
+$  walt
  $:  ::  scanned: whether we've checked all addresses for prior activity
      ::  ching: non-change addresses
      ::  chang: change addresses
      ::  watch: address -> [change address_index]
      ::
      =wilt
      scanned=?
      ching=((mop @ address-info) gth)
      chang=((mop @ address-info) gth)
      watch=(map address [change=@ index=@])
  ==
--
