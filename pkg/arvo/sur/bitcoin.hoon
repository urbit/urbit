::  Types
::
|%
+$  xpub     tape
+$  network  ?(%main %regtest %testnet)
::
+$  bitcoin-action
  $%  [%add =xpub]
      [%remove ~]
      [%derive [=ship net=network]]
      [%request [=ship net=network]]
      [%receive @uc]
  ==
::
+$  bitcoin-update 
  $%  bitcoin-action
  ==
--
