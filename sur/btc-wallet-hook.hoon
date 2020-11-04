/-  *btc, bws=btc-wallet-store
|%
::  req-id: hash of [xpub chyg idx]
::  addo: wallet index to send result of request to
::  back: lookup table of req-id -> addo
::
+$  req-id  @ux
+$  addo  [=xpub =chyg:bws =idx:bws]
+$  back  (map req-id addo)
+$  action
  $%  [%set-provider provider=ship]
  ==
--
