/-  *btc, bws=btc-wallet-store, bp=btc-provider
|%
::  req-id: hash of [xpub chyg idx]
::  addo: wallet index to send result of request to
::  back: lookup table of req-id -> addo
::
+$  addo  [=xpub =chyg:bws =idx:bws]
+$  back  (map req-id:bp addo)
+$  action
  $%  [%set-provider provider=ship]
  ==
--
