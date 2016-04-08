::  Test url +https://api.twitter.com/1.1/account/verify_credentials.json
::
::::  /hoon/twitter/com/sec
  ::
/+    oauth1
!:
::::
  ::
|_  {bal/(bale keys:oauth1) tok/token:oauth1}
::  ++aut is a "standard oauth1" core, which implements the
::  most common handling of oauth1 semantics. see lib/oauth1 for more details,
::  and examples at the bottom of the file.
++  aut  (~(standard oauth1 bal tok) . |=(tok/token:oauth1 +>(tok tok)))
++  out
  %+  out-add-header:aut
    token-request='https://api.twitter.com/oauth/request_token'
  oauth-dialog='https://api.twitter.com/oauth/authorize'
::
++  res  res-handle-request-token:aut
::
++  in
  %-  in-exchange-token:aut
  exchange-url='https://api.twitter.com/oauth/access_token'
::
++  bak  bak-save-token:aut
:: ++  wyp  ~
--
