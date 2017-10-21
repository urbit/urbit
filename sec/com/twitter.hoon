::  Test url +https://api.twitter.com/1.1/account/verify_credentials.json
::
::::  /hoon/twitter/com/sec
  ::
/+    oauth1
::
::::
  ::
|_  {bal/(bale:eyre keys:oauth1) tok/token:oauth1}
::  ++aut is a "standard oauth1" core, which implements the
::  most common handling of oauth1 semantics. see lib/oauth1 for more details,
::  and examples at the bottom of the file.
++  aut  (~(standard oauth1 bal tok) . |=(tok/token:oauth1 +>(tok tok)))
++  filter-request
  %+  out-add-header:aut
    token-request='https://api.twitter.com/oauth/request_token'
  oauth-dialog='https://api.twitter.com/oauth/authorize'
::
++  filter-response  res-handle-request-token:aut
::
++  receive-auth-query-string
  %-  in-exchange-token:aut
  exchange-url='https://api.twitter.com/oauth/access_token'
::
++  receive-auth-response  bak-save-token:aut
:: ++  discard-state  ~
--
