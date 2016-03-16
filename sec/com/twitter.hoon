::  Test url +https://api.twitter.com/1.1/account/verify_credentials.json
:: 
::::  /hoon/twitter/com/sec
  ::
/+    oauth1
!:
::::
  ::
=+  ^=  aut
    %^    oauth1
        'https://api.twitter.com/oauth/request_token'
      'https://api.twitter.com/oauth/authorize'
    'https://api.twitter.com/oauth/access_token'
|_  {(bale keys:oauth1) tok/token:oauth1}
++  aut  ~(. ^aut . +<- +<+)  :: XX electroplating
++  out  out-math:aut
++  in   in-oauth-token:aut
++  bak  (bak-save-access:aut save-token)
++  res  (res-handle-reqt:aut save-token)
++  save-token  |=(tok/token:aut +>(tok tok))
::++  wyp  ~
--
