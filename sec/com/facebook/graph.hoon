::  Test url +https://graph.facebook.com/v2.5/me
::
::::  /hoon/graph/facebook/com/sec
  ::
/+    oauth2
::
::::
  ::
=+  ^=  aut
    %+   oauth2
      dialog='https://www.facebook.com/dialog/oauth?response_type=code'
    exchange='https://graph.facebook.com/v2.3/oauth/access_token'
|_  {bal/(bale keys.aut) access-token/token.aut}
++  auth  ~(. aut bal /'user_about_me'/'user_posts')
++  out  (out-quay:auth key='access_token' value=access-token)
++  in   in-code:auth
++  bak
  %-  (bak-parse:auth . access-token.aut expires-in.aut ~)
  |=  {access-token/@t expires-in/@u}
  ?:  (lth expires-in ^~((div ~d7 ~s1)))  ::  short-lived token
    %^  toke-req:auth  grant-type='fb_exchange_token'
      [key='fb_exchange_token' value=access-token]
    ~
  [[%redo ~] ..bak(access-token access-token)]
::++  wyp  ~
--
