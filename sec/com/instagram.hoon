::  
::::  /hoon/instagram/com/sec
  ::
/+    oauth2
::
::::
  ::
=+  ^=  aut
    %+  oauth2
      'https://api.instagram.com/oauth/authorize?response_type=code'
    'https://api.instagram.com/oauth/access_token'
|_  {(bale keys:oauth2) tok/token.aut}
++  aut  ~(. ^aut +<- /basic)
++  out
  |=  a/hiss
  =;  mow  ~&  db-authorized+mow  mow
  %.  a
  (out-quay:aut 'access_token'^tok)
++  in   in-code:aut
++  bak  (bak-save-access:aut . |=(tok/token:aut +>(tok tok)))
--

