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

::  create a developer app on https://www.instagram.com/developer/ to get a
::  client id and secret

::  |init-oauth2

::  Enter home this sample command to get your user information.
::  Before you receive the response, you'll have to clink on the link.
::  If you successfully auth, you should receive the response in the dojo


