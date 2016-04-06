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

::  Be sure to be on https://localhost:8443, and to have registered 
::  http://localhost:8443/~/ac/instagram.com/~./in as the redirect URI.
::  (If unable to change port number of ship, change the redirect URI port in %eyre)
::  |init-oauth2 |init-oauth2 /com/instagram

::  Enter home this sample command to get your user information:
::  +https://api.instagram.com/v1/users/self

::  Before you receive the response, you'll have to clink on the link to
::  authenicate yourself. You should then receive the response.

