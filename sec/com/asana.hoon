::  
::::  /hoon/digitalocean/com/sec
  ::
/+    oauth2
::
::::
  ::
=+  ^=  aut
    %+  oauth2
      'https://app.asana.com/1.0/oauth_authorize?response_type=code&state=ste'
    'https://app.asana.com/1.0/oauth_token'
|_  {(bale keys:oauth2) tok/token.aut}
++  aut  ~(. ^aut +<- /read/write)
++  out
  |=  a/hiss
  =;  mow  ~&  db-authorized+mow  mow
  %.  a
  (out-quay:aut tok)
++  in   in-code:aut
++  bak  (bak-save-access:aut . |=(tok/token:aut +>(tok tok)))
--


::  create a developer app on https://cloud.digitalocean.com/settings/api/applications/new
::  to get a client id and secret

::  Be sure to be on https://localhost:8443 and to have registered 
::  'http://localhost:8443/~/ac/digitalocean.com/~./in' as the redirect URI.
::  (If unable to change port number of ship, change the redirect URI port in %eyre)

::  |init-oauth2 |init-oauth2 /com/digitalocean

::  Enter home this sample command to get your user information:
::  +https://api.digitalocean.com/v2/account
::  Before you receive the response, you'll have to clink on the link.
::  If you successfully auth, you should receive the response in the dojo.


