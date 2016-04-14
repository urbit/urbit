::  Test url +https://api.dropboxapi.com/2/files/list_folder
::::  /hoon/dropboxapi/com/sec
  ::
/+    oauth2
::
::::
  ::
=+  ^=  aut
    %+  oauth2
      'https://www.dropbox.com/1/oauth2/authorize?response_type=code'
    'https://api.dropboxapi.com/1/oauth2/token'
|_  {(bale keys:oauth2) tok/token.aut}
++  aut  ~(. ^aut +<- /)
++  out
  |=  a/hiss
  =;  mow  ~&  db-authorized+mow  mow
  %.  a
  (out-math:aut tok)
++  in   in-code:aut
++  bak  (bak-save-access:aut . |=(tok/token:aut +>(tok tok)))
--

::  create a developer app on https://www.instagram.com/developer/ to get a
::  client id and secret.

::  Be sure to be on https://localhost:8443 and to have registered 
::  'http://localhost:8443/~/ac/dropboxapi.com/~./in' as the redirect URI.
::  (If unable to change port number of ship, change the redirect URI port in %eyre)

::  |init-oauth2 |init-oauth2 /com/instagram

::  Enter this sample command to list the contents of one of your directories:
::  +https://api.dropboxapi.com/2/files/list_folder &json (joba %paths+'/{INSERT-DIR-NAME')

::  Before you receive the response, you'll have to click on the link in the
::  dojo to authenticate yourself.

:: You should receive a response listing the contents of that directory. 
