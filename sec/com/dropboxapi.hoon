::  Test url +https://api.dropboxapi.com/2/users/get_current_account &json ~
::
::::  /hoon/dropboxapi/com/sec
  ::
/+    oauth2
::
::::
  ::
|%
++  dialog-url    'https://www.dropbox.com/1/oauth2/authorize?response_type=code'
++  exchange-url  'https://api.dropboxapi.com/1/oauth2/token'
--
::
::::
  ::
|_  {bal/(bale keys:oauth2) tok/token:oauth2}
::  aut is a "standard oauth2" core, which implements the 
::  most common handling of oauth2 semantics. see lib/oauth2 for more details.
++  aut  (~(standard oauth2 bal tok) . |=(tok/token:oauth2 +>(tok tok)))
++  out  (out-add-header:aut scope=~ dialog-url)
::
++  in   (in-code-to-token:aut exchange-url)
++  bak  bak-save-token:aut
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
