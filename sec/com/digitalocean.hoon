::  Test url  +https://api.digitalocean.com/v2/account
::
::::  /hoon/digitalocean/com/sec
  ::
/+    oauth2
::
::::
  ::
|%
++  dialog-url    'https://cloud.digitalocean.com/v1/oauth/authorize?response_type=code'
++  exchange-url  'https://cloud.digitalocean.com/v1/oauth/token'
--
::
::::
  ::
|_  {bal/(bale:eyre keys:oauth2) tok/token:oauth2}
::  ++aut is a "standard oauth2" core, which implements the
::  most common handling of oauth2 semantics. see lib/oauth2 for more details,
::  and examples at the bottom of the file.
++  aut  (~(standard oauth2 bal tok) . |=(tok/token:oauth2 +>(tok tok)))
++  filter-request  (out-add-header:aut scope=~[%read %write] dialog-url)
::
++  receive-auth-query-string  (in-code-to-token:aut exchange-url)
++  receive-auth-response      bak-save-token:aut
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
