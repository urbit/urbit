::  Test url +https://app.asana.com/api/1.0/users/me
::
::::  /hoon/asana/com/sec
  ::
/+    oauth2
::
::::
  ::
|%
++  dialog-url    'https://app.asana.com/-/oauth_authorize?response_type=code'
++  exchange-url  'https://app.asana.com/-/oauth_token'
--
::
::::
  ::
|_  {bal/(bale:eyre keys:oauth2) tok/token:oauth2}
::  ++aut is a "standard oauth2" core, which implements the
::  most common handling of oauth2 semantics. see lib/oauth2 for more details,
::  and examples at the bottom of the file.
++  aut  (~(standard oauth2 bal tok) . |=(tok/token:oauth2 +>(tok tok)))
++  filter-request  (out-add-header:aut scope=~ dialog-url)
::
++  receive-auth-query-string   (in-code-to-token:aut exchange-url)
++  receive-auth-response       bak-save-token:aut
--
::  create a developer app by logging into https://app.asana.com/, and clicking
::  "My Profile Settings" > Apps > "Manage my developer apps"

::  Be sure to be on https://localhost:8443 and to have registered
::  'http://localhost:8443/~/ac/asana.com/~./in' as the redirect URI.
::  (If unable to change port number of ship, change the redirect URI port in %eyre)

::  |init-oauth2 /com/asana

::  Enter this sample command to get your user information:
::  +https://app.asana.com/api/1.0/users/me

::  Before you receive the response, you'll have to clink on the link.
::  If you successfully auth, you should receive the response in the dojo.
