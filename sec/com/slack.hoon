::  Test url +https://slack.com/api/auth.test
::
::::  /hoon/slack/com/sec
  ::
/+    oauth2
::
::::
  ::
|_  {bal/(bale:eyre keys:oauth2) tok/token:oauth2}
::  ++aut is a "standard oauth2" core, which implements the
::  most common handling of oauth2 semantics. see lib/oauth2 for more details,
::  and examples at the bottom of the file.
++  aut  (~(standard oauth2 bal tok) . |=(tok/token:oauth2 +>(tok tok)))
++  filter-request
  %^  out-add-query-param:aut  'token'
    scope=~[%client %admin]
  oauth-dialog='https://slack.com/oauth/authorize'
::
++  receive-auth-query-string  (in-code-to-token:aut url='https://slack.com/api/oauth.access')
++  receive-auth-response      bak-save-token:aut
--
