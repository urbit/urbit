::  Test url +https://slack.com/api/auth.test
::
::::  /hoon/slack/com/sec
  ::
/+    oauth2
::
::::
  ::
=+  ^=  aut
    %+  oauth2
      'https://slack.com/oauth/authorize'
    'https://slack.com/api/oauth.access'
|_  {(bale keys:oauth2) tok/token.aut}
++  aut  ~(. ^aut +<- /client/admin)
++  out  (out-quay:aut 'token'^tok)
++  in   in-code:aut
++  bak  (bak-save-access:aut . |=(tok/token:aut +>(tok tok)))
--
