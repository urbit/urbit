::  Test url +https://slack.com/api/auth.test
::
::::  /hoon/slack/com/sec
  ::
/+    oauth1
::
::::
  ::
::=+  ^=  aut
::    %+  oauth2
::      'https://slack.com/oauth/authorize'
::    'https://slack.com/api/oauth.access'
|_  {(bale @t:keys:oauth1) tok/{@t @t}}
::++  aut  ~(. ^aut +<- /client/admin)
::++  out  (out-quay:aut 'token'^tok)
:: ++  in   in-code:aut
:: ++  bak  (bak-save-access:aut . |=(tok/token:aut +>(tok tok)))
++  out
  |=  a/hiss  ^-  {$send hiss}
  ~|  %authorization
  ~&  twit-auth+(~(got by q.q.a) %authorization)
  [%send a]
--
