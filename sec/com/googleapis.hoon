::  Test url +https://www.googleapis.com/oauth2/v1/userinfo
::
::::  /hoon/googleapis/com/sec
  ::
/+    oauth2, interpolate, hep-to-cab
::
::::
  ::
|%
++  user-state  {ber/token ref/refresh}:oauth2
++  suffix-email
  %+  cook  welp
  ;~  plug
    (star ;~(less pat prn))
    ;~(pose (plus prn) (easy "@gmail.com"))
  ==
::
++  auth-usr
  |=  usr/iden
  =+  lon=(fall (slaw %t usr) usr)
  %+  add-query:interpolate  'https://accounts.google.com/o/oauth2/v2/auth'
  %-  quay:hep-to-cab
  :~  login-hint+?~(lon '' (crip (rash lon suffix-email)))
      access-type+%offline
      response-type+%code
      prompt+%consent
  ==
++  scopes
  :~  'https://mail.google.com'
      'https://www.googleapis.com/auth/plus.me'
      'https://www.googleapis.com/auth/userinfo.email'
  ==
::
++  exchange-url  'https://www.googleapis.com/oauth2/v4/token'
--
!:
::::
  ::
|_  {bal/(bale keys:oauth2) own/user-state}
++  auth
  =+  a=~(standard-refreshing oauth2 bal ber.own)
  (a(state-usr &) ..auth ref.own |=(a/user-state ..auth(own a)))
::
++  out  (out-refresh-or-add-header:auth exchange-url scopes dialog-url)
++  dialog-url  (auth-usr usr.bal)
::
++  res  res-save-after-refresh:auth
::
++  in   (in-code-to-token:auth exchange-url)
++  bak  bak-save-both-tokens:auth
:: ++  upd  *user-state
--
