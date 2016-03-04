::  Test url +https://www.googleapis.com/oauth2/v1/userinfo
::
::::  /hoon/www/googleapis/com/sec
  ::
/+    oauth2
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
  =<  .(state-usr &)
  %-  oauth2
  :_  exchange='https://www.googleapis.com/oauth2/v4/token'
  ^=  dialog
  %*  .  (need (epur 'https://accounts.google.com/o/oauth2/v2/auth'))
      r
    %-  fass:oauth2
    :~  login-hint+?~(lon '' (crip (rash lon suffix-email)))
        access-type+%offline
        response-type+%code
        prompt+%consent
    ==
  ==
--
!:
::::
  ::
|_  {bal/(bale keys:oauth2) user-state}
++  auth-re  ~(. (re:auth .) ref |=(a/_ref +>(ref a)))
++  auth  ~(. (auth-usr usr.bal) bal scopes)
++  scopes
  :~  'https://mail.google.com'
      'https://www.googleapis.com/auth/plus.me'
      'https://www.googleapis.com/auth/userinfo.email'
  ==
::
++  out  (out-fix-expired:auth-re (out-math:auth ber))
++  res  |=(a/httr ((res-handle-refreshed:auth-re save-access res-give:auth) a))
::
++  save-access  |=(a/cord:[token:oauth2] +>(ber a))
::
++  in  
  |=  a/quay
  (in-code:auth a)
++  bak  |=(a/httr ((bak-save-tokens:auth-re save-access) a))
++  upd  *user-state
--
