/+    oauth2
::
::::
  ::
|%
++  user-state  ,[ber=token ref=refresh]:oauth2
++  auth-lon
  |=  lon=span
  =<  .(state-usr &)
  %-  oauth2
  =-  [[`/com/google/accounts /o/oauth2/v2/auth -] /oauth2/v4/token]
  :~  login-hint/?~(lon '' (cat 3 lon '@gmail.com'))
      access-type/%offline
      response-type/%code
  ==
--
::
::::
  ::
|_  [bal=(bale keys:oauth2) user-state]
++  auth-re  ~(. (re:auth .) ref |=(a=_ref +>(ref a)))
++  auth  ~(. (auth-lon usr.bal) bal (scopes 'userinfo.email' 'plus.me' ~))
++  scopes
  =+  scope=|=(b=@ta (endpoint:oauth2 dom.bal /auth/[b]))
  |=(a=(list ,@ta) (turn a |=(b=@ta (crip (earn (scope b))))))
::
++  out  (out-fix-expired:auth-re (out-math:auth ber))
++  res  (res-handle-refreshed:auth-re save-access res-give:auth)
++  save-access  |=(a=cord:[token:oauth2] +>(ber a))
::
++  in  in-code:auth
++  bak  (bak-save-tokens:auth-re save-access)
:: ++  wipe  ~
--
