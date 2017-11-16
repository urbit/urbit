::  Test url +https://graph.facebook.com/v2.5/me
::
::::  /hoon/facebook/com/sec
  ::
/+    oauth2
::
::::
  ::
|%
++  dialog-url    'https://www.facebook.com/dialog/oauth?response_type=code'
++  exchange-url  'https://graph.facebook.com/v2.3/oauth/access_token'
--
::
::::
  ::
|_  {bal/(bale:eyre keys:oauth2) access-token/token:oauth2}
::  ++aut is a "standard oauth2" core, which implements the
::  most common handling of oauth2 semantics. see lib/oauth2 for more details,
::  and examples at the bottom of the file.
++  aut
  %+  ~(standard oauth2 bal access-token)  .
  |=(access-token/token:oauth2 +>(access-token access-token))
::
++  filter-request
  %^  out-add-query-param:aut  'access_token'
    scope=~['user_about_me' 'user_posts']
  dialog-url
::
++  receive-auth-query-string  (in-code-to-token:aut exchange-url)
::
++  receive-auth-response
  |=  a/httr:eyre  ^-  core-move:aut
  ?:  (bad-response:aut p.a)
    [%give a]  :: [%redo ~]  ::  handle 4xx?
  =+  `{access-token/@t expires-in/@u}`(grab-expiring-token:aut a)
  ?.  (lth expires-in ^~((div ~d7 ~s1)))  ::  short-lived token
    [[%redo ~] +>.$(access-token access-token)]
  :-  %send
  %^  request-token:aut  exchange-url
    grant-type='fb_exchange_token'
  [key='fb_exchange_token' value=access-token]~
--
