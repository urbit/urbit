/+    oauth2
::
::::
  ::
=+  [`/com/facebook/www /dialog/oauth response-type/%code ~]
=+  aut=(oauth2 - /'v2.3'/oauth/'access_token')
|_  [bal=(bale keys.aut) access-token=token.aut]
++  auth  ~(. aut bal /'user_about_me'/'user_posts')
++  out  (out-quay:auth 'access_token'^access-token)
++  in   in-code:auth
++  bak
  %-  (bak-parse-access:auth . expires-in.aut ~)
  |=  [access-token=token.aut expires-in=@u]
  =+  token-expires=`@da`(add now.bal (mul ~s1 expires-in))
  ~&  authenticated-until/token-expires   :: XX handle timeout
  +>.$(access-token access-token)
--
