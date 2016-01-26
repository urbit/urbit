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
  |=  res=httr
  =+  a=auth
  ?:  (bad-response.a p.res)  [%redo ~]
  =+  ^-  [access-token=@t expires-in=@u]
      (grab-json.a res (ot:jo access-token expires-in ~):a)
  ?:  (lth expires-in ^~((div ~d7 ~s1)))  ::  short-lived token
    (toke-req:a 'fb_exchange_token' fb-exchange-token/access-token ~)
  [[%redo ~] ..bak(access-token access-token)]
::++  wipe  !!
--
