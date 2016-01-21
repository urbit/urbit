|%
++  fass                                                ::  rewrite quay
  |=  a=quay
  %+  turn  a
  |=  [p=@t q=@t]  ^+  +<
  [(gsub '-' '_' p) q]
::
++  gsub                                                ::  replace chars
  |=  [a=@t b=@t t=@t]
  ^-  @t
  ?~  t  t
  %+  add  (lsh 3 1 $(t (rsh 3 1 t)))
  =+  c=(mod t (bex 8))
  ?:(=(a c) b c)
--
::
::::
  ::
|_  [(bale ,@t) access-token=@t]
++  decode-key                        :: XX from bale w/ typed %jael
  ((hard ,[client-id=@t client-secret=@t ~]) (lore key))
::
++  redirect-uri  'http://localhost:8443/~/ac/graph.facebook.com/auth'
++  aut
  =+  key=decode-key :: XX
  ^-  quay
  %-  fass
  :~  client-id/client-id.key
      redirect-uri/redirect-uri
      scope/'user_about_me user_posts'
  ==
::
++  out
  |=  a=hiss  ^-  sec-move
  ?~  access-token
    [%show [& ~ `/com/facebook/www] `/dialog/oauth aut]
  [%send %_(a r.p :_(r.p.a 'access_token'^access-token))]
::
::
++  graph  [& ~ `/com/facebook/graph]
++  in
  =+  key=decode-key :: XX
  |=  a=quay  ^-  sec-move
  =+  cod=~|(%no-code (~(got by (mo a)) %code))
  =-  [%send [graph `/'v2.3'/oauth/'access_token' -] %get ~ ~]
  %-  fass
  :~  code/cod
      client-id/client-id.key
      client-secret/client-secret.key
      redirect-uri/redirect-uri
      grant-type/'authorization_code'
  ==
::
::
++  parse-bak
  |=  [@u a=@t]
  %.  a
  ;~  biff
    poja
    =>  jo  %-  ot  :~
      'access_token'^so
      'expires_in'^ni
    ==
  ==
::
++  bak
  |=  res=httr  ^-  [sec-move _+>]
  =+  ~|  bad-json/r.res
      ^-  [access-token=@t expires-in=@u]
      (need (parse-bak (need r.res)))
  ~&  res
  =+  token-expires=`@da`(add now (mul ~s1 expires-in))
  ~&  authenticated-until/token-expires   :: XX handle timeout
  :-  [%redo ~]
  +>.$(access-token access-token)
--
