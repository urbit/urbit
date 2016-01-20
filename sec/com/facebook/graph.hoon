|%
++  bowl-ish  ,~
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
=+  :-  client-id='255263714659582'
    client-secret=XX
|_  [bowl-ish ber=@t]
++  redirect-uri  'http://localhost:8443/~/ac/graph.facebook.com/auth'
++  aut
  ^-  quay
  %-  fass
  :~  client-id/client-id
      redirect-uri/redirect-uri
      scope/'user_about_me user_posts'
  ==
::
++  out
  |=  a=hiss
  ?~  ber  [%| [& ~ `/com/facebook/www] `/dialog/oauth aut]
  [%& %_(a r.p :_(r.p.a 'access_token'^ber))]
::
::
++  graph  [& ~ `/com/facebook/graph]
++  in
  |=  a=quay  ^-  (each hiss ,_!!)
  =+  cod=~|(%no-code (~(got by (mo a)) %code))
  =-  [%& [graph `/'v2.3'/oauth/'access_token' -] %get ~ ~]
  %-  fass
  :~  code/cod
      client-id/client-id
      client-secret/client-secret
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
  |=  res=httr  ^-  [(each ,_!! ,%retry) _+>]
  =+  ~|  bad-json/r.res
      ^-  [ber=@t tim=@u]
      (need (parse-bak (need r.res)))
  :-  [%| %retry]  :: XX handle timeout
  +>.$(ber ber)
--
