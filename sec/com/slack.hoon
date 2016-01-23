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
|_  [(bale ,@t) token=@t]
++  decode-key                        :: XX from bale w/ typed %jael
  ((hard ,[client-id=@t client-secret=@t ~]) (lore key))
::
++  redirect-uri  (cat 3 'http://localhost:8443/~/ac/slack.com/' (scot %ta usr))
++  aut
  =+  key=decode-key :: XX
  ^-  quay
  %-  fass
  :~  client-id/client-id.key
      redirect-uri/redirect-uri
      scope/'client admin'
  ==
::
++  out
  |=  a=hiss  ^-  sec-move
  ?~  token
    [%show [& ~ `/com/slack] `/oauth/authorize aut]
  [%send %_(a r.p :_(r.p.a 'token'^token))]
::
::
++  in
  =+  key=decode-key :: XX
  |=  a=quay  ^-  sec-move
  =+  cod=~|(%no-code (~(got by (mo a)) %code))
  =-  [%send [[& ~ `/com/slack] `/api/'oauth.access' -] %get ~ ~]
  %-  fass
  :~  code/cod
      client-id/client-id.key
      client-secret/client-secret.key
      redirect-uri/redirect-uri
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
    ==
  ==
::
++  bak
  |=  res=httr  ^-  [sec-move _+>]
  =+  ~|  bad-json/r.res
      ^-  token=@t
      (need (parse-bak (need r.res)))
  :-  [%redo ~]
  +>.$(token token)
--
