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
::
++  join  |=([a=tape b=(list tank)] rose/[a ~ ~]^b)
++  auth-url
  |=  [cid=@t sop=(list cord)]  ^-  purl
  :+  [& ~ `/com/google/accounts]  [~ /o/oauth2/v2/auth]
  %-  fass  :~
    client-id/cid
    access-type/%offline
    response-type/%code
    =<  scope/(crip ~(ram re (join " " (turn sop .))))
    |=(a=cord leaf/(earn [& ~ `/com/googleapis/www] `/auth/[a] ~))
    redirect-uri/'http://localhost:8443/~/ac/www.googleapis.com/auth'
  ==
--
::
::::
  ::
=|  [ber=@t client-id=@t]
::|_  [bowl-ish user-state]
::++  out
  :: =.  ber  XX
  =.  client-id  XX
  ::  XX dynamic
  |=  a=hiss  ^-  (each hiss purl)
  ?~  ber  [%| (auth-url client-id 'userinfo.email' 'plus.me' ~)]
  [%& %_(a q.q (~(add ja q.q.a) %authorization (cat 3 'Bearer ' ber)))]
::--
