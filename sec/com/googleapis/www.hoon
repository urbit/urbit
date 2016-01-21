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
|%
:: ++  crypto                                              ::  XX in zuse
::   |=  [our=@p now=@da]
::   =+  `mac=mace`p:;;(buck .^(a//(crip <our>)/buck/(crip <now>)/(crip <our>)))
::   ?>  ?=([^ ~] mac)  :: current, single life
::   (weur q.i.mac)
::
++  join  |=([a=tape b=(list tank)] rose/[a ~ ~]^b)
++  endpoint  |=(a=path [[& ~ `/com/googleapis/www] [~ a] ~])
++  toke-url  (endpoint /oauth2/v4/token)
++  dbg-post  `purl`[[| `6.000 `/localhost] `/testing /]
++  auth-url
  |=  [cid=@t sop=(list cord)]  ^-  purl
  :+  [& ~ `/com/google/accounts]  [~ /o/oauth2/v2/auth]
  %-  fass  :~
    client-id/cid
    access-type/%offline
    response-type/%code
    =<  scope/(crip ~(ram re (join " " (turn sop .))))
    |=(a=cord leaf/(earn (endpoint /auth/[a])))
  ::
    redirect-uri/redirect-uri
  ==
++  redirect-uri  'http://localhost:8443/~/ac/www.googleapis.com/auth'
++  user-state  ,[ber=@t]
--
::
::::
  ::
|_  [(bale ,@t) user-state]
++  decode-keys  ((hard ,[cid=@t cis=@t ~]) (lore key)) :: XX typed %jael
++  client-id      cid:decode-keys
++  client-secret  cis:decode-keys
++  out
  |=  a=hiss  ^-  sec-move
  ?~  ber  [%show (auth-url client-id 'userinfo.email' 'plus.me' ~)]
  [%send %_(a q.q (~(add ja q.q.a) %authorization (cat 3 'Bearer ' ber)))]
::
++  in
  |=  a=quay  ^-  sec-move
  =+  cod=~|(%no-code (~(got by (mo a)) %code))
  =+  hed=(mo ~[content-type/~['application/x-www-form-urlencoded']])
  =-  [%send toke-url %post hed `(tact +:(tail:earn code/cod -))]
  %-  fass
  :~  client-id/client-id
      client-secret/client-secret
      redirect-uri/redirect-uri
      grant-type/'authorization_code'
  ==
::
++  parse-auth
  |=  [@u a=@t]
  %.  a
  ;~  biff
    poja
    =>  jo  %-  ot  :~
      'token_type'^(su (jest 'Bearer'))
      'access_token'^so
      'refresh_token'^so
      'expires_in'^ni
    ==
  ==
::
++  bak
  |=  res=httr  ^-  [sec-move _+>]
  ?.  ?=(2 (div p.res 100))    :: bad response
    ~&  bad-httr/p.res
    [[%redo ~] +>.$]
  =+  ~|  bad-json/r.res
      ^-  [@ ber=@t ref=@t tim=@u]
      (need (parse-auth (need r.res)))
  :-  [%redo ~]
  +>.$(ber ber)
--
