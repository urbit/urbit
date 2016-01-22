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
++  user-state  ,[ber=@t ref=@t ded=@da]
--
::
::::
  ::
|_  [(bale ,@t) user-state]
++  decode-keys  ((hard ,[cid=@t cis=@t ~]) (lore key)) :: XX typed %jael
++  client-id      cid:decode-keys
++  client-secret  cis:decode-keys
::
++  need-refresh  (lth ded (add now ~m1))
++  out
  |=  a=hiss  ^-  sec-move
  ?~  ber  [%show (auth-url client-id 'userinfo.email' 'plus.me' ~)]
  ?:  need-refresh
    [%send toke-url (toke-req refresh-token/ref grant-type/'refresh_token' ~)]
  [%send %_(a q.q (~(add ja q.q.a) %authorization (cat 3 'Bearer ' ber)))]
::
++  toke-req
  |=  quy=quay  ^-  moth 
  :+  %post  (mo ~[content-type/~['application/x-www-form-urlencoded']])
  =-  `(tact +:(tail:earn -))
  %-  fass
  %+  welp  quy
  :~  client-id/client-id
      client-secret/client-secret
      redirect-uri/redirect-uri
      
  ==
++  in
  |=  a=quay  ^-  sec-move
  =+  cod=~|(%no-code (~(got by (mo a)) %code))
  [%send toke-url (toke-req code/cod grant-type/'authorization_code' ~)]
::
++  res
  |=  a=httr  ^-  [sec-move _+>]
  ?:  need-refresh
    ?.  ?=(2 (div p.a 100))    :: bad response
      ~&  bad-httr/p.a
      [[%redo ~] +>.$]
    ~|  %refreshed-token
    [[%redo ~] (new-token (grab a parse-toke))]
  [[%give a] +>.$]
::
++  new-token
  |=  [typ=term ber=@t tim=@u]
  ?>  ?=(%bearer typ)
  +>.$(ber ber, ded (add now (mul ~s1 tim)))
::
++  grab
  |*  [a=httr b=fist:jo]
  ~|  bad-json/r.a
  (need (;~(biff poja b) q:(need r.a)))
::
++  parse-toke
  =>  jo  %-  ot  :~
    'token_type'^(cu cass sa)
    'access_token'^so
    'expires_in'^ni
  ==
::
++  bak
  |=  a=httr  ^-  [sec-move _+>]
  ?.  ?=(2 (div p.a 100))    :: bad response
    ~&  bad-httr/p.a
    [[%redo ~] +>.$]
  :-  [%redo ~]
  =.  ref  (grab a (ot 'refresh_token'^so ~):jo)
  (new-token (grab a parse-toke))
--
