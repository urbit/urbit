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
++  join  
  |=  [a=cord b=(list cord)]
  ?~  b  ''
  (rap 3 |-([i.b ?~(t.b ~ [a $(b t.b)])]))
::
++  bad-response  |=(a=@u ?:(=(2 (div a 100)) | ~&(bad-httr/a &)))
++  grab-json
  |*  [a=httr b=fist:jo]
  ~|  bad-json/r.a
  (need (;~(biff poja b) q:(need r.a)))
--
::
::::
  ::
|%
++  token  ?(~ @t)
++  keys  cord:,[cid=@t cis=@t]
++  decode-keys                       :: XX from bale w/ typed %jael
  |=(key=keys ((hard ,[cid=@t cis=@t ~]) (lore key)))
--
::
::::
  ::
|=  [dialog=[p=host q=path r=quay] code-exchange=path]
=+  state-usr=&
|_  [(bale keys) scope=(list cord)]
++  client-id      cid:(decode-keys key)
++  client-secret  cis:(decode-keys key)
::
++  urb-hart  [| `8.443 `/localhost]  :: XX get from eyre
++  endpoint  |=(a=path [[& ~ `dom] [~ a] ~])
++  toke-url  (endpoint code-exchange)
++  auth-url
  ^-  purl
  :+  [& ~ p.dialog]  [~ q.dialog]
  %-  fass  
  %+  welp  r.dialog
  :~  state/?.(state-usr '' (pack usr /''))
      client-id/client-id
      redirect-uri/redirect-uri
      scope/(join ' ' scope)
  ==
::
++  redirect-uri  
  %-    crip    %-  earn
  =+  usr-span=?:(state-usr '_state' (scot %ta usr))
  [urb-hart `/~/ac/(join '.' (flop dom))/[usr-span] ~]
::
++  refresh-expiring
  |=  [[expires=@da refresh=token] otherwise=$+(hiss sec-move)]
  |=  a=hiss
  ?~  refresh  (otherwise a)
  ?:  (lth expires (add now ~m1))
    (otherwise a)
  [%send toke-url (toke-req 'refresh_token' refresh-token/refresh ~)]
::  
++  out-filtered
  |=  [tok=token aut=$+(hiss hiss)]
  |=  a=hiss  ^-  sec-move
  ?~(tok [%show auth-url] [%send (aut a)])
::
++  out-quay
  |=  [nam=span tok=token]
  %+  out-filtered  tok
  |=(a=hiss %_(a r.p :_(r.p.a nam^`@t`tok)))
::
++  out-math
  |=  ber=token
  =+  hed=authorization/(cat 3 'Bearer ' `@t`ber)
  %+  out-filtered  ber
  |=(a=hiss %_(a q.q (~(add ja q.q.a) hed)))
::
++  toke-req
  |=  [grant-type=cord quy=quay]  ^-  moth 
  :+  %post  (mo ~[content-type/~['application/x-www-form-urlencoded']])
  =-  `(tact +:(tail:earn -))
  %-  fass
  %+  welp  quy
  :~  client-id/client-id
      client-secret/client-secret
      redirect-uri/redirect-uri
      grant-type/grant-type
  ==
::
++  in-code
  |=  a=quay  ^-  sec-move
  =+  code=~|(%no-code (~(got by (mo a)) %code))
  [%send toke-url (toke-req 'authorization_code' code/code ~)]
::
++  token-type  'token_type'^(cu cass sa):jo
++  expires-in  'expires_in'^ni:jo
++  access-token  'access_token'^so:jo
++  refresh-token  'refresh_token'^so:jo
++  bak-parse-access
  |*  [done=* parse=(pole ,[span fist]:jo)]
  |=  handle=$+(_?~(parse *token [*token (need *(ot:jo parse))]) _done)
  |=  a=httr  ^-  [sec-move _done] 
  :-  [%redo ~]
  ?:  (bad-response p.a)  done  ::  handle 4xx?
  (handle (grab-json a (ot:jo access-token parse)))
::
:: ++  bak-parse-refresh
::   |=  a=httr  ^-  [sec-move _+>]
::   ?:  (bad-response p.a)  [[%redo ~] +>.$]  ::  handle 4xx?
::   =.  ref  (grab a (ot 'refresh_token'^so ~):jo)
::   [[%redo ~] (new-token a)]
:: ++  res-catch-refresh
::   |=  a=httr  ^-  [sec-move _+>]
::   ?:  need-refresh
::     ?:  (bad-response p.a)  [[%redo ~] +>.$]  ::  handle 4xx?
::     ~|  %refreshed-token
::     [[%redo ~] (new-token a)]
::   [[%give a] +>.$]
::
:: ++  new-token
::   |=  a=httr  ^+  +>
::   =+  `[typ=term ber=@t tim=@u]`(grab a parse-toke)
::   ?>  ?=(%bearer typ)
::   +>.$(ber ber, ded (add now (mul ~s1 tim)))
::
--
