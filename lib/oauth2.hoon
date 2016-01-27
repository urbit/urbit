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
++  dbg-post  `purl`[[| `6.000 `/localhost] `/testing /]
++  endpoint  |=([dom=(list cord) a=path] [[& ~ `dom] [~ a] ~])
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
++  refresh  ,[tok=token needed=@da pending=_`?`|]
++  keys  cord:,[cid=@t cis=@t]
++  core-move  |*(a=* $&([sec-move _a] sec-move))
++  decode-keys                       :: XX from bale w/ typed %jael
  |=(key=keys ((hard ,[cid=@t cis=@t ~]) (lore key)))
--
::
::::
  ::
|=  [dialog=[p=host q=path r=quay] code-exchange=path]
=+  state-usr=|
|_  [(bale keys) scope=(list cord)]
++  client-id      cid:(decode-keys key)
++  client-secret  cis:(decode-keys key)
::
++  urb-hart  [| `8.443 `/localhost]  :: XX get from eyre
++  toke-url  (endpoint dom code-exchange)
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
  |=  [grant-type=cord quy=quay]  ^-  [%send hiss]
  :+  %send  toke-url
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
  (toke-req 'authorization_code' code/code ~)
::
++  token-type  'token_type'^(cu cass sa):jo
++  expires-in  'expires_in'^ni:jo
++  access-token  'access_token'^so:jo
++  refresh-token  'refresh_token'^so:jo
++  bak-save-access
  |*  [done=* handle=$+(cord:token *)]  :: $+(token _done)
  %-  (bak-parse done access-token ~)
  |=(tok=cord:token [[%redo ~] (handle tok)])
::
++  bak-parse
  |*  [done=* parse=(pole ,[span fist]:jo)]
  |=  handle=$+(_?~(parse ~ (need *(ot:jo parse))) (core-move done))
  |=  a=httr  ^-  (core-move done)
  ?:  (bad-response p.a)  [%redo ~]  ::  handle 4xx?
  (handle (grab-json a (ot:jo parse)))
::
++  res-give  |=(a=httr [%give a])
::
++  re
  |*  cor=*           :: XX redundant with *export, but type headaches
  |_  [ref=refresh export=$+(refresh _cor)]
  ++  out-fix-expired
    |=  default=$+(hiss sec-move)
    ^-  $+(hiss (core-move cor))
    ?~  tok.ref  default
    ?.  (lth needed.ref (add now ~m59.s30))
      default
    |=  a=hiss
    :_  (export ref(pending &))
    (toke-req 'refresh_token' refresh-token/tok.ref ~)
  ::
  ++  res-handle-refreshed
    |=  [handle-access=_=>(cor |=(@t +>)) default=$+(httr sec-move)]
    ^-  $+(httr (core-move cor))
    ?.  pending.ref  default
    %-  (bak-parse cor expires-in access-token ~)
    |=  [exp=@u tok=axs=@t]  ^-  [sec-move _cor]
    =.  +>.handle-access
      (export tok.ref (add now (mul ~s1 exp)) |)
    [[%redo ~] (handle-access axs.tok)]
  ::
  ++  bak-save-tokens
    |=  handle-access=_=>(cor |=(@t +>))
    %-  (bak-parse cor expires-in access-token refresh-token ~)
    |=  [exp=@u tok=[axs=@t ref=@t]]  ^-  [sec-move _cor]
    =.  +>.handle-access
      (export ref.tok (add now (mul ~s1 exp)) |)
    [[%redo ~] (handle-access axs.tok)]
  --
--
                                                                
