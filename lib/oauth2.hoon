::  OAuth 2.0 %authorization
::
::::  /hoon/oauth2/lib
  ::
/+    hep-to-cab, interpolate
=,  ^eyre
=,  html
|%
++  parse-url  parse-url:interpolate
++  join
  |=  {a/cord b/(list cord)}
  ?~  b  ''
  (rap 3 |-([i.b ?~(t.b ~ [a $(b t.b)])]))
::
++  post-quay
  |=  {a/purl b/quay}  ^-  hiss
  =.  b  (quay:hep-to-cab b)
  =-  [a %post - ?~(b ~ (some (tact +:(tail:earn b))))]
  (my content-type+['application/x-www-form-urlencoded']~ ~)
::
++  mean-wall  !.
  |=  {a/term b/tape}  ^+  !!
  =-  (mean (flop `tang`[>a< -]))
  (turn (to-wain (crip b)) |=(c/cord leaf+(trip c)))
::
++  bad-response  |=(a/@u ?:(=(2 (div a 100)) | ~&(bad-httr+a &)))
++  grab-json
  |*  {a/httr b/fist:jo}
  ~|  bad-json+r.a
  ~|  (de-json q:(need r.a))
  (need (;~(biff de-json b) q:(need r.a)))
--
::
::::
  ::
|%
++  token  ?($~ @t)
++  refresh  {tok/token expiry/@da pending/_`?`|}
++  both-tokens  {token refresh}
++  keys  cord:{cid/@t cis/@t}
--
::
::::
  ::
=+  state-usr=|
|_  {(bale keys) tok/token}
++  client-id      cid:decode-keys
++  client-secret  cis:decode-keys
++  decode-keys                       :: XX from bale w/ typed %jael
  ^-  {cid/@t cis/@t $~}
  ?.  =(~ `@`key)
    ~|  %oauth-bad-keys
    ((hard {cid/@t cis/@t $~}) (to-wain key))
  %+  mean-wall  %oauth-no-keys
  """
  Run |init-oauth2 {<`path`dom>}
  If necessary, obtain client keys configured for a redirect_uri of
    {(trip redirect-uri)}
  """
::
++  auth-url
  |=  {scopes/(list @t) url/$@(@t purl)}  ^-  purl
  ~&  [%oauth-warning "Make sure this urbit ".
                      "is running on {(earn our-host `~ ~)}"]
  %+  add-query:interpolate  url
  %-  quay:hep-to-cab
  :~  state+?.(state-usr '' (pack usr /''))
      client-id+client-id
      redirect-uri+redirect-uri
      scope+(join ' ' scopes)
  ==
::
++  our-host  .^(hart %e /(scot %p our)/host/fake)
++  redirect-uri
  %-    crip    %-  earn
  %^  into-url:interpolate  'https://our-host/~/ac/:domain/:user/in'
    `our-host
  :~  domain+(join '.' (flop dom))
      user+?:(state-usr '_state' (scot %ta usr))
  ==
::
::
++  request-token
  |=  {a/$@(@t purl) grant-type/cord quy/quay}  ^-  hiss
  %+  post-quay  (parse-url a)
  %-  quay:hep-to-cab
  %+  welp  quy
  :~  client-id+client-id
      client-secret+client-secret
      redirect-uri+redirect-uri
      grant-type+grant-type
  ==
::
++  request-token-by-code
  |=({a/$@(@t purl) b/@t} (request-token a 'authorization_code' code+b ~))
::
++  grab-token
  |=  a/httr  ^-  axs/@t
  (grab-json a (ot 'access_token'^so ~):jo)
::
++  grab-expiring-token
  |=  a/httr  ^-  {axs/@t exp/@u}
  (grab-json a (ot 'access_token'^so 'expires_in'^ni ~):jo)
::
++  grab-both-tokens
  |=  a/httr  ^-  {axs/@t exp/@u ref/@t}
  (grab-json a (ot 'access_token'^so 'expires_in'^ni 'refresh_token'^so ~):jo)
::
++  auth
  ?~  tok  ~|(%no-bearer-token !!)
  |%
  ++  header  `cord`(cat 3 'Bearer ' `@t`tok)
  ++  query   `cord`tok
  --
::
++  add-auth-header
  |=  request/{url/purl meth hed/math (unit octs)}
  ^+  request
  ::  =.  p.url.request  [| `6.000 [%& /localhost]]       ::  for use with unix nc
  ~&  add-auth-header+(earn url.request)
  request(hed (~(add ja hed.request) %authorization header:auth))
::
++  add-auth-query
  |=  {token-name/cord request/{url/purl meth math (unit octs)}}
  ^+  request
  ::  =.  p.url.request  [| `6.000 [%& /localhost]]       ::  for use with unix nc
  ~&  add-auth-query+(earn url.request)
  request(r.url [[token-name query:auth] r.url.request])
::
++  re
  |_  ref/refresh
  ++  needs-refresh  ?~(tok.ref | is-expired)
  ++  is-expired  (lth expiry.ref (add now ~m5))
  ++  update
    |=  exp/@u  ^+  ref
    ref(pending |, expiry (add now (mul ~s1 exp)))
  ::
  ++  update-if-needed
    |=  exchange-url/$@(@t purl)
    ^-  {(unit hiss) refresh}
    ?~  tok.ref  `ref
    ?.  is-expired  `ref
    :_  ref(pending &)
    `(request-token exchange-url 'refresh_token' refresh-token+tok.ref ~)
  --
::
::  expected semantics, to be copied and modified if anything doesn't work
++  standard
  |*  {done/* save/$-(token *)}
  |%
  ++  save  ^-($-(token _done) ^save)                   ::  shadow(type canary)
  ++  core-move  $^({sec-move _done} sec-move)          ::  stateful
  ::
  ::  Insert token into query string. expects:
  ::    ++  in   (in-code-to-token 'http://...')        ::  handle callback
  ++  out-add-query-param
    |=  {token-name/knot scopes/(list cord) dialog/$@(@t purl)}
    ::
    |=  a/hiss  ^-  $%({$send hiss} {$show purl})
    ?~  tok  [%show (auth-url scopes dialog)]
    [%send (add-auth-query token-name a)]
  ::
  ::  Add token as a header. expects:
  ::    ++  in   (in-code-to-token 'http://...')        ::  handle callback
  ++  out-add-header
    |=  {scopes/(list cord) dialog/$@(@t purl)}
    ::
    |=  a/hiss  ^-  sec-move
    ?~  tok  [%show (auth-url scopes dialog)]
    [%send (add-auth-header a)]
  ::
  ::  Exchange code in query string for access token. expects:
  ::    ++  bak  bak-save-token                         :: save access token
  ++  in-code-to-token
    |=  exchange-url/$@(@t purl)
    ::
    |=  a/quay  ^-  sec-move
    =+  code=~|(%no-code (~(got by (malt a)) %code))
    [%send (request-token-by-code exchange-url code)]
  ::
  ::  If an access token has been returned, save it
  ++  bak-save-token
    |=  a/httr  ^-  core-move
    ?:  (bad-response p.a)
      [%give a]  :: [%redo ~]  ::  handle 4xx?
    [[%redo ~] (save `token`(grab-token a))]
  --
::
++  standard-refreshing
  |*  {done/* ref/refresh save/$-({token refresh} *)}
  =+  s=(standard done |=(tok/token (save tok ref)))
  |%
  ++  save  ^-($-(both-tokens _done) ^save)             ::  shadow(type canary)
  ++  core-move  $^({sec-move _done} sec-move)          ::  stateful
  ::
  ::  See ++out-add-query-param:standard
  ::  Refresh token if we have an expired one, ask for authentication if none is present,
  ::  insert auth token into the query string if it's valid. expects:
  ::    ++  in   (in-code-to-token 'http://...')        ::  handle callback
  ::    ++  res  res-save-after-refresh
  ++  out-refresh-or-add-query-param
    |=  {exchange/$@(@t purl) s-args/{knot (list cord) $@(@t purl)}}
    ::
    |=  a/hiss  ^-  core-move
    =^  upd  ref  (~(update-if-needed re ref) exchange)
    ?^  upd  [[%send u.upd] (save tok ref)]
    %.(a (out-add-query-param.s s-args))
  ::
  ::  See ++out-add-header:standard
  ::  Refresh token if we have an expired one, ask for authentication if none is present,
  ::  add token as a header if it's valid. expects:
  ::    ++  in   (in-code-to-token 'http://...')        ::  handle callback
  ::    ++  res  res-save-after-refresh
  ++  out-refresh-or-add-header
    |=  {exchange/$@(@t purl) s-args/{(list cord) dialog/$@(@t purl)}}
    ::
    |=  a/hiss  ^-  core-move
    =^  upd  ref  (~(update-if-needed re ref) exchange)
    ?^  upd  [[%send u.upd] (save tok ref)]
    %.(a (out-add-header.s s-args))
  ::
  ::  If the last request refreshed the access token, save it.
  ++  res-save-after-refresh
    |=  a/httr  ^-  core-move
    ?.  pending.ref  [%give a]
    =+  `{axs/token exp/@u}`(grab-expiring-token a)
    =.  ref  (~(update re ref) exp)
    [[%redo ~] (save axs ref)]
  ::
  ::  Exchange code in query string for access and refresh tokens. expects:
  ::    ++  bak  bak-save-both-tokens                   :: save access token
  ++  in-code-to-token  in-code-to-token.s
  ::
  ::  If valid access and refresh tokens have been returned, save them
  ++  bak-save-both-tokens
    |=  a/httr  ^-  core-move
    =+  `{axs/token exp/@u ref-new/token}`(grab-both-tokens a)
    =.  tok.ref  ref-new
    =.  ref  (~(update re ref) exp)
    [[%redo ~] (save axs ref)]
  --
--
::
::  XX move-me
::
::
::::  Example "standard" sec/ core:
  ::
::
::  ::
::  ::::  /hoon/my-api/com/sec
::    ::
::  /+    oauth2
::  ::
::  ::::
::    ::
::  |_  {bal/(bale keys:oauth2) tok/token:oauth2}
::  ++  aut  (~(standard oauth2 bal tok) . |=(tok/token:oauth2 +>(tok tok)))
::  ++  out
::    %+  out-add-header:aut  scope=/full
::    oauth-dialog='https://my-api.com/authorize'
::  ::
::  ++  in
::    %-  in-code-to-token:aut
::    exchange-url='https://my-api.com/access_token'
::  ::
::  ++  bak  bak-save-token:aut
::  --
::
::
::::  Equivalent imperative code:
  ::
::
::  ::
::  ::::  /hoon/my-api/com/sec
::    ::
::  /+    oauth2
::  ::
::  ::::
::    ::
::  |_  {bal/(bale keys:oauth2) tok/token:oauth2}
::  ++  aut  ~(. oauth2 bal tok)
::  ++  out  ::  add header
::    =+  aut
::    |=  req/hiss  ^-  $%({$send hiss} {$show purl})
::    ?~  tok
::      [%show (auth-url scope=/full 'https://my-api.com/authorize')]
::    [%send (add-auth-header req)]
::  ::
::  ++  in  :: code to token
::    =+  aut
::    |=  inp/quay  ^-  {$send hiss}
::    =+  code=~|(%no-code (~(got by (malt inp)) %code))
::    [%send (request-token-by-code 'https://my-api.com/access_token' code)]
::  ::
::  ++  bak  ::  save token
::    =+  aut
::    |=  bak/httr  ^-  $%({{$redo $~} _..bak} {$give httr})
::    ?:  (bad-response bak)  [%give bak]
::    =.  tok  (grab-token bak)
::    [[%redo ~] ..bak]
::  --
::
:::   :::
  ::::: ::
:::   :::
::
::::  Example "standard-refreshing" sec/ core:
  ::
::
::  ::
::  ::::  /hoon/my-api/com/sec
::    ::
::  /+    oauth2
::  ::
::  ::::
::    ::
::  |_  {bal/(bale keys:oauth2) tok/token:oauth2 ref/refresh:oauth2}
::  ++  aut
::    %^  ~(standard-refreshing oauth2 bal tok)  .  ref
::    |=({tok/token ref/refresh}:oauth2 +>(tok tok, ref ref))
::  ::
::  ++  exchange-url  'https://my-api.com/access_token'
::  ++  out
::    %^  out-refresh-or-add-header:aut  exchange-url
::      scope=/full
::    oauth-dialog='https://my-api.com/authorize'
::  ::
::  ++  res  res-save-after-refresh:aut
::  ++  in  (in-code-to-token:aut exchange-url)
::  ++  bak  bak-save-both-tokens:aut
::  --
::
::
::::  Equivalent imperative code:
  ::
::
::  ::
::  ::::  /hoon/my-api/com/sec
::    ::
::  /+    oauth2
::  ::
::  ::::
::    ::
::  |_  {bal/(bale keys:oauth2) axs/token:oauth2 ref/refresh:oauth2}
::  ++  aut  ~(. oauth2 bal axs)
::  ++  exchange-url  'https://my-api.com/access_token'
::  ++  out  :: refresh or add header
::    =+  aut
::    |=  req/hiss  ^-  $^({{$send hiss} _..out} $%({$send hiss} {$show purl}))
::    ?~  axs
::      [%show (auth-url scope=/full 'https://my-api.com/authorize')]
::    =^  upd  ref  (~(update-if-needed re ref) exchange-url)
::    ?^  upd  [[%send u.upd] ..out]
::    [%send (add-auth-header req)]
::   ::
::  ++  res  :: save after refresh
::    =+  aut
::    |=  a/httr  ^-  $^({{$redo $~} _..res} {$give httr})
::    ?.  pending.ref  [%give a]
::    =+  `{axs/token exp/@u}`(grab-expiring-token a)
::    [[%redo ~] ..out(axs axs, ref (~(update re ref) exp))]
::  ::
::  ++  in  :: exchange token
::    =+  aut
::    |=  inp/quay  ^-  {$send hiss}
::    =+  code=~|(%no-code (~(got by (malt inp)) %code))
::    [%send (request-token-by-code exchange-url code)]
::
::   ++  bak  :: save both tokens
::     =+  aut
::     |=  a/httr  ^-  {{$redo $~} _..res}
::     =+  `{axs/token exp/@u ref-new/token}`(grab-both-tokens a)
::     =.  tok.ref  ref-new
::     [[%redo ~] ..bak(axs axs, ref (~(update re ref) exp))]
::  ::
::  ::
::  ++  bak
::    =+  aut
::    |=  bak/httr  ^-  $%({{$redo $~} _..bak} {$give httr})
::    ?:  (bad-response bak)  [%give bak]
::    =.  tok  (grab-token bak)
::    [[%redo ~] ..bak]
::  --
::
