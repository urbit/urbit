::  OAuth 2.0 %authorization
::
::::  /hoon/oauth2/lib
  ::
/+    hep-to-cab, interpolate
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
  (turn (lore (crip b)) |=(c/cord leaf+(trip c)))
::
++  bad-response  |=(a/@u ?:(=(2 (div a 100)) | ~&(bad-httr+a &)))
++  grab-json
  |*  {a/httr b/fist:jo}
  ~|  bad-json+r.a
  ~|  (poja q:(need r.a))
  (need (;~(biff poja b) q:(need r.a)))
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
    ((hard {cid/@t cis/@t $~}) (lore key))
  %+  mean-wall  %oauth-no-keys
  """
  Run |init-oauth2 {<`path`dom>}
  If necessary, obtain client keys configured for a redirect_uri of
    {(trip redirect-uri)}
  """
::
++  auth-url
  |=  {scopes/(list @t) url/$@(@t purl)}  ^-  purl
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
  ~&  [%oauth-warning "Make sure this urbit ".
                      "is running on {(earn our-host `~ ~)}"]
  %-    crip    %-  earn
  %^  interpolate  'https://our-host/~/ac/:domain/:user/in'
    `our-host
  :~  domain+(join '.' (flop dom))
      user+?:(state-usr '_state' (scot %ta usr))
  ==
::
::
++  token-request
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
++  grab-token
  |=  a/httr  ^-  token
  (grab-json a (ot 'access_token'^so ~):jo)
::
++  grab-token-after-refresh
  |=  a/httr  ^-  {exp/@u axs/token}
  (grab-json a (ot 'expires_in'^ni 'access_token'^so ~):jo)
::
++  grab-refresh-token
  |=  a/httr  ^-  {exp/@u ref/token axs/token}
  (grab-json a (ot 'expires_in'^ni 'refresh_token'^so 'access_token'^so ~):jo)
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
  ::  =.  url.request  [| `6.000 [%& /localhost]]       ::  for use with unix nc
  ~&  add-auth-header+(earn url.request)
  request(hed (~(add ja hed.request) %authorization header:auth))
::
++  add-auth-query
  |=  {token-name/cord request/{url/purl meth math (unit octs)}}
  ^+  request
  ::  =.  url.request  [| `6.000 [%& /localhost]]       ::  for use with unix nc
  ~&  add-auth-query+(earn url.request)
  request(r.url [[token-name query:auth] r.url.request])
::
++  re
  |_  ref/refresh
  ++  needs-refresh  ?~(tok.ref | is-expired)
  ++  is-expired  (lth expiry.ref (add now ~m59.s30))
  ++  update
    |=  exp/@u  ^+  ref
    ref(pending |, expiry (add now (mul ~s1 exp)))
  --
::
++  standard
  |*  {done/* save/$-(token *)}                         ::  save/$-(token _done)
  |%
  ++  core-move  $^({sec-move _done} sec-move)          ::  stateful
  ::
  ++  out-add-query-param
    |=  {token-name/knot scopes/(list cord) dialog/$@(@t purl)}
    ::
    |=  a/hiss  ^-  $%({$send hiss} {$show purl})
    ?~  tok  [%show (auth-url scopes dialog)]
    [%send (add-auth-query token-name a)]
  ::
  ++  out-add-header
    |=  {scopes/(list cord) dialog/$@(@t purl)}
    ::
    |=  a/hiss  ^-  sec-move
    ?~  tok  [%show (auth-url scopes dialog)]
    [%send (add-auth-header a)]
  ::
  ++  in-code-to-token
    |=  exchange-url/$@(@t purl)
    ::
    |=  a/quay  ^-  sec-move
    =+  code=~|(%no-code (~(got by (malt a)) %code))
    [%send (token-request exchange-url 'authorization_code' code+code ~)]
  ::
  ++  bak-save-token
    |=  a/httr  ^-  core-move
    ?:  (bad-response p.a)  
      [%give a]  :: [%redo ~]  ::  handle 4xx?
    [[%redo ~] (save `token`(grab-token a))]
  --
::
++  standard-refreshing
  |*  {done/* ref/refresh save/$-({token refresh} *)}   ::  $-(both-tokens _done)
  =+  s=(standard done |=(tok/token (save tok ref)))
  |%
  ++  core-move  $^({sec-move _done} sec-move)          ::  stateful
  ::
  ::  See ++out-add-query-param:standard
  ++  out-refresh-or-add-query-param
    |=  {exchange/$@(@t purl) {knot (list cord) $@(@t purl)}}
    ?.  ~(needs-refresh re ref)  (out-add-query-param.s +<+)
    =;  exchange  [[%send exchange] (save tok ref(pending &))]
    (token-request exchange 'refresh_token' refresh-token+tok.ref ~)
  ::
  ::  See ++out-add-header:standard
  ++  out-refresh-or-add-header
    |=  {exchange/$@(@t purl) {(list cord) dialog/$@(@t purl)}}
    ?.  ~(needs-refresh re ref)  (out-add-header.s +<+)
    =;  exchange  [[%send exchange] (save tok ref(pending &))]
    (token-request exchange 'refresh_token' refresh-token+tok.ref ~)
  ::
  ++  res-handle-refreshed
    |=  a/httr  ^-  core-move
    ?.  pending.ref  [%give a]
    =+  `{exp/@u axs/@t}`(grab-token-after-refresh a)
    =.  ref  %.(exp ~(update re ref))
    [[%redo ~] (save axs ref)]
  ::
  ++  in-code-to-token  in-code-to-token.s
  ++  bak-save-both-tokens
    |=  a/httr  ^-  sec-move
    =+  `{exp/@u axs/@t ref-new/@t}`(grab-refresh-token a)
    =.  tok.ref  ref-new
    =.  ref  (~(update re ref) exp)
    [[%redo ~] (save axs ref)]
  --
--
