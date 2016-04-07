::  OAuth 2.0 %authorization
::
::::  /hoon/oauth2/lib
  ::
/+    hep-to-cab, interpolate
|%
++  join  
  |=  {a/cord b/(list cord)}
  ?~  b  ''
  (rap 3 |-([i.b ?~(t.b ~ [a $(b t.b)])]))
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
::
++  parse-url  parse-url:interpolate
--
::
::::
  ::
|%
++  token  ?($~ @t)
++  refresh  {tok/token needed/@da pending/_`?`|}
++  keys  cord:{cid/@t cis/@t}
++  core-move  |*(a/* $^({sec-move _a} sec-move)) ::here's a change
--
::
::::
  ::
|=  {dialog/$@(cord:purl purl) code-exchange/$@(cord:purl purl)}
=+  :+  state-usr=|
      dialog-url=(parse-url dialog)
    exchange-url=(parse-url code-exchange)
|_  {(bale keys) scope/(list cord)}
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
++  our-host  .^(hart %e /(scot %p our)/host/fake)
++  auth-url
  ~&  [%oauth-warning "Make sure this urbit ".
                      "is running on {(earn our-host `~ ~)}"]
  ^-  purl
  %_    dialog-url
      r
    %+  welp  r.dialog-url
    %-  quay:hep-to-cab  
    :~  state+?.(state-usr '' (pack usr /''))
        client-id+client-id
        redirect-uri+redirect-uri
        scope+(join ' ' scope)
    ==
  ==
::
++  redirect-uri
  %-    crip    %-  earn
  %^  interpolate  'https://our-host/~/ac/:domain/:user/in'
    `our-host
  :~  domain+(join '.' (flop dom))
      user+?:(state-usr '_state' (scot %ta usr))
  ==
::
::
++  out-filtered
  |=  {tok/token aut/$-(hiss hiss)}
  |=  a/hiss  ^-  sec-move
  ?~(tok [%show auth-url] [%send (aut a)])
::
++  out-quay
  |=  {nam/knot tok/token}
  %+  out-filtered  tok
  |=  a/hiss  ^-  hiss
  :: =.  p.p.a   [| `6.000 [%& /localhost]]             ::  for use with unix nc
  %_(a r.p :_(r.p.a nam^`@t`tok))
::
++  out-math
  |=  ber/token
  =+  hed=(cat 3 'Bearer ' `@t`ber)
  %+  out-filtered  ber
  |=  a/hiss  ^+  a
  :: =.  p.p.a   [| `6.000 [%& /localhost]]             ::  for use with unix nc
  %_(a q.q (~(add ja q.q.a) %authorization hed))
::
++  toke-req
  |=  {grant-type/cord quy/quay}  ^-  {$send hiss}
  :+  %send  exchange-url
  :+  %post  (malt ~[content-type+~['application/x-www-form-urlencoded']])
  =-  `(tact +:(tail:earn -))
  %-  quay:hep-to-cab
  %+  welp  quy
  :~  client-id+client-id
      client-secret+client-secret
      redirect-uri+redirect-uri
      grant-type+grant-type
  ==
::
++  in-code
  |=  a/quay  ^-  sec-move
  =+  code=~|(%no-code (~(got by (malt a)) %code))
  (toke-req 'authorization_code' code+code ~)
::
++  token-type  'token_type'^(cu cass sa):jo
++  expires-in  'expires_in'^ni:jo
++  access-token  'access_token'^so:jo
++  refresh-token  'refresh_token'^so:jo
++  bak-save-access
  |*  {done/* handle/$-(cord:token *)}  :: $+(token _done)
  %-  (bak-parse done access-token ~)
  |=(tok/cord:token [[%redo ~] (handle tok)])
::
++  bak-parse
  |*  {done/* parse/(pole {knot fist}:jo)}
  |=  handle/$-(_?~(parse ~ (need *(ot:jo parse))) (core-move done))
  |=  a/httr  ^-  (core-move done)
  ?:  (bad-response p.a)
    [%give a]
    :: [%redo ~]  ::  handle 4xx?
  (handle (grab-json a (ot:jo parse)))
::
++  res-give  |=(a/httr [%give a])
::
++  re
  |*  cor/*           :: XX redundant with *export, but type headaches
  |_  {ref/refresh export/$-(refresh _cor)}
  ++  out-fix-expired
    |=  default/$-(hiss sec-move)
    ^-  $-(hiss (core-move cor))
    ?~  tok.ref  default
    ?.  (lth needed.ref (add now ~m59.s30))
      default
    |=  a/hiss
    :_  (export ref(pending &))
    (toke-req 'refresh_token' refresh-token+tok.ref ~)
  ::
  ++  res-handle-refreshed
    |=  {handle-access/_=>(cor |=(@t +>)) default/$-(httr sec-move)}
    ^-  $-(httr (core-move cor))
    ?.  pending.ref  default
    %-  (bak-parse cor expires-in access-token ~)
    |=  {exp/@u tok/axs/@t}  ^-  {sec-move _cor}
    =.  +>.handle-access
      (export tok.ref (add now (mul ~s1 exp)) |)
    [[%redo ~] (handle-access axs.tok)]
  ::
  ++  bak-save-tokens
    |=  handle-access/_=>(cor |=(@t +>))
    %-  (bak-parse cor expires-in access-token refresh-token ~)
    |=  {exp/@u tok/{axs/@t ref/@t}}  ^-  {sec-move _cor}
    =.  +>.handle-access
      (export ref.tok (add now (mul ~s1 exp)) |)
    [[%redo ~] (handle-access axs.tok)]
  --
--
