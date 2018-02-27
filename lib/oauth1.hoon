::  OAuth 1.0 %authorization header
::
::::  /hoon/oauth1/lib
  ::
/+    interpolate, hep-to-cab
=,  mimes:html
=,  eyre
|%
++  keys  cord:{key/@t sec/@t}                          ::  app key pair
++  token                                               ::  user keys
  $@  $~                                                ::  none
  $%  {$request-token oauth-token/@t token-secret/@t}   ::  intermediate
      {$access-token oauth-token/@t token-secret/@t}    ::  full
  ==
++  quay-enc  (list tape)              ::  partially rendered query string
--
::
::::
  ::
|%
++  parse-url  parse-url:interpolate
++  join  
  |=  {a/cord b/(list cord)}
  ?~  b  ''
  (rap 3 |-([i.b ?~(t.b ~ [a $(b t.b)])]))
::
++  joint                                              ::  between every pair
  |=  {a/tape b/wall}  ^-  tape
  ?~  b  b
  |-  ^-  tape
  ?~  t.b  i.b
  :(weld i.b a $(b t.b))
::
++  join-en-urle  |=(a/(list tape) (joint "&" (turn a en-urlt:html)))
::   query string in oauth1 'k1="v1", k2="v2"' form
++  to-header
  |=  a/quay  ^-  tape
  %+  joint  ", "
  (turn a |=({k/@t v/@t} `tape`~[k '="' v '"']))        ::  normalized later
::
::   partial tail:en-purl:html for sorting
++  encode-pairs
  |=  a/quay  ^-  quay-enc
  %+  turn  a
  |=  {k/@t v/@t}  ^-  tape
  :(weld (en-urlt:html (trip k)) "=" (en-urlt:html (trip v)))
::
++  parse-pairs                                         ::  x-form-en-urlt:htmlncoded
  |=  bod/(unit octs)  ^-  quay-enc
  ~|  %parsing-body
  ?~  bod  ~
  (rash q.u.bod (more pam (plus ;~(less pam prn))))
::
++  post-quay
  |=  {a/purl b/quay}  ^-  hiss
  =.  b  (quay:hep-to-cab b)
  =-  [a %post - ?~(b ~ (some (as-octt +:(tail:en-purl:html b))))]
  (my content-type+['application/x-www-form-urlencoded']~ ~)
::
::
++  mean-wall  !.
  |=  {a/term b/tape}  ^+  !!
  =-  (mean (flop `tang`[>a< -]))
  (turn (to-wain:format (crip b)) |=(c/cord leaf+(trip c)))
::
++  bad-response  |=(a/@u ?:(=(2 (div a 100)) | ~&(bad-httr+a &)))
++  quay-keys  |-($@(knot {$ $}))  :: improper tree
++  grab-quay  :: ?=({@t @t @t} (grab-quay r:*httr %key1 %key2 %key3))
  |*  {a/(unit octs) b/quay-keys}
  =+  ~|  bad-quay+a
      c=(rash q:(need `(unit octs)`a) yquy:de-purl:html)
  ~|  grab-quay+[c b]
  =+  all=(malt c)
  %.  b
  |*  b/quay-keys
  ?@  b  ~|(b (~(got by all) b))
  [(..$ -.b) (..$ +.b)]
--
::
::::
  ::
|_  {(bale keys) tok/token}
++  consumer-key     key:decode-keys
++  consumer-secret  sec:decode-keys
++  decode-keys                       :: XX from bale w/ typed %jael
  ^-  {key/@t sec/@t $~}
  ?.  =(~ `@`key)
    ~|  %oauth-bad-keys
    ((hard {key/@t sec/@t $~}) (to-wain:format key))
  %+  mean-wall  %oauth-no-keys
  """
  Run |init-oauth1 {<`path`dom>}
  If necessary, obtain consumer keys configured for a oauth_callback of
    {(trip oauth-callback)}
  """
::
++  exchange-token
  |=  a/$@(@t purl)  ^-  hiss
  (post-quay (parse-url a) ~)
::
++  request-token
  |=  a/$@(@t purl)  ^-  hiss
  (post-quay (parse-url a) oauth-callback+oauth-callback ~)
::
++  our-host  .^(hart %e /(scot %p our)/host/fake)
++  oauth-callback
  ~&  [%oauth-warning "Make sure this urbit ".
                      "is running on {(en-purl:html our-host `~ ~)}"]
  %-    crip    %-  en-purl:html
  %^  into-url:interpolate  'https://our-host/~/ac/:domain/:user/in'
    `our-host
  :~  domain+(join '.' (flop dom))
      user+(scot %ta usr)
  ==
::
++  auth-url
  |=  url/$@(@t purl)  ^-  purl
  %+  add-query:interpolate  url
  %-  quay:hep-to-cab
  ?.  ?=({$request-token ^} tok)
    ~|(%no-token-for-dialog !!)
  :-  oauth-token+oauth-token.tok
  ?~(usr ~ [screen-name+usr]~)
::
++  grab-token-response
  |=  a/httr  ^-  {tok/@t sec/@t}
  (grab-quay r.a 'oauth_token' 'oauth_token_secret')
::
++  identity
  %+  weld
    ?~(usr "default identity for " "{(trip usr)}@")
  (trip (join '.' (flop dom)))
::
++  check-screen-name
  |=  a/httr  ^-  ?
  =+  nam=(grab-quay r.a 'screen_name')
  ?~  usr  &
  ?:  =(usr nam)  &
  =<  |
  %-  %*(. slog pri 1)
  ::  XX cgyarvin should figure out why we need to cast to $~
  (flop p:(mule |.(~|(wrong-user+[req=usr got=nam] `$~`!!))))
::
++  check-token-quay
  |=  a/quay  ^+  %&
  =.  a  (sort a aor)
  ?.  ?=({{$'oauth_token' oauth-token/@t} {$'oauth_verifier' @t} $~} a)
    ~|(no-token+a !!)
  ?~  tok
    %+  mean-wall  %no-secret-for-token
    """
    Attempting to authorize {identity}
    """
  ?.  =(oauth-token.tok oauth-token.q.i.a)
    ~|  wrong-token+[id=usr q.i.a]
    ~|(%multiple-tokens-unsupported !!)
  %&
::
++  auth
  |%  
  ++  header
    |=  {auq/quay url/purl med/meth math bod/(unit octs)}
    ^-  cord
    =^  quy  url  [r.url url(r ~)]      :: query string handled separately
    =.  auq  (quay:hep-to-cab (weld auq computed-query))
    =+  ^-  qen/quay-enc                 :: semi-encoded for sorting
        %+  weld  (parse-pairs bod)
        (encode-pairs (weld auq quy))
    =+  bay=(base-string med url qen)
    =+  sig=(sign signing-key bay)
    =.  auq  ['oauth_signature'^(crip (en-urlt:html sig)) auq]
    (crip "OAuth {(to-header auq)}")
  ::
  ++  computed-query
    ^-  quay
    :~  oauth-consumer-key+consumer-key
        oauth-nonce+(scot %uw (shaf %non eny))
        oauth-signature-method+'HMAC-SHA1'
        oauth-timestamp+(rsh 3 2 (scot %ui (unt:chrono:userlib now)))
        oauth-version+'1.0'
    ==
  ++  base-string
    |=  {med/meth url/purl qen/quay-enc}  ^-  tape
    =.  qen  (sort qen aor)
    %-  join-en-urle
    :~  (cuss (trip `@t`med))
        (en-purl:html url)
        (joint "&" qen)
    ==
  ++  sign
    |=  {key/cord bay/tape}  ^-  tape
    (en-base64:mimes:html (swp 3 (hmac:crypto key (crip bay))))
  ::
  ++  signing-key
    %-  crip
    %-  join-en-urle  :~
      (trip consumer-secret)
      (trip ?^(tok token-secret.tok ''))
    ==
  --
::
++  add-auth-header
  |=  {extra/quay request/{url/purl meth hed/math (unit octs)}}
  ^-  hiss
  ::  =.  url.request  [| `6.000 [%& /localhost]]       ::  for use with unix nc
  ~&  add-auth-header+(en-purl:html url.request)
  %_    request
      hed
    (~(add ja hed.request) %authorization (header:auth extra request))
  ==
::  expected semantics, to be copied and modified if anything doesn't work
++  standard
  |*  {done/* save/$-(token *)}                         ::  save/$-(token _done)
  |%
  ++  save  ^-($-(token _done) ^save)                   ::  shadow(type canary)
  ++  core-move  $^({sec-move _done} sec-move)          ::  stateful
  ::
  ::  use token to sign authorization header. expects:
  ::    ++  res  res-handle-request-token               ::  save request token
  ::    ++  in   (in-token-exhange 'http://...')        ::  handle callback
  ++  out-add-header
    |=  {request-url/$@(@t purl) dialog-url/$@(@t purl)}
    ::
    |=  a/hiss  ^-  $%({$send hiss} {$show purl})
    ?-    tok
        $~
      [%send (add-auth-header ~ (request-token request-url))]
    ::
        {$access-token ^}
      [%send (add-auth-header [oauth-token+oauth-token.tok]~ a)]
    ::
        {$request-token ^}
      [%show (auth-url dialog-url)]
    ==
  ::
  ::  If no token is saved, the http response we just got has a request token
  ++  res-handle-request-token
    |=  a/httr  ^-  core-move
    ?^  tok  [%give a]
    ?.  =(%true (grab-quay r.a 'oauth_callback_confirmed'))
      ~|(%callback-rejected !!)    
    =+  request-token=(grab-token-response a)
    [[%redo ~] (save `token`[%request-token request-token])]
  ::
  ::  Exchange oauth_token in query string for access token. expects:
  ::    ++  bak  bak-save-token                         :: save access token
  ++  in-exchange-token
    |=  exchange-url/$@(@t purl)
    ::
    |=  a/quay  ^-  sec-move
    ?>  (check-token-quay a)
    [%send (add-auth-header a (exchange-token exchange-url))]
  ::
  ::  If a valid access token has been returned, save it
  ++  bak-save-token
    |=  a/httr  ^-  core-move
    ?:  (bad-response p.a)
      [%give a]  :: [%redo ~]  ::  handle 4xx?
    ?.  (check-screen-name a)
      [[%redo ~] (save `token`~)]
    =+  access-token=(grab-token-response a)
    [[%redo ~] (save `token`[%access-token access-token])]
  --
--
::
::::  Example "standard" sec/ core:
  ::
::
::  :: 
::  ::::  /hoon/my-api/com/sec
::    ::
::  /+    oauth1
::  ::
::  ::::
::    ::
::  |_  {bal/(bale keys:oauth1) tok/token:oauth1}
::  ++  aut  (~(standard oauth1 bal tok) . |=(tok/token:oauth1 +>(tok tok)))
::  ++  out
::    %+  out-add-header:aut
::      request-token='https://my-api.com/request_token'
::    oauth-dialog='https://my-api.com/authorize'
::  ::
::  ++  res  res-handle-request-token:aut
::  ++  in
::    %-  in-exchagne-token:aut
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
::  /+    oauth1
::  ::
::  ::::
::    ::
::  |_  {bal/(bale keys:oauth1) tok/token:oauth1}
::  ++  aut  ~(. oauth1 bal tok)
::  ++  out  ::  add header
::    =+  aut
::    |=  req/hiss  ^-  $%({$send hiss} {$show purl})
::    ?~  tok
::      [%send (add-auth-header ~ (request-token 'https://my-api.com/request_token'))]
::    ?:  ?=($request-token -.tok)
::      [%show (auth-url 'https://my-api.com/authorize')]
::    [%send (add-auth-header [oauth-token+ouath-token.tok]~ req)]
::  ::
::  ++  res  :: handle request token
::    =+  aut
::    |=  res/httr  ^-  $%({{$redo $~} _..res} {$give httr})
::    ?^  tok  [%give a]
::    ?>  =(%true (grab r.res 'oauth_callback_confirmed'))
::    =.  tok  [%request-token (grab-token-response res)]
::    [[%redo ~] ..res]
::  ::
::  ++  in  ::  exchange token
::    =+  aut
::    |=  inp/quay  ^-  {$send hiss}
::    ?>  (check-token-quay inp)
::    :-  %send
::    (add-auth-header inp (exchange-token 'https://my-api.com/access_token'))
::  ::
::  ++  bak  ::  save token
::    =+  aut
::    |=  bak/httr  ^-  $%({{$redo $~} _..bak} {$give httr})
::    ?:  (bad-response bak)  [%give bak]
::    =.  tok  [%access-token (grab-token-response res)]
::    [[%redo ~] ..bak]
::  --
::
