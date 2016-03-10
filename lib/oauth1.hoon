::  OAuth 1.0 %authorization header
::
::::  /hoon/oauth1/lib
  ::
|%
++  keys  cord:{key/@t sec/@t}                          ::  app key pair
++  token  $@(req/@t {pub/@t sec/@t})                   ::  pending/authorized
++  quay-enc  (list tape):quay        ::  partially rendered query string
--
::
::::
  ::
|%
++  fass                                                ::  rewrite quay
  |=  a/quay
  %+  turn  a
  |=  {p/@t q/@t}  ^+  +<
  [(gsub '-' '_' p) q]
::
++  gsub                                                ::  replace chars
  |=  {a/@t b/@t t/@t}
  ^-  @t
  ?:  =('' t)  t
  %+  mix  (lsh 3 1 $(t (rsh 3 1 t)))
  =+  c=(end 3 1 t)
  ?:(=(a c) b c)
::
++  join  
  |=  {a/cord b/(list cord)}
  ?~  b  ''
  (rap 3 |-([i.b ?~(t.b ~ [a $(b t.b)])]))
::
++  joint                                              ::  between every pair
  |=  {a/tape b/wall}  ^-  tape
  ?~(b b |-(?~(t.b i.b :(weld i.b a $(b t.b)))))
::
++  join-urle  |=(a/(list tape) (joint "&" (turn a urle)))
::   query string in oauth1 'k1="v1", k2="v2"' form
++  to-header
  |=  a/quay  ^-  tape
  %+  joint  ", "
  (turn a |=({k/@t v/@t} `tape`~[k '="' v '"']))      ::  normalized later
::
::   partial tail:earn for sorting
++  encode-pairs
  |=  a/quay  ^-  quay-enc
  %+  turn  a
  |=  {k/@t v/@t}  ^-  tape
  :(weld (urle (trip k)) "=" (urle (trip v)))
::
++  parse-pairs                                       ::  x-form-urlencoded
  |=  bod/(unit octs)  ^-  quay-enc
  ~|  %parsing-body
  ?~  bod  ~
  (rash q.u.bod (more pam (plus ;~(less pam prn))))
::
::
++  mean-wall  !.
  |=  {a/term b/tape}  ^+  !!
  =-  (mean (flop `tang`[>a< -]))
  (turn (lore (crip b)) |=(c/cord leaf+(trip c)))
::
++  dbg-post  `purl`[`hart`[| `6.000 [%& /localhost]] `pork``/testing `quay`/]
++  bad-response  |=(a/@u ?:(=(2 (div a 100)) | ~&(bad-httr+a &)))
++  quay-keys  |-($@(knot {$ $}))  :: improper tree
++  grab-quay  :: ?=({@t @t @t} ((grab-quay *httr) %key1 %key2 %key3))
  |=  a/httr
  ~|  bad-quay+r.a
  =+  quy=(rash q:(need r.a) yquy:urlp)
  ~|  quy
  =+  all=(malt quy)
  |*  b/quay-keys
  ?@  b  ~|(b (~(got by all) b))
  [(..$ -.b) (..$ +.b)]
::
++  parse-url
  |=  a/$@(cord:purl purl)  ^-  purl
  ?^  a  a
  ~|  bad-url+a
  (rash a auri:epur)
::
++  interpolate-url
  |=  {a/$@(cord purl) b/(unit hart) c/(list (pair term knot))}
  ^-  purl
  ?@  a  $(a (parse-url a))  :: deal with cord
  %_  a
    p    ?^(b u.b p.a)
    q.q  (interpolate-path q.q.a c)
  ==
::
++  interpolate-path    ::  [/a/:b/c [%b 'foo']~] -> /a/foo/c
  |=  {a/path b/(list (pair term knot))}  ^-  path
  ?~  a  ?~(b ~ ~|(unused-values+b !!))
  =+  (rush i.a ;~(pfix col sym))
  ?~  -  [i.a $(a t.a)]  ::  not interpolable
  ?~  b  ~|(no-value+u !!)
  ?.  =(u p.i.b)  ~|(mismatch+[u p.i.b] !!)
  [q.i.b $(a t.a, b t.b)]
--
!:
::::
  ::
|=  {request/$@(@t purl) dialog/$@(@t purl) code-exchange/$@(@t purl)}
=+  :+  dialog-url=(parse-url dialog)
      exchange-url=(parse-url code-exchange)
    token-reqs-url=(parse-url request)
|_  {done/* (bale keys) oauth-token/token}
++  core-move  $^({sec-move _done} sec-move)  :: stateful
++  consumer-key     key:decode-keys
++  consumer-secret  sec:decode-keys
++  decode-keys                       :: XX from bale w/ typed %jael
  ^-  {key/@t sec/@t $~}
  ?.  =(~ `@`key)
    ~|  %oauth-bad-keys
    ((hard {cid/@t cis/@t $~}) (lore key))
  %+  mean-wall  %oauth-no-keys
  """
  Run |init-oauth1
  If necessary, obtain consumer keys configured for a oauth_callback of
    {(trip oauth-callback)}
  """
::
++  our-host  .^(hart %e /(scot %p our)/host/fake)
++  oauth-callback
  ~&  [%oauth-warning "Make sure this urbit ".
                      "is running on {(earn our-host `~ ~)}"]
  %-    crip    %-  earn
  %^  interpolate-url  'https://our-host/~/ac/:domain/:user/in'
    `our-host
  :~  domain+(join '.' (flop dom))
      user+(scot %ta usr)
  ==
::
::
:: post with blank user-secret, used in token exhcange flow
++  post-no-secret                    
  |=  {url/purl auq/quay}  ^-  {$send hiss}
  ?^  oauth-token  ~|(%should-use-token !!)
  [%send (add-auth '' auq url %post *math ~)]
::
++  toke-url
  |=  quy/quay  ^-  purl
  %_  dialog-url
    r  (fass ?~(usr quy [screen-name+usr quy]))
  ==
::
::  use token to sign authorization header. requires:
::    ++  res  (res-handle-reqt handle-token)      :: take request token
::    ++  bak  (res-save-access handle-token)      :: obtained access token
++  out-math
  ^-  $-(hiss $%({$send hiss} {$show purl}))
  ?:  =('' oauth-token)  :: XX ?~
    _(post-no-secret token-reqs-url oauth-callback+oauth-callback ~)
  ?@  oauth-token
    _[%show (toke-url oauth-token+req.oauth-token ~)]
  =+  auq=[oauth-token+pub.oauth-token]~
  |=  a/hiss  ^-  {$send hiss}
  [%send (add-auth sec.oauth-token auq a)]
::
++  in-oauth-token
  |=  a/quay  ^-  sec-move
  ~|  no-token+a
  ?>  ?=({{$'oauth_token' @} $~} a)
  (post-no-secret exchange-url a)
::
+-  bak-save-access
  |=  handle/$-(token _done)
  %-  (res-parse 'oauth_token' 'oauth_secret')
  |=(tok/{@t @t} [[%redo ~] (handle `token`tok)])
::
+-  res-parse
  |*  para/quay-keys
  |=  handle/$-(_para core-move)  :: same structure of values as keys
  |=  a/httr  ^-  core-move
  ?:  (bad-response p.a)
    [%give a]
    :: [%redo ~]  ::  handle 4xx?
  (handle ((grab-quay a) para))
::
++  res-give  |=(a/httr [%give a])
+-  res-handle-reqt
  |=  handle/$-(token _done)
  ?~  oauth-token
    (res-save-reqt . handle)
  res-give
::
+-  res-save-reqt
  |=  handle/$-(token _done)
  %-  (res-parse 'oauth_token' 'oauth_callback_confirmed')
  |=  {tok/@t cof/term}  ^-  core-move
  ?.  =(%true cof)
    ~|(%callback-rejected !!)
  [[%redo ~] (handle tok)]
::
::
++  add-auth
  |=  $:  token-secret/@t
          auq/quay                    :: extra oauth parameters
          hiz/{purl meth hed/math (unit octs)}
      ==
  ^-  hiss
  =<  %_  hiz
        hed  (~(add ja hed.hiz) %authorization authorization)
      ==
  |%  
  ++  authorization
    =+  [url med ~ bod]=hiz
    =^  quy  url  [r.url url(r ~)]      :: query string handled separately
    =.  auq  (fass (weld auq auth-quay))
    =+  ^-  qen/quay-enc                 :: semi-encoded for sorting
        %+  weld  (parse-pairs bod)
        (encode-pairs (weld auq quy))
    =+  hds=(base-string med url qen)
    =+  sig=(sign hds)
    =.  auq  ['oauth_signature'^(crip (urle sig)) auq]
    (crip "OAuth {(to-header auq)}")
  ::
  ++  auth-quay
    ^-  quay
    :~  oauth-consumer-key+consumer-key
        oauth-nonce+(scot %uw (shaf %non eny))
        oauth-signature-method+'HMAC-SHA1'
        oauth-timestamp+(rsh 3 2 (scot %ui (unt now)))
        oauth-version+'1.0'
    ==
  ++  base-string
    |=  {med/meth url/purl qen/quay-enc}  ^-  tape
    =.  qen  (sort qen aor)
    %-  join-urle
    :~  (trip (cuss (trip `@t`med)))
        (earn url)
        (joint "&" qen)
    ==
  ++  sign
    |=  bay/tape  ^-  tape
    (sifo (swap 3 (hmac (swap 3 signing-key) (crip bay))))
  ::
  ++  signing-key
    %-  crip
    %-  join-urle  :~
      (trip consumer-secret)
      (trip token-secret)
    ==
  --
--
