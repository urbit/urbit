::  OAuth 1.0 %authorization header
::
::::  /hoon/oauth1/lib
  ::
|%
++  keys
  $:  con/{tok/@t sec/@t}                             ::  user key pair
      acc/{tok/@t sec/@t}                             ::  app key pair
  ==
++  quay-enc  (list tape):quay        ::  partially rendered query string
--
::
::::
  ::
|%
++  join                                              ::  between every pair
  |=  {a/tape b/wall}  ^-  tape
  ?~(b b |-(?~(t.b i.b :(weld i.b a $(b t.b)))))
::
++  join-urle  |=(a/(list tape) (join "&" (turn a urle)))
::   query string in oauth1 'k1="v1", k2="v2"' form
++  to-header
  |=  a/quay  ^-  tape
  %+  join  ", "
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
--
::
::::
  ::
|=  {key/keys now/time eny/@}
=<  apex
|%
++  apex
  |=  hiz/{purl meth hed/math (unit octs)}  ^-  hiss
  %_  hiz
    hed  (~(add ja hed.hiz) %authorization (authorization hiz))
  ==
++  authorization
  |=  {url/purl med/meth math bod/(unit octs)}
  =^  quy  url  [r.url url(r ~)]      :: query string handled separately
  =+  aut=auth-quay
  =+  ^-  qen/quay-enc                 :: semi-encoded for sorting
      %+  weld  (parse-pairs bod)
      (encode-pairs (weld aut quy))
  =+  hds=(base-string med url qen)
  =+  sig=(sign hds)
  =.  aut  ['oauth_signature'^(crip (urle sig)) aut]
  (crip "OAuth {(to-header aut)}")
::
++  auth-quay
  ^-  quay
  :~  'oauth_consumer_key'^tok.con.key
      'oauth_nonce'^(scot %uw (shaf %non eny))
      'oauth_signature_method'^'HMAC-SHA1'
      'oauth_timestamp'^(rsh 3 2 (scot %ui (unt now)))
      'oauth_token'^tok.acc.key
      'oauth_version'^'1.0'
  ==
++  base-string
  |=  {med/meth url/purl qen/quay-enc}  ^-  tape
  =.  qen  (sort qen aor)
  %-  join-urle
  :~  (trip (cuss (trip `@t`med)))
      (earn url)
      (join "&" qen)
  ==
++  sign
  |=  bay/tape  ^-  tape
  =+  sky=(crip (join-urle (trip sec.con.key) (trip sec.acc.key) ~))
  (sifo (swap 3 (hmac (swap 3 sky) (crip bay))))
--
