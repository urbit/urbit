::  OAuth 1.0 %authorization header
::
::::  /hoon/oauth1/lib
  ::
|%
++  keys
  $:  con/{tok/@t sec/@t}                             ::  user key pair
      acc/{tok/@t sec/@t}                             ::  app key pair
  ==
++  join                                              ::  between every pair
  |=  {a/tape b/wall}  ^-  tape
  ?~(b b |-(?~(t.b i.b :(weld i.b a $(b t.b)))))
::
::   query string in oauth1 'k1="v1", k2="v2"' form
++  to-header
  |=  a/quay  ^-  tape
  %+  join  ", "
  (turn a |=({k/@t v/@t} `tape`~[k '="' v '"']))      :: normalized later
::
::   partial tail:earn for sorting
++  encode-pairs
  |=  a/quay  ^-  (list tape)
  %+  turn  a
  |=  {k/@t v/@t}  ^-  tape
  :(weld (urle (trip k)) "=" (urle (trip v)))
--
::
::::
  ::
|=  $:  med/meth 
        url/purl 
        quy/quay
        key/keys
        now/time 
        eny/@
    ==
^-  @t
=+  ^-  aut-quy/quay
    :~  'oauth_consumer_key'^tok.con.key
        'oauth_nonce'^(scot %uw (shaf %non eny))
        'oauth_signature_method'^'HMAC-SHA1'
        'oauth_timestamp'^(rsh 3 2 (scot %ui (unt now)))
        'oauth_token'^tok.acc.key
        'oauth_version'^'1.0'
    ==
=+  ^=  bas  
    ^-  tape
    =+  hds=(sort (encode-pairs (weld quy aut-quy)) aor)
    %+  join  "&"
    :~  (trip (cuss (trip `@t`med)))
        (urle (earn url))
        (urle (join "&" hds))
    ==
=+  sky=(crip :(weld (urle (trip sec.con.key)) "&" (urle (trip sec.acc.key))))
=+  sig=`tape`(sifo (swap 3 (hmac (swap 3 sky) (crip bas))))
%-  crip
%+  weld  "OAuth "
%+  to-header
  'oauth_signature'^(crip (urle sig))
aut-quy
