::  OAuth 1.0 %authorization header
::
::::  /hoon/oauth1/lib
  ::
|%
++  keys
  $:  con/{tok/@t sec/@t}                             ::  user key pair
      acc/{tok/@t sec/@t}                             ::  app key pair
  ==
--
::
::::
  ::
|=  $:  med/meth 
        url/tape 
        pas/(list tape) 
        key/keys
        now/time 
        eny/@
    ==
^-  @t
=+  non=(scow %uw (shaf %non eny))
=+  tim=(slag 2 (scow %ui (unt now)))
=+  sky=(crip :(weld (urle (trip sec.con.key)) "&" (urle (trip sec.acc.key))))
=+  ^=  bas  
    ^-  tape
    =+  ^=  hds
        =-  (reel (sort - aor) |=({p/tape q/tape} :(weld p "&" q)))
        %+  weld  pas
        ^-  (list tape)
        :~  :(weld "oauth_consumer_key=" (trip tok.con.key))
            :(weld "oauth_nonce=" non)
            :(weld "oauth_signature_method=HMAC-SHA1")
            :(weld "oauth_timestamp=" tim)
            :(weld "oauth_token=" (trip tok.acc.key))
            :(weld "oauth_version=1.0")
        ==
    ;:  weld
      (trip (cuss (trip `@t`med)))  "&"
      (urle url)  "&"
      (urle (scag (dec (lent hds)) `tape`hds))
    ==
=+  sig=`tape`(sifo (swap 3 (hmac (swap 3 sky) (crip bas))))
%-  crip
;:  weld  "OAuth "
    "oauth_consumer_key="  "\""  (trip tok.con.key)  "\", "
    "oauth_nonce="  "\""  non  "\", "
    "oauth_signature="  "\""  (urle sig)  "\", "
    "oauth_signature_method=\"HMAC-SHA1\", "
    "oauth_timestamp="  "\""  tim  "\", "
    "oauth_token="  "\""  (trip tok.acc.key)  "\", "
    "oauth_version=1.0"
==
