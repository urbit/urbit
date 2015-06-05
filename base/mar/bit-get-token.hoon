::
::::  /hoon/bit-get-token/mar
  ::
/?    314
/-    *bit-api
/+    http
|_  req=bit-get-token
::
++  grow  
  |%  ++  httpreq
    =-  [/com/coinbase/sandbox /oauth/token [%post ~] ~ `quay`-]
    :~  ['grant_type' 'authorization_code']
        ['code' oat.req]
        ['redirect_uri' (crip (earn red.req))]
        ['client_id' cid.req]
        ['client_secret' sec.req]
    ==
  --
--
