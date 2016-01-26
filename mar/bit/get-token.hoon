::
::::  /hoon/bit-get-token/mar
  ::
/?    314
/-    bit-api
/+    http
[bit-api .]
|_  req/bit-get-token
::
++  grow  
  |%  ++  httpreq
    ^-  request:http
    =-  [/com/coinbase/sandbox /oauth/token [%post ~] ~ `quay`-]
    :~  ['grant_type' 'authorization_code']
        ['code' oat.req]
        ['redirect_uri' (crip (earn red.req))]
        ['client_id' cid.req]
        ['client_secret' sec.req]
    ==
  --
--
