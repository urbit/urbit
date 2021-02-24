/-  *gcp
|%
++  token-to-json
  |=  =token
  ^-  json
  =,  enjs:format
  %+  frond  %gcp-token
  %:  pairs
    access+s+access.token
    expiry+(time expiry.token)
  ==
--
