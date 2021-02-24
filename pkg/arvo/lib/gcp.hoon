/-  *gcp
|%
++  token-to-json
  |=  =token
  ^-  json
  =,  enjs:format
  %+  frond  %gcp-token
  %:  pairs
    access-key+s+access-key.token
    :-  %expires-in
    %-  numb
    (div (mul 1.000 expires-in.token) ~s1)
    ~
  ==
--
