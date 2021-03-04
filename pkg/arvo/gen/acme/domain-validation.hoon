::  :acme domain validation request handler
::
|=  [[now=@da eny=@ bek=beak] ~ ~]
|=  [authorized=? =request:http]
^-  simple-payload:http
=/  url=(unit (pair pork:eyre quay:eyre))
  %+  rush  url.request
  ;~(plug ;~(pose apat:de-purl:encoding (easy *pork:eyre)) yque:de-purl:encoding)
::
::  url doesn't match expected binding from :acme
::
?.  ?=(^ url)
    ~|  [%invalid-url url.request]  !!
?.  ?=([%'.well-known' %acme-challenge *] q.p.u.url)
  ~|  [%unknown-url url.request]  !!
::
::  404 if token missing from url or not in app
::
?~  t.t.q.p.u.url
  [[%404 ~] ~]
=/  challenge=@t  i.t.t.q.p.u.url
=/  response
  .^  (unit @t)
    %gx
    (scot %p p.bek)
    %acme
    (scot %da now)
    /domain-validation/[challenge]/noun
  ==
?~  response
  [[%404 ~] ~]
:-  [200 ['content-type' 'text/html']~]
(some (as-octs:mimes:encoding u.response))
