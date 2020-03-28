/+  *server
::  @p request handler
::
|=  [[now=@da eny=@ bek=beak] ~ ~]
|=  [authorized=? =request:http]
^-  simple-payload:http
=/  url=(unit pork:eyre)
  (rush url.request apat:de-purl:html)
::
::  url doesn't match expected binding from :launch
::
?.  ?=(^ url)
  ~|  [%invalid-url url.request]  !!
?.  ?=([%who *] q.u.url)
  ~|  [%unknown-url url.request]  !!
::
::
?.  ?=(~ t.q.u.url)
  [[%404 ~] ~]
=/  response=json
  (frond:enjs:format %who (ship:enjs:format p.bek))
(json-response:gen (json-to-octs response))
