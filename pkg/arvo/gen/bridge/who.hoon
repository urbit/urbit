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
?~  url
  ~|  [%invalid-url url.request]  !!
?.  ?=([%who *] q.u.url)
  ~|  [%unknown-url url.request]  !!
::
::
::
?^  t.q.u.url
  [[%404 ~] ~]
=/  response=json
  (frond:enjs:format %who (ship:enjs:format p.bek))
:-
  :-  %200
  :~  ['content-type' 'application/json']
      ['access-control-allow-origin' 'https://bridge.urbit.org']
  ==
`(json-to-octs response)
