/+  http-handler
=,  eyre
|%
::
::  +parse-request-line: take a cord and parse out a url
::
++  parse-request-line
  |=  url=@t
  ^-  [[ext=(unit @ta) site=(list @t)] args=(list [key=@t value=@t])]
  (fall (rush url ;~(plug apat:de-purl:html yque:de-purl:html)) [[~ ~] ~])
::
++  manx-to-octs
  |=  man=manx
  ^-  octs
  (as-octt:mimes:html (en-xml:html man))
::
++  json-to-octs
  |=  jon=json
  ^-  octs
  (as-octt:mimes:html (en-json:html jon))
::
++  app
  |%
  ::
  ::  +require-authorization: redirect to the login page when unauthenticated
  ::
  ++  require-authorization
    |=  handler=$-(inbound-request:eyre simple-payload:http)
    |=  =inbound-request:eyre
    ^-  simple-payload:http
    ::
    ?:  authenticated.inbound-request
      ~!  this
      ~!  +:*handler
      (handler inbound-request)
    ::
    =/  redirect=cord
      %-  crip
      "/~/login?redirect={(trip url.request.inbound-request)}"
    [[307 ['location' redirect]~] ~]
  ::
  ++  html-response
    |=  oct-html=octs
    ^-  http-event:http
    [%start [200 ['content-type' 'text/html']~] [~ oct-html] %.y]
  ::
  ++  js-response
    |=  oct-js=octs
    ^-  http-event:http
    [%start [200 ['content-type' 'text/javascript']~] [~ oct-js] %.y]
  ::
  ++  json-response
    |=  oct-js=octs
    ^-  http-event:http
    [%start [200 ['content-type' 'application/json']~] [~ oct-js] %.y]
  ::
  ++  css-response
    |=  oct-css=octs
    ^-  http-event:http
    [%start [200 ['content-type' 'text/css']~] [~ oct-css] %.y]
  ::
  ++  manx-response
    |=  man=manx
    ^-  http-event:http
    [%start [200 ['content-type' 'text/html']~] [~ (manx-to-octs man)] %.y]
  ::
  ++  png-response
    |=  oct-png=octs
    ^-  http-event:http
    [%start [200 ['content-type' 'image/png']~] [~ oct-png] %.y]
  ::
  ++  woff2-response
    |=  oct-woff=octs
    ^-  http-event:http
    [%start [200 ['content-type' 'font/woff2']~] [~ oct-woff] %.y]
  ::
  ++  not-found
    ^-  http-event:http
    [%start [404 ~] ~ %.y]
  ::
  ++  login-redirect
    |=  =inbound-request:eyre
    ^-  http-event:http
    =/  redirect=cord
      %-  crip
      "/~/login?redirect={(trip url.request.inbound-request)}"
    [%start [307 ['location' redirect]~] ~ %.y]
  ::
  ++  redirect
    |=  redirect=cord
    ^-  http-event:http
    [%start [307 ['location' redirect]~] ~ %.y]
  ::
  --
++  gen
  |%
  ::
  ++  html-response
    |=  =octs
    ^-  simple-payload:http
    [[200 ['content-type' 'text/html']~] `octs]
  ::
  ++  js-response
    |=  =octs
    ^-  simple-payload:http
    [[200 ['content-type' 'text/javascript']~] `octs]
  ::
  ++  json-response
    |=  =octs
    ^-  simple-payload:http
    [[200 ['content-type' 'application/json']~] `octs]
  ::
  ++  css-response
    |=  =octs
    ^-  simple-payload:http
    [[200 ['content-type' 'text/css']~] `octs]
  ::
  ++  not-found
    ^-  simple-payload:http
    [[404 ~] ~]
  ::
  ++  login-redirect
    |=  =request:http
    ^-  simple-payload:http
    =/  redirect=cord
      %-  crip
      "/~/login?redirect={(trip url.request)}"
    [[307 ['location' redirect]~] ~]
  ::
  ++  redirect
    |=  redirect=cord
    ^-  simple-payload:http
    [[307 ['location' redirect]~] ~]
  ::
  --
--
