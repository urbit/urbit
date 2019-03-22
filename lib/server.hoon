=,  http-server
|%
::
::  +parse-request-line: take a cord and parse out a url
::
++  parse-request-line
  |=  url=@t
  ^-  [[(unit @ta) site=(list @t)] args=(list [key=@t value=@t])]
  (fall (rush url ;~(plug apat:de-purl:html yque:de-purl:html)) [[~ ~] ~])
::
++  manx-to-octs
  |=  man=manx
  ^-  octs
  (as-octs:mimes:html (crip (en-xml:html man)))
::
++  app
  |%
  ::
  ::  +require-authorization: redirect to the login page when unauthenticated
  ::
  ++  require-authorization
    |*  [=bone move=mold this=*]
    |=  handler=$-(inbound-request:http-server (quip move _this))
    |=  =inbound-request:http-server
    ^-  (quip move _this)
    ::
    ?:  authenticated.inbound-request
      (handler inbound-request)
    ::
    :_  this
    ^-  (list move)
    =/  redirect=cord
      %-  crip
      "/~/login?redirect={(trip url.request.inbound-request)}"
    [bone [%http-response %start [307 ['location' redirect]~] ~ %.y]]~
  ::
  ++  html-response
    |=  oct-html=octs
    ^-  http-event:http
    [%start [200 ['content-type' 'text/html']~] [~ oct-html] %.y]
  ::
  ++  js-response
    |=  oct-js=octs
    ^-  http-event:http
    [%start [200 ['content-type' 'application/js']~] [~ oct-js] %.y]
  ::
  ++  css-response
    |=  oct-css=octs
    ^-  http-event:http
    [%start [200 ['content-type' 'text/css']~] [~ oct-css] %.y]
  ::
  ++  login-redirect
    |=  =inbound-request:http-server
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
    [[200 ['content-type' 'application/js']~] `octs]
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
