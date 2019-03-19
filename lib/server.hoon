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
++  manx-to-octs
  |=  man=manx
  ^-  octs
  (as-octs:mimes:html (crip (en-xml:html man)))
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
--
