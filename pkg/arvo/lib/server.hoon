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
    |=  $:  =inbound-request:eyre
            handler=$-(inbound-request:eyre simple-payload:http)
        ==
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
  ++  give-simple-payload
    |=  [eyre-id=@ta =simple-payload:http]
    ^-  (list card:agent:mall)
    =/  header-cage
      [%http-response-header !>(response-header.simple-payload)]
    =/  data-cage
      [%http-response-data !>(data.simple-payload)]
    :~  [%give %fact `/http-response/[eyre-id] header-cage]
        [%give %fact `/http-response/[eyre-id] data-cage]
        [%give %kick `/http-response/[eyre-id] ~]
    ==
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
  ++  manx-response
    |=  man=manx
    ^-  simple-payload:http
    [[200 ['content-type' 'text/html']~] `(manx-to-octs man)]
  ::
  ++  png-response
    |=  =octs
    ^-  simple-payload:http
    [[200 ['content-type' 'image/png']~] `octs]
  ::
  ++  woff2-response
    |=  =octs
    ^-  simple-payload:http
    [[200 ['content-type' 'font/woff2']~] `octs]
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
  --
--
