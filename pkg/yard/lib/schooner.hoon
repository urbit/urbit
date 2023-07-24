  ::  /lib/schooner.hoon
::::  Dalten Collective, with modifications by ~hanfel-dovned & ~lagrev-nocfep
::    Version ~2023.7.24
::
::  Schooner is a Hoon library intended to de-clutter raw HTTP handling
::  in Gall agents.
::
::  It expects to receive a [=eyre-id =http-status =headers =resource]
::  which are conveniently defined below.
::
/+  server
::
|%
::
+$  eyre-id  @ta
+$  header  [key=@t value=@t]
+$  headers  (list header)
::
+$  resource
  $%
    [%application-javascript p=tape]
    [%audio-wav p=@]
    [%html h=cord]
    [%image-jpeg p=@]
    [%image-png p=@]
    [%json j=json]
    [%login-redirect l=cord]
    [%manx m=manx]
    [%plain p=tape]
    [%redirect o=cord]
    ::
    [%none ~]
    [%stock ~]
  ==
::
+$  http-status  @ud
::
++  response
  |=  [=eyre-id =http-status =headers =resource]
  ^-  (list card:agent:gall)
  %+  give-simple-payload:app:server
    eyre-id
  ^-  simple-payload:http
  ?-  -.resource
    ::
      %application-javascript
    :_  `(as-octt:mimes:html p.resource)
    :-  http-status
    (weld headers ['content-type'^'text/javascript']~)
    ::
      %audio-wav
    :_  `(as-octs:mimes:html p.resource)
    :-  http-status
    (weld headers ['content-type'^'audio/wav']~)
    ::
      %json
    :-  :-  http-status
        %+  weld  headers
        ['content-type'^'application/json']~
    `(as-octt:mimes:html (en-json:html j.resource))
    ::
      %html
    :-  :-  http-status
      (weld headers ['content-type'^'text/html']~)
    `(as-octs:mimes:html h.resource)
    ::
      %image-png
    :_  `(as-octs:mimes:html p.resource)
    :-  http-status
    (weld headers ['content-type'^'image/png']~)
    ::
      %login-redirect
    =+  %^  cat  3
      '/~/login?redirect='
    l.resource
    :_  ~
    :-  http-status
    (weld headers [['location' -]]~)    
    ::
      %manx
    :-  :-  http-status
      (weld headers ['content-type'^'text/html']~)
    `(as-octt:mimes:html (en-xml:html m.resource))
    ::
      %plain
    :_  `(as-octt:mimes:html p.resource)
    :-  http-status
    (weld headers ['content-type'^'text/html']~)
    ::
      %redirect
    :_  ~
    :-  http-status
    (weld headers ['location'^o.resource]~)
    ::
      %text-plain
    :_  `(as-octt:mimes:html p.resource)
    :-  http-status
    (weld headers ['content-type'^'text/plain']~)
    ::
    ::
      %none
    [[http-status headers] ~]
    ::
      %stock
    (stock-error headers http-status)
    ::
  ==
::
::  response when MIME type is not pre-configured as in +response (just octs)
::
++  general-response
  |=  [=eyre-id =http-status =headers resource=[term @]]
  ^-  (list card:agent:gall)
  %+  give-simple-payload:app:server
    eyre-id
  ^-  simple-payload:http
  :_  `(as-octt:mimes:html p.resource)
  :-  http-status
  =/  a  (trip -.resource)
  =/  b  (find "-" a)
  =/  c  (crip (snap a b '/'))
  (weld headers ['content-type'^c]~)
::
++  stock-error
  |=  [=headers code=@ud]
  ^-  simple-payload:http
  :-  :-  code
  (weld headers ['content-type'^'text/html']~)
  :-  ~
  =+  (title-content code)
  %-  as-octt:mimes:html
    %-  en-xml:html
  ;html
    ;head
      ;title:"{-.-}"
      ;meta(name "viewport", content "width=device-width, initial-scale=1", charset "utf-8");
      ;style:"{(trip style)}"
    ==
    ;body
      ;span(class "blur-banner")
        ;h2:"{-.-}"
        ;p:"{+.-}"
      ==
    ==
  ==
::
++  title-content
  |=  status=@ud
  ~&  status
  ?+  status
    :-  "500 Error - Internal Server Error" 
    ;:  weld  "This urbit is experiencing presence. "
      "You might try back later, or ask again. "
      "Sorry for the inconvenience."
    ==
    ::
      %403
    :-  "403 Error - FORBIDDEN!"
    ;:  weld  "Another one of them new worlds. "
      "No beer, no women, no pool partners, nothin'. "
      "Nothin' to do but throw rocks at tin cans, and we have to bring our own tin cans."
    ==
    ::
      %404
    :-  "404 Error - Page Not Found"
    %+  weld  "You've attempted to access absence. "
    "Impossible. Try a different path. Sorry for the inconvenience."
    ::
      %405
    :-  "405 Error - Method Not Allowed"
    %+  weld  "Something went wrong with your request. "
    "You should probably just go back. Sorry for the inconvenience."
    ::
  ==
::
++  style
  '''
  .blur-banner { 
    position: relative; 
    top: 60%; 
    left: 0%; 
    right: 0%; 
    bottom: 0%; 
    height: auto; 
    width: 80%; 
    padding: 15px 15px 15px 15px; 
    margin: 0px auto 0px auto; 
    display: block; 
    background: rgba(255, 255, 255, 1.0); 
    font-size: 14pt; 
    color: #997300; 
    font-family: Menlo, Consolas, Monaco, "Lucida Console", monospace; 
    border: 6px #997300 dashed; 
    border-radius: 20px; 
    filter: blur(2px) sepia(25%) brightness(100%) saturate(173%); 
    -webkit-filter: blur(1.5px) sepia(25%) brightness(100%) saturate(175%); 
    -moz-filter: blur(1.5px) sepia(25%) brightness(100%) saturate(175%); 
    -ms-filter: blur(1.5px) sepia(25%) brightness(100%) saturate(175%); 
    -o-filter: blur(1.5px) sepia(25%) brightness(100%) saturate(175%); 
  }
  '''
--
