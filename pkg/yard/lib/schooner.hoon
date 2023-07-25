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
    [%application-javascript p=tape]            :: js
    [%application-json p=@]                     :: json
    [%application-pdf p=@]                      :: pdf
    [%application-rtf p=@]                      :: rtf
    [%application-xml p=@]                      :: xml
    [%audio-aac p=@]                            :: aac
    [%audio-flac p=@]                           :: flac
    [%audio-mid p=@]                            :: mid, midi
    [%audio-mpeg p=@]                           :: mp3
    [%audio-ogg p=@]                            :: oga
    [%audio-wav p=@]                            :: wav
    [%audio-webm p=@]                           :: weba
    [%font-otf p=@]                             :: otf
    [%font-ttf p=@]                             :: ttf
    [%font-woff2 p=@]                           :: woff2
    [%html h=cord]                              :: htm, html
    [%image-bmp p=@]                            :: bmp
    [%image-gif p=@]                            :: gif
    [%image-ico p=@]                            :: ico
    [%image-jpeg p=@]                           :: jpg, jpeg
    [%image-png p=@]                            :: png
    [%image-svg p=@]                            :: svg
    [%image-tiff p=@]                           :: tiff
    [%image-webp p=@]                           :: webp
    [%json j=json]                              :: json (type not mark)
    [%manx m=manx]                              :: manx (type not mark)
    [%plain p=tape]                             :: txt
    [%text-css p=@]                             :: css
    [%text-csv p=@]                             :: csv
    [%text-javascript p=@]                      :: js
    [%text-plain p=@]                           :: txt
    [%text-xml p=@]                             :: xml
    [%video-avi p=@]                            :: avi
    [%video-mp4 p=@]                            :: mp4
    [%video-mpeg p=@]                           :: mpeg
    [%video-ogg p=@]                            :: ogv
    [%video-webm p=@]                           :: webm
    ::
    [%login-redirect l=cord]                    ::
    [%none ~]
    [%redirect o=cord]
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
    (weld headers ['content-type'^'application/javascript']~)
    ::
      %application-json
    :_  `(as-octt:mimes:html p.resource)
    :-  http-status
    (weld headers ['content-type'^'application/json']~)
    ::
      %application-pdf
    :_  `(as-octt:mimes:html p.resource)
    :-  http-status
    (weld headers ['content-type'^'application/pdf']~)
    ::
      %application-rtf
    :_  `(as-octt:mimes:html p.resource)
    :-  http-status
    (weld headers ['content-type'^'application/rtf']~)
    ::
      %application-xml
    :_  `(as-octt:mimes:html p.resource)
    :-  http-status
    (weld headers ['content-type'^'application/xml']~)
    ::
      %audio-aac
    :_  `(as-octs:mimes:html p.resource)
    :-  http-status
    (weld headers ['content-type'^'audio/aac']~)
    ::
      %audio-flac
    :_  `(as-octs:mimes:html p.resource)
    :-  http-status
    (weld headers ['content-type'^'audio/flac']~)
    ::
      %audio-mid
    :_  `(as-octs:mimes:html p.resource)
    :-  http-status
    (weld headers ['content-type'^'audio/midi']~)
    ::
      %audio-mpeg
    :_  `(as-octs:mimes:html p.resource)
    :-  http-status
    (weld headers ['content-type'^'audio/mpeg']~)
    ::
      %audio-ogg
    :_  `(as-octs:mimes:html p.resource)
    :-  http-status
    (weld headers ['content-type'^'audio/ogg']~)
    ::
      %audio-wav
    :_  `(as-octs:mimes:html p.resource)
    :-  http-status
    (weld headers ['content-type'^'audio/wav']~)
    ::
      %audio-weba
    :_  `(as-octs:mimes:html p.resource)
    :-  http-status
    (weld headers ['content-type'^'audio/webm']~)
    ::
      %font-otf
    :_  `(as-octs:mimes:html p.resource)
    :-  http-status
    (weld headers ['content-type'^'font/otf']~)
    ::
      %font-ttf
    :_  `(as-octs:mimes:html p.resource)
    :-  http-status
    (weld headers ['content-type'^'font/ttf']~)
    ::
      %font-woff2
    :_  `(as-octs:mimes:html p.resource)
    :-  http-status
    (weld headers ['content-type'^'fonts/woff2']~)
    ::
      %html
    :-  :-  http-status
      (weld headers ['content-type'^'text/html']~)
    `(as-octs:mimes:html h.resource)
    ::
      %image-bmp
    :_  `(as-octs:mimes:html p.resource)
    :-  http-status
    (weld headers ['content-type'^'image/bmp']~)
    ::
      %image-gif
    :_  `(as-octs:mimes:html p.resource)
    :-  http-status
    (weld headers ['content-type'^'image/gif']~)
    ::
      %image-ico
    :_  `(as-octs:mimes:html p.resource)
    :-  http-status
    (weld headers ['content-type'^'image/vnd.microsoft.icon']~)
    ::
      %image-jpeg
    :_  `(as-octs:mimes:html p.resource)
    :-  http-status
    (weld headers ['content-type'^'image/jpeg']~)
    ::
      %image-png
    :_  `(as-octs:mimes:html p.resource)
    :-  http-status
    (weld headers ['content-type'^'image/png']~)
    ::
      %image-svg
    :_  `(as-octs:mimes:html p.resource)
    :-  http-status
    (weld headers ['content-type'^'image/svg+xml']~)
    ::
      %image-tiff
    :_  `(as-octs:mimes:html p.resource)
    :-  http-status
    (weld headers ['content-type'^'image/tiff']~)
    ::
      %image-webp
    :_  `(as-octs:mimes:html p.resource)
    :-  http-status
    (weld headers ['content-type'^'image/webp']~)
    ::
      %json
    :-  :-  http-status
        %+  weld  headers
        ['content-type'^'application/json']~
    `(as-octt:mimes:html (en-json:html j.resource))
    ::
      %manx
    :-  :-  http-status
      (weld headers ['content-type'^'text/html']~)
    `(as-octt:mimes:html (en-xml:html m.resource))
    ::
      %plain
    :_  `(as-octt:mimes:html p.resource)
    :-  http-status
    (weld headers ['content-type'^'text/plain']~)
    ::
      %text-css
    :_  `(as-octs:mimes:html p.resource)
    :-  http-status
    (weld headers ['content-type'^'text/css']~)
    ::
      %text-csv
    :_  `(as-octs:mimes:html p.resource)
    :-  http-status
    (weld headers ['content-type'^'text/csv']~)
    ::
      %text-javascript
    :_  `(as-octs:mimes:html p.resource)
    :-  http-status
    (weld headers ['content-type'^'text/javascript']~)
    ::
      %text-plain
    :_  `(as-octs:mimes:html p.resource)
    :-  http-status
    (weld headers ['content-type'^'text/plain']~)
    ::
      %text-xml                                           :: overrides text/
    :_  `(as-octs:mimes:html p.resource)
    :-  http-status
    (weld headers ['content-type'^'application/xml']~)
    ::
      %video-avi
    :_  `(as-octs:mimes:html p.resource)
    :-  http-status
    (weld headers ['content-type'^'video/x-msvideo']~)
    ::
      %video-mp4
    :_  `(as-octs:mimes:html p.resource)
    :-  http-status
    (weld headers ['content-type'^'video/mp4']~)
    ::
      %video-mpeg
    :_  `(as-octs:mimes:html p.resource)
    :-  http-status
    (weld headers ['content-type'^'video/mpeg']~)
    ::
      %video-ogg
    :_  `(as-octs:mimes:html p.resource)
    :-  http-status
    (weld headers ['content-type'^'video/ogg']~)
    ::
      %video-webm
    :_  `(as-octs:mimes:html p.resource)
    :-  http-status
    (weld headers ['content-type'^'video/webm']~)
    ::
    ::
      %login-redirect
    =+  %^  cat  3
      '/~/login?redirect='
    l.resource
    :_  ~
    :-  http-status
    (weld headers [['location' -]]~)    
    ::
      %none
    [[http-status headers] ~]
    ::
      %redirect
    :_  ~
    :-  http-status
    (weld headers ['location'^o.resource]~)
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
