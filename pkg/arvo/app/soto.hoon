::
::  Soto: A Dojo relay for Urbit's Landscape interface
::  Relays sole-effects to subscribers and forwards sole-action pokes
::
/-  sole
/+  *server, *soto, default-agent
/=  index
  /^  octs
  /;  as-octs:mimes:html
  /:  /===/app/soto/index
  /|  /html/
      /~  ~
  ==
/=  tile-js
  /^  octs
  /;  as-octs:mimes:html
  /:  /===/app/soto/js/tile
  /|  /js/
      /~  ~
  ==
/=  script
  /^  octs
  /;  as-octs:mimes:html
  /:  /===/app/soto/js/index
  /|  /js/
      /~  ~
  ==
/=  style
  /^  octs
  /;  as-octs:mimes:html
  /:  /===/app/soto/css/index
  /|  /css/
      /~  ~
  ==
/=  soto-png
  /^  (map knot @)
  /:  /===/app/soto/img  /_  /png/
::
|%
+$  card  card:agent:gall
+$  state-zero  ~
::
--
=|  state-zero
=*  state  -
^-  agent:gall
|_  bol=bowl:gall
+*  this      .
    soto-core  +>
    sc        ~(. soto-core bol)
    def       ~(. (default-agent this %|) bol)
::
++  on-init
  :_  this
  :~  [%pass /bind/soto %arvo %e %connect [~ /'~dojo'] %soto]
      :*  %pass  /launch/soto  %agent  [our.bol %launch]  %poke
          %launch-action  !>([%add %soto /sototile '/~dojo/js/tile.js'])
      ==
  ==
++  on-save  !>(state)
::
++  on-load
  |=  old=vase
  [~ this(state !<(state-zero old))]
::
++  on-poke
  |=  [mar=mark vas=vase]
  ^-  (quip card _this)
  ?>  (team:title our.bol src.bol)
  ?.  ?=(%handle-http-request mar)
    (on-poke:def mar vas)
  =+  !<([id=@ta req=inbound-request:eyre] vas)
  :_  this
  %+  give-simple-payload:app    id
  %+  require-authorization:app  req
  |=  =inbound-request:eyre
  ^-  simple-payload:http
  =/  request-line  (parse-request-line url.request.inbound-request)
  ?+  request-line
    not-found:gen
  ::  main page
  ::
      [[~ [%'~dojo' *]] *]
    (html-response:gen index)
  ::  main js
  ::
      [[[~ %js] [%'~dojo' %js %index ~]] ~]
    (js-response:gen script)
  ::  tile js
  ::
      [[[~ %js] [%'~dojo' %js %tile ~]] ~]
    (js-response:gen tile-js)
  ::  styling
  ::
      [[[~ %css] [%'~dojo' %css %index ~]] ~]
    (css-response:gen style)
  ::  images
  ::
      [[[~ %png] [%'~dojo' %img @t ~]] ~]
    =/  filename=@t  i.t.t.site.request-line
    =/  img  (~(get by soto-png) filename)
    ?~  img
      not-found:gen
    (png-response:gen (as-octs:mimes:html u.img))
  ==
::
++  on-watch
  |=  pax=path
  ^-  (quip card _this)
  ?+    pax  (on-watch:def pax)
      [%http-response *]
    [~ this]
  ::
      [%sototile ~]
    :_  this
    [%give %fact ~ %json !>(~)]~
  ==
::
++  on-agent  on-agent:def
::
++  on-arvo
  |=  [wir=wire sin=sign-arvo]
  ^-  (quip card _this)
  ?:  ?=(%bound +<.sin)
    [~ this]
  (on-arvo:def wir sin)
::
++  on-fail   on-fail:def
++  on-leave  on-leave:def
++  on-peek   on-peek:def
::
--
