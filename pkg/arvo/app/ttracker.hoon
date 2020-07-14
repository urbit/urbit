/-  ttracker
/+  server, default-agent
/=  index
  /^  octs
  /;  as-octs:mimes:html
  /:  /===/app/ttracker/index
  /|  /html/
      /~  ~
  ==
/=  tile-js
  /^  octs
  /;  as-octs:mimes:html
  /:  /===/app/ttracker/js/tile
  /|  /js/
      /~  ~
  ==
/=  script
  /^  octs
  /;  as-octs:mimes:html
  /:  /===/app/ttracker/js/index
  /|  /js/
      /~  ~
  ==
/=  style
  /^  octs
  /;  as-octs:mimes:html
  /:  /===/app/ttracker/css/index
  /|  /css/
      /~  ~
  ==
/=  ttracker-png
  /^  (map knot @)
  /:  /===/app/ttracker/img  /_  /png/
::
|%
+$  card  card:agent:gall

+$  state-zero
  $:  api-key=@tas
  ==
+$  versioned-state
  $%
    [%0 state-zero]
  ==
--

=|  state=versioned-state


^-  agent:gall
=<
  |_  bol=bowl:gall
  +*  this       .
      ttracker-core  +>
      cc         ~(. ttracker-core bol)
      def        ~(. (default-agent this %|) bol)
  ::
  ++  on-init
    ^-  (quip card _this)
    =/  launcha  [%launch-action !>([%add %ttracker / '/~ttracker/js/tile.js'])]
    :_  this
    :~  [%pass / %arvo %e %connect [~ /'~ttracker'] %ttracker]
        [%pass /ttracker %agent [our.bol %launch] %poke launcha]
    ==
  ++  on-poke
    |=  [=mark =vase]
    ^-  (quip card _this)
    ?+    mark  (on-poke:def mark vase)
        %ttracker-action
        :_  this 
        (poke-ttracker-action:cc !<(action:ttracker vase))
        %handle-http-request
      :_  this  
      ::
          =+  !<([eyre-id=@ta =inbound-request:eyre] vase)
          %+  give-simple-payload:app:server  eyre-id
          %+  require-authorization:app:server  inbound-request
          poke-handle-http-request:cc
    ==
    
  ::
  ++  on-watch
    |=  =path
    ^-  (quip card _this)
    ?:  ?=([%mbtaalert *] path)
      =/  full-alert-request
        =/  out  *outbound-config:iris
        =/  req  alerts-req:cc
        [%pass /arequest %arvo %i %request req out]
      [~[full-alert-request] this]
      ?:  ?=([%mbtastation *] path)
      =/  full-stations-request
        =/  out  *outbound-config:iris
        =/  req  stations-req:cc
        [%pass /srequest %arvo %i %request req out]
      [~[full-stations-request] this]
      ?:  ?=([%mbtaroutes *] path)
      =/  full-route-request
        =/  out  *outbound-config:iris
        =/  req  route-req:cc
        [%pass /rrequest %arvo %i %request req out]
      [~[full-route-request] this]
      ?:  ?=([%mbtafacility *] path)
        [[~] this]
  ?:  ?=([%http-response *] path)
      `this
    ?.  =(/ path)
      (on-watch:def path)
    [[%give %fact ~ %json !>(*json)]~ this]
  ::
  ++  on-agent  on-agent:def
  ::
  ++  on-arvo
    |=  [=wire =sign-arvo]
    ^-  (quip card _this)
    ?:  ?=(%http-response +<.sign-arvo)
      =/  http-moves=(list card)
      ?+  wire   ~
        [%arequest *]
        =/  value=json  (parse-req:cc client-response.sign-arvo)
        ?>  ?=(%o -.value)
        =/  update=json  (pairs:enjs:format [alerts+o+p.value ~])
        [%give %fact ~[/mbtaalert] %json !>(update)]~
        [%srequest *]
        =/  value=json  (parse-req:cc client-response.sign-arvo)
        ?>  ?=(%o -.value)
        =/  update=json  (pairs:enjs:format [stations+o+p.value ~])
        [%give %fact ~[/mbtastation] %json !>(update)]~
        [%frequest *]
        =/  value=json  (parse-req:cc client-response.sign-arvo)
        ?>  ?=(%o -.value)
        =/  update=json  (pairs:enjs:format [facilities+o+p.value ~])
        [%give %fact ~[/mbtafacility] %json !>(update)]~
        [%rrequest *]
        =/  value=json  (parse-req:cc client-response.sign-arvo)
        ?>  ?=(%o -.value)
        =/  update=json  (pairs:enjs:format [routes+o+p.value ~])
        [%give %fact ~[/mbtaroutes] %json !>(update)]~
         
      ::
      ==
      [http-moves this]
    ?.  ?=(%bound +<.sign-arvo)
      (on-arvo:def wire sign-arvo)
    [~ this]
  ::
  ++  on-save  on-save:def
  ++  on-load  on-load:def
  ++  on-leave  on-leave:def
  ++  on-peek   on-peek:def
  ++  on-fail   on-fail:def
  --
::
::
|_  bol=bowl:gall
::
++  poke-handle-http-request
  |=  =inbound-request:eyre
  ^-  simple-payload:http
  =+  gen=gen:server

  =+  url=(parse-request-line:server url.request.inbound-request)
  ?+  site.url  not-found:gen
      [%'~ttracker' %css %index ~]  (css-response:gen style)
      [%'~ttracker' %js %tile ~]    (js-response:gen tile-js)
      [%'~ttracker' %js %index ~]   (js-response:gen script)
  ::
      [%'~ttracker' %img @t *]
    =/  name=@t  i.t.t.site.url
    =/  img  (~(get by ttracker-png) name)
    ?~  img
      not-found:gen
    (png-response:gen (as-octs:mimes:html u.img))
  ::
      [%'~ttracker' *]  (html-response:gen index)
  ==
::
++  alerts-req
  ^-  request:http
  [%'GET' 'https://api-v3.mbta.com/alerts?sort=informed_entity&filter%5Broute%5D=Red%2COrange%2CGreen%2CMattapan%2CGreen-B%2CGreen-C%2CGreen-E%2CGreen-D' ~ *(unit octs)]

++  stations-req
  ^-  request:http
  [%'GET' 'https://api-v3.mbta.com/stops?filter%5Broute_type%5D=0%2C1' ~ *(unit octs)]

++  facilities-req
  |=  station-name=@tas
  ^-  request:http
  ~&  station-name
  =/  url=@t  (crip (weld "https://api-v3.mbta.com/facilities?filter%5Bstop%5D=" (trip station-name)))
  ~&  url
  [%'GET' url ~ *(unit octs)]

  ++  route-req
  ^-  request:http
  [%'GET' 'https://api-v3.mbta.com/routes?filter%5Btype%5D=0%2C1' ~ *(unit octs)]


++  parse-req
|=  response=client-response:iris
^-  json
=,  format
  ?.  ?=(%finished -.response)
    %-  pairs:enjs  [err+s+'Unable to fetch response' ~]
  =/  data=(unit mime-data:iris)  full-file.response
  ?~  data  %-  pairs:enjs  ~
  =/  ujon=(unit json)  (de-json:html q.data.u.data)
  ?~  ujon   %-  pairs:enjs  ~
  ?>  ?=(%o -.u.ujon)
  =/  parsed-json=json  u.ujon
  parsed-json

++  poke-ttracker-action
  |=  =action:ttracker
  ^-  (list card)
  ?.  ?=(%facilities -.action)
    !!
  =/  req  (facilities-req +.action)
  =/  out  *outbound-config:iris
  [[%pass /frequest %arvo %i %request req out] ~]

--
