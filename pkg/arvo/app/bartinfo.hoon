/+  *server, default-agent
/=  index
  /^  octs
  /;  as-octs:mimes:html
  /:  /===/app/bartinfo/index
  /|  /html/
      /~  ~
  ==
/=  tile-js
  /^  octs
  /;  as-octs:mimes:html
  /:  /===/app/bartinfo/js/tile
  /|  /js/
      /~  ~
  ==
/=  script
  /^  octs
  /;  as-octs:mimes:html
  /:  /===/app/bartinfo/js/index
  /|  /js/
      /~  ~
  ==
/=  style
  /^  octs
  /;  as-octs:mimes:html
  /:  /===/app/bartinfo/css/index
  /|  /css/
      /~  ~
  ==
/=  bartinfo-png
  /^  (map knot @)
  /:  /===/app/bartinfo/img  /_  /png/
::
|%
+$  card  card:agent:gall
--
^-  agent:gall
=<
  |_  bol=bowl:gall
  +*  this       .
      bartinfo-core  +>
      cc         ~(. bartinfo-core bol)
      def        ~(. (default-agent this %|) bol)
  ::
  ++  on-init
    ^-  (quip card _this)
    =/  launcha  [%launch-action !>([%add %bartinfo / '/~bartinfo/js/tile.js'])]
    :_  this
    :~  [%pass / %arvo %e %connect [~ /'~bartinfo'] %bartinfo]
        [%pass /bartinfo %agent [our.bol %launch] %poke launcha]
    ==
  ++  on-poke
    |=  [=mark =vase]
    ^-  (quip card _this)
    ?>  (team:title our.bol src.bol)
    ?+    mark  (on-poke:def mark vase)
        %handle-http-request
      =+  !<([eyre-id=@ta =inbound-request:eyre] vase)
      :_  this
      %+  give-simple-payload:app  eyre-id
      %+  require-authorization:app  inbound-request
      poke-handle-http-request:cc
        %json
      =+  !<(jon=json vase)
      :_  this
      (poke-handle-json:cc jon)
    ::
    ==
  ::
  ++  on-watch
    |=  =path
    ^-  (quip card _this)
    ~&  "on-watch path: {<path>}"
    ?:  ?=([%bartstations *] path)
      =/  bart-station-request
        =/  out  *outbound-config:iris
        =/  req  bart-api-request-stations:cc
        [%pass /bartstationrequest %arvo %i %request req out]
      [~[bart-station-request] this]
    ?:  ?=([%elevators *] path)
      =/  elevator-status-request
        =/  out  *outbound-config:iris
        =/  req  bart-api-elevator-status:cc
        [%pass /elevators %arvo %i %request req out]
      [~[elevator-status-request] this]
    ?:  ?=([%routes *] path)
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
    ~&  "The on-arvo wire: {<wire>}"
    ?:  ?=(%http-response +<.sign-arvo)
      =/  http-moves=(list card)
      ?+  wire   ~
        [%bartstationrequest *]
        =/  value=json  (parse-request-stations-response:cc client-response.sign-arvo)
        ?>  ?=(%o -.value)
        =/  update=json  (pairs:enjs:format [update+o+p.value ~])
        [%give %fact ~[/bartstations] %json !>(update)]~
      ::
      [%elevators *]
      =/  value=json  (parse-elevator-status-response:cc client-response.sign-arvo)
      ?>  ?=(%o -.value)
      =/  update=json  (pairs:enjs:format [update+o+p.value ~])
      [%give %fact ~[/elevators] %json !>(update)]~
      ::
      [%routeplan *]
      =/  value=json  (parse-routeplan-response:cc client-response.sign-arvo)
      ?>  ?=(%o -.value)
      =/  update=json  (pairs:enjs:format [update+o+p.value ~])
      [%give %fact ~[/routes] %json !>(update)]~
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
:: request to http://api.bart.gov/api/stn.aspx?cmd=stns&key=Q5RQ-PUEB-999T-DWEI&json=y
:: get .root | .stations | .station for list of stations
++  bart-api-key  "Q5RQ-PUEB-999T-DWEI"
++  bart-api-url-base  "http://api.bart.gov/api"
++  with-json-handler
  |=  [response=client-response:iris jsonhandler=$-(json json)]
  ^-  json
  =,  format
  ?.  ?=(%finished -.response)
    %-  pairs:enjs  [fulltext+s+'bart response error' ~]
  =/  data=(unit mime-data:iris)  full-file.response
  ?~  data  %-  pairs:enjs  ~
  =/  ujon=(unit json)  (de-json:html q.data.u.data)
  ?~  ujon   %-  pairs:enjs  ~
  ?>  ?=(%o -.u.ujon)
  =/  parsed-json=json   u.ujon
  (jsonhandler parsed-json)
++  bart-api-request-stations
  ^-  request:http
  =/  url  (crip "{bart-api-url-base}/stn.aspx?cmd=stns&key={bart-api-key}&json=y")
  =/  headers  [['Accept' 'application/json']]~
  [%'GET' url headers *(unit octs)]
::
++  parse-request-stations-response
  |=  response=client-response:iris
  ^-  json
  =,  format
  =/  handler  |=  jon=json
    =/  root       ((ot:dejs ~[['root' same]]) jon)
    =/  stations   ((ot:dejs ~[['stations' same]]) root)
    =/  station    ((ot:dejs ~[['station' (ar:dejs same)]]) stations)
    =/  abbr-and-name   %-  turn  :-  station  |=  item=json
      ^-  json
      =/  [name=tape abbr=tape]  
            ((ot:dejs ~[['name' sa:dejs] ['abbr' sa:dejs]]) item)
      (pairs:enjs ~[name+(tape:enjs name) abbr+(tape:enjs abbr)])
    (pairs:enjs [[%stations %a abbr-and-name] ~])
  (with-json-handler response handler)
::
++  bart-api-elevator-status
  ^-  request:http
  =/  url  (crip "{bart-api-url-base}/bsa.aspx?cmd=elev&key={bart-api-key}&json=y")
  =/  headers  [['Accept' 'application/json']]~
  [%'GET' url headers *(unit octs)]
++  parse-elevator-status-response
  |=  response=client-response:iris
  ^-  json
  =,  format
  =/  handler  |=  jon=json
    =/  root=json  ((ot:dejs ~[['root' same]]) jon)
    =/  bsa=(list json)   ((ot:dejs ~[['bsa' (ar:dejs same)]]) root)
    (pairs:enjs [[%elevators %a bsa] ~])
  (with-json-handler response handler)
::
++  bart-api-routeplan
::  Documentation: http://api.bart.gov/docs/sched/depart.aspx
  |=  [from=tape to=tape hour=@ min=@ ispm=?]
  ^-  request:http
  =/  meridian  ?:(ispm "pm" "am")
  =/  minstr  ?:  =(min 0)   "00"
              ?:  (lte min 9)  "0{<min>}"
              "{<min>}"
  =/  time  "{<hour>}:{minstr}{meridian}"
  =/  before  1
  =/  after   3
  =/  url  (crip "{bart-api-url-base}/sched.aspx?cmd=depart&orig={from}&a={<after>}&b={<before>}&dest={to}&time={time}&key={bart-api-key}&json=y")
  ~&  "Making BART API request to {<url>}"
  =/  headers  [['Accept' 'application/json']]~
  [%'GET' url headers *(unit octs)]
++  parse-routeplan-response
  |=  response=client-response:iris
  ^-  json
  =,  format
  =/  handler
    |=  jon=json
    =/  root=json  ((ot:dejs [['root' same] ~]) jon)
    =/  schedule=json  ((ot:dejs [['schedule' same] ~]) root)
    (pairs:enjs ~[[%routes schedule]])
  (with-json-handler response handler)
++  poke-handle-json
  |=  jon=json
  ^-  (list card)
  ~&  jon
  =,  format
  ?.  ?=(%o -.jon)
    [~]
  =/  [hour=@ min=@ ispm=? from-station=tape to-station=tape]
    %.
      jon
      %:  ot:dejs
        ['hour' ni:dejs]
        ['min' ni:dejs]
        ['isPM' bo:dejs]
        ['from' sa:dejs]
        ['to' sa:dejs]
        ~
      ==
  =/  req  (bart-api-routeplan from-station to-station hour min ispm)
  =/  out  *outbound-config:iris
  [[%pass /routeplan %arvo %i %request req out] ~]
::
++  poke-handle-http-request
  |=  =inbound-request:eyre
  ^-  simple-payload:http
  =+  url=(parse-request-line url.request.inbound-request)
  ?+  site.url  not-found:gen
      [%'~bartinfo' %css %index ~]  (css-response:gen style)
      [%'~bartinfo' %js %tile ~]    (js-response:gen tile-js)
      [%'~bartinfo' %js %index ~]   (js-response:gen script)
  ::
      [%'~bartinfo' %img @t *]
    =/  name=@t  i.t.t.site.url
    =/  img  (~(get by bartinfo-png) name)
    ?~  img
      not-found:gen
    (png-response:gen (as-octs:mimes:html u.img))
  ::
      [%'~bartinfo' *]  (html-response:gen index)
  ==
--
