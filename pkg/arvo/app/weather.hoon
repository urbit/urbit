/+  *server, *server, default-agent, verb, dbug
/=  tile-js
  /^  octs
  /;  as-octs:mimes:html
  /:  /===/app/weather/js/tile
  /|  /js/
      /~  ~
  ==
/=  weather-png
  /^  (map knot @)
  /:  /===/app/weather/img  /_  /png/
=,  format
::
|%
::
+$  card  card:agent:gall
+$  versioned-state
  $%  state-zero
  ==
+$  state-zero  [%0 data=json time=@da location=@t timer=(unit @da)]
--
=|  state-zero
=*  state  -
%+  verb  |
%-  agent:dbug
^-  agent:gall
=<
  |_  bol=bowl:gall
  +*  this  .
      weather-core  +>
      wc    ~(. weather-core bol)
      def   ~(. (default-agent this %|) bol)
  ++  on-init
    :_  this
    :~  [%pass /bind/weather %arvo %e %connect [~ /'~weather'] %weather]
        :*  %pass  /launch/weather  %agent  [our.bol %launch]  %poke
            %launch-action  !>([%add %weather /weathertile '/~weather/js/tile.js'])
        ==
    ==
  ++  on-save  !>(state)
  ++  on-load
    |=  old=vase
    `this(state !<(state-zero old))
  ::
  ++  on-poke
    |=  [=mark =vase]
    ^-  (quip card _this)
    =^  cards  state
      ?+    mark  (on-poke:def mark vase)
          %json
        (poke-json:wc !<(json vase))
          %handle-http-request
        =+  !<([eyre-id=@ta =inbound-request:eyre] vase)
        :_  state
        %+  give-simple-payload:app  eyre-id
        %+  require-authorization:app  inbound-request
        poke-handle-http-request:wc
      ==
    [cards this]
  ::
  ++  on-watch
    |=  =wire
    ^-  (quip card _this)
    ?:  ?=([%weathertile ~] wire)
      :_  this
      [%give %fact ~ %json !>(data)]~
    ?:  ?=([%http-response *] wire)
      [~ this]
    (on-watch:def wire)
  ::
  ++  on-arvo
    |=  [=wire =sign-arvo]
    ^-  (quip card:agent:gall _this)
    ?:  ?=(%bound +<.sign-arvo)
      [~ this]
    ?:  ?=(%wake +<.sign-arvo)
      =^  cards  state
        (wake:wc wire error.sign-arvo)
      [cards this]
    ?:  ?=(%http-response +<.sign-arvo)
      =^  cards  state
        (http-response:wc wire client-response.sign-arvo)
      [cards this]
    (on-arvo:def wire sign-arvo)

  ::
  ++  on-leave  on-leave:def
  ++  on-peek   on-peek:def
  ++  on-agent  on-agent:def
  ++  on-fail   on-fail:def
  --
::
|_  bol=bowl:gall
::
++  poke-json
  |=  jon=json
  ^-  (quip card _state)
  ?.  ?=(%s -.jon)
    [~ state]
  =/  str=@t  +.jon
  =/  req=request:http  (request-darksky str)
  =/  out  *outbound-config:iris
  =/  lismov  [%pass /[(scot %da now.bol)] %arvo %i %request req out]~
  ?~  timer
    :-  [[%pass /timer %arvo %b %wait (add now.bol ~h3)] lismov]
    %=  state
      location  str
      timer    `(add now.bol ~h3)
    ==
  [lismov state(location str)]
::
++  request-darksky
  |=  location=@t
  ^-  request:http
  =/  base  'https://api.darksky.net/forecast/634639c10670c7376dc66b6692fe57ca/'
  =/  url=@t  (cat 3 (cat 3 base location) '?units=auto')
  =/  hed  [['Accept' 'application/json']]~
  [%'GET' url hed *(unit octs)]
::
++  http-response
  |=  [=wire response=client-response:iris]
  ^-  (quip card _state)
  ::  ignore all but %finished
  ?.  ?=(%finished -.response)
    [~ state]
  =/  data=(unit mime-data:iris)  full-file.response
  ?~  data
    :: data is null
    [~ state]
  =/  ujon=(unit json)  (de-json:html q.data.u.data)
  ?~  ujon
     [~ state]
  ?>  ?=(%o -.u.ujon)
  ?:  (gth 200 status-code.response-header.response)
    [~ state]
  =/  jon=json  %-  pairs:enjs:format  :~
    currently+(~(got by p.u.ujon) 'currently')
    daily+(~(got by p.u.ujon) 'daily')
  ==
  :-  [%give %fact ~[/weathertile] %json !>(jon)]~
  %=  state
    data  jon
    time  now.bol
  ==
::
++  poke-handle-http-request
  |=  =inbound-request:eyre
  ^-  simple-payload:http
  ::
  =/  request-line  (parse-request-line url.request.inbound-request)
  =/  back-path  (flop site.request-line)
  =/  name=@t
    =/  back-path  (flop site.request-line)
    ?~  back-path
      ''
    i.back-path
  ::
  ?~  back-path
    not-found:gen
  ?:  =(name 'tile')
    (js-response:gen tile-js)
  ?:  (lte (lent back-path) 1)
    not-found:gen
  ?:  =(&2:site.request-line 'img')
    =/  img  (as-octs:mimes:html (~(got by weather-png) `@ta`name))
    (png-response:gen img)
  not-found:gen
::
++  wake
  |=  [wir=wire err=(unit tang)]
  ^-  (quip card _state)
  ?~  err
    =/  req/request:http  (request-darksky location)
    =/  out  *outbound-config:iris
    :_  state(timer `(add now.bol ~h3))
    :~  [%pass /[(scot %da now.bol)] %arvo %i %request req out]
        [%pass /timer %arvo %b %wait (add now.bol ~h3)]
    ==
  %-  (slog u.err)
  [~ state]
::
--
