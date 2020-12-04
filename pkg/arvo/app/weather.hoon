::  weather [landscape]:
::
::  holds latlong, gets weather data from API, passes it on to subscribers
::
/+  *server, default-agent, verb, dbug
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
  ++  on-init  [~ this]
  ++  on-save  !>(state)
  ++  on-load
    |=  old=vase
    :_  this(state !<(state-zero old))
    [%pass /bind/weather %arvo %e %disconnect [~ /'~weather']]~
  ::
  ++  on-poke
    |=  [=mark =vase]
    ^-  (quip card _this)
    =^  cards  state
      ?+  mark   (on-poke:def mark vase)
          %json  (poke-json:wc !<(json vase))
      ==
    [cards this]
  ::
  ++  on-watch
    |=  =wire
    ^-  (quip card _this)
    ?.  ?=([%all ~] wire)  (on-watch:def wire)
    =/  jon
      %-  pairs:enjs:format
      :*  ['location' s+location]
        ::
          ?.  ?=([%o *] data)  ~
          ~(tap by p.data)
      ==
    :_  this
    [%give %fact ~ %json !>(jon)]~
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
  =/  req=request:http  (request-wttr str)
  =/  out  *outbound-config:iris
  =/  lismov=(list card)
    [%pass /[(scot %da now.bol)] %arvo %i %request req out]~
  ?~  timer
    :-  %+  weld  lismov
        ^-  (list card)
        :~  [%pass /timer %arvo %b %wait (add now.bol ~h3)]
            [%give %fact ~[/all] %json !>((frond:enjs:format %location jon))]
        ==
    %=  state
      location  str
      timer    `(add now.bol ~h3)
    ==
  :_  state(location str)
  %+  weld  lismov
  ^-  (list card)
  [%give %fact ~[/all] %json !>((frond:enjs:format %location jon))]~
::
++  request-wttr
  |=  location=@t
  ^-  request:http
  =/  base  'https://wttr.in/'
  =/  url=@t  (cat 3 (cat 3 base location) '?format=j1')
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
  =/  error  (~(get by p.u.ujon) 'error')
  ?^  error
    ~&  "fetching weather failed: {<u.error>}"
    [~ state]
  =/  jon=json
    %+  frond:enjs:format  %weather
    %-  pairs:enjs:format
    :~  [%current-condition (~(got by p.u.ujon) 'current_condition')]
        [%weather (~(got by p.u.ujon) 'weather')]
        [%nearest-area (~(got by p.u.ujon) 'nearest_area')]
    ==
  :-  [%give %fact ~[/all] %json !>(jon)]~
  %=  state
    data  jon
    time  now.bol
  ==
::
++  wake
  |=  [wir=wire err=(unit tang)]
  ^-  (quip card _state)
  ?~  err
    =/  req=request:http  (request-wttr location)
    =/  out  *outbound-config:iris
    :_  state(timer `(add now.bol ~h3))
    :~  [%pass /[(scot %da now.bol)] %arvo %i %request req out]
        [%pass /timer %arvo %b %wait (add now.bol ~h3)]
    ==
  %-  (slog u.err)
  [~ state]
::
--
