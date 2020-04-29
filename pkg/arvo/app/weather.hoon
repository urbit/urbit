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
    ?.  ?=([%weathertile ~] wire)  (on-watch:def wire)
    :_  this
    [%give %fact ~ %json !>(data)]~
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
