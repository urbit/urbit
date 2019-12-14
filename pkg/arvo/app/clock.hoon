/+  *server, default-agent, verb
/=  tile-js
  /^  octs
  /;  as-octs:mimes:html
  /:  /===/app/clock/js/tile
  /|  /js/
      /~  ~
  ==
=,  format
::
%+  verb  |
^-  agent:gall
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
::
++  on-init
  ^-  (quip card:agent:gall _this)
  =/  launcha
    [%launch-action !>([%clock /tile '/~clock/js/tile.js'])]
  :_  this
  :~  [%pass / %arvo %e %connect [~ /'~clock'] %clock]
      [%pass /clock %agent [our.bowl %launch] %poke launcha]
  ==
::  bootstrapping to get %goad started OTA
::
++  on-save   !>(%2)
++  on-load
  |=  old-state=vase
  =/  old  !<(?(~ %1 %2) old-state)
  =^  cards  this
    ?:  ?=(%2 old)
      `this
    :_  this  :_  ~
    [%pass /behn %arvo %b %wait +(now.bowl)]
  ::
  [cards this]
::
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card:agent:gall _this)
  ?.  ?=(%handle-http-request mark)
    (on-poke:def mark vase)
  =+  !<([eyre-id=@ta =inbound-request:eyre] vase)
  :_  this
  %+  give-simple-payload:app  eyre-id
  %+  require-authorization:app  inbound-request
  |=  =inbound-request:eyre
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
  not-found:gen
::
++  on-watch
  |=  =path
  ^-  (quip card:agent:gall _this)
  ?:  ?=([%http-response *] path)
    `this
  ?.  =(/tile path)
    (on-watch:def path)
  [[%give %fact ~ %json !>(*json)]~ this]
::
++  on-leave  on-leave:def
++  on-peek   on-peek:def
++  on-agent  on-agent:def
++  on-arvo
  |=  [=wire =sign-arvo]
  ^-  (quip card:agent:gall _this)
  ?:  ?=(%wake +<.sign-arvo)
    ?^  error.sign-arvo
      :_  this  :_  ~
      [%pass /dill %arvo %d %flog %crud %clock-fail u.error.sign-arvo]
    :_  this  :_  ~
    [%pass /gall %arvo %g %goad | `%hood]
  ::
  ?.  ?=(%bound +<.sign-arvo)
    (on-arvo:def wire sign-arvo)
  [~ this]
::
++  on-fail   on-fail:def
--
