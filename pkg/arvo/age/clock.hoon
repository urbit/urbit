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
%+  verb  &
%-  http-handler
^-  agent:mall
|_  =bowl:mall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
::
++  on-init
  ^-  (quip card:agent:mall _this)
  =/  launcha
    [%launch-action !>([%clock /tile '/~clock/js/tile.js'])]
  :_  this
  :~  [%pass / %arvo %e %connect [~ /'~clock'] %clock]
      [%pass /clock %agent [our.bowl %launch] %poke launcha]
  ==
++  on-save   on-save:def
++  on-load   on-load:def
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card:agent:mall _this)
  ?.  ?=(%http-request mark)
    (on-poke:def mark vase)
  =+  !<([=path =inbound-request:eyre] vase)
  :_  this
  %+  give-simple-payload:app  path
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
  ^-  (quip card:agent:mall _this)
  ?.  =(/tile path)
    (on-watch:def path)
  [[%give %fact ~ %json !>(*json)]~ this]
::
++  on-leave  on-leave:def
++  on-peek   on-peek:def
++  on-agent  on-agent:def
++  on-arvo
  |=  [=wire =sign-arvo]
  ^-  (quip card:agent:mall _this)
  ?.  ?=(%bound +<.sign-arvo)
    (on-arvo:def wire sign-arvo)
  [~ this]
::
++  on-fail   on-fail:def
--
