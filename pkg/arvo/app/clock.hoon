/+  *server, default-agent, verb, dbug
/=  tile-js
  /^  octs
  /;  as-octs:mimes:html
  /:  /===/app/clock/js/tile
  /|  /js/
      /~  ~
  ==
=,  format
::
|%
::
+$  card  card:agent:gall
+$  versioned-state
  $%  state-zero
  ==
+$  state-zero  [%0 data=json]
--
%+  verb  |
%-  agent:dbug
=|  state-zero
=*  state  -
^-  agent:gall
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
::
++  on-init
  ^-  (quip card:agent:gall _this)
  =/  launcha
    [%launch-action !>([%add %clock /clocktile '/~clock/js/tile.js'])]
  :_  this
  :~  [%pass / %arvo %e %connect [~ /'~clock'] %clock]
      [%pass /clock %agent [our.bowl %launch] %poke launcha]
  ==
::  bootstrapping to get %goad started OTA
::
++  on-save   !>(%3)
++  on-load
  |=  old-state=vase
  ^-  (quip card _this)
  =/  old  !<(?(~ %1 %2 %3) old-state)
  =^  cards  this
    ?:  ?=(%3 old)
      `this
    ?:  ?=(%2 old)
      ::  ensure launch is set up to listen to us correctly
      ::
      =/  launcha
        [%launch-action !>([%add %clock /clocktile '/~clock/js/tile.js'])]
      :_  this
      [%pass /clock %agent [our.bowl %launch] %poke launcha]~
    :_  this  :_  ~
    [%pass /behn %arvo %b %wait +(now.bowl)]
  ::
  [cards this]
::
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card:agent:gall _this)
  |^
  ?:  ?=(%json mark)
    (poke-json !<(json vase))
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
  ++  poke-json
    |=  jon=json
    ^-  (quip card:agent:gall _this)
    =.  data.state  jon
    :_  this
    [%give %fact ~[/clocktile] %json !>(jon)]~
  --
::
++  on-watch
  |=  =path
  ^-  (quip card:agent:gall _this)
  ?:  ?=([%http-response *] path)
    `this
  ?.  =(/clocktile path)
    (on-watch:def path)
  [[%give %fact ~ %json !>(data.state)]~ this]
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
