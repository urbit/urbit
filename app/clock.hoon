/+  *server
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
:: +move: output effect
::
+$  move  [bone card]
:: +card: output effect payload
::
+$  poke
  $%  [%noun [@tas path @t]]
  ==
::
+$  card
  $%  [%poke wire dock poke]
      [%http-response =http-event:http]
      [%connect wire binding:http-server term]
      [%diff %json json]
  ==
::
--
::
|_  [bol=bowl:gall ~]
::
++  this  .
::
++  bound
  |=  [wir=wire success=? binding=binding:http-server]
  ^-  (quip move _this)
  [~ this]
::
++  prep
  |=  old=(unit ~)
  ^-  (quip move _this)
  =/  launchnoun  [%noun [%clock /tile '/~clock/js/tile.js']]
  :_  this
  :~
    [ost.bol %connect / [~ /'~clock'] %clock]
    [ost.bol %poke /clock [our.bol %launch] launchnoun]
  ==
::
++  peer-tile
  |=  pax=path
  ^-  (quip move _this)
  [[ost.bol %diff %json *json]~ this]
::
++  send-tile-diff
  |=  jon=json
  ^-  (list move)
  %+  turn  (prey:pubsub:userlib /tile bol)
  |=  [=bone ^]
  [bone %diff %json jon]
::
++  poke-handle-http-request
  %-  (require-authorization:app ost.bol move this)
  |=  =inbound-request:http-server
  ^-  (quip move _this)
  =/  request-line  (parse-request-line url.request.inbound-request)
  =/  back-path  (flop site.request-line)
  =/  name=@t
    =/  back-path  (flop site.request-line)
    ?~  back-path
      ''
    i.back-path
  ::
  ?~  back-path
    [[ost.bol %http-response not-found:app]~ this]
  ?:  =(name 'tile')
    [[ost.bol %http-response (js-response:app tile-js)]~ this]
  [[ost.bol %http-response not-found:app]~ this]
::
--
