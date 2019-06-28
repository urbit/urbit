/+  *server
|%
:: +move: output effect
::
+$  move  [bone card]
:: +card: output effect payload
::
+$  card
  $%  [%connect wire binding:http-server term]
      [%disconnect wire binding:http-server]
      [%http-response =http-event:http]
  ==
::
--
::
|_  [bow=bowl:gall ~]
::
++  this  .
::
++  prep
  |=  old=(unit *)
  ^-  (quip move _this)
  ?~  old
    :_  this
    [ost.bow %connect / [~ /'~modulo'] %modulo]~
  [~ this]
::
::  alerts us that we were bound. we need this because the vane calls back.
::
++  bound
  |=  [wir=wire success=? binding=binding:http-server]
  ^-  (quip move _this)
  [~ this]
::
++  session-js
  ^-  octs
  %-  as-octt:mimes:html
  ;:  weld
      "window.ship = '{+:(scow %p our.bow)}';"
      "window.urb = new Channel();"
  ==
::
::  +poke-handle-http-request: received on a new connection established
::
++  poke-handle-http-request
  %-  (require-authorization:app ost.bow move this)
  |=  =inbound-request:http-server
  ^-  (quip move _this)
  [[ost.bow %http-response (js-response:app session-js)]~ this]
::
--
