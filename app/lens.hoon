/+  *server
=,  format
|%
:: +move: output effect
::
+$  move  [bone card]
:: +card: output effect payload
::
+$  card
  $%  [%connect wire binding:http-server term]
      [%serve wire binding:http-server generator:http-server]
      [%disconnect wire binding:http-server]
      [%http-response =http-event:http]
      [%poke wire dock poke]
      [%diff %json json]
  ==
::
+$  poke
  $%  [%modulo-bind app=term]
      [%modulo-unbind app=term]
  ==
::
+$  state
  $%  $:  %0   
          session=(map term @t)
          order=(list term)
          cur=(unit [term @])
      ==
  ==
::
--
::
|_  [bow=bowl:gall sta=state]
::
++  this  .
::
++  prep
  |=  old=(unit *)
  ^-  (quip move _this)
  [~ this]
::
::  alerts us that we were bound. we need this because the vane calls back.
::
++  bound
  |=  [wir=wire success=? binding=binding:http-server]
  ^-  (quip move _this)
  [~ this]
::
++  poke-handle-http-request
  %-  (require-authorization:app ost.bow move this)
  |=  =inbound-request:http-server
  ^-  (quip move _this)
  ::
  =/  request-line  (parse-request-line url.request.inbound-request)
  =/  site  (flop site.request-line)
  ::
  =/  htm
    %-  manx-to-octs
    ;div: successfully contacted lens
  ~&  lens+inbound-request
  :_  this
  [ost.bow %http-response (html-response:app htm)]~
::
::  +poke-handle-http-cancel: received when a connection was killed
::
++  poke-handle-http-cancel
  |=  =inbound-request:http-server
  ^-  (quip move _this)
  ::  the only long lived connections we keep state about are the stream ones.
  ::
  [~ this]
::
++  poke-noun
  |=  a=*
  ^-  (quip move _this)
  ~&  poke+a
  [~ this]
::
--
