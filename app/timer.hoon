/+  *server
/=  tile-js
  /^  octs
  /;  as-octs:mimes:html
  /:  /===/app/timer/js/tile
  /|  /js/
      /~  ~
  ==
/=  timer-png
  /^  (map knot @)
  /:  /===/app/timer/img  /_  /png/
=,  format
::
|%
:: +move: output effect
::
+$  move  [bone card]
:: +card: output effect payload
::
+$  poke
  $%  [%noun [@tas path]]
  ==
::
+$  card
  $%  [%poke wire dock poke]
      [%http-response =http-event:http]
      [%connect wire binding:http-server term]
      [%diff %json json]
      [%wait wire @da]
      [%rest wire @da]
  ==
::
--
::
|_  [bol=bowl:gall tim=@da]
::
++  this  .
::
++  bound
  |=  [wir=wire success=? binding=binding:http-server]
  ^-  (quip move _this)
  [~ this]
::
++  prep
  |=  old=(unit tim=@da)
  ^-  (quip move _this)
  =/  lismov/(list move)  %+  weld
    `(list move)`[ost.bol %connect / [~ /'~timer'] %timer]~
    `(list move)`[ost.bol %poke /timer [our.bol %launch] [%noun [%timer /tile]]]~
  :-  lismov
  ?~  old
    this
  %=  this
    tim  tim.u.old
  ==
::
++  peer-tile
  |=  pax=path
  ^-  (quip move _this)
  ?:  =(tim *@da)
    [[ost.bol %diff %json [%s '']]~ this]
  [[ost.bol %diff %json [%s (scot %da tim)]]~ this]
::
++  send-tile-diff
  |=  jon=json
  ^-  (list move)
  %+  turn  (prey:pubsub:userlib /tile bol)
  |=  [=bone ^]
  [bone %diff %json jon]
::
++  poke-json
  |=  jon=json
  ^-  (quip move _this)
  ~&  jon
  ?.  ?=(%s -.jon)
    [~ this]
  =/  str/@t  +.jon
  ?:  =(str 'start')
    =/  data/@da  (add now.bol ~s10)
    :-  %+  weld
      `(list move)`(send-tile-diff [%s (scot %da data)])
      `(list move)`[ost.bol %wait /timer data]~
    this(tim data)
  ?:  =(str 'stop')
    :-  %+  weld
      `(list move)`(send-tile-diff [%s ''])
      `(list move)`[ost.bol %rest /timer tim]~
    this(tim *@da)
  [~ this]
::
++  poke-handle-http-request
  %-  (require-authorization:app ost.bol move this)
  |=  =inbound-request:http-server
  ^-  (quip move _this)
  =+  request-line=(parse-request-line url.request.inbound-request)
  =+  back-path=(flop site.request-line)
  =/  name=@t
    =+  back-path=(flop site.request-line)
    ?~  back-path
      ''
    i.back-path
  ::
  ~&  site.request-line
  ?+  site.request-line
    [[ost.bol %http-response not-found:app]~ this]
  ::
  ::  tile
  ::
      [%'~timer' %tile ~]
    [[ost.bol %http-response (js-response:app tile-js)]~ this]
  ::
  ::  images
  ::
      [%'~timer' %img *]
    =/  img  (as-octs:mimes:html (~(got by timer-png) `@ta`name))
    :_  this
    [ost.bol %http-response (png-response:app img)]~
  ==
::
++  wake
  |=  [wir=wire err=(unit tang)]
  ^-  (quip move _this)
  :-  (send-tile-diff [%s 'alarm'])
  this(tim *@da)
::
--
