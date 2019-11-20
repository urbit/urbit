::
::  Soto: A Dojo relay for Urbit's Landscape interface
::  Relays sole-effects to subscribers and forwards sole-action pokes
::
/-  sole
/+  *server, *soto
/=  index
  /^  octs
  /;  as-octs:mimes:html
  /:  /===/app/soto/index
  /|  /html/
      /~  ~
  ==
/=  tile-js
  /^  octs
  /;  as-octs:mimes:html
  /:  /===/app/soto/js/tile
  /|  /js/
      /~  ~
  ==
/=  script
  /^  octs
  /;  as-octs:mimes:html
  /:  /===/app/soto/js/index
  /|  /js/
      /~  ~
  ==
/=  style
  /^  octs
  /;  as-octs:mimes:html
  /:  /===/app/soto/css/index
  /|  /css/
      /~  ~
  ==
/=  soto-png
  /^  (map knot @)
  /:  /===/app/soto/img  /_  /png/
::
|%
+$  state
::  bon: bone from Dojo peer
::
  $%  [%0 bon=bone]
  ==
::
+$  move  [bone card]
::
+$  poke
  $%  [%launch-action [@tas path @t]]
      [%sole-action sole-action]
      [%json json]
  ==
::
+$  card
  $%  [%poke wire dock poke]
      [%peer wire dock path]
      [%http-response =http-event:http]
      [%connect wire binding:eyre term]
      [%diff diff]
  ==
::
+$  diff
  $%  [%json json]
      [%sole-effect sole-effect]
  ==
::
--
::
|_  [bol=bowl:gall sta=state]
::
++  this  .
::
++  prep
  |=  old=(unit state)
  ^-  (quip move _this)
  =/  launcha=poke
    [%launch-action [%soto /sototile '/~dojo/js/tile.js']]
  ?~  old
    :_  this
    :~ 
        [ost.bol %connect / [~ /'~dojo'] %soto]
        [ost.bol %poke /soto [our.bol %launch] launcha]
    ==
  [~ this(sta u.old)]
::
++  peer-sototile
  |=  wir=wire
  ^-  (quip move _this)
  :_  this
  [ost.bol %diff %json *json]~
::
:: Peering Dojo when peered by front-end, initiating the session
::
++  peer-primary
  |=  wir=wire
  ^-  (quip move _this)
  ?>  (team:title our.bol src.bol)
  ::  poke/peer need same wire
  ::
  :_  this(bon.sta ost.bol)
  [ost.bol %peer / [our.bol %dojo] /sole]~
::
++  poke-json
  |=  =json
  ^-  (quip move _this)
  ?>  (team:title our.bol src.bol)
  (poke-sole-action (json-to-action json))
::
++  poke-sole-action
  |=  act=sole-action
  ^-  (quip move _this)
  ::  poke/peer need same wire
  ::
  :_  this
  [bon.sta %poke / [our.bol %dojo] [%sole-action act]]~
::
++  diff-sole-effect
  |=  [=wire fec=sole-effect]
  ^-  (quip move _this)
  :_  this
  [bon.sta %diff %json (effect-to-json fec)]~
::
++  bound
  |=  [wir=wire success=? binding=binding:eyre]
  ^-  (quip move _this)
  [~ this]
::
++  poke-handle-http-request
  %-  (require-authorization:app ost.bol move this)
  |=  =inbound-request:eyre
  ^-  (quip move _this)
  ::
  =+  request-line=(parse-request-line url.request.inbound-request)
  =/  name=@t
    =+  back-path=(flop site.request-line)
    ?~  back-path
      ''
    i.back-path
  ?:  =(name 'tile')
    [[ost.bol %http-response (js-response:app tile-js)]~ this]
  ?+  site.request-line
    :_  this
    [ost.bol %http-response not-found:app]~
  ::
  ::  styling
  ::
      [%'~dojo' %css %index ~]
    :_  this
    [ost.bol %http-response (css-response:app style)]~
  ::
  ::  javascript
  ::
      [%'~dojo' %js %index ~]
    :_  this
    [ost.bol %http-response (js-response:app script)]~
  ::
  ::  images
  ::
      [%'~dojo' %img *]
    =/  img  (as-octs:mimes:html (~(got by soto-png) `@ta`name))
    :_  this
    [ost.bol %http-response (png-response:app img)]~
  ::
  ::  index page
  ::
     [%'~dojo' *]
    :_  this
    [ost.bol %http-response (html-response:app index)]~
  ==
::
--
