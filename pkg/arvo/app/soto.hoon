::  Soto: A Dojo relay for Urbit's Landscape interface
::  Relays sole-effects to subscribers and forwards sole-action pokes
/-  sole
/+  *server, *sole
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
=,  format
|%
+$  state
  $%  [%0 bon=bone]                                     :: store bone from Dojo peer
  ==
::
+$  move  [bone card]
::
+$  poke
  $%  [%launch-action [@tas path @t]]
      [%sole-action sole-action:sole]
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
      [%sole-effect sole-effect:sole]
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
    [%launch-action [%soto /sototile '/~soto/js/tile.js']]
  ?~  old
    :_  this
    :~ 
        [ost.bol %connect / [~ /'~soto'] %soto]
        [ost.bol %poke /soto [our.bol %launch] launcha]
    ==
  :-  [ost.bol %poke /soto [our.bol %launch] launcha]~
  this(sta u.old)
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
  :-  [ost.bol %peer / [our.bol %dojo] /sole]~                                 :: poke/peer need same wire
  %=  this
    bon.sta  ost.bol
  ==
::
++  poke-sole-action
  |=  act=sole-action
  ^-  (quip move _this)
  :_  this
  [bon.sta %poke / [our.bol %dojo] [%sole-action act]]~                        :: poke/peer need same wire
::
++  diff-sole-effect
  |=  [=wire fec=sole-effect]
  ^-  (quip move _this)
  :_  this
  [bon.sta %diff %sole-effect fec]~
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
      [%'~soto' %css %index ~]
    :_  this
    [ost.bol %http-response (css-response:app style)]~
  ::
  ::  javascript
  ::
      [%'~soto' %js %index ~]
    :_  this
    [ost.bol %http-response (js-response:app script)]~
  ::
  ::  images
  ::
      [%'~soto' %img *]
    =/  img  (as-octs:mimes:html (~(got by soto-png) `@ta`name))
    :_  this
    [ost.bol %http-response (png-response:app img)]~
  ::
  ::  index page
  ::
     [%'~soto' *]
    :_  this
    [ost.bol %http-response (html-response:app index)]~
  ==
::
--
