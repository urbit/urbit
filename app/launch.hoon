/+  *server, collections
/=  index
  /^  octs
  /;  as-octs:mimes:html
  /:  /===/app/launch/index
  /|  /html/
      /~  ~
  ==
/=  script
  /^  octs
  /;  as-octs:mimes:html
  /:  /===/app/launch/js/index
  /|  /js/
      /~  ~
  ==
/=  style
  /^  octs
  /;  as-octs:mimes:html
  /:  /===/app/launch/css/index
  /|  /css/
      /~  ~
  ==
/=  launch-png
  /^  (map knot @)
  /:  /===/app/launch/img  /_  /png/
::
|%
::
+$  move  [bone card]
::
+$  card
  $%  [%http-response =http-event:http]
      [%connect wire binding:http-server term]
      [%peer wire dock path]
      [%diff %json json]
  ==
+$  tile  [name=@tas subscribe=path]
+$  tile-data  (map @tas json)
+$  state
  $%  [%0 tiles=(set tile) data=tile-data path-to-tile=(map path @tas)]
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
  ?~  old
    :_  this
    [ost.bol %connect / [~ /] %launch]~
  [~ this(sta u.old)]
::
++  bound
  |=  [wir=wire success=? binding=binding:http-server]
  ^-  (quip move _this)
  [~ this]
::
++  poke-noun
  |=  [name=@tas subscribe=path]
  ^-  (quip move _this)
  :-  [ost.bol %peer subscribe [our.bol name] subscribe]~
  %=  this
    tiles.sta  (~(put in tiles.sta) [name subscribe])
    data.sta   (~(put by data.sta) name *json)
    path-to-tile.sta  (~(put by path-to-tile.sta) subscribe name)
  ==
::
++  diff-json
  |=  [pax=path jon=json]
  =/  name/@tas  (~(got by path-to-tile.sta) pax)
  :-
  %+  turn  (prey:pubsub:userlib /main bol)
  |=  [=bone *]
  [bone %diff %json (frond:enjs:format name jon)]
  %=  this
    data.sta  (~(put by data.sta) name jon)
  ==
::
++  peer-main
  |=  [pax=path]
  ^-  (quip move _this)
  =/  data/json  %-  pairs:enjs:format  ~(tap by data.sta)
  :_  this
  [ost.bol %diff %json data]~
::
++  poke-handle-http-request
  %-  (require-authorization:app ost.bol move this)
  |=  =inbound-request:http-server
  ^-  (quip move _this)
  ::
  =+  request-line=(parse-request-line url.request.inbound-request)
  =/  name=@t
    =+  back-path=(flop site.request-line)
    ?~  back-path
      ''
    i.back-path
  =/  site  (flop site.request-line)
  ?~  site
    :_  this
    [ost.bol %http-response (html-response:app index)]~
  ?+  site.request-line
    :_  this
    [ost.bol %http-response not-found:app]~
  ::
  ::  styling
  ::
      [%'~launch' %css %index ~]
    :_  this
    [ost.bol %http-response (css-response:app style)]~
  ::
  ::  javascript
  ::
      [%'~launch' %js %index ~]
    :_  this
    [ost.bol %http-response (js-response:app script)]~
  ::
  ::  images
  ::
      [%'~launch' %img *]
    =/  img  (as-octs:mimes:html (~(got by launch-png) `@ta`name))
    :_  this
    [ost.bol %http-response (png-response:app img)]~
  ==
::
--
