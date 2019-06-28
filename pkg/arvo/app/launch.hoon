
/+  *server, collections
/=  index
  /^  $-(marl manx)
  /:  /===/app/launch/index  /!noun/
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
+$  tile-data  (map @tas [jon=json url=@t])
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
  ~&  'launch prep'
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
  |=  [name=@tas subscribe=path url=@t]
  ^-  (quip move _this)
  =/  beforedata  (~(get by data.sta) name)
  =/  newdata
    ?~  beforedata
      (~(put by data.sta) name [*json url])
    (~(put by data.sta) name [jon.u.beforedata url])
  :-  [ost.bol %peer subscribe [our.bol name] subscribe]~
  %=  this
    tiles.sta  (~(put in tiles.sta) [name subscribe])
    data.sta  newdata
    path-to-tile.sta  (~(put by path-to-tile.sta) subscribe name)
  ==
::
++  diff-json
  |=  [pax=path jon=json]
  ^-  (quip move _this)
  =/  name/@tas  (~(got by path-to-tile.sta) pax)
  =/  data/(unit [json url=@t])  (~(get by data.sta) name)
  ?~  data
    [~ this]
  ::
  :-
  %+  turn  (prey:pubsub:userlib /main bol)
  |=  [=bone *]
  [bone %diff %json (frond:enjs:format name jon)]
  ::
  %=  this
    data.sta  (~(put by data.sta) name [jon url.u.data])
  ==
::
++  peer-main
  |=  [pax=path]
  ^-  (quip move _this)
  =/  data/json
    %-  pairs:enjs:format
    %+  turn  ~(tap by data.sta)
    |=  [key=@tas [jon=json url=@t]]
    [key jon]
  :_  this
  [ost.bol %diff %json data]~
::
++  generate-script-marl
  |=  data=tile-data
  ^-  marl
  %+  turn  ~(tap by data)
  |=  [key=@tas [jon=json url=@t]]
  ^-  manx
  ;script@"{(trip url)}";
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
    =/  hym=manx  (index (generate-script-marl data.sta))
    :_  this
    [ost.bol %http-response (manx-response:app hym)]~
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
