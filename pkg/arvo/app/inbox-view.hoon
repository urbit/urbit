/+  *server
/=  index
  /^  octs
  /;  as-octs:mimes:html
  /:  /===/app/chat/index
  /|  /html/
      /~  ~
  ==
/=  tile-js
  /^  octs
  /;  as-octs:mimes:html
  /:  /===/app/chat/js/tile
  /|  /js/
      /~  ~
  ==
/=  script
  /^  octs
  /;  as-octs:mimes:html
  /:  /===/app/chat/js/index
  /|  /js/
      /~  ~
  ==
/=  style
  /^  octs
  /;  as-octs:mimes:html
  /:  /===/app/chat/css/index
  /|  /css/
      /~  ~
  ==
/=  chat-png
  /^  (map knot @)
  /:  /===/app/chat/img  /_  /png/
::
|%
::
+$  move  [bone card]
::
+$  card
  $%  [%http-response =http-event:http]
      [%connect wire binding:eyre term]
      [%poke wire dock [%launch-action [@tas path @t]]]
  ==
--
::
|_  [bol=bowl:gall ~]
::
++  this  .
::
::  +prep: set up the app, migrate the state
::
++  prep
  |=  old=*
  ^-  (quip move _this)
  :_  this
  :~  [ost.bol %connect / [~ /'~chat'] %inbox-view]
      (launch-poke [/chattile '/~chat/js/tile.js'])
  ==
::
::  +bound: lient tells us we successfully bound our server to the ~chat url
::
++  bound
  |=  [wir=wire success=? binding=binding:eyre]
  ^-  (quip move _this)
  [~ this]
::
::  +poke-handle-http-request: serve pages from file system based on URl path
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
      [%'~chat' %css %index ~]
    :_  this
    [ost.bol %http-response (css-response:app style)]~
  ::
  ::  javascript
  ::
      [%'~chat' %js %index ~]
    :_  this
    [ost.bol %http-response (js-response:app script)]~
  ::
  ::  images
  ::
      [%'~chat' %img *]
    =/  img  (as-octs:mimes:html (~(got by chat-png) `@ta`name))
    :_  this
    [ost.bol %http-response (png-response:app img)]~
  ::
  ::
  ::  inbox page
  ::
     [%'~chat' *]
    :_  this
    [ost.bol %http-response (html-response:app index)]~
  ==
::
++  launch-poke
  |=  [=path =cord]
  ^-  move
  [ost.bol %poke /chat [our.bol %launch] [%launch-action %chat path cord]]
::
--
