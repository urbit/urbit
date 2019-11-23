::  contact-view: sets up contact JS client and combines commands
::  into semantic actions for the UI
::
/+  *server, *contact-json
/=  index
  /^  octs
  /;  as-octs:mimes:html
  /:  /===/app/contacts/index
  /|  /html/
      /~  ~
  ==
/=  tile-js
  /^  octs
  /;  as-octs:mimes:html
  /:  /===/app/contacts/js/tile
  /|  /js/
      /~  ~
  ==
/=  script
  /^  octs
  /;  as-octs:mimes:html
  /:  /===/app/contacts/js/index
  /|  /js/
      /~  ~
  ==
/=  style
  /^  octs
  /;  as-octs:mimes:html
  /:  /===/app/contacts/css/index
  /|  /css/
      /~  ~
  ==
/=  contact-png
  /^  (map knot @)
  /:  /===/app/contacts/img  /_  /png/
::
|%
::
+$  move  [bone card]
::
+$  card
  $%  [%http-response =http-event:http]
      [%connect wire binding:eyre term]
      [%quit ~]
      [%poke wire dock poke]
  ==
::
+$  poke
  $%  [%launch-action [@tas path @t]]
  ==
--
::
|_  [bol=bowl:gall ?]
::
++  this  .
::
++  prep
  |=  old=(unit ?)
  ^-  (quip move _this)
  ?~  old
    :_  this
    :~  [ost.bol %connect / [~ /'~contacts'] %contact-view]
        (launch-poke [/configs '/~contacts/js/tile.js'])
    ==
  [~ this(+<+ u.old)]
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
  =+  url=(parse-request-line url.request.inbound-request)
  =/  name=@t
    =+  back-path=(flop site.url)
    ?~  back-path
      ''
    i.back-path
  ?:  =(name 'tile')
    [[ost.bol %http-response (js-response:app tile-js)]~ this]
  ?+  site.url
    :_  this
    [ost.bol %http-response not-found:app]~
  ::
  ::  styling
  ::
      [%'~contacts' %css %index ~]
    :_  this
    [ost.bol %http-response (css-response:app style)]~
  ::
  ::  javascript
  ::
      [%'~contacts' %js %index ~]
    :_  this
    [ost.bol %http-response (js-response:app script)]~
  ::
  ::  images
  ::
      [%'~contacts' %img *]
    =/  img  (as-octs:mimes:html (~(got by contact-png) `@ta`name))
    :_  this
    [ost.bol %http-response (png-response:app img)]~
  ::
  ::  main page
  ::
     [%'~contacts' *]
    :_  this
    [ost.bol %http-response (html-response:app index)]~
  ==
::
::
::  +utilities
::
++  launch-poke
  |=  [pax=path =cord]
  ^-  move
  [ost.bol %poke / [our.bol %launch] [%launch-action [%contact-view pax cord]]]
--
