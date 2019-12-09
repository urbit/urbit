::  contact-view: sets up contact JS client and combines commands
::  into semantic actions for the UI
::
/-  *group-store
/+  *server, *contact-json, base64
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
      [%peer wire dock path]
      [%poke wire dock poke]
      [%diff %json json]
  ==
::
+$  poke
  $%  [%launch-action [@tas path @t]]
      [%contact-action contact-action]
      [%group-action group-action]
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
    :~  [ost.bol %peer / [our.bol %contact-store] /updates]
        [ost.bol %connect / [~ /'~contacts'] %contact-view]
        (launch-poke [/configs '/~contacts/js/tile.js'])
        (contact-poke [%create /~/default])
        (group-poke [%bundle /~/default])
        (contact-poke [%add /~/default our.bol *contact])
        (group-poke [%add [our.bol ~ ~] /~/default])
    ==
  [~ this(+<+ u.old)]
::
++  poke-json
  |=  jon=json
  ^-  (quip move _this)
  ?>  (team:title our.bol src.bol)
  (poke-contact-view-action (json-to-view-action jon))
::
++  poke-contact-view-action
  |=  act=contact-view-action
  ^-  (quip move _this)
  ?>  (team:title our.bol src.bol)
  :_  this
  ?-  -.act
      %create
    :~  (group-poke [%bundle path.act])
        (contact-poke [%create path.act])
    ==
  ::
      %delete
    :~  (group-poke [%unbundle path.act])
        (contact-poke [%delete path.act])
    ==
  ::
      %add
    :~  (group-poke [%add [ship.act ~ ~] path.act])
        (contact-poke [%add path.act ship.act contact.act])
    ==
  ::
      %remove
    :~  (group-poke [%remove [ship.act ~ ~] path.act])
        (contact-poke [%remove path.act ship.act])
    ==
  ==
::
++  peer-primary
  |=  pax=path
  ^-  (quip move _this)
  ?>  (team:title our.bol src.bol)
  ::  create inbox with 100 messages max per mailbox and send that along
  ::  then quit the subscription
  :_  this
  [ost.bol %diff %json (rolodex-to-json all-scry)]~
::
++  diff-contact-update
  |=  [wir=wire upd=contact-update]
  ^-  (quip move _this)
  =/  updates-json  (update-to-json upd)
  :_  this
  %+  turn  (prey:pubsub:userlib /primary bol)
  |=  [=bone *]
  [bone %diff %json updates-json]
::
++  quit
  |=  wir=wire
  ^-  (quip move _this)
  :_  this
  [ost.bol %peer / [our.bol %contact-store] /updates]~
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
  ::  user interface images
  ::
      [%'~contacts' %img *]
    =/  img  (as-octs:mimes:html (~(got by contact-png) `@ta`name))
    :_  this
    [ost.bol %http-response (png-response:app img)]~
  ::
  ::  avatar images
  ::
      [%'~contacts' %avatar @ *]
    =/  pax=path  `path`t.t.site.url 
    ?~  pax  [[ost.bol %http-response not-found:app]~ this]
    =/  pas  `path`(flop pax)
    ?~  pas  [[ost.bol %http-response not-found:app]~ this]
    =/  pav  `path`(flop t.pas)
    ~&  pav+pav
    ~&  name+name
    =/  contact  (contact-scry `path`(weld pav [name]~))
    ?~  contact  [[ost.bol %http-response not-found:app]~ this]
    ?~  avatar.u.contact  [[ost.bol %http-response not-found:app]~ this]
    =*  avatar  u.avatar.u.contact
    =*  content-type  content-type.avatar
    =*  octs  octs.avatar
    ~&  type+content-type
    :_  this
    :~  :*  ost.bol
            %http-response
            :^  %start
            [200 ['content-type' content-type]~]
            [~ (need (de:base64 q.octs))]
            %.y
    ==  ==
  ::
  ::  main page
  ::
     [%'~contacts' *]
    :_  this
    [ost.bol %http-response (html-response:app index)]~
  ==
::
::
++  bound
  |=  [wir=wire success=? binding=binding:eyre]
  ^-  (quip move _this)
  [~ this]
::
::  +utilities
::
++  contact-poke
  |=  act=contact-action
  ^-  move
  [ost.bol %poke / [our.bol %contact-store] [%contact-action act]]
::
++  launch-poke
  |=  [pax=path =cord]
  ^-  move
  [ost.bol %poke / [our.bol %launch] [%launch-action [%contact-view pax cord]]]
::
++  group-poke
  |=  act=group-action
  ^-  move
  [ost.bol %poke / [our.bol %group-store] [%group-action act]]
::
++  all-scry
  ^-  rolodex
  .^(rolodex %gx /=contact-store/(scot %da now.bol)/all/noun)
::
++  contact-scry
  |=  pax=path
  ^-  (unit contact)
  =.  pax  ;:(weld /=contact-store/(scot %da now.bol)/contact pax /noun)
  .^((unit contact) %gx pax)
--
