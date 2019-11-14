::  chat-view: sets up chat JS client, paginates data, and combines commands
::  into semantic actions for the UI
::
/-  *permission-store,
    *permission-hook,
    *group-store,
    *permission-group-hook,
    *chat-hook
/+  *server, *chat-json
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
      [%peer wire dock path]
      [%poke wire dock poke]
      [%diff %json json]
      [%quit ~]
  ==
::
+$  poke
  $%  [%launch-action [@tas path @t]]
      [%chat-action chat-action]
      [%group-action group-action]
      [%chat-hook-action chat-hook-action]
      [%permission-hook-action permission-hook-action]
      [%permission-group-hook-action permission-group-hook-action]
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
    :~  [ost.bol %peer / [our.bol %chat-store] /updates]
        [ost.bol %connect / [~ /'~chat'] %chat-view]
        (launch-poke [/configs '/~chat/js/tile.js'])
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
    [%'~chat' %paginate @t @t *]
    =/  start  (need (rush i.t.t.site.url dem))
    =/  end  (need (rush i.t.t.t.site.url dem))
    =/  pax  t.t.t.t.site.url
    =/  envelopes  (envelope-scry [(scot %ud start) (scot %ud end) pax])
    :_  this
    :~
      :+  ost.bol
        %http-response
      %-  json-response:app
      %-  json-to-octs 
      %-  update-to-json
      [%messages pax start end envelopes]
    ==
  ::
  ::  inbox page
  ::
     [%'~chat' *]
    :_  this
    [ost.bol %http-response (html-response:app index)]~
  ==
::
++  poke-json
  |=  jon=json
  ^-  (quip move _this)
  ?.  =(src.bol our.bol)
    [~ this]
  (poke-chat-view-action (json-to-view-action jon))
::
++  poke-chat-view-action
  |=  act=chat-view-action
  ^-  (quip move _this)
  ?.  =(src.bol our.bol)
    [~ this]
  ?-  -.act
      %create
    =/  pax  [(scot %p our.bol) path.act]
    =/  group-read=path  [%chat (weld pax /read)]
    =/  group-write=path  [%chat (weld pax /write)]
    :_  this
    %-  zing
    :~  :~  (group-poke [%bundle group-read])
            (group-poke [%bundle group-write])
            (group-poke [%add read.act group-read])
            (group-poke [%add write.act group-write])
            (chat-poke [%create our.bol path.act])
            (chat-hook-poke [%add-owned pax security.act allow-history.act])
        ==
        (create-security [%chat pax] security.act)
        :~  (permission-hook-poke [%add-owned group-read group-read])
            (permission-hook-poke [%add-owned group-write group-read])
        ==
    ==
  ::
      %delete
    =/  group-read  [%chat (weld path.act /read)]
    =/  group-write  [%chat (weld path.act /write)]
    :_  this
    :~  (chat-hook-poke [%remove path.act])
        (permission-hook-poke [%remove group-read])
        (permission-hook-poke [%remove group-write])
        (group-poke [%unbundle group-read])
        (group-poke [%unbundle group-write])
        (chat-poke [%delete path.act])
    ==
  ::
      %join
    =/  group-read  [%chat (scot %p ship.act) (weld path.act /read)]
    =/  group-write  [%chat (scot %p ship.act) (weld path.act /write)]
    :_  this
    :~  (chat-hook-poke [%add-synced ship.act path.act ask-history.act])
        (permission-hook-poke [%add-synced ship.act group-write])
        (permission-hook-poke [%add-synced ship.act group-read])
    ==
  ::
  ==
::
++  peer-primary
  |=  pax=path
  ^-  (quip move _this)
  ?>  (team:title our.bol src.bol)
  ::  create inbox with 100 messages max per mailbox and send that along
  ::  then quit the subscription
  :_  this
  [ost.bol %diff %json (inbox-to-json (truncate-inbox all-scry))]~
::
++  peer-configs
  |=  pax=path
  ^-  (quip move _this)
  ?>  (team:title our.bol src.bol)
  :_  this
  [ost.bol %diff %json *json]~
::
++  diff-chat-update
  |=  [wir=wire upd=chat-update]
  ^-  (quip move _this)
  =/  updates-json  (update-to-json upd)
  =/  configs-json  (configs-to-json configs-scry)
  :_  this
  %+  weld
    %+  turn  (prey:pubsub:userlib /primary bol)
    |=  [=bone *]
    [bone %diff %json updates-json]
  %+  turn  (prey:pubsub:userlib /configs bol)
  |=  [=bone *]
  [bone %diff %json configs-json]
::
++  quit
  |=  wir=wire
  ^-  (quip move _this)
  :_  this
  [ost.bol %peer / [our.bol %chat-store] /updates]~
::
::  +utilities
::
++  launch-poke
  |=  [=path =cord]
  ^-  move
  [ost.bol %poke / [our.bol %launch] [%launch-action %chat-view path cord]]
::
++  chat-poke
  |=  act=chat-action
  ^-  move
  [ost.bol %poke / [our.bol %chat-store] [%chat-action act]]
::
++  group-poke
  |=  act=group-action
  ^-  move
  [ost.bol %poke / [our.bol %group-store] [%group-action act]]
::
++  chat-hook-poke
  |=  act=chat-hook-action
  ^-  move
  [ost.bol %poke / [our.bol %chat-hook] [%chat-hook-action act]]
::
++  permission-hook-poke
  |=  act=permission-hook-action
  ^-  move
  [ost.bol %poke / [our.bol %permission-hook] [%permission-hook-action act]]
::
++  perm-group-hook-poke
  |=  act=permission-group-hook-action
  ^-  move
  =/  pok  [%permission-group-hook-action act]
  [ost.bol %poke / [our.bol %permission-group-hook] pok]
::
++  envelope-scry
  |=  pax=path
  ^-  (list envelope)
  =.  pax  ;:(weld /=chat-store/(scot %da now.bol)/envelopes pax /noun)
  .^((list envelope) %gx pax)
::
++  all-scry
  ^-  inbox
  .^(inbox %gx /=chat-store/(scot %da now.bol)/all/noun)
::
++  configs-scry
  ^-  chat-configs
  .^(chat-configs %gx /=chat-store/(scot %da now.bol)/configs/noun)
::
++  create-security
  |=  [pax=path sec=chat-security]
  ^-  (list move)
  =/  read   (weld pax /read)
  =/  write  (weld pax /write)
  ?-  sec
      %channel
    :~  (perm-group-hook-poke [%associate read [[read %black] ~ ~]])
        (perm-group-hook-poke [%associate write [[write %black] ~ ~]])
    ==
  ::
      %village
    :~  (perm-group-hook-poke [%associate read [[read %white] ~ ~]])
        (perm-group-hook-poke [%associate write [[write %white] ~ ~]])
    ==
  ::
      %journal
    :~  (perm-group-hook-poke [%associate read [[read %black] ~ ~]])
        (perm-group-hook-poke [%associate write [[write %white] ~ ~]])
    ==
  ::
      %mailbox
    :~  (perm-group-hook-poke [%associate read [[read %white] ~ ~]])
        (perm-group-hook-poke [%associate write [[write %black] ~ ~]])
    ==
  ::
  ==
::
++  truncate-envelopes
  |=  envelopes=(list envelope)
  ^-  (list envelope)
  =/  length  (lent envelopes)
  ?:  (lth length 100)
    envelopes
  (swag [(sub length 100) 100] envelopes)
::
++  truncate-inbox
  |=  box=inbox
  ^-  inbox
  %-  ~(run by box)
  |=  mail=mailbox
  ^-  mailbox
  :-  config.mail
  (truncate-envelopes envelopes.mail)
--
