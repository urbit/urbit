/-  *permission-store, *group-store, *permission-group-hook, *chat-hook
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
  [~ this]
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
      %+  envelopes-update
        envelopes
      [start end pax]
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
    ::  TODO: add invites
    =/  group-read=path  [%chat (weld path.act /read)]
    =/  group-write=path  [%chat (weld path.act /write)]
    :_  this
    %+  weld
      :~  (chat-poke [%create path.act our.bol])
          (group-poke [%bundle group-read])
          (group-poke [%bundle group-write])
          (group-poke [%add read.act group-read])
          (group-poke [%add write.act group-write])
          (chat-hook-poke [%add-owned path.act security.act])
      ==
    (create-security [%chat path.act] security.act)
  ::
      %delete
    =/  group-read  [%chat (weld path.act /read)]
    =/  group-write  [%chat (weld path.act /write)]
    :_  this
    :~  (chat-hook-poke [%remove path.act])
        (group-poke [%unbundle group-read])
        (group-poke [%unbundle group-write])
        (chat-poke [%delete path.act])
    ==
  ::
  ==
::
++  peer-initial
  |=  pax=path
  ^-  (quip move _this)
  ?.  =(src.bol our.bol)  !!
  ::  create inbox with 100 messages max per mailbox and send that along
  ::  then quit the subscription
  :_  this
  :~  [ost.bol %diff %json (inbox-to-json (truncate-inbox all-scry))]
      [ost.bol %quit ~]
  ==
::
++  peer-updates
  |=  pax=path
  ^-  (quip move _this)
  ?.  =(src.bol our.bol)  !!
  ::  send along all subsequent updates
  [~ this]
::
++  peer-configs
  |=  pax=path
  ^-  (quip move _this)
  ?.  =(src.bol our.bol)  !!
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
    %+  turn  (prey:pubsub:userlib /updates bol)
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
++  envelopes-update
  |=  [envelopes=(list envelope) start=@ud end=@ud pax=path]
  ^-  json
  =,  enjs:format
  %+  frond  %chat-update
  %-  pairs
  :~
    :-  %messages
    %-  pairs
    :~  [%path (path pax)]
        [%start (numb start)]
        [%end (numb end)]
        [%envelopes [%a (turn envelopes enve)]]
    ==
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
::
--
