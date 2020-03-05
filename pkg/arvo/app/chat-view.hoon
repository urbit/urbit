::  chat-view: sets up chat JS client, paginates data, and combines commands
::  into semantic actions for the UI
::
/-  *permission-store,
    *permission-hook,
    *group-store,
    *invite-store,
    *metadata-store,
    *permission-group-hook,
    *chat-hook,
    *metadata-hook
/+  *server, *chat-json, default-agent, verb, dbug
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
+$  card  card:agent:gall
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
%+  verb  |
%-  agent:dbug
^-  agent:gall
=<
  |_  bol=bowl:gall
  +*  this       .
      chat-core  +>
      cc         ~(. chat-core bol)
      def        ~(. (default-agent this %|) bol)
  ::
  ++  on-init
    ^-  (quip card _this)
    =/  launcha  [%launch-action !>([%chat-view /configs '/~chat/js/tile.js'])]
    :_  this
    :~  [%pass /updates %agent [our.bol %chat-store] %watch /updates]
        [%pass / %arvo %e %connect [~ /'~chat'] %chat-view]
        [%pass /chat-view %agent [our.bol %launch] %poke launcha]
    ==
  ++  on-poke
    |=  [=mark =vase]
    ^-  (quip card _this)
    ?>  (team:title our.bol src.bol)
    ?+    mark  (on-poke:def mark vase)
        %handle-http-request
      =+  !<([eyre-id=@ta =inbound-request:eyre] vase)
      :_  this
      %+  give-simple-payload:app  eyre-id
      %+  require-authorization:app  inbound-request
      poke-handle-http-request:cc
    ::
        %json
      :_  this
      (poke-chat-view-action:cc (json-to-view-action !<(json vase)))
    ::
        %chat-view-action
      :_  this
      (poke-chat-view-action:cc !<(chat-view-action vase))
    ==
  ::
  ++  on-watch
    |=  =path
    ^-  (quip card _this)
    ?>  (team:title our.bol src.bol)
    |^
    ?:  ?=([%http-response *] path)
      [~ this]
    ?:  =(/primary path)
      ::  create inbox with 100 messages max per mailbox and send that along
      ::  then quit the subscription
      :_  this
      [%give %fact ~ %json !>((inbox-to-json truncated-inbox-scry))]~
    ?:  =(/configs path)
      [[%give %fact ~ %json !>(*json)]~ this]
    (on-watch:def path)
    ::
    ++  truncated-inbox-scry
      ^-  inbox
      =/  =inbox  .^(inbox %gx /=chat-store/(scot %da now.bol)/all/noun)
      %-  ~(run by inbox)
      |=  =mailbox
      ^-  ^mailbox
      [config.mailbox (truncate-envelopes envelopes.mailbox)]
    ::
    ++  truncate-envelopes
      |=  envelopes=(list envelope)
      ^-  (list envelope)
      =/  length  (lent envelopes)
      ?:  (lth length 100)
        envelopes
      (swag [(sub length 100) 100] envelopes)
    --
  ::
  ++  on-agent
    |=  [=wire =sign:agent:gall]
    ^-  (quip card _this)
    ?+    -.sign  (on-agent:def wire sign)
        %kick
      :_  this
      [%pass / %agent [our.bol %chat-store] %watch /updates]~
    ::
        %fact
      ?+  p.cage.sign  (on-agent:def wire sign)
          %chat-update
        :_  this
        (diff-chat-update:cc !<(chat-update q.cage.sign))
      ==
    ==
  ::
  ++  on-arvo   
    |=  [=wire =sign-arvo]
    ^-  (quip card _this)
    ?.  ?=(%bound +<.sign-arvo)
      (on-arvo:def wire sign-arvo)
    [~ this]
  ::
  ++  on-save  on-save:def
  ++  on-load  on-load:def
  ++  on-leave  on-leave:def
  ++  on-peek   on-peek:def
  ++  on-fail   on-fail:def
  --
::
::
|_  bol=bowl:gall
::
++  poke-handle-http-request
  |=  =inbound-request:eyre
  ^-  simple-payload:http
  =+  url=(parse-request-line url.request.inbound-request)
  ?+  site.url  not-found:gen
      [%'~chat' %css %index ~]  (css-response:gen style)
      [%'~chat' %js %tile ~]    (js-response:gen tile-js)
      [%'~chat' %js %index ~]   (js-response:gen script)
  ::
      [%'~chat' %img @t *]
    =/  name=@t  i.t.t.site.url
    =/  img  (~(get by chat-png) name)
    ?~  img
      not-found:gen
    (png-response:gen (as-octs:mimes:html u.img))
  ::
      [%'~chat' %paginate @t @t *]
    =/  start  (need (rush i.t.t.site.url dem))
    =/  end  (need (rush i.t.t.t.site.url dem))
    =/  pax  t.t.t.t.site.url
    =/  envelopes  (envelope-scry [(scot %ud start) (scot %ud end) pax])
    %-  json-response:gen
    %-  json-to-octs 
    %-  update-to-json
    [%messages pax start end envelopes]
  ::
      [%'~chat' *]  (html-response:gen index)
  ==
::
++  poke-json
  |=  jon=json
  ^-  (list card)
  ?>  (team:title our.bol src.bol)
  (poke-chat-view-action (json-to-view-action jon))
::
++  poke-chat-view-action
  |=  act=chat-view-action
  ^-  (list card)
  |^
  ?>  (team:title our.bol src.bol)
  ?-  -.act
      %create
    ?>  ?=(^ app-path.act)
    ?>  |(=(group-path.act app-path.act) =(~(tap in members.act) ~))
    ?^  (chat-scry app-path.act)
      ~&  %chat-already-exists
      ~
    %-  zing
    :~  (create-chat app-path.act security.act allow-history.act)
        %-  create-group
        :*  group-path.act
            app-path.act
            security.act
            members.act
            title.act
            description.act
        ==
        (create-metadata title.act description.act group-path.act app-path.act)
    ==
  ::
      %delete
    =/  group-path  (group-from-chat app-path.act)
    ?>  ?=(^ app-path.act)
    %-  zing
    :~  :~  (chat-hook-poke [%remove app-path.act])
            (chat-poke [%delete app-path.act])
        ==
      ::
        ?.  (is-creator group-path %chat app-path.act)  ~
        [(metadata-poke [%remove group-path [%chat app-path.act]])]~
      ::
        ?:  (is-managed group-path)  ~
        :~  (group-poke [%unbundle group-path])
            (metadata-hook-poke [%remove group-path])
            (metadata-store-poke [%remove group-path [%chat app-path.act]])
        ==
    ==
  ::
      %join
    =/  group-path
      ?.  (is-managed app-path.act)  app-path.act
      (group-from-chat app-path.act)
    :~  (chat-hook-poke [%add-synced ship.act app-path.act ask-history.act])
        (permission-hook-poke [%add-synced ship.act group-path])
        (metadata-hook-poke [%add-synced ship.act group-path])
    ==
  ==
  ::
  ++  create-chat
    |=  [=path security=rw-security history=?]
    ^-  (list card)
    :~  (chat-poke [%create path])
        (chat-hook-poke [%add-owned path security history])
    ==
  ::
  ++  create-group
    |=  [=path app-path=path sec=rw-security ships=(set ship) title=@t desc=@t]
    ^-  (list card)
    =/  group  (group-scry path)
    ?^  group
      %-  zing
      %+  turn  ~(tap in u.group)
      |=  =ship
      ?:  =(ship our.bol)  ~
      [(send-invite app-path ship)]~
    ::  do not create a managed group if this is a sig path or a blacklist
    ::
    ?:  =(sec %channel)
      :~  (group-poke [%bundle path])
          (create-security path sec)
          (permission-hook-poke [%add-owned path path])
      ==
    ?:  (is-managed path)
      ~[(contact-view-poke [%create path ships title desc])]
    :~  (group-poke [%bundle path])
        (group-poke [%add ships path])
        (create-security path sec)
        (permission-hook-poke [%add-owned path path])
    ==
  ::
  ++  create-security
    |=  [pax=path sec=rw-security]
    ^-  card
    ?+  sec       !!
        %channel
      (perm-group-hook-poke [%associate pax [[pax %black] ~ ~]])
    ::
        %village
      (perm-group-hook-poke [%associate pax [[pax %white] ~ ~]])
    ==
  ::
  ++  create-metadata
    |=  [title=@t description=@t group-path=path app-path=path]
    ^-  (list card)
    =/  =metadata
      %*  .  *metadata
          title         title
          description   description
          date-created  now.bol
          creator
        %+  slav  %p
        ?:  (is-managed app-path)  (snag 0 app-path)
        (snag 1 app-path)
      ==
    :~  (metadata-poke [%add group-path [%chat app-path] metadata])
        (metadata-hook-poke [%add-owned group-path])
    ==
  ::
  ++  contact-view-poke
    |=  act=[%create =path ships=(set ship) title=@t description=@t]
    ^-  card
    [%pass / %agent [our.bol %contact-view] %poke %contact-view-action !>(act)]
  ::
  ++  metadata-poke
    |=  act=metadata-action
    ^-  card
    [%pass / %agent [our.bol %metadata-hook] %poke %metadata-action !>(act)]
  ::
  ++  metadata-store-poke
    |=  act=metadata-action
    ^-  card
    [%pass / %agent [our.bol %metadata-store] %poke %metadata-action !>(act)]
  ::
  ++  metadata-hook-poke
    |=  act=metadata-hook-action
    ^-  card
    :*  %pass  /  %agent
        [our.bol %metadata-hook]
        %poke  %metadata-hook-action
        !>(act)
    ==
  ::
  ++  send-invite
    |=  [=path =ship]
    ^-  card
    =/  =invite
      :*  our.bol  %chat-hook
          path  ship  ''
      ==
    =/  act=invite-action  [%invite /chat (shaf %msg-uid eny.bol) invite]
    [%pass / %agent [our.bol %invite-hook] %poke %invite-action !>(act)]
  ::
  ++  chat-scry
    |=  pax=path
    ^-  (unit mailbox)
    =.  pax  ;:(weld /=chat-store/(scot %da now.bol)/mailbox pax /noun)
    .^((unit mailbox) %gx pax)
  ::
  ++  group-from-chat
    |=  app-path=path
    ^-  path
    ?.  .^(? %gu (scot %p our.bol) %metadata-store (scot %da now.bol) ~)
      ?:  ?=([@ ^] app-path)
        ~&  [%assuming-ported-legacy-chat app-path]
        [%'~' app-path]
      ~&  [%weird-chat app-path]
      !!
    =/  resource-indices
      .^  (jug resource group-path)
        %gy
        (scot %p our.bol)
        %metadata-store
        (scot %da now.bol)
        /resource-indices
      ==
    =/  groups=(set path)  (~(got by resource-indices) [%chat app-path])
    (snag 0 ~(tap in groups))
  ::
  ++  is-managed
    |=  =path
    ^-  ?
    ?>  ?=(^ path)
    !=(i.path '~')
  ::
  ++  is-creator
    |=  [group-path=path app-name=@ta app-path=path]
    ^-  ?
    =/  meta=(unit metadata)
      .^  (unit metadata)
        %gx
        (scot %p our.bol)
        %metadata-store
        (scot %da now.bol)
        %metadata
        (scot %t (spat group-path))
        app-name
        (scot %t (spat app-path))
        /noun
      ==
    ?~  meta  !!
    =(our.bol creator.u.meta)
  --
::
++  diff-chat-update
  |=  upd=chat-update
  ^-  (list card)
  =/  updates-json  (update-to-json upd)
  =/  configs-json  (configs-to-json configs-scry)
  :~  [%give %fact ~[/primary] %json !>(updates-json)]
      [%give %fact ~[/configs] %json !>(configs-json)]
  ==
::
::  +utilities
::
++  chat-poke
  |=  act=chat-action
  ^-  card
  [%pass / %agent [our.bol %chat-store] %poke %chat-action !>(act)]
::
++  group-poke
  |=  act=group-action
  ^-  card
  [%pass / %agent [our.bol %group-store] %poke %group-action !>(act)]
::
++  permission-poke
  |=  act=permission-action
  ^-  card
  [%pass / %agent [our.bol %permission-store] %poke %permission-action !>(act)]
::
++  chat-hook-poke
  |=  act=chat-hook-action
  ^-  card
  [%pass / %agent [our.bol %chat-hook] %poke %chat-hook-action !>(act)]
::
++  permission-hook-poke
  |=  act=permission-hook-action
  ^-  card
  :*  %pass  /  %agent  [our.bol %permission-hook]
      %poke  %permission-hook-action  !>(act)
  ==
::
++  perm-group-hook-poke
  |=  act=permission-group-hook-action
  ^-  card
  :*  %pass  /  %agent  [our.bol %permission-group-hook]
      %poke  %permission-group-hook-action  !>(act)
  ==
::
++  envelope-scry
  |=  pax=path
  ^-  (list envelope)
  =.  pax  ;:(weld /=chat-store/(scot %da now.bol)/envelopes pax /noun)
  .^((list envelope) %gx pax)
::
++  configs-scry
  ^-  chat-configs
  .^(chat-configs %gx /=chat-store/(scot %da now.bol)/configs/noun)
::
++  group-scry
  |=  pax=path
  ^-  (unit group)
  .^((unit group) %gx ;:(weld /=group-store/(scot %da now.bol) pax /noun))
::
--
