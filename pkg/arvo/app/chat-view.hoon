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
    *metadata-hook,
    *rw-security,
    hook=chat-hook
/+  *server, default-agent, verb, dbug,
    store=chat-store,
    view=chat-view
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
~%  %chat-view-top  ..is  ~
|%
+$  card  card:agent:gall
::
+$  poke
  $%  [%launch-action [@tas path @t]]
      [%chat-action action:store]
      [%group-action group-action]
      [%chat-hook-action action:hook]
      [%permission-hook-action permission-hook-action]
      [%permission-group-hook-action permission-group-hook-action]
  ==
--
%+  verb  |
%-  agent:dbug
^-  agent:gall
=<
  ~%  %chat-view-agent-core  ..poke-handle-http-request  ~
  |_  bol=bowl:gall
  +*  this       .
      chat-core  +>
      cc         ~(. chat-core bol)
      def        ~(. (default-agent this %|) bol)
  ::
  ++  on-init
    ^-  (quip card _this)
    =/  launcha  [%launch-action !>([%add %chat-view /configs '/~chat/js/tile.js'])]
    :_  this
    :~  [%pass /updates %agent [our.bol %chat-store] %watch /updates]
        [%pass / %arvo %e %connect [~ /'~chat'] %chat-view]
        [%pass /chat-view %agent [our.bol %launch] %poke launcha]
    ==
  ++  on-poke
    ~/  %chat-view-poke
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
      (poke-chat-view-action:cc (action:dejs:view !<(json vase)))
    ::
        %chat-view-action
      :_  this
      (poke-chat-view-action:cc !<(action:view vase))
    ==
  ::
  ++  on-watch
    ~/  %chat-view-watch
    |=  =path
    ^-  (quip card _this)
    ?>  (team:title our.bol src.bol)
    |^
    ?:  ?=([%http-response *] path)
      [~ this]
    ?:  =(/primary path)
      ::  create inbox with 20 messages max per mailbox and send that along
      ::  then quit the subscription
      :_  this
      [%give %fact ~ %json !>((inbox:enjs:store truncated-inbox-scry))]~
    ?:  =(/configs path)
      [[%give %fact ~ %json !>(*json)]~ this]
    (on-watch:def path)
    ::
    ++  message-limit  20
    ::
    ++  truncated-inbox-scry
      ^-  inbox:store
      =/  =inbox:store
        .^(inbox:store %gx /=chat-store/(scot %da now.bol)/all/noun)
      %-  ~(run by inbox)
      |=  =mailbox:store
      ^-  mailbox:store
      [config.mailbox (scag message-limit envelopes.mailbox)]
    --
  ::
  ++  on-agent
    ~/  %chat-view-agent
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
        (diff-chat-update:cc !<(update:store q.cage.sign))
      ==
    ==
  ::
  ++  on-arvo
    ~/  %chat-view-arvo
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
~%  %chat-view-library  ..card  ~
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
    %-  update:enjs:store
    [%messages pax start end envelopes]
  ::
      [%'~chat' *]  (html-response:gen index)
  ==
::
++  poke-json
  |=  jon=json
  ^-  (list card)
  ?>  (team:title our.bol src.bol)
  (poke-chat-view-action (action:dejs:view jon))
::
++  poke-chat-view-action
  |=  act=action:view
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
    :~  (create-chat app-path.act allow-history.act)
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
    ?>  ?=(^ app-path.act)
    ::  always just delete the chat from chat-store
    ::
    :+  (chat-hook-poke [%remove app-path.act])
      (chat-poke [%delete app-path.act])
    ::  if we still have metadata for the chat, remove it, and the associated
    ::  group if it's unmanaged
    ::
    ::    we aren't guaranteed to have metadata: the chat might have been
    ::    deleted by the host, which pushes metadata deletion down to us.
    ::
    =/  group-path=(unit path)
      (maybe-group-from-chat app-path.act)
    ?~  group-path  ~
    =*  group  u.group-path
    %-  zing
    :~  ?.  (is-creator group %chat app-path.act)  ~
        [(metadata-poke [%remove group [%chat app-path.act]])]~
      ::
        ?:  (is-managed group)  ~
        :~  (group-poke [%unbundle group])
            (metadata-hook-poke [%remove group])
            (metadata-store-poke [%remove group [%chat app-path.act]])
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
  ::
      %groupify
    ?>  ?=([%'~' ^] app-path.act)
    ::  retrieve old data
    ::
    =/  data=(unit mailbox:store)
      (scry-for (unit mailbox:store) %chat-store [%mailbox app-path.act])
    ?~  data
      ~&  [%cannot-groupify-nonexistent app-path.act]
      ~
    =/  permission=(unit permission)
      (scry-for (unit permission) %permission-store [%permission app-path.act])
    ?:  |(?=(~ permission) ?=(%black kind.u.permission))
      ~&  [%cannot-groupify-blacklist app-path.act]
      ~
    =/  =metadata
      =-  (fall - *metadata)
      %^  scry-for  (unit metadata)
        %metadata-store
      =/  encoded-path=@ta
        (scot %t (spat app-path.act))
      /metadata/[encoded-path]/chat/[encoded-path]
    ::  figure out new data
    ::
    =/  chat-path=^path      (slag 1 `path`app-path.act)
    ::  group-path: the group to associate with the chat
    ::  members: members of group, if it's new
    ::  new-members: new members of group, if it already exists
    ::
    =/  [group-path=path members=(set ship) new-members=(set ship)]
      ?~  existing.act
        [chat-path who.u.permission ~]
      :+  group-path.u.existing.act
        ~
      ?.  inclusive.u.existing.act  ~
      %-  ~(dif in who.u.permission)
      ~|  [%groupifying-with-nonexistent-group group-path.u.existing.act]
      %-  need
      (group-scry group-path.u.existing.act)
    ::  make changes
    ::
    ;:  weld
      ::  delete the old chat
      ::
      (poke-chat-view-action %delete app-path.act)
    ::
      ::  create the new chat. if needed, creates the new group.
      ::
      %-  poke-chat-view-action
      :*  %create
          title.metadata
          description.metadata
          chat-path
          group-path
          %village
          members
          &
      ==
    ::
      ::  if needed, add members to the existing group
      ::
      ?~  new-members  ~
      [(group-poke [%add new-members group-path])]~
    ::
      ::  import messages into the new chat
      ::
      [(chat-poke %messages chat-path envelopes.u.data)]~
    ==
  ==
  ::
  ++  create-chat
    |=  [=path history=?]
    ^-  (list card)
    :~  (chat-poke [%create path])
        (chat-hook-poke [%add-owned path history])
    ==
  ::
  ++  create-group
    |=  [=path app-path=path sec=rw-security ships=(set ship) title=@t desc=@t]
    ^-  (list card)
    ?^  (group-scry path)
      :~  (create-security path %village)
          (permission-hook-poke [%add-owned path path])
      ==
    ::  do not create a managed group if this is a sig path or a blacklist
    ::
    ?:  =(sec %channel)
      :~  (group-poke [%bundle path])
          (create-security path sec)
          (permission-hook-poke [%add-owned path path])
      ==
    ?:  (is-managed path)
      ~[(contact-view-poke [%create path ships title desc])]
    %+  welp
      :~  (group-poke [%bundle path])
          (group-poke [%add ships path])
          (create-security path sec)
          (permission-hook-poke [%add-owned path path])
      ==
    %-  zing
    %+  turn  ~(tap in ships)
    |=  =ship
    ?:  =(ship our.bol)  ~
    [(send-invite app-path ship)]~
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
    ^-  (unit mailbox:store)
    =.  pax  ;:(weld /=chat-store/(scot %da now.bol)/mailbox pax /noun)
    .^((unit mailbox:store) %gx pax)
  ::
  ++  maybe-group-from-chat
    |=  app-path=path
    ^-  (unit path)
    ?.  .^(? %gu (scot %p our.bol) %metadata-store (scot %da now.bol) ~)
      ?:  ?=([@ ^] app-path)
        ~&  [%assuming-ported-legacy-chat app-path]
        `[%'~' app-path]
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
    =/  groups=(set path)
      %+  fall
        (~(get by resource-indices) [%chat app-path])
      *(set path)
    ?~  groups  ~
    `n.groups
  ::
  ++  group-from-chat
    (cork maybe-group-from-chat need)
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
  |=  upd=update:store
  ^-  (list card)
  =/  updates-json  (update:enjs:store upd)
  =/  configs-json  (configs:enjs:store configs-scry)
  :~  [%give %fact ~[/primary] %json !>(updates-json)]
      [%give %fact ~[/configs] %json !>(configs-json)]
  ==
::
::  +utilities
::
++  chat-poke
  |=  act=action:store
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
  |=  act=action:hook
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
  ^-  (list envelope:store)
  (scry-for (list envelope:store) %chat-store [%envelopes pax])
::
++  configs-scry
  ^-  configs:store
  (scry-for configs:store %chat-store /configs)
::
++  group-scry
  |=  pax=path
  ^-  (unit group)
  (scry-for (unit group) %group-store pax)
::
++  scry-for
  |*  [=mold app=term =path]
  .^  mold
    %gx
    (scot %p our.bol)
    app
    (scot %da now.bol)
    (snoc `^path`path %noun)
  ==
--
