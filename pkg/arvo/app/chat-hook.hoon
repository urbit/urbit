::  chat-hook:
::  mirror chat data from foreign to local based on read permissions
::  allow sending chat messages to foreign paths based on write perms
::
/-  *permission-store, *invite-store, *metadata-store,
    *permission-hook, *group-store, *permission-group-hook,  ::TMP  for upgrade
    hook=chat-hook,
    view=chat-view
/+  default-agent, verb, dbug, store=chat-store
~%  %chat-hook-top  ..is  ~
|%
+$  card  card:agent:gall
::
+$  versioned-state
  $%  state-0
      state-1
  ==
::
+$  state-1
  $:  %1
      loaded-cards=(list card)
      state-base
  ==
+$  state-0  [%0 state-base]
+$  state-base
  $:  =synced:hook
      invite-created=_|
      allow-history=(map path ?)
  ==
::
+$  poke
  $%  [%chat-action action:store]
      [%permission-action permission-action]
      [%invite-action invite-action]
      [%chat-view-action action:view]
  ==
::
+$  fact
  $%  [%chat-update update:store]
  ==
--
=|  state-1
=*  state  -
::
%-  agent:dbug
%+  verb  |
^-  agent:gall
=<
  ~%  %chat-hook-agent-core  ..poke-json  ~
  |_  bol=bowl:gall
  +*  this       .
      chat-core  +>
      cc         ~(. chat-core bol)
      def        ~(. (default-agent this %|) bol)
  ::
  ++  on-init
    ^-  (quip card _this)
    :_  this(invite-created %.y)
    :~  (invite-poke:cc [%create /chat])
        [%pass /invites %agent [our.bol %invite-store] %watch /invitatory/chat]
        [%pass /permissions %agent [our.bol %permission-store] %watch /updates]
    ==
  ++  on-save   !>(state)
  ++  on-load
    |=  old-vase=vase
    ^-  (quip card _this)
    |^
    =/  old  !<(versioned-state old-vase)
    ?:  ?=(%1 -.old)
      :_  this(state old)
      %+  murn  ~(tap by wex.bol)
      |=  [[=wire =ship =term] *]
      ^-  (unit card)
      ?.  &(?=([%mailbox *] wire) =(our.bol ship) =(%chat-store term))
        ~
      `[%pass wire %agent [our.bol %chat-store] %leave ~]
    ::  path structure ugprade logic
    ::
    =/  keys=(set path)  (scry:cc (set path) %chat-store /keys)
    =/  upgraded-state
        %*  .  *state-1
            synced  synced
            invite-created  invite-created
            allow-history  allow-history
            loaded-cards
          %-  zing
          ^-  (list (list card))
          %+  turn  ~(tap in keys)  generate-cards
        ==
    [loaded-cards.upgraded-state this(state upgraded-state)]
    ::
    ++  generate-cards
      |=  old-chat=path
      ^-  (list card)
      =/  host=ship  (slav %p (snag 0 old-chat))
      =/  new-chat  [%'~' old-chat]
      =/  newp=permission  (unify-permissions old-chat)
      =/  old-group=path  [%chat old-chat]
      %-  zing
      :~  :~  (delete-group host (snoc old-group %read))
              (delete-group host (snoc old-group %write))
          ==
        ::
          (create-group new-chat who.newp)
          (hookup-group new-chat kind.newp)
          [(record-group new-chat new-chat)]~
          (recreate-chat host old-chat new-chat)
      ==
    ::
    ++  recreate-chat
      |=  [host=ship chat=path new-chat=path]
      ^-  (list card)
      =/  old-mailbox=mailbox:store
        (need (scry:cc (unit mailbox:store) %chat-store [%mailbox chat]))
      =*  enves  envelopes.old-mailbox
      :~  (chat-poke:cc [%delete new-chat])
          (chat-poke:cc [%delete chat])
          (chat-poke:cc [%create new-chat])
          (chat-poke:cc [%messages new-chat enves])
          (chat-poke:cc [%read new-chat])
          %^  make-poke  %chat-hook  %chat-hook-action
          !>  ^-  action:hook
          ?:  =(our.bol host)  [%add-owned new-chat %.y]
          [%add-synced host new-chat %.y]
      ==
    ::
    ++  unify-permissions
      |=  chat=path
      ^-  permission
      =/  read=(unit permission)   (get-permission chat %read)
      =/  write=(unit permission)  (get-permission chat %write)
      ?.  &(?=(^ read) ?=(^ write))
        ~&  [%missing-permission chat read=?=(~ read) write=?=(~ write)]
        [%white [(slav %p (snag 0 chat)) ~ ~]]
      ?+  [kind.u.read kind.u.write]  !!
        ::  village: exclusive to writers
        ::
        [%white %white]  [%white who.u.write]
      ::
        ::  channel: merge blacklists
        ::
        [%black %black]  [%black (~(uni in who.u.read) who.u.write)]
      ::
        ::  journal: exclusive to writers
        ::
        [%black %white]  [%white who.u.write]
      ::
        ::  mailbox: exclusive to readers
        ::
        [%white %black]  [%white who.u.read]
      ==
    ::
    ++  get-permission
      |=  [chat=path what=?(%read %write)]
      %^  scry:cc  (unit permission)
        %permission-store
      [%permission %chat (snoc chat what)]
    ::
    ++  make-poke
      |=  [app=term =mark =vase]
      ^-  card
      [%pass /on-load/[app]/[mark] %agent [our.bol app] %poke mark vase]
    ::
    ++  delete-group
      |=  [host=ship group=path]
      ^-  card
      ::  if we host the group, delete it directly
      ::
      ?:  =(our.bol host)
        %^  make-poke  %group-store
          %group-action
        !>  ^-  group-action
        [%unbundle group]
      ::  else, just delete the sync in the hook
      ::
      %^  make-poke  %permission-hook
        %permission-hook-action
      !>  ^-  permission-hook-action
      [%remove group]
    ::
    ++  create-group
      |=  [group=path who=(set ship)]
      ^-  (list card)
      :~  %^  make-poke  %group-store
            %group-action
          !>  ^-  group-action
          [%bundle group]
        ::
          %^  make-poke  %group-store
            %group-action
          !>  ^-  group-action
          [%add who group]
      ==
    ::
    ++  hookup-group
      |=  [group=path =kind]
      ^-  (list card)
      :*  %^  make-poke  %permission-group-hook
            %permission-group-hook-action
          !>  ^-  permission-group-hook-action
          [%associate group [group^kind ~ ~]]
        ::
          =/  =ship  (slav %p (snag 1 group))
          ?.  =(our.bol ship)  ~
          :_  ~
          %^  make-poke  %permission-hook
            %permission-hook-action
          !>  ^-  permission-hook-action
          [%add-owned group group]
      ==
    ::
    ++  record-group
      |=  [group=path chat=path]
      ^-  card
      =/  =metadata
        ~|  [%weird-chat-path chat]
        %*  .  *metadata
          title         (snag 2 chat)
          date-created  now.bol
          creator       (slav %p (snag 1 chat))
        ==
      %^  make-poke  %metadata-store
        %metadata-action
      !>  ^-  metadata-action
      [%add group [%chat chat] metadata]
    --
  ::
  ++  on-poke
    ~/  %chat-hook-poke
    |=  [=mark =vase]
    ^-  (quip card _this)
    =^  cards  state
      ?+  mark  (on-poke:def mark vase)
          %json              (poke-json:cc !<(json vase))
          %chat-action       (poke-chat-action:cc !<(action:store vase))
          %noun
        ?:  =(%store-load q.vase)
          [loaded-cards.state state(loaded-cards ~)]
        [~ state]
      ::
          %chat-hook-action
        (poke-chat-hook-action:cc !<(action:hook vase))
      ==
    [cards this]
  ::
  ++  on-watch
    ~/  %chat-hook-watch
    |=  =path
    ^-  (quip card _this)
    ?+  path          (on-watch:def path)
        [%backlog *]  [(watch-backlog:cc t.path) this]
        [%mailbox *]  [(watch-mailbox:cc t.path) this]
        [%synced *]   [(watch-synced:cc t.path) this]
    ==
  ::
  ++  on-agent
    ~/  %chat-hook-agent
    |=  [=wire =sign:agent:gall]
    ^-  (quip card _this)
    ?+  -.sign  (on-agent:def wire sign)
        %watch-ack
      =^  cards  state
        (watch-ack:cc wire p.sign)
      [cards this]
    ::
        %kick
      =^  cards  state
        (kick:cc wire)
      [cards this]
    ::
        %fact
      ?+  p.cage.sign  (on-agent:def wire sign)
          %chat-update
        =^  cards  state
          (fact-chat-update:cc wire !<(update:store q.cage.sign))
        [cards this]
      ::
          %invite-update
        =^  cards  state
          (fact-invite-update:cc wire !<(invite-update q.cage.sign))
        [cards this]
      ::
          %permission-update
        =^  cards  state
          (fact-permission-update:cc wire !<(permission-update q.cage.sign))
        [cards this]
      ==
    ==
  ::
  ++  on-leave  on-leave:def
  ++  on-peek   on-peek:def
  ++  on-arvo   on-arvo:def
  ++  on-fail   on-fail:def
  --
::
::
~%  %chat-hook-library  ..card  ~
|_  bol=bowl:gall
::
++  poke-json
  |=  jon=json
  ^-  (quip card _state)
  (poke-chat-action (action:dejs:store jon))
::
++  poke-chat-action
  |=  act=action:store
  ^-  (quip card _state)
  ?>  ?=(%message -.act)
  ::  local
  :_  state
  ?:  (team:title our.bol src.bol)
    ?.  (~(has by synced) path.act)
      ~
    =*  letter  letter.envelope.act
    =?  letter  &(?=(%code -.letter) ?=(~ output.letter))
      =/  =hoon  (ream expression.letter)
      letter(output (eval:store bol hoon))
    =/  ship  (~(got by synced) path.act)
    =/  appl  ?:(=(ship our.bol) %chat-store %chat-hook)
    [%pass / %agent [ship appl] %poke %chat-action !>(act)]~
  ::  foreign
  =/  ship  (~(get by synced) path.act)
  ?~  ship  ~
  ?.  =(u.ship our.bol)  ~
  ::  check if write is permitted
  ?.  (is-permitted src.bol path.act)  ~
  =:  author.envelope.act  src.bol
      when.envelope.act  now.bol
  ==
  [%pass / %agent [our.bol %chat-store] %poke %chat-action !>(act)]~
::
++  poke-chat-hook-action
  |=  act=action:hook
  ^-  (quip card _state)
  ?-  -.act
      %add-owned
    ?>  (team:title our.bol src.bol)
    =/  chat-path  [%mailbox path.act]
    =/  chat-wire  [%store path.act]
    ?:  (~(has by synced) path.act)  [~ state]
    =:  synced  (~(put by synced) path.act our.bol)
        allow-history  (~(put by allow-history) path.act allow-history.act)
    ==
    :_  state
    :~  [%pass chat-wire %agent [our.bol %chat-store] %watch chat-path]
        [%give %fact [/synced]~ %chat-hook-update !>([%initial synced])]
    ==
  ::
      %add-synced
    ?>  (team:title our.bol src.bol)
    ?:  (~(has by synced) path.act)  [~ state]
    =.  synced  (~(put by synced) path.act ship.act)
    ?.  ask-history.act
      =/  chat-path  [%mailbox path.act]
      :_  state
      [%pass chat-path %agent [ship.act %chat-hook] %watch chat-path]~
    =/  mailbox=(unit mailbox:store)  (chat-scry path.act)
    =/  chat-history=path
      :-  %backlog
      %+  weld  path.act
      ?~(mailbox /0 /(scot %ud (lent envelopes.u.mailbox)))
    :_  state
    :~  [%pass chat-history %agent [ship.act %chat-hook] %watch chat-history]
        [%give %fact [/synced]~ %chat-hook-update !>([%initial synced])]
    ==
  ::
      %remove
    =/  ship=(unit ship)
      =/  ship  (~(get by synced) path.act)
      ?^  ship  ship
      =?  path.act  ?=([%'~' *] path.act)  t.path.act
      ?~  path.act  ~
      (slaw %p i.path.act)
    ?~  ship
      ~&  [dap.bol %unknown-host-cannot-leave path.act]
      [~ state]
    ?:  &(!=(u.ship src.bol) ?!((team:title our.bol src.bol)))
      [~ state]
    =.  synced  (~(del by synced) path.act)
    :_  state
    :*  [%give %kick ~[[%mailbox path.act]] ~]
        [%give %fact [/synced]~ %chat-hook-update !>([%initial synced])]
        (pull-wire u.ship [%mailbox path.act])
        (pull-backlog-subscriptions u.ship path.act)
    ==
  ==
::
++  watch-synced
  |=  pax=path
  ^-  (list card)
  ?>  (team:title our.bol src.bol)
  [%give %fact ~ %chat-hook-update !>([%initial synced])]~
::
++  watch-mailbox
  |=  pax=path
  ^-  (list card)
  ?>  ?=(^ pax)
  ?>  (~(has by synced) pax)
  ::  check if read is permitted
  ?>  (is-permitted src.bol pax)
  =/  box  (chat-scry pax)
  ?~  box  !!
  [%give %fact ~ %chat-update !>([%create pax])]~
::
++  watch-backlog
  |=  pax=path
  ^-  (list card)
  ?>  ?=(^ pax)
  =/  last  (dec (lent pax))
  =/  backlog-latest=(unit @ud)  (rush (snag last `(list @ta)`pax) dem:ag)
  =/  pas  `path`(oust [last 1] `(list @ta)`pax)
  ?>  ?=([* ^] pas)
  ?>  (~(has by synced) pas)
  ?>  (is-permitted src.bol pas)
  =/  envs  envelopes:(need (chat-scry pas))
  =/  length  (lent envs)
  =/  latest
    ?~  backlog-latest  length
    ?:  (gth u.backlog-latest length)  length
    (sub length u.backlog-latest)
  =.  envs  (scag latest envs)
  =/  =vase  !>([%messages pas 0 latest envs])
  %-  zing
  :~  [%give %fact ~ %chat-update !>([%create pas])]~
      ?.  ?&(?=(^ backlog-latest) (~(has by allow-history) pas))  ~
      [%give %fact ~ %chat-update vase]~
      [%give %kick [%backlog pax]~ `src.bol]~
  ==
::
++  fact-invite-update
  |=  [wir=wire fact=invite-update]
  ^-  (quip card _state)
  :_  state
  ?+  -.fact  ~
      %accepted
    =/  ask-history  ?~((chat-scry path.invite.fact) %.y %.n)
    =*  shp       ship.invite.fact
    =*  app-path  path.invite.fact
    ~[(chat-view-poke [%join shp app-path ask-history])]
==
::
++  fact-permission-update
  |=  [wir=wire fact=permission-update]
  ^-  (quip card _state)
  |^
  :_  state
  ?+  -.fact   ~
      %add     (handle-permissions [%add path.fact who.fact])
      %remove  (handle-permissions [%remove path.fact who.fact])
  ==
  ::
  ++  handle-permissions
    |=  [kind=?(%add %remove) pax=path who=(set ship)]
    ^-  (list card)
    %-  zing
    %+  turn
      (chats-of-group pax)
    |=  chat=path
    ^-  (list card)
    =/  owner  (~(get by synced) chat)
    ?~  owner  ~
    ?.  =(u.owner our.bol)  ~
    %-  zing
    %+  turn  ~(tap in who)
    |=  =ship
    ?:  (is-permitted ship chat)
      ?:  ?|(=(kind %remove) =(ship our.bol) (is-managed pax))  ~
      ::  if ship has just been added to the permitted group,
      ::  send them an invite
      ~[(send-invite chat ship)]
    ::  if ship is not permitted, kick their subscription
    [%give %kick [%mailbox chat]~ `ship]~
  ::
  ++  send-invite
    |=  [=path =ship]
    ^-  card
    =/  =invite  [our.bol %chat-hook path ship '']
    =/  act=invite-action  [%invite /chat (shaf %msg-uid eny.bol) invite]
    [%pass / %agent [our.bol %invite-hook] %poke %invite-action !>(act)]
  ::
  ++  is-managed
    |=  =path
    ^-  ?
    ?>  ?=(^ path)
    !=(i.path '~')
  --
::
++  fact-chat-update
  |=  [wir=wire =update:store]
  ^-  (quip card _state)
  ?:  (team:title our.bol src.bol)
    (handle-local update)
  (handle-foreign update)
::
++  handle-local
  |=  =update:store
  ^-  (quip card _state)
  ?+  -.update     [~ state]
      %delete
    ?.  (~(has by synced) path.update)  [~ state]
    =.  synced  (~(del by synced) path.update)
    :_  state
    :~  [%pass [%mailbox path.update] %agent [our.bol %chat-store] %leave ~]
        [%give %fact [/synced]~ %chat-hook-update !>([%initial synced])]
    ==
  ::
      %message
    :_  state
    [%give %fact [%mailbox path.update]~ %chat-update !>(update)]~
  ::
      %messages
    :_  state
    [%give %fact [%mailbox path.update]~ %chat-update !>(update)]~
  ==
::
++  handle-foreign
  |=  =update:store
  ^-  (quip card _state)
  ?+  -.update   [~ state]
      %create
    :_  state
    ?>  ?=([* ^] path.update)
    =/  shp  (~(get by synced) path.update)
    ?~  shp  ~
    ?.  =(src.bol u.shp)  ~
    [(chat-poke [%create path.update])]~
  ::
      %delete
    ?>  ?=([* ^] path.update)
    =/  shp  (~(get by synced) path.update)
    ?~  shp  [~ state]
    ?.  =(u.shp src.bol)  [~ state]
    =.  synced  (~(del by synced) path.update)
    :_  state
    :-  (chat-poke [%delete path.update])
    :~  [%pass [%mailbox path.update] %agent [src.bol %chat-hook] %leave ~]
        [%give %fact [/synced]~ %chat-hook-update !>([%initial synced])]
    ==
  ::
      %message
    :_  state
    ?>  ?=([* ^] path.update)
    =/  shp  (~(get by synced) path.update)
    ?~  shp  ~
    ?.  =(src.bol u.shp)  ~
    [(chat-poke [%message path.update envelope.update])]~
  ::
      %messages
    :_  state
    ?>  ?=([* ^] path.update)
    =/  shp  (~(get by synced) path.update)
    ?~  shp  ~
    ?.  =(src.bol u.shp)  ~
    [(chat-poke [%messages path.update envelopes.update])]~
  ==
::
++  kick
  |=  wir=wire
  ^-  (quip card _state)
  ?:  =(wir /permissions)
    :_  state
    [%pass /permissions %agent [our.bol %permission-store] %watch /updates]~
  ::
  ?+  wir  !!
      [%store @ *]
    ~&  store-kick+wir
    ?.  (~(has by synced) t.wir)  [~ state]
    ~&  %chat-store-resubscribe
    =/  mailbox=(unit mailbox:store)
      (chat-scry t.wir)
    :_  state
    [%pass wir %agent [our.bol %chat-store] %watch [%mailbox t.wir]]~
  ::
      [%mailbox @ *]
    ~&  mailbox-kick+wir
    ?.  (~(has by synced) t.wir)  [~ state]
    ~&  %chat-hook-resubscribe
    =/  =ship  (~(got by synced) t.wir)
    =/  mailbox=(unit mailbox:store)  (chat-scry t.wir)
    =/  chat-history
      %+  welp  backlog+t.wir
      ?~(mailbox /0 /(scot %ud (lent envelopes.u.mailbox)))
    :_  state
    [%pass chat-history %agent [ship %chat-hook] %watch chat-history]~
  ::
      [%backlog @ @ *]
    =/  chat=path  (oust [(dec (lent t.wir)) 1] `(list @ta)`t.wir)
    ?.  (~(has by synced) chat)  [~ state]
    =/  =ship
      ?:  =('~' i.t.wir)
        (slav %p i.t.t.wir)
      (slav %p i.t.wir)
    =/  =path  ?~((chat-scry chat) wir [%mailbox chat])
    :_  state
    [%pass path %agent [ship %chat-hook] %watch path]~
  ==
::
++  watch-ack
  |=  [wir=wire saw=(unit tang)]
  ^-  (quip card _state)
  ?~  saw  [~ state]
  ?+  wir  [~ state]
      [%store @ *]
    (poke-chat-hook-action %remove t.wir)
  ::
      [%backlog @ @ @ *]
    =/  chat=path  (oust [(dec (lent t.wir)) 1] `(list @ta)`t.wir)
    :_  state
    %.  ~[(chat-view-poke %delete chat)]
    %-  slog
    :*  leaf+"chat-hook failed subscribe on {(spud chat)}"
        leaf+"stack trace:"
        u.saw
    ==
  ==
::
++  chat-poke
  |=  act=action:store
  ^-  card
  [%pass / %agent [our.bol %chat-store] %poke %chat-action !>(act)]
::
++  chat-view-poke
  |=  act=action:view
  ^-  card
  [%pass / %agent [our.bol %chat-view] %poke %chat-view-action !>(act)]
::
++  invite-poke
  |=  act=invite-action
  ^-  card
  [%pass / %agent [our.bol %invite-store] %poke %invite-action !>(act)]
::
++  sec-to-perm
  |=  [pax=path =kind]
  ^-  permission-action
  [%create pax kind *(set ship)]
::
++  chat-scry
  |=  pax=path
  ^-  (unit mailbox:store)
  %^  scry  (unit mailbox:store)
    %chat-store
  [%mailbox pax]
::
++  invite-scry
  |=  uid=serial
  ^-  (unit invite)
  %^  scry  (unit invite)
    %invite-store
  /invite/chat/(scot %uv uid)
::
++  chats-of-group
  |=  =group-path
  ^-  (list path)
  ::  if metadata-store isn't running yet, we're still in the upgrade ota phase.
  ::  we can't get chats from the metadata-store, but can make assumptions
  ::  about group path shape, and the chat that would match it.
  ::TODO  remove me at some point.
  ::
  ?.  .^(? %gu (scot %p our.bol) %metadata-store (scot %da now.bol) ~)  ~
  %+  murn
    ^-  (list resource)
    =;  resources
      %~  tap  in
      %+  ~(gut by resources)
        group-path
      *(set resource)
    .^  (jug path resource)
      %gy
      (scot %p our.bol)
      %metadata-store
      (scot %da now.bol)
      /group-indices
    ==
  |=  resource
  ^-  (unit path)
  ?.  =(%chat app-name)  ~
  `app-path
::
++  groups-of-chat
  |=  chat=path
  ^-  (list group-path)
  ::  if metadata-store isn't running yet, we're still in the upgrade ota phase.
  ::  we can't get groups from the metadata-store, but can make assumptions
  ::  about chat path shape, and the chat that would match it.
  ::TODO  remove me at some point.
  ::
  ?.  .^(? %gu (scot %p our.bol) %metadata-store (scot %da now.bol) ~)  ~
  =;  resources
    %~  tap  in
    %+  ~(gut by resources)
      [%chat chat]
    *(set group-path)
  .^  (jug resource group-path)
    %gy
    (scot %p our.bol)
    %metadata-store
    (scot %da now.bol)
    /resource-indices
  ==
::
::NOTE  this assumes permission paths match group paths
++  is-permitted
  |=  [who=ship chat=path]
  ^-  ?
  %+  lien  (groups-of-chat chat)
  |=  =group-path
  %^  scry  ?
    %permission-store
  [%permitted (scot %p who) group-path]
::
++  scry
  |*  [=mold app=term =path]
  .^  mold
    %gx
    (scot %p our.bol)
    app
    (scot %da now.bol)
    (snoc `^path`path %noun)
  ==
::
++  pull-backlog-subscriptions
  |=  [target=ship chat=path]
  ^-  (list card)
  %+  murn  ~(tap by wex.bol)
  |=  [[=wire =ship =term] [acked=? =path]]
  ^-  (unit card)
  ?.  ?&  =(ship target)
          ?=([%backlog *] wire)
          =(`1 (find chat wire))
      ==
    ~
  `(pull-wire target wire)
::
++  pull-wire
  |=  [=ship =wire]
  ^-  card
  ?:  =(ship our.bol)
    [%pass wire %agent [our.bol %chat-store] %leave ~]
  [%pass wire %agent [ship %chat-hook] %leave ~]
--
