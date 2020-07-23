::  contact-hook:
::
/-  group-hook,
    *contact-hook,
    *contact-view,
    *invite-store,
    *metadata-hook,
    *metadata-store,
    *group
/+  *contact-json, default-agent, dbug, group-store, verb, resource, grpl=group
~%  %contact-hook-top  ..is  ~
|%
+$  card  card:agent:gall
::
+$  versioned-state
  $%  state-zero
      state-one
      state-two
      state-three
  ==
::
+$  state-zero  [%0 state-base]
+$  state-one   [%1 state-base]
+$  state-two   [%2 state-base]
+$  state-three  [%3 state-base]
+$  state-base
  $:  =synced
      invite-created=_|
  ==
--
=|  state-three
=*  state  -
%-  agent:dbug
%+  verb  |
^-  agent:gall
=<
  |_  bol=bowl:gall
  +*  this       .
      contact-core  +>
      cc         ~(. contact-core bol)
      def        ~(. (default-agent this %|) bol)
  ::
  ++  on-init
    ^-  (quip card _this)
    :_  this(invite-created %.y)
    :~  (invite-poke:cc [%create /contacts])
        [%pass /inv %agent [our.bol %invite-store] %watch /invitatory/contacts]
        [%pass /group %agent [our.bol %group-store] %watch /groups]
    ==
  ++  on-save   !>(state)
  ++  on-load
    |=  old-vase=vase
    ^-  (quip card _this)
    =/  old  !<(versioned-state old-vase)
    =|  cards=(list card)
    |^
    |-  ^-  (quip card _this) 
    ?:  ?=(%3 -.old)
      [cards this(state old)]
    ?:  ?=(%2 -.old)
      %_  $
        old  [%3 +.old]
      ::
          cards
        %+  welp
          cards
        %-  zing
        %+  turn
          ~(tap by synced.old)
        |=  [=path =ship]
        ^-  (list card)
        ?.  =(ship our.bol)
          ~
        ?>  ?=([%ship *] path)
        :~  (pass-store contacts+t.path %leave ~)
            (pass-store contacts+path %watch contacts+path)
        ==
      ==
    ?:  ?=(%1 -.old)
      %_    $
        -.old  %2
        ::
          synced.old  
        %-  malt
        %+  turn
          ~(tap by synced.old)
        |=  [=path =ship]
        [ship+path ship]
        ::
          cards
        ^-  (list card)
        ;:  welp
          :~  [%pass /group %agent [our.bol %group-store] %leave ~]
              [%pass /group %agent [our.bol %group-store] %watch /groups]
          ==
          kick-old-subs
          cards
        ==
      ==
    %_    $
      -.old  %1
    ::
        cards
      :_  cards
      [%pass /group %agent [our.bol %group-store] %watch /updates]
    ==
    ++  kick-old-subs
      =/  paths
        %+  turn
          ~(val by sup.bol)
        |=([=ship =path] path)
      ?~  paths  ~
      [%give %kick paths ~]~
    ::
    ++  pass-store
      |=  [=wire =task:agent:gall]
      ^-  card
      [%pass wire %agent [our.bol %contact-store] task]
    --
  ::
  ++  on-poke
    |=  [=mark =vase]
    ^-  (quip card _this)
    =^  cards  state
      ?+  mark  (on-poke:def mark vase)
          %json
        (poke-json:cc !<(json vase))
      ::
          %contact-action  
        (poke-contact-action:cc !<(contact-action vase))
      ::
          %contact-hook-action
        (poke-hook-action:cc !<(contact-hook-action vase))
      ==
    [cards this]
  ::
  ++  on-watch
    |=  =path
    ^-  (quip card _this)
    ?+  path          (on-watch:def path)
        [%contacts *]  [(watch-contacts:cc t.path) this]
        [%synced *]    [(watch-synced:cc t.path) this]
    ==
  ::
  ++  on-agent
    |=  [=wire =sign:agent:gall]
    ^-  (quip card _this)
    ?+  -.sign  (on-agent:def wire sign)
        %kick       [(kick:cc wire) this]
        %watch-ack
      =^  cards  state
        (watch-ack:cc wire p.sign) 
      [cards this]
    ::
        %fact
      ?+  p.cage.sign  (on-agent:def wire sign)
          %contact-update
        =^  cards  state
          (fact-contact-update:cc wire !<(contact-update q.cage.sign))
        [cards this]
      ::
          %group-update
        =^  cards  state
          (fact-group-update:cc wire !<(update:group-store q.cage.sign))
        [cards this]
      ::
          %invite-update
        =^  cards  state
          (fact-invite-update:cc wire !<(invite-update q.cage.sign))
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
|_  bol=bowl:gall
++  grp  ~(. grpl bol)
::
++  poke-json
  |=  jon=json
  ^-  (quip card _state)
  (poke-contact-action (json-to-action jon))
::
++  poke-contact-action
  |=  act=contact-action
  ^-  (quip card _state)
  :_  state
  ?+  -.act  !!
    %edit    (handle-contact-action path.act ship.act act)
    %add     (handle-contact-action path.act ship.act act)
    %remove  (handle-contact-action path.act ship.act act)
  ==
::
++  handle-contact-action
  |=  [=path =ship act=contact-action]
  ^-  (list card)
  ::  local
  ?:  (team:title our.bol src.bol)
    ?.  |(=(path /~/default) (~(has by synced) path))  ~
    =/  shp  ?:(=(path /~/default) our.bol (~(got by synced) path))
    =/  appl  ?:(=(shp our.bol) %contact-store %contact-hook)
    [%pass / %agent [shp appl] %poke %contact-action !>(act)]~
  ::  foreign
  =/  shp  (~(got by synced) path)
  ?.  |(=(shp our.bol) =(src.bol ship))  ~
  ::  scry group to check if ship is a member
  =/  =group  (need (group-scry path))
  ?.  (~(has in members.group) shp)  ~
  [%pass / %agent [our.bol %contact-store] %poke %contact-action !>(act)]~
::
++  poke-hook-action
  |=  act=contact-hook-action
  ^-  (quip card _state)
  ?-  -.act
      %add-owned
    ?>  (team:title our.bol src.bol)
    =/  contact-path  [%contacts path.act]
    ?:  (~(has by synced) path.act)
      [~ state]
    =.  synced  (~(put by synced) path.act our.bol)
    :_  state
    :~  [%pass contact-path %agent [our.bol %contact-store] %watch contact-path]
        [%give %fact [/synced]~ %contact-hook-update !>([%initial synced])]
    ==
  ::
      %add-synced
    ?>  (team:title our.bol src.bol)
    ?:  (~(has by synced) path.act)  [~ state]
    =.  synced  (~(put by synced) path.act ship.act)
    =/  contact-path  [%contacts path.act]
    :_  state
    :~  [%pass contact-path %agent [ship.act %contact-hook] %watch contact-path]
        [%give %fact [/synced]~ %contact-hook-update !>([%initial synced])]
    ==
  ::
      %remove
    =/  ship  (~(get by synced) path.act)
    ?~  ship  [~ state]
    ?:  &(=(u.ship our.bol) (team:title our.bol src.bol))
      ::  delete one of our.bol own paths
      :_  state(synced (~(del by synced) path.act))
      %-  zing
      :~  (pull-wire [%contacts path.act])
          [%give %kick ~[[%contacts path.act]] ~]~
          [%give %fact [/synced]~ %contact-hook-update !>([%initial synced])]~
      ==
    ?.  |(=(u.ship src.bol) (team:title our.bol src.bol))
      ::  if neither ship = source or source = us, do nothing
      [~ state]
    ::  delete a foreign ship's path
    =/  cards
      (handle-contact-action path.act our.bol [%remove path.act our.bol])
    :_  state(synced (~(del by synced) path.act))
    %-  zing
    :~  (pull-wire [%contacts path.act])
        [%give %fact [/synced]~ %contact-hook-update !>([%initial synced])]~
        cards
    ==
  ==
::
++  watch-contacts
  |=  pax=path
  ^-  (list card)
  ?>  ?=(^ pax)
  ?>  (~(has by synced) pax)
  ::  scry groups to check if ship is a member
  =/  =group  (need (group-scry pax))
  ?>  (~(has in members.group) src.bol)
  =/  contacts  (need (contacts-scry pax))
  [%give %fact ~ %contact-update !>([%contacts pax contacts])]~
::
++  watch-synced
  |=  pax=path
  ^-  (list card)
  ?>  (team:title our.bol src.bol)
  [%give %fact ~ %contact-hook-update !>([%initial synced])]~
::
++  watch-ack
  |=  [wir=wire saw=(unit tang)]
  ^-  (quip card _state)
  ?~  saw
    [~ state]
  ?>  ?=(^ wir)
  [~ state(synced (~(del by synced) t.wir))]
::
++  migrate
  |=  wir=wire
  ^-  wire
  ?>  ?=([%contacts @ @ *] wir)
  [%contacts %ship t.wir]
::
++  kick
  |=  wir=wire
  ^-  (list card)
  ?+  wir  !!
      [%inv ~]
    [%pass /inv %agent [our.bol %invite-store] %watch /invitatory/contacts]~
  ::
      [%group ~]
    [%pass /group %agent [our.bol %group-store] %watch /groups]~
  ::
      [%contacts @ *]
    =/  wir  
      ?:  =(%ship i.t.wir) 
        wir
      (migrate wir)
    ?>  ?=([%contacts @ @ *] wir)
    ?.  (~(has by synced) t.wir)  ~
    =/  =ship  (~(got by synced) t.wir)
    ?:  =(ship our.bol)
      [%pass wir %agent [our.bol %contact-store] %watch wir]~
    [%pass wir %agent [ship %contact-hook] %watch wir]~
  ==
::
++  fact-contact-update
  |=  [wir=wire fact=contact-update]
  ^-  (quip card _state)
  |^
  ?:  (team:title our.bol src.bol)
    (local fact)
  :_  state
  (foreign fact)
  ::
  ++  give-fact
    |=  [=path update=contact-update]
    ^-  (list card)
    [%give %fact ~[[%contacts path]] %contact-update !>(update)]~
  ::
  ++  local
    |=  fact=contact-update
    ^-  (quip card _state)
    ?+  -.fact  [~ state]
        %add
      :_  state
      (give-fact path.fact [%add path.fact ship.fact contact.fact])
    ::
        %edit
      :_  state
      (give-fact path.fact [%edit path.fact ship.fact edit-field.fact])
    ::
        %delete
      =.  synced  (~(del by synced) path.fact)
      `state
    ==
  ::
  ++  foreign
    |=  fact=contact-update
    ^-  (list card)
    ?+  -.fact  ~
        %contacts
      =/  owner  (~(got by synced) path.fact)
      ?>  =(owner src.bol)
      =/  have-contacts=(unit contacts)
        (contacts-scry path.fact)
      ?~  have-contacts
        ::  if we don't have any contacts yet,
        ::  create the entry, and %add every contact
        ::
        :-  (contact-poke [%create path.fact])
        %+  turn  ~(tap by contacts.fact)
        |=  [=ship =contact]
        (contact-poke [%add path.fact ship contact])
      ::  if we already have some, decide between %add, %remove and recreate
      ::  on a per-contact basis
      ::
      %-  zing
      %+  turn
        %~  tap  in
        %-  ~(uni in ~(key by contacts.fact))
        ~(key by u.have-contacts)
      |=  =ship
      ^-  (list card)
      =/  have=(unit contact)  (~(get by u.have-contacts) ship)
      =/  want=(unit contact)  (~(get by contacts.fact) ship)
      ?~  have
        [(contact-poke %add path.fact ship (need want))]~
      ?~  want
        [(contact-poke %remove path.fact ship)]~
      ?:  =(u.want u.have)  ~
      ::TODO  probably want an %all edit-field that resolves to more granular
      ::      updates within the contact-store?
      :~  (contact-poke %remove path.fact ship)
          (contact-poke %add path.fact ship u.want)
      ==
    ::
        %add
      =/  owner  (~(get by synced) path.fact)
      ?~  owner  ~
      ?>  |(=(u.owner src.bol) =(src.bol ship.fact))
      ~[(contact-poke [%add path.fact ship.fact contact.fact])]
    ::
        %remove
      =/  owner  (~(get by synced) path.fact)
      ?~  owner  ~
      ?>  |(=(u.owner src.bol) =(src.bol ship.fact))
      ~[(contact-poke [%remove path.fact ship.fact])]
    ::
        %edit
      =/  owner  (~(got by synced) path.fact)
      ?>  |(=(owner src.bol) =(src.bol ship.fact))
      ~[(contact-poke [%edit path.fact ship.fact edit-field.fact])]
    ==
  --
::
++  fact-group-update
  |=  [wir=wire fact=update:group-store]
  ^-  (quip card _state)
  ?:  ?=(%initial -.fact)
    [~ state]
  =/  group=(unit group)
    (scry-group:grp resource.fact)
  |^
  ?+  -.fact     [~ state]
      %initial-group   (initial-group +.fact)
      %remove-members    (remove +.fact)
      %remove-group  (unbundle +.fact)
  ==
  ::
  ++  initial-group
    |=  [rid=resource =^group]
    ^-  (quip card _state)
    ?:  hidden.group  [~ state]
    =/  =path
      (en-path:resource rid)
    ?:  (~(has by synced) path)
      [~ state]
    (poke-hook-action %add-synced entity.rid path)
  ::
  ++  unbundle
    |=  [rid=resource ~]
    ^-  (quip card _state)
    =/  =path
      (en-path:resource rid)
    ?.  (~(has by synced) path)
      ?~  (contacts-scry path)
        [~ state]
      :_  state
      [(contact-poke [%delete path])]~
    :_  state(synced (~(del by synced) path))
    :~  [%pass [%contacts path] %agent [our.bol %contact-store] %leave ~]
        [(contact-poke [%delete path])]
    ==
  ::
  ++  remove
    |=  [rid=resource ships=(set ship)]
    ^-  (quip card _state)
    ::  if pax is synced, remove member from contacts and kick their sub
    ?~  group
      [~ state]
    ?:  hidden.u.group  [~ state]
    =/  =path
      (en-path:resource rid)
    =/  owner=(unit ship)  (~(get by synced) path)
    ?~  owner
      :_  state
      %+  turn  ~(tap in ships)
      |=  =ship
      (contact-poke [%remove path ship])
    :_  state
    %-  zing
    %+  turn  ~(tap in ships)
    |=  =ship
    :~  [%give %kick ~[[%contacts path]] `ship]
        ?:  =(ship our.bol)
          (contact-poke [%delete path])
        (contact-poke [%remove path ship])
    ==
  ::
  ++  send-invite-poke
    |=  [=path =ship]
    ^-  card
    =/  =invite
      :*  our.bol  %contact-hook
          path  ship  ''
      ==
    =/  act=invite-action  [%invite /contacts (shaf %msg-uid eny.bol) invite]
    [%pass / %agent [our.bol %invite-hook] %poke %invite-action !>(act)]
  --
::
++  fact-invite-update
  |=  [wir=wire fact=invite-update]
  ^-  (quip card _state)
  ?+  -.fact  [~ state]
      %accepted
    =/  rid=resource
      (de-path:resource path.invite.fact)
    :_  state
    ~[(contact-view-poke %join rid)]
  ==
::
++  group-hook-poke
  |=  =action:group-hook
  ^-  card
  [%pass / %agent [our.bol %group-hook] %poke %group-hook-action !>(action)]
::
++  invite-poke
  |=  act=invite-action
  ^-  card
  [%pass / %agent [our.bol %invite-store] %poke %invite-action !>(act)]
::
++  contact-poke
  |=  act=contact-action
  ^-  card
  [%pass / %agent [our.bol %contact-store] %poke %contact-action !>(act)]
::
++  contact-view-poke
  |=  act=contact-view-action
  ^-  card
  [%pass / %agent [our.bol %contact-view] %poke %contact-view-action !>(act)]
::
++  group-poke
  |=  act=action:group-store
  ^-  card
  [%pass / %agent [our.bol %group-store] %poke %group-action !>(act)]
::
++  metadata-poke
  |=  act=metadata-action
  ^-  card
  [%pass / %agent [our.bol %metadata-store] %poke %metadata-action !>(act)]
::
++  metadata-hook-poke
  |=  act=metadata-hook-action
  ^-  card
  [%pass / %agent [our.bol %metadata-hook] %poke %metadata-hook-action !>(act)]
::
++  contacts-scry
  |=  pax=path
  ^-  (unit contacts)
  =.  pax
    ;:  weld
      /(scot %p our.bol)/contact-store/(scot %da now.bol)/contacts
      pax
      /noun
    ==
  .^((unit contacts) %gx pax)
::
++  invite-scry
  |=  uid=serial
  ^-  (unit invite)
  =/  pax
    ;:  weld
      /(scot %p our.bol)/invite-store/(scot %da now.bol)
      /invite/contacts/(scot %uv uid)/noun
    ==
  .^((unit invite) %gx pax)
::
++  group-scry
  |=  pax=path
  .^  (unit group)
    %gx
    ;:(weld /(scot %p our.bol)/group-store/(scot %da now.bol) /groups pax /noun)
  ==
::
++  pull-wire
  |=  pax=path
  ^-  (list card)
  ?>  ?=(^ pax)
  =/  shp  (~(get by synced) t.pax)
  ?~  shp  ~
  ?:  =(u.shp our.bol)
    [%pass pax %agent [our.bol %contact-store] %leave ~]~
  [%pass pax %agent [u.shp %contact-hook] %leave ~]~
--
