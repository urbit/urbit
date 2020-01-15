::  contact-hook:
::
/-  *group-store, *group-hook, *contact-hook, *invite-store
/+  *contact-json, default-agent
|%
+$  card  card:agent:gall
::
+$  versioned-state
  $%  state-zero
  ==
::
+$  state-zero
  $:  %0
      synced=(map path ship)
      invite-created=_|
  ==
--
=|  state-zero
=*  state  -
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
        [%pass /group %agent [our.bol %group-store] %watch /updates]
    ==
  ++  on-save   !>(state)
  ++  on-load
    |=  old=vase
    `this(state !<(state-zero old))
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
          (fact-group-update:cc wire !<(group-update q.cage.sign))
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
::
++  poke-json
  |=  jon=json
  ^-  (quip card _state)
  (poke-contact-action (json-to-action jon))
::
++  poke-contact-action
  |=  act=contact-action
  ^-  (quip card _state)
  ?>  ?=(%edit -.act)
  ::  local
  :_  state
  ?:  (team:title our.bol src.bol)
    =/  ship  (~(got by synced) path.act)
    =/  appl  ?:(=(ship our.bol) %contact-store %contact-hook)
    [%pass / %agent [ship appl] %poke %contact-action !>(act)]~
  ::  foreign
  =/  ship  (~(got by synced) path.act)
  ?.  |(=(ship our.bol) =(src.bol ship.act))  ~
  ::  scry group to check if ship is a member
  =/  =group  (need (group-scry path.act))
  ?.  (~(has in group) ship)  ~
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
    [%pass contact-path %agent [our.bol %contact-store] %watch contact-path]~
  ::
      %add-synced
    ?>  (team:title our.bol src.bol)
    ?:  (~(has by synced) path.act)
      [~ state]
    =.  synced  (~(put by synced) path.act ship.act)
    =/  contact-path  [%contacts path.act]
    :_  state
    [%pass contact-path %agent [ship.act %contact-hook] %watch contact-path]~
  ::
      %remove
    =/  ship  (~(get by synced) path.act)
    ?~  ship
      [~ state]
    ?:  &(=(u.ship our.bol) (team:title our.bol src.bol))
      ::  delete one of our.bol own paths
      :_  state(synced (~(del by synced) path.act))
      %-  zing
      :~  (pull-wire [%contacts path.act])
          [%give %kick `[%contacts path.act] ~]~
      ==
    ?.  |(=(u.ship src.bol) (team:title our.bol src.bol))
      ::  if neither ship = source or source = us, do nothing
      [~ state]
    ::  delete a foreign ship's path
    :-  (pull-wire [%contacts path.act])
    state(synced (~(del by synced) path.act))
  ==
::
++  watch-contacts
  |=  pax=path
  ^-  (list card)
  ?>  ?=(^ pax)
  ?>  (~(has by synced) pax)
  ::  scry groups to check if ship is a member
  =/  =group  (need (group-scry pax))
  ?>  (~(has in group) src.bol)
  =/  contacts  (need (contacts-scry pax))
  :~  :*
    %give  %fact  ~  %contact-update
    !>([%contacts pax contacts])
  ==  ==
::
++  watch-ack
  |=  [wir=wire saw=(unit tang)]
  ^-  (quip card _state)
  ?~  saw
    [~ state]
  ?>  ?=(^ wir)
  :_  state(synced (~(del by synced) t.wir))
  %.  ~
  %-  slog
  :*  leaf+"contact-hook failed subscribe on {(spud t.wir)}"
      leaf+"stack trace:"
      u.saw
  ==
::
++  kick
  |=  wir=wire
  ^-  (list card)
  ?+  wir  !!
      [%inv ~]
    [%pass /inv %agent [our.bol %invite-store] %watch /invitatory/contacts]~
  ::
      [%group ~]
    [%pass /group %agent [our.bol %group-store] %watch /updates]~
  ::
      [%contacts @ *]
    ~&  contacts-kick+wir
    ?.  (~(has by synced) t.wir)  ~
    ~&  %contact-hook-resubscribe
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
  (foreign fact)
  ::
  ++  local
    |=  fact=contact-update
    ^-  (quip card _state)
    ?+  -.fact  [~ state]
        %add
      [(give-fact [%add path.fact ship.fact contact.fact]) state]
    ::
        %remove
      :_  state
      :-  [%give %kick `[%contacts path.fact] `ship.fact]
      (give-fact [%remove path.fact ship.fact])
    ::
        %edit
      [(give-fact [%edit path.fact ship.fact edit-field.fact]) state]
    ==
  ::
  ++  give-fact
    |=  update=contact-update
    ^-  (list card)
    [%give %fact ~ %contact-update !>(update)]~
  ::
  ++  foreign
    |=  fact=contact-update
    ^-  (quip card _state)
    ?+  -.fact  [~ state]
        %contacts
      =/  owner  (~(got by synced) path.fact)
      ?>  =(owner src.bol)
      :_  state
      %+  weld
      :~  (contact-poke [%delete path.fact])
          (contact-poke [%create path.fact])
      ==
      %+  turn  ~(tap by contacts.fact)
      |=  [=ship =contact]
      (contact-poke [%add path.fact ship contact])
    ::
        %add
      =/  owner  (~(got by synced) path.fact)
      ?>  =(owner src.bol)
      [~[(contact-poke [%add path.fact ship.fact contact.fact])] state]
    ::
        %remove
      =/  owner  (~(got by synced) path.fact)
      ?>  =(owner src.bol)
      [~[(contact-poke [%remove path.fact ship.fact])] state]
    ::
        %edit
      =/  owner  (~(got by synced) path.fact)
      ?>  =(owner src.bol)
      [~[(contact-poke [%edit path.fact ship.fact edit-field.fact])] state]
    ==
  --
::
++  fact-group-update
  |=  [wir=wire fact=group-update]
  ^-  (quip card _state)
  |^
  ?+  -.fact     [~ state]
      %remove    (remove +.fact)
      %unbundle  (unbundle +.fact)
  ==
  ::
  ++  unbundle
    |=  =path
    ^-  (quip card _state)
    ?.  (~(has by synced) path)
      [~ state]
    :_  state(synced (~(del by synced) path))
    [%pass [%contacts path] %agent [our.bol %contact-store] %leave ~]~
  ::
  ++  remove
    |=  [members=group =path]
    ^-  (quip card _state)
    ::  if pax is synced, remove member from contacts and kick their sub
    ?.  (~(has by synced) path)
      [~ state]
    :_  state
    %-  zing
    %+  turn  ~(tap in members)
    |=  =ship
    :~  [%give %kick `[%contacts path] `ship]
        (contact-poke [%remove path ship])
    ==
  --
::
++  fact-invite-update
  |=  [wir=wire fact=invite-update]
  ^-  (quip card _state)
  ?+  -.fact  [~ state]
      %accepted
    =/  changes
      (poke-hook-action [%add-synced ship.invite.fact path.invite.fact])
    :-  
    %+  welp
      [(group-hook-poke [%add ship.invite.fact path.invite.fact])]~
    -.changes
    +.changes
  ==
::
++  group-hook-poke
  |=  act=group-hook-action
  ^-  card
  [%pass / %agent [our.bol %group-hook] %poke %group-hook-action !>(act)]
::
++  invite-poke
  |=  act=invite-action
  ^-  card
  [%pass / %agent [our.bol %invite-store] %poke %invite-action !>(act)]
::
++  contact-poke
  |=  act=contact-action
  ^-  card
  ~&  contact+act
  [%pass / %agent [our.bol %contact-store] %poke %contact-action !>(act)]
::
++  contacts-scry
  |=  pax=path
  ^-  (unit contacts)
  =.  pax  ;:(weld /=contact-store/(scot %da now.bol)/contacts pax /noun)
  .^((unit contacts) %gx pax)
::
++  invite-scry
  |=  uid=serial
  ^-  (unit invite)
  =/  pax
    /=invite-store/(scot %da now.bol)/invite/contacts/(scot %uv uid)/noun
  .^((unit invite) %gx pax)
::
++  group-scry
  |=  pax=path
  ^-  (unit group)
  .^((unit group) %gx ;:(weld /=group-store/(scot %da now.bol) pax /noun))
::
++  pull-wire
  |=  pax=path
  ^-  (list card)
  ?>  ?=(^ pax)
  =/  shp  (~(get by synced) t.pax)
  ?~  shp  ~
  ?:  =(u.shp our.bol)
    [%pass pax %agent [our.bol %chat-store] %leave ~]~
  [%pass pax %agent [u.shp %chat-hook] %leave ~]~
--
