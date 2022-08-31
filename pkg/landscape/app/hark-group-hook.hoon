::  hark-group-hook: notifications for groups [landscape]
::
/-  store=hark-store, post, group-store, metadata=metadata-store, hook=hark-group-hook
/+  resource, mdl=metadata, default-agent, dbug, graph-store
::
~%  %hark-group-hook-top  ..part  ~
|%
+$  card  card:agent:gall
+$  versioned-state
  $%  state-0
  ==
::
+$  state-0
  $:  %0
      watching=(set resource)
  ==
+$  update
  $>(?(%add-members %remove-members) update:group-store)
::
--
::
=|  state-0
=*  state  -
::
=<
%-  agent:dbug
^-  agent:gall
~%  %hark-group-hook-agent  ..card  ~
|_  =bowl:gall
+*  this  .
    ha    ~(. +> bowl)
    def   ~(. (default-agent this %|) bowl)
    met   ~(. mdl bowl)
::
++  on-init
  :_  this
  :~  watch-metadata:ha
      watch-groups:ha
  ==
::
++  on-save  !>(state)
++  on-load
  |=  old=vase
  ^-  (quip card _this)
  `this(state !<(state-0 old))
::
++  on-watch
  |=  =path
  ?.  ?=([%updates ~] path)
    (on-watch:def path)
  :_  this
  =;  =cage
    [%give %fact ~ cage]~
  :-  %hark-group-hook-update
  !>  ^-  update:hook
  [%initial watching]
::
++  on-poke
  ~/  %hark-group-hook-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  |^
  ?>  (team:title our.bowl src.bowl)
  =^  cards  state
    ?+  mark           (on-poke:def mark vase)
        %hark-group-hook-action
      (hark-group-hook-action !<(action:hook vase))
    ==
  [cards this]
  ::
  ++  hark-group-hook-action
    |=  =action:hook
    ^-  (quip card _state)
    |^
    ?-  -.action
      %listen  (listen +.action)
      %ignore  (ignore +.action)
    ==
    ++  listen
      |=  group=resource
      ^-  (quip card _state)
      :-  (give %listen group)
      state(watching (~(put in watching) group))
    ::
    ++  ignore
      |=  group=resource
      ^-  (quip card _state)
      :-  (give %ignore group)
      state(watching (~(del in watching) group))
    ::
    ++  give
      |=  =update:hook
      ^-  (list card)
      [%give %fact ~[/updates] %hark-group-hook-update !>(update)]~
    --
  --
::
++  on-agent
  ~/  %hark-group-hook-agent
  |=  [=wire =sign:agent:gall]
  ^-  (quip card _this)
  |^
  ?+  -.sign  (on-agent:def wire sign)
      %kick
    :_  this
    ?+  wire  ~
      [%group ~]     ~[watch-groups:ha]
      [%metadata ~]  ~[watch-metadata:ha]
    ==
  ::
      %fact
    ?+  p.cage.sign  (on-agent:def wire sign)
        %group-update-0
      =^  cards  state
        (group-update !<(update:group-store q.cage.sign))
      [cards this]
    ::
        %metadata-update-2
      =^  cards  state
        (metadata-update !<(update:metadata q.cage.sign))
      [cards this]
    ==
  ==
  ::
  ++  group-update
    |=  =update:group-store
    ^-  (quip card _state)
    ?.  ?=(?(%add-members %remove-members) -.update)
      [~ state]
    ?.  (~(has in watching) resource.update)
      [~ state]
    =/  body=(unit body:store)
      (get-content:ha update)
    ?~  body  `state
    =/  =bin:store
      (get-bin:ha resource.update -.update)
    :_  state
    ~[(add-unread bin u.body)]
  ::  +metadata-update is stubbed for now, for the following reasons
  ::    - There's no semantic difference in metadata-store between
  ::    adding and editing a channel
  ::    - We have no way of retrieving old metadata to e.g. get a
  ::    channel's old name when it is renamed
  ++  metadata-update
    |=  =update:metadata
    ^-  (quip card _state)
    [~ state]
  ::
  ++  add-unread
    |=  [=bin:store =body:store]
    ^-  card
    =-  [%pass / %agent [our.bowl %hark-store] %poke -]
    :-  %hark-action
    !>  ^-  action:store
    [%add-note bin body]
  --
::
++  on-peek  on-peek:def
++  on-leave  on-leave:def
++  on-arvo  on-arvo:def
++  on-fail   on-fail:def
--
|_  =bowl:gall
+*  met  ~(. mdl bowl)
++  get-content
  |=  =update:group-store
  ^-  (unit body:store)
  ?.  ?=(?(%add-members %remove-members) -.update)  ~
  ?~  meta=(peek-metadatum:met %groups resource.update)
    ~
  =/  ships=(list content:store)
    %+  turn  ~(tap in ships.update)
    |=  =ship
    ^-  content:store
    ship+ship
  =/  sep=content:store  text+', '
  =.  ships
    (join sep ships)
  ?-  -.update
      %add-members
    :-  ~
    :*  (snoc ships text+(rap 3 ' joined ' title.u.meta ~))
        ~
        now.bowl
        /
        /
    ==
  ::
      %remove-members
    :-  ~
    :*  (snoc ships text+(rap 3 ' left ' title.u.meta ~))
        ~
        now.bowl
        /
        /
    ==
  ==
++  get-bin
  |=  [rid=resource reason=@t]
  ^-  bin:store
  [/[reason] (get-place rid)]
++  get-place
  |=  rid=resource
  ^-  place:store
  [q.byk.bowl /(scot %p entity.rid)/[name.rid]]
::
++  watch-groups
  ^-  card
  [%pass /group %agent [our.bowl %group-store] %watch /groups]
::
++  watch-metadata
  ^-  card
  [%pass /metadata %agent [our.bowl %metadata-store] %watch /updates]
--
