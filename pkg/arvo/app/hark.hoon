::  hark: notifications [landscape]
::
/-  *resource, store=hark, post, group-store, graph-store, metadata-store
/+  res=resource, metadata, default-agent, dbug
=*  resource  resource:post
::
~%  %hark-top  ..is  ~
|%
+$  card  card:agent:gall
+$  versioned-state
  $%  state-0
  ==
::
+$  state-0
  $:  %0
      unreads=(map group=resource (map =app=resource unread-mop:store))
  ==
::
++  orm  ((ordered-map index:post unread:store) compare-indexes:post)
--
::
=|  state-0
=*  state  -
::
%-  agent:dbug
^-  agent:gall
~%  %hark-agent  ..card  ~
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
    met   ~(. metadata bowl)
::
++  on-init
  :_  this
  :~  [%pass /metadata %agent [our.bowl %metadata-store] %watch /updates]
      [%pass /group %agent [our.bowl %group-store] %watch /groups]
      [%pass /graph %agent [our.bowl %graph-store] %watch /updates]
  ==
::
++  on-save  !>(state)
++  on-load
  |=  old=vase
  ^-  (quip card _this)
  [~ this(state !<(state-0 old))]
::
++  on-watch
  ~/  %hark-watch
  |=  =path
  ^-  (quip card _this)
  |^
  ?>  (team:title our.bowl src.bowl)
  =/  cards=(list card)
    ?+  path           (on-watch:def path)
        [%updates ~]
      %-  give
      :-  %keys
      %-  ~(run by unreads)
      |=  res=(map app=resource unread-mop:store)
      ~(key by res)
    ==
  [cards this]
  ::
  ++  give
    |=  =update-0:store
    ^-  (list card)
    [%give %fact ~ [%hark-update !>([%0 update-0])]]~
  --
::
++  on-poke
  ~/  %hark-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  |^
  ?>  (team:title our.bowl src.bowl)
  =^  cards  state
    ?+  mark           (on-poke:def mark vase)
        %hark-action   (hark-action !<(action:store vase))
    ==
  [cards this]
  ::
  ++  hark-action
    |=  =action:store
    ^-  (quip card _state)
    |^
    ?-  -.action
      %listen  (listen +.action)
      %ignore  (ignore +.action)
      %read    (read +.action)
    ==
    ::
    ++  listen
      |=  =app=resource
      ^-  (quip card _state)
      =/  group-resource=resource  (get-group-resource app-resource)
      ::  set up subscriptions and create entry in maps
      ::
      =/  by-group
        %+  ~(gut by unreads)
          group-resource
        *(map =app=resource unread-mop:store)
      ::
      ?:  (~(has by by-group) app-resource)
        [~ state]
      ::
      =.  unreads
        %+  ~(put by unreads)  group-resource
        (~(put by by-group) app-resource *unread-mop:store)
      ::
      :_  state
      [%give %fact [/updates]~ %hark-update !>([%0 %listen app-resource])]~
    ::
    ++  ignore
      |=  =app=resource
      ^-  (quip card _state)
      =/  group-resource=resource  (get-group-resource app-resource)
      =/  by-group  (~(get by unreads) group-resource)
      ?~  by-group
        [~ state]
      =.  u.by-group  (~(del by u.by-group) app-resource)
      =.  unreads
        ?~  u.by-group
          (~(del by unreads) group-resource)
        (~(put by unreads) group-resource u.by-group)
      ::
      :_  state
      [%give %fact [/updates]~ %hark-update !>([%0 %ignore app-resource])]~
    ::
    ++  read
      |=  item=read-type:store
      ^-  (quip card _state)
      :-  [%give %fact [/updates]~ %hark-update !>([%0 %read item])]~
      ?-  -.item
          %group
        =.  unreads
          %+  ~(jab by unreads)  group-resource.item
          |=  by-group=(map =app=resource unread-mop:store)
          (~(run by by-group) |=(* *unread-mop:store))
        state
     ::
          %app
        =/  group-resource=resource  (get-group-resource app-resource.item)
        =.  unreads
          %+  ~(jab by unreads)  group-resource
          |=  by-group=(map =app=resource unread-mop:store)
          (~(put by by-group) app-resource.item *unread-mop:store)
        state
     ::
          %app-at-index
        =/  group-resource=resource  (get-group-resource app-resource.item)
        =.  unreads
          %+  ~(jab by unreads)  group-resource
          |=  by-group=(map =app=resource unread-mop:store)
          %+  ~(jab by by-group)  app-resource.item
          |=  by-app=unread-mop:store
          +:(del:orm by-app index.item)
        state
      ==
    ::
    ++  give
      |=  [paths=(list path) update=update-0:store]
      ^-  (list card)
      [%give %fact paths [%hark-update !>([%0 update])]]~
    ::
    ++  get-group-resource
      |=  =app=resource
      ^-  resource
      =/  group-resource=(unit resource)
        (group-from-app-resource:met %graph app-resource)
      ?~  group-resource
        ~|  no-group-for-app-resource+app-resource
        !!
      u.group-resource
    --
  --
::
++  on-agent
  ~/  %hark-agent
  |=  [=wire =sign:agent:gall]
  ^-  (quip card _this)
  |^
  ?+  -.sign  (on-agent:def wire sign)
      %kick
    ::  TODO resubscribe on kick
    [~ this]
  ::
      %fact
    ?+  p.cage.sign  (on-agent:def wire sign)
        %group-update
      =^  cards  state
        (group-update !<(update:group-store q.cage.sign))
      [cards this]
    ::
        %graph-update
      =^  cards  state
        (graph-update !<(update:graph-store q.cage.sign))
      [cards this]
    ::
        %metadata-update
      =^  cards  state
        (metadata-update !<(metadata-update:metadata-store q.cage.sign))
      [cards this]
    ==
  ==
  ::
  ++  group-update
    |=  =update:group-store
    ^-  (quip card _state)
    [~ state]
  ::
  ++  graph-update
    |=  =update:graph-store
    ^-  (quip card _state)
    [~ state]
  ::
  ++  metadata-update
    |=  update=metadata-update:metadata-store
    ^-  (quip card _state)
    [~ state]
  --
::
++  on-peek
  ~/  %hark-peek
  |=  =path
  ^-  (unit (unit cage))
  !!
::
++  on-leave  on-leave:def
++  on-arvo  on-arvo:def
++  on-fail   on-fail:def
--
