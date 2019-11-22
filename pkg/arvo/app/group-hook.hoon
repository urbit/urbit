::  group-hook: allow syncing group data from foreign paths to local paths
::
/-  *group-store, *group-hook
/+  default-agent
|%
+$  card  card:agent:gall
::
++  versioned-state
  $%  state-zero
  ==
::
::
+$  state-zero
  $:  %0
      synced=(map path ship)
  ==
::
--
::
=|  state-zero
=*  state  -
^-  agent:gall
=<
  |_  =bowl:gall
  +*  this        .
      group-core  +>
      gc          ~(. group-core bowl)
      def         ~(. (default-agent this %|) bowl)
  ::
  ++  on-init  on-init:def
  ++  on-save  !>(state)
  ++  on-load
    |=  old=vase
    `this(state !<(state-zero old))
  ++  on-leave  on-leave:def
  ++  on-peek   on-peek:def
  ++  on-arvo   on-arvo:def
  ++  on-fail   on-fail:def
  ::
  ++  on-poke
    |=  [=mark =vase]
    ^-  (quip card _this)
    ?.  ?=(%group-hook-action mark)
      (on-poke:def mark vase)
    =^  cards  state
      (poke-group-hook-action:gc !<(group-hook-action vase))
    [cards this]
  ::
  ++  on-watch
    |=  =path
    ^-  (quip card _this)
    ?.  ?=([%group @ *] path)
      (on-watch:def path)
    ?.  (~(has by synced.state) t.path)
      (on-watch:def path)
    =/  scry-path=^path
      :(welp /=group-store/(scot %da now.bowl) t.path /noun)
    =/  grp=(unit group)
      .^((unit group) %mx scry-path)
    ?~  grp
      (on-watch:def path)
    :_  this
    [%give %fact ~ %group-update !>([%path u.grp t.path])]~
  ::
  ++  on-agent
    |=  [=wire =sign:agent:gall]
    ^-  (quip card _this)
    ?+    -.sign  (on-agent:def wire sign)
    ::
        %watch-ack
    ^-  (quip card _this)
      ?~  p.sign
        [~ this]
      %-  (slog u.p.sign)
      ?>  ?=([@ @ *] wire)
      =/  =ship   (slav %p i.wire)
      =.  synced.state  (~(del by synced.state) t.t.wire)
      [~ this]
    ::
        %kick
    ^-  (quip card _this)
      ?>  ?=([@ @ *] wire)
      =/  =ship  (slav %p i.wire)
      ?.  (~(has by synced.state) wire)
        [~ this]
      =/  group-path  [%group wire]
      =/  group-wire  [i.wire group-path]
      :_  this
      [%pass group-wire %agent [ship %group-hook] %watch group-path]~
    ::
        %fact
    ^-  (quip card _this)
      ?.  ?=(%group-update p.cage.sign)
        (on-agent:def wire sign)
      =^  cards  state
        ?:  (team:title our.bowl src.bowl)
          (handle-local:gc !<(group-update q.cage.sign))
        (handle-foreign:gc !<(group-update q.cage.sign))
      [cards this]
    ==
  --
::
|_  bol=bowl:gall
::
++  poke-group-hook-action
  |=  act=group-hook-action
  ^-  (quip card _state)
  ?-  -.act
      %add
    ?.  (team:title our.bol src.bol)
      [~ state]
    =/  group-path  [%group path.act]
    =/  group-wire  [(scot %p ship.act) group-path]
    ?:  (~(has by synced.state) path.act)
      [~ state]
    =.  synced.state  (~(put by synced.state) path.act ship.act)
    :_  state
    ?:  =(ship.act our.bol)
      [%pass group-wire %agent [ship.act %group-store] %watch group-path]~
    [%pass group-wire %agent [ship.act %group-hook] %watch group-path]~
  ::
      %remove
    =/  ship  (~(get by synced.state) path.act)
    ?~  ship
      [~ state]
    ?:  &(=(u.ship our.bol) (team:title our.bol src.bol))
      ::  delete one of our own paths
      =/  group-wire  [(scot %p our.bol) %group path.act]
      :_  state(synced (~(del by synced.state) path.act))
      %+  snoc
        (pull-wire group-wire path.act)
      [%give %kick `[%group path.act] ~]
    ?:  |(=(u.ship src.bol) (team:title our.bol src.bol))
      ::  delete a foreign ship's path
      =/  group-wire  [(scot %p u.ship) %group path.act]
      :_  state(synced (~(del by synced.state) path.act))
      (pull-wire group-wire path.act)
    :: don't allow
    [~ state]
  ==
::
++  handle-local
  |=  diff=group-update
  ^-  (quip card _state)
  ?-  -.diff
      %keys    [~ state]
      %path    [~ state]
      %bundle  [~ state]
      %add     [(update-subscribers [%group pax.diff] diff) state]
      %remove  [(update-subscribers [%group pax.diff] diff) state]
  ::
      %unbundle
    :_  state(synced (~(del by synced.state) pax.diff))
    %+  snoc
      (update-subscribers [%group pax.diff] diff)
    [%give %kick `[%group pax.diff] ~]
  ==
::
++  handle-foreign
  |=  diff=group-update
  ^-  (quip card _state)
  ?-  -.diff
      %keys    [~ state]
      %bundle  [~ state]
  ::
      %path
    :_  state
    ?~  pax.diff  ~
    =/  ship  (~(get by synced.state) pax.diff)
    ?~  ship  ~
    ?.  =(src.bol u.ship)  ~
    :~  (group-poke pax.diff [%unbundle pax.diff])
        (group-poke pax.diff [%bundle pax.diff])
        (group-poke pax.diff [%add members.diff pax.diff])
    ==
  ::
      %add
    :_  state
    ?~  pax.diff  ~
    =/  ship  (~(get by synced.state) pax.diff)
    ?~  ship  ~
    ?.  =(src.bol u.ship)  ~
    [(group-poke pax.diff diff)]~
  ::
      %remove
    :_  state
    ?~  pax.diff  ~
    =/  ship  (~(get by synced.state) pax.diff)
    ?~  ship  ~
    ?.  =(src.bol u.ship)  ~
    [(group-poke pax.diff diff)]~
  ::
      %unbundle
    ?~  pax.diff
      [~ state]
    =/  ship  (~(get by synced.state) pax.diff)
    ?~  ship
      [~ state]
    ?.  =(src.bol u.ship)
      [~ state]
    :_  state(synced (~(del by synced.state) pax.diff))
    [(group-poke pax.diff diff)]~
  ==
::
++  group-poke
  |=  [pax=path action=group-action]
  ^-  card
  [%pass pax %agent [our.bol %group-store] %poke %group-action !>(action)]
::
++  group-scry
  |=  pax=path
  ^-  (unit group)
  .^((unit group) %mx ;:(weld /=group-store/(scot %da now.bol) pax /noun))
::
++  update-subscribers
  |=  [pax=path diff=group-update]
  ^-  (list card)
  [%give %fact `pax %group-update !>(diff)]~
::
++  pull-wire
  |=  [wir=wire pax=path]
  ^-  (list card)
  =/  shp  (~(get by synced.state) pax)
  ?~  shp
    ~
  ?:  =(u.shp our.bol)
    [%pass wir %agent [our.bol %group-store] %leave ~]~
  [%pass wir %agent [u.shp %group-hook] %leave ~]~
::
--
