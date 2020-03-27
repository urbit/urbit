::  metadata-hook: allow syncing foreign metadata
::
::  watch paths:
::  /group/%group-path                      all updates related to this group
::
/-  *metadata-store, *metadata-hook
/+  default-agent, dbug
|%
+$  card  card:agent:gall
+$  versioned-state
  $%  state-zero
  ==
::
+$  state-zero
  $:  %0
      synced=(map group-path ship)
  ==
--
=|  state-zero
=*  state  -
%-  agent:dbug
^-  agent:gall
=<
  |_  =bowl:gall
  +*  this        .
      hc          ~(. +> bowl)
      def         ~(. (default-agent this %|) bowl)
  ::
  ++  on-init
    [[%pass /updates %agent [our.bowl %metadata-store] %watch /updates]~ this]
  ::
  ++  on-save   !>(state)
  ++  on-load   |=(=vase `this(state !<(state-zero vase)))
  ++  on-leave  on-leave:def
  ++  on-peek   on-peek:def
  ++  on-arvo   on-arvo:def
  ++  on-fail   on-fail:def
  ++  on-poke
    |=  [=mark =vase]
    ^-  (quip card _this)
    ?+  mark  (on-poke:def mark vase)
        %metadata-hook-action
      =^  cards  state
        (poke-hook-action:hc !<(metadata-hook-action vase))
      [cards this]
    ::
        %metadata-action
      [(poke-action:hc !<(metadata-action vase)) this]
    == 
  ::
  ++  on-watch
    |=  =path
    ^-  (quip card _this)
    ?+  path        (on-watch:def path)
        [%group *]  [(watch-group:hc t.path) this]
    ==
  ::
  ++  on-agent
    |=  [=wire =sign:agent:gall]
    ^-  (quip card _this)
    ?+  -.sign  (on-agent:def wire sign)
        %kick       =^(cards state (kick:hc wire) [cards this])
        %watch-ack  =^(cards state (watch-ack:hc wire p.sign) [cards this])
        %fact
      ?+  p.cage.sign  (on-agent:def wire sign)
          %metadata-update
        =^  cards  state
          (fact-metadata-update:hc wire !<(metadata-update q.cage.sign))
        [cards this]
      ==
    ==
  --
::
|_  =bowl:gall
++  poke-hook-action
  |=  act=metadata-hook-action
  ^-  (quip card _state)
  |^
  ?-  -.act
      %add-owned
    ?>  (team:title our.bowl src.bowl)
    :-  ~
    ?:  (~(has by synced) path.act)  state
    state(synced (~(put by synced) path.act our.bowl))
  ::
      %add-synced
    ?>  (team:title our.bowl src.bowl)
    =/  =path  [%group path.act]
    ?:  (~(has by synced) path.act)  [~ state]
    :_  state(synced (~(put by synced) path.act ship.act))
    [%pass path %agent [ship.act %metadata-hook] %watch path]~
  ::
      %remove
    =/  ship  (~(get by synced) path.act)
    ?~  ship  [~ state]
    ?:  &(!=(u.ship src.bowl) ?!((team:title our.bowl src.bowl)))
      [~ state]
    :_  state(synced (~(del by synced) path.act))
    %-  zing
    :~  (unsubscribe [%group path.act] u.ship)
        [%give %kick ~[[%group path.act]] ~]~
    ==
  ==
  ::
  ++  unsubscribe
    |=  [=path =ship]
    ^-  (list card)
    ?:  =(ship our.bowl)
      [%pass path %agent [our.bowl %metadata-store] %leave ~]~
    [%pass path %agent [ship %metadata-hook] %leave ~]~
  --
::
++  poke-action
  |=  act=metadata-action
  ^-  (list card)
  |^
  ?:  (team:title our.bowl src.bowl)
    ?-  -.act
        %add     (send group-path.act)
        %remove  (send group-path.act)
    ==
  ?>  (is-permitted src.bowl group-path.act)
  ?-  -.act
      %add     (metadata-poke our.bowl %metadata-store)
      %remove  (metadata-poke our.bowl %metadata-store)
  ==
  ::
  ++  send
    |=  =group-path
    ^-  (list card)
    =/  =ship
      %+  slav  %p
      ?:  (is-managed group-path)  (snag 0 group-path)
      (snag 1 group-path)
    =/  app  ?:(=(ship our.bowl) %metadata-store %metadata-hook)
    (metadata-poke ship app)
  ::
  ++  metadata-poke
    |=  [=ship app=@tas]
    ^-  (list card)
    [%pass / %agent [ship app] %poke %metadata-action !>(act)]~
  ::
  ++  is-managed
    |=  =path
    ^-  ?
    ?>  ?=(^ path)
    !=(i.path '~')
  --
::
++  watch-group
  |=  =path
  ^-  (list card)
  |^
  ?>  =(our.bowl (~(got by synced) path))
  ?>  (is-permitted src.bowl path)
  %+  turn  ~(tap by (metadata-scry path))
  |=  [[=group-path =resource] =metadata]
  ^-  card
  [%give %fact ~ %metadata-update !>([%add group-path resource metadata])]
  ::
  ++  metadata-scry
    |=  pax=^path
    ^-  associations
    =.  pax  ;:(weld /=metadata-store/(scot %da now.bowl)/group pax /noun)
    .^(associations %gx pax)
  --
::
++  fact-metadata-update
  |=  [wir=wire fact=metadata-update]
  ^-  (quip card _state)
  |^
  [?:((team:title our.bowl src.bowl) handle-local handle-foreign) state]
  ::
  ++  handle-local
    ?+  -.fact  ~
        %add
      ?.  (~(has by synced) group-path.fact)  ~
      (give group-path.fact fact)
    ::
        %update-metadata
      ?.  (~(has by synced) group-path.fact)  ~
      (give group-path.fact fact)
    ::
        %remove
      ?.  (~(has by synced) group-path.fact)  ~
      (give group-path.fact fact)
    ==
  ::
  ++  handle-foreign
    ?+  -.fact  ~
        %add
      ?.  =(src.bowl (~(got by synced) group-path.fact))  ~
      (poke fact)
    ::
        %update-metadata
      ?.  =(src.bowl (~(got by synced) group-path.fact))  ~
      (poke [%add +.fact])
    ::
        %remove
      ?.  =(src.bowl (~(got by synced) group-path.fact))  ~
      (poke fact)
    ==
  ::
  ++  give
    |=  [=path upd=metadata-update]
    ^-  (list card)
    [%give %fact ~[[%group path]] %metadata-update !>(upd)]~
  ::
  ++  poke
    |=  act=metadata-action
    ^-  (list card)
    [%pass / %agent [our.bowl %metadata-store] %poke %metadata-action !>(act)]~
  --
::
++  kick
  |=  wir=wire
  ^-  (quip card _state)
  :_  state
  ?+  wir  !!
      [%updates ~]
    [%pass /updates %agent [our.bowl %metadata-store] %watch /updates]~
  ::
      [%group @ *]
    ?.  (~(has by synced) t.wir)  ~
    =/  =ship  (~(got by synced) t.wir)
    ?:  =(ship our.bowl)
      [%pass wir %agent [our.bowl %metadata-store] %watch wir]~
    [%pass wir %agent [ship %metadata-hook] %watch wir]~
  ==
::
++  watch-ack
  |=  [wir=wire saw=(unit tang)]
  ^-  (quip card _state)
  ?>  ?=(^ wir)
  [~ ?~(saw state state(synced (~(del by synced) t.wir)))]
::
++  is-permitted
  |=  [=ship pax=path]
  ^-  ?
  =.  pax
    ;:  weld
        /=permission-store/(scot %da now.bowl)/permitted
        [(scot %p ship) pax]
        /noun
    ==
  .^(? %gx pax)
--
