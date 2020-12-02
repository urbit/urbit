::  metadata-hook [landscape]:
::
::  allow syncing foreign metadata
::
::  watch paths:
::  /group/%group-path                      all updates related to this group
::
/-  *metadata-store, *metadata-hook
/+  default-agent, dbug, verb, grpl=group, *migrate
~%  %metadata-hook-top  ..is  ~
|%
+$  card  card:agent:gall
+$  versioned-state
  $%  state-zero
      state-one
  ==
::
+$  state-zero
  $:  %0
      synced=(map group-path ship)
  ==
+$   state-one
  $:  %1
      synced=(map group-path ship)
  ==
--
=|  state-one
=*  state  -
%-  agent:dbug
%+  verb  |
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
  ++  on-load
    |=  =vase
    =/  old
      !<(versioned-state vase)
    ?:  ?=(%1 -.old)
      `this(state old)
    ::  groups OTA did not migrate metadata syncs
    ::  we clear our syncs, and wait for metadata-store
    ::  to poke us with the syncs
    `this
  ::
  ++  on-leave  on-leave:def
  ++  on-peek
    |=  =path
    ^-  (unit (unit cage))
    ?+  path  (on-peek:def path)
        [%x %export ~]
      ``noun+!>(state)
    ==
  ::
  ++  on-arvo
    |=  [=wire =sign-arvo]
    ^-  (quip card _this)
    ?.  ?=([%try-rejoin @ @ *] wire)
      (on-arvo:def wire sign-arvo)
    =/  nack-count=@ud  (slav %ud i.t.wire)
    =/  who=@p          (slav %p i.t.t.wire)
    =/  pax             t.t.t.wire
    ?>  ?=([%b %wake *] sign-arvo)
    ~?  ?=(^ error.sign-arvo)
      "behn errored in backoff timers, continuing anyway"
    :_  this
    [(try-rejoin:hc who pax +(nack-count))]~
  ::
  ++  on-fail   on-fail:def
  ::
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
    ::
        %import
      ?>  (team:title our.bowl src.bowl)
      =^  cards  state
        (poke-import:hc q.vase)
      [cards this]
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
+*  grp  ~(. grpl bowl)
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
  ?>  (is-member:grp src.bowl group-path.act)
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
++  poke-import
  |=  arc=*
  ^-  (quip card _state)
  =/  sty=state-one
    [%1 (remake-map ;;((tree [group-path ship]) +.arc))]
  :_  sty
  %+  murn  ~(tap by synced.sty)
  |=  [=group-path =ship]
  ?:  =(ship our.bowl)
    ~
  =/  =path  [%group group-path]
  `(try-rejoin ship path 0)
::
++  try-rejoin
  |=  [who=@p pax=path nack-count=@ud]
  ^-  card
  =/  =wire
    [%try-rejoin (scot %ud nack-count) (scot %p who) pax]
  [%pass wire %agent [who %metadata-hook] %watch pax]
::
++  watch-group
  |=  =path
  ^-  (list card)
  |^
  ?>  =(our.bowl (~(got by synced) path))
  ?>  (is-member:grp src.bowl path)
  %+  turn  ~(tap by (metadata-scry path))
  |=  [[=group-path =md-resource] =metadata]
  ^-  card
  [%give %fact ~ %metadata-update !>([%add group-path md-resource metadata])]
  ::
  ++  metadata-scry
    |=  pax=^path
    ^-  associations
    =.  pax
      ;:  weld
        /(scot %p our.bowl)/metadata-store/(scot %da now.bowl)/group
        pax
        /noun
      ==
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
  |-
  ?+  wir  !!
      [%try-rejoin @ @ *]
    $(wir t.t.t.wir)
  ::
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
  ?:  ?=([%try-rejoin @ *] wir)
    ?~  saw
      [~ state]
    =/  nack-count=@ud  (slav %ud i.t.wir)
    =/  wakeup=@da
      (add now.bowl (mul ~s1 (bex (min 19 nack-count))))
    :_  state
    [%pass wir %arvo %b %wait wakeup]~
  ?>  ?=(^ wir)
  [~ ?~(saw state state(synced (~(del by synced) t.wir)))]
::
--
