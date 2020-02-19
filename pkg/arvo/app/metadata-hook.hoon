::  metadata-hook: allow syncing foreign metadata
::
/-  *metadata-store, *metadata-hook
/+  default-agent
|%
+$  card  card:agent:gall
::
++  versioned-state
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
^-  agent:gall
=<
  |_  =bowl:gall
  +*  this        .
      hook-core  +>
      hc          ~(. hook-core bowl)
      def         ~(. (default-agent this %|) bowl)
  ::
  ++  on-init
    [[%pass /updates %agent [our.bol %metadata-store] %watch /updates]~ this]
  ::
  ++  on-save   !>(state)
  ++  on-load   |=(=vase `this(state !<(state-zero vase)))
  ++  on-leave  on-leave:def
  ++  on-peek   on-peek:def
  ++  on-arvo   on-arvo:def
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
    == 
  ::
  ++  on-watch
    |=  =path
    ^-  (quip card _this)
    ?+  path        (on-watch:def path)
        [%group *]  [(watch-group:cc t.path) this]
    ==
  ::
  ++  on-agent
    |=  [=wire =sign:agent:gall]
    ^-  (quip card _this)
    ?+  -.sign  (on-agent:def wire sign)
        %kick
      =^  cards  state
        (kick:hc wire)
      [cards this]
    ::
        %watch-ack
      =^  cards  state
        (watch-ack:hc wire p.sign) 
      [cards this]
    ::
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
|_  bol=bowl:gall
++  poke-hook-action
  |=  act=metadata-hook-action
  ^-  (quip card _state)
  |^
  ?-  -.act
      %add-owned
    ?>  (team:title our.bol src.bol)
    :-  ~
    ?:  (~(has by synced) path.act)  state
    state(synced (~(put by synced) path.act our.bol))
  ::
      %add-synced
    ?>  (team:title our.bol src.bol)
    =/  =path  [%group path.act]
    ?:  (~(has by synced) path.act)  [~ state]
    :_  state(synced (~(put by synced) path.act ship.act))
    [%pass path %agent [ship.act %metadata-hook] %watch path]~
  ::
      %remove
    =/  ship  (~(get by synced) path.act)
    ?~  ship  [~ state]
    ?:  &(!(u.ship our.bol) !(team.title our.bol src.bol))
      [~ state]
    :_  state(synced (~(del by synced) path.act))
    %-  zing
    :~  (unsubscribe [%group path.act])
        ?.  &(=(u.ship our.bol) (team:title our.bol src.bol))  ~
        [%give %kick ~[[%group path.act]] ~]~
    ==
  ==
  ::
  ++  unsubscribe
    |=  pax=path
    ^-  (list card)
    ?>  ?=(^ pax)
    =/  shp  (~(get by synced) t.pax)
    ?~  shp  ~
    ?:  =(u.shp our.bol)
      [%pass pax %agent [our.bol %chat-store] %leave ~]~
    [%pass pax %agent [u.shp %chat-hook] %leave ~]~
  --
::
++  watch-group
  |=  =path
  ^-  (list card)
  ?>  (~(has by synced) path)
  ~
::
++  fact-metadata-update
  |=  [wir=wire fact=metadata-update]
  ^-  (quip card _state)
  |^
  [?:((team:title our.bol src.bol) handle-local handle-foreign) state]
  ::
  ++  handle-local
    ?+  -.fact  ~
        %add
      ~
    ::
        %update-metadata
      ~
    ::
        %remove
      ~
    ==
  ::
  ++  handle-foreign
    ?+  -.fact  ~
        %add
      ~
    ::
        %update-metadata
      ~
    ::
        %remove
      ~
    ==
  --
::
++  kick
  |=  wir=wire
  ^-  (quip card _state)
  [~ state]
::
++  watch-ack
  |=  [wir=wire saw=(unit tang)]
  ^-  (quip card _state)
  ?~  saw  [~ state]
  ?>  ?=(^ wir)
  [~ state(synced (~(del by synced) t.wir))]
--
