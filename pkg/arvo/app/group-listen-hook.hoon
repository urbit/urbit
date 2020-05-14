::  group-listen-hook: receive group updates
::
::
::
/-  *group, hook=group-hook
/+  dbug, verb, store=group-store, default-agent
|%
+$  card  card:agent:gall
+$  versioned-state
  $%  state-zero
  ==
+$  state-zero
  $:  %0
      listening=(set group-id)
  ==
--

=|  state-zero
=*  state  -
::
%-  agent:dbug
%+  verb  |
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
    |=  =vase
    ^-  (quip card _this)
    =/  old  !<(state-zero vase)
    `this(state old)
  ::
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
      (poke-hook-action:gc !<(action:hook vase))
    [cards this]
  ::
  ++  on-watch  on-watch:def

  ++  on-agent
    |=  [=wire =sign:agent:gall]
    ^-  (quip card _this)
    =^  cards  state
      ?+  -.wire  ~|([dap.bowl %weird-agent-wire wire] !!)
        %store  (take-store-sign sign)
        %proxy  (take-proxy-sign +.wire sign)
      ==
    [cards this]
  --
|_  bol=bowl:gall
::  +poke-hook-action: Start/stop syncing a foreign group
::
++  poke-hook-action
  |=  =action:hook
  ^-  (quip card _state)
  |^
  ?-  -.action
    %add     (add +.action)
    %remove  (remove +.action)
  ==
  ++  add
    |=  =group-id
    ^-  (quip card _state)
    ?:  (~(has in listening) group-id)
      `state
    =.  listening
      (~(put in listening) group-id)
    =/  group-path
      (group-id:en-path:store group-id)
    :_  state
    %+  weld
      (listen-group group-id)
    (add-self group-id)
  ++  remove
    |=  =group-id
    ^-  (quip card _state)
    ?.  (~(has in listening) group-id)
      `state
    =.  listening
      (~(del in listening) group-id)
    :_  state
    (leave-group group-id)
  --
::  +take-store-sign: take sign from %group-store
::
::    We only poke %group-store to remove ourselves when we leave a channel.
++  take-store-sign
  |=  =sign:agent:gall
  ^-  (quip card _state)
  ?-  -.sign
    %watch-ack   [~ state]
    %kick        [~ state]
    %fact        [~ state]
    %poke-ack
      ?~  p.sign  [~ state]
      =/  =tank
        :-  %leaf
        "{(trip dap.bol)} failed poke to group-store"
      %-  (slog tank u.p.sign)
      [~ state]
  ==
::  +take-proxy-sign: take sign from %group-proxy-hook
::
++  take-proxy-sign
  |=  [=wire =sign:agent:gall]
  ^-  (quip card _state)
  =/  =group-id
    ~|  "bad proxy wire: {<wire>}"
    ~|  <sign>
    (need (group-id:de-path:store wire))
  ?-  -.sign
    %kick       [(listen-group group-id) state]
  ::
      %poke-ack
    ?~  p.sign  [~ state]
    =/  =tank
      :-  %leaf
      "{(trip dap.bol)} failed poke to group-proxy!"
    %-  (slog tank u.p.sign)
    [~ state]
  ::
      %watch-ack
    ?~  p.sign  [~ state]
    =/  =tank
      :-  %leaf
      "{(trip dap.bol)} failed subscribe to group-proxy!"
    %-  (slog tank u.p.sign)
    [(store-leave-group group-id) state]
  ::
      %fact
    ?>  ?=(%group-update p.cage.sign)
    (take-proxy-update wire !<(update:store q.cage.sign))
  ==
::
::  +take-proxy-update: Handle new update from %group-proxy-hook
::
++  take-proxy-update
  |=  [=wire =update:store]
  ^-  (quip card _state)
  ?:  ?=(%initial -.update)
    [~ state]
  :_  state
  [%pass [%listen wire] %agent [our.bol %group-store] %poke %group-update !>(update)]~
::  +listen-group: Start a new subscription to the proxied .group-id
::
++  listen-group
  |=  =group-id
  ^-  (list card)
  =/   pax=path
    (group-id:en-path:store group-id)
  [%pass [%listen pax] %agent [ship.group-id %group-proxy-hook] %watch [%groups pax]]~
::  +add-self: Add self to group
++  add-self
  |=  =group-id
  ^-  (list card)
  =/   pax=path
    (group-id:en-path:store group-id)
  [%pass [%listen pax] %agent [ship.group-id %group-proxy-hook] %poke %group-action !>([%add-members group-id (sy our.bol ~) ~])]~
::  +leave-group: Leave a foreign group
++  leave-group
  |=  =group-id
  ^-  (list card)
  =/  pax=path
    (group-id:en-path:store group-id)
  :~  [%pass [%listen pax] %agent [ship.group-id %group-proxy-hook] %leave ~]
      [%pass [%store pax] %agent [our.bol %group-store] %poke %group-update !>([%remove-group group-id ~])]
      [%pass [%listen pax] %agent [ship.group-id %group-proxy-hook] %poke %group-action !>([%remove-members group-id (sy our.bol ~)])]
  ==
::  +store-leave-group: Remove a foreign group from our group-store
++  store-leave-group
  |=  =group-id
  ^-  (list card)
  =/  pax=path
    (group-id:en-path:store group-id)
  [%pass [%store pax] %agent [our.bol %group-store] %poke %group-update !>([%remove-group group-id ~])]~
::
--
