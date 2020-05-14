::  group-proxy-hook: propagate group updates
::
::    This hook relays foreign subscriptions and pokes into local stores, if
::    permissions are met.
::
::  Subscriptions:
::  - /groups/[group-id]: updates for a particular group
::
::  Pokes:
::  -  %group-update: Proxy update to local group update. Crashes if permissions
::     checks fail.
::
/-  *group, hook=group-hook
/+  dbug, verb, store=group-store, default-agent
|%
+$  card  card:agent:gall
+$  state-zero
  $:  %0
      proxied=(jug path ship)
  ==
+$  versioned-state
  $%  state-zero
  ==
--
::
=|  state-zero
=*  state  -
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
  ++  on-init
    :_  this
    :~  [%pass / %arvo %d %flog %text "{(trip dap.bowl)} started"]
        [%pass /store %agent [our.bowl %group-store] %watch /groups]
    ==
  ++  on-save  !>(state)
  ++  on-load
    |=  =vase
    `this(state !<(state-zero vase))
  ++  on-peek   on-peek:def
  ++  on-arvo   on-arvo:def
  ++  on-fail   on-fail:def
  ::
  ++  on-leave
    |=  =path
    ^-  (quip card _this)
    =^  cards   state
      (stop-proxy:gc src.bowl path)
    [cards this]
  ::
  ++  on-poke
    |=  [=mark =vase]
    ^-  (quip card _this)
    =^  cards  state
      ?+  mark  (on-poke:def mark vase)
        %group-action  (poke-group-update:gc !<(action:store vase))
      ==
    [cards this]
  ::
  ++  on-watch
    |=  =path
    ^-  (quip card _this)
    ?<  (team:title [our src]:bowl)
    ?>  ?=([%groups *] path)
    ?>  (permitted:gc src.bowl t.path)
    =^  cards  state
      (start-proxy:gc src.bowl t.path)
    [cards this]
 ::
  ++  on-agent
    |=  [=wire =sign:agent:gall]
    ^-  (quip card _this)
    =^  cards  state
      ?+  -.wire  ~|([dap.bowl %bad-agent-wire wire] !!)
        %store  (take-store-sign sign)
        %listen  (take-listen-sign sign)
      ==
    [cards this]
  --
|_  bol=bowl:gall
::  +should-proxy-poke: Check if poke should be proxied
::
::    We only allow users to add and remove themselves.
++  should-proxy-poke
  |=  =update:store
  ^-  ?
  ?:  ?=(%add-members -.update)
    =(~(tap in ships.update) ~[src.bol])
  ?:  ?=(%remove-members -.update)
    =(~(tap in ships.update) ~[src.bol])
  %.n
::  +poke-group-update: Proxy poke to %group-store
::
::    Only proxy pokes if permissions are correct and we host the group.
::
++  poke-group-update
  |=  =update:store
  ^-  (quip card _state)
  ?:  ?=(%initial -.update)
    [~ state]
  ?>  =(ship.group-id.update our.bol)
  ?>  (should-proxy-poke update)
  =/  =path
    (group-id:en-path:store group-id.update)
  ?>  (permitted src.bol path)
  :_  state
  [%pass [%store path] %agent [our.bol %group-store] %poke %group-update !>(update)]~
::  +take-listen-sign: Handle incoming sign from %group-listen-hook
::
::    group-listen-hook doesn't send us anything except pokes
++  take-listen-sign
  |=  =sign:agent:gall
  ^-  (quip card _state)
  ?-  -.sign
    %kick       [~ state]
    %watch-ack  [~ state]
    %fact       [~ state]
  ::
      %poke-ack
    ?~  p.sign  [~ state]
    =/  =tank
      :-  %leaf
      "{(trip dap.bol)} failed poke to group-listen. very wrong!"
    %-  (slog tank u.p.sign)
    [~ state]
  ==
::  +take-store-sign: Handle incoming sign from %group-store
::
::    group-store should send us all its store updates over a subscription on
::    /groups. We also proxy pokes to it.
::
++  take-store-sign
  |=  =sign:agent:gall
  ^-  (quip card _state)
  ?-  -.sign
    %kick      [watch-store state]
  ::
      %poke-ack
    ?~  p.sign  [~ state]
    =/  =tank
      :-  %leaf
      "{(trip dap.bol)} failed poke to group-store"
    %-  (slog tank u.p.sign)
    [~ state]
  ::
      %watch-ack
    ?~  p.sign  [~ state]
    =/  =tank
      :-  %leaf
      "{(trip dap.bol)} failed subscribe to group-store. very wrong!"
    %-  (slog tank u.p.sign)
    [~ state]
  ::
      %fact
    ?+  p.cage.sign  ~|("{<dap.bol>} unexpected mark: {<p.cage.sign>}" !!)
      %group-initial  [~ state]
      %group-update  (take-store-update !<(update:store q.cage.sign))
    ==
  ==
::  +take-store-update: Handle new %fact from %group-store
::
::    We forward the update onto the correct path, and recalculate permissions,
::    kicking any subscriptions whose permissions have been revoked.
::
++  take-store-update
  |=  =update:store
  ^-  (quip card _state)
  ?:  ?=(%initial -.update)
    `state
  =/  =path
     (group-id:en-path:store group-id.update)
  ?.  (~(has by proxied) path)
    [~ state]
  =^  cards  state
    (handle-revocations update path)
  :_  state
  :-  [%give %fact [%groups path]~ %group-update !>(update)]
  cards
::
::  +handle-revocations: Handle revoked permissions from a update:store
::
::    If the mutation to the group store has impacted permissions, then kick
::    relevant subscriptions and remove them from our state
::
++  handle-revocations
  |=  [=update:store =path]
  ^-  (quip card _state)
  |^
  ?:  ?=(%remove-group -.update)
    =.  proxied
      (~(del by proxied) group-id.update)
    :_  state
    [%give %kick [%groups path]~ ~]~
  ?:  ?=(%change-policy -.update)
    (handle-permissions-change diff.update)
  ?.  ?=(%remove-members -.update)
    [~ state]
  (revoked-permissions ships.update)
  ::
  ++  handle-permissions-change
    |=  =diff:policy
    ^-  (quip card _state)
    ?+  -.diff  [~ state]
      %ban-ships   (revoked-permissions ships.diff)
    ==
  ::
  ++  revoked-permissions
    |=  ships=(set ship)
    ^-  (quip card _state)
    =/  to-kick=(list ship)
      ~(tap in ships)
    =/  subs=(set ship)
      (~(gut by proxied) path ~)
    =|  cards=(list card)
    |-
    ?~  to-kick
      [cards state]
    =.  proxied
      (~(del ju proxied) path i.to-kick)
    =.  cards
      [[%give %kick [%groups path]~ `i.to-kick] cards]
    $(to-kick t.to-kick)
  --
:: +start-proxy: Start proxying .path to .who
::
++  start-proxy
  |=  [who=ship =path]
  ^-  (quip card _state)
  =.  proxied
    (~(put ju proxied) path who)
  [(give-initial path) state]
::  +stop-proxy: Stop proxying .path to .who
::
++  stop-proxy
  |=  [who=ship =path]
  ^-  (quip card _state)
  =.  proxied
    (~(del ju proxied) path who)
  `state
::  +watch-store: Watch group-store for changes
::
++  watch-store
  ^-  (list card)
  [%pass /group-store %agent [our.bol %group-store] %watch /groups]~
::  +permitted: check if .ship can access .path
::
++  permitted
  |=  [=ship =path]
  ^-  ?
  ?>  ?=([@ @ *] path)
  =/  u-group-id
    (group-id:de-path:store path)
  ?~  u-group-id
    %.n
  =*  group-id  u.u-group-id
  =/  pax=^path
    (welp [%groups path] /permitted/[(scot %p ship)])
  (scry-store ? pax)
::  +give-initial: give initial state for .path
::
::    Must be called in +on-watch. No-ops if the group does not exist yet
++  give-initial
  |=  =path
  ^-  (list card)
  =/  u-group
    (scry-store (unit group) [%groups path])
  ?~  u-group  ~
  =*  group  u.u-group
  =/  =group-id
    (need (group-id:de-path:store path))
  =/  =cage
    :-  %group-update
    !>  ^-  update:store
    [%initial-group group-id group]
  [%give %fact ~ cage]~
::
++  scry-store
  |*  [=mold =path]
  .^  mold
      %gx
      (scot %p our.bol)
      %group-store
      (scot %da now.bol)
      (welp path /noun)
  ==
--
