::  group-hook: allow syncing group data from foreign paths to local paths
::
/-  *group, hook=group-hook, *invite-store
/+  default-agent, verb, dbug, store=group-store, grpl=group
~%  %group-hook-top  ..is  ~
|%
+$  card  card:agent:gall
::
++  versioned-state
  $%  state-zero
      state-one
  ==
::
::
+$  state-zero
  $:  %0
      synced=(map path ship)
  ==
::
+$  state-one
  $:  %1
      listening=(set group-id)
      proxied=(jug group-id ship)
  ==
::
--
::
=|  state-one
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
  ++  on-init
    ^-  (quip card _this)
    :_  this
    ~[watch-store:gc]
  ++  on-save  !>(state)
  ++  on-load
    |=  =vase
    ^-  (quip card _this)
    =/  old  !<(state-one vase)
    `this(state old)
    ::  %+  murn  ~(tap by synced.old)
    ::  |=  [=path =ship]
    ::  ^-  (unit card)
    ::  =/  =wire  [(scot %p ship) %group path]
    ::  =/  =term  ?:(=(our.bowl ship) %group-store %group-hook)
    ::  ?:  (~(has by wex.bowl) [wire ship term])  ~
    ::  `[%pass wire %agent [ship term] %watch [%group path]]
  ::
  ++  on-poke
    |=  [=mark =vase]
    ^-  (quip card _this)
    =^  cards  state
      ?+  mark  (on-poke:def mark vase)
        %group-action       (poke-group-update:gc !<(action:store vase))
        %group-hook-action  (poke-hook-action:gc !<(action:hook vase))
      ==
    [cards this]
  ::
  ++  on-agent
    |=  [=wire =sign:agent:gall]
    ^-  (quip card _this)
    =^  cards  state
      ?+  -.wire  (on-agent:def:gc wire sign)
        %invites  (take-invite-sign:gc wire sign)
        %store    (take-store-sign:gc wire sign)
        %proxy    (take-proxy-sign:gc wire sign)
      ==
    [cards this]
  ::
  ++  on-watch
    |=  =path
    ^-  (quip card _this)
    ?<  (team:title [our src]:bowl)
    ?>  ?=([%groups *] path)
    =/  =group-id
      (need (group-id:de-path:store t.path))
    ?>  (can-join:grp:gc group-id src.bowl)
    =^  cards  state
      (start-proxy:gc src.bowl group-id)
    [cards this]
  ::
  ++  on-leave
    |=  =path
    ^-  (quip card _this)
    ?>  ?=([%groups @ @ ~] path)
    =/  =group-id
      (need (group-id:de-path:store t.path))
    =^  cards   state
      (stop-proxy:gc src.bowl group-id)
    [cards this]
  ++  on-peek   on-peek:def
  ++  on-arvo   on-arvo:def
  ++  on-fail   on-fail:def
  --
|_  bol=bowl:gall
++  def  ~(. (default-agent state %|) bol)
++  grp  ~(. grpl bol)
::  +|  %pokes
::
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
  =/  =path
    (group-id:en-path:store group-id.update)
  ?>  (should-proxy-poke update)
  ?>  (can-join:grp group-id.update src.bol)
  :_  state
  [%pass [%store path] %agent [our.bol %group-store] %poke %group-update !>(update)]~
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
    :_  state
    :~  (listen-group group-id)
        (add-self group-id)
    ==
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
::  |signs: signs from agents
::
::  +|  %signs
++  take-invite-sign
  |=  [=wire =sign:agent:gall]
  ^-  (quip card _state)
  |^
  ?+  -.sign  (on-agent:def wire sign)
    %kick   [~[watch-invites] state]
  ::
      %fact
    ?.  =(%invite-update p.cage.sign)
      [~ state]
    (fact !<(invite-update q.cage.sign))
  ==
  ++  fact
    |=  update=invite-update
    ^-  (quip card _state)
    ?+  -.update  [~ state]
        %invite
      =*  invite  invite.update
      ?.  =(our.bol ship.invite)
        [~ state]
      =/  =group-id
        (need (group-id:de-path:store path.invite))
      =/  =cage
        :-  %group-update
        !>  ^-  update:store
        [%change-policy group-id [%add-invites (sy recipient.invite ~)]]
      :_  state
      [%pass [%store path.invite] %agent [our.bol %group-store] %poke cage]~
        %accepted
      =*  invite  invite.update
      ?.  =(our.bol ship.invite)
        [~ state]
      =/  =group-id
        (need (group-id:de-path:store path.update))
      =/  =cage
        :-  %group-update
        !>  ^-  update:store
        [%add-members group-id (sy recipient.invite ~) ~]
      :_  state
      [%pass [%store path.invite] %agent [our.bol %group-store] %poke cage]~
    ==
  --
::  +take-proxy-sign: take sign from foreign %group-hook
::
++  take-proxy-sign
  |=  [=wire =sign:agent:gall]
  ^-  (quip card _state)
  =/  =group-id
    ~|  "bad proxy wire: {<wire>}"
    (need (group-id:de-path:store +.wire))
  |^
  ?+  -.sign  (on-agent:def wire sign)
    %kick  kick
  ::
      %fact
    ?.  ?=(%group-update p.cage.sign)
      [~ state]
    (fact !<(update:store q.cage.sign))
  ==
  ::  +kick: Handle kick
  ::
  ::    Only rejoin if user is still in group
  ++  kick
    =/  group=(unit group)
      (scry-initial group-id)
    ?~  group
      [~ state]
    ?.  (~(has in members.u.group) our.bol)
      [~ state]
    :_  state
    ~[(listen-group group-id)]
  ::
  ::  +fact: Handle new update from %group-hook
  ::
  ++  fact
    |=  =update:store
    ^-  (quip card _state)
    ?:  ?=(%initial -.update)
      [~ state]
    :_  state
    [%pass [%store wire] %agent [our.bol %group-store] %poke %group-update !>(update)]~
  --
::  +take-store-sign: Handle incoming sign from %group-store
::
::    group-store should send us all its store updates over a subscription on
::    /groups. We also proxy pokes to it.
::
++  take-store-sign
  |=  [=wire =sign:agent:gall]
  ^-  (quip card _state)
  |^
  ?+  -.sign  (on-agent:def wire sign)
    %kick      [~[watch-store] state]
  ::
      %fact
    ?+  p.cage.sign  ~|("{<dap.bol>} unexpected mark: {<p.cage.sign>}" !!)
      %group-initial  [~ state]
      %group-update  (fact !<(update:store q.cage.sign))
    ==
  ==
  ::  +fact: Handle new %fact from %group-store
  ::
  ::    We forward the update onto the correct path, and recalculate permissions,
  ::    kicking any subscriptions whose permissions have been revoked.
  ::
  ++  fact
    |=  =update:store
    ^-  (quip card _state)
    ?:  ?=(%initial -.update)
      `state
    =/  =path
      (group-id:en-path:store group-id.update)
    ?.  (~(has by proxied) group-id.update)
      [~ state]
    =^  cards  state
      (handle-revocations update)
    :_  state
    :-  [%give %fact [%groups path]~ %group-update !>(update)]
    cards
  --
::  +listen-group: Start a new subscription to the proxied .group-id
::
++  listen-group
  |=  =group-id
  ^-  card
  =/   pax=path
    (group-id:en-path:store group-id)
  [%pass [%proxy pax] %agent [ship.group-id %group-hook] %watch [%groups pax]]
::  +add-self: Add self to group
++  add-self
  |=  =group-id
  ^-  card
  =/   pax=path
    (group-id:en-path:store group-id)
  =/  =cage
    :-  %group-action
    !>  ^-  action:store
    [%add-members group-id (sy our.bol ~) ~]
  [%pass [%proxy pax] %agent [ship.group-id %group-hook] %poke cage]
::  +leave-group: Leave a foreign group
:: 
++  leave-group
  |=  =group-id
  ^-  (list card)
  =/  pax=path
    (group-id:en-path:store group-id)
  :~  [%pass [%proxy pax] %agent [ship.group-id %group-hook] %leave ~]
      [%pass [%store pax] %agent [our.bol %group-store] %poke %group-update !>([%remove-group group-id ~])]
      [%pass [%proxy pax] %agent [ship.group-id %group-hook] %poke %group-action !>([%remove-members group-id (sy our.bol ~)])]
  ==
::  +store-leave-group: Remove a foreign group from our group-store
::
++  store-leave-group
  |=  =group-id
  ^-  card
  =/  pax=path
    (group-id:en-path:store group-id)
  [%pass [%store pax] %agent [our.bol %group-store] %poke %group-update !>([%remove-group group-id ~])]
::  +should-proxy-poke: Check if poke should be proxied
::
::    We only allow users to add and remove themselves.
++  should-proxy-poke
  |=  =update:store
  ^-  ?
  =-  ~&  -  -
  ?:  ?=(%initial -.update)
    %.n
  |^
  =/  role=(unit (unit role-tag))
    (role-for-ship:grp group-id.update src.bol)
  ?~  role
    %.n
  ?~  u.role
    member
  ?-  u.u.role
    %admin      admin
    %moderator  moderator
    %janitor    member
  ==
  ++  member
    ?:  ?=(%add-members -.update)
      =(~(tap in ships.update) ~[src.bol])
    ?:  ?=(%remove-members -.update)
      =(~(tap in ships.update) ~[src.bol])
    %.n
  ++  admin
    !?=(?(%remove-group %add-group) -.update)
  ++  moderator
    ?=  $?  %add-members  %remove-members
            %add-tag      %remove-tag   ==
    -.update
  ++  non-member
    ?&  ?=(%add-members -.update)
        (can-join:grp group-id.update src.bol)
    ==
  --
::
::  +handle-revocations: Handle revoked permissions from a update:store
::
::    If the mutation to the group store has impacted permissions, then kick
::    relevant subscriptions and remove them from our state
::
++  handle-revocations
  |=  =update:store
  ^-  (quip card _state)
  ?+  -.update  [~ state]
  ::
      %remove-group
    =*  group-id  group-id.update
    =/  =path
      (group-id:en-path:store group-id)
    =.  proxied
      (~(del by proxied) group-id)
    :_  state
    [%give %kick [%groups path]~ ~]~
  ::
      %remove-members
    =*  group-id  group-id.update
    =/  =path
      (group-id:en-path:store group-id)
    =/  to-kick=(list ship)
      ~(tap in ships.update)
    =/  subs=(set ship)
      (~(gut by proxied) group-id ~)
    =|  cards=(list card)
    |-
    ?~  to-kick
      [cards state]
    =.  proxied
      (~(del ju proxied) group-id i.to-kick)
    =.  cards
      [[%give %kick [%groups path]~ `i.to-kick] cards]
    $(to-kick t.to-kick)
  ==
:: +start-proxy: Start proxying .path to .who
::
++  start-proxy
  |=  [who=ship =group-id]
  ^-  (quip card _state)
  =.  proxied
    (~(put ju proxied) group-id who)
  [(give-initial group-id) state]
::  +stop-proxy: Stop proxying .path to .who
::
++  stop-proxy
  |=  [who=ship =group-id]
  ^-  (quip card _state)
  =.  proxied
    (~(del ju proxied) group-id who)
  `state
::  +can-join: check if .ship can join .group-id
::
++  can-join
  |=  [=ship =group-id]
  ^-  ?
  =/  =path
    (group-id:en-path:store group-id)
  =*  group-id  u.u-group-id
  =/  scry-path=^path
    (welp [%groups path] /join/[(scot %p ship)])
  (scry-store ? scry-path)
::  +give-initial: give initial state for .group-id
::
::    Must be called in +on-watch. No-ops if the group does not exist yet
++  give-initial
  |=  =group-id
  ^-  (list card)
  =/  =path
    (group-id:en-path:store group-id)
  =/  u-group
    (scry-store (unit group) [%groups path])
  ?~  u-group  ~
  =*  group  u.u-group
  =/  =cage
    :-  %group-update
    !>  ^-  update:store
    [%initial-group group-id group]
  [%give %fact ~ cage]~
++  scry-initial
  |=  =group-id
  ^-  (unit group)
  =/  =path
    (group-id:en-path:store group-id)
  (scry-store (unit group) [%groups path])
::
++  scry-role
  |=  [=group-id =ship]
  %+  scry-store
    (unit role-tag)
  %+  welp
    `path`[%groups (group-id:en-path:store group-id)]
  /role/[(scot %p ship)]
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
::
++  watch-invites
  ^-  card
  [%pass /invites %agent [our.bol %invite-store] %watch /invitatory/groups]
::
++  watch-store
  ^-  card
  [%pass /store %agent [our.bol %group-store] %watch /groups]
--
