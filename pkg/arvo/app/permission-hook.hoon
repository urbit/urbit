::  permission-hook: mirror remote permissions
::
::    allows mirroring permissions between local and foreign ships.
::    local permission path are exposed according to the permssion paths
::    configured for them as `access-control`.
::
/-  *permission-hook
/+  *permission-json, default-agent, verb, dbug
::
~%  %permission-hook-top  ..is  ~
|%
+$  state
  $%  [%0 state-0]
  ==
::
+$  owner-access  [ship=ship access-control=path]
::
+$  state-0
  $:  synced=(map path owner-access)
      access-control=(map path (set path))
      boned=(map wire (list bone))
  ==
::
+$  card  card:agent:gall
--
::
=|  state-0
=*  state  -
::
%-  agent:dbug
%+  verb  |
^-  agent:gall
=<
  |_  =bowl:gall
  +*  this  .
      do    ~(. +> bowl)
      def   ~(. (default-agent this %|) bowl)
  ::
  ++  on-init  on-init:def
  ++  on-save  !>(state)
  ++  on-load
    |=  old=vase
    ^-  (quip card _this)
    [~ this(state !<(state-0 old))]
  ::
  ++  on-poke
    |=  [=mark =vase]
    ^-  (quip card _this)
    ?+  mark  (on-poke:def mark vase)
        %permission-hook-action
      =^  cards  state
        (handle-permission-hook-action:do !<(permission-hook-action vase))
      [cards this]
    ==
  ::
  ++  on-watch
    |=  =path
    ^-  (quip card _this)
    ?.  ?=([%permission ^] path)  (on-watch:def path)
    =^  cards  state
      (handle-watch-permission:do t.path)
    [cards this]
  ::
  ++  on-agent
    |=  [=wire =sign:agent:gall]
    ^-  (quip card _this)
    ?-  -.sign
      %poke-ack  (on-agent:def wire sign)
    ::
        %fact
      ?.  ?=(%permission-update p.cage.sign)
        (on-agent:def wire sign)
      =^  cards  state
        (handle-permission-update:do wire !<(permission-update q.cage.sign))
      [cards this]
    ::
        %watch-ack
      ?~  p.sign  [~ this]
      ?>  ?=(^ wire)
      :_  this(synced (~(del by synced) t.wire))
      ::NOTE  we could've gotten rejected for permission reasons, so we don't
      ::      try to resubscribe automatically.
      %.  ~
      %-  slog
      :*  leaf+"permission-hook failed subscribe on {(spud t.wire)}"
          leaf+"stack trace:"
          u.p.sign
      ==
    ::
        %kick
      ?>  ?=([* ^] wire)
      ::  if we're not actively using it, we can safely ignore the %kick.
      ::
      ?.  (~(has by synced) t.wire)
        [~ this]
      ::  otherwise, resubscribe.
      ::
      =/  =owner-access  (~(got by synced) t.wire)
      :_  this
      [%pass wire %agent [ship.owner-access %permission-hook] %watch wire]~
    ==
  ::
  ++  on-leave  on-leave:def
  ++  on-peek   on-peek:def
  ++  on-arvo   on-arvo:def
  ++  on-fail   on-fail:def
  --
::
|_  =bowl:gall
++  handle-permission-hook-action
  |=  act=permission-hook-action
  ^-  (quip card _state)
  ?-  -.act
      %add-owned
    ?>  (team:title our.bowl src.bowl)
    ?:  (~(has by synced) owned.act)
      [~ state]
    =.  synced  (~(put by synced) owned.act [our.bowl access.act])
    =.  access-control
      (~(put ju access-control) access.act owned.act)
    =/  perm-path  [%permission owned.act]
    :_  state
    [%pass perm-path %agent [our.bowl %permission-store] %watch perm-path]~
  ::
      %add-synced
    ?>  (team:title our.bowl src.bowl)
    ?:  (~(has by synced) path.act)
      [~ state]
    =.  synced  (~(put by synced) path.act [ship.act ~])
    =/  perm-path  [%permission path.act]
    :_  state
    [%pass perm-path %agent [ship.act %permission-hook] %watch perm-path]~
  ::
      %remove
    =/  owner-access=(unit owner-access)
      (~(get by synced) path.act)
    ?~  owner-access
      [~ state]
    ::  if we own it, and it's us asking,
    ::
    ?:  ?&  =(ship.u.owner-access our.bowl)
            (team:title our.bowl src.bowl)
        ==
      ::  delete the permission path and its subscriptions from this hook.
      ::
      :-  :-  [%give %kick [%permission path.act]~ ~]
          (leave-permission path.act)
      %_  state
        synced  (~(del by synced) path.act)
      ::
          access-control
        (~(del by access-control) access-control.u.owner-access)
      ==
    ::  else, if either source = ship or source = us,
    ::
    ?:  |(=(ship.u.owner-access src.bowl) (team:title our.bowl src.bowl))
      ::  delete a foreign ship's path.
      ::
      :-  (leave-permission path.act)
      %_  state
        synced  (~(del by synced) path.act)
        boned  (~(del by boned) [%permission path.act])
      ==
    ::  else, ignore action entirely.
    ::
    [~ state]
  ==
::
++  handle-watch-permission
  |=  =path
  ^-  (quip card _state)
  =/  =owner-access  (~(got by synced) path)
  ?>  =(our.bowl ship.owner-access)
  ::  scry permissions to check if subscriber is allowed
  ::
  ?>  (permitted src.bowl access-control.owner-access)
  =/  pem  (permission-scry path)
  :_  state
  [%give %fact ~ %permission-update !>([%create path pem])]~
::
++  handle-permission-update
  |=  [=wire diff=permission-update]
  ^-  (quip card _state)
  ?:  (team:title our.bowl src.bowl)
    (handle-local diff)
  (handle-foreign diff)
::
++  handle-local
  |=  diff=permission-update
  ^-  (quip card _state)
  ?-  -.diff
      %create  [~ state]
      %add     (change-local-permission %add [path who]:diff)
      %remove  (change-local-permission %remove [path who]:diff)
    ::
      %delete
    ?.  (~(has by synced) path.diff)
      [~ state]
    =/  control=(unit path)
      =+  (~(got by synced) path.diff)
      ?.  =(our.bowl ship)  ~
      `access-control
    :_  %_  state
          synced          (~(del by synced) path.diff)
          access-control  ?~  control  access-control
                          (~(del ju access-control) u.control path.diff)
        ==
    :_  ~
    :*  %pass
        [%permission path.diff]
        %agent
        [our.bowl %permission-store]
        [%leave ~]
    ==
  ==
::
++  change-local-permission
  |=  [kind=?(%add %remove) pax=path who=(set ship)]
  ^-  (quip card _state)
  :_  state
  :-  ?-  kind
        %add     (update-subscribers [%permission pax] [%add pax who])
        %remove  (update-subscribers [%permission pax] [%remove pax who])
      ==
  =/  access-paths=(unit (set path))  (~(get by access-control) pax)
  ::  check if this path changes the access permissions for other paths
  ?~  access-paths  ~
  (quit-subscriptions kind pax who u.access-paths)
::
++  handle-foreign
  |=  diff=permission-update
  ^-  (quip card _state)
  ?-  -.diff
      ?(%create %add %remove)
    (change-foreign-permission path.diff diff)
  ::
      %delete
    ?>  ?=([* ^] path.diff)
    =/  owner-access=(unit owner-access)
      (~(get by synced) path.diff)
    ?~  owner-access
      [~ state]
    ?.  =(ship.u.owner-access src.bowl)
      [~ state]
    :_  state(synced (~(del by synced) path.diff))
    :~  (permission-poke diff)
      ::
        :*  %pass
            [%permission path.diff]
            %agent
            [src.bowl %permission-hook]
            [%leave ~]
        ==
    ==
  ==
::
++  change-foreign-permission
  |=  [=path diff=permission-update]
  ^-  (quip card _state)
  ?>  ?=([* ^] path)
  =/  owner-access=(unit owner-access)
    (~(get by synced) path)
  :_  state
  ?~  owner-access  ~
  ?.  =(src.bowl ship.u.owner-access)  ~
  [(permission-poke diff)]~
::
++  quit-subscriptions
  |=  $:  kind=?(%add %remove)
          perm-path=path
          who=(set ship)
          access-paths=(set path)
      ==
  ^-  (list card)
  =/  perm  (permission-scry perm-path)
  ::  if the change resolves to "allow",
  ::
  ?.  ?|  ?&(=(%black kind.perm) =(%add kind))
          ?&(=(%white kind.perm) =(%remove kind))
      ==
    ::  do nothing.
    ~
  ::  else, it resolves to "deny"/"ban".
  ::  kick subscriptions for all ships, at all affected paths.
  ::
  %-  zing
  %+  turn  ~(tap in who)
  |=  check-ship=ship
  ^-  (list card)
  %+  turn  ~(tap in access-paths)
  |=  access-path=path
  [%give %kick [%permission access-path]~ `check-ship]
::
++  permission-scry
  |=  pax=path
  ^-  permission
  =.  pax  ;:(weld /=permission-store/(scot %da now.bowl)/permission pax /noun)
  (need .^((unit permission) %gx pax))
::
++  permitted
  |=  [who=ship =path]
  .^  ?
    %gx
    (scot %p our.bowl)
    %permission-store
    (scot %da now.bowl)
    %permitted
    (scot %p src.bowl)
    (snoc path %noun)
  ==
::
++  permission-poke
  |=  act=permission-action
  ^-  card
  :*  %pass
      /permission-action
      %agent
      [our.bowl %permission-store]
      %poke
      %permission-action
      !>(act)
  ==
::
++  update-subscribers
  |=  [=path upd=permission-update]
  ^-  card
  [%give %fact ~[path] %permission-update !>(upd)]
::
++  leave-permission
  |=  =path
  ^-  (list card)
  =/  owner-access=(unit owner-access)
    (~(get by synced) path)
  ?~  owner-access  ~
  :_  ~
  =/  perm-path  [%permission path]
  ?:  =(ship.u.owner-access our.bowl)
    [%pass perm-path %agent [our.bowl %permission-store] %leave ~]
  [%pass perm-path %agent [ship.u.owner-access %permission-hook] %leave ~]
--
