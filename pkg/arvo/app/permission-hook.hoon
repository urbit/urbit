::  permission-hook: allows mirroring permissions between local and foreign
::  ships. access control to an owned permission path is specified by the
::  access-control path.
::
/-  *permission-hook
/+  *permission-json
|%
+$  move  [bone card]
::
+$  card
  $%  [%diff [%permission-update permission-update]]
      [%quit ~]
      [%poke wire dock [%permission-action permission-action]]
      [%pull wire dock ~]
      [%peer wire dock path]
  ==
::
+$  state
  $%  [%0 state-zero]
  ==
::
+$  owner-access  [ship=ship access-control=path]
::
+$  state-zero
  $:  synced=(map path owner-access)
      access-control=(map path (set path))
      boned=(map wire (list bone))
  ==
::
--
::
|_  [bol=bowl:gall state]
::
++  this  .
::
++  prep
  |=  old=(unit state)
  ^-  (quip move _this)
  [~ ?~(old this this(+<+ u.old))]
::
++  poke-permission-hook-action
  |=  act=permission-hook-action
  ^-  (quip move _this)
  ?-  -.act
      %add-owned
    ?>  (team:title our.bol src.bol)
    ?:  (~(has by synced) owned.act)
      [~ this]
    =.  synced  (~(put by synced) owned.act [our.bol access.act])
    =/  access-paths
      ?.  (~(has by access-control) access.act)
        [owned.act ~ ~]
      (~(put in (~(got by access-control) access.act)) owned.act)
    =.  access-control
      (~(put by access-control) access.act access-paths)
    =/  perm-path  [%permission owned.act]
    :_  (track-bone perm-path)
    [ost.bol %peer perm-path [our.bol %permission-store] perm-path]~
  ::
      %add-synced
    ?>  (team:title our.bol src.bol)
    ?:  (~(has by synced) path.act)
      [~ this]
    =.  synced  (~(put by synced) path.act [ship.act ~])
    =/  perm-path  [%permission path.act]
    :_  (track-bone perm-path)
    [ost.bol %peer perm-path [ship.act %permission-hook] perm-path]~
  ::
      %remove
    =/  owner-access=(unit owner-access)  (~(get by synced) path.act)
    ?~  owner-access
      [~ this]
    ?:  &(=(ship.u.owner-access our.bol) (team:title our.bol src.bol))
      ::  delete one of our.bol own paths
      :_  %_  this
              synced  (~(del by synced) path.act)
              boned  (~(del by boned) [%permission path.act])
          ::
              access-control
            (~(del by access-control) access-control.u.owner-access)
          ==
      %-  zing
      :~  (pull-wire [%permission path.act])
          ^-  (list move)
          %+  turn  (prey:pubsub:userlib [%permission path.act] bol)
          |=  [=bone *]
          [bone %quit ~]
      ==
    ?.  |(=(ship.u.owner-access src.bol) (team:title our.bol src.bol))
      :: if neither ship = source or source = us, do nothing
      [~ this]
    ::  delete a foreign ship's path
    :_  %_  this
            synced  (~(del by synced) path.act)
            boned  (~(del by boned) [%permission path.act])
        ==
    (pull-wire [%permission path.act])
  ==
::
++  peer-permission
  |=  pax=path
  ^-  (quip move _this)
  ?>  ?=([* ^] pax)
  =/  =owner-access  (~(got by synced) pax)
  ?>  =(our.bol ship.owner-access)
  ::  scry permissions to check if subscriber is allowed
  ?>  (permitted-scry (scot %p src.bol) access-control.owner-access)
  =/  pem  (permission-scry pax)
  :_  this
  [ost.bol %diff %permission-update [%create pax pem]]~
::
++  diff-permission-update
  |=  [wir=wire diff=permission-update]
  ^-  (quip move _this)
  ?:  (team:title our.bol src.bol)
    (handle-local diff)
  (handle-foreign diff)
::
++  handle-local
  |=  diff=permission-update
  ^-  (quip move _this)
  ?-  -.diff
      %create  [~ this]
      %add     (change-local-permission [%add path.diff who.diff]) 
      %remove  (change-local-permission [%remove path.diff who.diff])
    ::
      %delete
    ?.  (~(has by synced) path.diff)
      [~ this]
    :_  this(synced (~(del by synced) path.diff))
    [ost.bol %pull [%permission path.diff] [our.bol %permission-store] ~]~
  ==
::
++  change-local-permission
  |=  [kind=?(%add %remove) pax=path who=(set ship)]
  ^-  (quip move _this)
  :_  this
  %+  weld
    ?-  kind
      %add     (update-subscribers [%permission pax] [%add pax who])
      %remove  (update-subscribers [%permission pax] [%remove pax who])
    ==
  =/  access-paths=(unit (set path))  (~(get by access-control) pax)
  ::  check if this path changes the access permissions for other paths
  ?~  access-paths
    ~
  (quit-subscriptions kind pax who u.access-paths)
::
++  handle-foreign
  |=  diff=permission-update
  ^-  (quip move _this)
  ?-  -.diff
      %create  (change-foreign-permission path.diff diff)
      %add     (change-foreign-permission path.diff diff)
      %remove  (change-foreign-permission path.diff diff)
  ::
      %delete
    ?>  ?=([* ^] path.diff)
    =/  owner-access=(unit owner-access)  (~(get by synced) path.diff)
    ?~  owner-access
      [~ this]
    ?.  =(ship.u.owner-access src.bol)
      [~ this]
    :_  this(synced (~(del by synced) path.diff))
    :~  (permission-poke diff)
        [ost.bol %pull [%permission path.diff] [src.bol %permission-hook] ~]
    ==
  ==
::
++  change-foreign-permission
  |=  [pax=path diff=permission-update]
  ^-  (quip move _this)
  ?>  ?=([* ^] pax)
  =/  owner-access=(unit owner-access)  (~(get by synced) pax)
  :_  this
  ?~  owner-access  ~
  ?.  =(src.bol ship.u.owner-access)  ~
  [(permission-poke diff)]~
::
++  quit-subscriptions
  |=  [kind=?(%add %remove) pax=path who=(set ship) access-paths=(set path)]
  ^-  (list move)
  =/  perm  (permission-scry pax)
  ?.  ?|
        ?&(=(kind.perm %black) =(kind %add))
        ?&(=(kind.perm %white) =(kind %remove))
      ==
    ::  if allow, do nothing
    ~    
  =/  sup
    %-  ~(gas by *(map [ship path] bone))
    %+  turn  ~(tap by sup.bol)
    |=([=bone anchor=[ship path]] [anchor bone])
  ::  if ban, iterate through
  ::  all ships that have been banned
  ::  and all affected paths that have had their permissions changed
  ::  then quit their subscriptions
  ::
  %-  zing
  %+  turn  ~(tap in who)
  |=  check-ship=ship
  ^-  (list move)
  %-  zing
  %+  turn  ~(tap in access-paths)
  |=  access-path=path
  ^-  (list move)
  =/  bne  (~(get by sup) [check-ship [%permission access-path]])
  ?~(bne ~ [u.bne %quit ~]~)
::
++  quit
  |=  wir=wire
  ^-  (quip move _this)
  ~&  permission-hook-quit+wir
  ?>  ?=([* ^] wir)
  ?.  (~(has by synced) t.wir)
    ::  no-op
    [~ this]
  =/  =owner-access  (~(got by synced) t.wir)
  ~&  %permission-hook-resubscribe
  :_  (track-bone wir)
  [ost.bol %peer wir [ship.owner-access %permission-hook] wir]~
::
++  reap
  |=  [wir=wire saw=(unit tang)]
  ^-  (quip move _this)
  ?~  saw
    [~ this]
  ?>  ?=(^ wir)
  :_  this(synced (~(del by synced) t.wir))
  %.  ~
  %-  slog
  :*  leaf+"permission-hook failed subscribe on {(spud t.wir)}"
      leaf+"stack trace:"
      u.saw
  ==
::
++  permission-scry
  |=  pax=path
  ^-  permission
  =.  pax  ;:(weld /=permission-store/(scot %da now.bol)/permission pax /noun)
  (need .^((unit permission) %gx pax))
::
++  permitted-scry
  |=  pax=path
  ^-  ?
  .^(? %gx ;:(weld /=permission-store/(scot %da now.bol)/permitted pax /noun))
::
++  permission-poke
  |=  act=permission-action
  ^-  move
  [ost.bol %poke / [our.bol %permission-store] [%permission-action act]]
::
++  update-subscribers
  |=  [pax=path upd=permission-update]
  ^-  (list move)
  %+  turn  (prey:pubsub:userlib pax bol)
  |=  [=bone *]
  [bone %diff %permission-update upd]
::
++  track-bone
  |=  wir=wire
  ^+  this
  =/  bnd  (~(get by boned) wir)
  ?^  bnd
    this(boned (~(put by boned) wir (snoc u.bnd ost.bol)))
  this(boned (~(put by boned) wir [ost.bol]~))
::
++  pull-wire
  |=  pax=path
  ^-  (list move)
  ?>  ?=([* ^] pax)
  =/  bnd  (~(get by boned) pax)
  ?~  bnd  ~
  =/  owner-access=(unit owner-access)  (~(get by synced) t.pax)
  ?~  owner-access  ~
  %+  turn  u.bnd
  |=  =bone
  ?:  =(ship.u.owner-access our.bol)
    [bone %pull pax [our.bol %permission-store] ~]
  [bone %pull pax [ship.u.owner-access %permission-hook] ~]
::
--
