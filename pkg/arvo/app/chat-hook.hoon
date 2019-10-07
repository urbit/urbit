::  chat-hook:
::  mirror chat data from foreign to local based on read permissions
::  allow sending chat messages to foreign paths based on write perms
::
/-  *permission-store, *chat-hook
/+  *chat-json
|%
+$  move  [bone card]
::
+$  card
  $%  [%diff [%chat-update chat-update]]
      [%quit ~]
      [%poke wire dock poke]
      [%pull wire dock ~]
      [%peer wire dock path]
  ==
::
+$  state
  $%  [%0 state-zero]
  ==
::
+$  state-zero
  $:  synced=(map path ship)
      boned=(map wire (list bone))
  ==
::
+$  poke
  $%  [%chat-action chat-action]
      [%permission-action permission-action]
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
  ?~  old
    :_  this
    [ost.bol %peer /permissions [our.bol %permission-store] /updates]~
  [~ this(+<+ u.old)]
::
++  poke-noun
  |=  a=*
  ^-  (quip move _this)
  ~&  synced
  ~&  boned
  [~ this]
::
++  poke-json
  |=  jon=json
  ^-  (quip move _this)
  (poke-chat-action (json-to-action jon))
::
++  poke-chat-action
  |=  act=chat-action
  ^-  (quip move _this)
  ?>  ?=(%message -.act)
  ::  local
  :_  this
  ?:  (team:title our.bol src.bol)
    ?.  (~(has by synced) path.act)
      ~
    =/  ship  (~(got by synced) path.act)
    =/  appl  ?:(=(ship our.bol) %chat-store %chat-hook)
    [ost.bol %poke / [ship appl] [%chat-action act]]~
  ::  foreign
  =/  ship  (~(get by synced) path.act)
  ?~  ship
    ~
  ?.  =(u.ship our.bol)
    ~
  ::  scry permissions to check if write is permitted
  ?.  (permitted-scry [(scot %p src.bol) %chat (weld path.act /write)])
    ~
  =:  author.envelope.act  src.bol
      when.envelope.act  now.bol
  ==
  [ost.bol %poke / [our.bol %chat-store] [%chat-action act]]~
::
++  poke-chat-hook-action
  |=  act=chat-hook-action
  ^-  (quip move _this)
  ?-  -.act
      %add-owned
    ?>  (team:title our.bol src.bol)
    =/  chat-path  [%mailbox path.act]
    ?:  (~(has by synced) path.act)
      [~ this]
    =.  synced  (~(put by synced) path.act our.bol)
    :_  (track-bone chat-path)
    %+  weld
      [ost.bol %peer chat-path [our.bol %chat-store] chat-path]~
    (create-permission [%chat path.act] security.act)
  ::
      %add-synced
    ?>  (team:title our.bol src.bol)
    =/  chat-path  [%mailbox (scot %p ship.act) path.act]
    ?:  (~(has by synced) [(scot %p ship.act) path.act])
      [~ this]
    =.  synced  (~(put by synced) [(scot %p ship.act) path.act] ship.act)
    :_  (track-bone chat-path)
    [ost.bol %peer chat-path [ship.act %chat-hook] chat-path]~
  ::
      %remove
    =/  ship  (~(get by synced) path.act)
    ?~  ship
      [~ this]
    ?:  &(=(u.ship our.bol) (team:title our.bol src.bol))
      ::  delete one of our.bol own paths
      :_  %_  this
            synced  (~(del by synced) path.act)
            boned  (~(del by boned) [%mailbox path.act])
          ==
      %-  zing
      :~  (pull-wire [%mailbox path.act])
          (delete-permission [%chat path.act])
          ^-  (list move)
          %+  turn  (prey:pubsub:userlib [%mailbox path.act] bol)
          |=  [=bone *]
          ^-  move
          [bone %quit ~]
      ==
    ?.  |(=(u.ship src.bol) (team:title our.bol src.bol))
      :: if neither ship = source or source = us, do nothing
      [~ this]
    ::  delete a foreign ship's path
    :-  (pull-wire [%mailbox path.act])
    %_  this
      synced  (~(del by synced) path.act)
      boned  (~(del by boned) [%mailbox path.act])
    ==
  ==
::
++  peer-mailbox
  |=  pax=path
  ^-  (quip move _this)
  ?>  ?=([* ^] pax)
  ?>  (~(has by synced) pax)
  ::  scry permissions to check if read is permitted
  ?>  (permitted-scry [(scot %p src.bol) %chat (weld pax /read)])
  =/  box  (chat-scry pax)
  ?~  box  !!
  :_  this
  [ost.bol %diff %chat-update [%create (slav %p i.pax) pax]]~
::
++  diff-permission-update
  |=  [wir=wire diff=permission-update]
  ^-  (quip move _this)
  :_  this
  ?-  -.diff
      %create  ~
      %delete  ~
      %add     (handle-permissions [%add path.diff who.diff])
      %remove  (handle-permissions [%remove path.diff who.diff])
  ==
::
++  handle-permissions
  |=  [kind=?(%add %remove) pax=path who=(set ship)]
  ^-  (list move)
  ?>  ?=([* *] pax)
  ?.  =(%chat i.pax)  ~
  ::  check path to see if this is a %read permission
  ?.  =(%read (snag (dec (lent pax)) `(list @t)`pax))
    ~
  =/  sup
    %-  ~(gas by *(map [ship path] bone))
    %+  turn  ~(tap by sup.bol)
    |=([=bone anchor=[ship path]] [anchor bone])
  %-  zing
  %+  turn  ~(tap in who)
  |=  check-ship=ship
  ?:  (permitted-scry [(scot %p check-ship) pax])
    ::  if ship is permitted to be subscribed, do nothing
    ~
  :: if ship is not permitted, quit their subscription
  =/  mail-path
    (oust [(dec (lent t.pax)) (lent t.pax)] `(list @t)`t.pax)
  =/  bne  (~(get by sup) [check-ship [%mailbox mail-path]])
  ?~(bne ~ [u.bne %quit ~]~)
::
++  diff-chat-update
  |=  [wir=wire diff=chat-update]
  ^-  (quip move _this)
  ?:  (team:title our.bol src.bol)
    (handle-local diff)
  (handle-foreign diff)
::
++  handle-local
  |=  diff=chat-update
  ^-  (quip move _this)
  ?-  -.diff
      %keys    [~ this]
      %config  [~ this]
      %create  [~ this]
      %read    [~ this]
      %delete
    ?.  (~(has by synced) path.diff)
      [~ this]
    :_  this(synced (~(del by synced) path.diff))
    :-  (chat-poke diff)
    [ost.bol %pull [%mailbox path.diff] [our.bol %chat-store] ~]~
  ::
      %message
    :_  this
    %+  turn  (prey:pubsub:userlib [%mailbox path.diff] bol)
    |=  [=bone *]
    ^-  move
    [bone %diff [%chat-update diff]]
  ==
::
++  handle-foreign
  |=  diff=chat-update
  ^-  (quip move _this)
  ?-  -.diff
      %keys    [~ this]
      %config  [~ this]
      %read    [~ this]
      %create
    :_  this
    ?>  ?=([* ^] path.diff)
    =/  shp  (~(get by synced) path.diff)
    ?~  shp  ~
    ?.  =(src.bol u.shp)  ~
    [(chat-poke [%create ship.diff t.path.diff])]~
  ::
      %delete
    ?>  ?=([* ^] path.diff)
    =/  shp  (~(get by synced) path.diff)
    ?~  shp
      [~ this]
    ?.  =(u.shp src.bol)
      [~ this]
    :_  this(synced (~(del by synced) path.diff))
    :-  (chat-poke diff)
    [ost.bol %pull [%mailbox path.diff] [src.bol %chat-hook] ~]~
  ::
      %message
    :_  this
    ?>  ?=([* ^] path.diff)
    =/  shp  (~(get by synced) path.diff)
    ?~  shp  ~
    ?.  =(src.bol u.shp)  ~
    [(chat-poke diff)]~
  ==
::
++  quit
  |=  wir=wire
  ^-  (quip move _this)
  ~&  chat-hook-quit+wir
  ?:  =(wir /permissions)
    :_  this
    [ost.bol %peer /permissions [our.bol %permission-store] /updates]~
  ?>  ?=([* ^] wir)
  ?.  (~(has by synced) t.wir)
    ::  no-op
    [~ this]
  ~&  %chat-hook-resubscribe
  :_  (track-bone wir)
  [ost.bol %peer wir [(slav %p i.t.wir) %chat-hook] wir]~
::
++  reap
  |=  [wir=wire saw=(unit tang)]
  ^-  (quip move _this)
  ?~  saw
    [~ this]
  ?>  ?=(^ wir)
  ~&  %chat-hook-reap
  [((slog u.saw) ~) this(synced (~(del by synced) t.wir))]
::
++  chat-poke
  |=  act=chat-action
  ^-  move
  [ost.bol %poke / [our.bol %chat-store] [%chat-action act]]
::
++  permission-poke
  |=  act=permission-action
  ^-  move
  [ost.bol %poke / [our.bol %permission-store] [%permission-action act]]
::
++  create-permission
  |=  [pax=path sec=chat-security]
  ^-  (list move)
  =/  read-perm   (weld pax /read)
  =/  write-perm  (weld pax /write)
  ?-  sec
      %channel
    :~  (permission-poke (sec-to-perm read-perm %black))
        (permission-poke (sec-to-perm write-perm %black))
    ==
  ::
      %village
    :~  (permission-poke (sec-to-perm read-perm %white))
        (permission-poke (sec-to-perm write-perm %white))
    ==
  ::
      %journal
    :~  (permission-poke (sec-to-perm read-perm %black))
        (permission-poke (sec-to-perm write-perm %white))
    ==
  ::
      %mailbox
    :~  (permission-poke (sec-to-perm read-perm %white))
        (permission-poke (sec-to-perm write-perm %black))
    ==
  ==
::
++  delete-permission
  |=  pax=path
  ^-  (list move)
  =/  read-perm   (weld pax /read)
  =/  write-perm  (weld pax /write)
  :~  (permission-poke [%delete read-perm])
      (permission-poke [%delete write-perm])
  ==
::
++  sec-to-perm
  |=  [pax=path =kind]
  ^-  permission-action
  [%create pax kind *(set ship)]
::
++  chat-scry
  |=  pax=path
  ^-  (unit mailbox)
  =.  pax  ;:(weld /=chat-store/(scot %da now.bol)/mailbox pax /noun)
  .^((unit mailbox) %gx pax)
::
++  permitted-scry
  |=  pax=path
  ^-  ?
  .^(? %gx ;:(weld /=permission-store/(scot %da now.bol)/permitted pax /noun))
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
  =/  shp  (~(get by synced) t.pax)
  ?~  shp  ~
  %+  turn  u.bnd
  |=  =bone
  ^-  move
  ?:  =(u.shp our.bol)
    [bone %pull pax [our.bol %chat-store] ~]
  [bone %pull pax [u.shp %chat-hook] ~]
::
--
