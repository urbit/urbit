::  chat-hook:
::  mirror chat data from foreign to local based on read permissions
::  allow sending chat messages to foreign paths based on write perms
::
/-  *permission-store, *chat-hook, *invite-store
/+  *chat-json, default-agent, verb
|%
+$  card  card:agent:gall
::
+$  versioned-state
  $%  state-zero
  ==
::
+$  state-zero
  $:  %0
      synced=(map path ship)
      invite-created=_|
      allow-history=(map path ?)
  ==
::
+$  poke
  $%  [%chat-action chat-action]
      [%permission-action permission-action]
      [%invite-action invite-action]
      [%chat-view-action chat-view-action]
  ==
::
+$  fact
  $%  [%chat-update chat-update]
  ==
--
=|  state-zero
=*  state  -
%+  verb  |
^-  agent:gall
=<
  |_  bol=bowl:gall
  +*  this       .
      chat-core  +>
      cc         ~(. chat-core bol)
      def        ~(. (default-agent this %|) bol)
  ::
  ++  on-init
    ^-  (quip card _this)
    :_  this(invite-created %.y)
    :~  (invite-poke:cc [%create /chat])
        [%pass /invites %agent [our.bol %invite-store] %watch /invitatory/chat]
        [%pass /permissions %agent [our.bol %permission-store] %watch /updates]
    ==
  ++  on-save   !>(state)
  ++  on-load
    |=  old=vase
    `this(state !<(state-zero old))
  ::
  ++  on-poke
    |=  [=mark =vase]
    ^-  (quip card _this)
    =^  cards  state
      ?+  mark  (on-poke:def mark vase)
        %json              (poke-json:cc !<(json vase))
        %chat-action       (poke-chat-action:cc !<(chat-action vase))
        %chat-hook-action  (poke-chat-hook-action:cc !<(chat-hook-action vase))
      ==
    [cards this]
  ::
  ++  on-watch
    |=  =path
    ^-  (quip card _this)
    ?+  path          (on-watch:def path)
        [%backlog *]  [(watch-backlog:cc t.path) this]
        [%mailbox *]  [(watch-mailbox:cc t.path) this]
    ==
  ::
  ++  on-agent
    |=  [=wire =sign:agent:gall]
    ^-  (quip card _this)
    ?+  -.sign  (on-agent:def wire sign)
        %watch-ack
      =^  cards  state
        (watch-ack:cc wire p.sign) 
      [cards this]
    ::
        %kick
      =^  cards  state
        (kick:cc wire)
      [cards this]
    ::
        %fact
      ?+  p.cage.sign  (on-agent:def wire sign)
          %chat-update
        =^  cards  state
          (fact-chat-update:cc wire !<(chat-update q.cage.sign))
        [cards this]
      ::
          %invite-update
        =^  cards  state
          (fact-invite-update:cc wire !<(invite-update q.cage.sign))
        [cards this]
      ::
          %permission-update
        =^  cards  state
          (fact-permission-update:cc wire !<(permission-update q.cage.sign))
        [cards this]
      ==
    ==
  ::
  ++  on-leave  on-leave:def
  ++  on-peek   on-peek:def
  ++  on-arvo   on-arvo:def
  ++  on-fail   on-fail:def
  --
::
::
|_  bol=bowl:gall
::
++  poke-json
  |=  jon=json
  ^-  (quip card _state)
  (poke-chat-action (json-to-action jon))
::
++  poke-chat-action
  |=  act=chat-action
  ^-  (quip card _state)
  ?>  ?=(%message -.act)
  ::  local
  :_  state
  ?:  (team:title our.bol src.bol)
    ?.  (~(has by synced) path.act)
      ~
    =/  ship  (~(got by synced) path.act)
    =/  appl  ?:(=(ship our.bol) %chat-store %chat-hook)
    [%pass / %agent [ship appl] %poke %chat-action !>(act)]~
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
  [%pass / %agent [our.bol %chat-store] %poke %chat-action !>(act)]~
::
++  poke-chat-hook-action
  |=  act=chat-hook-action
  ^-  (quip card _state)
  ?-  -.act
      %add-owned
    ?>  (team:title our.bol src.bol)
    =/  chat-path  [%mailbox path.act]
    ?:  (~(has by synced) path.act)
      [~ state]
    =:  synced  (~(put by synced) path.act our.bol)
        allow-history  (~(put by allow-history) path.act allow-history.act)
    ==
    :_  state
    %+  weld
      [%pass chat-path %agent [our.bol %chat-store] %watch chat-path]~
    (create-permission [%chat path.act] security.act)
  ::
      %add-synced
    ?>  (team:title our.bol src.bol)
    ?:  (~(has by synced) [(scot %p ship.act) path.act])
      [~ state]
    =.  synced  (~(put by synced) [(scot %p ship.act) path.act] ship.act)
    ?.  ask-history.act
      =/  chat-path  [%mailbox (scot %p ship.act) path.act]
      :_  state
      [%pass chat-path %agent [ship.act %chat-hook] %watch chat-path]~
    ::  TODO: only ask for backlog from previous point
    =/  chat-history  [%backlog (scot %p ship.act) (weld path.act /0)]
    :_  state
    [%pass chat-history %agent [ship.act %chat-hook] %watch chat-history]~
  ::
      %remove
    =/  ship  (~(get by synced) path.act)
    ?~  ship
      [~ state]
    ?:  &(=(u.ship our.bol) (team:title our.bol src.bol))
      ::  delete one of our.bol own paths
      :_  state(synced (~(del by synced) path.act))
      %-  zing
      :~  (pull-wire [%backlog (weld path.act /0)])
          (pull-wire [%mailbox path.act])
          (delete-permission [%chat path.act])
          [%give %kick `[%mailbox path.act] ~]~
      ==
    ?.  |(=(u.ship src.bol) (team:title our.bol src.bol))
      ::  if neither ship = source or source = us, do nothing
      [~ state]
    ::  delete a foreign ship's path
    :-  (pull-wire [%mailbox path.act])
    state(synced (~(del by synced) path.act))
  ==
::
++  watch-mailbox
  |=  pax=path
  ^-  (list card)
  ?>  ?=(^ pax)
  ?>  (~(has by synced) pax)
  ::  scry permissions to check if read is permitted
  ?>  (permitted-scry [(scot %p src.bol) %chat (weld pax /read)])
  =/  box  (chat-scry pax)
  ?~  box  !!
  [%give %fact ~ %chat-update !>([%create (slav %p i.pax) pax])]~
::
++  watch-backlog
  |=  pax=path
  ^-  (list card)
  ?>  ?=(^ pax)
  =/  last  (dec (lent pax))
  =/  backlog-start=(unit @ud)
    %+  rush
      (snag last `(list @ta)`pax)
    dem:ag
  =/  pas  `path`(oust [last 1] `(list @ta)`pax)
  ?>  ?=([* ^] pas)
  ?>  (~(has by synced) pas)
  ::  scry permissions to check if read is permitted
  ?>  (permitted-scry [(scot %p src.bol) %chat (weld pas /read)])
  =/  box  (chat-scry pas)
  ?~  box  !!
  :-  [%give %fact ~ %chat-update !>([%create (slav %p i.pas) pas])]
  %-  zing
  :~
    ?:  ?&(?=(^ backlog-start) (~(got by allow-history) pas))
      (paginate-messages pas u.box u.backlog-start)
    ~
    [%give %kick `[%backlog pax] `src.bol]~
  ==
::
++  paginate-messages
  |=  [=path =mailbox start=@ud]
  ^-  (list card)
  =/  cards=(list card)  ~
  =/  end  (lent envelopes.mailbox)
  ?:  |((gte start end) =(end 0))
    cards
  =.  envelopes.mailbox  (slag start `(list envelope)`envelopes.mailbox)
  |-  ^-  (list card)
  ?~  envelopes.mailbox
    cards
  ?:  (lte end 5.000)
    =.  cards
      %+  snoc  cards
      %-  messages-fact
      [path start (lent envelopes.mailbox) envelopes.mailbox]
    $(envelopes.mailbox ~)
  =.  cards
    %+  snoc  cards
    %-  messages-fact
    :^  path  start
    (add start 5.000)
    (scag 5.000 `(list envelope)`envelopes.mailbox)
  =:  start  (add start 5.000)
      end    (sub end 5.000)
  ==
  $(envelopes.mailbox (slag 5.000 `(list envelope)`envelopes.mailbox))
::
++  fact-invite-update
  |=  [wir=wire fact=invite-update]
  ^-  (quip card _state)
  ?+  -.fact
    [~ state]
  ::
      %accepted
    =/  ask-history
      ?~  (chat-scry [(scot %p ship.invite.fact) path.invite.fact])
        %.y
      %.n
    :_  state
    [(chat-view-poke [%join ship.invite.fact path.invite.fact ask-history])]~
  ==
::
++  fact-permission-update
  |=  [wir=wire fact=permission-update]
  ^-  (quip card _state)
  :_  state
  ?-  -.fact
      %create  ~
      %delete  ~
      %add     (handle-permissions [%add path.fact who.fact])
      %remove  (handle-permissions [%remove path.fact who.fact])
  ==
::
++  handle-permissions
  |=  [kind=?(%add %remove) pax=path who=(set ship)]
  ^-  (list card)
  ?>  ?=([* *] pax)
  ?.  =(%chat i.pax)  ~
  ::  check path to see if this is a %read permission
  ?.  =(%read (snag (dec (lent pax)) `(list @t)`pax))
    ~
  %-  zing
  %+  turn  ~(tap in who)
  |=  =ship
  ?:  (permitted-scry [(scot %p ship) pax])
    ~
  ::  if ship is not permitted, kick their subscription
  =/  mail-path
    (oust [(dec (lent t.pax)) (lent t.pax)] `(list @t)`t.pax)
  [%give %kick `[%mailbox mail-path] `ship]~
::
++  fact-chat-update
  |=  [wir=wire fact=chat-update]
  ^-  (quip card _state)
  ?:  (team:title our.bol src.bol)
    (handle-local fact)
  (handle-foreign fact)
::
++  handle-local
  |=  fact=chat-update
  ^-  (quip card _state)
  ?-  -.fact
      %keys      [~ state]
      %read      [~ state]
      %config    [~ state]
      %create    [~ state]
      %delete
    ?.  (~(has by synced) path.fact)
      [~ state]
    :_  state(synced (~(del by synced) path.fact))
    [%pass [%mailbox path.fact] %agent [our.bol %chat-store] %leave ~]~
  ::
      %message
    :_  state
    [%give %fact `[%mailbox path.fact] %chat-update !>(fact)]~
  ::
      %messages
    :_  state
    [%give %fact `[%mailbox path.fact] %chat-update !>(fact)]~
  ==
::
++  handle-foreign
  |=  fact=chat-update
  ^-  (quip card _state)
  ?-  -.fact
      %keys    [~ state]
      %read    [~ state]
      %config  [~ state]
      %create
    :_  state
    ?>  ?=([* ^] path.fact)
    =/  shp  (~(get by synced) path.fact)
    ?~  shp  ~
    ?.  =(src.bol u.shp)  ~
    [(chat-poke [%create ship.fact t.path.fact])]~
  ::
      %delete
    ?>  ?=([* ^] path.fact)
    =/  shp  (~(get by synced) path.fact)
    ?~  shp
      [~ state]
    ?.  =(u.shp src.bol)
      [~ state]
    :_  state(synced (~(del by synced) path.fact))
    :-  (chat-poke [%delete path.fact])
    [%pass [%mailbox path.fact] %agent [src.bol %chat-hook] %leave ~]~
  ::
      %message
    :_  state
    ?>  ?=([* ^] path.fact)
    =/  shp  (~(get by synced) path.fact)
    ?~  shp  ~
    ?.  =(src.bol u.shp)  ~
    [(chat-poke [%message path.fact envelope.fact])]~
  ::
      %messages
    :_  state
    ?>  ?=([* ^] path.fact)
    =/  shp  (~(get by synced) path.fact)
    ?~  shp  ~
    ?.  =(src.bol u.shp)  ~
    [(chat-poke [%messages path.fact envelopes.fact])]~
  ==
::
++  kick
  |=  wir=wire
  ^-  (quip card _state)
  ~&  chat-hook-kick+wir
  ?:  =(wir /permissions)
    :_  state
    [%pass /permissions %agent [our.bol %permission-store] %watch /updates]~
  ::
  ?:  ?=([%mailbox @ *] wir)
    ~&  mailbox-kick+wir
    ?.  (~(has by synced) t.wir)
      ::  no-op
      [~ state]
    ~&  %chat-hook-resubscribe
    =/  =ship  (~(got by synced) t.wir)
    =/  mailbox=(unit mailbox)  (chat-scry t.wir)
    =/  chat-history
      %+  welp  backlog+t.wir
      ?~  mailbox
        /0
      /(scot %ud (lent envelopes.u.mailbox))
    :_  state
    [%pass chat-history %agent [ship %chat-hook] %watch chat-history]~
  ::
  ?:  ?=([%backlog @ *] wir)
    ~&  backlog-kick+wir
    =/  pax  `path`(oust [(dec (lent t.wir)) 1] `(list @ta)`t.wir)
    =/  mailbox=(unit mailbox)  (chat-scry pax)
    =.  pax  ?~(mailbox wir [%mailbox pax])
    ~&  chat-hook-resubscribe+pax
    :_  state
    [%pass pax %agent [(slav %p i.t.wir) %chat-hook] %watch pax]~
  !!
::
++  watch-ack
  |=  [wir=wire saw=(unit tang)]
  ^-  (quip card _state)
  ?~  saw
    [~ state]
  ?>  ?=(^ wir)
  :_  state(synced (~(del by synced) t.wir))
  %.  ~
  %-  slog
  :*  leaf+"chat-hook failed subscribe on {(spud t.wir)}"
      leaf+"stack trace:"
      u.saw
  ==
::
++  chat-poke
  |=  act=chat-action
  ^-  card
  [%pass / %agent [our.bol %chat-store] %poke %chat-action !>(act)]
::
++  chat-view-poke
  |=  act=chat-view-action
  ^-  card
  [%pass / %agent [our.bol %chat-view] %poke %chat-view-action !>(act)]
::
++  permission-poke
  |=  act=permission-action
  ^-  card
  [%pass / %agent [our.bol %permission-store] %poke %permission-action !>(act)]
::
++  invite-poke
  |=  act=invite-action
  ^-  card
  [%pass / %agent [our.bol %invite-store] %poke %invite-action !>(act)]
::
++  messages-fact
  |=  [=path start=@ud end=@ud envelopes=(list envelope)]
  ^-  card
  [%give %fact ~ %chat-update !>([%messages path start end envelopes])]
::
++  create-permission
  |=  [pax=path sec=rw-security]
  ^-  (list card)
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
  ^-  (list card)
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
++  invite-scry
  |=  uid=serial
  ^-  (unit invite)
  =/  pax  /=invite-store/(scot %da now.bol)/invite/chat/(scot %uv uid)/noun
  .^((unit invite) %gx pax)
::
++  permitted-scry
  |=  pax=path
  ^-  ?
  .^(? %gx ;:(weld /=permission-store/(scot %da now.bol)/permitted pax /noun))
::
++  pull-wire
  |=  pax=path
  ^-  (list card)
  ?>  ?=(^ pax)
  =/  shp  (~(get by synced) t.pax)
  ?~  shp  ~
  ?:  =(u.shp our.bol)
    [%pass pax %agent [our.bol %chat-store] %leave ~]~
  [%pass pax %agent [u.shp %chat-hook] %leave ~]~
--
