::  chat-hook [landscape]:
::  mirror chat data from foreign to local based on read
::  allow sending chat messages to foreign paths based on write perms
::
/-  inv=invite-store, *metadata-store, *group-store,
    hook=chat-hook, view=chat-view, *group,
    push-hook, pull-hook
/+  default-agent, verb, dbug, store=chat-store, group-store, grpl=group,
    resource, graph-store, *migrate
~%  %chat-hook-top  ..part  ~
|%
+$  card  card:agent:gall
::
+$  versioned-state
  $%  state-0
      state-1
      state-2
      state-3
      state-4
      state-5
      state-6
      state-7
      state-8
      state-9
      state-10
      state-11
  ==
::
+$  migration-state
  (map resource @ud)
::
+$  state-11  [%11 state-base migrate=migration-state]
+$  state-10  [%10 state-base]
+$  state-9  [%9 state-base]
+$  state-8  [%8 state-base]
+$  state-7  [%7 state-base]
+$  state-6  [%6 state-base]
+$  state-5  [%5 state-base]
+$  state-4  [%4 state-base]
+$  state-3  [%3 state-base]
+$  state-2  [%2 state-base]
::
+$  state-1
  $:  %1
      loaded-cards=*
      state-base
  ==
+$  state-0  [%0 state-base]
+$  state-base
  $:  =synced:hook
      invite-created=_|
      allow-history=(map path ?)
  ==
::
+$  poke
  $%  [%chat-action action:store]
      [%invite-action action:inv]
      [%chat-view-action action:view]
  ==
::
+$  fact
  $%  [%chat-update update:store]
  ==
--
=|  state-11
=*  state  -
::
%-  agent:dbug
%+  verb  |
^-  agent:gall
=<
  ~%  %chat-hook-agent-core  ..poke-json  ~
  |_  bol=bowl:gall
  +*  this       .
      chat-core  +>
      cc         ~(. chat-core bol)
      def        ~(. (default-agent this %|) bol)
  ::
  ++  on-init
    ^-  (quip card _this)
    :_  this(invite-created %.y)
    [(invite-poke:cc [%create %chat]) ~]
  ++  on-save   !>(state)
  ++  on-load
    |=  old-vase=vase
    ^-  (quip card _this)
    |^
    =/  old  !<(versioned-state old-vase)
    =|  cards=(list card)
    |-
    ?:  ?=(%11 -.old)
      [cards this(state old)]
    ?:  ?=(%10 -.old)
      =.  cards
        :_  cards
        =-  [%pass /self-poke %agent [our.bol %chat-hook] %poke -]
        noun+!>(%migrate-graph)
      $(old [%11 +.old ~])
    ::
    ?:  ?=(%9 -.old)
      =.  cards
        :_  cards
        [%pass /self-poke %agent [our.bol %chat-hook] %poke %noun !>(%run-upg9)]
      $(-.old %10)
    ?:  ?=(%8 -.old)
      $(-.old %9)
    ?:  ?=(%7 -.old)
      =.  cards
        :_  cards
        [%pass /self-poke %agent [our.bol %chat-hook] %poke %noun !>(%run-upg7)]
      $(-.old %8)
    ?:  ?=(%6 -.old)
      =.  cards
        %+  weld  cards
        ^-  (list card)
        [%pass /s %agent [our.bol %chat-hook] %poke %noun !>(%fix-out-of-sync)]~
      $(-.old %7)
    ?:  ?=(?(%3 %4 %5) -.old)
      =.  cards
        %+  weld  cards
        ^-  (list card)
        [%pass /pokeme %agent [our.bol %chat-hook] %poke %noun !>(%fix-dm)]~
      $(-.old %6)
    ?:  ?=(%2 -.old)
      =.  cards
        %+  weld  cards
        :~  watch-groups:cc
        ==
      =^  new-cards=(list card)  old
        =|  crds=(list card)
        =/  syncs
          ~(tap by synced.old)
        |-
        ?~  syncs
          [crds old]
        =/  [pax=path =ship]
          i.syncs
        ?>  ?=(^ pax)
        ?.  =('~' i.pax)
          $(syncs t.syncs)
        =/  new-path=path
          t.pax
        =.  synced.old
          (~(del by synced.old) pax)
        ?.  =(ship our.bol)
          =.  synced.old
            (~(put by synced.old) new-path ship)
          $(syncs t.syncs)
        =/  history=?
          (~(gut by allow-history.old) pax %.y)
        =.  allow-history.old
          (~(del by allow-history.old) pax)
        =.  allow-history.old
          (~(put by allow-history.old) new-path history)
        =.  crds
          %+  weld  crds
          :-  (add-owned new-path history)
          (kick-old-subs pax)
        $(syncs t.syncs)
      =.  cards
        (weld cards new-cards)
      $(-.old %3)
    ::
    ?:  ?=(%1 -.old)
      =.  cards
        %+  welp  cards
        ^-  (list card)
        %+  murn  ~(tap by wex.bol)
        |=  [[=wire =ship =term] *]
        ^-  (unit card)
        ?.  &(?=([%mailbox *] wire) =(our.bol ship) =(%chat-store term))
          ~
        `[%pass wire %agent [our.bol %chat-store] %leave ~]
      $(old [%2 +>.old])
    ::  path structure ugprade logic
    ::
    =/  keys=(set path)  (scry:cc (set path) %chat-store /keys)
    %=    $
        -.old  %2
      ::
        cards
      %-  zing
      ^-  (list (list card))
      (turn ~(tap in keys) generate-cards)
    ==
    ::
    ++  scry-for
      |*  [=mold app=term =path]
      .^  mold
        %gx
        (scot %p our.bol)
        app
        (scot %da now.bol)
        (snoc `^path`path %noun)
      ==
    ::
    ++  kick-old-subs
      |=  old-path=path
      ^-  (list card)
      ?>  ?=(^ old-path)
      ?.  =('~' i.old-path)
        ~
      [%give %kick ~[mailbox+old-path] ~]~
    ::
    ++  add-members-group
      |=  [=path ships=(set ship)]
      ^-  card
      ?>  ?=([@ @ ~] path)
      =/  rid=resource
        [(slav %p i.path) i.t.path]
      =-  [%pass / %agent [our.bol %group-store] %poke %group-action -]
      !>(`action:group-store`[%add-members rid ships])
    ::
    ++  add-synced
      |=  [=ship =path]
      ^-  card
      =-  [%pass / %agent [our.bol %chat-hook] %poke %chat-hook-action -]
      !>(`action:hook`[%add-synced ship path %.y])
    ::
    ++  add-owned
      |=  [=path history=?]
      ^-  card
      =-  [%pass / %agent [our.bol %chat-hook] %poke %chat-hook-action -]
      !>(`action:hook`[%add-owned path history])
    ::
    ++  generate-cards
      |=  old-chat=path
      ^-  (list card)
      =/  host=ship  (slav %p (snag 0 old-chat))
      =/  new-chat  [%'~' old-chat]
      =/  old-group=path  [%chat old-chat]
      %-  zing
      :~  :~  (delete-group host (snoc old-group %read))
              (delete-group host (snoc old-group %write))
          ==
        ::
          [(record-group new-chat new-chat)]~
          (recreate-chat host old-chat new-chat)
      ==
    ::
    ++  recreate-chat
      |=  [host=ship chat=path new-chat=path]
      ^-  (list card)
      =/  old-mailbox=mailbox:store
        (need (scry:cc (unit mailbox:store) %chat-store [%mailbox chat]))
      =*  enves  envelopes.old-mailbox
      :~  (chat-poke:cc [%delete new-chat])
          (chat-poke:cc [%delete chat])
          (chat-poke:cc [%create new-chat])
          (chat-poke:cc [%messages new-chat enves])
          (chat-poke:cc [%read new-chat])
          %^  make-poke  %chat-hook  %chat-hook-action
          !>  ^-  action:hook
          ?:  =(our.bol host)  [%add-owned new-chat %.y]
          [%add-synced host new-chat %.y]
      ==
    ::
    ++  make-poke
      |=  [app=term =mark =vase]
      ^-  card
      [%pass /on-load/[app]/[mark] %agent [our.bol app] %poke mark vase]
    ::
    ++  delete-group
      |=  [host=ship group=path]
      ^-  card
      %^  make-poke  %group-store
        %group-action
      !>  ^-  action:group-store
      [%remove-group (de-path:resource group) ~]
    ::
    ++  create-group
      |=  [group=path who=(set ship)]
      ^-  (list card)
      =/  rid=resource
        (de-path:resource group)
      :~  %^  make-poke  %group-store
            %group-action
          !>  ^-  action:group-store
          [%add-group rid *invite:policy %.n]
        ::
          %^  make-poke  %group-store
            %group-action
          !>  ^-  action:group-store
          [%add-members rid who]
      ==
    ::
    ++  record-group
      |=  [group=path chat=path]
      ^-  card
      =/  =metadata
        ~|  [%weird-chat-path chat]
        %*  .  *metadata
          title         (snag 2 chat)
          date-created  now.bol
          creator       (slav %p (snag 1 chat))
        ==
      %^  make-poke  %metadata-store
        %metadata-action
      !>  ^-  metadata-action
      [%add group [%chat chat] metadata]
    --
  ::
  ++  on-poke
    ~/  %chat-hook-poke
    |=  [=mark =vase]
    ^-  (quip card _this)
    =^  cards  state
      ?+  mark  (on-poke:def mark vase)
          %json         (poke-json:cc !<(json vase))
          %chat-action  (poke-chat-action:cc !<(action:store vase))
          %noun
        %-  poke-noun:cc
        !<  ?(%fix-dm %fix-out-of-sync %run-upg7 %run-upg9 %migrate-graph)
        vase
      ::
          %chat-hook-action
        (poke-chat-hook-action:cc !<(action:hook vase))
      ::
          %import
        ?>  (team:title our.bol src.bol)
        (poke-import:cc q.vase)
      ==
    [cards this]
  ::
  ++  on-watch
    ~/  %chat-hook-watch
    |=  =path
    ^-  (quip card _this)
    ?+  path          (on-watch:def path)
        [%backlog *]  [(watch-backlog:cc t.path) this]
        [%mailbox *]  [(watch-mailbox:cc t.path) this]
        [%synced *]   [(watch-synced:cc t.path) this]
    ==
  ::
  ++  on-agent
    ~/  %chat-hook-agent
    |=  [=wire =sign:agent:gall]
    ^-  (quip card _this)
    |^
    ?.  ?=([%migrate-graph *] wire)
      `this
    =/  rid=resource
      (de-path:resource t.wire)
    ?.  ?=(%watch-ack -.sign)
      ~|  "Expected error, please ignore"
      (on-agent:def wire sign)
    ?~  p.sign
      :_  this(migrate (~(del by migrate) rid))
      ~[(poke-graph-pull-hook %add entity.rid rid)]
    =/  nack-count=@ud
      +((~(gut by migrate) rid 0))
    ?:  (gte nack-count 24)
      ~&  >>>  "failed to migrate notebook {<rid>} to graph-store"
      [~ this]
    :_  this(migrate (~(put by migrate) rid nack-count))
    =/  wakeup=@da
      (add now.bol (mul ~s1 (bex (min 19 nack-count))))
    [%pass wire %arvo %b %wait wakeup]~
    ::
    ++  poke-graph-pull-hook
      |=  =action:pull-hook
      ^-  card
      =-  [%pass / %agent [our.bol %graph-pull-hook] %poke -]
      pull-hook-action+!>(action)
    --
  ::
  ++  on-leave  on-leave:def
  ++  on-peek
    |=  =path
    ^-  (unit (unit cage))
    ?+  path  (on-peek:def path)
        [%x %export ~]
      ``noun+!>(state)
    ==
  ++  on-arvo   
    |=  [=wire =sign-arvo]
    ^-  (quip card _this)
    ?+  wire  (on-arvo:def wire sign-arvo)
        [%migrate-graph *]
      =/  rid=resource
        (de-path:resource t.wire)
      ?>  ?=([%behn %wake *] sign-arvo)
      ~?  ?=(^ error.sign-arvo)
        "behn errored in backoff timers, continuing anyway" 
      :_  this
      ~[(watch-graph:cc rid)]
    ::
        [%try-rejoin @ @ *]
      ::  TODO: check whether we have migrated to graph yet??
      =/  nack-count=@ud  (slav %ud i.t.wire)
      =/  who=@p          (slav %p i.t.t.wire)
      =/  pax             t.t.t.wire
      ?>  ?=([%behn %wake *] sign-arvo)
      ~?  ?=(^ error.sign-arvo)
        "behn errored in backoff timers, continuing anyway"
      :_  this
      [(try-rejoin:cc who pax +(nack-count))]~
    ==
  ++  on-fail   on-fail:def
  --
::
::
~%  %chat-hook-library  ..card  ~
|_  bol=bowl:gall
++  grp  ~(. grpl bol)
++  watch-graph
  |=  rid=resource
  ^-  card
  =/  =path
    (en-path:resource rid)
  [%pass migrate-graph+path %agent [entity.rid %graph-push-hook] %watch resource+path]
::
++  poke-noun
  |=  a=?(%fix-dm %fix-out-of-sync %run-upg7 %run-upg9 %migrate-graph)
  ^-  (quip card _state)
  |^
  ?-  a
      %fix-dm    [(fix-dm %fix-dm) state]
      %fix-out-of-sync  [(fix-out-of-sync %fix-out-of-sync) state]
      %run-upg7    run-7-to-8
      %run-upg9   run-9-to-10
      %migrate-graph  migrate-graph
  ==
  ::
  ++  poke-our
    |=  [app=term =cage]
    ^-  card
    [%pass / %agent [our.bol app] %poke cage]
  ::
  ++  poke-graph-push-hook
    |=  =action:push-hook
    ^-  card
    (poke-our %graph-push-hook %push-hook-action !>(action))
  ::
  ++  poke-graph-store
    |=  =update:graph-store
    ^-  card
    (poke-our %graph-store %graph-update !>(update))
  ::
  ++  migrate-graph
    ^-  (quip card _state)
    =/  syncs=(list [=path =ship])
      ~(tap by synced)
    =|  cards=(list card)
    |- 
    ?~  syncs  [cards state]
    =,  i.syncs
    =/  rid=resource
      (path-to-resource:store path)
    ~&  migrating+path
    ~&  to+rid
    ?:  =(nobody:store entity.rid) 
      %_   $
       syncs  t.syncs
       ::
         cards
        :_  cards
        %-  poke-graph-store
        :+  %0  now.bol
        archive-graph+rid
      ==
    ?:  =(our.bol ship)
      %_    $  
        cards  :_(cards (poke-graph-push-hook %add rid))
        syncs  t.syncs
      ==
    %_   $
      cards    :_(cards (watch-graph rid))
      syncs    t.syncs
      migrate  (~(put by migrate) rid 0)
    ==
  ::
  ++  scry-for
    |*  [=mold app=term =path]
    .^  mold
      %gx
      (scot %p our.bol)
      app
      (scot %da now.bol)
      (snoc `^path`path %noun)
    ==
  ::
  ++  add-synced
    |=  [=ship =path]
    ^-  card
    =-  [%pass / %agent [our.bol %chat-hook] %poke %chat-hook-action -]
    !>(`action:hook`[%add-synced ship path %.y])
  ::
  ++  add-owned
    |=  [=path history=?]
    ^-  card
    =-  [%pass / %agent [our.bol %chat-hook] %poke %chat-hook-action -]
    !>(`action:hook`[%add-owned path history])
  ::
  ++  run-7-to-8
    ^-  (quip card _state)
    :_  state
    =/  subscribers=(jug path ship)
      %+  roll  ~(val by sup.bol)
      |=  [[=ship =path] out=(jug path ship)]
      ::  /(mailbox|backlog)/~ship/resource.name
      ::
      ?.  ?=([@ @ @ *] path)  out
      =/  pax=^path  [i.t.path i.t.t.path ~]
      (~(put ju out) pax ship)
    =/  group  ~(. grpl bol)
    ^-  (list card)
    %+  murn  ~(tap in ~(key by synced.state))
    |=  =path
    ^-  (unit card)
    ?>  ?=([@ @ ~] path)
    =/  group-paths  (groups-of-chat path)
    ?~  group-paths  ~
    =/  members     (members-from-path:group i.group-paths)
    ?:  (is-managed-path:group i.group-paths)  ~
    =/  ships=(set ship)  (~(get ju subscribers) path)
    %-  some
    =+  [%invite path (~(dif in members) ships)]
    [%pass /inv %agent [our.bol %chat-view] %poke %chat-view-action !>(-)]
  ::
  ++  run-9-to-10
    ^-  (quip card _state)
    :_
      =/  list-paths=(list path)
        %+  murn  ~(tap in ~(key by synced.state))
        |=  =app=path
        ^-  (unit path)
        ?~  (groups-of-chat app-path)
          `app-path
        ~
      |-
      ?~  list-paths
        state
      =.  synced.state  (~(del by synced.state) i.list-paths)
      $(list-paths t.list-paths)
    %+  weld
      ^-  (list card)
      %+  roll  ~(tap in ~(key by wex.bol))
      |=  [[=wire =ship =term] out=(list card)]
      ?>  ?=([@ *] wire)
      ?.  ?&(=(ship our.bol) =(term %chat-hook))
        out
      :_  out
      =-  [%pass / %agent [our.bol %chat-hook] %poke %chat-hook-action !>(-)]
      [%remove t.wire]
    =/  chat-keys=(set path)  (scry-for (set path) %chat-store [%keys ~])
    ^-  (list card)
    %+  turn  ~(tap in chat-keys)
    |=  =app=path
    ^-  card
    ?>  ?=([@ @ ~] app-path)
    =/  =ship  (slav %p i.app-path)
    ?:  =(ship our.bol)
      (add-owned app-path %.y)
    (add-synced ship app-path)
  ::
  ++  fix-out-of-sync
    |=  b=%fix-out-of-sync
    ^-  (list card)
    %-  zing
    %+  turn  ~(tap by synced)
    |=  [=path host=ship]
    ^-  (list card)
    ?:  =(host our.bol)  ~
    ?>  ?=([@ @ ~] path)
    =/  =ship  (slav %p i.path)
    :~  =-  [%pass / %agent [our.bol %chat-hook] %poke %chat-hook-action -]
        !>  ^-  action:hook
        [%remove path]
    ::
        =-  [%pass / %agent [our.bol %chat-hook] %poke %chat-hook-action -]
        !>  ^-  action:hook
        [%add-synced ship path %.y]
    ==
  ::
  ++  fix-dm
    |=  b=%fix-dm
    ^-  (list card)
    %-  zing
    %+  turn
      ~(tap by synced)
    |=  [=path host=ship]
    ^-  (list card)
    ?>  ?=([@ @ *] path)
    =/  =ship  (slav %p i.path)
    ?:  =(ship our.bol)
      ::  local dm, no need to do cleanup
      ~
    ?:  ?=(^ (groups-of-chat path))
      ::  correctly initialized, no need to do cleanup
      ::
      ~
    ?.  =((end [3 4] i.t.path) 'dm--')
      ~
    :-  =-  [%pass /fixdm %agent [our.bol %chat-view] %poke %chat-view-action -]
        !>  ^-  action:view
        [%delete path]
    =/  new-dm  /(scot %p our.bol)/(crip (weld "dm--" (trip (scot %p ship))))
    =/  mailbox=(unit mailbox:store)  (chat-scry path)
    ?~  mailbox
      ~
    :~  =-  [%pass /fixdm %agent [our.bol %chat-view] %poke %chat-view-action -]
        !>  ^-  action:view
        :*  %create
            %-  crip
            (zing [(trip (scot %p our.bol)) " <-> " (trip (scot %p ship)) ~])
            ''
            new-dm
            ship+new-dm
            [%invite (silt ~[ship])]
            (silt ~[ship])
            %.y
            %.n
        ==
      ::
        =-  [%pass /fixdm %agent [our.bol %chat-store] %poke %chat-action -]
        !>  ^-  action:store
        [%messages new-dm envelopes.u.mailbox]
    ==
  --
::
++  poke-json
  |=  jon=json
  ^-  (quip card _state)
  (poke-chat-action (action:dejs:store jon))
::
++  poke-chat-action
  |=  act=action:store
  ^-  (quip card _state)
  ?>  ?=(%message -.act)
  ::  local
  :_  state
  ?:  (team:title our.bol src.bol)
    ?.  (~(has by synced) path.act)
      ~
    =*  letter  letter.envelope.act
    =?  letter  &(?=(%code -.letter) ?=(~ output.letter))
      =/  =hoon  (ream expression.letter)
      letter(output (eval:store bol hoon))
    =/  ship  (~(got by synced) path.act)
    =/  appl  ?:(=(ship our.bol) %chat-store %chat-hook)
    [%pass / %agent [ship appl] %poke %chat-action !>(act)]~
  ::  foreign
  =/  ship  (~(get by synced) path.act)
  ?~  ship  ~
  ?.  =(u.ship our.bol)  ~
  ::  check if write is permitted
  ?.  (is-member:grp src.bol (group-from-chat path.act))  ~
  =:  author.envelope.act  src.bol
      when.envelope.act  now.bol
  ==
  [%pass / %agent [our.bol %chat-store] %poke %chat-action !>(act)]~
::
++  poke-chat-hook-action
  |=  act=action:hook
  ^-  (quip card _state)
  ?-  -.act
      %add-owned
    ?>  (team:title our.bol src.bol)
    =/  chat-path  [%mailbox path.act]
    =/  chat-wire  [%store path.act]
    ?:  (~(has by synced) path.act)  [~ state]
    =:  synced  (~(put by synced) path.act our.bol)
        allow-history  (~(put by allow-history) path.act allow-history.act)
    ==
    :_  state
    :~  [%pass chat-wire %agent [our.bol %chat-store] %watch chat-path]
        [%give %fact [/synced]~ %chat-hook-update !>([%initial synced])]
    ==
  ::
      %add-synced
    ?>  (team:title our.bol src.bol)
    ?<  =(ship.act our.bol)
    ?:  (~(has by synced) path.act)  [~ state]
    =.  synced  (~(put by synced) path.act ship.act)
    ?.  ask-history.act
      =/  chat-path  [%mailbox path.act]
      :_  state
      [%pass chat-path %agent [ship.act %chat-hook] %watch chat-path]~
    =/  mailbox=(unit mailbox:store)  (chat-scry path.act)
    =/  chat-history=path
      :-  %backlog
      %+  weld  path.act
      ?~(mailbox /0 /(scot %ud (lent envelopes.u.mailbox)))
    :_  state
    :~  [%pass chat-history %agent [ship.act %chat-hook] %watch chat-history]
        [%give %fact [/synced]~ %chat-hook-update !>([%initial synced])]
    ==
  ::
      %remove
    =/  ship=(unit ship)
      =/  ship  (~(get by synced) path.act)
      ?^  ship  ship
      =?  path.act  ?=([%'~' *] path.act)  t.path.act
      ?~  path.act  ~
      (slaw %p i.path.act)
    ?~  ship
      ~&  [dap.bol %unknown-host-cannot-leave path.act]
      [~ state]
    ?:  &(!=(u.ship src.bol) ?!((team:title our.bol src.bol)))
      [~ state]
    =.  synced  (~(del by synced) path.act)
    :_  state
    :*  [%give %kick ~[[%mailbox path.act]] ~]
        [%give %fact [/synced]~ %chat-hook-update !>([%initial synced])]
        (pull-wire u.ship [%mailbox path.act])
        (pull-wire u.ship [%store path.act])
        (pull-backlog-subscriptions u.ship path.act)
    ==
  ==
::
++  poke-import
  |=  arc=*
  ^-  (quip card _state)
  =/  sty=state-11
    :+  %11
      :+  (remake-map ;;((tree [path ship]) +<.arc))
        ;;(? +>-.arc)
      (remake-map ;;((tree [path ?]) +>+.arc))
    ~
  :_  sty
  :_  ~
  =-  [%pass /self-poke %agent [our.bol %chat-hook] %poke -]
  noun+!>(%migrate-graph)
::
++  try-rejoin
  |=  [who=@p pax=path nack-count=@ud]
  ^-  card
  =/  =wire
    [%try-rejoin (scot %ud nack-count) (scot %p who) pax]
  [%pass wire %agent [who %chat-hook] %watch pax]
::
++  watch-synced
  |=  pax=path
  ^-  (list card)
  ?>  (team:title our.bol src.bol)
  [%give %fact ~ %chat-hook-update !>([%initial synced])]~
::
++  watch-mailbox
  |=  pax=path
  ^-  (list card)
  ?>  ?=(^ pax)
  ?>  (~(has by synced) pax)
  ::  check if read is permitted
  ?>  (is-member:grp src.bol (group-from-chat pax))
  =/  box  (chat-scry pax)
  ?~  box  !!
  [%give %fact ~ %chat-update !>([%create pax])]~
::
++  watch-backlog
  |=  pax=path
  ^-  (list card)
  ?>  ?=(^ pax)
  =/  last  (dec (lent pax))
  =/  backlog-latest=(unit @ud)  (rush (snag last `(list @ta)`pax) dem:ag)
  =/  pas  `path`(oust [last 1] `(list @ta)`pax)
  ?>  ?=([* ^] pas)
  ?>  (is-member:grp src.bol (group-from-chat pas))
  =/  envs  envelopes:(need (chat-scry pas))
  =/  length  (lent envs)
  =/  latest
    ?~  backlog-latest  length
    ?:  (gth u.backlog-latest length)  0
    (sub length u.backlog-latest)
  =.  envs  (scag latest envs)
  =/  =vase  !>([%messages pas 0 latest envs])
  %-  zing
  :~  [%give %fact ~ %chat-update !>([%create pas])]~
      ?.  ?&(?=(^ backlog-latest) (~(has by allow-history) pas))  ~
      ?:  =(0 latest)  ~
      [%give %fact ~ %chat-update vase]~
      [%give %kick [%backlog pax]~ `src.bol]~
  ==
::
++  fact-invite-update
  |=  [wir=wire fact=update:inv]
  ^-  (quip card _state)
  :_  state
  ?+  -.fact  ~
      %accepted
    =*  resource  resource.invite.fact
    =/  =path  [(scot %p entity.resource) name.resource ~]
    :_  ~
    %-  chat-view-poke
    :^  %join  ship.invite.fact
      path
    ?=(~ (chat-scry path))
==
::
++  fact-group-update
  |=  [wir=wire =update:group-store]
  ^-  (quip card _state)
  :_  state
  ?.  ?=(%remove-members -.update)
    ~
  =/  =path
    (en-path:resource resource.update)
  =/  chats
    (chats-of-group path)
  %-  zing
  %+  turn
    chats
  |=  chat=^path
  ^-  (list card)
  =/  owner
    (~(get by synced) chat)
  ?~  owner  ~
  ?.  =(u.owner our.bol)
    ~
  %+  turn
    ~(tap in ships.update)
  |=  =ship
  [%give %kick [%mailbox chat]~ `ship]
::
++  fact-chat-update
  |=  [wir=wire =update:store]
  ^-  (quip card _state)
  ?:  (team:title our.bol src.bol)
    (handle-local update)
  (handle-foreign update)
::
++  handle-local
  |=  =update:store
  ^-  (quip card _state)
  ?+  -.update     [~ state]
      %delete
    ?.  (~(has by synced) path.update)  [~ state]
    =.  synced  (~(del by synced) path.update)
    :_  state
    :~  [%pass [%mailbox path.update] %agent [our.bol %chat-store] %leave ~]
        [%give %fact [/synced]~ %chat-hook-update !>([%initial synced])]
    ==
  ::
      %message
    :_  state
    [%give %fact [%mailbox path.update]~ %chat-update !>(update)]~
  ::
      %messages
    :_  state
    [%give %fact [%mailbox path.update]~ %chat-update !>(update)]~
  ==
::
++  handle-foreign
  |=  =update:store
  ^-  (quip card _state)
  ?+  -.update   [~ state]
      %create
    :_  state
    ?>  ?=([* ^] path.update)
    =/  shp  (~(get by synced) path.update)
    ?~  shp  ~
    ?.  =(src.bol u.shp)  ~
    [(chat-poke [%create path.update])]~
  ::
      %delete
    ?>  ?=([* ^] path.update)
    =/  shp  (~(get by synced) path.update)
    ?~  shp  [~ state]
    ?.  =(u.shp src.bol)  [~ state]
    =.  synced  (~(del by synced) path.update)
    :_  state
    :-  (chat-poke [%delete path.update])
    :~  [%pass [%mailbox path.update] %agent [src.bol %chat-hook] %leave ~]
        [%give %fact [/synced]~ %chat-hook-update !>([%initial synced])]
    ==
  ::
      %message
    :_  state
    ?>  ?=([* ^] path.update)
    =/  shp  (~(get by synced) path.update)
    ?~  shp  ~
    ?.  =(src.bol u.shp)  ~
    [(chat-poke [%message path.update envelope.update])]~
  ::
      %messages
    :_  state
    ?>  ?=([* ^] path.update)
    =/  shp  (~(get by synced) path.update)
    ?~  shp  ~
    ?.  =(src.bol u.shp)  ~
    [(chat-poke [%messages path.update envelopes.update])]~
  ==
::
++  kick
  |=  wir=wire
  ^-  (quip card _state)
  ?+  wir  !!
    [%try-rejoin @ @ *]
      $(wir t.t.t.wir)
  ::
    [%groups ~]  [~[watch-groups] state]
  ::
      [%store @ *]
    ~&  store-kick+wir
    ?:  =('~' i.t.wir)
      (migrate-store t.t.wir)
    ?.  (~(has by synced) t.wir)  [~ state]
    ~&  %chat-store-resubscribe
    =/  mailbox=(unit mailbox:store)
      (chat-scry t.wir)
    :_  state
    [%pass wir %agent [our.bol %chat-store] %watch [%mailbox t.wir]]~
  ::
      [%mailbox @ *]
    ~&  mailbox-kick+wir
    ?:  =('~' i.t.wir)
      (migrate-listen t.t.wir)
    ?.  (~(has by synced) t.wir)  [~ state]
    ~&  %chat-hook-resubscribe
    =/  =ship  (~(got by synced) t.wir)
    =/  mailbox=(unit mailbox:store)  (chat-scry t.wir)
    =/  chat-history
      %+  welp  backlog+t.wir
      ?~(mailbox /0 /(scot %ud (lent envelopes.u.mailbox)))
    :_  state
    [%pass chat-history %agent [ship %chat-hook] %watch chat-history]~
  ::
      [%backlog @ @ *]
    =/  chat=path  (oust [(dec (lent t.wir)) 1] `(list @ta)`t.wir)
    ?:  =('~' i.t.wir)
      ?>  ?=(^ chat)
      (migrate-listen t.chat)
    ?.  (~(has by synced) chat)  [~ state]
    =/  =ship
      ?:  =('~' i.t.wir)
        (slav %p i.t.t.wir)
      (slav %p i.t.wir)
    =/  =path  ?~((chat-scry chat) wir [%mailbox chat])
    :_  state
    [%pass path %agent [ship %chat-hook] %watch path]~
  ==
++  migrate-listen
  |=  =wire
  ^-  (quip card _state)
  ~&  listen-migrate+wire
  ?>  ?=([@ @ ~] wire)
  =/  =ship
    (slav %p i.wire)
  :_  state
  ~[(chat-view-poke %join ship wire %.y)]
::
++  migrate-store
  |=  =wire
  ^-  (quip card _state)
  ~&  store-migrate+wire
  (kick store+wire)
::
++  chat-poke
  |=  act=action:store
  ^-  card
  [%pass / %agent [our.bol %chat-store] %poke %chat-action !>(act)]
::
++  chat-view-poke
  |=  act=action:view
  ^-  card
  [%pass / %agent [our.bol %chat-view] %poke %chat-view-action !>(act)]
::
++  invite-poke
  |=  =action:inv
  ^-  card
  [%pass / %agent [our.bol %invite-store] %poke %invite-action !>(action)]
::
++  chat-scry
  |=  pax=path
  ^-  (unit mailbox:store)
  %^  scry  (unit mailbox:store)
    %chat-store
  [%mailbox pax]
::
++  invite-scry
  |=  uid=serial:inv
  ^-  (unit invite:inv)
  %^  scry  (unit invite:inv)
    %invite-store
  /invite/chat/(scot %uv uid)
::
++  chats-of-group
  |=  =group-path
  ^-  (list path)
  ::  if metadata-store isn't running yet, we're still in the upgrade ota phase.
  ::  we can't get chats from the metadata-store, but can make assumptions
  ::  about group path shape, and the chat that would match it.
  ::TODO  remove me at some point.
  ::
  ?.  .^(? %gu (scot %p our.bol) %metadata-store (scot %da now.bol) ~)  ~
  %+  murn
    ^-  (list md-resource)
    =;  resources
      %~  tap  in
      %+  ~(gut by resources)
        group-path
      *(set md-resource)
    .^  (jug path md-resource)
      %gy
      (scot %p our.bol)
      %metadata-store
      (scot %da now.bol)
      /group-indices
    ==
  |=  md-resource
  ^-  (unit path)
  ?.  =(%chat app-name)  ~
  `app-path
::
++  groups-of-chat
  |=  chat=path
  ^-  (list group-path)
  ::  if metadata-store isn't running yet, we're still in the upgrade ota phase.
  ::  we can't get groups from the metadata-store, but can make assumptions
  ::  about chat path shape, and the chat that would match it.
  ::TODO  remove me at some point.
  ::
  ?.  .^(? %gu (scot %p our.bol) %metadata-store (scot %da now.bol) ~)  ~
  =;  resources
    %~  tap  in
    %+  ~(gut by resources)
      [%chat chat]
    *(set group-path)
  .^  (jug md-resource group-path)
    %gy
    (scot %p our.bol)
    %metadata-store
    (scot %da now.bol)
    /resource-indices
  ==
::
++  group-from-chat
  |=  app-path=path
  ^-  group-path
  =/  groups=(list group-path)
    (groups-of-chat app-path)
  ?>  ?=(^ groups)
  i.groups
::
++  scry
  |*  [=mold app=term =path]
  .^  mold
    %gx
    (scot %p our.bol)
    app
    (scot %da now.bol)
    (snoc `^path`path %noun)
  ==
::
++  pull-backlog-subscriptions
  |=  [target=ship chat=path]
  ^-  (list card)
  %+  murn  ~(tap by wex.bol)
  |=  [[=wire =ship =term] [acked=? =path]]
  ^-  (unit card)
  ?.  ?&  =(ship target)
          ?=([%backlog *] wire)
          =(`1 (find chat wire))
      ==
    ~
  `(pull-wire target wire)
::
++  pull-wire
  |=  [=ship =wire]
  ^-  card
  ?:  =(ship our.bol)
    [%pass wire %agent [our.bol %chat-store] %leave ~]
  [%pass wire %agent [ship %chat-hook] %leave ~]
++  watch-groups
  ^-  card
  [%pass /groups %agent [our.bol %group-store] %watch /groups]
--
