::  chat-hook [landscape]:
::  mirror chat data from foreign to local based on read
::  allow sending chat messages to foreign paths based on write perms
::
/-  inv=invite-store, *metadata-store, *group-store,
    hook=chat-hook, *group, push-hook, pull-hook, store=chat-store
/+  default-agent, verb, dbug, group-store, grpl=group,
    resource, graph-store, *migrate
|%
+$  card  card:agent:gall
::
+$  versioned-state
  $%  state-10
      state-11
  ==
::
+$  migration-state  (map resource @ud)
+$  state-11  [%11 state-base migrate=migration-state]
+$  state-10  [%10 state-base]
+$  state-base
  $:  =synced:hook
      invite-created=_|
      allow-history=(map path ?)
  ==
--
=|  state-11
=*  state  -
::
%-  agent:dbug
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
    =.  cards
      :_  cards
      =-  [%pass /self-poke %agent [our.bol %chat-hook] %poke -]
      noun+!>(%migrate-graph)
    $(old [%11 +.old ~])
    --
  ::
  ++  on-poke
    |=  [=mark =vase]
    ^-  (quip card _this)
    =^  cards  state
      ?+  mark  (on-poke:def mark vase)
          %noun
        %-  poke-noun:cc
        !<  ?(%migrate-graph)
        vase
      ::
          %import
        ?>  (team:title our.bol src.bol)
        (poke-import:cc q.vase)
      ==
    [cards this]
  ::
  ++  on-watch  on-watch:def
  ::
  ++  on-agent
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
  ++  on-peek   on-peek:def
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
    ==
  ++  on-fail   on-fail:def
  --
::
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
  |=  a=?(%migrate-graph)
  ^-  (quip card _state)
  |^
  ?-  a
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
    (poke-our %graph-store %graph-update-3 !>(update))
  ::
  ++  nobody
    ^-  @p
    (bex 128)
  ::
  ++  path-to-resource
    |=  =path
    ^-  resource
    ?.  ?=([@ @ ~] path)  
      nobody^(spat path)
    =/  m-ship=(unit ship)
      (slaw %p i.path)
    ?~  m-ship
      nobody^(spat path)
    [u.m-ship i.t.path]
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
      (path-to-resource path)
    ~&  migrating+path
    ~&  to+rid
    ?:  =(nobody entity.rid) 
      %_   $
       syncs  t.syncs
       ::
         cards
        :_  cards
        %-  poke-graph-store
        :-  now.bol
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
  --
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
++  invite-poke
  |=  =action:inv
  ^-  card
  [%pass / %agent [our.bol %invite-store] %poke %invite-action !>(action)]
--
