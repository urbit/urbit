::  link-listen-hook: get your friends' bookmarks
::
::    subscribes to all %link resources in the metadata-store.
::    for all ships in groups associated with those resources, we subscribe to
::    their link's local-pages and annotations at the resource path (through
::    link-proxy-hook), and forward all entries into our link-store as
::    submissions and comments.
::
::    if a subscription to a target fails, we assume it's because their
::    metadata+groups definition hasn't been updated to include us yet.
::    we retry with exponential backoff, maxing out at one hour timeouts.
::    to expede this process, we prod other potential listeners when we add
::    them to our metadata+groups definition.
::
/-  *metadata-store, *link, group-store
/+  metadata, default-agent, verb, dbug
::
|%
+$  state-0
  $:  %0
      retry-timers=(map target @dr)
      ::  reasoning: the resources we're subscribed to,
      ::             and the groups that cause that.
      ::
      ::    we don't strictly need to track this in state, but doing so heavily
      ::    simplifies logic and reduces the amount of big scries we do.
      ::    this also gives us the option to check & restore subscriptions,
      ::    should we ever need that.
      ::
      reasoning=(jug [ship app-path] group-path)
  ==
::
+$  what-target  ?(%local-pages %annotations)
+$  target
  $:  what=what-target
      who=ship
      where=path
  ==
++  wire-to-target
  |=  =wire
  ^-  target
  ?>  ?=([what-target @ ^] wire)
  [i.wire (slav %p i.t.wire) t.t.wire]
++  target-to-wire
  |=  target
  ^-  wire
  [what (scot %p who) where]
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
  ++  on-init
    ^-  (quip card _this)
    :_  this
    ~[watch-metadata:do watch-groups:do]
  ::
  ++  on-save  !>(state)
  ++  on-load
    |=  old=vase
    ^-  (quip card _this)
    [~ this(state !<(state-0 old))]
  ::
  ++  on-agent
    |=  [=wire =sign:agent:gall]
    ^-  (quip card _this)
    =^  cards  state
      ?+  wire  ~|([dap.bowl %weird-agent-wire wire] !!)
          [%metadata ~]
        (take-metadata-sign:do sign)
      ::
          [%groups ~]
        (take-groups-sign:do sign)
      ::
          [%links ?(%local-pages %annotations) @ ^]
        (take-link-sign:do (wire-to-target t.wire) sign)
      ::
          [%forward ^]
        (take-forward-sign:do t.wire sign)
      ::
          [%prod *]
        ?>  ?=(%poke-ack -.sign)
        ?~  p.sign  [~ state]
        %-  (slog leaf+"prod failed" u.p.sign)
        [~ state]
      ==
    [cards this]
  ::
  ++  on-poke
    |=  [=mark =vase]
    ?.  ?=(%link-listen-poke mark)
      (on-poke:def mark vase)
    =/  =path  !<(path vase)
    :_  this
    %+  weld
      (take-retry:do %local-pages src.bowl path)
    (take-retry:do %annotations src.bowl path)
  ::
  ++  on-arvo
    |=  [=wire =sign-arvo]
    ^-  (quip card _this)
    ?+  sign-arvo  (on-arvo:def wire sign-arvo)
        [%g %done *]
      ?~  error.sign-arvo  [~ this]
      =/  =tank  leaf+"{(trip dap.bowl)}'s message went wrong!"
      %-  (slog tank tang.u.error.sign-arvo)
      [~ this]
    ::
        [%b %wake *]
      ?>  ?=([%retry @ @ ^] wire)
      ?^  error.sign-arvo
        =/  =tank  leaf+"wake on {(spud wire)} went wrong!"
        %-  (slog tank u.error.sign-arvo)
        [~ this]
      :_  this
      (take-retry:do (wire-to-target t.wire))
    ==
  ::
  ++  on-peek   on-peek:def
  ++  on-watch  on-watch:def
  ++  on-leave  on-leave:def
  ++  on-fail   on-fail:def
  --
::
::
|_  =bowl:gall
+*  md  ~(. metadata bowl)
::
::  metadata subscription
::
++  watch-metadata
  ^-  card
  [%pass /metadata %agent [our.bowl %metadata-store] %watch /app-name/link]
::
++  take-metadata-sign
  |=  =sign:agent:gall
  ^-  (quip card _state)
  ?-  -.sign
    %poke-ack     ~|([dap.bowl %unexpected-poke-ack /metadata] !!)
    %kick         [[watch-metadata]~ state]
  ::
      %watch-ack
    ?~  p.sign  [~ state]
    =/  =tank
      :-  %leaf
      "{(trip dap.bowl)} failed subscribe to metadata store. very wrong!"
    %-  (slog tank u.p.sign)
    [~ state]
  ::
      %fact
    =*  mark  p.cage.sign
    =*  vase  q.cage.sign
    ?.  ?=(%metadata-update mark)
      ~|  [dap.bowl %unexpected-mark mark]
      !!
    %-  handle-metadata-update
    !<(metadata-update vase)
  ==
::
++  handle-metadata-update
  |=  upd=metadata-update
  ^-  (quip card _state)
  ?+  -.upd  [~ state]
      %associations
    =/  socs=(list [=group-path resource])
      ~(tap in ~(key by associations.upd))
    =|  cards=(list card)
    |-  ::TODO  try for +roll maybe?
    ?~  socs  [cards state]
    =^  caz  state
      =,  i.socs
      ?.  =(%link app-name)  [~ state]
      (listen-to-group app-path group-path)
    $(socs t.socs, cards (weld cards caz))
  ::
      %add
    ?>  =(%link app-name.resource.upd)
    (listen-to-group app-path.resource.upd group-path.upd)
  ::
      %remove
    ?>  =(%link app-name.resource.upd)
    (leave-from-group app-path.resource.upd group-path.upd)
  ==
::
::  groups subscriptions
::
++  watch-groups
  ^-  card
  [%pass /groups %agent [our.bowl %group-store] %watch /all]
::
++  take-groups-sign
  |=  =sign:agent:gall
  ^-  (quip card _state)
  ?-  -.sign
    %poke-ack   ~|([dap.bowl %unexpected-poke-ack /groups] !!)
    %kick       [[watch-groups]~ state]
  ::
      %watch-ack
    ?~  p.sign  [~ state]
    =/  =tank
      :-  %leaf
      "{(trip dap.bowl)} failed subscribe to groups. very wrong!"
    %-  (slog tank u.p.sign)
    [~ state]
  ::
      %fact
    =*  mark  p.cage.sign
    =*  vase  q.cage.sign
    ?+  mark  ~|([dap.bowl %unexpected-mark mark] !!)
      %group-initial  [~ state]  ::NOTE  initial handled using metadata
      %group-update   (handle-group-update !<(group-update:group-store vase))
    ==
  ==
::
++  handle-group-update
  |=  upd=group-update:group-store
  ^-  (quip card _state)
  ?.  ?=(?(%path %add %remove) -.upd)
    [~ state]
  =/  socs=(list app-path)
    (app-paths-from-group:md %link pax.upd)
  =/  whos=(list ship)
    ~(tap in members.upd)
  =|  cards=(list card)
  |-
  =*  loop-socs  $
  ?~  socs  [cards state]
  |-
  =*  loop-whos  $
  ?~  whos  loop-socs(socs t.socs)
  =^  caz  state
    ?:  ?=(%remove -.upd)
      (leave-from-peer i.socs pax.upd i.whos)
    (listen-to-peer i.socs pax.upd i.whos)
  loop-whos(whos t.whos, cards (weld cards caz))
::
::  link subscriptions
::
++  listen-to-group
  |=  [=app-path =group-path]
  ^-  (quip card _state)
  =/  peers=(list ship)
    ~|  group-path
    %~  tap  in
    =-  (fall - *group:group-store)
    %^  scry-for  (unit group:group-store)
      %group-store
    group-path
  =|  cards=(list card)
  |-
  ?~  peers  [cards state]
  =^  caz  state
    (listen-to-peer app-path group-path i.peers)
  $(peers t.peers, cards (weld cards caz))
::
++  leave-from-group
  |=  [=app-path =group-path]
  ^-  (quip card _state)
  =/  peers=(list ship)
    %~  tap  in
    =-  (fall - *group:group-store)
    %^  scry-for  (unit group:group-store)
      %group-store
    group-path
  =|  cards=(list card)
  |-
  ?~  peers  [cards state]
  =^  caz  state
    (leave-from-peer app-path group-path i.peers)
  $(peers t.peers, cards (weld cards caz))
::
++  listen-to-peer
  |=  [=app-path =group-path who=ship]
  ^-  (quip card _state)
  ?:  =(our.bowl who)
    [~ state]
  :_  =-  state(reasoning -)
      (~(put ju reasoning) [who app-path] group-path)
  :-  (prod-other-listener who app-path)
  ?^  (~(get ju reasoning) [who app-path])
    ~
  (start-link-subscriptions who app-path)
::
++  leave-from-peer
  |=  [=app-path =group-path who=ship]
  ^-  (quip card _state)
  ?:  =(our.bowl who)
    [~ state]
  :_  =-  state(reasoning -)
      (~(del ju reasoning) [who app-path] group-path)
  ?.  (~(has ju reasoning) [who app-path] group-path)
    ~
  (end-link-subscriptions who app-path)
::
++  start-link-subscriptions
  |=  [=ship =app-path]
  ^-  (list card)
  :~  (start-link-subscription %local-pages ship app-path)
      (start-link-subscription %annotations ship app-path)
  ==
::
++  start-link-subscription
  |=  =target
  ^-  card
  :*  %pass
      [%links (target-to-wire target)]
      %agent
      [who.target %link-proxy-hook]
      %watch
      ?-  what.target
        %local-pages  [what where]:target
        %annotations  [what %$ where]:target
      ==
  ==
::
++  end-link-subscriptions
  |=  [who=ship where=path]
  ^-  (list card)
  |^  ~[(end %local-pages) (end %annotations)]
  ::
  ++  end
    |=  what=what-target
    :*  %pass
        [%links (target-to-wire what who where)]
        %agent
        [who %link-proxy-hook]
        %leave
        ~
    ==
  --
::
++  prod-other-listener
  |=  [who=ship where=path]
  ^-  card
  :*  %pass
      [%prod (scot %p who) where]
      %agent
      [who %link-listen-hook]
      %poke
      %link-listen-poke
      !>(where)
  ==
::
++  take-link-sign
  |=  [=target =sign:agent:gall]
  ^-  (quip card _state)
  ?-  -.sign
    %poke-ack   ~|([dap.bowl %unexpected-poke-ack /links target] !!)
    %kick       [[(start-link-subscription target)]~ state]
  ::
      %watch-ack
    ?~  p.sign
      =.  retry-timers  (~(del by retry-timers) target)
      [~ state]
    ::  our subscription request got rejected,
    ::  most likely because our group definition is out of sync with theirs.
    ::  set timer for retry.
    ::
    (start-retry target)
  ::
      %fact
    =*  mark  p.cage.sign
    =*  vase  q.cage.sign
    ?+  mark  ~|([dap.bowl %unexpected-mark mark] !!)
        %link-initial
      %-  handle-link-initial
      [who.target where.target !<(initial vase)]
    ::
        %link-update
      %-  handle-link-update
      [who.target where.target !<(update vase)]
    ==
  ==
::
++  start-retry
  |=  =target
  ^-  (quip card _state)
  =/  timer=@dr
    %+  min  ~h1
    %+  mul  2
    (~(gut by retry-timers) target ~s15)
  =.  retry-timers
    (~(put by retry-timers) target timer)
  :_  state
  :_  ~
  :*  %pass
      [%retry (target-to-wire target)]
      [%arvo %b %wait (add now.bowl timer)]
  ==
::
++  take-retry
  |=  =target
  ^-  (list card)
  ::  relevant: whether :who is still associated with resource :where
  ::
  =;  relevant=?
    ?.  relevant  ~
    [(start-link-subscription target)]~
  ?:  %-  ~(has by wex.bowl)
      [[%links (target-to-wire target)] who.target %link-proxy-hook]
    |
  %+  lien  (groups-from-resource:md %link where.target)
  |=  =group-path
  ^-  ?
  =-  (~(has in (fall - *group:group-store)) who.target)
  %^  scry-for  (unit group:group-store)
    %group-store
  group-path
::
++  do-link-action
  |=  [=wire =action]
  ^-  card
  :*  %pass
      wire
      %agent
      [our.bowl %link-store]
      %poke
      %link-action
      !>(action)
  ==
::
++  handle-link-initial
  |=  [who=ship where=path =initial]
  ^-  (quip card _state)
  ?>  =(src.bowl who)
  ?+  -.initial  ~|([dap.bowl %unexpected-initial -.initial] !!)
      %local-pages
    =/  =pages  (~(got by pages.initial) where)
    (handle-link-update who where [%local-pages where pages])
  ::
      %annotations
    =/  urls=(list [=url =notes])
      ~(tap by (~(got by notes.initial) where))
    =|  cards=(list card)
    |-  ^-  (quip card _state)
    ?~  urls  [cards state]
    =^  caz  state
      ^-  (quip card _state)
      =,  i.urls
      (handle-link-update who where [%annotations where url notes])
    $(urls t.urls, cards (weld cards caz))
  ==
::
++  handle-link-update
  |=  [who=ship where=path =update]
  ^-  (quip card _state)
  ?>  =(src.bowl who)
  :_  state
  ?+  -.update  ~|([dap.bowl %unexpected-update -.update] !!)
      %local-pages
    %+  turn  pages.update
    |=  =page
    %+  do-link-action
      [%forward %local-page (scot %p who) where]
    [%hear where who page]
  ::
      %annotations
    %+  turn  notes.update
    |=  =note
    ^-  card
    %+  do-link-action
      [%forward %annotation (scot %p who) where]
    [%read where url.update who note]
  ==
::
++  take-forward-sign
  |=  [=wire =sign:agent:gall]
  ^-  (quip card _state)
  ~|  [%unexpected-sign on=[%forward wire] -.sign]
  ?>  ?=(%poke-ack -.sign)
  ?~  p.sign  [~ state]
  =/  =tank
    :-  %leaf
    ;:  weld
      (trip dap.bowl)
      " failed to save submission from "
      (spud wire)
    ==
  %-  (slog tank u.p.sign)
  [~ state]
::
++  scry-for
  |*  [=mold =app-name =path]
  .^  mold
    %gx
    (scot %p our.bowl)
    app-name
    (scot %da now.bowl)
    (snoc `^path`path %noun)
  ==
--
