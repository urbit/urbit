::  link-listen-hook: get your friends' bookmarks
::
::    keeps track of a listening=(set app-path). automatically adds to that
::    whenever new %link resources get added in the metadata-store. users
::    can manually remove from and add back to this set.
::
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
/-  link-listen-hook, *metadata-store, *link, group-store
/+  mdl=metadata, default-agent, verb, dbug
::
|%
+$  versioned-state
  $%  [%0 state-0]
      [%1 state-1]
      [%2 state-2]
  ==
+$  state-2  state-1
+$  state-1
  $:  listening=(set app-path)
      state-0
  ==
+$  state-0
  $:  retry-timers=(map target @dr)
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
=|  [%2 state-2]
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
    |=  =vase
    ^-  (quip card _this)
    =/  old=versioned-state
      !<(versioned-state vase)
    |-
    ?-  -.old
      %2  [~ this(state old)]
    ::
        %1
      ::  the upgrade from 0 left out local-only collections.
      ::  here, we pull those back in.
      ::
      =.  state  [%2 +.old]
      =.  listening.state
        (~(run in ~(key by reasoning.old)) tail)
      =/  resources=(list [=group-path =app-path])
        %~  tap  in
        %.  %link
        %~  get  ju
        .^  (jug app-name [group-path app-path])
          %gy
          (scot %p our.bowl)
          %metadata-store
          (scot %da now.bowl)
          /app-indices
        ==
      =|  cards=(list card)
      |-
      ?~  resources  [cards this]
      =,  i.resources
      =/  =group:group-store
        =-  (fall - *group:group-store)
        (scry-for:do (unit group:group-store) %group-store group-path)
      ::  if we're the only group member, this got incorrectly ignored
      ::  during 0's upgrade logic. watch it now.
      ::
      ?.  &(=(1 ~(wyt in group)) (~(has in group) our.bowl))
        $(resources t.resources)
      =^  more-cards  state
        (handle-listen-action:do %watch app-path)
      $(resources t.resources, cards (weld more-cards cards))
    ::
        %0
      =/  listening=(set app-path)
        (~(run in ~(key by reasoning.old)) tail)
      $(old [%1 listening +.old])
    ==
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
    ?+  mark  (on-poke:def mark vase)
        %link-listen-poke
      =/  =path  !<(path vase)
      :_  this
      %+  weld
        (take-retry:do %local-pages src.bowl path)
      (take-retry:do %annotations src.bowl path)
    ::
        %link-listen-action
      ?>  (team:title [our src]:bowl)
      =^  cards  state
        ~|  p.vase
        (handle-listen-action:do !<(action:link-listen-hook vase))
      [cards this]
    ==
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
  ++  on-peek
    |=  =path
    ^-  (unit (unit cage))
    ?+  path  ~
      [%x %listening ~]  ``noun+!>(listening)
      [%x %listening ^]  ``noun+!>((~(has in listening) t.t.path))
    ==
  ::
  ++  on-watch
    |=  =path
    ^-  (quip card _this)
    ?.  ?=([%listening ~] path)  (on-watch:def path)
    ?>  (team:title [our src]:bowl)
    :_  this
    [%give %fact ~ %link-listen-update !>([%listening listening])]~
  ::
  ++  on-leave  on-leave:def
  ++  on-fail   on-fail:def
  --
::
::
|_  =bowl:gall
+*  md  ~(. mdl bowl)
::
::  user actions & updates
::
++  handle-listen-action
  |=  =action:link-listen-hook
  ^-  (quip card _state)
  ::NOTE  no-opping where appropriate happens further down the call stack.
  ::      we *could* no-op here, as %watch when we're already listening should
  ::      result in no-ops all the way down, but walking through everything
  ::      makes this a nice "resurrect if broken unexpectedly" option.
  ::
  =*  app-path  path.action
  =^  cards  listening
    ^-  (quip card _listening)
    =/  had=?  (~(has in listening) app-path)
    ?-  -.action
        %watch
      :_  (~(put in listening) app-path)
      ?:(had ~ [(send-update action)]~)
    ::
        %leave
      :_  (~(del in listening) app-path)
      ?.(had ~ [(send-update action)]~)
    ==
  =/  groups=(list group-path)
    (groups-from-resource:md %link app-path)
  |-
  ?~  groups  [cards state]
  =^  more-cards  state
    ?-  -.action
      %watch  (listen-to-group app-path i.groups)
      %leave  (leave-from-group app-path i.groups)
    ==
  $(cards (weld cards more-cards), groups t.groups)
::
++  send-update
  |=  =update:link-listen-hook
  ^-  card
  [%give %fact ~[/listening] %link-listen-update !>(update)]
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
    =^  more-cards  state
      =,  i.socs
      ?.  =(%link app-name)  [~ state]
      %-  handle-metadata-update
      [%add group-path [%link app-path] *metadata]
    $(socs t.socs, cards (weld cards more-cards))
  ::
      %add
    ?>  =(%link app-name.resource.upd)
    =,  resource.upd
    =^  update  listening
      ^-  (quip card _listening)
      ?:  (~(has in listening) app-path)
        [~ listening]
      :-  [(send-update %watch app-path)]~
      (~(put in listening) app-path)
    =^  cards  state
      (listen-to-group app-path group-path.upd)
    [(weld update cards) state]
  ::
      %remove
    ?>  =(%link app-name.resource.upd)
    =?  listening
        ?=(~ (groups-from-resource:md %link app-path.resource.upd))
      (~(del in listening) app-path.resource.upd)
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
  ?.  (~(has in listening) i.socs)
    loop-socs(socs t.socs)
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
  =.  reasoning  (~(del ju reasoning) [who app-path] group-path)
  ::NOTE  leaving is always safe, so we just do it unconditionally
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
  ^-  (quip card _state)
  =.  retry-timers  (~(del by retry-timers) [%local-pages who where])
  =.  retry-timers  (~(del by retry-timers) [%annotations who where])
  :_  state
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
  ?.  (~(has in listening) where.target)
    |
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
