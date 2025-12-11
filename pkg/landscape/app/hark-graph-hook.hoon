::  hark-graph-hook: notifications for graph-store [landscape]
::
/-  post, group-store, metadata=metadata-store, hook=hark-graph-hook, store=hark-store
/-  hist=hark-store-historical
/+  resource, mdl=metadata, default-agent, dbug, graph-store, graph, grouplib=group
/+  agentio
::
::
~%  %hark-graph-hook-top  ..part  ~
|%
+$  card  card:agent:gall
+$  versioned-state
  $%  state-0
      state-1
      state-2
  ==
::
+$  state-0
  [%0 base-state-0]
::
+$  state-1
  [%1 base-state-0]
::
+$  state-2
  [%2 base-state-1]
::
+$  base-state-0
  $:  watching=(set [resource index:post])
      mentions=_&
      watch-on-self=_&
  ==
::
+$  base-state-1
  $:  watching=(set [resource index:post])
      mentions=_&
      watch-on-self=_&
      places=(jug resource place:store)
  ==
--
::
=|  state-2
=*  state  -
::
=<
%-  agent:dbug
^-  agent:gall
~%  %hark-graph-hook-agent  ..card  ~
|_  =bowl:gall
+*  this  .
    ha    ~(. +> bowl)
    def   ~(. (default-agent this %|) bowl)
    met   ~(. mdl bowl)
    grp   ~(. grouplib bowl)
    gra   ~(. graph bowl)
    io    ~(. agentio bowl)
    pass  pass:io
::
++  on-init
  :_  this
  ~[watch-graph:ha]
::
++  on-save  !>(state)
++  on-load
  |=  =vase
  ^-  (quip card _this)
  =+  !<(old=versioned-state vase)
  =|  cards=(list card)
  |-
  ?-  -.old
      ?(%1 %0)
    %_  $
    ::
        old
      %*  .  *state-2
        watching       watching.old
        mentions       mentions.old
        watch-on-self  watch-on-self.old
      ==
    ==
  ::
      %2
    :_  this(state old)
    =.  cards  (flop cards)
    ?:  (~(has by wex.bowl) [/graph our.bowl %graph-store])
      cards
    [watch-graph:ha cards]
  ==
::
++  on-watch
  |=  =path
  ^-  (quip card _this)
  =^  cards  state
    ?+    path  (on-watch:def path)
      ::
        [%updates ~]
      :_  state
      %+  give:ha  ~
      :*  %initial
          watching
          mentions
          watch-on-self
      ==
    ==
  [cards this]
::
++  on-poke
  ~/  %hark-graph-hook-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  |^
  ?>  (team:title our.bowl src.bowl)
  =^  cards  state
    ?+  mark           (on-poke:def mark vase)
        %hark-graph-migrate
      =+  !<(old=versioned-state:hist vase)
      ?.  ?=(%7 -.old)  ~|(%old-hark-dropping !!)
      (hark-graph-migrate old)
    ::
        %hark-graph-hook-action
      (hark-graph-hook-action !<(action:hook vase))
    ::
        %noun
      (poke-noun !<(* vase))
    ==
  [cards this]
  ::
  ++  hark-graph-migrate
    |=  old=state-7:hist
    =|  cards=(list card)
    |^
    [(flop get-places) state]
    ::
    ++  hark
      |=  =action:store
      [(poke-our:pass %hark-store hark-action+!>(action)) cards]
    ::
    ++  get-places
      ^-  (list card)
      =/  stats-indices=(set stats-index:hist)
        (~(uni in ~(key by last-seen.old)) ~(key by unreads-count.old))
      %-  zing
      (turn ~(tap in stats-indices) get-stats)
    ::
    ++  get-stats
      |=  =stats-index:hist
      ^-  (list card)
      =/  place=(unit place:store)
        (stats-index-to-place stats-index)
      ?~  place  ~
      =/  count  (~(get by unreads-count.old) stats-index)
      =?  cards  ?=(^ count)
        (hark %unread-count u.place & u.count)
      =/  last   (~(get by last-seen.old) stats-index)
      =?  cards  ?=(^ last)
        (hark %saw-place u.place `u.last)
      cards
    ::
    ++  stats-index-to-place
      |=  =stats-index:hist
      ^-  (unit place:store)
      ?.  ?=(%graph -.stats-index)  ~
      `(get-place [graph index]:stats-index)
    --
  ::
  ++  poke-noun
    |=  non=*
    [~ state]
  ::
  ++  hark-graph-hook-action
    |=  =action:hook
    ^-  (quip card _state)
    |^
    :-  (give:ha ~[/updates] action)
    ?-  -.action
      %listen  (listen +.action)
      %ignore  (ignore +.action)
      %set-mentions  (set-mentions +.action)
      %set-watch-on-self  (set-watch-on-self +.action)
    ==
    ++  listen
      |=  [graph=resource =index:post]
      ^+  state
      state(watching (~(put in watching) [graph index]))
    ::
    ++  ignore
      |=  [graph=resource =index:post]
      ^+  state
      state(watching (~(del in watching) [graph index]))
    ::
    ++  set-mentions
      |=  ment=?
      ^+  state
      state(mentions ment)
    ::
    ++  set-watch-on-self
      |=  self=?
      ^+  state
      state(watch-on-self self)
    --
  --
::
++  on-agent
  ~/  %hark-graph-hook-agent
  |=  [=wire =sign:agent:gall]
  ^-  (quip card _this)
  |^
  ?+  -.sign  (on-agent:def wire sign)
      %kick
    :_  this
    ?.  ?=([%graph ~] wire)
      ~
    ~[watch-graph:ha]
  ::
      %fact
    ?.  ?=(%graph-update-3 p.cage.sign)
      (on-agent:def wire sign)
    =^  cards  state
      (graph-update !<(update:graph-store q.cage.sign))
    [cards this]
  ==
  ::
  ++  graph-update
    |=  =update:graph-store
    ^-  (quip card _state)
    ?+    -.q.update  `state
        %add-graph  (add-graph resource.q.update)
      ::
        ?(%remove-graph %archive-graph)
      (remove-graph resource.q.update)
    ::
        %remove-posts
      (remove-posts resource.q.update indices.q.update)
    ::
        %add-nodes
      =*  rid  resource.q.update
      =/  assoc=(unit association:metadata)
        (peek-association:met %graph rid)
      (check-nodes ~(val by nodes.q.update) rid assoc)
    ==
  ::  this is awful, but notification kind should always switch
  ::  on the index, so hopefully doesn't matter
  ::  TODO: rethink this
  ++  remove-posts
    |=  [rid=resource indices=(set index:graph-store)]
    =/  cor=(unit _handle-update:ha)  (abed:handle-update:ha rid)
    ?~  cor  `state
    abet:remove-posts:u.cor
  ::
  ++  poke-hark
    |=  =action:store
    ^-  card
    [%pass / %agent [our.bowl %hark-store] %poke hark-action+!>(action)]
  ::
  ++  remove-graph
    |=  rid=resource
    =/  unwatched
      %-  ~(gas in *(set [resource index:graph-store]))
      %+  skim  ~(tap in watching)
      |=  [r=resource idx=index:graph-store]
      =(r rid)
    :_
      %_  state
        watching  (~(dif in watching) unwatched)
        places    (~(del by places) rid)
      ==
    %+  turn  ~(tap in (~(get ju places) rid))
    |=  =place:store
    (poke-hark %del-place place)
    ::  XX: fix
  ::
  ++  add-graph
    |=  rid=resource
    ^-  (quip card _state)
    =/  graph=graph:graph-store  :: graph in subscription is bunted
      (get-graph-mop:gra rid)
    =/  node=(unit node:graph-store)
      (bind (pry:orm:graph-store graph) |=([@ =node:graph-store] node))
    =/  assoc=(unit association:metadata)
      (peek-association:met %graph rid)
    =^  cards  state
      (check-nodes (drop node) rid assoc)
    [cards state]
  ::
  ++  check-nodes
    |=  $:  nodes=(list node:graph-store)
            rid=resource
            assoc=(unit association:metadata)
        ==
    =/  cor=(unit _handle-update)  (abed:handle-update:ha rid)
    ?~  cor  `state
    abet:(add-nodes:u.cor nodes)
  --
::
++  on-peek  on-peek:def
::
++  on-leave  on-leave:def
++  on-arvo
  |=  [=wire =sign-arvo]
  ^-  (quip card _this)
  ?+  wire  (on-arvo:def wire sign-arvo)
    ::
    ::  no longer necessary
    [%validator @ ~]  [~ this]
  ==
++  on-fail   on-fail:def
--
::
|_  =bowl:gall
+*  met   ~(. mdl bowl)
    grp   ~(. grouplib bowl)
    gra   ~(. graph bowl)
::
++  graph-index-to-path
  |=  =index:graph-store
  ^-  path
  (turn index (cork (cury scot %ui) (cury rsh 4)))
::
++  get-place
  |=  [rid=resource =index:graph-store]
  :-  q.byk.bowl
  %+  welp  /graph/(scot %p entity.rid)/[name.rid]
  (graph-index-to-path index)
::
++  summarize
  |=  contents=(list content:post)
  %+  rap  3
  %+  join  ' '
  %+  turn  contents
  |=  =content:post
  ?-  -.content
    %text  text.content
    %url   url.content
    %code  '<Code fragment>'
    %reference  '<A reference>'
    %mention    (scot %p ship.content)
  ==
::
++  give
  |=  [paths=(list path) =update:hook]
  ^-  (list card)
  [%give %fact paths hark-graph-hook-update+!>(update)]~
::
++  watch-graph
  ^-  card
  [%pass /graph %agent [our.bowl %graph-store] %watch /updates]
::
++  poke-hark
  |=  =action:store
  ^-  card
  =-  [%pass / %agent [our.bowl %hark-store] %poke -]
  hark-action+!>(action)
::
++  flatten-nodes
  |=  nodes=(list node:graph-store)
  %+  roll  nodes
  |=  [=node:graph-store out=(list node:graph-store)]
  ^-  (list node:graph-store)
  %+  welp  [node(children empty/~) out]
  ?:  ?=(%empty -.children.node)  ~
  (flatten-nodes (turn (bap:orm:graph-store p.children.node) tail))
::
++  handle-update
  |_  $:  rid=resource  ::  input
          =mark
          hark-pokes=(list action:store)  :: output
          new-watches=(list index:graph-store)
      ==
  ++  update-core  .
  ::
  ++  abed
    |=  r=resource
    ^-  (unit _update-core)
    =/  m=(unit ^mark)
      (get-mark:gra r)
    ?~  m  ~
    :-  ~
    %_  update-core
      rid      r
      mark     u.m
    ==
  ::
  ++  title
    ~+  ^-  cord
    =/  gra-met  (peek-association:met %graph rid)
    ?~  gra-met  (crip "{(scow %p entity.rid)}/{(trip name.rid)}")
    =/  grp-met  (peek-association:met %groups group.u.gra-met)
    =*  gra-title  title.metadatum.u.gra-met
    ?~  grp-met  gra-title
    (rap 3 title.metadatum.u.grp-met ': ' gra-title ~)
  ::
  ++  abet
    ^-  (quip card _state)
    :_  state(watching (~(uni in watching) (silt (turn new-watches (lead rid)))))
    ^-  (list card)
    %+  welp  (turn (flop hark-pokes) poke-hark)
    %-  zing
    %+  turn  (flop new-watches)
    |=(=index:graph-store (give ~[/updates] [%listen rid index]))
  ::
  ++  hark
    |=  =action:store
    ^+  update-core
    update-core(hark-pokes [action hark-pokes])
  ::
  ++  add-nodes
    |=  updates=(list node:graph-store)
    =.  updates  (flatten-nodes updates)
    |-  ^+  update-core
    ?~  updates  update-core
    =/  cor=(unit _post-core)
      (abed:post-core i.updates)
    ?~  cor  $(updates t.updates)
    $(updates t.updates, update-core abet:added:u.cor)
  ::
  ++  remove-posts
    |=  indices=(list index:graph-store)
    |-  ^+  update-core
    ?~  indices  update-core
    =|  =post:graph-store
    =.  index.post  i.indices
    =/  =node:graph-store
      [&/post empty/~]
    =/  cor=(unit _post-core)
      (abed:post-core node)
    ?~  cor  $(indices t.indices)
    $(indices t.indices, update-core abet:removed:u.cor)
  ::
  ++  post-core
    |_  [kind=notif-kind:hook =post:graph-store]
    ++  post-core  .
    ++  abet
      =.  places  (~(put ju places) rid place)
      update-core
    ++  abed
      |=  =node:graph-store
      ^-  (unit _post-core)
      ?:  ?=(%| -.post.node)  ~
      =/  not-kind  (notif-kind p.post.node)
      ?~  not-kind  ~
      `post-core(post p.post.node, kind u.not-kind)
    ++  parent-idx
      (scag parent.index-len:kind index.post)
    ++  self-idx
      (scag self.index-len:kind index.post)
    ++  is-mention
      =/  contents  contents.post
      |-  ^-  ?
      ?.  mentions  %.n
      ?~  contents  %.n
      ?.  ?=(%mention -.i.contents)
        $(contents t.contents)
      ?:  =(our.bowl ship.i.contents)
        %.y
      $(contents t.contents)
    ::
    ++  bin
      ^-  bin:store
      [?:(is-mention /mention /) place]
    ::
    ++  place
      ^-  place:store
      (get-place rid parent-idx)
    ::
    ++  should-notify
      ?|  is-mention
          (~(has in watching) [rid parent-idx])
          =(mark %graph-validator-dm)
      ==
    ::
    ++  add-note
      ^+  post-core
      ?.  should-notify  post-core
      =/  title=(list content:store)
        ?:  =(title (crip "{(scow %p our.bowl)}/dm-inbox"))  title.kind
        ?.  is-mention   title.kind
        ~[text/(rap 3 'You were mentioned in ' title ~)]
      =/  link=path
        %+  welp  /[mark]/(scot %p entity.rid)/[name.rid]
        (graph-index-to-path index.post)
      =/  =body:store
        [title body.kind now.bowl path:bin link]
      post-core(update-core (hark %add-note bin body))
    ::
    ++  added-unread
      ^+  post-core
      %_  post-core
          update-core
        ?-  mode.kind
          %count  (hark %unread-count place %.y 1)
          %each   (hark %unread-each place /(rsh 4 (scot %ui (rear self-idx))))
          %none   update-core
        ==
      ==
    ::
    ++  removed-unread
      ^+  post-core
      %_  post-core
          update-core
        ?-  mode.kind
          %count  (hark %unread-count place %.n 1)
          %each   (hark %read-each place /(rsh 4 (scot %ui (rear self-idx))))
          %none   update-core
        ==
      ==
    ::
    ++  added
      ^+  post-core
      ?:  =(our.bowl author.post)  self-post
      =>  added-unread
      add-note
    ::
    ::  TODO: delete notifications?
    ++  removed
      ^+  post-core
      removed-unread
    ::
    ++  self-post
      ^+  post-core
      =.  update-core
        (hark %saw-place place ~)
      =?  update-core  ?=(%count mode.kind)
        (hark %read-count place.bin)
      new-watch
    ::
    ++  new-watch
      ^+  post-core
      ?.  watch-on-self  post-core
      =/  watch-for  watch-for:kind
      =?  new-watches  =(%siblings watch-for)
        [parent-idx new-watches]
      =?  new-watches  =(%children watch-for)
        [self-idx new-watches]
      post-core
    --
  ::
  ++  notif-kind
    |=  p=post:graph-store
    ^-  (unit notif-kind:hook)
    |^
    ?+  mark  ~
      %graph-validator-chat     chat
      %graph-validator-publish  publish
      %graph-validator-link     link
      %graph-validator-dm       dm
      %graph-validator-post     post
    ==
    ::
    ++  chat
      ?.  ?=([@ ~] index.p)  ~
      :-  ~
      :*  ~[text+(rap 3 'New messages in ' title ~)]
          [ship+author.p text+': ' (hark-contents:graph-store contents.p)]
          [0 1]  %count  %none
      ==
    ::
    ++  publish
      ^-  (unit notif-kind:hook)
      ?+  index.p   ~
          [@ %1 %1 ~]
        :-  ~
        :*  [%text (rap 3 'New notes in ' title ~)]~
            ~[(hark-content:graph-store (snag 0 contents.p)) text+' by ' ship+author.p]
            [0 1]  %each  %children
        ==
      ::
          [@ %2 @ %1 ~]
        :-  ~
        :*  [%text (rap 3 'New comments in ' title ~)]~
            [ship+author.p text+': ' (hark-contents:graph-store contents.p)]
            [1 3]  %count  %siblings
        ==
      ==
    ::
    ++  link
      ^-  (unit notif-kind:hook)
      ?+  index.p  ~
          [@ ~]
        :-  ~
        :*  [text+(rap 3 'New links in ' title ~)]~
            [ship+author.p text+': ' (hark-contents:graph-store contents.p)]
            [0 1]  %each  %children
        ==
      ::
          [@ @ %1 ~]
        :-  ~
        :*  [text+(rap 3 'New comments on a post in ' title ~)]~
            [ship+author.p text+': ' (hark-contents:graph-store contents.p)]
            [1 2]  %count  %siblings
        ==
      ==
    ::
    ++  post
      =/  len  (lent index.p)
      =/  =mode:hook
        ?:(=(1 len) %count %none)
      :-  ~
      :*  ~[text+(rap 3 'Your post in ' title ' received replies ' ~)]
          [ship+author.p text+': ' (hark-contents:graph-store contents.p)]
          [(dec len) len]  mode  %children
      ==
    ::
    ++  dm
      ?+  index.p  ~
          [@ @ ~]
        :-  ~
        :*  ~[text+'New messages from ' ship+author.p]
            (hark-contents:graph-store contents.p)
            [1 2]  %count  %none
        ==
      ==
    --
  ::
  --
--
