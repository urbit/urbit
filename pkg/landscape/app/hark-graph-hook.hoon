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
      places=(map resource place:store)
  ==
::
++  scry
  |*  [[our=@p now=@da] =mold p=path]
  ?>  ?=(^ p)
  ~!  p
  ?>  ?=(^ t.p)
  .^(mold i.p (scot %p our) i.t.p (scot %da now) t.t.p)
::
++  scry-notif-conversion
  |=  [[our=@p now=@da] desk=term =mark]
  ^-  $-(indexed-post:graph-store $-(cord (unit notif-kind:hook)))
  %^  scry  [our now]
    $-(indexed-post:graph-store $-(cord (unit notif-kind:hook)))
  /cf/[desk]/[mark]/notification-kind
--
::
=|  state-1
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
  =.  cards  [watch-graph:ha cards]
  |-
  ?:  ?=(%0 -.old)
    %_    $
      -.old  %1
      ::
        cards  
      :_  cards
      [%pass / %agent [our dap]:bowl %poke noun+!>(%rewatch-dms)]
    ==
  :_  this(state old)
  =.  cards  (flop cards)
  ?:  (~(has by wex.bowl) [/graph our.bowl %graph-store])
    cards
  [watch-graph:ha cards]
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
        (hark %seen-index u.place `u.last)
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
::    ?>  ?=(%rewatch-dms non)
::    =/  graphs=(list resource)
::      ~(tap in get-keys:gra)
::    %_   state
::        watching  
::      %-  ~(gas in watching)
::      (murn graphs |=(rid=resource ?:((should-watch:ha rid) `[rid ~] ~)))
::    ==
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
    ?.  ?=(%graph-update-2 p.cage.sign)
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
    =/  to-remove
      %-  ~(gas by *(set [resource index:graph-store]))
      (turn ~(tap in indices) (lead rid))
    :_  state(watching (~(dif in watching) to-remove))
    =/  convert  (get-conversion:ha rid '')
    %+  roll
      ~(tap in indices)
    |=  [=index:graph-store out=(list card)]
    =|  =indexed-post:graph-store
    =.  index.p.indexed-post  index
    =/  notif-kind=(unit notif-kind:hook)
      (convert indexed-post)
    ?~  notif-kind  out
    =/  =place:store
      (get-place rid index)
    ?.  ?=(%each mode.u.notif-kind)  out
    :_  out 
    (poke-hark %read-each place index)
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
    :_  state(watching (~(dif in watching) unwatched))
    ^-  (list card)
    ~
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
    ?.  (should-watch:ha rid assoc)
      [cards state]
    :_   state(watching (~(put in watching) [rid ~]))
    (weld cards (give:ha ~[/updates] %listen [rid ~]))
  ::
  ++  check-nodes
    |=  $:  nodes=(list node:graph-store)
            rid=resource
            assoc=(unit association:metadata)
        ==
    abet:check:(abed:handle-update:ha rid nodes)
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
++  get-place
  |=  [rid=resource =index:graph-store]
  :-  q.byk.bowl
  (welp /graph/(scot %p entity.rid)/[name.rid] (graph-index-to-path index))
::
++  get-bin
  |=  [rid=resource parent=index:graph-store is-mention=?]
  [?:(is-mention /mention /) (get-place rid parent)]
::
++  get-conversion
  |=  [rid=resource title=cord]
  ^-  $-(indexed-post:graph-store (unit notif-kind:hook))
  =+  %^  scry  [our now]:bowl
         ,mark=(unit mark)
      /gx/graph-store/graph/(scot %p entity.rid)/[name.rid]/mark/noun
  ?~  mark
    |=(=indexed-post:graph-store ~)
  =/  f=$-(indexed-post:graph-store $-(cord (unit notif-kind:hook)))
    (scry-notif-conversion [our now]:bowl q.byk.bowl u.mark)
  |=  =indexed-post:graph-store
  ((f indexed-post) title)
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
++  is-mention
  |=  contents=(list content:post)
  ^-  ?
  ?.  mentions  %.n
  ?~  contents  %.n
  ?.  ?=(%mention -.i.contents)
    $(contents t.contents)
  ?:  =(our.bowl ship.i.contents)
    %.y
  $(contents t.contents)
::
++  should-watch
  |=  [rid=resource assoc=(unit association:metadata)]
  ^-  ?
  ?~  assoc
    %.y
  &(watch-on-self =(our.bowl entity.rid))
::
++  handle-update
  |_  $:  rid=resource  ::  input
          updates=(list node:graph-store)
          mark=(unit mark)
          hark-pokes=(list action:store)  :: output
          new-watches=(list index:graph-store)
      ==
  ++  update-core  .
  ::
  ++  abed
    |=  [r=resource upds=(list node:graph-store)]
    =/  m=(unit ^mark)
      (get-mark:gra r)
    update-core(rid r, updates upds, mark m)
  ::
  ++  title
    ~+  title:(fall (peek-metadatum:met %graph rid) *metadatum:metadata)
  ::
  ++  get-conversion
    ~+  (^get-conversion rid title)
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
  ++  new-watch
    |=  [=index:graph-store =watch-for:hook =index-len:hook]
    =?  new-watches  =(%siblings watch-for)
      [(scag parent.index-len index) new-watches]
    =?  new-watches  =(%children watch-for)
      [(scag self.index-len index) new-watches]
    update-core
  ::
  ++  check
    |-  ^+  update-core
    ?~  updates  
      update-core
    =/  core=_update-core
      (check-node i.updates)
    =.  updates.core  t.updates
    $(update-core core)
  ::
  ++  check-node-children
    |=  =node:graph-store
    ^+  update-core
    ?:  ?=(%empty -.children.node)
      update-core
    =/  children=(list [=atom =node:graph-store])
      (tap:orm:graph-store p.children.node)
    |-  ^+  update-core
    ?~  children
      update-core
    =.  update-core  (check-node node.i.children)
    $(children t.children)
  ::
  ++  check-node
    |=  =node:graph-store
    ^+  update-core
    =.  update-core  (check-node-children node)
    ?:  ?=(%| -.post.node)
      update-core
    ::?~  mark  update-core
    =*  pos  p.post.node
    =/  notif-kind=(unit notif-kind:hook)
      (get-conversion [0 pos]) 
    ?~  notif-kind
      update-core
    =*  not-kind  u.notif-kind
    =/  parent=index:post
      (scag parent.index-len.not-kind index.pos)
    =/  is-mention  (is-mention contents.pos)
    =/  =bin:store
      (get-bin rid parent is-mention)
    ?:  =(our.bowl author.pos)
      (self-post node bin u.notif-kind)
    =.  update-core
      %^  update-unread-count  u.notif-kind  bin
      (scag self.index-len.not-kind index.pos)
    =?    update-core
        ?|  is-mention
            (~(has in watching) [rid parent])
            =(mark `%graph-validator-dm)
        ==
      =/  link=path
        (welp /(fall mark '')/(scot %p entity.rid)/[name.rid] (graph-index-to-path index.pos))
      =/  =body:store
        [title.not-kind body.not-kind now.bowl path.bin link]
      (add-unread bin body)
    update-core
  ::
  ::
  ++  update-unread-count
    |=  [=notif-kind:hook =bin:store =index:graph-store]
    ?-  mode.notif-kind 
      %count  (hark %unread-count place.bin %.y 1)
      %each   (hark %unread-each place.bin /(rsh 4 (scot %ui (rear index))))
      %none   update-core
    ==
  ::
  ++  self-post
    |=  $:  =node:graph-store
            =bin:store
            =notif-kind:hook
        ==
    ^+  update-core 
    ?>  ?=(%& -.post.node)
    =.  update-core
      (hark %seen-index place.bin `now.bowl)
    =?  update-core  ?=(%count mode.notif-kind)
      (hark %read-count place.bin)
    =?  update-core  watch-on-self
      (new-watch index.p.post.node [watch-for index-len]:notif-kind)
    update-core
  ::
  ++  add-unread
    |=  [=bin:store =body:store]
    (hark %add-note bin body)
  --
--
