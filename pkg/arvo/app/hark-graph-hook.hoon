::  hark-graph-hook: notifications for graph-store [landscape]
::
/-  store=hark-store, post, group-store, metadata-store, hook=hark-graph-hook
/+  resource, metadata, default-agent, dbug, graph-store
::
~%  %hark-graph-hook-top  ..is  ~
|%
+$  card  card:agent:gall
+$  versioned-state
  $%  state-0
  ==
::
+$  state-0
  $:  %0
      watching=(set [resource index:post])
      mentions=_&
      watch-on-self=_&
  ==
::
--
::
=|  state-0
=*  state  -
::
=>
  |_  =bowl:gall
  ::
  ++  scry
    |*  [=mold p=path]
    ?>  ?=(^ p)
    ?>  ?=(^ t.p)
    .^(mold i.p (scot %p our.bowl) i.t.p (scot %da now.bowl) t.t.p)
  ::
  ++  give
    |=  [paths=(list path) =update:hook]
    ^-  (list card)
    [%give %fact paths hark-graph-hook-update+!>(update)]~
  ::
  ++  watch-graph
    ^-  card
    [%pass /graph %agent [our.bowl %graph-store] %watch /updates]
  --
%-  agent:dbug
^-  agent:gall
~%  %hark-graph-hook-agent  ..card  ~
|_  =bowl:gall
+*  this  .
    ha    ~(. +> bowl)
    def   ~(. (default-agent this %|) bowl)
    met   ~(. metadata bowl)
::
++  on-init
  :_  this
  ~[watch-graph:ha]
::
++  on-save  !>(state)
++  on-load
  |=  old=vase
  ^-  (quip card _this)
  `this(state !<(state-0 old))
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
        %hark-graph-hook-action
      (hark-graph-hook-action !<(action:hook vase))
    ==
  [cards this]
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
    ?.  ?=(%graph-update p.cage.sign)
      (on-agent:def wire sign)
    =^  cards  state
      (graph-update !<(update:graph-store q.cage.sign))
    [cards this]
  ==
  ++  add-graph
    |=  rid=resource
    ^-  (quip card _state)
    ?.  &(watch-on-self =(our.bowl entity.rid))
      [~ state]
    `state(watching (~(put in watching) [rid ~]))
  ::
  ++  graph-update
    |=  =update:graph-store
    ^-  (quip card _state)
    ?:  ?=(%add-graph -.q.update)
      (add-graph resource.q.update)
    ?.  ?=(%add-nodes -.q.update)
      [~ state]
    =/  group=resource
      (need (group-from-app-resource:met %graph resource.q.update))
    =/  =metadata:metadata-store
      (need (peek-metadata:met %graph group resource.q.update))
    =*  rid  resource.q.update
    =+  %+  scry:ha
           ,mark=(unit mark)
        /gx/graph-store/graph-mark/(scot %p entity.rid)/[name.rid]/noun
    =+  %+  scry:ha
          ,=tube:clay
        /cc/[q.byk.bowl]/[(fall mark %graph-validator-link)]/notification-kind
    =/  nodes=(list [p=index:graph-store q=node:graph-store])
      ~(tap by nodes.q.update)
    =|  cards=(list card)
    |^
    ?~  nodes
      [cards state]
    =*  index  p.i.nodes
    =*  node   q.i.nodes
    =^  node-cards  state
      (check-node node tube)
    %_    $
      nodes  t.nodes
      cards  (weld node-cards cards)
    ==
    ::
    ++  check-node-children
      |=  [=node:graph-store =tube:clay]
      ^-  (quip card _state)
      ?:  ?=(%empty -.children.node)
        [~ state]
      =/  children=(list [=atom =node:graph-store])
        (tap:orm:graph-store p.children.node)
      =|  cards=(list card)
      |-  ^-  (quip card _state)
      ?~  children
        [cards state]
      =^  new-cards  state
        (check-node node.i.children tube)
      %_    $
        cards  (weld cards new-cards)
        children  t.children
      ==
    ::
    ++  check-node
      |=  [=node:graph-store =tube:clay]
      ^-  (quip card _state)
      =^  child-cards  state
        (check-node-children node tube)
      ?:  =(our.bowl author.post.node)
        =^  self-cards  state
          (self-post node)
        :_  state
        (weld child-cards self-cards)
      =+  !<  notif-kind=(unit [name=@t parent-lent=@ud])
          (tube !>([0 post.node]))
      ?~  notif-kind
        [child-cards state]
      =/  desc=@t
        ?:  (is-mention contents.post.node)
          %mention
        name.u.notif-kind
      =/  parent=index:post
        (scag parent-lent.u.notif-kind index.post.node)
      ?.  ?|  =(desc %mention)
              (~(has in watching) [rid parent])
          ==
        [child-cards state]
      =/  notif-index=index:store
        [%graph group rid module.metadata desc]
      =/  =contents:store
        [%graph (limo post.node ~)]
      :_  state
      %+  snoc  child-cards
      (add-unread notif-index [time-sent.post.node %.n contents])
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
    ++  self-post
      |=  =node:graph-store
      ^-  (quip card _state)
      ?.  ?=(%.y watch-on-self)
        [~ state]
      `state(watching (~(put in watching) [rid index.post.node]))
    ::
    ++  add-unread
      |=  [=index:store =notification:store]
      ^-  card
      =-  [%pass / %agent [our.bowl %hark-store] %poke -]
      hark-action+!>([%add index notification])
    ::
    --
  --
::
++  on-peek  on-peek:def
::
++  on-leave  on-leave:def
++  on-arvo  on-arvo:def
++  on-fail   on-fail:def
--

