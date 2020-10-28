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
      watching=(set resource)
      mentions=_&
      watch-on-self=_&
  ==
::
--
::
=|  state-0
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
  |^
  =^  cards  state
    ?+  path  (on-watch:def path)
      [%updates ~]  updates
    ==
  [cards this]
  ::
  ++  updates
    ^-  (quip card _state)
    :_  state
    %+  give:ha  ~
    :*  %initial
        watching
        mentions
        watch-on-self
    ==
  --
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
    ?-  -.action
      %listen  (listen +.action)
      %ignore  (ignore +.action)
      %set-mentions  (set-mentions +.action)
      %set-watch-on-self  (set-watch-on-self +.action)
    ==
    ++  listen
      |=  graph=resource
      ^-  (quip card _state)
      :-  (give:ha ~[/updates] [%listen graph])
      state(watching (~(put in watching) graph))
    ::
    ++  ignore
      |=  graph=resource
      ^-  (quip card _state)
      :-  (give:ha ~[/updates] [%ignore graph])
      state(watching (~(del in watching) graph))
    ::
    ++  set-mentions
      |=  ment=?
      ^-  (quip card _state)
      :-  (give:ha ~[/updates] %set-mentions ment)
      state(mentions ment)
    ::
    ++  set-watch-on-self
      |=  self=?
      ^-  (quip card _state)
      :-  (give:ha ~[/updates] %set-watch-on-self self)
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
  ::
  ++  graph-update
    |=  =update:graph-store
    ^-  (quip card _state)
    |^
    ?.  ?=(%add-nodes -.q.update)
      [~ state]
    =/  group=resource
      (need (group-from-app-resource:met %graph resource.q.update))
    =/  =metadata:metadata-store
      (need (peek-metadata:met %graph group resource.q.update))
    =*  rid  resource.q.update
    =+  (scry ,mark=(unit mark) /gx/graph-store/graph-mark/(scot %p entity.rid)/[name.rid]/noun)
    =+  (scry ,=tube:clay /cc/[q.byk.bowl]/[(fall mark %graph-validator-link)]/notification-kind)
    ^-  (quip card _state)
    =/  nodes=(list [p=index:graph-store q=node:graph-store])
      ~(tap by nodes.q.update)
    =|  cards=(list card)
    |-
    ?~  nodes 
      [cards state]
    =*  index  p.i.nodes
    =*  node   q.i.nodes
    ?:  =(our.bowl author.post.node)
      =^  self-cards  state
        (self-post rid node)
      $(cards (weld cards self-cards), nodes t.nodes)
    =+  !<(notification-kind=(unit @t) (tube !>([0 post.node])))
    ?~  notification-kind
      $(nodes t.nodes)
    =/  desc=@t
      ?:  (is-mention contents.post.node)
        %mention 
      u.notification-kind
    ?.  ?|  =(desc %mention)
            (~(has in watching) rid)
        ==
      $(nodes t.nodes)
    =/  notif-index=index:store
      [%graph group rid module.metadata desc]
    =/  =contents:store
      [%graph (limo post.node ~)]
    %_    $
        nodes  t.nodes
        cards  
      :_  cards
      (add-unread notif-index [time-sent.post.node %.n contents]) 
    ==
    ::
    ++  is-mention
      |=  contents=(list content:post)
      ^-  ?
      ?.  mentions  %.n
      ?~  contents  %.n
      ?.  ?=(%text -.i.contents)
        $(contents t.contents)
      =/  res
         (find (scow %p our.bowl) (trip text.i.contents))       
      ?^  res
        %.y
      $(contents t.contents)
    ::
    ++  self-post
      |=  [rid=resource =node:graph-store]
      ^-  (quip card _state)
      ?.  ?=(%.y watch-on-self)
        [~ state]
      `state(watching (~(put in watching) rid))
    ::
    ++  add-unread
      |=  [=index:store =notification:store]
      ^-  card 
      =-  [%pass / %agent [our.bowl %hark-store] %poke -]
      hark-action+!>([%add index notification])
    ::
    ++  scry
      |*  [=mold p=path]
      ?>  ?=(^ p)
      ?>  ?=(^ t.p)
      .^(mold i.p (scot %p our.bowl) i.t.p (scot %da now.bowl) t.t.p)
    --
  --
::
++  on-peek  on-peek:def
::
++  on-leave  on-leave:def
++  on-arvo  on-arvo:def
++  on-fail   on-fail:def
--
|_  =bowl:gall
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
