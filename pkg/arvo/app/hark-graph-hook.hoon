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
      watching=(jug resource notifiable:hook)
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
++  on-watch  on-watch:def
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
    ==
    ++  listen
      |=  [=notifiable:hook graph=resource]
      ^-  (quip card _state)
      :-  ~
      state(watching (~(put ju watching) graph notifiable))
    ::
    ++  ignore
      |=  [=notifiable:hook graph=resource]
      ^-  (quip card _state)
      :-  ~
      state(watching (~(del ju watching) graph notifiable))
    ::
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
    ?.  ?=(%add-nodes -.q.update)
      [~ state]
    =/  group=resource
      (need (group-from-app-resource:met %graph resource.q.update))
    =/  =metadata:metadata-store
      (need (peek-metadata:met %graph group resource.q.update))
    =*  rid  resource.q.update
    =+  (scry:ha ,mark=(unit mark) /gx/graph-store/graph-mark/(scot %p entity.rid)/[name.rid]/noun)
    ?~  mark
      [~ state]
    =+  (scry:ha ,=tube:clay /cc/[q.byk.bowl]/[u.mark]/notification-kind)
    :_  state
    ^-  (list card)
    %+  murn
      ~(val by nodes.q.update)
    |=  =node:graph-store
    ^-  (unit card)
    =+  !<(notification-kind=(unit @t) (tube !>([0 post.node])))
    ?~  notification-kind
      ~
    =/  =index:store
      [%graph group rid module.metadata u.notification-kind]
    `(add-unread:ha index [time-sent.post.node %.n [%graph contents.post.node]]) 
  --
::
++  on-peek  on-peek:def
::
++  on-leave  on-leave:def
++  on-arvo  on-arvo:def
++  on-fail   on-fail:def
--
|_  =bowl:gall
+*  met  ~(. metadata bowl)
::
++  add-unread
  |=  [=index:store =notification:store]
  ^-  card 
  [%pass / %agent [our.bowl %hark-store] %poke %hark-action !>([%add index notification])]
::
++  watch-graph
  ^-  card
  [%pass /graph %agent [our.bowl %graph-store] %watch /updates]
::
++  scry
  |*  [=mold p=path]
  ?>  ?=(^ p)
  ?>  ?=(^ t.p)
  .^(mold i.p (scot %p our.bowl) i.t.p (scot %da now.bowl) t.t.p)
--
