/+  view=graph-view, store=graph-store, sigs=signatures, default-agent, dbug
~%  %graph-view-top  ..is  ~
|%
+$  card  card:agent:gall
+$  versioned-state
  $%  state-0
  ==
::
+$  state-0
  $:  %0
      connections=(map atom:store time)
  ==
--
::
=|  state-0
=*  state  -
::
%-  agent:dbug
^-  agent:gall
~%  %graph-view-agent  ..card  ~
|_  =bowl:gall
+*  this       .
    def        ~(. (default-agent this %|) bowl)
::
++  on-init
  ^-  (quip card _this)
  :_  this
  [%pass /updates %agent [our.bowl %graph-store] %watch /updates]~
::
++  on-poke
  ~/  %graph-view-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  |^
  ?>  (team:title our.bowl src.bowl)
  =^  cards  state
    ?+  mark                (on-poke:def mark vase)
        %graph-action       (action !<(action:store vase))
        %json               (action (action:dejs:store !<(json vase)))
        %graph-view-action  (view-action !<(action:view vase))
    ==
  [cards this]
  ::
  ++  action
    |=  =action:store
    ^-  (quip card _state)
    |^
    ::  TODO: decide who to send it to based on resource
    ::
    ?>  ?=(%0 -.action)
    :_  state
    ?+  +<.action           [(poke-store action) ~]
        %add-nodes          (add-nodes +>.action)
        %add-signatures     (add-signatures +>.action)
    ==
    ::
    ++  add-nodes
      |=  [=resource:store nodes=(map index:store node:store)]
      ^-  (list card)
      :_  ~
      %-  poke-store
      :-  %0
      :+  %add-nodes
        resource
      (sign-nodes resource nodes)
    ::
    ++  add-signatures
      |=  [=uid:store =signatures:store]
      ^-  (list card)
      :_  ~
      %-  poke-store
      :-  %0
      :+  %add-signatures
        uid
      =*  resource  resource.uid
      =*  index     index.uid
      =*  ship      entity.resource.uid
      =*  name      name.resource.uid
      %-  ~(gas in *signatures:store)
      :_  ~
      -:(sign-node resource (scry-for-node ship name index))
    ::
    ++  sign-nodes
      |=  [=resource:store nodes=(map index:store node:store)]
      ^-  (map index:store node:store)
      %-  ~(run by nodes)
      |=  =node:store
      ^-  node:store
      +:(sign-node resource node)
    ::
    ++  sign-node
      |=  [=resource:store =node:store]
      ^-  [signature:store node:store]
      =*  p  post.node
      =*  ship  entity.resource
      =*  name  name.resource
      =/  parent-hash  (scry-for-parent-hash ship name index.p)
      =/  =validated-portion:store
        [parent-hash author.p index.p time-sent.p contents.p]
      =/  =hash:store  (mug validated-portion)
      =/  =signature:store  (sign:sigs our.bowl now.bowl hash)
      :-  signature
      %_  node
          hash.post  `hash
          signatures.post
        %-  ~(uni in signatures.post.node)
        (~(gas in *signatures:store) [signature]~)
      ==  
    ::
    ++  scry-for-node
      |=  [=ship =term =index:store]
      ^-  node:store
      %+  scry-for  node:store
      %+  weld
        /node/(scot %p ship)/[term]
      (index-to-path index)
    ::
    ++  scry-for-parent-hash
      |=  [=ship =term =index:store]
      ^-  (unit hash:store)
      ?~  index    ~
      ?~  t.index  ~
      =/  lngth=@  (dec (lent index))
      =/  ind=index:store  `(list atom)`(scag lngth `(list atom)`index)
      =/  parent=node:store
        %+  scry-for  node:store
        %+  weld
          /node/(scot %p ship)/[term]
        (index-to-path ind)
      hash.post.parent
    ::
    ++  index-to-path
      |=  =index:store
      ^-  path
      %+  turn  index
      |=  i=atom:store
      (scot %ud i)
    ::
    ++  poke-store
      |=  =action:store
      ^-  card
      :*  %pass
          /(scot %da now.bowl)
          %agent
          [our.bowl %graph-store]
          %poke
          [%graph-action !>(action)]
      ==
    --
  ::
  ++  view-action
    |=  =action:view
    ^-  (quip card _state)
    ?-  -.action
        %fetch  (fetch +.action)
    ==
  ::
  ++  scry-for
    |*  [=mold =path]
    .^  mold
      %gx
      (scot %p our.bowl)
      %graph-store
      (scot %da now.bowl)
      (snoc `^path`path %noun)
    ==
  ::
  ++  fetch
    |=  [conn=atom:store type=fetch-type:view]
    ^-  (quip card _state)
    =/  keys    (scry-for resources:store /keys)
    :_  state
    :-  (give conn [%graph-update !>([%0 [%keys keys]])])
    %+  turn  ~(tap in keys)
    |=  [=ship =term]
    (give conn [%graph-update !>((add-graph ship term))])
  ::
  ++  add-graph
    |=  [=ship =term]
    ^-  update:store
    :-  %0
    :+  %add-graph
      [ship term]
    (scry-for graph:store /graph/(scot %p ship)/[term])
  ::
  ++  give
      |=  [conn=atom:store =cage]
      ^-  card
      [%give %fact [/updates/(scot %ud conn)]~ cage]
  --
::
++  on-watch
  ~/  %graph-view-watch
  |=  =path
  ^-  (quip card _this)
  ?>  (team:title our.bowl src.bowl)
  ?+  path  (on-watch:def path)
      [%updates @ ~]
    :-  [%give %fact ~ %json !>([(frond:enjs:format %graph-view s+'bound')])]~
    this(connections (~(put by connections) (slav %ud i.t.path) now.bowl))
  ==
::
++  on-agent
  |=  [=wire =sign:agent:gall]
  ^-  (quip card _this)
  |^
  ?+  -.sign  (on-agent:def wire sign)
      %kick
    :_  this
    [%pass /updates %agent [our.bowl %graph-store] %watch /updates]~
  ::
      %fact
    ?+  p.cage.sign  (on-agent:def wire sign)
        %graph-update
      :_  this
      %+  give
        %+  turn  ~(tap by connections)
        |=  [=atom:store *]
        ^-  path
        /updates/(scot %ud atom)
      cage.sign
    ==
  ==
  ::
  ++  give
    |=  [paths=(list path) =cage]
    ^-  (list card)
    [%give %fact paths cage]~
  --
::
++  on-save   !>(state)
++  on-load   on-load:def
++  on-arvo   on-arvo:def
++  on-leave  on-leave:def
++  on-peek   on-peek:def
++  on-fail   on-fail:def
--
