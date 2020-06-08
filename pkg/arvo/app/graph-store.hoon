/+  store=graph-store, sigs=signatures, *or-map, default-agent, dbug
|%
+$  card  card:agent:gall
+$  versioned-state
  $%  state-0
  ==
+$  state-0  [%0 network:store]
++  orm      ((or-map atom:store node:store) lth)
++  orm-log  ((or-map time action:store) lth)
--
::
=|  state-0
=*  state  -
::
%-  agent:dbug
^-  agent:gall
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
::
++  on-init  [~ this]
++  on-save  !>(state)
++  on-load
  |=  old=vase
  ^-  (quip card _this)
  ::[~ this]
  [~ this(state !<(state-0 old))]
::
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  |^
  ?>  (team:title our.bowl src.bowl)
  =^  cards  state
    ?+  mark           (on-poke:def mark vase)
        %graph-action  (graph-action !<(action:store vase))
    ==
  [cards this]
  ::
  ++  graph-action
    |=  =action:store
    ^-  (quip card _state)
    |^
    ?>  ?=(%0 -.action)
    ?-  +<.action
        %add-graph          (add-graph +>.action)
        %remove-graph       (remove-graph +>.action)
        %add-nodes          (add-nodes +>.action)
        %remove-nodes       (remove-nodes +>.action)
        %add-signatures     (add-signatures +>.action)
        %remove-signatures  (remove-signatures +>.action)
        %add-tag            (add-tag +>.action)
        %remove-tag         (remove-tag +>.action)
    ==
    ::
    ++  add-graph
      |=  [=resource:store =graph:store]
      ^-  (quip card _state)
      ?<  (~(has by graphs) resource)
      :-  (give [/all /keys ~] [%add-graph resource graph])
      %=  state
          graphs       (~(put by graphs) resource graph)
          action-logs  (~(put by action-logs) resource (gas:orm-log ~ ~))
      ==
    ::
    ++  remove-graph
      |=  =resource:store
      ^-  (quip card _state)
      ?>  (~(has by graphs) resource)
      :-  (give [/all /keys ~] [%remove-graph resource])
      %=  state
          graphs       (~(del by graphs) resource)
          action-logs  (~(del by action-logs) resource)
      ==
    ::
    ++  add-nodes
      |=  [=resource:store nodes=(map index:store node:store)]
      ^-  (quip card _state)
      |^
      =/  =graph:store       (~(got by graphs) resource)
      =/  =action-log:store  (~(got by action-logs) resource)
      =.  action-log
        (put:orm-log action-log now.bowl [%0 [%add-nodes resource nodes]])
      ::
      :-  (give [/all]~ [%add-nodes resource nodes])
      %_  state
          action-logs  (~(put by action-logs) resource action-log)
          graphs
        %+  ~(put by graphs)
          resource
        (add-node-list resource graph ~(tap by nodes))
      ==
      ::
      ++  add-node-list
        |=  $:  =resource:store
                =graph:store
                node-list=(list [index:store node:store])
            ==
        ^-  graph:store
        ?~  node-list  graph
        =*  index  -.i.node-list
        =*  node   +.i.node-list
        %_  $
            node-list  t.node-list
            graph  (add-node-at-index graph index node)
        ==
      ::
      ++  add-node-at-index
        |=  [=graph:store =index:store =node:store]
        ^-  graph:store
        ?~  index  graph
        =*  atom   i.index
        ::  last index in list
        ::
        ?~  t.index
          ::  TODO: validate that hash of node matches
          (put:orm graph atom node)
        ::  multiple indices left in list
        ::
        =/  parent=(unit node:store)  (get:orm graph atom)
        ?~  parent
          ~&  "index does not exist to add a node to!"
          graph
        ?+  -.children.u.parent
          ::  replace empty graph with graph containing one child
          ::
          %^  put:orm
              graph
            atom
          %=  u.parent
              children
            ^-  internal-graph:store
            [%graph $(graph (gas:orm ~ ~), index t.index)]
          ==
        ::
            %graph
          :: recurse into children
          ::
          %^  put:orm
              graph
            atom
          %_  u.parent
              p.children  $(graph p.children.u.parent, index t.index)
          ==
        ==
      --
    ::
    ++  remove-nodes
      |=  [=resource:store indices=(set index:store)]
      ^-  (quip card _state)
      |^
      =/  =graph:store        (~(got by graphs) resource)
      =/  =action-log:store   (~(got by action-logs) resource)
      =.  action-log
        (put:orm-log action-log now.bowl [%0 [%remove-nodes resource indices]])
      ::
      :-  (give [/all]~ [%remove-nodes resource indices])
      %_  state
          action-logs  (~(put by action-logs) resource action-log)
          graphs
        %+  ~(put by graphs)
          resource
        (remove-indices resource graph ~(tap in indices))
      ==
      ::
      ++  remove-indices
        |=  [=resource:store =graph:store indices=(list index:store)]
        ^-  graph:store
        ?~  indices  graph
        %_  $
            indices  t.indices
            graph    (remove-index graph i.indices)
        ==
      ::
      ++  remove-index
        |=  [=graph:store =index:store]
        ^-  graph:store
        ?~  index  graph
        =*  atom   i.index
        ::  last index in list
        ::
        ?~  t.index
          +:`[* graph:store]`(del:orm graph atom)
        ::  multiple indices left in list
        ::
        =/  parent=(unit node:store)  (get:orm graph atom)
        ?~  parent
          ~&  "index does not exist to remove a node from!"
          graph
        ?+  -.children.u.parent
          ~&  "child index does not exist to remove a node from!"
          graph
        ::
            %graph
          :: recurse into children
          ::
          %^  put:orm
              graph
            atom
          %_  u.parent
              p.children  $(graph p.children.u.parent, index t.index)
          ==
        ==
      --
    ::
    ++  add-signatures
      |=  [=uid:store =signatures:store]
      ^-  (quip card _state)
      |^
      =*  resource  resource.uid
      =/  =graph:store       (~(got by graphs) resource)
      =/  =action-log:store  (~(got by action-logs) resource)
      =.  action-log
        (put:orm-log action-log now.bowl [%0 [%add-signatures uid signatures]])
      ::
      :-  (give [/all]~ [%add-signatures uid signatures])
      %_  state
          action-logs  (~(put by action-logs) resource action-log)
          graphs
        (~(put by graphs) resource (add-at-index graph index.uid signatures))
      ==
      ::
      ++  add-at-index
        |=  [=graph:store =index:store =signatures:store]
        ^-  graph:store
        ?~  index  graph
        =*  atom   i.index
        =/  node=(unit node:store)  (get:orm graph atom)
        ?~  node
          ~|("node does not exist to add signatures to!" !!)
        ::  last index in list
        ::
        ?~  t.index
          ::  TODO: finish this
          ?.  (are-signatures-valid:sigs signatures *hash:store now.bowl)
            ~|("signatures did not match public keys!" !!)
          =/  new-signatures  (~(uni in signatures) p.signatures.post.u.node)
          %^  put:orm
              graph
            atom
          %_  u.node
              p.signatures.post  new-signatures
              q.signatures.post  (sha256-mug:sigs new-signatures)
          ==
        ::  multiple indices left in list
        ::
        ?+  -.children.u.node
          ~|("child graph does not exist to add signatures to!" !!)
        ::
            %graph
          :: recurse into children
          ::
          %^  put:orm
              graph
            atom
          %_  u.node
              p.children  $(graph p.children.u.node, index t.index)
          ==
        ==
      --
    ::
    ++  remove-signatures
      |=  [=uid:store =signatures:store]
      ^-  (quip card _state)
      [~ state]
    ::
    ++  add-tag
      |=  [=term =resource:store]
      ^-  (quip card _state)
      ?>  (~(has by graphs) resource)
      :-  (give [/all]~ [%add-tag term resource])
      %_  state
          tag-queries  (~(put ju tag-queries) term resource)
      ==
    ::
    ++  remove-tag
      |=  [=term =resource:store]
      ^-  (quip card _state)
      ?>  (~(has by graphs) resource)
      :-  (give [/all]~ [%remove-tag term resource])
      %_  state
          tag-queries  (~(del ju tag-queries) term resource)
      ==
    ::
    ++  give
      |=  [paths=(list path) update=update-0:store]
      ^-  (list card)
      [%give %fact paths [%graph-update !>([%0 update])]]~
    --
  --
::
++  on-watch
  |=  =path
  ^-  (quip card _this)
  |^
  ?>  (team:title our.bowl src.bowl)
  =/  cards=(list card)
    ?+  path       (on-watch:def path)
        [%all ~]   (give [%initial graphs tag-queries])
        [%keys ~]  (give [%keys ~(key by graphs)])
    ==
  [cards this]
  ::
  ++  give
    |=  update=update-0:store
    ^-  (list card)
    [%give %fact ~ [%graph-update !>([%0 update])]]~
  --
::
++  on-peek
  |=  =path
  ^-  (unit (unit cage))
  ?+  path  (on-peek:def path)
      [%x %keys ~]  ``noun+!>(~(key by graphs))
  ==
::
++  on-arvo  on-arvo:def
++  on-agent  on-agent:def
++  on-leave  on-leave:def
++  on-fail   on-fail:def
--
