/+  store=graph-store, default-agent, dbug
|%
+$  card  card:agent:gall
+$  versioned-state
  $%  state-0
  ==
+$  state-0  [%0 network:store]
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
    ?-  -.action
        %add-graph          (add-graph +.action)
        %remove-graph       (remove-graph +.action)
        %add-nodes          (add-nodes +.action)
        %remove-nodes       (remove-nodes +.action)
        %add-signatures     (add-signatures +.action)
        %remove-signatures  (remove-signatures +.action)
        %add-tag            (add-tag +.action)
        %remove-tag         (remove-tag +.action)
    ==
    ::
    ++  add-graph
      |=  [=resource:store =graph:store]
      ^-  (quip card _state)
      ?<  (~(has by graphs) resource)
      :-  (give [/all /keys ~] [%add-graph resource graph])
      state(graphs (~(put by graphs) resource graph))
    ::
    ++  remove-graph
      |=  =resource:store
      ^-  (quip card _state)
      ?>  (~(has by graphs) resource)
      :-  (give [/all /keys ~] [%remove-graph resource])
      state(graphs (~(del by graphs) resource))
    ::
    ++  add-nodes
      |=  nodes=(map resource:store (map index:store node:store))
      ^-  (quip card _state)
      =/  resource-list  ~(tap by nodes)
      |^
      ?~  resource-list
        :_  state
        (give [/all]~ [%add-nodes nodes])
      =*  resource       -.i.resource-list
      =*  indexed-nodes  +.i.resource-list
      =/  graph=(unit graph)  (~(get by graphs) resource)
      ?~  graph
        ~|  "graph {<resource>} does not exist to add a node to!"
        $(resource-list t.resource-list)
      %_  $
          resource-list  t.resource-list
          graphs
        %+  ~(put by graphs)
          resource
        (add-node-list resource u.graph ~(tap by indexed-nodes))
      ==
      ::
      ++  add-node-list
        |=  $:  =resource:store
                =graph:store
                node-list=(list [index:store node:store])
            ==
        ^-  graph:store
        |-
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
        ?~  t.index  (put:orm graph atom node)
        ::  multiple indices left in list
        ::  TODO: replace normal map function with ordered-map version
        ::  of get. look at find-ducts in behn
        =/  parent=(unit node)  (~(get by graph) atom)
        ?~  parent
          ~|  "{<atom>} does not exist to add a node to!"
          graph
        ?+  -.children.u.parent
          ::  replace empty graph with graph containing one child
          %^  put:orm
              graph
            atom
          %_  u.parent
              children
            [%graph $(graph (gas:orm ~ ~), index t.index) now.bowl]
          ==
        ::
            %graph
          :: recurse into children
          %^  put:orm
              graph
            atom
          %_  u.parent
              p.children  $(graph p.children.u.parent, index t.index)
              q.children  now.bowl
          ==
        ==
      ::
      ++  orm  ((ordered-map atom:store node:store) lth)
      --
    ::
    ++  remove-nodes
      |=  uids=(set uid:store)
      ^-  (quip card _state)
      [~ state]
    ::
    ++  add-signatures
      |=  [=uid:store =signatures:store]
      ^-  (quip card _state)
      [~ state]
    ::
    ++  remove-signatures
      |=  [=uid:store =signatures:store]
      ^-  (quip card _state)
      [~ state]
    ::
    ++  add-tag
      |=  [=term =resources:store]
      ^-  (quip card _state)
      [~ state]
    ::
    ++  remove-tag
      |=  [=term =resources:store]
      ^-  (quip card _state)
      [~ state]
    ::
    ::
    ++  give
      |=  [paths=(list path) =update:store]
      ^-  (list card)
      [%give %fact paths [%graph-update !>(update)]]~
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
        [%all ~]   (give [%initial graphs tags tag-queries])
        [%keys ~]  (give [%keys ~(key by graphs)])
    ==
  [cards this]
  ::
  ++  give
    |=  =update:store
    ^-  (list card)
    [%give %fact ~ [%graph-update !>(update)]]~
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
