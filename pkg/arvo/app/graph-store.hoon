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
      :-  (give [/updates /keys ~] [%add-graph resource graph])
      %_  state
          graphs       (~(put by graphs) resource graph)
          action-logs  (~(put by action-logs) resource (gas:orm-log ~ ~))
      ==
    ::
    ++  remove-graph
      |=  =resource:store
      ^-  (quip card _state)
      ?>  (~(has by graphs) resource)
      :-  (give [/updates /keys ~] [%remove-graph resource])
      %_  state
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
      :-  (give [/updates]~ [%add-nodes resource nodes])
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
            graph      (add-node-at-index graph index node ~)
        ==
      ::
      ++  add-node-at-index
        |=  $:  =graph:store
                =index:store
                =node:store
                parent-hash=(unit hash:store)
            ==
        ^-  graph:store
        ?~  index  graph
        =*  atom   i.index
        ::  last index in list
        ::
        %^  put:orm
            graph
          atom
        ?~  t.index
          ::  verify hash if it exists, otherwise calculate
          ::
          =*  p  post.node
          =/  =validated-portion:store
            [parent-hash author.p index.p time-sent.p contents.p]
          =/  calculated-hash  (mug validated-portion)
          ?^  hash.p
            ::  hash is present, validate it
            ~|  "hash of post does not match calculated hash"
            ?>  =(calculated-hash u.hash.p)
            node
          ::  no hash present
          ::
          %=  node
              hash.post  `calculated-hash
              signatures.post
            ?.  =(our.bowl author.post.node)  ~
            %-  ~(gas in *signatures:store)
            :_  ~
            :+  `@ux`(sign:as:crub:crypto calculated-hash)
              our.bowl
            .^(=life %j /=life/(scot %p our.bowl))
          ==
        ::  multiple indices left in list
        ::
        ~|  "index does not exist to add a node to!"
        =/  parent=node:store  (need (get:orm graph atom))
        %_  parent
            children
          ^-  internal-graph:store
          :-  %graph
          %_  $
              index        t.index
              parent-hash  hash.post.parent
              graph
            ?:  ?=(%graph -.children.parent)
              :: recurse into children
              ::
              p.children.parent
            ::  replace empty graph with graph containing one child
            ::
            (gas:orm ~ ~)
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
      :-  (give [/updates]~ [%remove-nodes resource indices])
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
        ~|  "parent index does not exist to remove a node from!"
        =/  =node:store  (need (get:orm graph atom))
        ~|  "child index does not exist to remove a node from!"
        ?>  ?=(%graph -.children.node)
        :: recurse into children
        ::
        %^  put:orm
            graph
          atom
        node(p.children $(graph p.children.node, index t.index))
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
      :-  (give [/updates]~ [%add-signatures uid signatures])
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
        ~|  "node does not exist to add signatures to!" 
        =/  =node:store  (need (get:orm graph atom))
        ::  last index in list
        ::
        %^  put:orm
            graph
          atom
        ?~  t.index
          ~|  "cannot add signatures to a node missing a hash"
          ?>  ?=(^ hash.post.node)
          ~|  "signatures did not match public keys!"
          ?>  (are-signatures-valid:sigs signatures u.hash.post.node now.bowl)
          node(signatures.post (~(uni in signatures) signatures.post.node))
        ::  multiple indices left in list
        ::
        ~|  "child graph does not exist to add signatures to!"
        ?>  ?=(%graph -.children.node)
        :: recurse into children
        ::
        node(p.children $(graph p.children.node, index t.index))
      --
    ::
    ++  remove-signatures
      |=  [=uid:store =signatures:store]
      ^-  (quip card _state)
      |^
      =*  resource  resource.uid
      =/  =graph:store       (~(got by graphs) resource)
      =/  =action-log:store  (~(got by action-logs) resource)
      =.  action-log
        %^  put:orm-log  action-log
          now.bowl
        [%0 [%remove-signatures uid signatures]]
      ::
      :-  (give [/updates]~ [%remove-signatures uid signatures])
      %_  state
          action-logs  (~(put by action-logs) resource action-log)
          graphs
        %+  ~(put by graphs)  resource
        (remove-at-index graph index.uid signatures)
      ==
      ::
      ++  remove-at-index
        |=  [=graph:store =index:store =signatures:store]
        ^-  graph:store
        ?~  index  graph
        =*  atom   i.index
        ~|  "node does not exist to add signatures to!" 
        =/  =node:store  (need (get:orm graph atom))
        ::  last index in list
        ::
        %^  put:orm
            graph
          atom
        ?~  t.index
          node(signatures.post (~(dif in signatures) signatures.post.node))
        ::  multiple indices left in list
        ::
        ~|  "child graph does not exist to add signatures to!"
        ?>  ?=(%graph -.children.node)
        :: recurse into children
        ::
        node(p.children $(graph p.children.node, index t.index))
      --
    ::
    ++  add-tag
      |=  [=term =resource:store]
      ^-  (quip card _state)
      ?>  (~(has by graphs) resource)
      :-  (give [/updates]~ [%add-tag term resource])
      %_  state
          tag-queries  (~(put ju tag-queries) term resource)
      ==
    ::
    ++  remove-tag
      |=  [=term =resource:store]
      ^-  (quip card _state)
      ?>  (~(has by graphs) resource)
      :-  (give [/updates]~ [%remove-tag term resource])
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
        [%updates ~]   ~
        [%keys ~]      (give [%keys ~(key by graphs)])
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
