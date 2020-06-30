/+  store=graph-store, sigs=signatures, default-agent, dbug
~%  %graph-store-top  ..is  ~
|%
+$  card  card:agent:gall
+$  versioned-state
  $%  state-0
  ==
::
+$  state-0  [%0 network:store]
++  orm      orm:store
++  orm-log  orm-log:store
--
::
=|  state-0
=*  state  -
::
%-  agent:dbug
^-  agent:gall
~%  %graph-store-agent  ..card  ~
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
::
++  on-init  [~ this]
++  on-save  !>(state)
++  on-load
  |=  old=vase
  ^-  (quip card _this)
  [~ this(state !<(state-0 old))]
::
++  on-watch
  ~/  %graph-store-watch
  |=  =path
  ^-  (quip card _this)
  |^
  ?>  (team:title our.bowl src.bowl)
  =/  cards=(list card)
    ?+  path       (on-watch:def path)
        [%updates ~]   ~
        [%keys ~]      (give [%keys ~(key by graphs)])
        [%tags ~]      (give [%tags ~(key by tag-queries)])
    ==
  [cards this]
  ::
  ++  give
    |=  =update-0:store
    ^-  (list card)
    [%give %fact ~ [%graph-update !>([%0 now.bowl update-0])]]~
  --
::
++  on-poke
  ~/  %graph-store-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  |^
  ?>  (team:title our.bowl src.bowl)
  =^  cards  state
    ?+  mark           (on-poke:def mark vase)
        %graph-update  (graph-update !<(update:store vase))
    ==
  [cards this]
  ::
  ++  graph-update
    |=  =update:store
    ^-  (quip card _state)
    |^
    ?>  ?=(%0 -.update)
    ?-  -.q.update
        %add-graph          (add-graph +.q.update)
        %remove-graph       (remove-graph +.q.update)
        %add-nodes          (add-nodes p.update +.q.update)
        %remove-nodes       (remove-nodes p.update +.q.update)
        %add-signatures     (add-signatures p.update +.q.update)
        %remove-signatures  (remove-signatures p.update +.q.update)
        %add-tag            (add-tag +.q.update)
        %remove-tag         (remove-tag +.q.update)
        %archive-graph      (archive-graph +.q.update)
        %unarchive-graph    (unarchive-graph +.q.update)
        %run-updates        (run-updates +.q.update)
        ::
        ::  NOTE: cannot send these updates as pokes
        ::
        %keys               !!
        %tags               !!
        %tag-queries        !!
    ==
    ::
    ++  add-graph
      |=  [=resource:store =graph:store]
      ^-  (quip card _state)
      ?<  (~(has by archive) resource)
      ?<  (~(has by graphs) resource)
      :-  (give [/updates /keys ~] [%add-graph resource graph])
      %_  state
          graphs       (~(put by graphs) resource graph)
          update-logs  (~(put by update-logs) resource (gas:orm-log ~ ~))
      ==
    ::
    ++  remove-graph
      |=  =resource:store
      ^-  (quip card _state)
      ?<  (~(has by archive) resource)
      ?>  (~(has by graphs) resource)
      :-  (give [/updates /keys ~] [%remove-graph resource])
      %_  state
          graphs       (~(del by graphs) resource)
          update-logs  (~(del by update-logs) resource)
      ==
    ::
    ++  add-nodes
      |=  [=time =resource:store nodes=(map index:store node:store)]
      ^-  (quip card _state)
      |^
      =/  =graph:store       (~(got by graphs) resource)
      =/  =update-log:store  (~(got by update-logs) resource)
      =.  update-log
        (put:orm-log update-log time [%0 time [%add-nodes resource nodes]])
      ::
      :-  (give [/updates]~ [%add-nodes resource nodes])
      %_  state
          update-logs  (~(put by update-logs) resource update-log)
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
        %^  put:orm
            graph
          atom
        ::  add child
        ::
        ?~  t.index
          =*  p  post.node
          =/  =validated-portion:store
            [parent-hash author.p index.p time-sent.p contents.p]
          =/  =hash:store  (mug validated-portion)
          ?~  hash.p  node
          ~|  "hash of post does not match calculated hash"
          ?>  =(hash u.hash.p)
          node
        ::  recurse children
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
              p.children.parent
            (gas:orm ~ ~)
          ==
        ==
      --
    ::
    ++  remove-nodes
      |=  [=time =resource:store indices=(set index:store)]
      ^-  (quip card _state)
      |^
      =/  =graph:store        (~(got by graphs) resource)
      =/  =update-log:store   (~(got by update-logs) resource)
      =.  update-log
        (put:orm-log update-log time [%0 time [%remove-nodes resource indices]])
      ::
      :-  (give [/updates]~ [%remove-nodes resource indices])
      %_  state
          update-logs  (~(put by update-logs) resource update-log)
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
        ~|  "parent index does not exist to remove a node from!"
        =/  =node:store  (need (get:orm graph atom))
        ~|  "child index does not exist to remove a node from!"
        ?>  ?=(%graph -.children.node)
        %^  put:orm
            graph
          atom
        node(p.children $(graph p.children.node, index t.index))
      --
    ::
    ++  add-signatures
      |=  [=time =uid:store =signatures:store]
      ^-  (quip card _state)
      |^
      =*  resource  resource.uid
      =/  =graph:store       (~(got by graphs) resource)
      =/  =update-log:store  (~(got by update-logs) resource)
      =.  update-log
        (put:orm-log update-log time [%0 time [%add-signatures uid signatures]])
      ::
      :-  (give [/updates]~ [%add-signatures uid signatures])
      %_  state
          update-logs  (~(put by update-logs) resource update-log)
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
        ~|  "child graph does not exist to add signatures to!"
        ?>  ?=(%graph -.children.node)
        node(p.children $(graph p.children.node, index t.index))
      --
    ::
    ++  remove-signatures
      |=  [=time =uid:store =signatures:store]
      ^-  (quip card _state)
      |^
      =*  resource  resource.uid
      =/  =graph:store       (~(got by graphs) resource)
      =/  =update-log:store  (~(got by update-logs) resource)
      =.  update-log
        %^  put:orm-log  update-log
          time
        [%0 time [%remove-signatures uid signatures]]
      ::
      :-  (give [/updates]~ [%remove-signatures uid signatures])
      %_  state
          update-logs  (~(put by update-logs) resource update-log)
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
        ~|  "child graph does not exist to add signatures to!"
        ?>  ?=(%graph -.children.node)
        node(p.children $(graph p.children.node, index t.index))
      --
    ::
    ++  add-tag
      |=  [=term =resource:store]
      ^-  (quip card _state)
      ?>  (~(has by graphs) resource)
      :-  (give [/updates /tags ~] [%add-tag term resource])
      %_  state
          tag-queries  (~(put ju tag-queries) term resource)
      ==
    ::
    ++  remove-tag
      |=  [=term =resource:store]
      ^-  (quip card _state)
      ?>  (~(has by graphs) resource)
      :-  (give [/updates /tags ~] [%remove-tag term resource])
      %_  state
          tag-queries  (~(del ju tag-queries) term resource)
      ==
    ::
    ++  archive-graph
      |=  =resource:store
      ^-  (quip card _state)
      ?<  (~(has by archive) resource)
      ?>  (~(has by graphs) resource)
      :-  (give [/updates /keys /tags ~] [%archive-graph resource])
      %_  state
          archive      (~(put by archive) resource (~(got by graphs) resource))
          graphs       (~(del by graphs) resource)
          update-logs  (~(del by update-logs) resource)
          tag-queries
        %-  ~(run by tag-queries)
        |=  =resources:store
        (~(del in resources) resource)
      ==
    ::
    ++  unarchive-graph
      |=  =resource:store
      ^-  (quip card _state)
      ?>  (~(has by archive) resource)
      ?<  (~(has by graphs) resource)
      :-  (give [/updates /keys ~] [%unarchive-graph resource])
      %_  state
          archive      (~(del by archive) resource)
          graphs       (~(put by graphs) resource (~(got by archive) resource))
          update-logs  (~(put by update-logs) resource (gas:orm-log ~ ~))
      ==
    ::
    ++  run-updates
      |=  [=resource:store =update-log:store]
      ^-  (quip card _state)
      ?<  (~(has by archive) resource)
      ?>  (~(has by graphs) resource)
      :_  state
      %+  turn  (tap:orm-log update-log)
      |=  [=time =update:store]
      ^-  card
      :*  %pass
          /run-updates/(scot %da time)
          %agent
          [our.bowl %graph-store]
          %poke
          [%graph-update !>(update)]
      ==
    ::
    ++  give
      |=  [paths=(list path) update=update-0:store]
      ^-  (list card)
      [%give %fact paths [%graph-update !>([%0 now.bowl update])]]~
    --
  --
::
++  on-peek
  ~/  %graph-store-peek
  |=  =path
  ^-  (unit (unit cage))
  |^
  ?>  (team:title our.bowl src.bowl)
  ?+  path  (on-peek:def path)
      [%x %keys ~]         ``noun+!>(~(key by graphs))
      [%x %tags ~]         ``noun+!>(~(key by tag-queries))
      [%x %tag-queries ~]  ``noun+!>(tag-queries)
      [%x %graph @ @ ~]
    =/  =ship   (slav %p i.t.t.path)
    =/  =term   i.t.t.t.path
    =/  graph=(unit graph:store)  (~(get by graphs) [ship term])
    ?~  graph  ~
    ``noun+!>(u.graph)
  ::
      [%x %graph-subset @ @ @ @ ~]
    =/  =ship   (slav %p i.t.t.path)
    =/  =term   i.t.t.t.path
    =/  start=(unit atom:store)  (rush i.t.t.t.t.path dem:ag)
    =/  end=(unit atom:store)    (rush i.t.t.t.t.t.path dem:ag)
    =/  graph=(unit graph:store)  (~(get by graphs) [ship term])
    ?~  graph  ~
    ``noun+!>(`graph:store`(subset:orm u.graph start end))
  ::
      [%x %node @ @ @ *]
    =/  =ship         (slav %p i.t.t.path)
    =/  =term         i.t.t.t.path
    =/  =index:store  (turn t.t.t.t.path |=(=cord (slav %ud cord)))
    =/  node=(unit node:store)  (get-node ship term index)
    ?~  node  ~
    ``noun+!>(u.node)
  ::
      [%x %post @ @ @ *]
    =/  =ship         (slav %p i.t.t.path)
    =/  =term         i.t.t.t.path
    =/  =index:store  (turn t.t.t.t.path |=(=cord (slav %ud cord)))
    =/  node=(unit node:store)  (get-node ship term index)
    ?~  node  ~
    ``noun+!>(post.u.node)
  ::
      [%x %node-children @ @ @ *]
    =/  =ship         (slav %p i.t.t.path)
    =/  =term         i.t.t.t.path
    =/  =index:store  (turn t.t.t.t.path |=(=cord (slav %ud cord)))
    =/  node=(unit node:store)  (get-node ship term index)
    ?~  node  ~
    ?-  -.children.u.node
        %empty  ~
        %graph  ``noun+!>(p.children.u.node)
    ==
  ::
      [%x %node-children-subset @ @ @ @ @ *]
    =/  =ship         (slav %p i.t.t.path)
    =/  =term         i.t.t.t.path
    =/  start=(unit atom:store)  (rush i.t.t.t.t.path dem:ag)
    =/  end=(unit atom:store)    (rush i.t.t.t.t.t.path dem:ag)
    =/  =index:store  (turn t.t.t.t.t.t.path |=(=cord (slav %ud cord)))
    =/  node=(unit node:store)  (get-node ship term index)
    ?~  node  ~
    ?-  -.children.u.node
        %empty  ~
        %graph  ``noun+!>(`graph:store`(subset:orm p.children.u.node start end))
    ==
  ::
      [%x %update-log @ @ ~]
    =/  =ship   (slav %p i.t.t.path)
    =/  =term   i.t.t.t.path
    =/  update-log=(unit update-log:store)  (~(get by update-logs) [ship term])
    ?~  update-log  ~
    ``noun+!>(u.update-log)
  ::
      [%x %peek-update-log @ @ ~]
    =/  =ship   (slav %p i.t.t.path)
    =/  =term   i.t.t.t.path
    =/  update-log=(unit update-log:store)  (~(get by update-logs) [ship term])
    ?~  update-log  ~
    =/  result=(unit [time update:store])
      (peek:orm-log:store u.update-log) 
    ?~  result  ``noun+!>(~)
    ``noun+!>([~ -.u.result])
  ==
  ::
  ++  get-node
    |=  [=ship =term =index:store]
    ^-  (unit node:store)
    =/  parent-graph=(unit graph:store)  (~(get by graphs) [ship term])
    ?~  parent-graph  ~
    =/  node=(unit node:store)  ~
    =/  =graph:store  u.parent-graph
    |-
    ?~  index
      node
    ?~  t.index
      (get:orm graph i.index)
    =.  node  (get:orm graph i.index)
    ?~  node  ~
    ?-  -.children.u.node
        %empty  ~
        %graph  $(graph p.children.u.node, index t.index)
    ==
  --
::
++  on-arvo  on-arvo:def
++  on-agent  on-agent:def
++  on-leave  on-leave:def
++  on-fail   on-fail:def
--
