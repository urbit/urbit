/-  spider
/+  strandio, store=graph-store, gra=graph, graph-view, sig=signatures
=,  strand=strand:spider
=>
|%
++  scry-graph
  |=  rid=resource:store
  =/  m  (strand ,graph:store)
  ^-  form:m
  ;<  =update:store  bind:m
    %+  scry:strandio  update:store
    /gx/graph-store/graph/(scot %p entity.rid)/[name.rid]/noun
  ?>  ?=(%0 -.update)
  ?>  ?=(%add-graph -.q.update)
  (pure:m graph.q.update)
--
::
^-  thread:spider
|=  arg=vase
=/  m  (strand:spider ,vase)
^-  form:m
=+  !<([~ =update:store] arg)
?>  ?=(%add-nodes -.q.update)
=*  poke-our  poke-our:strandio
;<  =bowl:spider  bind:m  get-bowl:strandio
;<  =graph:store  bind:m  (scry-graph resource.q.update)
|^
=.  nodes.q.update
  %-  ~(gas by *(map index:store node:store))
  %+  turn
    (concat-by-parent (sort-nodes nodes.q.update))
  add-hash-to-node
=/  hashes  (nodes-to-pending-indices nodes.q.update)
;<  ~  bind:m
  %^  poke-our  %graph-push-hook
    %graph-update
  !>(update)
(pure:m !>(`action:graph-view`[%pending-indices hashes]))
::
++  sort-nodes
  |=  nodes=(map index:store node:store)
  ^-  (list [index:store node:store])
  %+  sort  ~(tap by nodes)
  |=  [p=[=index:store *] q=[=index:store *]]
  ^-  ?
  (lth (lent index.p) (lent index.q))
::
++  concat-by-parent
  |=  lis=(list [index:store node:store])
  ^-  (list [index:store node:store])
  %~  tap  by
  %+  roll  lis
  |=  $:  [=index:store =node:store]
          nds=(map index:store node:store)
      ==
  ?:  ?=(~ index)  !!
  ?:  ?=([@ ~] index)
    (~(put by nds) index node)
  =/  ind  (snip `(list atom)`index)
  =/  nod  (~(get by nds) ind)
  ?~  nod
    (~(put by nds) index node)
  =.  children.u.nod
    :-  %graph
    ?:  ?=(%empty -.children.u.nod)
      %+  gas:orm:store  *graph:store
      [(rear index) node]~
    %^  put:orm:store  p.children.u.nod
      (rear index)
    node
  (~(put by nds) ind u.nod)
::
++  add-hash-to-node
  =|  parent-hash=(unit hash:store)
  |=  [=index:store =node:store]
  ^-  [index:store node:store]
  =*  loop  $
  :-  index
  =*  p  post.node
  =/  =hash:store
    =-  `@ux`(sham -)
    :^  ?^  parent-hash
          parent-hash
        (index-to-parent-hash index)
        author.p
      time-sent.p
    contents.p
  %_  node
    hash.post  `hash
  ::
  ::  TODO: enable signing our own post as soon as we're ready
  ::    signatures.post
  ::  %-  ~(gas in *signatures:store)
  ::  [(sign:sig our.bowl now.bowl hash)]~
  ::
      children
    ?:  ?=(%empty -.children.node)
      children.node
    :-  %graph
    %+  gas:orm:store  *graph:store
    %+  turn  (tap:orm:store p.children.node)
    |=  [=atom =node:store]
    =/  [* nod=node:store]
      %_  loop
        parent-hash  `hash
        index        (snoc index atom)
        node         node
      ==
    [atom nod]
  ==
::
++  index-to-parent-hash
  |=  =index:store
  ^-  (unit hash:store)
  ?:  ?=(~ index)
    !!
  ?:  ?=([@ ~] index)
    ~
  =/  node  (got-deep:gra graph (snip `(list atom)`index))
  hash.post.node
::
++  nodes-to-pending-indices
  |=  nodes=(map index:store node:store)
  ^-  (map hash:store index:store)
  %-  ~(gas by *(map hash:store index:store))
  %+  turn  ~(tap by nodes)
  |=  [=index:store =node:store]
  ^-  [hash:store index:store]
  ?>  ?=(^ hash.post.node)
  [u.hash.post.node index]
--
