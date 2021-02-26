/-  spider
/+  strandio, store=graph-store, graph, graph-view
=,  strand=strand:spider
^-  thread:spider
|=  arg=vase
=/  m  (strand:spider ,vase)
^-  form:m
=+  !<([~ =update:store] arg)
?>  ?=(%add-nodes -.q.update)
=*  poke-our  poke-our:strandio
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
  =/  hash=(unit hash:store)
    :-  ~
    =-  `@ux`(sham -)
    :^  ?^  parent-hash
          parent-hash
        (index-to-parent-hash index)
        author.p
      time-sent.p
    contents.p
  %_  node
    hash.post  hash
  ::
      children
    ?:  ?=(%empty -.children.node)
      children.node
    :-  %graph
    %+  gas:orm:store  *graph:store
    %+  turn
      %+  turn
        (tap-deep:graph index p.children.node)
      |=  [=index:store =node:store]
      ^-  [index:store node:store]
      %_  loop
        parent-hash  hash
        index        index
        node         node
      ==
    |=  [=index:store =node:store]
    ^-  [atom node:store]
    [(rear index) node]
  ==
::
++  index-to-parent-hash
  |=  =index:store
  ^-  (unit hash:store)
  ?:  ?=(~ index)
    !!
  ?:  ?=([@ ~] index)
    ~
  =/  node
    %+  got-node:graph
      resource.q.update
    (snip `(list atom)`index)
  hash.post.node
::
++  nodes-to-pending-indices
  |=  nodes=(map index:store node:store)
  ^-  (map index:store hash:store)
  %-  ~(gas by *(map index:store hash:store))
  %+  turn  ~(tap by nodes)
  |=  [=index:store =node:store]
  ^-  [index:store hash:store]
  ?>  ?=(^ hash.post.node)
  [index u.hash.post.node]
--
