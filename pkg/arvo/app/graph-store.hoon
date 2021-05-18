::  graph-store [landscape]
::
::
/+  store=graph-store, sigs=signatures, res=resource, default-agent, dbug, verb
~%  %graph-store-top  ..part  ~
|%
+$  card  card:agent:gall
+$  versioned-state
  $%  [%0 network:zero:store]
      [%1 network:zero:store]
      [%2 network:zero:store]
      [%3 network:one:store]
      [%4 network:store]
      state-5
  ==
::
+$  state-5  [%5 network:store]
++  orm      orm:store
++  orm-log  orm-log:store
+$  debug-input  [%validate-graph =resource:store]
::
+$  cache
  $:  validators=(map mark dais:clay)
  ==
::
::  TODO: come back to this and potentially use ford runes or otherwise
::  send a %t to be notified of validator changes
+$  inflated-state
  $:  state-5
      cache
  ==
--
::
=|  inflated-state
=*  state  -
::
%-  agent:dbug
%+  verb  |
^-  agent:gall
~%  %graph-store-agent  ..card  ~
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
::
++  on-init  [~ this]
++  on-save  !>(-.state)
++  on-load
  |=  =old=vase
  ^-  (quip card _this)
  =+  !<(old=versioned-state old-vase)
  =|  cards=(list card)
  |-
  ?-    -.old
      %0
    =*  zro  zero-load:upgrade:store
    %_    $
      -.old  %1
    ::
        graphs.old
      %-  ~(run by graphs.old)
      |=  [=graph:zero:store q=(unit mark)]
      ^-  [graph:zero:store (unit mark)]
      :-  (convert-unix-timestamped-graph:zro graph)
      ?^  q  q
      `%graph-validator-link
    ::
        update-logs.old
      %-  ~(run by update-logs.old)
      |=(a=* *update-log:zero:store)
    ==
  ::
      %1
    =*  zro  zero-load:upgrade:store
    %_  $
      -.old       %2
      graphs.old  (~(run by graphs.old) change-revision-graph:zro)
    ::
        update-logs.old
      %-  ~(run by update-logs.old)
      |=(a=* *update-log:zero:store)
    ==
  ::
      %2
    =*  upg  upgrade:store
    %_  $
      -.old            %3
      update-logs.old  (~(run by update-logs.old) update-log-to-one:upg)
      graphs.old       (~(run by graphs.old) marked-graph-to-one:upg)
      archive.old      (~(run by archive.old) marked-graph-to-one:upg)
    ==
  ::
      %3
    =*  upg  upgrade:store
    %_  $
      -.old  %4
      graphs.old       (~(run by graphs.old) marked-graph-to-two:upg)
      archive.old      (~(run by archive.old) marked-graph-to-two:upg)
      tag-queries.old  *tag-queries:store
      validators.old   ~
    ::
        update-logs.old
      %-  ~(run by update-logs.old)
      |=(a=* *update-log:store)
    ==
  ::
      %4
    %_   $
      -.old  %5
    ::
        update-logs.old
      %-  ~(gas by *update-logs:store)
      %+  turn  ~(tap by graphs.old)
      |=  [=resource:store =graph:store mar=(unit mark)]
      :-  resource
      =/  log  (~(got by update-logs.old) resource)
      ?.  =(~ log)  log
      =/  =logged-update:store
        [now.bowl %add-graph resource graph mar %.y]
      (gas:orm-log ~ [now.bowl logged-update] ~)
    ==
  ::
    %5  [cards this(-.state old, +.state *cache)]
  ==
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
    |=  =action:store
    ^-  (list card)
    [%give %fact ~ [%graph-update-2 !>([now.bowl action])]]~
  --
::
++  on-poke
  ~/  %graph-store-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  |^
  ?>  (team:title our.bowl src.bowl)
  =^  cards  state
    ?+  mark             (on-poke:def mark vase)
        %graph-update-2  (graph-update !<(update:store vase))
        %noun            (debug !<(debug-input vase))
        %import          (poke-import q.vase)
    ==
  [cards this]
  ::
  ++  graph-update
    |=  =update:store
    ^-  (quip card _state)
    |^
    =?  p.update  =(p.update *time)  now.bowl
    ?-  -.q.update
        %add-graph          (add-graph p.update +.q.update)
        %remove-graph       (remove-graph +.q.update)
        %add-nodes          (add-nodes p.update +.q.update)
        %remove-posts       (remove-posts p.update +.q.update)
        %add-signatures     (add-signatures p.update +.q.update)
        %remove-signatures  (remove-signatures p.update +.q.update)
        %add-tag            (add-tag +.q.update)
        %remove-tag         (remove-tag +.q.update)
        %archive-graph      (archive-graph +.q.update)
        %unarchive-graph    (unarchive-graph +.q.update)
        %run-updates        (run-updates +.q.update)
        %keys               ~|('cannot send %keys as poke' !!)
        %tags               ~|('cannot send %tags as poke' !!)
        %tag-queries        ~|('cannot send %tag-queries as poke' !!)
    ==
    ::
    ++  add-graph
      |=  $:  =time
              =resource:store
              =graph:store
              mark=(unit mark:store)
              overwrite=?
          ==
      ^-  (quip card _state)
      ?>  ?|  overwrite
              ?&  !(~(has by archive) resource)
                  !(~(has by graphs) resource)
          ==  ==
      ~|  "validation of graph {<resource>} failed using mark {<mark>}"
      =^  is-valid  state
        (validate-graph graph mark)
      ?>  is-valid
      =/  =logged-update:store
        [time %add-graph resource graph mark overwrite]
      =/  =update-log:store
        (gas:orm-log ~ [time logged-update] ~)
      :_  %_  state
              graphs       (~(put by graphs) resource [graph mark])
              update-logs  (~(put by update-logs) resource update-log)
              archive      (~(del by archive) resource)
          ==
      %-  zing
      :~  (give [/keys ~] %keys (~(put in ~(key by graphs)) resource))
          (give [/updates ~] %add-graph resource *graph:store mark overwrite)
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
      |=  $:  =time
              =resource:store
              nodes=(map index:store node:store)
          ==
      ^-  (quip card _state)
      |^
      =/  [=graph:store mark=(unit mark:store)]
        (~(got by graphs) resource)
      ~|  "cannot add duplicate nodes to {<resource>}"
      ?<  (check-for-duplicates graph ~(key by nodes))
      ~|  "validation of nodes failed using mark {<mark>}"
      =^  is-valid  state
        (check-validity ~(tap by nodes) mark)
      ?>  is-valid
      =/  =update-log:store  (~(got by update-logs) resource)
      =.  update-log
        (put:orm-log update-log time [time [%add-nodes resource nodes]])
      ::
      :-  (give [/updates]~ [%add-nodes resource nodes])
      %_  state
          update-logs  (~(put by update-logs) resource update-log)
          graphs
        %+  ~(put by graphs)
          resource
        :_  mark
        (add-node-list resource graph mark (sort-nodes nodes))
      ==
      ::
      ++  check-validity
        |=  [lis=(list (pair index:store node:store)) mark=(unit ^mark)]
        ^-  [? _state]
        |-
        ?~  lis  [& state]
        =^  is-valid  state
          (validate-graph (gas:orm ~ [(rear p.i.lis) q.i.lis]~) mark)
        ?.  is-valid
          [| state]
        $(lis t.lis)
      ::
      ++  check-for-duplicates
        |=  [=graph:store nodes=(set index:store)]
        ^-  ?
        |^
        %+  lien  ~(tap in nodes)
        |=  =index:store
        (has-node graph index)
        ::
        ++  has-node
          |=  [=graph:store =index:store]
          ^-  ?
          =/  node=(unit node:store)  ~
          |-
          ?~  index
            ?=(^ node)
          ?~  t.index
            ?=(^ (get:orm graph i.index))
          =.  node  (get:orm graph i.index)
          ?~  node  %.n
          ?-  -.children.u.node
              %empty  %.n
              %graph  $(graph p.children.u.node, index t.index)
          ==
        --
      ::
      ++  sort-nodes
        |=  nodes=(map index:store node:store)
        ^-  (list [index:store node:store])
        %+  sort  ~(tap by nodes)
        |=  [p=[=index:store *] q=[=index:store *]]
        ^-  ?
        (lth (lent index.p) (lent index.q))
      ::
      ++  add-node-list
        |=  $:  =resource:store
                =graph:store
                mark=(unit mark:store)
                node-list=(list [index:store node:store])
            ==
        ^-  graph:store
        ?~  node-list  graph
        =*  index  -.i.node-list
        =*  node   +.i.node-list
        ~|  "cannot add deleted post"
        ?>  ?=(%& -.post.node)
        =*  p  p.post.node
        ~|  "graph indexes must match"
        ?>  =(index index.p)
        %_  $
            node-list  t.node-list
            graph      (add-node-at-index graph index node mark)
        ==
      ::
      ++  add-node-at-index
        =|  parent-hash=(unit hash:store)
        |=  $:  =graph:store
                =index:store
                =node:store
                mark=(unit mark:store)
            ==
        ^-  graph:store
        ?<  ?=(~ index)
        =*  atom   i.index
        %^  put:orm
            graph
          atom
        ::  add child
        ::
        ?~  t.index
          ~|  "cannot add deleted post"
          ?>  ?=(%& -.post.node)
          =*  p  p.post.node
          ?~  hash.p  node(signatures.p.post *signatures:store)
          =/  =validated-portion:store
            [parent-hash author.p time-sent.p contents.p]
          =/  =hash:store  `@ux`(sham validated-portion)
          ~|  "hash of post does not match calculated hash"
          ?>  =(hash u.hash.p)
          ~|  "signatures do not match the calculated hash"
          ?>  (are-signatures-valid:sigs our.bowl signatures.p hash now.bowl)
          node
        ::  recurse children
        ::
        =/  parent=node:store
          ~|  "index does not exist to add a node to!"
          (need (get:orm graph atom))
        %_  parent
            children
          ^-  internal-graph:store
          :-  %graph
          %_  $
            index        t.index
          ::
              parent-hash
            ?-  -.post.parent
              %|  `p.post.parent
              %&  hash.p.post.parent
            ==
          ::
              graph
            ?:  ?=(%graph -.children.parent)
              p.children.parent
            (gas:orm ~ ~)
          ==
        ==
      --
    ::
    ++  remove-posts
      |=  [=time =resource:store indices=(set index:store)]
      ^-  (quip card _state)
      |^
      =/  [=graph:store mark=(unit mark:store)]
        (~(got by graphs) resource)
      =/  =update-log:store  (~(got by update-logs) resource)
      =.  update-log
        (put:orm-log update-log time [time [%remove-posts resource indices]])
      :-  (give [/updates]~ [%remove-posts resource indices])
      %_  state
        update-logs  (~(put by update-logs) resource update-log)
      ::
          graphs
        %+  ~(put by graphs)
          resource
        :_  mark
        %^  remove-indices
            resource
          graph
        (sort ~(tap in indices) by-lent)
      ==
      ::
      ++  by-lent
        |*  [a=(list) b=(list)]
        ^-  ?
        (gth (lent a) (lent b))
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
        =|  parent-hash=(unit hash:store)
        |=  [=graph:store =index:store]
        ^-  graph:store
        ?~  index  graph
        =*  atom   i.index
        %^  put:orm
            graph
          atom
        ::  last index in list
        ::
        ?~  t.index
          =/  =node:store
            ~|  "cannot remove index that does not exist {<index>}"
            (need (get:orm graph atom))
          %_    node
              post
            ~|  "cannot remove post that has already been removed"
            ?>  ?=(%& -.post.node)
            =*  p  p.post.node
            ^-  maybe-post:store
            :-  %|
            ?~  hash.p
              =/  =validated-portion:store
                [parent-hash author.p time-sent.p contents.p]
              `@ux`(sham validated-portion)
            u.hash.p
          ==
        ::  recurse children
        ::
        =/  parent=node:store
          ~|  "parent index does not exist to remove a node from!"
          (need (get:orm graph atom))
        ~|  "child index does not exist to remove a node from!"
        ?>  ?=(%graph -.children.parent)
        %_    parent
            p.children
          %_  $
            index  t.index
            graph  p.children.parent
          ::
              parent-hash
            ?-  -.post.parent
              %|  `p.post.parent
              %&  hash.p.post.parent
            ==
          ==
        ==
      --
    ::
    ++  add-signatures
      |=  [=time =uid:store =signatures:store]
      ^-  (quip card _state)
      |^
      =*  resource  resource.uid
      =/  [=graph:store mark=(unit mark:store)]
        (~(got by graphs) resource)
      =/  =update-log:store  (~(got by update-logs) resource)
      =.  update-log
        (put:orm-log update-log time [time [%add-signatures uid signatures]])
      ::
      :-  (give [/updates]~ [%add-signatures uid signatures])
      %_  state
          update-logs  (~(put by update-logs) resource update-log)
          graphs
        %+  ~(put by graphs)  resource
        [(add-at-index graph index.uid signatures) mark]
      ==
      ::
      ++  add-at-index
        |=  [=graph:store =index:store =signatures:store]
        ^-  graph:store
        ?~  index  graph
        =*  atom   i.index
        =/  =node:store
          ~|  "node does not exist to add signatures to!"
          (need (get:orm graph atom))
        ::  last index in list
        ::
        %^  put:orm
            graph
          atom
        ?~  t.index
          ~|  "cannot add signatures to a deleted post"
          ?>  ?=(%& -.post.node)
          ~|  "cannot add signatures to a node missing a hash"
          ?>  ?=(^ hash.p.post.node)
          ~|  "signatures did not match public keys!"
          ?>  (are-signatures-valid:sigs our.bowl signatures u.hash.p.post.node now.bowl)
          node(signatures.p.post (~(uni in signatures) signatures.p.post.node))
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
      =/  [=graph:store mark=(unit mark:store)]
        (~(got by graphs) resource)
      =/  =update-log:store  (~(got by update-logs) resource)
      =.  update-log
        %^  put:orm-log  update-log
          time
        [time [%remove-signatures uid signatures]]
      ::
      :-  (give [/updates]~ [%remove-signatures uid signatures])
      %_  state
          update-logs  (~(put by update-logs) resource update-log)
          graphs
        %+  ~(put by graphs)  resource
        [(remove-at-index graph index.uid signatures) mark]
      ==
      ::
      ++  remove-at-index
        |=  [=graph:store =index:store =signatures:store]
        ^-  graph:store
        ?~  index  graph
        =*  atom   i.index
        =/  =node:store
          ~|  "node does not exist to add signatures to!"
          (need (get:orm graph atom))
        ::  last index in list
        ::
        %^  put:orm
            graph
          atom
        ?~  t.index
          ~|  "cannot add signatures to a deleted post"
          ?>  ?=(%& -.post.node)
          node(signatures.p.post (~(dif in signatures) signatures.p.post.node))
        ~|  "child graph does not exist to add signatures to!"
        ?>  ?=(%graph -.children.node)
        node(p.children $(graph p.children.node, index t.index))
      --
    ::
    ++  add-tag
      |=  [=term =uid:store]
      ^-  (quip card _state)
      ?>  (~(has by graphs) resource.uid)
      :-  (give [/updates /tags ~] [%add-tag term uid])
      %_  state
          tag-queries  (~(put ju tag-queries) term uid)
      ==
    ::
    ++  remove-tag
      |=  [=term =uid:store]
      ^-  (quip card _state)
      :-  (give [/updates /tags ~] [%remove-tag term uid])
      %_  state
          tag-queries  (~(del ju tag-queries) term uid)
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
      =/  updates=(list [=time upd=logged-update:store])
        ::  updates are time-ordered with most recent first
        ::  process with earliest first
        (bap:orm-log update-log)
      =|  cards=(list card)
      |-  ^-  (quip card _state)
      ?~  updates
        [cards state]
      =*  update  upd.i.updates
      =^  crds  state
        %-  graph-update
        ^-  update:store
        ?-  -.q.update
            %add-graph          update(resource.q resource)
            %add-nodes          update(resource.q resource)
            %remove-posts       update(resource.q resource)
            %add-signatures     update(resource.uid.q resource)
            %remove-signatures  update(resource.uid.q resource)
        ==
      $(cards (weld cards crds), updates t.updates)
    ::
    ++  give
      |=  [paths=(list path) update=action:store]
      ^-  (list card)
      [%give %fact paths [%graph-update-2 !>([now.bowl update])]]~
    --
  ::
  ++  debug
    |=  =debug-input
    ^-  (quip card _state)
    =/  [=graph:store mark=(unit mark:store)]
      (~(got by graphs) resource.debug-input)
    =^  is-valid  state
      (validate-graph graph mark)
    ?>  is-valid
    [~ state]
  ::
  ++  validate-graph
    |=  [=graph:store mark=(unit mark:store)]
    ^-  [? _state]
    ?~  mark   [%.y state]
    =/  has-dais  (~(has by validators) u.mark)
    =/  =dais:clay
      ?:  has-dais
        (~(got by validators) u.mark)
      .^  =dais:clay
          %cb
          /(scot %p our.bowl)/[q.byk.bowl]/(scot %da now.bowl)/[u.mark]
      ==
    :_  state(validators (~(put by validators) u.mark dais))
    |-  ^-  ?
    ?~  graph  %.y
    %+  roll  (tap:orm graph)
    |=  [[=atom =node:store] out=?]
    ^-  ?
    ?&  ?|  ?=(%| -.post.node)
            ?=(^ (vale:dais [atom p.post.node]))
        ==
      ::
        ?-  -.children.node
          %empty  %.y
          %graph  ^$(graph p.children.node)
    ==  ==
  ::
  ++  poke-import
    |=  arc=*
    ^-  (quip card _state)
    =^  cards  -.state
      (import:store arc our.bowl)
    [cards state]
  --
::
++  on-peek
  ~/  %graph-store-peek
  |=  =path
  ^-  (unit (unit cage))
  |^
  ?>  (team:title our.bowl src.bowl)
  ?+  path  (on-peek:def path)
      [%x %graph-mark @ @ ~]
    =/  =ship   (slav %p i.t.t.path)
    =/  =term   i.t.t.t.path
    =/  result=(unit marked-graph:store)
      (~(get by graphs) [ship term])
    ?~  result  [~ ~]
    ``noun+!>(q.u.result)
  ::
      [%x %keys ~]
    :-  ~  :-  ~  :-  %graph-update-2
    !>(`update:store`[now.bowl [%keys ~(key by graphs)]])
  ::
      [%x %tags ~]
    :-  ~  :-  ~  :-  %graph-update-2
    !>(`update:store`[now.bowl [%tags ~(key by tag-queries)]])
  ::
      [%x %tag-queries ~]
    :-  ~  :-  ~  :-  %graph-update-2
    !>(`update:store`[now.bowl [%tag-queries tag-queries]])
  ::
      [%x %graph @ @ ~]
    =/  =ship   (slav %p i.t.t.path)
    =/  =term   i.t.t.t.path
    =/  result=(unit marked-graph:store)
      (~(get by graphs) [ship term])
    ?~  result  [~ ~]
    :-  ~  :-  ~  :-  %graph-update-2
    !>  ^-  update:store
    :-  now.bowl
    [%add-graph [ship term] `graph:store`p.u.result q.u.result %.y]
  ::
      ::  note: near-duplicate of /x/graph
      ::
      [%x %archive @ @ ~]
    =/  =ship   (slav %p i.t.t.path)
    =/  =term   i.t.t.t.path
    =/  result=(unit marked-graph:store)
      (~(get by archive) [ship term])
    ?~  result
      ~&  no-archived-graph+[ship term]
      [~ ~]
    :-  ~  :-  ~  :-  %graph-update-2
    !>  ^-  update:store
    :-  now.bowl
    [%add-graph [ship term] `graph:store`p.u.result q.u.result %.y]
  ::
      [%x %export ~]
    ``noun+!>(state)
  ::
      [%x %graph-subset @ @ @ @ ~]
    =/  =ship  (slav %p i.t.t.path)
    =/  =term  i.t.t.t.path
    =/  start=(unit atom)  (rush i.t.t.t.t.path dem:ag)
    =/  end=(unit atom)    (rush i.t.t.t.t.t.path dem:ag)
    =/  graph=(unit marked-graph:store)
      (~(get by graphs) [ship term])
    ?~  graph  [~ ~]
    :-  ~  :-  ~  :-  %graph-update-2
    !>  ^-  update:store
    :-  now.bowl
    :+  %add-nodes
      [ship term]
    %-  ~(gas by *(map index:store node:store))
    %+  turn  (tap:orm `graph:store`(lot:orm p.u.graph start end))
    |=  [=atom =node:store]
    ^-  [index:store node:store]
    [~[atom] node]
  ::
      [%x %node-exists @ @ @ *]
    =/  =ship  (slav %p i.t.t.path)
    =/  =term  i.t.t.t.path
    =/  =index:store
      (turn t.t.t.t.path (cury slav %ud))
    =/  node=(unit node:store)
      (get-node ship term index)
    ``noun+!>(?=(^ node))
  ::
      [%x %node @ @ @ *]
    =/  =ship  (slav %p i.t.t.path)
    =/  =term  i.t.t.t.path
    =/  =index:store
      (turn t.t.t.t.path (cury slav %ud))
    =/  node=(unit node:store)  (get-node ship term index)
    ?~  node  [~ ~]
    :-  ~  :-  ~  :-  %graph-update-2
    !>  ^-  update:store
    :-  now.bowl
    :+  %add-nodes
      [ship term]
    (~(gas by *(map index:store node:store)) [index u.node] ~)
  ::
      [%x %node-siblings ?(%older %younger) @ @ @ *]
    =/  older  ?=(%older i.t.t.path)
    =/  =ship  (slav %p i.t.t.t.path)
    =/  =term  i.t.t.t.t.path
    =/  count  (slav %ud i.t.t.t.t.t.path)
    =/  =index:store
      (turn t.t.t.t.t.t.path (cury slav %ud))
    =/  parent=index:store
      (scag (dec (lent index)) index)
    =/  graph
      (get-node-children ship term parent)
    ?~  graph  [~ ~]
    :-  ~  :-  ~  :-  %graph-update-2
    !>  ^-  update:store
    :-  now.bowl
    :+  %add-nodes
      [ship term]
    %-  ~(gas by *(map index:store node:store))
    :: TODO time complexity not desirable
    ::   replace with custom ordered map functions
    %+  turn  
      =-  ?.(older (slag (safe-sub (lent -) count) -) (scag count -))
      %-  tap:orm
      %+  lot:orm  u.graph
      =/  idx
        (snag (dec (lent index)) index)
      ?:(older [`idx ~] [~ `idx])
    |=  [=atom =node:store]
    ^-  [index:store node:store]
    [(snoc parent atom) node]
  ::
      [%x ?(%newest %oldest) @ @ @ *]
    =/  newest  ?=(%newest i.t.path)
    =/  =ship  (slav %p i.t.t.path)
    =/  =term  i.t.t.t.path
    =/  count=@ud
      (slav %ud i.t.t.t.t.path)
    =/  =index:store
      (turn t.t.t.t.t.path (cury slav %ud))
    =/  children
      (get-node-children ship term index)
    ?~  children  [~ ~]
    :-  ~  :-  ~  :-  %graph-update-2
    !>  ^-  update:store
    :-  now.bowl
    :+  %add-nodes
      [ship term]
    %-  ~(gas by *(map index:store node:store))
    %+  turn
      %+  scag  count
      %-  ?:(newest same flop)
      (tap:orm u.children)
    |=  [=atom =node:store]
    ^-  [index:store node:store]
    [(snoc index atom) node]
  ::
      [%x %node-children-subset @ @ @ @ @ *]
    =/  =ship  (slav %p i.t.t.path)
    =/  =term  i.t.t.t.path
    =/  start=(unit atom)  (rush i.t.t.t.t.path dem:ag)
    =/  end=(unit atom)    (rush i.t.t.t.t.t.path dem:ag)
    =/  =index:store
      (turn t.t.t.t.t.t.path |=(=cord (slav %ud cord)))
    =/  node=(unit node:store)  (get-node ship term index)
    ?~  node  [~ ~]
    ?-  -.children.u.node
        %empty  [~ ~]
        %graph
      :-  ~  :-  ~  :-  %graph-update-2
      !>  ^-  update:store
      :-  now.bowl
      :+  %add-nodes
        [ship term]
      %-  ~(gas by *(map index:store node:store))
      %+  turn  (tap:orm `graph:store`(lot:orm p.children.u.node end start))
      |=  [=atom =node:store]
      ^-  [index:store node:store]
      [(snoc index atom) node]
    ==
  ::
      [%x %deep-nodes-older-than @ @ @ @ ~]
    =/  =ship  (slav %p i.t.t.path)
    =/  =term  i.t.t.t.path
    =/  count=(unit atom)  (rush i.t.t.t.t.path dem:ag)
    =/  start=(unit atom)  (rush i.t.t.t.t.t.path dem:ag)
    ?:  ?=(~ count)
      [~ ~]
    =/  result=(unit marked-graph:store)
      (~(get by graphs) [ship term])
    ?~  result
      [~ ~]
    :-  ~  :-  ~  :-  %graph-update-2
    !>  ^-  update:store
    :-  now.bowl
    :+  %add-nodes
      [ship term]
    =*  a  u.count
    =/  b=(list (pair atom node:store))
      (tab:orm p.u.result start u.count)
    =|  c=index:store
    =|  d=(map index:store node:store)
    =|  e=@ud
    =-  d
    |-  ^-  [e=@ud d=(map index:store node:store)]
    ?:  ?|(?=(~ b) =(e a))
      [e d]
    =*  atom  p.i.b
    =*  node  q.i.b
    =.  c  (snoc c atom)
    ?-  -.children.node
      %empty
      $(b t.b, e +(e), d (~(put by d) c node), c (snip c))
    ::
        %graph
      =/  f  $(b (tab:orm p.children.node ~ (sub a e)))
      ?:  =(e.f a)  f
      %_  $
        b  t.b
        e  +(e.f)
        d  (~(put by d.f) c node(children [%empty ~]))
      ==
    ==
  ::
      [%x %firstborn @ @ @ ~]
    =/  =ship  (slav %p i.t.t.path)
    =/  =term  i.t.t.t.path
    =/  =atom  (slav %ud i.t.t.t.t.path)
    =/  result=(unit marked-graph:store)
      (~(get by graphs) [ship term])
    ?~  result
      [~ ~]
    ?.  (has:orm p.u.result atom)
      [~ ~]
    =/  =node:store  (got:orm p.u.result atom)
    =/  =index:store  atom^~
    =/  map
      %-  ~(gas by *(map index:store node:store))
      [index node(children empty+~)]~
    |-
    ?:  ?=(%empty -.children.node)
      :-  ~  :-  ~  :-  %graph-update-2
      !>  ^-  update:store
      [now.bowl [%add-nodes [ship term] map]]
    =/  item=[k=^atom v=node:store]
      (need (ram:orm p.children.node))
    =.  index  (snoc index k.item)
    $(map (~(put by map) index v.item(children empty+~)), node v.item)
  ::
      [%x %update-log-subset @ @ @ @ ~]
    =/  =ship   (slav %p i.t.t.path)
    =/  =term   i.t.t.t.path
    =/  start=(unit time)  (slaw %da i.t.t.t.t.path)
    =/  end=(unit time)    (slaw %da i.t.t.t.t.t.path)
    =/  update-log=(unit update-log:store)  (~(get by update-logs) [ship term])
    ?~  update-log  [~ ~]
    ::  orm-log is ordered backwards, so swap start and end
    ``noun+!>((lot:orm-log u.update-log end start))
  ::
      [%x %update-log @ @ ~]
    =/  =ship   (slav %p i.t.t.path)
    =/  =term   i.t.t.t.path
    =/  update-log=(unit update-log:store)  (~(get by update-logs) [ship term])
    ?~  update-log  [~ ~]
    ``noun+!>(u.update-log)
  ::
      [%x %peek-update-log @ @ ~]
    =/  =ship   (slav %p i.t.t.path)
    =/  =term   i.t.t.t.path
    =/  m-update-log=(unit update-log:store)  (~(get by update-logs) [ship term])
    :-  ~  :-  ~  :-  %noun
    !>  ^-  (unit time)
    %+  biff  m-update-log
    |=  =update-log:store
    =/  result=(unit [=time =update:store])
      (pry:orm-log:store update-log)
    (bind result |=([=time update:store] time))
  ==
  ::
  ++  safe-sub
    |=  [a=@ b=@]
    ^-  @
    ?:  (gte b a)
      0
    (sub a b)
  ::
  ++  get-node-children
    |=  [=ship =term =index:store]
    ^-  (unit graph:store)
    ?:  ?=(~ index)
      =/  graph
        (~(get by graphs) [ship term])
      ?~  graph  ~
      `p.u.graph
    =/  node
      (get-node ship term index)
    ?~  node  ~
    ?:  ?=(%empty -.children.u.node)
      ~
    `p.children.u.node
  ::
  ++  get-node
    |=  [=ship =term =index:store]
    ^-  (unit node:store)
    =/  parent-graph=(unit marked-graph:store)
      (~(get by graphs) [ship term])
    ?~  parent-graph  ~
    =/  node=(unit node:store)  ~
    =/  =graph:store  p.u.parent-graph
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
++  on-arvo
  |=  [=wire =sign-arvo]
  ^-  (quip card _this)
  ?+  wire  (on-arvo:def wire sign-arvo)
  ::
  ::  old wire, do nothing 
      [%graph *]        [~ this]
      [%validator @ ~]  [~ this]
  ::
      [%try-rejoin @ *]
    =/  rid=resource:store  (de-path:res t.t.wire)
    =/  nack-count    (slav %ud i.t.wire)
    ?>  ?=([%behn %wake *] sign-arvo)
    ~?  ?=(^ error.sign-arvo)
      "behn errored in backoff timers, continuing anyway"
    =/  new=^wire  [%try-rejoin (scot %ud +(nack-count)) t.t.wire]
    :_  this
    [%pass new %agent [entity.rid %graph-push-hook] %watch resource+t.t.wire]~
  ==
::
++  on-agent
  |=  [=wire =sign:agent:gall]
  ^-  (quip card _this)
  ?.  ?=([%try-rejoin @ *] wire)
    (on-agent:def wire sign)
  ?.  ?=(%watch-ack -.sign)
    [~ this]
  =/  rid=resource:store  (de-path:res t.t.wire)
  ?~  p.sign
    =/  =cage  [%pull-hook-action !>([%add entity.rid rid])]
    :_  this
    :~  [%pass / %agent [our.bowl %graph-pull-hook] %poke cage]
        [%pass wire %agent [entity.rid %graph-push-hook] %leave ~]
    ==
  =/  nack-count=@ud  (slav %ud i.t.wire)
  =/  wakeup=@da
    (add now.bowl (mul ~s1 (bex (min 19 nack-count))))
  :_  this
  [%pass wire %arvo %b %wait wakeup]~
::
++  on-leave  on-leave:def
++  on-fail   on-fail:def
--
