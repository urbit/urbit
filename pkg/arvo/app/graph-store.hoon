::  graph-store [landscape]
::
::
/+  store=graph-store, sigs=signatures, res=resource, default-agent, dbug, verb,
    *migrate
~%  %graph-store-top  ..part  ~
|%
+$  card  card:agent:gall
+$  versioned-state
  $%  state-0
      state-1
      state-2
  ==
::
+$  state-0  [%0 network:store]
+$  state-1  [%1 network:store]
+$  state-2  [%2 network:store]
::
++  orm      orm:store
++  orm-log  orm-log:store
+$  debug-input  [%validate-graph =resource:store]
--
::
=|  state-2
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
++  on-save  !>(state)
++  on-load
  |=  =old=vase
  ^-  (quip card _this)
  =+  !<(old=versioned-state old-vase)
  =|  cards=(list card)
  |^
  ?-    -.old
      %0  
    %_    $
      -.old  %1
    ::
        validators.old
      (~(put in validators.old) %graph-validator-link)
    ::
        cards
      %+  weld  cards
      %+  turn
        ~(tap in (~(put in validators.old) %graph-validator-link))
      |=  validator=@t
      ^-  card
      =/  =wire  /validator/[validator]
      =/  =rave:clay  [%sing %b [%da now.bowl] /[validator]]
      [%pass wire %arvo %c %warp our.bowl [%home `rave]]
    ::
        graphs.old
      %-  ~(run by graphs.old)
      |=  [=graph:store q=(unit mark)]
      ^-  [graph:store (unit mark)]
      :-  (convert-unix-timestamped-graph graph)
      ?^  q  q
      `%graph-validator-link
    ::
        update-logs.old
      %-  ~(run by update-logs.old)
      |=(a=* *update-log:store)
    ==
  ::
      %1
    %_  $
      -.old       %2
      graphs.old  (~(run by graphs.old) change-revision-graph)
    ::
        update-logs.old
      %-  ~(run by update-logs.old)
      |=(a=* *update-log:store)
    ==
  ::
    %2  [cards this(state old)]
  ==
  ::
  ++  change-revision-graph
    |=  [=graph:store q=(unit mark)]
    ^-  [graph:store (unit mark)]
    |^
    :_  q
    ?+    q  graph
      [~ %graph-validator-link]     convert-links
      [~ %graph-validator-publish]  convert-publish
    ==
    ::
    ++  convert-links
      %+  gas:orm  *graph:store
      %+  turn  (tap:orm graph)
      |=  [=atom =node:store]
      ^-  [^atom node:store]
      ::  top-level
      ::
      :+  atom  post.node
      ?:  ?=(%empty -.children.node)
        [%empty ~]
      :-  %graph
      %+  gas:orm  *graph:store
      %+  turn  (tap:orm p.children.node)
      |=  [=^atom =node:store]
      ^-  [^^atom node:store]
      ::  existing comments get turned into containers for revisions
      ::
      :^    atom
          post.node(contents ~, hash ~)
        %graph
      %+  gas:orm  *graph:store
      :_  ~  :-  %1
      :_  [%empty ~]
      post.node(index (snoc index.post.node atom), hash ~)
    ::
    ++  convert-publish
      %+  gas:orm  *graph:store
      %+  turn  (tap:orm graph)
      |=  [=atom =node:store]
      ^-  [^atom node:store]
      ::  top-level
      ::
      :+  atom  post.node
      ?:  ?=(%empty -.children.node)
        [%empty ~]
      :-  %graph
      %+  gas:orm  *graph:store
      %+  turn  (tap:orm p.children.node)
      |=  [=^atom =node:store]
      ^-  [^^atom node:store]
      ::  existing container for publish note revisions
      ::
      ?+    atom  !!
          %1  [atom node]
          %2
        :+  atom  post.node
        ?:  ?=(%empty -.children.node)
          [%empty ~]
        :-  %graph
        %+  gas:orm  *graph:store
        %+  turn  (tap:orm p.children.node)
        |=  [=^^atom =node:store]
        ^-  [^^^atom node:store]
        :+  atom  post.node(contents ~, hash ~)
        :-  %graph
        %+  gas:orm  *graph:store
        :_  ~  :-  %1
        :_  [%empty ~]
        post.node(index (snoc index.post.node atom), hash ~)
      ==
    --
  ::  
  ++  maybe-unix-to-da
    |=  =atom
    ^-  @
    ::  (bex 127) is roughly 226AD
    ?.  (lte atom (bex 127))
      atom
    (add ~1970.1.1 (div (mul ~s1 atom) 1.000))
  ::
  ++  convert-unix-timestamped-node
    |=  =node:store
    ^-  node:store
    =.  index.post.node
      (convert-unix-timestamped-index index.post.node)
    ?.  ?=(%graph -.children.node)
      node
    :+  post.node
      %graph
    (convert-unix-timestamped-graph p.children.node)
  ::
  ++  convert-unix-timestamped-index
    |=  =index:store
    (turn index maybe-unix-to-da)
  ::
  ++  convert-unix-timestamped-graph
    |=  =graph:store
    %+  gas:orm  *graph:store
    %+  turn
      (tap:orm graph)
    |=  [=atom =node:store]
    ^-  [^atom node:store]
    :-  (maybe-unix-to-da atom)
    (convert-unix-timestamped-node node)
  --
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
        %noun          (debug !<(debug-input vase))
        %import        (poke-import q.vase)
    ==
  [cards this]
  ::
  ++  graph-update
    |=  =update:store
    ^-  (quip card _state)
    |^
    ?>  ?=(%0 -.update)
    =?  p.update  =(p.update *time)  now.bowl
    ?-  -.q.update
        %add-graph          (add-graph p.update +.q.update)
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
      ?>  (validate-graph graph mark)
      =/  =logged-update:store
        [%0 time %add-graph resource graph mark overwrite]
      =/  =update-log:store
        (gas:orm-log ~ [time logged-update] ~)
      :_  %_  state
              graphs       (~(put by graphs) resource [graph mark])
              update-logs  (~(put by update-logs) resource update-log)
              archive      (~(del by archive) resource)
            ::
              validators
            ?~  mark  validators
            (~(put in validators) u.mark)
          ==
      %-  zing
      :~  (give [/keys ~] %keys (~(put in ~(key by graphs)) resource))
          (give [/updates ~] %add-graph resource *graph:store mark overwrite)
          ?~  mark  ~
          ?:  (~(has in validators) u.mark)  ~
          =/  wire  /validator/[u.mark]
          =/  =rave:clay  [%sing %b [%da now.bowl] /[u.mark]]
          [%pass wire %arvo %c %warp our.bowl [%home `rave]]~
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
        :_  mark
        (add-node-list resource graph mark (sort-nodes nodes))
      ==
      ::
      ++  check-for-duplicates
        |=  [=graph:store nodes=(set index:store)]
        ^-  ?
        =/  node-list  ~(tap in nodes)
        |-
        ?~  node-list  %.n
        ?:  (has-node graph i.node-list)  %.y
        $(node-list t.node-list)
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
        ~|  "validation of node failed using mark {<mark>}"
        ?>  (validate-graph (gas:orm ~ [i.index node]~) mark)
        =*  atom   i.index
        %^  put:orm
            graph
          atom
        ::  add child
        ::
        ?~  t.index
          =*  p  post.node
          ?~  hash.p  node(signatures.post *signatures:store)
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
      =/  [=graph:store mark=(unit mark:store)]
        (~(got by graphs) resource)
      =/  =update-log:store  (~(got by update-logs) resource)
      =.  update-log
        (put:orm-log update-log time [%0 time [%remove-nodes resource indices]])
      =/  [affected-indices=(set index:store) new-graph=graph:store]
        (remove-indices resource graph (sort ~(tap in indices) by-lent))
      ::
      :-  (give [/updates]~ [%remove-nodes resource (~(uni in indices) affected-indices)])
      %_  state
          update-logs  (~(put by update-logs) resource update-log)
          graphs
        %+  ~(put by graphs)
          resource
        [new-graph mark]
      ==
      ::
      :: we always want to remove the deepest node first,
      :: so we don't remove parents before children
      ++  by-lent
        |*  [a=(list) b=(list)]
        ^-  ?
        (gth (lent a) (lent b))
      ::
      ++  remove-indices
        =|  affected=(set index:store)
        |=  [=resource:store =graph:store indices=(list index:store)]
        ^-  [(set index:store) graph:store]
        ?~  indices  [affected graph]
        =^  new-affected  graph
          (remove-index graph i.indices)
        %_  $
            indices  t.indices
            affected  (~(uni in affected) new-affected)
        ==
      ::
      ++  get-descendants
        |=  =graph:store
        =|  indices=(list index:store)
        =/  nodes  (tap:orm:store graph)
        %-  ~(gas in *(set index:store))
        |-  =*  tap-nodes  $
        ^+  indices
        %-  zing
        %+  turn  nodes
        |=  [atom =node:store]
        ^-  (list index:store)
        %+  welp
          index.post.node^~
        ?.  ?=(%graph -.children.node)
          ~
        %_  tap-nodes
          nodes  (tap:orm p.children.node)
        ==
      ::
      ++  remove-index
        =|  indices=(set index:store)
        |=  [=graph:store =index:store]
        ^-  [(set index:store) graph:store]
        ?~  index  [indices graph]
        =*  atom   i.index
        ::  last index in list
        ::
        ?~  t.index
          =^  rm-node  graph  (del:orm graph atom)
          ?~  rm-node  `graph
          ?.  ?=(%graph -.children.u.rm-node)
            `graph
          =/  new-indices
            (get-descendants p.children.u.rm-node)
          [(~(uni in indices) new-indices) graph]
        =/  =node:store
          ~|  "parent index does not exist to remove a node from!"
          (need (get:orm graph atom))
        ~|  "child index does not exist to remove a node from!"
        ?>  ?=(%graph -.children.node)
        =^  new-indices  p.children.node
          $(graph p.children.node, index t.index)
        :-  (~(uni in indices) new-indices)
        (put:orm graph atom node)
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
        (put:orm-log update-log time [%0 time [%add-signatures uid signatures]])
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
          ~|  "cannot add signatures to a node missing a hash"
          ?>  ?=(^ hash.post.node)
          ~|  "signatures did not match public keys!"
          ?>  (are-signatures-valid:sigs our.bowl signatures u.hash.post.node now.bowl)
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
      =/  [=graph:store mark=(unit mark:store)]
        (~(got by graphs) resource)
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
            %remove-nodes       update(resource.q resource)
            %add-signatures     update(resource.uid.q resource)
            %remove-signatures  update(resource.uid.q resource)
        ==
      $(cards (weld cards crds), updates t.updates)
    ::
    ++  give
      |=  [paths=(list path) update=update-0:store]
      ^-  (list card)
      [%give %fact paths [%graph-update !>([%0 now.bowl update])]]~
    --
  ::
  ++  debug
    |=  =debug-input
    ^-  (quip card _state)
    =/  [=graph:store mark=(unit mark:store)]
      (~(got by graphs) resource.debug-input)
    ?>  (validate-graph graph mark)
    [~ state]
  ::
  ++  validate-graph
    |=  [=graph:store mark=(unit mark:store)]
    ^-  ?
    ?~  mark   %.y
    ?~  graph  %.y
    =/  =dais:clay
      .^  =dais:clay
          %cb
          /(scot %p our.bowl)/[q.byk.bowl]/(scot %da now.bowl)/[u.mark]
      ==
    %+  roll  (tap:orm graph)
    |=  [[=atom =node:store] out=?]
    ?&  out
        =(%& -:(mule |.((vale:dais [atom post.node]))))
        ?-  -.children.node
            %empty  %.y
            %graph  ^$(graph p.children.node)
        ==
    ==
  ::
  ++  poke-import
    |=  arc=*
    ^-  (quip card _state)
    |^
    =/  sty=state-2  [%2 (remake-network ;;(tree-network +.arc))]
    :_  sty
    %+  turn  ~(tap by graphs.sty)
    |=  [rid=resource:store =marked-graph:store]
    ^-  card
    ?:  =(our.bowl entity.rid)
      =/  =cage  [%push-hook-action !>([%add rid])]
      [%pass / %agent [our.bowl %graph-push-hook] %poke cage]
    (try-rejoin rid 0)
    ::
    +$  tree-network
      $:  graphs=tree-graphs
          tag-queries=(tree [term (tree resource:store)])
          update-logs=tree-update-logs
          archive=tree-graphs
          validators=(tree ^mark)
      ==
    +$  tree-graphs          (tree [resource:store tree-marked-graph])
    +$  tree-marked-graph    [p=tree-graph q=(unit ^mark)]
    +$  tree-graph           (tree [atom tree-node])
    +$  tree-node            [post=tree-post children=tree-internal-graph]
    +$  tree-internal-graph
      $~  [%empty ~]
      $%  [%graph p=tree-graph]
          [%empty ~]
      ==
    +$  tree-update-logs     (tree [resource:store tree-update-log])
    +$  tree-update-log      (tree [time tree-logged-update])
    +$  tree-logged-update
      $:  %0
          p=time
          $=  q
          $%  [%add-graph =resource:store =tree-graph mark=(unit ^mark) ow=?]
              [%add-nodes =resource:store nodes=(tree [index:store tree-node])]
              [%remove-nodes =resource:store indices=(tree index:store)]
              [%add-signatures =uid:store signatures=tree-signatures]
              [%remove-signatures =uid:store signatures=tree-signatures]
          ==
      ==
    +$  tree-signatures      (tree signature:store)
    +$  tree-post
      $:  author=ship
          =index:store
          time-sent=time
          contents=(list content:store)
          hash=(unit hash:store)
          signatures=tree-signatures
      ==
    ::
    ++  remake-network
      |=  t=tree-network
      ^-  network:store
      :*  (remake-graphs graphs.t)
          (remake-jug tag-queries.t)
          (remake-update-logs update-logs.t)
          (remake-graphs archive.t)
          (remake-set validators.t)
      ==
    ::
    ++  remake-graphs
      |=  t=tree-graphs
      ^-  graphs:store
      %-  remake-map
      (~(run by t) remake-marked-graph)
    ::
    ++  remake-marked-graph
      |=  t=tree-marked-graph
      ^-  marked-graph:store
      [(remake-graph p.t) q.t]
    ::
    ++  remake-graph
      |=  t=tree-graph
      ^-  graph:store
      %+  gas:orm  *graph:store
      %+  turn  ~(tap by t)
      |=  [a=atom tn=tree-node]
      ^-  [atom node:store]
      [a (remake-node tn)]
    ::
    ++  remake-internal-graph
      |=  t=tree-internal-graph
      ^-  internal-graph:store
      ?:  ?=(%empty -.t)
        [%empty ~]
      [%graph (remake-graph p.t)]
    ::
    ++  remake-node
      |=  t=tree-node
      ^-  node:store
      :-  (remake-post post.t)
      (remake-internal-graph children.t)
    ::
    ++  remake-update-logs
      |=  t=tree-update-logs
      ^-  update-logs:store
      %-  remake-map
      (~(run by t) remake-update-log)
    ::
    ++  remake-update-log
      |=  t=tree-update-log
      ^-  update-log:store
      =/  ulm  ((ordered-map time logged-update:store) gth)
      %+  gas:ulm  *update-log:store
      %+  turn  ~(tap by t)
      |=  [=time tlu=tree-logged-update]
      ^-  [^time logged-update:store]
      [time (remake-logged-update tlu)]
    ::
    ++  remake-logged-update
      |=  t=tree-logged-update
      ^-  logged-update:store
      :+  %0  p.t
      ?-  -.q.t
          %add-graph
        :*  %add-graph
            resource.q.t
            (remake-graph tree-graph.q.t)
            mark.q.t
            ow.q.t
        ==
      ::
          %add-nodes
        :-  %add-nodes
        :-  resource.q.t
        %-  remake-map
        (~(run by nodes.q.t) remake-node)
      ::
          %remove-nodes
        [%remove-nodes resource.q.t (remake-set indices.q.t)]
      ::
          %add-signatures
        [%add-signatures uid.q.t (remake-set signatures.q.t)]
      ::
          %remove-signatures
        [%remove-signatures uid.q.t (remake-set signatures.q.t)]
      ==
    ::
    ++  remake-post
      |=  t=tree-post
      ^-  post:store
      t(signatures (remake-set signatures.t))
    --
  ::
  ++  try-rejoin
    |=  [rid=resource:store nack-count=@]
    ^-  card
    =/  res-path  (en-path:res rid)
    =/  wire  [%try-rejoin (scot %ud nack-count) res-path]
    [%pass wire %agent [entity.rid %graph-push-hook] %watch resource+res-path]
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
    :-  ~  :-  ~  :-  %graph-update
    !>(`update:store`[%0 now.bowl [%keys ~(key by graphs)]])
  ::
      [%x %tags ~]
    :-  ~  :-  ~  :-  %graph-update
    !>(`update:store`[%0 now.bowl [%tags ~(key by tag-queries)]])
  ::
      [%x %tag-queries ~]
    :-  ~  :-  ~  :-  %graph-update
    !>(`update:store`[%0 now.bowl [%tag-queries tag-queries]])
  ::
      [%x %graph @ @ ~]
    =/  =ship   (slav %p i.t.t.path)
    =/  =term   i.t.t.t.path
    =/  result=(unit marked-graph:store)
      (~(get by graphs) [ship term])
    ?~  result  [~ ~]
    :-  ~  :-  ~  :-  %graph-update
    !>  ^-  update:store
    :+  %0
      now.bowl
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
    :-  ~  :-  ~  :-  %graph-update
    !>  ^-  update:store
    :+  %0
      now.bowl
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
    :-  ~  :-  ~  :-  %graph-update
    !>  ^-  update:store
    :+  %0  now.bowl
    :+  %add-nodes
      [ship term]
    %-  ~(gas by *(map index:store node:store))
    %+  turn  (tap:orm `graph:store`(subset:orm p.u.graph start end))
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
    :-  ~  :-  ~  :-  %graph-update
    !>  ^-  update:store
    :+  %0
      now.bowl
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
    :-  ~  :-  ~  :-  %graph-update
    !>  ^-  update:store
    :+  %0
      now.bowl
    :+  %add-nodes
      [ship term]
    %-  ~(gas by *(map index:store node:store))
    :: TODO time complexity not desirable
    ::   replace with custom ordered map functions
    %+  turn  
      =-  ?.(older (slag (safe-sub (lent -) count) -) (scag count -))
      %-  tap:orm
      %+  subset:orm  u.graph
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
    :-  ~  :-  ~  :-  %graph-update
    !>  ^-  update:store
    :+  %0
      now.bowl
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
      :-  ~  :-  ~  :-  %graph-update
      !>  ^-  update:store
      :+  %0
        now.bowl
      :+  %add-nodes
        [ship term]
      %-  ~(gas by *(map index:store node:store))
      %+  turn  (tap:orm `graph:store`(subset:orm p.children.u.node end start))
      |=  [=atom =node:store]
      ^-  [index:store node:store]
      [(snoc index atom) node]
    ==
  ::
      [%x %update-log-subset @ @ @ @ ~]
    =/  =ship   (slav %p i.t.t.path)
    =/  =term   i.t.t.t.path
    =/  start=(unit time)  (slaw %da i.t.t.t.t.path)
    =/  end=(unit time)    (slaw %da i.t.t.t.t.t.path)
    =/  update-log=(unit update-log:store)  (~(get by update-logs) [ship term])
    ?~  update-log  [~ ~]
    ::  orm-log is ordered backwards, so swap start and end
    ``noun+!>((subset:orm-log u.update-log end start))
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
      (peek:orm-log:store update-log)
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
      [%graph *]  [~ this]
  ::
      [%validator @ ~]
    :_  this
    =*  validator  i.t.wire
    =/  =rave:clay  [%next %b [%da now.bowl] /[validator]]
    [%pass wire %arvo %c %warp our.bowl [%home `rave]]~
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
