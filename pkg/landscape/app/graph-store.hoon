::  graph-store [landscape]
::
/+  store=graph-store, sigs=signatures, res=resource, default-agent, dbug, verb, via
~%  %graph-store-top  ..part  ~
|%
+$  card  card:agent:gall
+$  versioned-state
  $%  [%0 *]
      [%1 *]
      [%2 network:zero:store]
      [%3 network:one:store]
      [%4 network:store]
      state-5
  ==
::
+$  state-5  [%5 network:store]
++  orm      orm:store
++  mar      %graph-update-3
--
::
=|  state-5
=*  state  -
::
%-  agent:dbug
%+  verb  &
^-  agent:gall
~%  %graph-store-agent  ..card  ~
=<  
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
    ev-core  ~(. +> [bowl ~])
::
++  on-init  [~ this]
++  on-save  !>(state)
++  on-load
  |=  =old=vase
  ^-  (quip card _this)
  =+  !<(old=state-5 old-vase)
  =|  cards=(list card)
  [cards this(state old)]
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
    |=  *
    *(list card)
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
    ::
        %graph-update-4     
      =+  !<(=update:store vase)
      abet:abet:(~(diff plot:ev-core p.update) ~ q.update)
      :: %graph-update-3  (graph-update !<(update:store vase))
      ::  %import          (poke-import q.vase)
      %noun  
      =+  ;;(rid=res q.vase)
      abet:abet:~(start-syncing plot:ev-core rid)
    ==
  [cards this]
  ::
  ++  graph-update
    |^
    `state
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
      !!
    ::
    ++  remove-graph
      |=  =resource:store
      ^-  (quip card _state)
      ?<  (~(has by archive) resource)
      ?>  (~(has by graphs) resource)
      :-  (give [/updates /keys ~] [%remove-graph resource])
      %_  state
          graphs       (~(del by graphs) resource)
          ::update-logs  (~(del by update-logs) resource)
      ==
    ::
    ++  add-nodes
      |=  [=time =resource:store nodes=(map index:store node:store)]
      ^-  (quip card _state)
      |^
      =/  [=graph:store mark=(unit mark:store)]
        [*graph:store ~]
      ~|  "cannot add duplicate nodes to {<resource>}"
      ?<  (check-for-duplicates graph ~(key by nodes))
      ~|  "validation of nodes failed using mark {<mark>}"
      =^  is-valid  state
        (check-validity ~(tap by nodes) mark)
      !!
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
            (has:orm graph i.index)
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
          ?~  hash.p
            node(signatures.p.post ~)
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
          (got:orm graph atom)
        %_  parent
            children
          ^-  internal-graph:store
          :-  %graph
          %_  $
            index  t.index
          ::
              parent-hash
            ?:  ?=(%| -.post.parent)
              `p.post.parent
            hash.p.post.parent
          ::
              graph
            ?.  ?=(%graph -.children.parent)
              ~
            p.children.parent
          ==
        ==
      --
    ::
    ++  remove-posts
      |=  [=time =resource:store indices=(set index:store)]
      ^-  (quip card _state)
      |^
      =/  [=graph:store mark=(unit mark:store)]
        [*graph:store ~]
      ::=/  count  (~(gut by update-logs) resource 0)

      `state
        ::update-logs  (~(put by update-logs) resource update-log)
      ::
::          graphs
::        %+  ~(put by graphs)
::          resource
::        :_  mark
::        %^  remove-indices
::            resource
::          graph
::        (sort ~(tap in indices) by-lent)
::      ==
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
            (got:orm graph atom)
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
          (got:orm graph atom)
        ~|  "child index does not exist to remove a node from!"
        ?>  ?=(%graph -.children.parent)
        %_    parent
            p.children
          %_  $
            index  t.index
            graph  p.children.parent
          ::
              parent-hash
            ?:  ?=(%| -.post.parent)
              `p.post.parent
            hash.p.post.parent
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
        [*graph:store ~]
      :-  ~
      %_  state
          graphs  graphs
        :: %+  ~(put by graphs)  resource
        :: [(add-at-index graph index.uid signatures) mark]
      ==
      ::
      ++  add-at-index
        |=  [=graph:store =index:store =signatures:store]
        ^-  graph:store
        ?~  index  graph
        =*  atom   i.index
        =/  =node:store
          ~|  "node does not exist to add signatures to!"
          (got:orm graph atom)
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
          ?>  %:  are-signatures-valid:sigs
                our.bowl  signatures
                u.hash.p.post.node  now.bowl
              ==
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
        [*graph:store ~]

      :-  ~
      %_  state
          :: update-logs  (~(put by update-logs) resource count)
          graphs  graphs
        ::  %+  ~(put by graphs)  resource
        :: [(remove-at-index graph index.uid signatures) mark]
      ==
      ::
      ++  remove-at-index
        |=  [=graph:store =index:store =signatures:store]
        ^-  graph:store
        ?~  index  graph
        =*  atom   i.index
        =/  =node:store
          ~|  "node does not exist to add signatures to!"
          (got:orm graph atom)
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
      :-  ~
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
          :: update-logs  (~(del by update-logs) resource)
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
          :: update-logs  (~(put by update-logs) resource ~)
      ==
    ::
    ++  give
      |=  *
      *(list card)
    --
  ::
  ++  validate-graph
    |=  [=graph:store mark=(unit mark:store)]
    ^-  [? _state]
    ?~  mark
      [%.y state]
    =/  validate=$-(indexed-post:store indexed-post:store)
      .^  $-(indexed-post:store indexed-post:store)
          %cf
          (scot %p our.bowl)
          q.byk.bowl
          (scot %da now.bowl)
          u.mark
          %graph-indexed-post
          ~
      ==
    :_  state
    |-  ^-  ?
    ?~  graph  %.y
    %+  all:orm  graph
    |=  [=atom =node:store]
    ^-  ?
    ?&  ?|  ?=(%| -.post.node)
            ?=(^ (validate [atom p.post.node]))
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
    =^  cards  state
      `state ::(import:store arc our.bowl)
    [cards state]
  --
::
++  on-peek
  ~/  %graph-store-peek
  |=  =path
  ^-  (unit (unit cage))
  ?+    path  (on-peek:def path)
    [%x %export ~]  ``noun+!>(state)
  ::
      [%x %keys ~]
    :-  ~  :-  ~  :-  mar
    !>([now.bowl [%keys ~(key by graphs)]])
  ::
      [%x %tag-queries *]
    :-  ~  :-  ~  :-  mar
    !>  
    :-  now.bowl
    ?+  t.t.path  (on-peek:def path)
      ~          [%tag-queries tag-queries]
      [%tags ~]  [%tags ~(key by tag-queries)]
    ==
  ::
      [%x %archive @ @ ~]
    =/  =ship   (slav %p i.t.t.path)
    =/  =term   i.t.t.t.path
    =/  root-graph=(unit root-graph:store)
      (~(get by graphs) [ship term])
    ?~  root-graph  [~ ~]
    =,  u.root-graph
    :-  ~  :-  ~  :-  mar
    !>([now.bowl [%add-graph [ship term] graph mark %.y]])
  ::
  ::
      [%x %graph @ @ *]
    =/  =ship   (slav %p i.t.t.path)
    =/  =term   i.t.t.t.path
    =/  root-graph=(unit root-graph:store)
      (~(get by graphs) [ship term])
    ?~  root-graph  [~ ~]
    =,  u.root-graph
    ?+    t.t.t.t.path  (on-peek:def path)
        ~
      :-  ~  :-  ~  :-  mar
      !>([now.bowl [%add-graph [ship term] graph mark %.y]])
    ::
        [%mark ~]
      ``noun+!>(`(unit ^mark)`mark)
    ::
        [%subset ?(%lone %kith) @ @ ~]
      =/  start=(unit atom)  (rush i.t.t.t.t.t.t.path dem:ag)
      =/  end=(unit atom)    (rush i.t.t.t.t.t.t.t.path dem:ag)
      :-  ~  :-  ~  :-  mar
      !>  
      :^  now.bowl  %add-nodes  [ship term]
      %-  ~(gas by *(map index:store node:store))
      %+  turn  (tap:orm (lot:orm graph start end))
      |=  [=atom =node:store]
      ^-  [index:store node:store]
      :-  atom^~
      ?:  ?=(%kith i.t.t.t.t.t.path)
        node
      node(children [%empty ~])
    ::
        [%node *]
      |^
      =*    pax  t.t.t.t.t.path
      ?+    pax  (on-peek:def path)
          [%exists ^]
        =/  =index:store
          (turn t.pax (cury slav %ud))
        =/  node  (get-node graph index)
        ``noun+!>(`?`?=(^ node))
      ::
          [%index ?(%lone %kith) ^]
        =/  =index:store
          (turn t.t.pax (cury slav %ud))
        =/  node  (get-node graph index)
        ?~  node  [~ ~]
        :-  ~  :-  ~  :-  mar
        !>  
        :^  now.bowl  %add-nodes  [ship term]
        %-  ~(gas by *(map index:store node:store))
        :_  ~  :-  index
        ?:  ?=(%kith i.t.pax)  u.node
        u.node(children [%empty ~])
      ::
          [%children ?(%lone %kith) @ @ *]
        =/  start=(unit atom)  (rush i.t.t.path dem:ag)
        =/  end=(unit atom)    (rush i.t.t.t.path dem:ag)
        =/  =index:store
          (turn t.t.t.t.pax (cury slav %ud))
        =/  node  (get-node graph index)
        ?:  ?&  ?=(~ node)
                ?=(^ index)
            ==
          [~ ~]
        =/  children=graph:store
          ?~  node
            graph
          ?:  ?=(%empty -.children.u.node)
            ~
          p.children.u.node
        :-  ~  :-  ~  :-  mar
        !>  
        :^  now.bowl  %add-nodes  [ship term]
        %-  ~(gas by *(map index:store node:store))
        %+  turn  (tap:orm (lot:orm children end start))
        |=  [=atom =node:store]
        ^-  [index:store node:store]
        :-  (snoc index atom)
        ?:  ?=(%kith i.t.pax)  node
        node(children [%empty ~])
      ::
          [%siblings ?(%older %newer %oldest %newest) ?(%lone %kith) @ *]
        =/  count  (slav %ud i.t.t.t.pax)
        =/  =index:store
          (turn t.t.t.t.pax (cury slav %ud))
        =/  parent=index:store  (snip index)
        =/  node
          (get-node graph ?:(?=(?(%oldest %newest) i.t.pax) index parent))
        =/  children=graph:store
          ?~  node
            graph
          ?:  ?=(%empty -.children.u.node)
            ~
          p.children.u.node
        :-  ~  :-  ~  :-  mar
        !>  
        :^  now.bowl  %add-nodes  [ship term]
        %-  ~(gas by *(map index:store node:store))
        %+  turn
          ?-    i.t.pax
            %oldest  (scag count (bap:orm children))
            %older   (tab:orm children `(rear index) count)
            %newest  (scag count (tap:orm children))
          ::
              %newer
            %+  slag  (safe-sub (lent -) count)
            (tap:orm (lot:orm children ~ `(rear index)))
          ==
        |=  [=atom =node:store]
        ^-  [index:store node:store]
        :-  %-  snoc
            :_  atom
            ?:(?=(?(%newest %oldest) i.t.pax) index parent)
        ?:  ?=(%kith i.t.t.pax)  node
        node(children [%empty ~])
      ::
          [%firstborn ^]
        |^
        =/  =index:store
          (turn t.pax (cury slav %ud))
        %-  (bond |.(`(unit (unit cage))`[~ ~]))
        %+  biff
          (collect-parents graph index)
        (corl some collect-firstborn)
        ::
        ++  collect-parents
          |=  [=graph:store =index:store]
          ^-  (unit [node:store index:store (map index:store node:store)])
          =|  =(map index:store node:store)
          =|  =node:store
          =|  ind=index:store
          =/  len  (lent index)
          |-
          ?:  (gte (lent ind) len)
            `[node ind map]
          ?>  ?=(^ index)
          =*  atom  i.index
          ?.  (has:orm graph atom)
            ~
          =:  node   (got:orm graph atom)
              ind    (snoc ind atom)
            ==
          ?:  ?=(%empty -.children.node)
            ?.  (gte (lent ind) len)
              ~
            `[node ind (~(put by map) ind node)]
          %_  $
            index  t.index
            graph  p.children.node
            map    (~(put by map) ind node(children empty+~))
          ==
        ::
        ++  collect-firstborn
          |=  [=node:store =index:store =(map index:store node:store)]
          ^-  (unit (unit cage))
          ?:  ?=(%empty -.children.node)
            :-  ~  :-  ~  :-  mar
            !>([now.bowl [%add-nodes [ship term] map]])
          =/  item=[k=atom v=node:store]
            (need (ram:orm p.children.node))
          =.  index  (snoc index k.item)
          $(map (~(put by map) index v.item(children empty+~)), node v.item)
        --
      ==
      ::
      ++  get-node
        |=  [=graph:store =index:store]
        ^-  (unit node:store)
        =|  node=(unit node:store)
        |-
        ?~  index    node
        ?~  t.index  (get:orm graph i.index)
        =.  node     (get:orm graph i.index)
        ?~  node     ~
        ?:  ?=(%empty -.children.u.node)
          ~
        $(graph p.children.u.node, index t.index)
      ::
      ++  safe-sub
        |=  [a=@ b=@]
        ^-  @
        ?:((gte b a) 0 (sub a b))
      --
    ::
        [%depth-first @ @ ~]
      =/  count=(unit atom)  (rush i.t.t.t.t.path dem:ag)
      =/  start=(unit atom)  (rush i.t.t.t.t.t.path dem:ag)
      ?:  ?=(~ count)
        [~ ~]
      :-  ~  :-  ~  :-  mar
      !>  
      :^  now.bowl  %add-nodes  [ship term]
      =*  a  u.count
      =/  b=(list (pair atom node:store))
        (tab:orm graph start u.count)
      =|  c=index:store
      =|  d=(map index:store node:store)
      =|  e=@ud
      =-  d
      |-  ^-  [e=@ud d=(map index:store node:store)]
      ?:  ?|(?=(~ b) =(e a))
        [e d]
      =*  atom  p.i.b
      =*  node  q.i.b
      =.  c     (snoc c atom)
      ?-    -.children.node
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
          c  (snip c)
        ==
      ==
    ==
  ==
::
++  on-arvo   
  |=  [=wire sign=sign-arvo]
  ~|  wire/wire
  ^-  (quip card _this)
  =^  cards  state
   ?+  wire  (on-arvo:def wire sign)
   ::
       [%plot @ @ @ *]
     =/  =ship  (slav %p i.t.wire)
     =/  rid=res  [ship i.t.t.wire]
     abet:abet:(~(arvo plot:ev-core rid) [t.t.t.t.wire sign])
   ==
  [cards this]
::
++  on-agent  on-agent:def
++  on-leave  on-leave:def
++  on-fail   on-fail:def
--
|_  [=bowl:gall cards=(list card)]
++  abet  ~&  (flop cards)  [(flop cards) state]
++  ev-core  .
++  emit  |=(=card ev-core(cards [card cards]))
++  plot
  |_  rid=res
  ++  plot  .
  ++  emit  |=(=card plot(ev-core (emit:ev-core card)))
  ++  pass-clay
    |=  [sfix=wire =task:clay]
    =/  =wire  
      (welp /plot/(scot %p entity.rid)/[name.rid]/(scot %uv eny.bowl) sfix)
    (emit %pass wire %arvo %c task)
  ++  abet  ev-core
  ::  +|  %helpers
  ++  exists      (~(has by graphs) rid)
  ++  root-graph  (~(got by graphs) rid)
  ++  archived    %.n  :: TODO: fix
  ++  via        ~(. ^via graph:root-graph)
  ::
  ::  +dirt: constants for clay
  ++  dirt
    |%
    ++  desk  %demo
    ++  loc  /graph-store/(scot %p entity.rid)/[name.rid]
    ++  pag  (welp log /pages)
    ++  log  (welp loc /logs)
    ++  diff
      |=  count=@ud
      (welp log /(scot %ud count)/noun)
    ::
    ++  current  
      =/  =root-graph:store  (~(gut by graphs) rid *root-graph:store)
      count:root-graph
    ++  next  +(current)
    ::
    ::  +now: current case, adjusted for potential clock skew
    ::    TODO: what should this constant be?
    ::
    ::  ++  now  `case:clay`da+(sub now.bowl 1)
    ++  now  `case:clay`da+now.bowl
    ++  warp-dir
      ^+  plot
      =/  =mood:clay  [%z now log]
      =/  =rave:clay  [%next mood]
      =/  =riff:clay  [desk `rave]
      (pass-clay /warp/log %warp entity.rid riff)
    ::
    ++  warp-node
      |=  count=@ud
      ^+  plot
      =/  =mood:clay  [%x now (diff count)]
      =/  =rave:clay  [%sing mood]
      =/  =riff:clay  [desk `rave]
      (pass-clay /warp/node %warp entity.rid riff)
    --
  ::
  ++  check-for-duplicates
    |=  indices=(set index:store)
    (lien ~(tap in indices) has:via)
  ::
  ++  log-diff
    |=  [prov=(unit ship) =diff:store]
    ^+  plot
    ?.  ?=(log-tags:store -.diff)
      plot
    ::
    =.  graphs
      (~(jab by graphs) rid |=(root-graph:store +<(count +(count))))
    =/  =miso:clay  [%ins noun+!>(diff)]
    =/  =soba:clay  [=,(dirt (diff current)) miso]~
    =/  =nori:clay  [%& soba]
    =/  =task:clay  [%info desk:dirt nori]
    =?  plot  ?=(^ prov)
      (warp-node:dirt next:dirt)
    (pass-clay /write task)
  ::
  ++  validator
    ^-  $-(indexed-post:store ?)
    =/  mark=(unit mark)  mark:root-graph
    ?~  mark  |=(=indexed-post:store %.y)
    =/  validate=$-(indexed-post:store indexed-post:store)
      .^  $-(indexed-post:store indexed-post:store)
        %cf
        (scot %p our.bowl)
        q.byk.bowl
        (scot %da now.bowl)
        u.mark
        %graph-indexed-post
        ~
      ==
    |=(=indexed-post:store =(~ (mole |.((validate indexed-post)))))
  ::
  ++  validate-graph  (all:via validator)
  ::
  ++  put-graph
    |=  =graph:store
    ^+  plot
    =/  new   %*(. root-graph graph graph)
    =.  graphs  (~(put by graphs) rid new)
    plot
  ::
  ::  +|  %action
  ++  start-syncing
    (warp-node:dirt 1)
  ::
  ++  arvo
    |=  [=wire sign=sign-arvo]
    ~|  "handling arvo sign for {<rid>}"
    ~|  wire/wire
    |^  ^+  plot
    ?+  wire  ~|(bad-wire/wire !!)
      [%write ~]  plot
    ::
        [%warp ?(%node %log) ~]
      ?>  ?=([?(%clay %behn) %writ *] sign)
      =/  is-node=?  ?=(%node i.t.wire)
      ?~  p.sign  
        (empty-writ is-node)
      ?:  is-node
        (take-node r.u.p.sign)
      (warp-node:dirt next:dirt)
    ==
    ::
    ++  take-node
      |=  =cage
      ~|  bad-sign/p.cage
      ?>  =(%noun p.cage)
      ::  TODO: versioning
      =+  ;;(d=diff:store q.q.cage)
      ~&  "Took update: {<current:dirt>}"
      =/  s=(unit ship)  [~ src.bowl]
      (diff:plot s d)
    ::
    ++  empty-writ
      |=  is-node=?
      ~&  "empty clay response"
      ~|  "Empty clay response for %cy"
      ?>  is-node
      warp-dir:dirt
    --
  ::
  ++  diff
    |=  [prov=(unit ship) =diff:store]
    |^  ^+  plot
    =.  plot
      ?+  -.diff  !!
        %add-graph  (add-graph +.diff)
        %add-nodes  (add-nodes +.diff)
        %remove-posts  (remove-posts +.diff)
        ::  %add-signatures  (add-signatures +.diff)
        ::  %remove-signatures  (remove-signatures +.diff)
      ==
    (log-diff prov diff)
    ::
    ++  add-graph
      |=  [=graph:store mark=(unit mark) overwrite=?]
      ^+  plot
      ?>  ?|(overwrite &(!archived !exists))
      ~|  "validation of graph {<rid>} failed using mark {<mark>}"
      =/  =root-graph:store  [graph mark prov 0 %.n]
      =.  graphs             (~(put by graphs) rid root-graph)
      :: ?>  validate-graph
      plot
    ::
    ++  add-nodes
      |=  nodes=(map index:store node:store)
      ::?<  (check-for-duplicates ~(key by nodes))
      =/  nodes  ~(tap by nodes)
      =/  graph  graph:root-graph
      |-
      ?~  nodes
        ?>  validate-graph :: TODO: incremental
        =.  graphs  
          (~(jab by graphs) rid |=(r=root-graph:store r(graph graph)))
        plot
      $(nodes t.nodes, graph (~(put ^via graph) i.nodes))
      ::%-  zing
      ::  :~  (give [/keys ~] %keys (~(put in ~(key by graphs)) resource))
          :: (give [/updates ~] %add-graph resource ~ mark overwrite)
          :: ~[append]
      ::==
    ++  remove-index
      |=  [=graph:store =index:store]
      ~|  "index: {<index>}"
      ^-  graph:store
      ~|  "cannot remove index that does not exist"
      =/  parent-hash=(unit hash:store)
        =/  n=(unit node:store)  (~(get-parent ^via graph) index)
        ?~  n  ~
        ?:  ?=(%| -.post.u.n)  `p.post.u.n
        hash.p.post.u.n
      =/  =node:store  (~(got via graph) index)
      =.  post.node
        ~|  "cannot remove post that is already removed"
        ?>  ?=(%& -.post.node)
        =*  p  p.post.node
        ^-  maybe-post:store
        :-  %|
        ?^  hash.p  u.hash.p
        =/  =validated-portion:store
          [parent-hash author.p time-sent.p contents.p]
        `@ux`(sham validated-portion)
      (~(put ^via graph) index node)
    ::
    ++  remove-posts
      |=  indices=(set index:store)
      =/  graph  graph:root-graph
      =.  graph
        %+  roll  ~(tap in indices)
        |=  [=index:store graph=_graph]
        (remove-index graph index)
      (put-graph graph)
    --
  --
--
