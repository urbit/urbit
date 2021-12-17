/-  sur=graph-store, pos=post, pull-hook, hark=hark-store
/+  res=resource, migrate
=<  [sur .]
=<  [pos .]
=,  sur
=,  pos
|%
++  hark-content
  |=  =content
  ^-  content:hark
  ?-  -.content
    %text       content
    %mention    ship+ship.content
    %url        text+url.content
    %code       text+'A code excerpt'
    %reference  text+'A reference'       
  ==
::
++  hark-contents
  |=  cs=(list content) 
  (turn cs hark-content)
::  NOTE: move these functions to zuse
++  nu                                              ::  parse number as hex
  |=  jon=json
  ?>  ?=([%s *] jon)
  (rash p.jon hex)
::
++  re                                                ::  recursive reparsers
  |*  [gar=* sef=_|.(fist:dejs-soft:format)]
  |=  jon=json
  ^-  (unit _gar)
  =-  ~!  gar  ~!  (need -)  -
  ((sef) jon)
::
++  dank                                              ::  tank
  ^-  $-(json (unit tank))
  =,  ^?  dejs-soft:format
  %+  re  *tank  |.  ~+
  %-  of  :~
    leaf+sa
    palm+(ot style+(ot mid+sa cap+sa open+sa close+sa ~) lines+(ar dank) ~)
    rose+(ot style+(ot mid+sa open+sa close+sa ~) lines+(ar dank) ~)
  ==
::
++  orm      ((on atom node) gth)
::
++  upgrade
  |%
  ::
  ::  +two
  ::
  ++  marked-graph-to-two
    |=  [=graph:one m=(unit mark)]
    [(graph-to-two graph) m]
  ::
  ++  graph-to-two
    |=  =graph:one
    (graph:(upgrade ,post:one ,maybe-post:two) graph post-to-two)
  ::
  ++  post-to-two
    |=  p=post:one
    ^-  maybe-post:two
    [%& p]
  ::
  ::
  ::  +one
  ::
  ++  update-log-to-one
    |=  =update-log:zero
    ^-  update-log:one
    %+  gas:orm-log:one  *update-log:one
    %+  turn  (tap:orm-log:zero update-log)
    |=  [=time =logged-update:zero]
    ^-  [^time logged-update:one]
    :-  time
    :-  p.logged-update  
    (logged-update-to-one q.logged-update)
  ::
  ++  logged-update-to-one
    |=  upd=logged-update-0:zero
    ^-  logged-action:one
    ?+  -.upd  upd
      %add-graph  upd(graph (graph-to-one graph.upd))
      %add-nodes  upd(nodes (~(run by nodes.upd) node-to-one))
    ==
  ::
  ++  node-to-one
    |=  =node:zero
    (node:(upgrade ,post:zero ,post:one) node post-to-one)
  ::
  ++  graph-to-one
    |=  =graph:zero
    (graph:(upgrade ,post:zero ,post:one) graph post-to-one)
  ::
  ++  marked-graph-to-one
    |=  [=graph:zero m=(unit mark)]
    [(graph-to-one graph) m]
  ::
  ++  post-to-one
    |=  p=post:zero
    ^-  post:one
    p(contents (contents-to-one contents.p))
  ::
  ++  contents-to-one
    |=  cs=(list content:zero)
    ^-  (list content:one)
    %+  murn  cs
    |=  =content:zero
    ^-  (unit content:one)
    ?:  ?=(%reference -.content)  ~
    `content
  ::
  ++  upgrade
    |*  [in-pst=mold out-pst=mold]
    =>
      |%
      ++  in-orm
        ((on atom in-node) gth)
      +$  in-node
        [post=in-pst children=in-internal-graph]
      +$  in-graph
        ((mop atom in-node) gth)
      +$  in-internal-graph
        $~  [%empty ~]
        $%  [%graph p=in-graph]
            [%empty ~]
        ==
      ::
      ++  out-orm
        ((on atom out-node) gth)
      +$  out-node
        [post=out-pst children=out-internal-graph]
      +$  out-graph
        ((mop atom out-node) gth)
      +$  out-internal-graph
        $~  [%empty ~]
        $%  [%graph p=out-graph]
            [%empty ~]
        ==
      --
    |%
    ::
    ++  graph
      |=  $:  gra=in-graph
              fn=$-(in-pst out-pst)
          ==
      ^-  out-graph
      %+  gas:out-orm  *out-graph
      ^-  (list [atom out-node])
      %+  turn  (tap:in-orm gra)
      |=  [a=atom n=in-node]
      ^-  [atom out-node]
      [a (node n fn)]
    ::
    ++  node
      |=  [nod=in-node fn=$-(in-pst out-pst)]
      ^-  out-node
      :-  (fn post.nod)
      ^-  out-internal-graph
      ?:  ?=(%empty -.children.nod)
        [%empty ~]
      [%graph (graph p.children.nod fn)]
    --
  ::
  ++  zero-load
    :: =* infinitely recurses
    =,  store=zero
    =,  orm=orm:zero
    =,  orm-log=orm-log:zero
    |%
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
        :_  ~  :-  %0
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
  --
--
