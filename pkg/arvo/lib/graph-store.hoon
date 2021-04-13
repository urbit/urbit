/-  sur=graph-store, pos=post
/+  res=resource
=<  [sur .]
=<  [pos .]
=,  sur
=,  pos
|%
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
++  orm      ((ordered-map atom node) gth)
++  orm-log  ((ordered-map time logged-update) gth)
::
++  enjs
  =,  enjs:format
  |%
  ::
  ++  signatures
    |=  s=^signatures
    ^-  json
    [%a (turn ~(tap in s) signature)]
  ::
  ++  signature
    |=  s=^signature
    ^-  json
    %-  pairs
    :~  [%signature s+(scot %ux p.s)]
        [%ship (ship q.s)]
        [%life (numb r.s)]
    ==
  ::
  ++  index
    |=  i=^index
    ^-  json
    ?:  =(~ i)  s+'/'
    =/  j=^tape  ""
    |-
    ?~  i  [%s (crip j)]
    =/  k=json  (numb i.i)
    ?>  ?=(%n -.k)
    %_  $
        i  t.i
        j  (weld j (weld "/" (trip +.k)))
    ==
  ::
  ++  uid
    |=  u=^uid
    ^-  json
    %-  pairs
    :~  [%resource (enjs:res resource.u)]
        [%index (index index.u)]
    ==
  ::
  ++  content
    |=  c=^content
    ^-  json
    ?-  -.c
        %mention    (frond %mention (ship ship.c))
        %text       (frond %text s+text.c)
        %url        (frond %url s+url.c)
        %reference  (frond %reference (reference +.c))
        %code
      %+  frond  %code
      %-  pairs
      :-  [%expression s+expression.c]
      :_  ~
      :-  %output
      ::  virtualize output rendering, +tank:enjs:format might crash
      ::
      =/  result=(each (list json) tang)
        (mule |.((turn output.c tank)))
      ?-  -.result
        %&  a+p.result
        %|  a+[a+[%s '[[output rendering error]]']~]~
      ==
    ==
  ::
  ++  reference
    |=  ref=^reference
    |^
    %+  frond  -.ref
    ?-  -.ref
      %graph  (graph +.ref)
      %group  (group +.ref)
    ==
    ::
    ++  graph
      |=  [grp=res gra=res idx=^index]
      %-  pairs
      :~  graph+s+(enjs-path:res gra)
          group+s+(enjs-path:res grp)
          index+(index idx)
      ==
    ::
    ++  group
      |=  grp=res
      s+(enjs-path:res grp)
    --
  ::
  ++  post
    |=  p=^post
    ^-  json
    %-  pairs
    :~  [%author (ship author.p)]
        [%index (index index.p)]
        [%time-sent (time time-sent.p)]
        [%contents [%a (turn contents.p content)]]
        [%hash ?~(hash.p ~ s+(scot %ux u.hash.p))]
        [%signatures (signatures signatures.p)]
    ==
  ::
  ++  update
    |=  upd=^update
    ^-  json
    |^  (frond %graph-update (pairs ~[(encode q.upd)]))
    ::
    ++  encode
      |=  upd=action
      ^-  [cord json]
      ?-  -.upd
          %add-graph
        :-  %add-graph
        %-  pairs
        :~  [%resource (enjs:res resource.upd)]
            [%graph (graph graph.upd)]
            [%mark ?~(mark.upd ~ s+u.mark.upd)]
            [%overwrite b+overwrite.upd]
        ==
      ::
          %remove-graph
        [%remove-graph (enjs:res resource.upd)]
      ::
          %add-nodes
        :-  %add-nodes
        %-  pairs
        :~  [%resource (enjs:res resource.upd)]
            [%nodes (nodes nodes.upd)]
        ==
      ::
          %remove-nodes
        :-  %remove-nodes
        %-  pairs
        :~  [%resource (enjs:res resource.upd)]
            [%indices (indices indices.upd)]
        ==
      ::
          %add-signatures
        :-  %add-signatures
        %-  pairs
        :~  [%uid (uid uid.upd)]
            [%signatures (signatures signatures.upd)]
        ==
      ::
          %remove-signatures
        :-  %remove-signatures
        %-  pairs
        :~  [%uid (uid uid.upd)]
            [%signatures (signatures signatures.upd)]
        ==
      ::
          %add-tag
        :-  %add-tag
        %-  pairs
        :~  [%term s+term.upd]
            [%resource (enjs:res resource.upd)]
        ==
      ::
          %remove-tag
        :-  %remove-tag
        %-  pairs
        :~  [%term s+term.upd]
            [%resource (enjs:res resource.upd)]
        ==
      ::
          %archive-graph
        [%archive-graph (enjs:res resource.upd)]
      ::
          %unarchive-graph
        [%unarchive-graph (enjs:res resource.upd)]
      ::
          %keys
        [%keys [%a (turn ~(tap in resources.upd) enjs:res)]]
      ::
          %tags
        [%tags [%a (turn ~(tap in tags.upd) |=(=term s+term))]]
      ::
          %run-updates
        [%run-updates ~]
      ::
          %tag-queries
        :-  %tag-queries
        %-  pairs
        %+  turn  ~(tap by tag-queries.upd)
        |=  [=term =resources]
        ^-  [cord json]
        [term [%a (turn ~(tap in resources) enjs:res)]]
      ==
    ::
    ++  graph
      |=  g=^graph
      ^-  json
      %-  pairs
      %+  turn
        (tap:orm g)
      |=  [a=atom n=^node]
      ^-  [@t json]
      :_  (node n)
      =/  idx  (numb a)
      ?>  ?=(%n -.idx)
      p.idx
    ::
    ++  node
      |=  n=^node
      ^-  json
      %-  pairs
      :~  [%post (post post.n)]
          :-  %children
          ?-  -.children.n
              %empty  ~
              %graph  (graph +.children.n)
          ==
      ==
    ::
            ::
    ++  nodes
      |=  m=(map ^index ^node)
      ^-  json
      %-  pairs
      %+  turn  ~(tap by m)
      |=  [n=^index o=^node]
      ^-  [@t json]
      :_  (node o)
      =/  idx  (index n)
      ?>  ?=(%s -.idx)
      p.idx
    ::
    ++  indices
      |=  i=(set ^index)
      ^-  json
      [%a (turn ~(tap in i) index)]
    ::
    --
  --
::
++  dejs
  =,  dejs:format
  |%
  ++  update
    |=  jon=json
    ^-  ^update
    :-  *time
    ^-  action
    =<  (decode jon)
    |%
    ++  decode
      %-  of
      :~  [%add-nodes add-nodes]
          [%remove-nodes remove-nodes]
          [%add-signatures add-signatures]
          [%remove-signatures remove-signatures]
        ::
          [%add-graph add-graph]
          [%remove-graph remove-graph]
        ::
          [%add-tag add-tag]
          [%remove-tag remove-tag]
        ::
          [%archive-graph archive-graph]
          [%unarchive-graph unarchive-graph]
          [%run-updates run-updates]
        ::
          [%keys keys]
          [%tags tags]
          [%tag-queries tag-queries]
      ==
    ::
    ++  add-graph
      %-  ot
      :~  [%resource dejs:res]
          [%graph graph]
          [%mark (mu so)]
          [%overwrite bo]
      ==
    ::
    ++  graph
      |=  a=json
      ^-  ^graph
      =/  or-mp  ((ordered-map atom ^node) gth)
      %+  gas:or-mp  ~
      %+  turn  ~(tap by ((om node) a))
      |*  [b=cord c=*]
      ^-  [atom ^node]
      =>  .(+< [b c]=+<)
      [(rash b dem) c]
    ::
    ++  remove-graph  (ot [%resource dejs:res]~)
    ++  archive-graph  (ot [%resource dejs:res]~)
    ++  unarchive-graph  (ot [%resource dejs:res]~)
    ::
    ++  add-nodes
      %-  ot
      :~  [%resource dejs:res]
          [%nodes nodes]
      ==
    ::
    ++  nodes  (op ;~(pfix fas (more fas dem)) node)
    ::
    ++  node
      %-  ot
      :~  [%post post]
          [%children internal-graph]
      ==
    ::
    ++  internal-graph
      |=  jon=json
      ^-  ^internal-graph
      ?~  jon
        [%empty ~]
      [%graph (graph jon)]
    ::
    ++  post
      %-  ot
      :~  [%author (su ;~(pfix sig fed:ag))]
          [%index index]
          [%time-sent di]
          [%contents (ar content)]
          [%hash (mu nu)]
          [%signatures (as signature)]
      ==
    ::
    ++  content
      %-  of
      :~  [%mention (su ;~(pfix sig fed:ag))]
          [%text so]
          [%url so]
          [%reference reference]
          [%code eval]
      ==
    ::
    ++  reference
      |^
      %-  of
      :~  graph+graph
          group+dejs-path:res
      ==
      ::
      ++  graph
        %-  ot
        :~  group+dejs-path:res
            graph+dejs-path:res
            index+index
        ==
      --
    ::
    ++  tang 
      |=  jon=^json
      ^-  ^tang
      ?>  ?=(%a -.jon)
      %-  zing
      %+  turn
        p.jon
      |=  jo=^json
      ^-  (list tank)
      ?>  ?=(%a -.jo)
      %+  turn
        p.jo
      |=  j=^json
      ?>  ?=(%s -.j)
      ^-  tank
      leaf+(trip p.j)
    ::
    ++  eval
      %-  ot
      :~  expression+so
          output+tang
      ==

    ::
    ++  remove-nodes
      %-  ot
      :~  [%resource dejs:res]
          [%indices (as index)]
      ==
    ::
    ++  add-signatures
      %-  ot
      :~  [%uid uid]
          [%signatures (as signature)]
      ==
    ::
    ++  remove-signatures
      %-  ot
      :~  [%uid uid]
          [%signatures (as signature)]
      ==
    ::
    ++  signature
      %-  ot
      :~  [%hash nu]
          [%ship (su ;~(pfix sig fed:ag))]
          [%life ni]
      ==
    ::
    ++  uid
      %-  ot
      :~  [%resource dejs:res]
          [%index index]
      ==
    ::
    ++  index  (su ;~(pfix fas (more fas dem)))
    ::
    ++  add-tag
      %-  ot
      :~  [%term so]
          [%resource dejs:res]
      ==
    ::
    ++  remove-tag
      %-  ot
      :~  [%term so]
          [%resource dejs:res]
      ==
    ::
    ++  keys
      |=  =json
      *resources
    ::
    ++  tags
      |=  =json
      *(set term)
    ::
    ++  tag-queries
      |=  =json
      *^tag-queries
    ::
    ++  run-updates
      |=  a=json
      ^-  [resource update-log]
      [*resource *update-log]
    --
  --
::
++  create
  |_  [our=ship now=time]
  ++  post
    |=  [=index contents=(list content)]
    ^-  ^post
    :*  our
        index
        now
        contents
        ~
        *signatures
    ==
  --
::
++  upgrade
  |%
  ++  update-log-to-one
    |=  =update-log:zero
    ^-  ^update-log
    %+  gas:orm-log  *^update-log
    %+  turn  (tap:orm-log:zero update-log)
    |=  [=time =logged-update:zero]
    :-  time
    :-  p.logged-update  
    (logged-update-to-one q.logged-update)
  ::
  ++  logged-update-to-one
    |=  upd=logged-update-0:zero
    ?+  -.upd  upd
      %add-graph  upd(graph (graph-to-one graph.upd))
      %add-nodes  upd(nodes (~(run by nodes.upd) node-to-one))
    ==
  ::
  ++  node-to-one
    |=  =node:zero
    (node:(upgrade ,post:zero ,post) node post-to-one)
  ::
  ++  graph-to-one
    |=  =graph:zero
    (graph:(upgrade ,post:zero ,post) graph post-to-one)
  ::
  ++  marked-graph-to-one
    |=  [=graph:zero m=(unit mark)]
    [(graph-to-one graph) m]
  ::
  ++  post-to-one
    |=  p=post:zero
    ^-  post
    p(contents (contents-to-one contents.p))
  ::
  ++  contents-to-one
    |=  cs=(list content:zero)
    ^-  (list content)
    %+  murn  cs
    |=  =content:zero
    ^-  (unit ^content)
    ?:  ?=(%reference -.content)  ~
    `content
  ::
  ++  upgrade
    |*  [in-pst=mold out-pst=mold]
    =>
      |%
      ++  in-orm
        ((ordered-map atom in-node) gth)
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
        ((ordered-map atom out-node) gth)
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
