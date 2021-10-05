/-  *resource
/+  store=graph-store
|_  =bowl:gall
++  cg
  |%
  ++  update
    |=  =update:store
    ^-  cage
    [%graph-update-3 !>(update)]
  --
::
++  scry-for
  |*  [=mold =path]
  .^  mold
    %gx
    (scot %p our.bowl)
    %graph-store
    (scot %da now.bowl)
    (snoc `^path`path %noun)
  ==
::
++  resource-for-update
  |=  =vase
  ^-  (list resource)
  =/  =update:store  !<(update:store vase)
  ?-  -.q.update
      %add-graph          ~[resource.q.update]
      %remove-graph       ~[resource.q.update]
      %add-nodes          ~[resource.q.update]
      %remove-posts       ~[resource.q.update]
      %add-signatures     ~[resource.uid.q.update]
      %remove-signatures  ~[resource.uid.q.update]
      %archive-graph      ~[resource.q.update]
      %unarchive-graph    ~
      %add-tag            ~
      %remove-tag         ~
      %keys               ~
      %tags               ~
      %tag-queries        ~
      %run-updates        ~[resource.q.update]
  ==
::
++  upgrade
  |*  [pst=mold out-pst=mold]
  =>
    |%
    ++  orm
      ((ordered-map atom node) gth)
    +$  node
      [post=pst children=internal-graph]
    +$  graph
      ((mop atom node) gth)
    +$  internal-graph
      $~  [%empty ~]
      $%  [%graph p=graph]
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

  |=  $:  gra=graph
          fn=$-(pst out-pst)
      ==
  ^-  out-graph
  %-  gas:out-orm
  %+  turn  (tap:orm gra)
  |=  [=atom =node]
  :-  (fn post.node)
  ?:  ?=(%empty -.children.node)
    [%empty ~]
  $(gra p.children.node)
::
++  get-graph
  |=  res=resource
  ^-  update:store
  =-  -(p *time)
  %+  scry-for  update:store
  /graph/(scot %p entity.res)/[name.res]
::
++  get-graph-mop
  |=  res=resource
  ^-  graph:store
  =/  =update:store
    (get-graph res)
  ?>  ?=(%add-graph -.q.update)
  graph.q.update
::
++  got-node
  |=  [res=resource =index:store]
  ^-  node:store
  =+  %+  scry-for  ,=update:store
      %+  weld
        /graph/(scot %p entity.res)/[name.res]/node/index/kith
      (turn index (cury scot %ud))
  ?>  ?=(%add-nodes -.q.update)
  ?>  ?=(^ nodes.q.update)
  q.n.nodes.q.update
::
++  check-node-existence
  |=  [res=resource =index:store]
  ^-  ?
  %+  scry-for  ,?
  %+  weld
    /graph/(scot %p entity.res)/[name.res]/node/exists
  (turn index (cury scot %ud))
::
++  get-update-log
  |=  rid=resource 
  ^-  update-log:store
  %+  scry-for  update-log:store
  /update-log/(scot %p entity.rid)/[name.rid]
::
++  peek-update-log
  |=  res=resource
  ^-  (unit time)
  (scry-for (unit time) /update-log/(scot %p entity.res)/[name.res]/latest)
::
++  get-update-log-subset
  |=  [res=resource start=@da]
  ^-  update-log:store
  %+  scry-for  update-log:store
  /update-log/(scot %p entity.res)/[name.res]/subset/'~'/(scot %da start)
::
++  get-keys
  ^-  resources
  =+  %+  scry-for  ,=update:store
      /keys
  ?>  ?=(%keys -.q.update)
  resources.q.update
::
++  tap-deep
  |=  [=index:store =graph:store]
  ^-  (list [index:store node:store])
  %+  roll  (tap:orm:store graph)
  |=  $:  [=atom =node:store]
          lis=(list [index:store node:store])
      ==
  =/  child-index     (snoc index atom)
  =/  childless-node  node(children [%empty ~])
  ?:  ?=(%empty -.children.node)
    (snoc lis [child-index childless-node])
  %+  weld
    (snoc lis [child-index childless-node])
  (tap-deep child-index p.children.node)
::
++  got-deep
  |=  [=graph:store =index:store]
  ^-  node:store
  =/  ind  index
  ?>  ?=(^ index)
  =/  =node:store  (need (get:orm:store graph `atom`i.index))
  =.  ind  t.index
  |-  ^-  node:store
  ?~  ind
    node
  ?:  ?=(%empty -.children.node)
    !!
  %_  $
    ind    t.ind
    node   (need (get:orm:store p.children.node i.ind))
  ==
::
++  get-mark
  |=  res=resource
  (scry-for ,(unit mark) /graph/(scot %p entity.res)/[name.res]/mark)
--
