/-  *resource
/+  store=graph-store
|_  =bowl:gall
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
      %remove-nodes       ~[resource.q.update]
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
++  get-graph
  |=  res=resource
  ^-  update:store
  %+  scry-for  update:store
  /graph/(scot %p entity.res)/[name.res]
::
++  get-graph-mop
  |=  res=resource
  ^-  graph:store
  =/  =update:store
    (get-graph res)
  ?>  ?=(%0 -.update)
  ?>  ?=(%add-graph -.q.update)
  graph.q.update
::
++  gut-younger-node-siblings
  |=  [res=resource =index:store]
  ^-  (map index:store node:store)
  =+  %+  scry-for  ,=update:store
      %+  weld
        /node-siblings/younger/(scot %p entity.res)/[name.res]/all
      (turn index (cury scot %ud))
  ?>  ?=(%0 -.update)
  ?>  ?=(%add-nodes -.q.update)
  nodes.q.update
::
++  got-node
  |=  [res=resource =index:store]
  ^-  node:store
  =+  %+  scry-for  ,=update:store
      %+  weld
        /node/(scot %p entity.res)/[name.res]
      (turn index (cury scot %ud))
  ?>  ?=(%0 -.update)
  ?>  ?=(%add-nodes -.q.update)
  ?>  ?=(^ nodes.q.update)
  q.n.nodes.q.update
::
++  check-node-existence
  |=  [res=resource =index:store]
  ^-  ?
  %+  scry-for  ,?
  %+  weld
    /node-exists/(scot %p entity.res)/[name.res]
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
  (scry-for (unit time) /peek-update-log/(scot %p entity.res)/[name.res])
::
++  get-update-log-subset
  |=  [res=resource start=@da]
  ^-  update-log:store
  %+  scry-for  update-log:store
  /update-log-subset/(scot %p entity.res)/[name.res]/(scot %da start)/'~'
::
++  get-keys
  ^-  resources
  =+  %+  scry-for  ,=update:store
      /keys
  ?>  ?=(%0 -.update)
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
  (scry-for ,(unit mark) /graph-mark/(scot %p entity.res)/[name.res])
--
