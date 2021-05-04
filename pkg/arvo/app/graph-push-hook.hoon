/-  *group, metadata=metadata-store
/+  store=graph-store, mdl=metadata, res=resource, graph, group, default-agent,
    dbug, verb, push-hook
::
~%  %graph-push-hook-top  ..part  ~
|%
+$  card  card:agent:gall
++  config
  ^-  config:push-hook
  :*  %graph-store
      /updates
      update:store
      %graph-update
      %graph-pull-hook
      2  2
  ==
::
+$  agent  (push-hook:push-hook config)
::
+$  state-null  ~
+$  state-zero  [%0 marks=(set mark)]
+$  state-one   [%1 ~]
+$  versioned-state
  $@  state-null
  $%  state-zero
      state-one
  ==
--
::
%-  agent:dbug
%+  verb  |
^-  agent:gall
%-  (agent:push-hook config)
^-  agent
=-
=|  state-one
=*  state  -
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
    grp   ~(. group bowl)
    gra   ~(. graph bowl)
    met   ~(. mdl bowl)
    hc    ~(. hook-core bowl)
::
++  on-init   on-init:def
++  on-save   !>(state)
++  on-load   
  |=  =vase
  =+  !<(old=versioned-state vase)
  =?  old  ?=(~ old)
    [%0 ~]
  =?  old  ?=(%0 -.old)
    [%1 ~]
  ?>  ?=(%1 -.old)
  `this(state old)
::
++  on-poke   on-poke:def
++  on-agent  on-agent:def
++  on-watch  on-watch:def
++  on-leave  on-leave:def
++  on-peek   on-peek:def
++  on-arvo   
  |=  [=wire =sign-arvo]
  ^-  (quip card _this)
  ?+  wire  (on-arvo:def wire sign-arvo)
    ::  XX: no longer necessary
    ::
    [%perms @ @ ~]        [~ this]
    [%transform-add @ ~]  [~ this]
  ==
::
++  on-fail   on-fail:def
++  transform-proxy-update
  |=  vas=vase
  ^-  (unit vase)
  =/  =update:store  !<(update:store vas)
  =*  rid  resource.q.update
  =.  p.update  now.bowl
  ?-  -.q.update
      %add-nodes
    ?.  (is-allowed-add:hc rid nodes.q.update)
      ~
    =/  mark  (get-mark:gra rid)
    ?~  mark  `vas
    |^
    =/  transform
      !<  $-([index:store post:store atom ?] [index:store post:store])
      %.  !>(*indexed-post:store)
      .^(tube:clay (scry:hc %cc %home /[u.mark]/transform-add-nodes))
    =/  [* result=(list [index:store node:store])]
      %+  roll
        (flatten-node-map ~(tap by nodes.q.update))
      (transform-list transform)
    =.  nodes.q.update
      %-  ~(gas by *(map index:store node:store))
      result
    [~ !>(update)]
    ::
    ++  flatten-node-map
      |=  lis=(list [index:store node:store])
      ^-  (list [index:store node:store])
      |^
      %-  sort-nodes
      %+  welp
        (turn lis empty-children)
      %-  zing
      %+  turn  lis
      |=  [=index:store =node:store]
      ^-  (list [index:store node:store])
      ?:  ?=(%empty -.children.node)
        ~
      %+  turn
        (tap-deep:gra index p.children.node)
      empty-children
      ::
      ++  empty-children
        |=  [=index:store =node:store]
        ^-  [index:store node:store]
        [index node(children [%empty ~])]
      ::
      ++  sort-nodes
        |=  unsorted=(list [index:store node:store])
        ^-  (list [index:store node:store])
        %+  sort  unsorted
        |=  [p=[=index:store *] q=[=index:store *]]
        ^-  ?
        (lth (lent index.p) (lent index.q))
      --
    ::
    ++  transform-list
      |=  transform=$-([index:store post:store atom ?] [index:store post:store])
      |=  $:  [=index:store =node:store]
              [indices=(set index:store) lis=(list [index:store node:store])]
          ==
      ~|  "cannot put a deleted post into %add-nodes {<post.node>}"
      ?>  ?=(%& -.post.node)
      =/  l  (lent index)
      =/  parent-modified=?
        %-  ~(rep in indices)
        |=  [i=index:store out=_|]
        ?:  out  out
        =/  k  (lent i)
        ?:  (lte l k)
          %.n
        =((swag [0 k] index) i)
      =/  [ind=index:store =post:store]
        (transform index p.post.node now.bowl parent-modified)
      :-  (~(put in indices) index)
      (snoc lis [ind node(p.post post)])
    --
  ::
      %remove-posts
    ?.  (is-allowed-remove:hc resource.q.update indices.q.update)
      ~
    `vas
  ::
    %add-graph          ~
    %remove-graph       ~
    %add-signatures     ~
    %remove-signatures  ~
    %archive-graph      ~
    %unarchive-graph    ~
    %add-tag            ~
    %remove-tag         ~
    %keys               ~
    %tags               ~
    %tag-queries        ~
    %run-updates        ~
  ==
::
++  resource-for-update  resource-for-update:gra
::
++  initial-watch
  |=  [=path =resource:res]
  ^-  vase
  |^
  ?>  (is-allowed resource)
  !>  ^-  update:store
  ?~  path
    ::  new subscribe
    ::
    (get-graph:gra resource)
  ::  resubscribe
  ::
  ?~  (get-update-log:gra resource)
    (get-graph:gra resource)
  =/  =time  (slav %da i.path)
  =/  =update-log:store  (get-update-log-subset:gra resource time)
  [now.bowl [%run-updates resource update-log]]
  ::
  ++  is-allowed
    |=  =resource:res
    =/  group-res=resource:res
      (need (peek-group:met %graph resource))
    (is-member:grp src.bowl group-res)
  --
::
++  take-update
  |=  =vase
  ^-  [(list card) agent]
  =/  =update:store  !<(update:store vase)
  ?+    -.q.update   [~ this]
      %remove-graph
    :_  this
    [%give %kick ~[resource+(en-path:res resource.q.update)] ~]~
  ::
      %archive-graph
    :_  this
    [%give %kick ~[resource+(en-path:res resource.q.update)] ~]~
  ==
--
::
^|  ^=  hook-core
|_  =bowl:gall
+*  grp  ~(. group bowl)
    met  ~(. mdl bowl)
    gra  ~(. graph bowl)
::
++  scry
  |=  [care=@t desk=@t =path]
  %+  weld
    /[care]/(scot %p our.bowl)/[desk]/(scot %da now.bowl)
  path
::
++  perm-mark
  |=  [=resource:res perm=@t vip=vip-metadata:metadata =indexed-post:store]
  ^-  permissions:store
  |^
  =-  (check vip)
  !<  check=$-(vip-metadata:metadata permissions:store)
  %.  !>(indexed-post)
  =/  mark  (get-mark:gra resource)
  ?~  mark  |=(=vase !>([%no %no %no]))
  .^(tube:clay (scry %cc %home /[u.mark]/(perm-mark-name perm)))
  ::
  ++  perm-mark-name
    |=  perm=@t
    ^-  @t
    (cat 3 'graph-permissions-' perm)
  --
::
++  get-permission
  |=  [=permissions:store is-admin=? writers=(set ship)]
  ^-  permission-level:store
  ?:  is-admin
    admin.permissions
  ?:  =(~ writers)
    writer.permissions
  ?:  (~(has in writers) src.bowl)
    writer.permissions
  reader.permissions
::
++  get-roles-writers-variation
  |=  =resource:res
  ^-  (unit [is-admin=? writers=(set ship) vip=vip-metadata:metadata])
  =/  assoc=(unit association:metadata)
     (peek-association:met %graph resource)
  ?~  assoc  ~
  =/  role=(unit (unit role-tag))
    (role-for-ship:grp group.u.assoc src.bowl)
  =/  writers=(set ship)
    (get-tagged-ships:grp group.u.assoc [%graph resource %writers])
  ?~  role  ~
  =/  is-admin=?
    ?=(?([~ %admin] [~ %moderator]) u.role)
  `[is-admin writers vip.metadatum.u.assoc]
::
++  node-to-indexed-post
  |=  =node:store
  ^-  indexed-post:store
  ?>  ?=(%& -.post.node)
  =*  index  index.p.post.node
  [(snag (dec (lent index)) index) p.post.node]
::
++  is-allowed-add
  |=  [=resource:res nodes=(map index:store node:store)] 
  ^-  ?
  |^
  %-  (bond |.(%.n))
  %+  biff  (get-roles-writers-variation resource)
  |=  [is-admin=? writers=(set ship) vip=vip-metadata:metadata]
  ^-  (unit ?)
  %-  some  
  %+  levy  ~(tap by nodes)
  |=  [=index:store =node:store]
  =/  parent-index=index:store
    (scag (dec (lent index)) index)
  ?:  (~(has by nodes) parent-index)  %.y
  ?:  ?=(%| -.post.node)
    %.n
  ?.  =(author.p.post.node src.bowl)
    %.n
  =/  =permissions:store
    %^  add-mark  resource  vip
    (node-to-indexed-post node)
  =/  =permission-level:store
    (get-permission permissions is-admin writers)
  ?-  permission-level
      %yes  %.y
      %no   %.n
    ::
        %self
      =/  parent-node=node:store
        (got-node:gra resource parent-index)
      ?:  ?=(%| -.post.parent-node)
        %.n
      =(author.p.post.parent-node src.bowl)
  ==
  ::
  ++  add-mark
    |=  [=resource:res vip=vip-metadata:metadata =indexed-post:store]
    (perm-mark resource %add vip indexed-post)
  --
::
++  is-allowed-remove
  |=  [=resource:res indices=(set index:store)]
  ^-  ?
  |^
  %-  (bond |.(%.n))
  %+  biff  (get-roles-writers-variation resource)
  |=  [is-admin=? writers=(set ship) vip=vip-metadata:metadata]
  %-  some  
  %+  levy  ~(tap by indices)
  |=  =index:store
  ^-  ?
  =/  =node:store
    (got-node:gra resource index)
  ?:  ?=(%| -.post.node)  %.n
  =/  =permissions:store
    %^  remove-mark  resource  vip
    (node-to-indexed-post node)
  =/  =permission-level:store
    (get-permission permissions is-admin writers)
  ?-  permission-level
    %yes   %.y
    %no    %.n
    %self  =(author.p.post.node src.bowl)
  ==
  ::
  ++  remove-mark
    |=  [=resource:res vip=vip-metadata:metadata =indexed-post:store]
    (perm-mark resource %remove vip indexed-post)
  --
--

