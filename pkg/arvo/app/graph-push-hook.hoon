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
  ==
::
+$  agent  (push-hook:push-hook config)
::
+$  state-null  ~
+$  state-zero  [%0 marks=(set mark)]
+$  versioned-state
  $@  state-null
  state-zero
--
::
=|  state-zero
=*  state  -
%-  agent:dbug
%+  verb  |
^-  agent:gall
%-  (agent:push-hook config)
^-  agent
=<
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
    grp   ~(. group bowl)
    gra   ~(. graph bowl)
    hc    ~(. +> bowl)
::
++  on-init   on-init:def
++  on-save   !>(state)
++  on-load   
  |=  =vase
  =+  !<(old=versioned-state vase)
  =?  old  ?=(~ old)
    [%0 ~]
  ?>  ?=(%0 -.old)
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
    ::
      [%perms @ @ ~]
    ?>  ?=(?(%add %remove) i.t.t.wire)
    =*  mark  i.t.wire
    :_  this
    (build-permissions mark i.t.t.wire %next)^~
  ::
      [%transform-add @ ~]
    =*  mark  i.t.wire
    :_  this
    (build-transform-add mark %next)^~
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
        (transform index post.node now.bowl parent-modified)
      :-  (~(put in indices) index)
      (snoc lis [ind node(post post)])
    --
  ::
      %remove-nodes
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
  ?>  (is-allowed:hc resource)
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
  [%0 now.bowl [%run-updates resource update-log]]
::
++  take-update
  |=  =vase
  ^-  [(list card) agent]
  =/  =update:store  !<(update:store vase)
  ?+    -.q.update   [~ this]
      %add-graph
    ?~  mark.q.update  `this
    =*  mark  u.mark.q.update
    ?:  (~(has in marks) mark)  `this
    :_  this(marks (~(put in marks) mark))
    :~  (build-permissions:hc mark %add %sing)
        (build-permissions:hc mark %remove %sing)
        (build-transform-add:hc mark %sing)
    ==
  ::
      %remove-graph
    :_  this
    [%give %kick ~[resource+(en-path:res resource.q.update)] ~]~
  ::
      %archive-graph
    :_  this
    [%give %kick ~[resource+(en-path:res resource.q.update)] ~]~
  ==
--
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
++  perm-mark-name
  |=  perm=@t
  ^-  @t
  (cat 3 'graph-permissions-' perm)
::
++  perm-mark
  |=  [=resource:res perm=@t vip=vip-metadata:metadata =indexed-post:store]
  ^-  permissions:store
  =-  (check vip)
  !<  check=$-(vip-metadata:metadata permissions:store)
  %.  !>(indexed-post)
  =/  mark  (get-mark:gra resource)
  ?~  mark  |=(=vase !>([%no %no %no]))
  .^(tube:clay (scry %cc %home /[u.mark]/(perm-mark-name perm)))
::
++  add-mark
  |=  [=resource:res vip=vip-metadata:metadata =indexed-post:store]
  (perm-mark resource %add vip indexed-post)
::
++  remove-mark
  |=  [=resource:res vip=vip-metadata:metadata =indexed-post:store]
  (perm-mark resource %remove vip indexed-post)
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
++  is-allowed
  |=  =resource:res
  =/  group-res=resource:res
    (need (peek-group:met %graph resource))
  (is-member:grp src.bowl group-res)
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
  =*  index  index.post.node
  [(snag (dec (lent index)) index) post.node]
::
++  is-allowed-add
  |=  [=resource:res nodes=(map index:store node:store)] 
  ^-  ?
  %-  (bond |.(%.n))
  %+  biff  (get-roles-writers-variation resource)
  |=  [is-admin=? writers=(set ship) vip=vip-metadata:metadata]
  %-  some  
  %+  levy  ~(tap by nodes)
  |=  [=index:store =node:store]
  ?.  =(author.post.node src.bowl)
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
      =/  parent-index=index:store
        (scag (dec (lent index)) index)
      =/  parent-node=node:store
        (got-node:gra resource parent-index)
      =(author.post.parent-node src.bowl)
  ==
::
++  is-allowed-remove
  |=  [=resource:res indices=(set index:store)]
  ^-  ?
  %-  (bond |.(%.n))
  %+  biff  (get-roles-writers-variation resource)
  |=  [is-admin=? writers=(set ship) vip=vip-metadata:metadata]
  %-  some  
  %+  levy  ~(tap by indices)
  |=  =index:store
  =/  =node:store
    (got-node:gra resource index)
  =/  =permissions:store
    %^  remove-mark  resource  vip
    (node-to-indexed-post node)
  =/  =permission-level:store
    (get-permission permissions is-admin writers)
  ?-  permission-level
    %yes   %.y
    %no    %.n
    %self  =(author.post.node src.bowl)
  ==
::
++  build-permissions
  |=  [=mark kind=?(%add %remove) mode=?(%sing %next)]
  ^-  card
  =/  =wire  /perms/[mark]/[kind]
  =/  =mood:clay  [%c da+now.bowl /[mark]/(perm-mark-name kind)]
  =/  =rave:clay  ?:(?=(%sing mode) [mode mood] [mode mood])
  [%pass wire %arvo %c %warp our.bowl %home `rave]
::
++  build-transform-add
  |=  [=mark mode=?(%sing %next)]
  ^-  card
  =/  =wire  /transform-add/[mark]
  =/  =mood:clay  [%c da+now.bowl /[mark]/transform-add-nodes]
  =/  =rave:clay  ?:(?=(%sing mode) [mode mood] [mode mood])
  [%pass wire %arvo %c %warp our.bowl %home `rave]
--

