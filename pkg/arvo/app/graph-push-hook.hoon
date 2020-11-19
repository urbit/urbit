/+  store=graph-store
/+  metadata
/+  res=resource
/+  graph
/+  group
/+  default-agent
/+  dbug
/+  push-hook
~%  %graph-push-hook-top  ..is  ~
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
++  is-allowed
  |=  [=resource:res =bowl:gall requires-admin=?]
  ^-  ?
  =/  grp  ~(. group bowl)
  =/  met  ~(. metadata bowl)
  =/  group-paths  (groups-from-resource:met [%graph (en-path:res resource)])
  ?~  group-paths  %.n
  ?:  requires-admin
    (is-admin:grp src.bowl i.group-paths)
  ?|  (is-member:grp src.bowl i.group-paths)
      (is-admin:grp src.bowl i.group-paths)
  ==
::
++  is-allowed-remove
  |=  [=resource:res indices=(set index:store) =bowl:gall]
  ^-  ?
  =/  gra   ~(. graph bowl)
  ?.  (is-allowed resource bowl %.n)
    %.n
  %+  levy
    ~(tap in indices)
  |=  =index:store
  ^-  ?
  =/  =node:store
    (got-node:gra resource index)
  ?|  =(author.post.node src.bowl)
      (is-allowed resource bowl %.y)
  ==
--
::
%-  agent:dbug
^-  agent:gall
%-  (agent:push-hook config)
^-  agent
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
    grp   ~(. group bowl)
    gra   ~(. graph bowl)
::
++  on-init   on-init:def
++  on-save   !>(~)
++  on-load   on-load:def
++  on-poke   on-poke:def
++  on-agent  on-agent:def
++  on-watch  on-watch:def
++  on-leave  on-leave:def
++  on-peek   on-peek:def
++  on-arvo   on-arvo:def
++  on-fail   on-fail:def
::
++  should-proxy-update
  |=  =vase
  ^-  ?
  =/  =update:store  !<(update:store vase)
  ?-  -.q.update
      %add-graph          (is-allowed resource.q.update bowl %.y)
      %remove-graph       (is-allowed resource.q.update bowl %.y)
      %add-nodes          (is-allowed resource.q.update bowl %.n)
      %remove-nodes       (is-allowed-remove resource.q.update indices.q.update bowl)
      %add-signatures     (is-allowed resource.uid.q.update bowl %.n)
      %remove-signatures  (is-allowed resource.uid.q.update bowl %.y)
      %archive-graph      (is-allowed resource.q.update bowl %.y)
      %unarchive-graph    %.n
      %add-tag            %.n
      %remove-tag         %.n
      %keys               %.n
      %tags               %.n
      %tag-queries        %.n
      %run-updates        (is-allowed resource.q.update bowl %.y)
  ==
::
++  resource-for-update
  |=  =vase
  ^-  (unit resource:res)
  =/  =update:store  !<(update:store vase)
  ?-  -.q.update
      %add-graph          `resource.q.update
      %remove-graph       `resource.q.update
      %add-nodes          `resource.q.update
      %remove-nodes       `resource.q.update
      %add-signatures     `resource.uid.q.update
      %remove-signatures  `resource.uid.q.update
      %archive-graph      `resource.q.update
      %unarchive-graph    ~
      %add-tag            ~
      %remove-tag         ~
      %keys               ~
      %tags               ~
      %tag-queries        ~
      %run-updates        `resource.q.update
  ==
::
++  initial-watch
  |=  [=path =resource:res]
  ^-  vase
  ?>  (is-allowed resource bowl %.n)
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
  ?+  -.q.update       [~ this]
      %remove-graph
    :_  this
    [%give %kick ~[resource+(en-path:res resource.q.update)] ~]~
  ::
      %archive-graph
    :_  this
    [%give %kick ~[resource+(en-path:res resource.q.update)] ~]~
  ==
--
