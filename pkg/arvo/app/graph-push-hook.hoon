/+  store=graph-store
/+  met=metadata
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
++  is-member
  |=  [=resource:res =bowl:gall]
  ^-  ?
  =/  grp  ~(. group bowl)
  =/  group-paths  (groups-from-resource:met [%graph (en-path:res resource)])
  ?~  group-paths  %.n
  (is-member:grp src.bowl i.group-paths)
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
      %add-graph          (is-member resource.q.update bowl)
      %remove-graph       (is-member resource.q.update bowl)
      %add-nodes          (is-member resource.q.update bowl)
      %remove-nodes       (is-member resource.q.update bowl)
      %add-signatures     (is-member resource.uid.q.update bowl)
      %remove-signatures  (is-member resource.uid.q.update bowl)
      %archive-graph      (is-member resource.q.update bowl)
      %unarchive-graph    %.n
      %add-tag            %.n
      %remove-tag         %.n
      %keys               %.n
      %tags               %.n
      %tag-queries        %.n
      %run-updates        (is-member resource.q.update bowl)
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
  ?>  (is-member resource bowl)
  !>  ^-  update:store
  ?~  path
    ::  new subscribe
    ::
    (get-graph:gra resource)
  ::  resubscribe
  ::
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
