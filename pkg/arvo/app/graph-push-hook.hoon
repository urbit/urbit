/+  store=graph-store
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
      %add-graph          %.y
      %remove-graph       %.y
      %add-nodes          %.y
      %remove-nodes       %.y
      %add-signatures     %.y
      %remove-signatures  %.y
      %archive-graph      %.y
      %unarchive-graph    %.n
      %add-tag            %.n
      %remove-tag         %.n
      %keys               %.n
      %tags               %.n
      %tag-queries        %.n
      %run-updates        %.y
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
      %run-updates        `resource.q.update
      %keys               ~
      %tags               ~
      %tag-queries        ~
  ==
::
++  initial-watch
  |=  [=path =resource:res]
  ^-  vase
  ?>  (can-join:grp resource src.bowl)
  ?~  path
    ::  new subscribe
    =/  [=graph:store mark=(unit mark:store)]
      (get-graph:gra resource)
    !>  ^-  update:store
    [%0 now.bowl [%add-graph resource graph mark]]
  ::  resubscribe
  ::
  ::  TODO: use action-log
  ::
  !!
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
