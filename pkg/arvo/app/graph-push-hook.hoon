/-  *resource
/+  store=graph-store, graph, group, default-agent, dbug, push-hook
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
  ?.  ?=(%0  -.update)    %.n
  ?-  +<.update
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
  ==
::
++  resource-for-update
  |=  =vase
  ^-  (unit resource)
  =/  =update:store  !<(update:store vase)
  ?.  ?=(%0  -.update)    ~
  ?-  +<.update
      %add-graph          `resource.update
      %remove-graph       `resource.update
      %add-nodes          `resource.update
      %remove-nodes       `resource.update
      %add-signatures     `resource.uid.update
      %remove-signatures  `resource.uid.update
      %archive-graph      `resource.update
      %unarchive-graph    ~
      %add-tag            ~
      %remove-tag         ~
      %keys               ~
      %tags               ~
      %tag-queries        ~
  ==
::
++  initial-watch
  |=  [=path =resource]
  ^-  vase
  ?>  (can-join:grp resource src.bowl)
  ?~  path
    ::  new subscribe
    =/  =graph:store  (get-graph:graph resource)
    !>  ^-  update:store
    [%add-graph resource graph]
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
  ?.  ?=(%0  -.update)  [~ this]
  ?+  +<.update         [~ this]
      %remove-graph
    :_  this
    [%give %kick ~[resource+(en-path:resource resource.update)] ~]~
      %archive-graph
    :_  this
    [%give %kick ~[resource+(en-path:resource resource.update)] ~]~
  ==
--
