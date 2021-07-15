/+  store=graph-store, default-agent, poke-proxy-hook, resource
|%
+$  card  card:agent:gall
--
^-  agent:gall
%-  (agent:poke-proxy-hook [%graph-push-hook %graph-update])
^-  poke-proxy-hook:poke-proxy-hook
|_  =bowl:gall
+*  this  .
    def  ~(. (default-agent this %|) bowl)
++  on-init  `this
  
::
++  on-save  !>(~)
::
++  on-load  
  |=  =vase
  ^-  (quip card poke-proxy-hook:poke-proxy-hook)
  `this
::
++  on-poke  
  |=  =cage
  ^-  (quip card poke-proxy-hook:poke-proxy-hook)
  `this
::
++  on-watch  
  |=  =path
  ^-  (quip card poke-proxy-hook:poke-proxy-hook)
  !!

::
++  on-agent  
  |=  [=wire =sign:agent:gall]
  ^-  (quip card poke-proxy-hook:poke-proxy-hook)
  `this
::
++  on-leave  
  |=  =path
  ^-  (quip card poke-proxy-hook:poke-proxy-hook)
  `this
++  on-arvo  
  |=  [=wire =sign-arvo]
  ^-  (quip card poke-proxy-hook:poke-proxy-hook)
  `this

++  on-fail   
  |=  =goof
  ^-  (quip card poke-proxy-hook:poke-proxy-hook)
  `this
++  on-peek  on-peek:def
::
++  on-proxied-poke
  |=  =vase
  ^-  (quip card poke-proxy-hook:poke-proxy-hook)
  `this
::
++  resource-for-update
  |=  =vase
  ^-  (unit resource)
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

--
