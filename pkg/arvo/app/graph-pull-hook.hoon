/-  *resource
/+  store=graph-store, graph, default-agent, verb, dbug, pull-hook
~%  %graph-pull-hook-top  ..part  ~
|%
+$  card  card:agent:gall
++  config
  ^-  config:pull-hook
  :*  %graph-store
      update:store
      %graph-update
      %graph-push-hook
      0  0
      %.n
  ==
--
::
%-  agent:dbug
%+  verb  |
^-  agent:gall
%-  (agent:pull-hook config)
^-  (pull-hook:pull-hook config)
|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
    dep   ~(. (default:pull-hook this config) bowl)
    gra   ~(. graph bowl)
::
++  on-init   on-init:def
++  on-save   !>(~)
++  on-load   on-load:def
++  on-poke   on-poke:def
++  on-peek   on-peek:def
++  on-arvo   on-arvo:def
++  on-fail   on-fail:def
++  on-agent  on-agent:def
++  on-watch  on-watch:def
++  on-leave  on-leave:def
++  on-pull-nack
  |=  [=resource =tang]
  ^-  (quip card _this)
  %-  (slog leaf+"nacked {<resource>}" tang)
  :_  this
  ?.  (~(has in get-keys:gra) resource)  ~
  =-  [%pass /pull-nack %agent [our.bowl %graph-store] %poke %graph-update-0 -]~
  !>  ^-  update:store
  [%0 now.bowl [%archive-graph resource]]
::
++  on-pull-kick
  |=  =resource
  ^-  (unit path)
  =/  maybe-time  (peek-update-log:gra resource)
  ?~  maybe-time  `/
  `/(scot %da u.maybe-time)
::
++  resource-for-update  resource-for-update:gra
--
