/-  *post, met=metadata-store, graph=graph-store, hark=hark-graph-hook
|_  i=indexed-post
++  grow
  |%
  ++  noun  i
  ::
  ++  graph-permissions-add
    |=  vip=vip-metadata:met
    ^-  permissions:graph
    ?+  index.p.i  !!
      [@ ~]  [%yes %yes %no]
    ==
  ::
  ++  graph-permissions-remove
    |=  vip=vip-metadata:met
    ^-  permissions:graph
    ?+  index.p.i  !!
      [@ ~]  [%self %self %no]
    ==
  ::
  ++  notification-kind
    ^-  (unit notif-kind:hark)
    ?+  index.p.i  ~
      [@ ~]  `[%message [0 1] %count %none]
    ==
  ::
  ++  transform-add-nodes
    |=  [=index =post =atom was-parent-modified=?]
    ^-  [^index ^post]
    =-  [- post(index -)]
    [atom ~]
  --
++  grab
  |%
  ++  noun
    |:  p=`*`%*(. *indexed-post index.p [0 ~])
    =/  ip  ;;(indexed-post p)
    ?>  ?=([@ ~] index.p.ip)
    ip
  --
::
++  grad  %noun
--
