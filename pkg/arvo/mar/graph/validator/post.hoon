/-  *post, met=metadata-store, graph=graph-store, hark=hark-graph-hook
|_  i=indexed-post
++  grow
  |%
  ++  noun  i
  ++  graph-permissions-add
    |=  vip=vip-metadata:met
    ^-  permissions:graph
    ?.  ?=([@ ~] index.p.i)
      [%yes %yes %yes]
    ?+  vip  [%yes %yes %yes]
      %admin-feed  [%yes %no %no]
      %host-feed   [%no %no %no]
    ==
  ::
  ++  graph-permissions-remove
    |=  vip=vip-metadata:met
    ^-  permissions:graph
    [%yes %self %self]
  ::  +notification-kind: don't track unreads, notify on replies
  ::
  ++  notification-kind  
    ^-  (unit notif-kind:hark)
    =/  len  (lent index.p.i)
    =/  =mode:hark
      ?:(=(1 len) %count %none)
    `[%post [(dec len) len] mode %children]
  ::
  ++  transform-add-nodes
    |=  [=index =post =atom was-parent-modified=?]
    ^-  [^index ^post]
    =-  [- post(index -)]
    ?~  index  !!
    ?:  ?=([@ ~] index)
      [atom ~]
    ?:  was-parent-modified
      ~|(%cannot-submit-parents-with-prepopulated-children !!)
    =/  ind=^index  index
    (snoc (snip ind) atom)
  --
++  grab
  |%
  :: +noun: validate post
  :: 
  ++  noun
    |:  p=`*`%*(. *indexed-post contents.p [%text '']~)
    =/  ip  ;;(indexed-post p)
    ?>  ?=(^ contents.p.ip)
    ip
  --
::
++  grad  %noun
--
