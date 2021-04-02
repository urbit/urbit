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
    ?:  =(1 len)  ~
    `[%post [(dec len) len] %none %children]
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
    |=  p=*
    =/  ip  ;;(indexed-post p)
    ?>  ?=(^ contents.p.ip)
    ip
  --
::
++  grad  %noun
--
