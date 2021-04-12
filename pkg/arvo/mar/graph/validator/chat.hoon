/-  *post, met=metadata-store
|_  i=indexed-post
++  grow
  |%
  ++  noun  i
  ::
  ++  graph-permissions-add
    |=  vip=vip-metadata:met
    ?+  index.p.i  !!
      [@ ~]  [%yes %yes %no]
    ==
  ::
  ++  graph-permissions-remove
    |=  vip=vip-metadata:met
    ?+  index.p.i  !!
      [@ ~]  [%self %self %no]
    ==
  ::
  ++  notification-kind
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
    |=  p=*
    =/  ip  ;;(indexed-post p)
    ?>  ?=([@ ~] index.p.ip)
    ip
  --
::
++  grad  %noun
--
