/-  *post, met=metadata-store, hark=hark-graph-hook
/+  graph=graph-store
|_  i=indexed-post
++  grow
  |%
  ++  noun  i
  ::
  ++  graph-indexed-post
    ^-  indexed-post
    ?>  ?=([@ ~] index.p.i)
    i
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
    |=  title=cord
    ^-  (unit notif-kind:hark)
    ?+  index.p.i  ~
        [@ ~]  
      :-  ~
      :*  ~[text+(rap 3 'New messages in ' title ~)]
          [ship+author.p.i text+': ' (hark-contents:graph contents.p.i)]
          [0 1]  %count  %none
      ==
    ==
  ::
  ++  transform-add-nodes
    |=  [=index =post =atom was-parent-modified=?]
    ^-  [^index ^post]
    =-  [- post(index -)]
    [atom ~]
  --
::
++  grab  
  |%
  ++  noun  indexed-post
  --
::
++  grad  %noun
--
