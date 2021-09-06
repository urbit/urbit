/-  *post, met=metadata-store, hark=hark-graph-hook
/+  graph=graph-store
|_  i=indexed-post
++  grow
  |%
  ++  noun  i
  ::
  ++  graph-indexed-post
    ^-  indexed-post
    ?>  ?=(^ contents.p.i)
    i
  ::
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
    |=  title=cord
    ^-  (unit notif-kind:hark)
    =/  len  (lent index.p.i)
    =/  =mode:hark
      ?:(=(1 len) %count %none)
    :-  ~
    :*  ~[text+(rap 3 'Your post in ' title ' received replies ' ~)]
        [ship+author.p.i text+': ' (hark-contents:graph contents.p.i)]
        [(dec len) len]  mode  %children
    ==
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
  ++  noun  indexed-post
  --
::
++  grad  %noun
--
