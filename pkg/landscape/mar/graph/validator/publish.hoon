/-  *post, met=metadata-store, graph=graph-store, hark=hark-graph-hook
/+  graph=graph-store
|_  i=indexed-post
++  grow
  |%
  ++  noun  i
  ::
  ++  graph-indexed-post
    ^-  indexed-post
    ?+    index.p.i  !!
    ::  top level post must have no content
        [@ ~]
      ?>  ?=(~ contents.p.i)
      i
    ::  container for revisions
    ::
        [@ %1 ~]  
      ?>  ?=(~ contents.p.i)
      i
    ::  specific revision
    ::  first content is the title
    ::  revisions are numbered by the revision count
    ::  starting at one
        [@ %1 @ ~]
      ?>  ?=([* * *] contents.p.i)
      ?>  ?=(%text -.i.contents.p.i)
      i
    ::  container for comments
    ::
        [@ %2 ~]
      ?>  ?=(~ contents.p.i)
      i
    ::  container for comment revisions
    ::
        [@ %2 @ ~]
      ?>  ?=(~ contents.p.i)
      i
    ::  specific comment revision
    ::
        [@ %2 @ @ ~]
      ?>  ?=(^ contents.p.i)
      i
    ==
  ::
  ++  graph-permissions-add
    |=  vip=vip-metadata:met
    ^-  permissions:graph
    ?+  index.p.i  !!
      [@ ~]            [%yes %yes %no]  :: new note
      [@ %1 @ ~]       [%self %self %no]
      [@ %2 @ ~]       [%yes %yes ?:(?=(%reader-comments vip) %yes %no)]
      [@ %2 @ @ ~]     [%self %self %self]
    ==
  ::
  ++  graph-permissions-remove
    |=  vip=vip-metadata:met
    ^-  permissions:graph
    ?+  index.p.i  !!
      [@ ~]            [%yes %self %self]
      [@ %1 @ @ ~]     [%yes %self %self]
      [@ %2 @ ~]       [%yes %self %self]
      [@ %2 @ @ ~]     [%yes %self %self]
    ==
  ::  +notification-kind
  ::    ignore all containers, only notify on content
  ::
  ++  notification-kind
    |=  title=cord
    ^-  (unit notif-kind:hark)
    ?+  index.p.i   ~
        [@ %1 %1 ~]
      :-  ~
      :*  [%text (rap 3 'New notes in ' title ~)]~
          ~[(hark-content:graph (snag 0 contents.p.i)) text+' by ' ship+author.p.i]
          [0 1]  %each  %children
      ==
    ::
        [@ %2 @ %1 ~]
      :-  ~
      :*  [%text (rap 3 'New comments in ' title ~)]~
          [ship+author.p.i text+': ' (hark-contents:graph contents.p.i)]
          [1 3]  %count  %siblings
      ==
    ==
  ::
  ++  transform-add-nodes
    |=  [=index =post =atom was-parent-modified=?]
    ^-  [^index ^post]
    =-  [- post(index -)]
    ?+    index  ~|(transform+[index post] !!)
        [@ ~]         [atom ~]
        [@ %1 ~]      [atom %1 ~]
    ::
        [@ %1 @ ~]
      ?:  was-parent-modified
        [atom %1 i.t.t.index ~]
      index
    ::
        [@ %2 ~]      [atom %2 ~]
        [@ %2 @ ~]    [i.index %2 atom ~]
        [@ %2 @ @ ~]
      ?:  was-parent-modified
        [i.index %2 atom i.t.t.t.index ~]
      index
    ==
  --
++  grab
  |%
  ++  noun  indexed-post
  --
::
++  grad  %noun
--
