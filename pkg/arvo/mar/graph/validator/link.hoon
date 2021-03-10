/-  *post, met=metadata-store
|_  i=indexed-post
++  grow
  |%
  ++  noun  i
  ::
  ++  graph-permissions-add
    |=  vip=vip-metadata:met
    =/  reader
      ?=(%reader-comments vip)
    ?+  index.p.i  !!
      [@ ~]       [%yes %yes %no]
      [@ @ ~]     [%yes %yes ?:(reader %yes %no)]
      [@ @ @ ~]   [%self %self %self]
    ==
  ::
  ++  graph-permissions-remove
    |=  vip=vip-metadata:met
    =/  reader
      ?=(%reader-comments vip)
    ?+  index.p.i  !!
      [@ ~]       [%yes %self %self]
      [@ @ ~]     [%yes %self %self]
      [@ @ @ ~]   [%yes %self %self]
    ==
  ::
  ++  notification-kind
    ?+  index.p.i  ~
      [@ ~]       `[%link [0 1] %each %children]
      [@ @ %1 ~]  `[%comment [1 2] %count %siblings]
      [@ @ @ ~]   `[%edit-comment [1 2] %none %none]
    ==
  ::
  ++  transform-add-nodes
    |=  [=index =post =atom was-parent-modified=?]
    ^-  [^index ^post]
    =-  [- post(index -)]
    ?+    index  ~|(transform+[index post] !!)
        [@ ~]    [atom ~]
        [@ @ ~]  [i.index atom ~]
        [@ @ @ ~]
      ?:  was-parent-modified
        [i.index atom i.t.t.index ~]
      index
    ==
  --
++  grab
  |%
  ++  noun
    |=  p=*
    =/  ip  ;;(indexed-post p)
    ?+    index.p.ip  ~|(index+index.p.ip !!)
        ::  top-level link post; title and url
        ::
        [@ ~]
      ?>  ?=([[%text @] [%url @] ~] contents.p.ip)
      ip
    ::
        ::  comment on link post; container structure
        ::
        [@ @ ~]
      ?>  ?=(~ contents.p.ip)
      ip
    ::
        ::  comment on link post; comment text
        ::
        [@ @ @ ~]
      ?>  ?=(^ contents.p.ip)
      ip
    ==
  --
++  grad  %noun
--
