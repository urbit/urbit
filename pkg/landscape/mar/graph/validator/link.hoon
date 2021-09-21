/-  *post, met=metadata-store, hark=hark-graph-hook
/+  graph=graph-store
|_  i=indexed-post
++  grow
  |%
  ++  noun  i
  ::
  ++  graph-indexed-post
    ^-  indexed-post
    ?+    index.p.i  ~|(index+index.p.i !!)
        ::  top-level link post; title and url
        ::
        [@ ~]
      ?>  ?=([[%text @] $%([%url @] [%reference *]) ~] contents.p.i)
      i
    ::
        ::  comment on link post; container structure
        ::
        [@ @ ~]
      ?>  ?=(~ contents.p.i)
      i
    ::
        ::  comment on link post; comment text
        ::
        [@ @ @ ~]
      ?>  ?=(^ contents.p.i)
      i
    ==
  ::
  ++  graph-permissions-add
    |=  vip=vip-metadata:met
    ^-  permissions:graph
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
    ^-  permissions:graph
    =/  reader
      ?=(%reader-comments vip)
    ?+  index.p.i  !!
      [@ ~]       [%yes %self %self]
      [@ @ ~]     [%yes %self %self]
      [@ @ @ ~]   [%yes %self %self]
    ==
  ::
  ++  notification-kind
    |=  title=cord
    ^-  (unit notif-kind:hark)
    ?+  index.p.i  ~
        [@ ~]       
      :-  ~
      :*  [text+(rap 3 'New links in ' title ~)]~
          [ship+author.p.i text+': ' (hark-contents:graph contents.p.i)]
          [0 1]  %each  %children
      ==


        [@ @ %1 ~]
      :-  ~
      :*  [text+(rap 3 'New comments on a post in ' title ~)]~
          [ship+author.p.i text+': ' (hark-contents:graph contents.p.i)]
          [1 2]  %count  %siblings
      ==

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
  ++  noun  indexed-post
  --
++  grad  %noun
--
