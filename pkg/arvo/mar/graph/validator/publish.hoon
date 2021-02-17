/-  *post, met=metadata-store
|_  i=indexed-post
++  grow
  |%
  ++  noun  i
  ++  graph-permissions-add
    |=  vip=vip-metadata:met
    ?+  index.p.i  !!
      [@ ~]            [%yes %yes %no]  :: new note
      [@ %1 @ ~]       [%self %self %no]
      [@ %2 @ ~]       [%yes %yes ?:(?=(%reader-comments vip) %yes %no)]
      [@ %2 @ @ ~]     [%self %self %self]
    ==
  ::
  ++  graph-permissions-remove
    |=  vip=vip-metadata:met
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
    ?+  index.p.i   ~
      [@ %1 %1 ~]    `[%note 0 %each %.n]
      [@ %1 @ ~]     `[%edit-note 0 %none %.n]
      [@ %2 @ %1 ~]  `[%comment 1 %count %.n]
      [@ %2 @ @ ~]   `[%edit-comment 1 %none %.n]
    ==
  --
++  grab
  |%
  :: +noun: validate publish post
  :: 
  ++  noun
    |=  p=*
    =/  ip  ;;(indexed-post p)
    ?+    index.p.ip  !!
    ::  top level post must have no content
        [@ ~]
      ?>  ?=(~ contents.p.ip)
      ip
    ::  container for revisions
    ::
        [@ %1 ~]  
      ?>  ?=(~ contents.p.ip)
      ip
    ::  specific revision
    ::  first content is the title
    ::  revisions are numbered by the revision count
    ::  starting at one
        [@ %1 @ ~]
      ?>  ?=([* * *] contents.p.ip)
      ?>  ?=(%text -.i.contents.p.ip)
      ip
    ::  container for comments
    ::
        [@ %2 ~]
      ?>  ?=(~ contents.p.ip)
      ip
    ::  container for comment revisions
    ::
        [@ %2 @ ~]
      ?>  ?=(~ contents.p.ip)
      ip
    ::  specific comment revision
    ::
        [@ %2 @ @ ~]
      ?>  ?=(^ contents.p.ip)
      ip
    ==
  --
::
++  grad  %noun
--
