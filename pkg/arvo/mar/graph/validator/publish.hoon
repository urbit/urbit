/-  *post
|_  i=indexed-post
++  grow
  |%
  ++  noun  i
  ::  +notification-kind
  ::    Ignore all containers, only notify on content
  ::
  ++  notification-kind
    ?+  index.p.i   ~
      [@ %1 %1 ~]    `[%note 0]
      [@ %1 @ ~]     `[%edit-note 0]
      [@ %2 @ %1 ~]  `[%comment 1]
      [@ %2 @ @ ~]   `[%edit-comment 1]
    ==
  --
++  grab
  |%
  :: +noun: Validate publish post
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
