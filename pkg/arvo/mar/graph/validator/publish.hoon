/-  *post
|_  i=indexed-post
++  grow
  |%
  ++  noun  i
  --
++  grab
  |%
  :: +noun: Validate publish post
  :: 
  ++  noun
    |=  p=*
    =/  ip  ;;(indexed-post p)
    ?+    index.p.ip  !!
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
      ?>  ?=([* * ~] contents.p.ip)
      ?>  ?=(%text -.i.contents.p.ip)
      ?>  ?=(%text -.t.i.contents.p.ip)
      ip
    ::  container for comments
        [@ %2 ~]
      ?>  ?=(~ contents.p.ip)
      ip
    ::  comment
        [@ %2 @ ~]
      ?>  ?=([* ~] contents.p.ip)
      ?>  ?=(%text -.i.contents.p.ip)
      ip
    ::  top level post must have no content
        [@ ~]
      ?>  ?=(~ contents.p.ip)
      ip
    ==
  --
::
++  grad  %noun
--
