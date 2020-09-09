/-  *post
|_  i=indexed-post
++  grow
  |%
  ++  noun  i
  --
++  grab
  |%
  ++  noun
    |=  p=*
    =/  ip  ;;(indexed-post p)
    |^
    ?+    index.p.ip  !!
      [@ @ ~]  ip
    ::  top level post must have title as first content
        [@ ~]
      ?>  ?=(^ contents.p.ip)
      ?>  ?=(%text -.i.contents.p.ip)
      ip
    ==
  --
::
++  grad  %noun
--
