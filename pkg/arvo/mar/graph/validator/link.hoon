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
    ?+    index.p.ip  ~|(index+index.p.ip !!)
        ::  top-level link post
        ::
        [@ ~]
      ?>  ?=([[%text @] [%url @] [%text @] ~] contents.p.ip)
      ip
    ::
        ::  comment on link post
        ::
        [@ @ ~]
      ?>  ?=([[%text @] ~] contents.p.ip)
      ip
    ==
  --
++  grad  %noun
--
