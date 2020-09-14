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
        ::  top-level link post; title and url
        ::
        [@ ~]
      ?>  ?=([[%text @] [%url @] ~] contents.p.ip)
      ip
    ::
        ::  comment on link post; comment text
        ::
        [@ @ ~]
      ?>  ?=([[%text @] ~] contents.p.ip)
      ip
    ==
  --
++  grad  %noun
--
