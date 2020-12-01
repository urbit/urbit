/-  *post
|_  i=indexed-post
++  grow
  |%
  ++  noun  i
  ++  notification-kind
    ?+  index.p.i  ~
      [@ ~]  `[%message 0 %count]
    ==
  --
++  grab
  |%
  ++  noun
    |=  p=*
    =/  ip  ;;(indexed-post p)
    ?>  ?=([@ ~] index.p.ip)
    ip
  --
::
++  grad  %noun
--
