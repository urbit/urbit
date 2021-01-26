/-  *post
|_  i=indexed-post
++  grow
  |%
  ++  noun  i
  ::  ?(%no %writer)
  ++  notification-kind
   ::
    ?+  index.p.i  ~
      [@ ~]  `[%message 0 %count %.n]
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
