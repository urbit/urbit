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
    ?>  (gte 2 (lent index.p.ip))
    ip
  --
::
++  grad  %noun
--
