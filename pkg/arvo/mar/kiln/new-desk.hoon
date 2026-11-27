|%
+$  new-desk  [=desk overwrite=?]
--
|_  n=new-desk
++  grad  %noun
++  grow
  |%
  ++  noun  n
  ++  json
    %-  pairs:enjs:format
    :~  desk+s+desk.n
        overwrite+b+overwrite.n
    ==
  --
++  grab
  |%
  ++  noun  new-desk
  ++  json
    ^-  $-(^json new-desk)
    =,  dejs:format
    %-  ot
    :~  desk+so
        overwrite+bo
    ==
  --
--
