::
::::  /hoon/approve-merge/kiln/mar
  ::
/-  h=hood
|_  req=[sync-record:h approve=?]
::
++  grow
  |%
  ++  noun  req
  --
++  grab
  |%
  ++  noun  ,[sync-record:h approve=?]
  ++  json
    =,  dejs:format
    %-  ot
    :~  [%sync (ot syd+so her+(se %p) sud+so ~)]
        [%approve bo]
    ==
  --
++  grad  %noun
--
