::
::::  /hoon/jump-opt/kiln/mar
  ::
|_  req=[old=dock new=dock yea=?]
::
++  grow
  |%
  ++  noun  req
  --
++  grab
  |%
  ++  noun  ,[dock dock ?]
  ++  json
    =,  dejs:format
    %-  ot
    :~  [%old (ot ship+(se %p) desk+so ~)]
        [%new (ot ship+(se %p) desk+so ~)]
        [%yea bo]
    ==
  --
++  grad  %noun
--
