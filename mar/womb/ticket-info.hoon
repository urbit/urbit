::
::::  /hoon/stat-all/womb/mar
  ::
/?    310
::
::::  ~fyr
  ::
|_  {pas/@uvH status/?($fail $good $used)}
::
++  grab
  |%
  ++  noun  {@uvH ?($fail $good $used)}
  --
++  grow
  |%
  ++  json
    %-  pairs:enjs:format  :~
      passcode+(tape:enjs:format <pas>)
      status+[%s status]
    ==
  --
--
