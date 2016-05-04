::
::::  /hoon/stat-all/womb/mar
  ::
/?    310
::
::::  ~fyr
  ::
|_  {pas/@pG status/?($fail $good $used)}
::
++  grab
  |%
  ++  noun  {@pG ?($fail $good $used)}
  --
++  grow
  |%
  ++  json
    %-  jobe  :~
      passcode+(jape +:<pas>)
      status+[%s status]
    ==
  --
--
