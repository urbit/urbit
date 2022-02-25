|%
++  start-of-day
  |=  tim=@da
  =/  =date  (yore tim)
  (year date(t [d.t.date 0 0 1 ~]))
::
++  tz
  |_  zone=@s
  ++  from-utc
    |=  utc=@dau
    ^-  @dal
    =/  [neg=? val=@ud]  (old:si zone)
    =.  val  (mul val ~m1)
    (?:(neg add sub) utc val)
  ::
  ++  to-utc
    |=  loc=@dal
    ^-  @dau
    =/  [neg=? val=@ud]  (old:si zone)
    =.  val  (mul val ~m1)
    (?:(neg sub add) loc val)
  --
--
